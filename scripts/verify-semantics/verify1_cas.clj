(ns verify1-cas
  "Semantic verification #1 — atomic CURRENT compare-and-set.
  Model + real-code check."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.migrate :as migrate]
            [evoclj.promotion.current :as current]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

;; ---------------------------------------------------------------
;; A. Abstract model: why BEGIN IMMEDIATE (atomic CAS) is necessary.
;;    A CAS transaction = {cond: current == expected, act: current = id}.
;;    - atomic model: the whole txn is one step (BEGIN IMMEDIATE)
;;    - statement model: cond and act interleave as separate steps
;;    Enumerate ALL interleavings of two sibling promotions and check
;;    the invariant: exactly one CURRENT, at most one winner.
;; ---------------------------------------------------------------
(defn atomic-run [order]
  (reduce (fn [s {:keys [expected id]}]
            (if (= (:current s) expected)
              (assoc s :current id :winners (conj (:winners s) id))
              (assoc s :stale (conj (:stale s) id))))
          {:current :G42 :winners [] :stale []}
          order))

;; manual permutation generator (no extra deps)
(defn- perms [coll]
  (if (empty? coll) [()]
    (mapcat (fn [x] (map #(cons x %) (perms (remove #{x} coll))))
            coll)))

(defn stmt-interleavings [order]
  (let [steps (mapcat (fn [{:keys [expected id]}]
                        [{:kind :cond :expected expected :id id}
                         {:kind :act :id id}])
                      order)]
    (filter (fn [p]
              (and (< (.indexOf p (first steps)) (.indexOf p (second steps)))
                   (< (.indexOf p (nth steps 2)) (.indexOf p (nth steps 3)))))
            (perms steps))))

(defn stmt-run [steps]
  (reduce (fn [s st]
            (case (:kind st)
              :cond (assoc s :cond-ok (= (:current s) (:expected st)))
              :act (if (:cond-ok s)
                     (-> s (assoc :current (:id st))
                          (assoc :winners (conj (:winners s) (:id st))))
                     (assoc s :stale (conj (:stale s) (:id st))))))
          {:current :G42 :winners [] :stale [] :cond-ok false}
          steps))

(let [t1 {:expected :G42 :id :G43a}
      t2 {:expected :G42 :id :G43b}
      orders [[t1 t2] [t2 t1]]]
  ;; atomic model: both orders safe
  (doseq [o orders]
    (let [r (atomic-run o)]
      (check! (str "atomic model, order " (map :id o))
              (and (= 1 (count (:winners r)))
                   (= (:current r) (first (:winners r)))
                   (= 1 (count (:stale r))))
              (pr-str r))))
  ;; statement model: find a broken interleaving (motivates BEGIN IMMEDIATE)
  (let [broken (filter (fn [p]
                         (let [r (stmt-run p)]
                           (not (and (= 1 (count (:winners r)))
                                     (= (:current r) (first (:winners r)))))))
                       (stmt-interleavings [t1 t2]))]
    (check! "statement-level CAS has broken interleavings (motivates BEGIN IMMEDIATE)"
            (seq broken)
            (str (count broken) " broken of "
                 (count (stmt-interleavings [t1 t2])) " interleavings; e.g. "
                 (pr-str (first broken))))))

;; ---------------------------------------------------------------
;; B. Real code: cas-current! on a real migrated SQLite store.
;; ---------------------------------------------------------------
(let [p (str (java.nio.file.Files/createTempFile "verify-cas-" ".db"
                                                 (make-array java.nio.file.attribute.FileAttribute 0)))
      db (sqlite/spec p)]
  (try
    (migrate/migrate! db)
    ;; contract discovered during verification: java.jdbc high-level
    ;; fns (insert!/query) take the SPEC (sqlite/with-db binds the
    ;; spec, not a Connection); raw primitives like cas-current! need
    ;; an explicit java.sql.Connection via jdbc/get-connection (the
    ;; same split promote.clj follows).
    (sqlite/with-db [spec db]
      (jdbc/insert! spec :generations
                    {:id "G42" :genome_id (str "sha256:" (apply str (repeat 64 "a")))
                     :resolution_id (str "sha256:" (apply str (repeat 64 "c")))
                     :parent_id nil :state "active" :current 1
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! spec :generations
                    {:id "G43a" :genome_id (str "sha256:" (apply str (repeat 64 "b")))
                     :resolution_id (str "sha256:" (apply str (repeat 64 "c")))
                     :parent_id "G42" :state "active" :current 0
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! spec :generations
                    {:id "G43b" :genome_id (str "sha256:" (apply str (repeat 64 "d")))
                     :resolution_id (str "sha256:" (apply str (repeat 64 "c")))
                     :parent_id "G42" :state "active" :current 0
                     :created_at "2025-01-01T00:00:00Z"}))
    (with-open [conn (jdbc/get-connection db)]
      (check! "cas-current! G42->G43a returns :ok"
              (= :ok (current/cas-current! conn "G42" "G43a"))
              "first promotion clears+sets")
      (check! "cas-current! G42->G43b (stale sibling) returns :stale"
              (= :stale (current/cas-current! conn "G42" "G43b"))
              "pointer already moved — 0 rows cleared")
      (let [cur (current/read-current conn)]
        (check! "exactly one CURRENT row, pointing at the winner"
                (and (= 1 (count (sqlite/query db ["SELECT * FROM generations WHERE current = 1"])))
                     (= "G43a" (:id cur)))
                (str "current = " (:id cur)))))
    (finally
      (java.nio.file.Files/deleteIfExists
       (java.nio.file.Paths/get p (make-array String 0))))))
(println "VERIFY1 DONE")
