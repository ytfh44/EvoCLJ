(ns verify7-session-states
  "Semantic verification #7 — session state machine closure/legality.
  Model: directed graph over 8 states; edges exactly as declared in
  evoclj.store.session/transitions; terminal states have no outgoing
  edges; the real transition-session! accepts exactly the declared
  edges and rejects everything else with :session/invalid-transition."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def all-states #{:created :resolving :running :waiting
                  :completed :failed :cancelled :budget-exhausted})
(def transitions session/transitions)
(def terminal #{:completed :failed :cancelled :budget-exhausted})

;; P1: declared edges are within the state set (closure)
(check! "closure: every source and target is a known state"
        (every? #(contains? all-states %)
                (concat (keys transitions)
                        (mapcat (fn [[_ ts]] ts) transitions)))
        (pr-str (keys transitions)))

;; P2: terminal states have no outgoing edges (absorption)
(check! "terminal states are absorbing (no declared outgoing edges)"
        (every? #(not (contains? transitions %)) terminal)
        "terminal states have no entry in the transition table")

;; P3: the real machine accepts exactly the declared edges and rejects
;; every other pair (including all terminal-source pairs and self-loops)
(let [p (str (java.nio.file.Files/createTempFile "verify-sess-" ".db"
                                                 (make-array java.nio.file.attribute.FileAttribute 0)))
      db (sqlite/spec p)
      hex64 (apply str (repeat 64 "a"))]
  (try
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "G42" :genome_id (str "sha256:" hex64)
                     :resolution_id (str "sha256:" hex64)
                     :parent_id nil :state "active" :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    (doseq [from (sort all-states) to (sort all-states)]
      (let [sid (:session/id (session/create-session! db {:genome/id (str "sha256:" hex64)
                                                          :resolution/id (str "sha256:" hex64)
                                                          :phenotype/id (str "sha256:" hex64)
                                                          :generation/id "G42"}))
            expected-legal? (contains? (get transitions from #{}) to)
            ;; legality is static: the CAS only checks the declared
            ;; table, so set the row state directly to make the
            ;; transition under test reachable
            _ (sqlite/with-db [conn db]
                (jdbc/update! conn :sessions {:state (name from)}
                              ["id = ?" (str sid)]))
            r (try
                (session/transition-session! db sid from to nil)
                :ok
                (catch clojure.lang.ExceptionInfo e
                  (:error/type (ex-data e))))]
        (if expected-legal?
          (check! (str "legal edge " from " -> " to " accepted")
                  (= :ok r)
                  (str "got " r))
          (check! (str "illegal edge " from " -> " to " rejected")
                  (= :session/invalid-transition r)
                  (str "got " r)))))
    (finally
      (java.nio.file.Files/deleteIfExists
       (java.nio.file.Paths/get p (make-array String 0))))))
(println "VERIFY7 DONE")
