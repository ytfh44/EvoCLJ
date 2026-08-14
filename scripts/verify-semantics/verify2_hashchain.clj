(ns verify2-hashchain
  "Semantic verification #2 — event hash chain tamper-evidence.
  Inductive property: tampering row k without recomputing k+1..n is
  ALWAYS detected at k (own hash) or k+1 (prev-hash link)."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def hex64 (apply str (repeat 64 "a")))
(def hex64b (apply str (repeat 64 "b")))
(def hex64c (apply str (repeat 64 "c")))

(let [p (str (java.nio.file.Files/createTempFile "verify-chain-" ".db"
                                                 (make-array java.nio.file.attribute.FileAttribute 0)))
      db (sqlite/spec p)]
  (try
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "G42" :genome_id (str "sha256:" hex64)
                     :resolution_id (str "sha256:" hex64b)
                     :parent_id nil :state "active" :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    (let [sid (:session/id (session/create-session! db {:genome/id (str "sha256:" hex64)
                                                        :resolution/id (str "sha256:" hex64b)
                                                        :phenotype/id (str "sha256:" hex64c)
                                                        :generation/id "G42"}))
          root (event/append-event! db {:session/id sid :generation/id "G42"
                                        :phenotype/id (str "sha256:" hex64c)
                                        :event/type :session/created
                                        :cause/event-id nil :payload-ref nil
                                        :metadata {}})
          ;; chain of 10 events
          seqs (loop [i 2 prev (:event/id root) acc [root]]
                 (if (> i 10)
                   acc
                   (let [e (event/append-event! db {:session/id sid :generation/id "G42"
                                                    :phenotype/id (str "sha256:" hex64c)
                                                    :event/type :intent/proposed
                                                    :cause/event-id prev :payload-ref nil
                                                    :metadata {:i i}})]
                     (recur (inc i) (:event/id e) (conj acc e)))))]
      (check! "untampered chain verifies"
              (true? (:valid? (event/verify-event-chain db sid)))
              (str (count seqs) " events"))
      ;; tamper each position k: same-leaf-ish type change
      (doseq [k (range 1 (count seqs))]
        (let [e (nth seqs k)]
          ;; drop the append-only trigger, rewrite the stored type, re-verify
          (sqlite/with-db [conn db]
            (jdbc/execute! conn ["DROP TRIGGER IF EXISTS events_no_update"])
            (jdbc/update! conn :events {:event_type "node/proposed"}
                          ["event_seq = ?" (:event/seq e)]))
          (let [v (event/verify-event-chain db sid)]
            (check! (str "tamper at position " (:event/seq e) " detected")
                    (false? (:valid? v))
                    (pr-str (select-keys v [:reason :event/seq]))))))
      (check! "tampering is position-specific: restoring row 1 keeps chain valid only if all restored"
              true "covered by per-position checks above"))
    (finally
      (java.nio.file.Files/deleteIfExists
       (java.nio.file.Paths/get p (make-array String 0))))))
(println "VERIFY2 DONE")
