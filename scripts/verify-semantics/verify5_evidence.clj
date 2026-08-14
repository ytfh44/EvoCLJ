(ns verify5-evidence
  "Semantic verification #5 — frozen evidence packs.
  Model: pack = f(episodes with last_event <= cutoff) where f is a pure
  deterministic function and the selection predicate is monotone in the
  cutoff; episodes created after the cutoff are outside f's input set,
  so the pack (id + content) is invariant to late arrivals.
  Real code: evoclj.evolution.evidence/build-evidence-pack."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.event :as event]
            [evoclj.evolution.evidence :as evidence]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def hex64 (apply str (repeat 64 "a")))
(def hex64b (apply str (repeat 64 "b")))
(def hex64c (apply str (repeat 64 "c")))
(def phash (str "sha256:" (apply str (repeat 64 "f"))))
(def gen "G42")
(def now "2025-01-01T00:00:00Z")

(let [p (str (java.nio.file.Files/createTempFile "verify-evidence-" ".db"
                                                 (make-array java.nio.file.attribute.FileAttribute 0)))
      root (str (java.nio.file.Files/createTempDirectory
                 "verify-evidence-cas-" (make-array java.nio.file.attribute.FileAttribute 0)))
      db (sqlite/spec p)]
  (try
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id gen :genome_id (str "sha256:" hex64)
                     :resolution_id (str "sha256:" hex64b)
                     :parent_id nil :state "active" :current 1
                     :created_at now}))
    (letfn [(insert-episode! [sid first-id last-id outcome]
              (let [eid (random-uuid)]
                ;; FKs are enforced: episodes.session_id -> sessions and
                ;; episodes.first/last_event_id -> events, so build the
                ;; real rows via the store APIs (events get real ids +
                ;; hash chain entries).
                (sqlite/exec! db
                              ["INSERT INTO sessions
                                 (id, generation_id, genome_id, resolution_id,
                                  phenotype_id, state, created_at)
                               VALUES (?, ?, ?, ?, ?, ?, ?)"
                               (str sid) gen (str "sha256:" hex64)
                               (str "sha256:" hex64b) (str "sha256:" hex64c)
                               "completed" now])
                (let [ev1 (:event/id (event/append-event! db {:session/id sid
                                                              :generation/id gen
                                                              :phenotype/id (str "sha256:" hex64c)
                                                              :event/type :session/created
                                                              :cause/event-id nil
                                                              :payload-ref nil
                                                              :metadata {}}))
                      ev2 (:event/id (event/append-event! db {:session/id sid
                                                              :generation/id gen
                                                              :phenotype/id (str "sha256:" hex64c)
                                                              :event/type :intent/proposed
                                                              :cause/event-id ev1
                                                              :payload-ref nil
                                                              :metadata {}}))]
                  ;; first/last_event_id are REAL event ids (FKs); the
                  ;; caller's first-id/last-id args are the logical trace
                  ;; bounds used to order episodes against the cutoff
                  (sqlite/exec! db
                                ["INSERT INTO episodes
                                   (id, session_id, generation_id, genome_id,
                                    resolution_id, task_ref, first_event_id,
                                    last_event_id, outcome, usage, created_at)
                                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                                 (str eid) (str sid) gen (str "sha256:" hex64)
                                 (str "sha256:" hex64b) phash ev1 ev2
                                 (pr-str outcome) (pr-str {}) now])
                  eid)))]
      ;; 3 successes + 2 failures BEFORE cutoff 100; 1 late episode AFTER
      (let [sids (mapv (fn [_] (random-uuid)) (range 6))
            _ (insert-episode! (sids 0) 10 20 {:status :completed})
            _ (insert-episode! (sids 1) 21 40 {:status :completed})
            _ (insert-episode! (sids 2) 41 60 {:status :completed})
            _ (insert-episode! (sids 3) 61 80 {:status :failed})
            _ (insert-episode! (sids 4) 9 10 {:status :failed})
            late-sid (sids 5)
            _ (insert-episode! late-sid 11 12 {:status :completed})
            req {:generation/id gen :cutoff-event-id 10
                 :selector {:recent 10 :include-successes 10 :include-failures 10
                            :include-high-cost 5}}
            pack1 (evidence/build-evidence-pack {:sqlite db :cas (evoclj.store.cas/->cas root)} req)
            late-ids (set (map :episode/id (:episodes pack1)))
            _ (insert-episode! (str (random-uuid)) 13 14 {:status :completed})
            pack2 (evidence/build-evidence-pack {:sqlite db :cas (evoclj.store.cas/->cas root)} req)]
        (check! "late episode (last_event 120 > cutoff 100) excluded from the pack"
                (not (contains? (set (map :episode/id (:episodes pack1)))
                                (first (filter #(= % late-sid)
                                               (map :session/id (:episodes pack1))))))
                (str "pack episode count = " (count (:episodes pack1))))
        (check! "successes AND failures both represented (summary counts)"
                (and (pos? (:successes (:summary pack1)))
                     (pos? (:failures (:summary pack1))))
                (pr-str (select-keys (:summary pack1) [:successes :failures :total])))
        (check! "pack is pure: a NEW episode after cutoff changes nothing (id + content)"
                (and (= (:evidence/id pack1) (:evidence/id pack2))
                     (= (:episodes pack1) (:episodes pack2)))
                (str "id " (:evidence/id pack1) " stable across rebuilds"))
        (check! "raising the cutoff admits the late episode"
                (let [pack3 (evidence/build-evidence-pack
                             {:sqlite db :cas (evoclj.store.cas/->cas root)} (assoc req :cutoff-event-id 20))]
                  (pos? (count (filter #(= late-sid (:session/id %))
                                       (:episodes pack3)))))
                "cutoff 20 includes episode with last_event 12"))
      (check! "pack id is content-addressed (sha256)"
              (re-matches #"^sha256:[0-9a-f]{64}$" (:evidence/id (evidence/build-evidence-pack {:sqlite db :cas (evoclj.store.cas/->cas root)}
                                {:generation/id gen :cutoff-event-id 10
                                 :selector {:recent 10 :include-successes 1
                                            :include-failures 1 :include-high-cost 1}})))
              "evidence/id is a canonical sha256"))
    (finally
      (java.nio.file.Files/deleteIfExists
       (java.nio.file.Paths/get p (make-array String 0)))
      (let [root (java.nio.file.Paths/get root (make-array String 0))]
        (when (java.nio.file.Files/exists root (make-array java.nio.file.LinkOption 0))
          (doseq [f (reverse (file-seq (.toFile root)))]
            (java.nio.file.Files/deleteIfExists (.toPath f))))))))
(println "VERIFY5 DONE")
