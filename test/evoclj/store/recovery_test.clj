(ns evoclj.store.recovery-test
  "Task 5.5 tests for restart recovery and integrity checks.

  Step 1: a session left in :running with no terminal event is
  classified as orphaned — recovery never pretends completion (no
  fabricated terminal event, no rewritten row state). Step 2: an event
  whose :payload-ref points at an absent CAS artifact is reported under
  :missing-artifacts, and the strict startup scan fails closed on it.
  Step 3: a candidate row left in a prepared state (:evaluating) is
  reported under :stale-candidates and recovery must not promote it —
  the row is untouched and no promotions row appears. Step 4: the
  startup integrity scan is configurable; the production default is
  strict (fail-closed) on current-generation corruption (Database
  Invariant 7: an active generation's Genome must exist in CAS and pass
  integrity) and on missing/ambiguous CURRENT (Database Invariant 6).

  Milestone 5 exit test: a realistic session -> intents -> completed
  trace is appended to a temp store, the connection is dropped, and the
  DB + CAS are REOPENED from disk; the event chain verifies and the
  pinned session identity is reconstructed with no in-memory state.

  Fresh temp databases are migrated from the classpath migrations and
  deleted after every test, as are the temp CAS roots."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)))

;; --- shared fixtures -------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private genome-bytes "seed genome manifest: task 5.5")
(def ^:private evidence-id (str "sha256:" (apply str (repeat 64 "d"))))

(def ^:private db-paths (atom []))
(def ^:private roots (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-recovery-" ".db"
                (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-root
  "A throwaway CAS root in the system temp dir, registered for cleanup."
  []
  (let [p (Files/createTempDirectory "evoclj-recovery-cas-"
                                     (make-array java.nio.file.attribute.FileAttribute 0))]
    (swap! roots conj p)
    p))

(defn- delete-tree!
  "Recursively delete a temp tree (children before parents)."
  [^Path root]
  (when (Files/exists root (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile root)))]
      (Files/deleteIfExists (.toPath f)))))

(defn- cleanup!
  "Delete every temp db file and CAS root created during this run."
  []
  (doseq [p @db-paths]
    (java.nio.file.Files/deleteIfExists
     (java.nio.file.Paths/get p (make-array String 0))))
  (doseq [r @roots] (delete-tree! r))
  (reset! db-paths [])
  (reset! roots []))

(use-fixtures :each (fn [f] (try (f) (finally (cleanup!)))))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- txt [s]
  (.getBytes s StandardCharsets/UTF_8))

(defn- put!
  "put-bytes! sugar: UTF-8 bytes with an EDN media type; returns the
  artifact descriptor map."
  [root s]
  (cas/put-bytes! root (txt s) {:media-type "application/edn"}))

(defn- put-genome!
  "Store the seed genome bytes in the CAS and return their content
  address (used as the generation's genome_id and the session's
  pinned :genome/id)."
  [root]
  (:artifact/id (put! root genome-bytes)))

(defn- seed-generation!
  "Insert the generation row sessions/candidates are pinned to (once per
  db). `current` defaults to 1 so the CURRENT pointer is valid."
  ([db genome-id] (seed-generation! db genome-id 1))
  ([db genome-id current]
   (sqlite/with-db [conn db]
     (jdbc/insert! conn :generations
                   {:id gen
                    :genome_id genome-id
                    :resolution_id resolution
                    :parent_id nil
                    :state "active"
                    :current current
                    :created_at now}))))

(defn- session-request
  "A valid create-session! request pinned to the given genome id;
  callers merge overrides."
  [genome-id & [overrides]]
  (merge {:genome/id genome-id
          :resolution/id resolution
          :phenotype/id phenotype
          :generation/id gen}
         overrides))

(defn- base-event
  "An append-event! request skeleton; callers override :event/type and
  supply a real :cause/event-id for non-root events."
  [sid & [overrides]]
  (merge {:session/id sid
          :generation/id gen
          :phenotype/id phenotype
          :event/type :intent/proposed
          :cause/event-id nil
          :payload-ref nil
          :metadata {:source :recovery-test}}
         overrides))

(defn- insert-mutation!
  "Insert a mutations row the candidate fixture references."
  [db mutation-id genome-id]
  (sqlite/exec! db
                ["INSERT INTO mutations
                    (id, parent_genome_id, hypothesis_id, evidence_id,
                     risk, ops, expected_effect, created_at)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                 mutation-id genome-id (str (random-uuid)) evidence-id
                 "parameter" "[]" "{}" now]))

(defn- insert-candidate!
  "Insert a candidates row in the given state. The composite FK
  (parent_generation_id, parent_genome_id) must match the seeded
  generation, and mutation_id must exist."
  [db candidate-id mutation-id genome-id cand-genome state]
  (sqlite/exec! db
                ["INSERT INTO candidates
                    (id, parent_generation_id, parent_genome_id, genome_id,
                     mutation_id, evidence_id, risk, state, created_at)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 candidate-id gen genome-id cand-genome mutation-id
                 evidence-id "parameter" state now]))

(defn- body-file
  "The CAS body File for an artifact id."
  [^Path root id]
  (let [hex (subs id 7)]
    (java.io.File. (.toFile root) (str "sha256/" (subs hex 0 2) "/" hex "/body"))))

(defn- delete-artifact!
  "Delete a CAS artifact's body file (simulates loss of the artifact)."
  [root id]
  (java.nio.file.Files/deleteIfExists
   (.toPath (body-file root id))))

(defn- tamper-artifact!
  "Overwrite a CAS artifact's body with different bytes (simulates
  corruption)."
  [root id]
  (spit (body-file root id) "tampered genome bytes"))

(defn- scan-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

;; ============================================================================
;; Step 1 — a session left :running with no terminal event is orphaned
;; ============================================================================

(deftest running-session-without-terminal-event-is-orphaned
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)
        sid (:session/id (session/create-session! db (session-request genome-id)))
        created (event/append-event! db (base-event sid {:event/type :session/created}))
        _ (session/transition-session! db sid :created :resolving {})
        _ (session/transition-session! db sid :resolving :running {})
        _ (event/append-event! db (base-event sid {:event/type :session/started
                                                   :cause/event-id (:event/id created)}))
        report (recovery/scan-recovery-state db root)]
    (testing "the crash-interrupted session is classified as orphaned, not completed"
      (is (= [sid] (mapv :session/id (:orphaned-sessions report))))
      (is (= :running (:state (first (:orphaned-sessions report)))))
      (is (= 2 (:last-event-seq (first (:orphaned-sessions report))))))
    (testing "recovery never pretends completion: no terminal event is fabricated"
      (is (empty? (event/events-by-type db sid :session/completed))))
    (testing "and the persisted row state is untouched by the scan"
      (is (= :running (:state (session/get-session db sid)))))
    (testing "the orphan is not corruption: an otherwise healthy store scans clean"
      (is (empty? (:missing-artifacts report)))
      (is (empty? (:invalid-event-chains report)))
      (is (empty? (:stale-candidates report)))
      (is (true? (:ok? (recovery/startup-integrity-scan db root)))))))

(deftest sessions-in-terminal-states-are-not-orphaned
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)
        done (:session/id (session/create-session! db (session-request genome-id)))
        created (event/append-event! db (base-event done {:event/type :session/created}))
        _ (event/append-event! db (base-event done {:event/type :session/started
                                                    :cause/event-id (:event/id created)}))
        _ (session/transition-session! db done :created :resolving {})
        _ (session/transition-session! db done :resolving :running {})
        _ (session/transition-session! db done :running :waiting {})
        _ (session/transition-session! db done :waiting :completed {})
        _ (event/append-event! db (base-event done {:event/type :session/completed
                                                    :cause/event-id (:event/id created)}))]
    (is (= [] (:orphaned-sessions (recovery/scan-recovery-state db root))))))

;; ============================================================================
;; Step 2 — an event referencing an absent CAS payload fails loudly
;; ============================================================================

(deftest missing-payload-artifact-is-reported
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)
        sid (:session/id (session/create-session! db (session-request genome-id)))
        created (event/append-event! db (base-event sid {:event/type :session/created}))
        ghost (str "sha256:" (apply str (repeat 64 "f")))
        _ (event/append-event! db (base-event sid {:event/type :intent/proposed
                                                   :payload-ref ghost
                                                   :cause/event-id (:event/id created)}))
        report (recovery/scan-recovery-state db root)]
    (testing "the unresolved content reference is reported with its event"
      (is (= [{:session/id sid
               :event/seq 2
               :event/type :intent/proposed
               :payload-ref ghost}]
             (:missing-artifacts report))))
    (testing "a reference that resolves is never reported"
      (let [present (:artifact/id (put! root "request body"))
            _ (event/append-event! db (base-event sid {:event/type :intent/normalized
                                                       :payload-ref present
                                                       :cause/event-id (:event/id created)}))]
        (is (= [ghost] (mapv :payload-ref (:missing-artifacts
                                           (recovery/scan-recovery-state db root)))))))))

(deftest strict-scan-fails-closed-on-missing-artifact
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)
        sid (:session/id (session/create-session! db (session-request genome-id)))
        created (event/append-event! db (base-event sid {:event/type :session/created}))
        ghost (str "sha256:" (apply str (repeat 64 "f")))
        _ (event/append-event! db (base-event sid {:event/type :intent/proposed
                                                   :payload-ref ghost
                                                   :cause/event-id (:event/id created)}))]
    (testing "the production default (strict) refuses to start"
      (let [e (scan-error #(recovery/startup-integrity-scan db root))]
        (is (some? e))
        (is (= :store/integrity-failure (:error/type (ex-data e))))
        (is (= ghost (-> e ex-data :missing-artifacts first :payload-ref)))))
    (testing "non-strict mode reports instead of throwing"
      (let [r (recovery/startup-integrity-scan db root {:strict? false})]
        (is (false? (:ok? r)))
        (is (= [ghost] (mapv :payload-ref (:missing-artifacts r))))))))

;; ============================================================================
;; Step 3 — a prepared but uncommitted candidate is stale, never promoted
;; ============================================================================

(deftest prepared-candidate-is-stale-and-never-promoted
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        cand-genome (str "sha256:" (apply str (repeat 64 "e")))
        _ (seed-generation! db genome-id)
        mutation-id (str (random-uuid))
        candidate-id (str (random-uuid))
        _ (insert-mutation! db mutation-id genome-id)
        _ (insert-candidate! db candidate-id mutation-id genome-id cand-genome "evaluating")
        report (recovery/scan-recovery-state db root)
        entry (first (:stale-candidates report))]
    (testing "the prepared candidate is reported stale with its original state"
      (is (= 1 (count (:stale-candidates report))))
      (is (= candidate-id (str (:candidate/id entry))))
      (is (= genome-id (:parent/genome-id entry)))
      (is (= cand-genome (:candidate/genome-id entry)))
      (is (= :evaluating (:state entry)))
      (is (= mutation-id (str (:mutation/id entry)))))
    (testing "recovery must not promote it: the row is untouched"
      (is (= "evaluating"
             (:state (first (sqlite/query db ["SELECT state FROM candidates WHERE id = ?"
                                              candidate-id]))))))
    (testing "and no promotion record appears"
      (is (empty? (sqlite/query db ["SELECT id FROM promotions WHERE candidate_id = ?"
                                    candidate-id]))))
    (testing "a stale candidate is recoverable crash residue, not corruption"
      (let [r (recovery/startup-integrity-scan db root {:strict? false})]
        (is (true? (:ok? r)))
        (is (= 1 (count (:stale-candidates r))))))))

(deftest promoted-and-rejected-candidates-are-not-stale
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)
        mk (fn [state]
             (let [mutation-id (str (random-uuid))
                   candidate-id (str (random-uuid))]
               (insert-mutation! db mutation-id genome-id)
               (insert-candidate! db candidate-id mutation-id genome-id
                                  (str "sha256:" (apply str (repeat 64 (rand-int 10))))
                                  state)))
        _ (mk "promoted")
        _ (mk "rejected")
        _ (mk "stale")]
    (is (= [] (:stale-candidates (recovery/scan-recovery-state db root))))))

;; ============================================================================
;; Step 4 — startup integrity scan: configurable strict mode, fail-closed
;; ============================================================================

(deftest empty-store-scans-clean
  (let [db (fresh-db)
        root (temp-root)
        r (recovery/startup-integrity-scan db root)]
    (is (true? (:ok? r)))
    (is (= :none (:status (:current-generation r))))
    (is (empty? (:orphaned-sessions r)))
    (is (empty? (:missing-artifacts r)))
    (is (empty? (:invalid-event-chains r)))
    (is (empty? (:stale-candidates r)))))

(deftest intact-current-generation-passes-strict-scan
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)
        r (recovery/startup-integrity-scan db root)]
    (is (true? (:ok? r)))
    (is (= {:status :ok :generation/id gen :genome/id genome-id}
           (:current-generation r)))))

(deftest strict-mode-fails-closed-on-missing-current-genome
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)]
    (delete-artifact! root genome-id)
    (testing "the production default (strict) refuses to start"
      (let [e (scan-error #(recovery/startup-integrity-scan db root))]
        (is (some? e))
        (is (= :store/integrity-failure (:error/type (ex-data e))))
        (is (= :missing (:status (:current-generation (ex-data e)))))
        (is (= genome-id (:genome/id (:current-generation (ex-data e)))))))
    (testing "non-strict mode reports instead of throwing"
      (let [r (recovery/startup-integrity-scan db root {:strict? false})]
        (is (false? (:ok? r)))
        (is (= :missing (:status (:current-generation r))))))))

(deftest strict-mode-fails-closed-on-corrupted-current-genome
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id)]
    (tamper-artifact! root genome-id)
    (let [e (scan-error #(recovery/startup-integrity-scan db root))]
      (is (some? e))
      (is (= :store/integrity-failure (:error/type (ex-data e))))
      (is (= :corrupt (:status (:current-generation (ex-data e))))))))

(deftest generations-without-a-current-row-violate-invariant-6
  (let [db (fresh-db)
        root (temp-root)
        genome-id (put-genome! root)
        _ (seed-generation! db genome-id 0)]
    (testing "generations exist but no CURRENT row: reported, not thrown in non-strict"
      (let [r (recovery/startup-integrity-scan db root {:strict? false})]
        (is (false? (:ok? r)))
        (is (= :missing-current (:status (:current-generation r))))))
    (testing "strict mode fails closed"
      (let [e (scan-error #(recovery/startup-integrity-scan db root))]
        (is (some? e))
        (is (= :store/integrity-failure (:error/type (ex-data e))))
        (is (= :missing-current (:status (:current-generation (ex-data e)))))))))

;; ============================================================================
;; Milestone 5 exit test — reopen the DB/CAS and reconstruct without memory
;; ============================================================================

(deftest milestone5-reopen-reconstructs-session-without-memory
  (let [db-file (temp-db-path)
        cas-dir (temp-root)
        db (sqlite/spec db-file)
        _ (migrate/migrate! db)
        genome-id (put-genome! cas-dir)
        _ (seed-generation! db genome-id)
        sid (:session/id (session/create-session! db (session-request genome-id)))
        ;; session created -> running
        created (event/append-event! db (base-event sid {:event/type :session/created}))
        _ (session/transition-session! db sid :created :resolving {})
        _ (session/transition-session! db sid :resolving :running {})
        started (event/append-event! db (base-event sid {:event/type :session/started
                                                         :cause/event-id (:event/id created)}))
        ;; intent -> provider call -> result
        request-id (:artifact/id (put! cas-dir "normalized request body"))
        proposed (event/append-event! db (base-event sid {:event/type :intent/proposed
                                                          :payload-ref request-id
                                                          :cause/event-id (:event/id started)}))
        normalized (event/append-event! db (base-event sid {:event/type :intent/normalized
                                                            :cause/event-id (:event/id proposed)}))
        authorized (event/append-event! db (base-event sid {:event/type :intent/authorized
                                                            :cause/event-id (:event/id normalized)}))
        call-started (event/append-event! db (base-event sid {:event/type :provider/call-started
                                                              :cause/event-id (:event/id authorized)}))
        result-id (:artifact/id (put! cas-dir "normalized result body"))
        call-completed (event/append-event! db (base-event sid {:event/type :provider/call-completed
                                                                :payload-ref result-id
                                                                :cause/event-id (:event/id call-started)}))
        intent-completed (event/append-event! db (base-event sid {:event/type :intent/completed
                                                                  :cause/event-id (:event/id call-completed)}))
        ;; session waits then completes
        waiting (event/append-event! db (base-event sid {:event/type :session/waiting
                                                         :cause/event-id (:event/id intent-completed)}))
        _ (session/transition-session! db sid :running :waiting {})
        _ (event/append-event! db (base-event sid {:event/type :session/completed
                                                   :cause/event-id (:event/id waiting)}))
        _ (session/transition-session! db sid :waiting :completed {})]
    ;; "terminate the process": drop every connection and any in-memory
    ;; state; reopen the SAME db file and CAS root from disk only.
    (let [db2 (sqlite/spec db-file)
          cas2 (cas/->cas cas-dir {:verify true})]
      (testing "the event chain verifies from the reopened database"
        (is (= {:valid? true :events 10} (event/verify-event-chain db2 sid))))
      (testing "the pinned session identity is reconstructed without memory"
        (let [s (session/get-session db2 sid)]
          (is (some? s))
          (is (= :completed (:state s)))
          (is (= [genome-id resolution phenotype gen]
                 [(:genome/id s) (:resolution/id s)
                  (:phenotype/id s) (:generation/id s)]))))
      (testing "the referenced payload artifacts are intact in the reopened CAS"
        (is (= (vec (txt "normalized request body")) (vec (cas/get-bytes cas2 request-id))))
        (is (= (vec (txt "normalized result body")) (vec (cas/get-bytes cas2 result-id)))))
      (testing "a recovery scan on the reopened store finds nothing to recover"
        (let [r (recovery/startup-integrity-scan db2 cas2)]
          (is (true? (:ok? r)))
          (is (empty? (:orphaned-sessions r)))
          (is (empty? (:missing-artifacts r)))
          (is (empty? (:invalid-event-chains r)))
          (is (empty? (:stale-candidates r))))))))
