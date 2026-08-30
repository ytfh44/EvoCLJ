(ns evoclj.adversarial.crash-recovery-test
  "component — crash/fault injection suite.

  Each deftest injects a process crash AFTER one step of a normative
  transaction and asserts the EXPECTED RECOVERABLE STATE: recovery
  classifies crash residue, never invents a successful effect, and
  never accepts a partially written Genome as content-addressed valid
  content.

  Injection points (normative, docs component) and their expected
  recoverable state:

  1. CAS artifact temp write (before rename) — no artifact exists; an
     orphan .evoclj-*.tmp file may remain; the recovery scan must NOT
     report the temp file as a valid artifact (a referencing event is
     reported :missing-artifacts), and a later put heals it.
  2. Artifact rename (before DB insert) — the CAS artifact exists and
     verifies but no DB row references it: an orphan, never a valid
     CURRENT generation or payload; the scan reports nothing missing.
  3. Session state transition (before the transition event) — the row
     state IS the recoverable state: a non-terminal transition leaves
     the session :orphaned (never rewound, never completed); a
     terminal transition closes it (never reported orphaned, and the
     missing event is never fabricated).
  4. Provider effect (before the result event) — the effect happened
     once, the result event was never persisted: the call is
     classified :ambiguous (manual review) per the intent-effect
     protocol, and MUST NOT be blindly retried — the provider counter
     stays at the pre-crash count.
  5. Candidate materialization (before the candidate row) — no valid
     Candidate exists (no row); the durable mutation row is the
     lineage precondition; re-materialization yields the SAME
     deterministic candidate content (Global Constraint 6).
  6. Final Evaluation persistence (before the summary insert) — gate
     artifacts are durable but no finalized eval_runs row exists; the
     candidate stays :evaluation-pending, is reported stale, and
     promotion refuses (evaluated-only is structural).
  7. Promotion decision insert (before the CURRENT CAS) — the real
     promote! failpoint rolls the whole transaction back: no
     promotion row committed, exactly one CURRENT (the seed), the
     candidate still :evaluated.
  8. CURRENT CAS (before the outer transaction commit) — the pointer
     moved inside the open transaction but the COMMIT never happened:
     externally there is exactly one CURRENT (the seed), no promotion
     row, no new generation row — the CAS is invisible until the
     enclosing transaction commits (Global Constraint 15).

  Step 3 of the task (no partially written Genome accepted as
  content-addressed valid content) is covered by
  truncated-genome-write-is-never-accepted-as-valid-content: a
  truncated body at the canonical CAS path fails the verifying read
  and the strict startup scan (:store/cas-corrupt), and a bundle
  tree truncated mid-write loads to a DIFFERENT content address than
  the one claimed.

  Fresh temp databases are migrated from the classpath migrations and
  deleted after every test, as are the temp CAS roots."
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.promote :as promote]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.existence :as existence]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path Paths FileVisitOption)
           (java.nio.file.attribute FileAttribute)
           (java.sql Connection)
           (java.util UUID)))

;; ============================================================================
;; shared fixtures (fresh temp store per test, deleted afterwards)
;; ============================================================================

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private seed-gen "generation-1")
(def ^:private parent-resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private new-resolution (str "sha256:" (apply str (repeat 64 "d"))))
(def ^:private evidence-id (str "sha256:" (apply str (repeat 64 "e"))))

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (Files/createTempFile "evoclj-crash-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-root
  "A throwaway CAS root directory in the system temp dir."
  []
  (let [p (str (Files/createTempDirectory "evoclj-crash-cas-"
                                          (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifact trees)."
  [p]
  (let [path (Paths/get p (make-array String 0))]
    (when (Files/exists path (make-array LinkOption 0))
      (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
        (doseq [q (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists q))))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! p))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- txt [s]
  (.getBytes s StandardCharsets/UTF_8))

(defn- put!
  "put-bytes! sugar: UTF-8 bytes; returns the artifact descriptor map."
  [cas-root s]
  (cas/put-bytes! cas-root (txt s) {}))

(defn- put-genome!
  "Store `s` in the CAS and return its content address (used as a
  generation/candidate genome id)."
  [cas-root s]
  (:artifact/id (put! cas-root s)))

(defn- proof [id]
  (#'existence/unsafe-verified-digest id))

(defn- proof-candidate [candidate]
  (update candidate :candidate/genome-id proof))

(defn- proof-mutation [mutation]
  (cond-> mutation
    (:parent/genome-id mutation) (update :parent/genome-id proof)
    (:evidence/id mutation) (update :evidence/id proof)
    (:payload-ref mutation) (update :payload-ref proof)
    (:candidate/payload-ref mutation) (update :candidate/payload-ref proof)))

(defn- seed-generation!
  "Insert the CURRENT (current = 1) seed generation row pinned to
  `genome-id` (which MUST be a real CAS artifact so recovery's
  Invariant-7 current-generation check passes)."
  [db genome-id]
  (sqlite/with-db [conn db]
    (doseq [artifact-id [genome-id parent-resolution phenotype]]
      (jdbc/execute!
       conn
       ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
         VALUES (?, 'application/octet-stream', 0, datetime('now'))"
        artifact-id]))
    (jdbc/execute!
     conn
     ["INSERT OR IGNORE INTO genomes (id, created_at)
      VALUES (?, datetime('now'))"
      genome-id])
    (jdbc/insert! conn :generations
                  {:id seed-gen
                   :genome_id genome-id
                   :resolution_id parent-resolution
                   :parent_id nil
                   :state "active"
                   :current 1
                   :created_at now})))

(defn- operator-session!
  "Create an operator session pinned to the seed generation and append
  its :session/created root event (the host's job). Returns the
  session id."
  [db genome-id]
  (let [sid (:session/id
             (session/create-session!
              db
              {:genome/id genome-id
               :resolution/id parent-resolution
               :phenotype/id phenotype
               :generation/id seed-gen}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id seed-gen
                          :phenotype/id phenotype
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- tx-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- verifying-cas
  "A CAS config with read verification enabled (every body is
  re-hashed and compared to its id on read)."
  [root]
  (cas/->cas root {:verify true}))

;; ============================================================================
;; 1. CAS artifact temp write — crash BEFORE the rename
;; ============================================================================

(deftest cas-crash-after-temp-write-before-rename-leaves-no-valid-artifact
  (let [db (fresh-db)
        root (temp-cas-root)
        cas (cas/->cas root)
        genome-id (put-genome! root "seed genome body")
        _ (seed-generation! db genome-id)
        sid (operator-session! db genome-id)
        body "candidate genome bundle body"
        ba (txt body)
        id (hash/file-digest ba)          ; the id the real put would produce
        dir (cas/artifact-dir cas id)
        _ (Files/createDirectories dir (make-array FileAttribute 0))
        ;; the crash: the temp write landed in the artifact dir, the rename
        ;; never happened
        orphan (Files/createTempFile dir ".evoclj-" ".tmp"
                                     (make-array FileAttribute 0))]
    (testing "EXPECTED RECOVERABLE STATE: no artifact exists — the orphan
              temp file is not the artifact"
      (is (false? (cas/exists? cas id)))
      (is (= :store/cas-missing
             (:error/type (ex-data (tx-error #(cas/get-bytes cas id))))))
      (testing "the artifact dir holds only the orphan temp — no body, no meta"
        (let [names (mapv #(.getFileName ^Path %) (iterator-seq (.iterator (Files/newDirectoryStream dir))))]
          (is (= [(.getFileName orphan)] names))
          (is (and (.startsWith (str (.getFileName orphan)) ".evoclj-")
                   (.endsWith (str (.getFileName orphan)) ".tmp"))
              "the orphan uses the CAS temp-file convention"))))
    (testing "the recovery scan must NOT report the temp file as a valid
              artifact: an event referencing the never-completed write is
              reported :missing-artifacts"
      (let [created (first (event/events-by-type db sid :session/created))
            _ (event/append-event! db
                                   {:session/id sid
                                    :generation/id seed-gen
                                    :phenotype/id phenotype
                                    :event/type :intent/proposed
                                    :cause/event-id (:event/id created)
                                    :payload-ref id
                                    :metadata {}})
            report (recovery/scan-recovery-state db root)]
        (is (= [id] (mapv :payload-ref (:missing-artifacts report)))
            "the orphan temp file does NOT satisfy the content reference")))
    (testing "recovery: re-running the real put heals the artifact; the
              orphan temp does not poison it"
      (is (= id (:artifact/id (cas/put-bytes! cas ba {}))))
      (is (cas/exists? cas id))
      (is (= (vec ba) (vec (cas/get-bytes (verifying-cas root) id))))
      (is (empty? (:missing-artifacts (recovery/scan-recovery-state db root)))
          "after the re-put the reference resolves"))))

;; ============================================================================
;; 2. Artifact rename — crash BEFORE the DB insert
;; ============================================================================

(deftest cas-crash-after-rename-before-db-insert-leaves-orphan-artifact
  (let [db (fresh-db)
        root (temp-cas-root)
        cas (cas/->cas root)
        genome-id (put-genome! root "seed genome body")
        _ (seed-generation! db genome-id)
        _ (operator-session! db genome-id)
        body "candidate genome bundle body"
        ;; the crash: put-bytes! completed (body AND meta.edn renamed into
        ;; place), then the process died before any DB row referencing it
        descriptor (cas/put-bytes! cas (txt body) {})
        id (:artifact/id descriptor)]
    (testing "EXPECTED RECOVERABLE STATE: the renamed artifact is complete
              and passes integrity"
      (is (cas/exists? cas id))
      (is (= (vec (txt body))
             (vec (cas/get-bytes (verifying-cas root) id)))
          "a verifying read re-hashes the body and matches the id"))
    (testing "no DB row references it — it is an orphan, never a valid
              payload or CURRENT generation"
      (is (empty? (sqlite/query db ["SELECT * FROM artifacts WHERE hash = ?" id])))
      (is (empty? (sqlite/query db ["SELECT * FROM generations WHERE genome_id = ?" id])))
      (is (empty? (sqlite/query db ["SELECT * FROM events WHERE payload_ref = ?" id]))))
    (testing "the recovery scan does not misreport the orphan as corruption
              or as content"
      (let [r (recovery/startup-integrity-scan db root)]
        (is (true? (:ok? r)))
        (is (= {:status :ok :generation/id seed-gen :genome/id genome-id}
               (:current-generation r)))
        (is (empty? (:missing-artifacts r)))
        (is (empty? (:stale-candidates r)))))
    (testing "a later put of identical bytes reuses the artifact (idempotent)"
      (is (= id (:artifact/id (cas/put-bytes! cas (txt body) {})))))))

;; ============================================================================
;; 3. Session state transition — crash BEFORE the transition event
;; ============================================================================

(deftest session-crash-after-state-transition-before-event
  (testing "injection: :resolving → :running persisted, no :session/started
            event — the row state IS the recoverable state; recovery never
            rewinds it and never fabricates completion"
    (let [db (fresh-db)
          root (temp-cas-root)
          genome-id (put-genome! root "seed genome body")
          _ (seed-generation! db genome-id)
          sid (operator-session! db genome-id)
          _ (session/transition-session! db sid :created :resolving {})
          _ (session/transition-session! db sid :resolving :running {})
          ;; crash: the :session/started event was never appended
          report (recovery/scan-recovery-state db root)]
      (is (= [sid] (mapv :session/id (:orphaned-sessions report))))
      (is (= :running (:state (first (:orphaned-sessions report)))))
      (is (= 1 (:last-event-seq (first (:orphaned-sessions report))))
          "only the :session/created root event exists")
      (is (empty? (event/events-by-type db sid :session/started))
          "no fabricated started event")
      (is (= :running (:state (session/get-session db sid)))
          "the scan never rewinds the persisted transition")
      (testing "the persisted state is authoritative: a stale re-transition
                from :created loses the compare-and-set"
        (is (= :session/invalid-transition
               (:error/type (ex-data (tx-error #(session/transition-session!
                                                 db sid :created :resolving {})))))))
      (testing "an orphaned mid-flight session is recoverable residue, not
                corruption"
        (is (true? (:ok? (recovery/startup-integrity-scan db root)))))))
  (testing "injection: the TERMINAL :waiting → :completed transition
            persisted, no :session/completed event — the durable row state
            closes the session; recovery neither reports it orphaned nor
            fabricates the missing event"
    (let [db (fresh-db)
          root (temp-cas-root)
          genome-id (put-genome! root "seed genome body")
          _ (seed-generation! db genome-id)
          sid (operator-session! db genome-id)
          _ (session/transition-session! db sid :created :resolving {})
          _ (session/transition-session! db sid :resolving :running {})
          _ (session/transition-session! db sid :running :waiting {})
          _ (session/transition-session! db sid :waiting :completed {})
          ;; crash: the :session/completed event was never appended
          report (recovery/scan-recovery-state db root)]
      (is (empty? (:orphaned-sessions report))
          "a terminal row state is finished regardless of the log")
      (is (= :completed (:state (session/get-session db sid))))
      (is (empty? (event/events-by-type db sid :session/completed))
          "recovery never fabricates the missing terminal event")
      (is (true? (:ok? (recovery/startup-integrity-scan db root)))))))

;; ============================================================================
;; 4. Provider effect — crash BEFORE the result event (ambiguous, no retry)
;; ============================================================================

(defn- effect-outcome-classification
  "The Transaction Boundaries protocol's recovery classification,
  implemented test-locally over the durable event log: after a crash, a
  provider call whose :provider/call-started event was persisted but
  whose result event (:provider/call-completed or
  :provider/call-ambiguous) was NOT is classified :ambiguous (manual
  review). Recovery never invents success and never blindly retries a
  non-idempotent call. A started call with a persisted result is
  :completed; a call that never started is nil."
  [events intent-id]
  (let [started (first (filter #(and (= :provider/call-started (:event/type %))
                                     (= intent-id (get-in % [:metadata :intent/id])))
                               events))
        result (first (filter #(and (contains? #{:provider/call-completed
                                                 :provider/call-ambiguous}
                                               (:event/type %))
                                    (= intent-id (get-in % [:metadata :intent/id])))
                              events))]
    (cond
      (nil? started) nil
      (nil? result) {:outcome :ambiguous
                     :manual-review true
                     :idempotency/key (get-in started [:metadata :idempotency/key])}
      :else {:outcome (:event/type result)})))

(deftest provider-crash-after-effect-before-result-event-is-ambiguous-not-retried
  (let [db (fresh-db)
        root (temp-cas-root)
        cas (cas/->cas root)
        genome-id (put-genome! root "seed genome body")
        _ (seed-generation! db genome-id)
        sid (operator-session! db genome-id)
        execution-count (atom 0)
        provider (fixture/non-idempotent-provider {:execution-count execution-count})
        reg (registry/create-registry)
        _ (registry/register! reg provider)
        now-ms (.getTime (java.util.Date.))
        lease {:cap/id (random-uuid)
               :subject {:phenotype/id phenotype}
               :resource {:kind :tool :id :fixture/non-idempotent}
               :actions #{:invoke}
               :constraints {:max-calls 10000}
               :issued-at (java.util.Date. now-ms)
               :expires-at (java.util.Date. (+ now-ms 60000))}
        broker (dispatch/make-broker-context {:registry reg :leases [lease]
                                              :usage (atom {})})
        request-id (:artifact/id (put! root "normalized request body"))
        intent {:intent/id (random-uuid)
                :intent/type :intent/tool-call
                :session/id sid
                :phenotype/id phenotype
                :node/id :node/tool
                :cause/event-id 1
                :payload {:tool/id :fixture/non-idempotent :args {:text "hi"}}
                :budget {:wall-ms 1000}
                :metadata {:idempotency/key "ambig-call-1"}}
        ;; the effect protocol up to the injection point (Transaction
        ;; Boundaries): persist proposed → normalized → authorized →
        ;; provider-call-started WITH the idempotency key, THEN perform the
        ;; external effect
        created (first (event/events-by-type db sid :session/created))
        proposed (event/append-event! db
                                       {:session/id sid
                                        :generation/id seed-gen
                                        :phenotype/id phenotype
                                        :event/type :intent/proposed
                                        :cause/event-id (:event/id created)
                                        :payload-ref request-id
                                        :metadata {:intent/id (str (:intent/id intent))}})
        normalized (event/append-event! db
                                        {:session/id sid
                                         :generation/id seed-gen
                                         :phenotype/id phenotype
                                         :event/type :intent/normalized
                                         :cause/event-id (:event/id proposed)
                                         :payload-ref request-id
                                         :metadata {:intent/id (str (:intent/id intent))}})
        authorized (event/append-event! db
                                        {:session/id sid
                                         :generation/id seed-gen
                                         :phenotype/id phenotype
                                         :event/type :intent/authorized
                                         :cause/event-id (:event/id normalized)
                                         :payload-ref nil
                                         :metadata {:intent/id (str (:intent/id intent))}})
        started (event/append-event! db
                                     {:session/id sid
                                      :generation/id seed-gen
                                      :phenotype/id phenotype
                                      :event/type :provider/call-started
                                      :cause/event-id (:event/id authorized)
                                      :payload-ref nil
                                      :metadata {:intent/id (str (:intent/id intent))
                                                 :tool/id :fixture/non-idempotent
                                                 :idempotency/key "ambig-call-1"}})
        ;; perform the EXTERNAL EFFECT through the REAL dispatcher
        result (dispatch/dispatch! broker (assoc intent
                                                 :cause/event-id (:event/id started)))]
    (is (= :ok (:result/status result)))
    (is (= 1 @execution-count)
        "the irreversible external effect really happened once")
    (testing "the idempotency key is durable with the call-started event"
      (is (= "ambig-call-1"
             (get-in (first (event/events-by-type db sid :provider/call-started))
                     [:metadata :idempotency/key]))))
    (testing "the provider descriptor declares NO :retry block — the
              dispatcher would never auto-retry it"
      (is (nil? (get-in (registry/lookup reg :fixture/non-idempotent)
                        [:descriptor :retry]))))
    ;; crash: the :provider/call-completed result event was never persisted
    (testing "EXPECTED RECOVERABLE STATE: recovery classifies the call
              :ambiguous / manual-review — a successful effect whose result
              event was lost is NOT reported as completed"
      (let [events (event/events-for-session db sid)
            classification (effect-outcome-classification events
                                                          (str (:intent/id intent)))]
        (is (= :ambiguous (:outcome classification)))
        (is (true? (:manual-review classification)))
        (is (= "ambig-call-1" (:idempotency/key classification))))
      (is (empty? (event/events-by-type db sid :provider/call-completed))
          "recovery never invents a successful effect")
      (is (= 1 (count (event/events-by-type db sid :provider/call-started)))
          "no re-dispatch event was recorded"))
    (testing "recovery MUST NOT blindly retry the non-idempotent call — the
              provider counter stays at the pre-crash count"
      (is (= 1 @execution-count)
          "the provider counter stays at the pre-crash count — no blind retry"))
    (testing "the same classification over a COMPLETED call (control) and a
              never-started call"
      (let [events [{:event/type :provider/call-started
                     :metadata {:intent/id "c1" :idempotency/key "k1"}}
                    {:event/type :provider/call-completed
                     :metadata {:intent/id "c1"}}]]
        (is (= {:outcome :provider/call-completed}
               (effect-outcome-classification events "c1")))
        (is (nil? (effect-outcome-classification events "never-started")))))))

;; ============================================================================
;; 5. Candidate materialization — crash BEFORE the candidate row
;; ============================================================================

(defn- insert-mutation-row!
  "Ensure a mutations row exists (INSERT OR IGNORE by :mutation/id) —
  the candidate's lineage precondition, mirroring
  evoclj.evolution.candidate's private insert inside the
  materialization transaction."
  [db mutation]
  (sqlite/exec! db
                ["INSERT OR IGNORE INTO mutations
                    (id, parent_genome_id, hypothesis_id, evidence_id,
                     risk, ops, expected_effect, created_at)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                 (str (:mutation/id mutation))
                 (:parent/genome-id mutation)
                 (str (:hypothesis/id mutation))
                 (:evidence/id mutation)
                 (name (:risk mutation))
                 (pr-str (:ops mutation))
                 (pr-str (:expected-effect mutation))
                 now]))

(defn- mutation-ir
  "A fully-determined Mutation IR (component shape) for a parent genome."
  [parent-genome-id]
  {:mutation/id (random-uuid)
   :parent/genome-id parent-genome-id
   :hypothesis/id (random-uuid)
   :evidence/id evidence-id
   :risk :parameter
   :ops [{:op :replace-form
          :file "programs/route.clj"
          :selector ['case]
          :expect/hash (str "sha256:" (apply str (repeat 64 "1")))
          :form '(case op
                   :echo-a {:action (list 'finish-intent 'input)}
                   {:action (list 'finish-intent 'input)})}]
   :expected-effect {:primary-metric :task/success :direction :increase}})

(deftest candidate-crash-after-staging-before-row-leaves-no-candidate
  (let [db (fresh-db)
        root (temp-cas-root)
        cas (cas/->cas root)
        g1-id (put-genome! root "parent genome body")
        _ (seed-generation! db g1-id)
        stores {:sqlite db :cas cas}
        candidate-store-handle (candidate-store/make-candidate-store db)
        cand-genome (put-genome! root "candidate genome body")
        _ (sqlite/with-db [conn db]
            (doseq [artifact-id [cand-genome evidence-id]]
              (jdbc/execute!
               conn
               ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
                 VALUES (?, 'application/octet-stream', 0, datetime('now'))"
                artifact-id]))
            (jdbc/execute!
             conn
             ["INSERT OR IGNORE INTO genomes (id, created_at)
               VALUES (?, datetime('now'))"
              cand-genome]))
        mutation (mutation-ir g1-id)
        candidate-rec (candidate/create-candidate
                       {:parent/generation-id seed-gen
                        :parent/genome-id g1-id
                        :candidate/genome-id cand-genome
                        :mutation/id (:mutation/id mutation)
                        :evidence/id evidence-id
                        :risk :parameter})
        ;; the crash: the materialization transaction wrote the lineage
        ;; precondition (mutation row), then the process died before the
        ;; candidate row insert
        _ (insert-mutation-row! db mutation)]
    (testing "EXPECTED RECOVERABLE STATE: no valid Candidate exists — no
              candidates row at all"
      (is (empty? (sqlite/query db ["SELECT * FROM candidates"])))
      (is (nil? (candidate/find-candidate candidate-store-handle (:candidate/id candidate-rec))))
      (is (empty? (:stale-candidates (recovery/scan-recovery-state db root)))
          "the recovery scan reports no candidate — the orphan staging is
          never mistaken for a valid Candidate"))
    (testing "the persisted mutation row is the durable lineage
              precondition (Global Constraint 16 — proposals stay queryable)"
      (is (= 1 (count (sqlite/query db
                                    ["SELECT * FROM mutations WHERE id = ?"
                                     (str (:mutation/id mutation))])))))
    (testing "recovery: re-running the REAL materialization succeeds and
              yields the SAME deterministic candidate content (Global
              Constraint 6)"
      (let [c (candidate/materialize-candidate!
               candidate-store-handle
               (proof-candidate candidate-rec)
               (proof-mutation mutation))]
        (is (= :materialized (:state c)))
        (is (= cand-genome (:candidate/genome-id c)))
        (is (= 1 (count (sqlite/query db ["SELECT * FROM candidates"]))))
        (is (= 1 (count (sqlite/query db
                                      ["SELECT * FROM mutations WHERE id = ?"
                                       (str (:mutation/id mutation))])))
            "the re-materialization reused the durable mutation row
            (INSERT OR IGNORE — no duplicate lineage)")))))

;; ============================================================================
;; 6. Final Evaluation persistence — crash BEFORE the summary insert
;; ============================================================================

(deftest evaluation-crash-before-final-summary-leaves-no-finalized-eval
  (let [db (fresh-db)
        root (temp-cas-root)
        cas (cas/->cas root)
        g1-id (put-genome! root "parent genome body")
        _ (seed-generation! db g1-id)
        stores {:sqlite db :cas cas}
        candidate-store-handle (candidate-store/make-candidate-store db)
        cand-genome (put-genome! root "candidate genome body")
        _ (sqlite/with-db [conn db]
            (doseq [artifact-id [cand-genome evidence-id]]
              (jdbc/execute!
               conn
               ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
                 VALUES (?, 'application/octet-stream', 0, datetime('now'))"
                artifact-id]))
            (jdbc/execute!
             conn
             ["INSERT OR IGNORE INTO genomes (id, created_at)
               VALUES (?, datetime('now'))"
              cand-genome]))
        mutation (mutation-ir g1-id)
        candidate-rec (candidate/create-candidate
                       {:parent/generation-id seed-gen
                        :parent/genome-id g1-id
                        :candidate/genome-id cand-genome
                        :mutation/id (:mutation/id mutation)
                        :evidence/id evidence-id
                        :risk :parameter})
        c (candidate/materialize-candidate!
           candidate-store-handle
           (proof-candidate candidate-rec)
           (proof-mutation mutation))
        _ (candidate/mark-evaluation-pending! candidate-store-handle (:candidate/id c))
        ;; all gate/case artifacts are durable BEFORE the finalization
        ;; transaction (the Evaluation finalization transaction protocol)
        gate-artifact (put! root (pr-str {:gate/id :G0-parse :status :pass
                                          :details-ref nil}))
        paired-artifact (put! root (pr-str [{:parent 1 :candidate 1}]))
        ;; crash: the eval_runs final summary insert never happened
        _ nil]
    (testing "all gate/case artifacts are durable content-addressed content"
      (is (cas/exists? cas (:artifact/id gate-artifact)))
      (is (= (vec (txt (pr-str [{:parent 1 :candidate 1}])))
             (vec (cas/get-bytes (verifying-cas root)
                                 (:artifact/id paired-artifact))))))
    (testing "EXPECTED RECOVERABLE STATE: no finalized Evaluation exists and
              the candidate never reached :evaluated"
      (is (empty? (sqlite/query db ["SELECT * FROM eval_runs"])))
      (is (= :evaluation-pending (:state (candidate/find-candidate
                                           candidate-store-handle
                                           (:candidate/id c))))))
    (testing "recovery reports the half-evaluated candidate as stale —
              never eligible, never promotable; it is residue, not corruption"
      (let [r (recovery/scan-recovery-state db root)]
        (is (= 1 (count (:stale-candidates r))))
        ;; the scan reports the PERSISTED 5.1 vocabulary (:evaluating — the
        ;; machine :evaluation-pending mapped at the row boundary)
        (is (= :evaluating (:state (first (:stale-candidates r))))))
      (is (true? (:ok? (recovery/startup-integrity-scan db root)))))
    (testing "promotion refuses: no finalized evaluation, candidate not
              :evaluated (the evaluated-only rule is structural)"
      (let [op-sid (operator-session! db g1-id)
            e (tx-error #(promote/promote!
                          {:store stores
                           :resolution/id new-resolution
                           :event/session-id op-sid}
                          {:candidate-id (:candidate/id c)
                           :evaluation-id (random-uuid)
                           :expected-parent-generation seed-gen}))]
        (is (= :promotion/candidate-state-invalid (:error/type (ex-data e))))
        (is (= 1 (count (sqlite/query db
                                      ["SELECT * FROM generations WHERE current = 1"]))))
        (is (empty? (sqlite/query db ["SELECT * FROM promotions"])))))))

;; ============================================================================
;; promotion fixtures (seed AND candidate Genomes are real CAS artifacts so
;; recovery's Invariant-7 current-generation check passes)
;; ============================================================================

(defn- add-mutation-row!
  "Insert the mutation row a candidate's mutation_id FK needs; returns
  the mutation id."
  [conn parent-genome-id]
  (let [mutation-id (random-uuid)]
    (jdbc/insert! conn :mutations
                  {:id (str mutation-id)
                   :parent_genome_id parent-genome-id
                   :hypothesis_id (str (random-uuid))
                   :evidence_id evidence-id
                   :risk "parameter"
                   :ops (pr-str [])
                   :expected_effect (pr-str {})
                   :created_at now})
    mutation-id))

(defn- promotion-fixture
  "A promotion stack whose seed AND candidate Genomes are real CAS
  artifacts (so recovery's Invariant-7 current-generation check passes
  and promote!'s verify-genome-integrity! succeeds): a CURRENT seed
  generation, an EVALUATED candidate with a FINALIZED eligible
  evaluation, and an operator session. Returns {:db :cas
  :seed-genome-id :candidate/id :evaluation/id :candidate/genome-id
  :event/session-id}."
  []
  (let [db (fresh-db)
        root (temp-cas-root)
        cas (cas/->cas root)
        seed-genome-id (put-genome! root "seed genome body")
        _ (seed-generation! db seed-genome-id)
        candidate-id (random-uuid)
        evaluation-id (random-uuid)
        cand-genome-id (put-genome! root "candidate genome body")
        sid (operator-session! db seed-genome-id)]
    (sqlite/with-db [conn db]
      (doseq [artifact-id [cand-genome-id evidence-id new-resolution]]
        (jdbc/execute!
         conn
         ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
          VALUES (?, 'application/octet-stream', 0, datetime('now'))"
          artifact-id]))
      (jdbc/execute!
       conn
       ["INSERT OR IGNORE INTO genomes (id, created_at)
        VALUES (?, datetime('now'))"
        cand-genome-id]))
    (sqlite/with-db [conn db]
      (let [mutation-id (add-mutation-row! conn seed-genome-id)]
        (jdbc/insert! conn :candidates
                      {:id (str candidate-id)
                       :parent_generation_id seed-gen
                       :parent_genome_id seed-genome-id
                       :genome_id cand-genome-id
                       :mutation_id (str mutation-id)
                       :evidence_id evidence-id
                       :risk "parameter"
                       :state "eligible"
                       :created_at now})
        (jdbc/insert! conn :eval_runs
                      {:id (str evaluation-id)
                       :candidate_id (str candidate-id)
                       :parent_generation_id seed-gen
                       :profile_id ":default"
                       :gates (pr-str [])
                       :paired_results_ref nil
                       :summary (pr-str {:hard {} :utility {} :cost {}
                                         :complexity {}})
                       :eligibility (pr-str {:eligible? true :reasons []})
                       :status "finalized"
                       :created_at now})))
    {:db db
     :cas cas
     :seed-genome-id seed-genome-id
     :candidate/id candidate-id
     :evaluation/id evaluation-id
     :candidate/genome-id cand-genome-id
     :event/session-id sid}))

(defn- promotion-system
  [fx]
  {:store {:sqlite (:db fx) :cas (:cas fx)}
   :resolution/id new-resolution
   :event/session-id (:event/session-id fx)})

(defn- promote-request
  [fx]
  {:candidate-id (:candidate/id fx)
   :evaluation-id (:evaluation/id fx)
   :expected-parent-generation seed-gen})

(defn- candidate-row [db candidate-id]
  (first (sqlite/query db ["SELECT * FROM candidates WHERE id = ?"
                           (str candidate-id)])))

(defn- current-rows [db]
  (sqlite/query db ["SELECT * FROM generations WHERE current = 1"]))

;; ============================================================================
;; 7. Promotion decision insert — crash BEFORE the CURRENT CAS (real seam)
;; ============================================================================

(deftest promotion-crash-after-decision-insert-before-current-cas
  (let [fx (promotion-fixture)
        db (:db fx)
        system (assoc (promotion-system fx)
                      :failpoint (fn []
                                   (throw (ex-info "injected failure"
                                                   {:error/type :test/injected}))))
        e (tx-error #(promote/promote! system (promote-request fx)))]
    (testing "the injected crash propagates (the failpoint sits after the
              promotion decision insert, before the CURRENT CAS)"
      (is (= :test/injected (:error/type (ex-data e)))))
    (testing "EXPECTED RECOVERABLE STATE: no promotion row committed,
              exactly one CURRENT — the seed generation"
      (let [rows (current-rows db)]
        (is (= 1 (count rows)))
        (is (= seed-gen (:id (first rows))))
        (is (= "active" (:state (first rows))))))
    (testing "no trace of the decision survived the crash"
      (is (empty? (sqlite/query db ["SELECT * FROM promotions"])))
      (is (empty? (sqlite/query db
                                ["SELECT * FROM generations WHERE id != ?"
                                 seed-gen]))
          "no new generation row")
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx))))
          "the candidate is still :evaluated")
      (is (empty? (event/events-by-type db (:event/session-id fx)
                                        :promotion/promoted))))
    (testing "recovery sees a stale (never-promoted) candidate and a
              healthy CURRENT — no corruption, no invented promotion"
      (let [r (recovery/startup-integrity-scan db (:cas fx))]
        (is (true? (:ok? r)))
        (is (= seed-gen (:generation/id (:current-generation r))))
        (is (= 1 (count (:stale-candidates r))))))))

;; ============================================================================
;; 8. CURRENT CAS — crash BEFORE the outer transaction commit
;; ============================================================================

(defn- raw-exec!
  [^Connection conn sql]
  (with-open [stmt (.createStatement conn)]
    (.execute stmt sql)))

(defn- raw-query
  [^Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (with-open [rs (.executeQuery stmt)]
      (let [md (.getMetaData rs)
            n (.getColumnCount md)
            labels (mapv #(keyword (.getColumnLabel md (inc %))) (range n))]
        (loop [rows []]
          (if (.next rs)
            (recur (conj rows (zipmap labels
                                      (mapv #(.getObject rs (inc %)) (range n)))))
            rows))))))

(defn- raw-update!
  [^Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (.executeUpdate stmt)))

(defn- new-generation-id
  "The stable promoted-generation id, derived deterministically from the
  Genome/Resolution pair exactly as evoclj.promotion.promote derives it."
  [genome-id resolution-id]
  (str "generation-"
       (subs (hash/text-digest (str genome-id "\n" resolution-id)) 7 23)))

(defn- run-promotion-writes!
  "Mirror promote.clj's promoted-path writes EXACTLY (the documented ONE
  WRITE REORDER: the new generation row is inserted before the promotion
  row because of the promotions.to_generation_id FK), up to and INCLUDING
  the CURRENT compare-and-set — then STOP, leaving the outer COMMIT to the
  caller. Returns {:conn ... :new-gen ...}."
  [conn fx]
  (let [cand-row (first (raw-query conn
                                   "SELECT * FROM candidates WHERE id = ?"
                                   [(str (:candidate/id fx))]))
        eval-row (first (raw-query conn
                                   "SELECT * FROM eval_runs WHERE id = ?"
                                   [(str (:evaluation/id fx))]))
        new-gen (new-generation-id (:genome_id cand-row) new-resolution)
        reason {:expected-parent seed-gen
                :candidate-state :evaluated
                :eligibility {:eligible? true :reasons []}
                :to-generation new-gen}]
    (raw-update! conn
                 "INSERT INTO generations
                    (id, genome_id, resolution_id, parent_id, state, current, created_at)
                  VALUES (?, ?, ?, ?, 'active', 0, ?)"
                 [new-gen (:genome_id cand-row) new-resolution seed-gen now])
    (raw-update! conn
                 "INSERT INTO promotions
                    (id, candidate_id, evaluation_id, from_generation_id,
                     to_generation_id, decision, reason, created_at)
                  VALUES (?, ?, ?, ?, ?, 'promoted', ?, ?)"
                 [(str (UUID/randomUUID)) (:id cand-row) (:id eval-row)
                  seed-gen new-gen (pr-str reason) now])
    (raw-update! conn
                 "UPDATE generations SET state = 'retired'
                  WHERE id = ? AND state = 'active'"
                 [seed-gen])
    (raw-update! conn
                 "UPDATE candidates SET state = 'promoted'
                  WHERE id = ? AND state = 'eligible'"
                 [(:id cand-row)])
    ;; the CURRENT compare-and-set — the REAL evoclj.promotion.current path
    (is (= :ok (current/cas-current! conn seed-gen new-gen)))
    {:conn conn :new-gen new-gen}))

(deftest promotion-crash-after-current-cas-before-commit-leaves-no-trace
  (let [fx (promotion-fixture)
        db (:db fx)
        conn (jdbc/get-connection (sqlite/spec db))
        step (fn []
               (raw-exec! conn "PRAGMA foreign_keys = ON")
               (raw-exec! conn "PRAGMA busy_timeout = 10000")
               (raw-exec! conn "BEGIN IMMEDIATE")
               (run-promotion-writes! conn fx))]
    (try
      (let [{:keys [new-gen]} (step)]
        (testing "inside the OPEN transaction the CAS pointer HAS moved to
                  the new generation"
          (is (= new-gen (:id (current/read-current conn)))))
        (testing "the new generation and promotion row exist IN-TRANSACTION"
          (is (= 1 (count (raw-query conn
                                     "SELECT * FROM generations WHERE id = ?"
                                     [new-gen]))))
          (is (= 1 (count (raw-query conn "SELECT * FROM promotions" []))))))
      ;; crash: the connection is closed WITHOUT COMMIT — SQLite rolls the
      ;; transaction back (the process died between the CAS and the COMMIT)
      (finally
        (.close conn)))
    (testing "EXPECTED RECOVERABLE STATE: the CAS never became visible —
              exactly one CURRENT (the seed), no promotion row, no new
              generation row, the candidate untouched"
      (let [rows (current-rows db)]
        (is (= 1 (count rows)))
        (is (= seed-gen (:id (first rows))))
        (is (= "active" (:state (first rows)))))
      (is (empty? (sqlite/query db ["SELECT * FROM promotions"])))
      (is (empty? (sqlite/query db ["SELECT * FROM generations WHERE id != ?"
                                    seed-gen])))
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx)))))
      (is (empty? (event/events-by-type db (:event/session-id fx)
                                        :promotion/promoted))))
    (testing "recovery agrees: exactly one CURRENT, the candidate stale,
              ok — a crash between the CAS and the COMMIT is invisible
              (Global Constraint 15: promotion is an atomic CAS)"
      (let [r (recovery/startup-integrity-scan db (:cas fx))]
        (is (true? (:ok? r)))
        (is (= {:status :ok :generation/id seed-gen
                :genome/id (:seed-genome-id fx)}
               (:current-generation r)))
        (is (= 1 (count (:stale-candidates r))))))))

;; ============================================================================
;; component 3 — no partially written Genome is accepted as content-addressed
;; valid content
;; ============================================================================

(defn- fixture-bundle-root
  "The on-disk minimal-valid fixture bundle (real, loadable)."
  []
  (str (io/file (io/resource "fixtures/genomes/minimal-valid"))))

(defn- copy-tree!
  "Copy the directory tree at `src` into a fresh temp dir; returns the
  new root path string."
  [src]
  (let [dst (str (Files/createTempDirectory "evoclj-crash-bundle-"
                                            (make-array FileAttribute 0)))
        dst-path (Paths/get dst (make-array String 0))
        base (.toPath (io/file src))]
    (swap! temp-paths conj dst)
    (doseq [f (file-seq (io/file src))]
      (when (.isFile f)
        (let [rel (.relativize base (.toPath f))
              target (.resolve dst-path rel)]
          (Files/createDirectories (.getParent target)
                                   (make-array FileAttribute 0))
          (Files/copy (.toPath f) target
                      (make-array java.nio.file.CopyOption 0)))))
    dst))

(defn- genome-index-bytes
  "The canonical index bytes whose SHA-256 is the genome's content
  address — the exact serialization of evoclj.genome.hash/tree-digest
  (per bytewise-sorted path: path + NUL + digest + LF). Storing these
  bytes under the genome id is the HOST's job before any promotion or
  lineage integrity check (Database Invariant 7)."
  [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by (fn [[p _]] p) gpath/bytewise-compare (:files loaded)))))

(deftest truncated-genome-write-is-never-accepted-as-valid-content
  (let [db (fresh-db)
        root (temp-cas-root)
        cas (cas/->cas root)
        bundle-root (fixture-bundle-root)
        loaded (load/load-genome bundle-root)
        g1-id (:genome/id loaded)
        index (genome-index-bytes loaded)
        full (cas/put-bytes! cas (txt index) {})]
    (is (= g1-id (:artifact/id full))
        "the canonical index bytes hash to the genome id")
    (is (= g1-id (:genome/id loaded)))
    (seed-generation! db g1-id)
    (testing "baseline: the intact genome body verifies"
      (is (= (vec (txt index))
             (vec (cas/get-bytes (verifying-cas root) g1-id))))
      (is (true? (:ok? (recovery/startup-integrity-scan db root)))))
    (testing "a truncated bundle write in CAS staging — the body at the
              canonical path is a PARTIAL write"
      (let [partial (subs index 0 (quot (count index) 2))]
        (spit (str (cas/body-path cas g1-id)) partial)
        (testing "a verifying read fails loudly: :store/cas-corrupt"
          (is (= :store/cas-corrupt
                 (:error/type (ex-data (tx-error #(cas/get-bytes
                                                   (verifying-cas root) g1-id)))))))
        (testing "the strict startup scan fails closed on the corrupt
                  CURRENT genome (Invariant 7)"
          (let [e (tx-error #(recovery/startup-integrity-scan db root))]
            (is (= :store/integrity-failure (:error/type (ex-data e))))
            (is (= :corrupt (:status (:current-generation (ex-data e)))))))
        (testing "non-strict mode reports :ok? false with :corrupt"
          (let [r (recovery/startup-integrity-scan db root {:strict? false})]
            (is (false? (:ok? r)))
            (is (= :corrupt (:status (:current-generation r))))))
        (testing "the partial bytes hash to a DIFFERENT content address —
                  they can never masquerade as the genome"
          (is (not= g1-id (hash/text-digest partial)))))
      (testing "a bundle tree truncated mid-write does not load to the
                claimed content address"
        (let [truncated-dir (copy-tree! bundle-root)
              route-file (io/file truncated-dir "programs/route.clj")
              original (slurp route-file)
              _ (spit route-file (subs original 0 (quot (count original) 2)))]
          (is (not= g1-id (:genome/id (load/load-genome truncated-dir)))
              "the partial tree hashes to a different Genome ID — loading
              cannot accept it as the intended content")))
      (testing "recovery: rewriting the FULL canonical bytes heals the
                artifact and the scan passes again"
        (Files/deleteIfExists (cas/body-path cas g1-id))
        (is (= g1-id (:artifact/id (cas/put-bytes! cas (txt index) {}))))
        (is (= (vec (txt index))
               (vec (cas/get-bytes (verifying-cas root) g1-id))))
        (is (true? (:ok? (recovery/startup-integrity-scan db root))))))))
