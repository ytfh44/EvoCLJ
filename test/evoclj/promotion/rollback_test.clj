(ns evoclj.promotion.rollback-test
  "component tests: explicit rollback semantics.

  rollback! is selection-only (Global Constraint 18): it changes ONLY
  which generation is chosen for FUTURE sessions. It moves the CURRENT
  pointer back through the component CAS machinery
  (evoclj.promotion.current/cas-current! — the ONLY code path that
  changes CURRENT), marks the rolled-back generation :rolled-back and
  reactivates the target :active. Nothing is deleted: G43 events,
  episodes, external-effect receipts, and promotion records stay
  queryable (Step 2). It refuses a target whose Genome fails CAS
  integrity verification with a typed error and no state change
  (Step 3), and it performs NO compensating external actions — by
  construction the rollback namespace requires no dispatch/provider/
  capability namespace (Step 4).

  The task's numbered steps:

  - Step 1: rollback! G43 → G42 restores CURRENT to G42 (:active),
    G43 becomes :rolled-back, exactly one current row remains, and
    nothing else changed (selection-only: no new promotion row, no
    candidate/evaluation/session rewrites). A stale rollback (from is
    no longer CURRENT) returns :stale and changes nothing.
  - Step 2: after the rollback, every G43-anchored artifact is still
    queryable — events, episodes, external-effect receipts
    (tool_calls/model_calls), promotion records, and the generation
    row itself.
  - Step 3: a rollback target whose Genome is absent from the CAS
    (:store/cas-missing) or whose body no longer re-hashes to its id
    (:store/cas-corrupt) is refused with a typed error; CURRENT, both
    generation states, and the event log are untouched.
  - Step 4: the rollback writes no new external-effect receipt, and
    the rollback namespace's :require set contains no dispatch,
    provider, capability, or runtime namespace (asserted by
    construction).

  Fixture: a real component promotion (promote!) makes G43 CURRENT over
  G42 ('retired'), a second operator session is created pinned to G43
  (the rollback event anchor), and G43-anchored episode/receipt rows
  are inserted before the rollback. Fresh temp databases are migrated
  from the classpath migrations and deleted after every test."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.promote :as promote]
            [evoclj.promotion.rollback :as rollback]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixtures -------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen42 "generation-42")
(def ^:private parent-resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private new-resolution (str "sha256:" (apply str (repeat 64 "d"))))

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (Files/createTempFile
                "evoclj-rollback-" ".db"
                (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-cas-root
  "A throwaway CAS root directory in the system temp dir."
  []
  (let [p (str (Files/createTempDirectory
                "evoclj-rollback-cas-"
                (make-array FileAttribute 0)))]
    (swap! cas-roots conj p)
    p))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifact
  bodies/meta files)."
  [p]
  (let [path (Paths/get p (make-array String 0))]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (with-open [stream (Files/walk path (make-array java.nio.file.FileVisitOption 0))]
        (doseq [q (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists q))))))

(defn- cleanup!
  "Delete every temp db file and CAS root created during this run."
  []
  (doseq [p @db-paths]
    (Files/deleteIfExists (Paths/get p (make-array String 0))))
  (doseq [p @cas-roots]
    (delete-tree! p))
  (reset! db-paths [])
  (reset! cas-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- seed-generation!
  "Insert the CURRENT (current = 1) seed generation row for `gen-id`
  carrying the REAL content hash `genome-id` (the rollback target's
  Genome must be CAS-resolvable — Step 3 verifies it)."
  [db gen-id genome-id]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :generations
                  {:id gen-id
                   :genome_id genome-id
                   :resolution_id parent-resolution
                   :parent_id nil
                   :state "active"
                   :current 1
                   :created_at now})))

(defn- add-mutation!
  "Insert the mutation row a candidate's mutation_id FK needs;
  returns the mutation id."
  [conn]
  (let [mutation-id (random-uuid)]
    (jdbc/insert! conn :mutations
                  {:id (str mutation-id)
                   :parent_genome_id (str "sha256:" (apply str (repeat 64 "e")))
                   :hypothesis_id (str (random-uuid))
                   :evidence_id (str "sha256:" (apply str (repeat 64 "f")))
                   :risk "parameter"
                   :ops (pr-str [])
                   :expected_effect (pr-str {})
                   :created_at now})
    mutation-id))

(defn- add-candidate!
  "Insert an EVALUATED (state 'eligible') candidate row for the given
  parent generation; returns the candidate id."
  [db candidate-id parent-generation-id parent-genome-id genome-id]
  (sqlite/with-db [conn db]
    (let [mutation-id (add-mutation! conn)]
      (jdbc/insert! conn :candidates
                    {:id (str candidate-id)
                     :parent_generation_id parent-generation-id
                     :parent_genome_id parent-genome-id
                     :genome_id genome-id
                     :mutation_id (str mutation-id)
                     :evidence_id (str "sha256:" (apply str (repeat 64 "f")))
                     :risk "parameter"
                     :state "eligible"
                     :created_at now})))
  candidate-id)

(defn- add-evaluation!
  "Insert a FINALIZED eval_runs row carrying `eligibility`; returns
  the evaluation id."
  [db evaluation-id candidate-id parent-generation-id eligibility]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :eval_runs
                  {:id (str evaluation-id)
                   :candidate_id (str candidate-id)
                   :parent_generation_id parent-generation-id
                   :profile_id ":default"
                   :gates (pr-str [])
                   :paired_results_ref nil
                   :summary (pr-str {:hard {} :utility {} :cost {} :complexity {}})
                   :eligibility (pr-str eligibility)
                   :status "finalized"
                   :created_at now}))
  evaluation-id)

(defn- operator-session!
  "Create an operator session pinned to `generation-id`/`genome-id`
  and append its :session/created root event. Returns the session id."
  [db generation-id genome-id]
  (let [sid (:session/id
             (session/create-session!
              db
              {:genome/id genome-id
               :resolution/id parent-resolution
               :phenotype/id phenotype
               :generation/id generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id generation-id
                          :phenotype/id phenotype
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- add-episode!
  "Insert a G43-anchored episodic-memory row (Step 2 fixture)."
  [db session-id generation-id genome-id resolution-id event-id]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :episodes
                  {:id (str (random-uuid))
                   :session_id (str session-id)
                   :generation_id generation-id
                   :genome_id genome-id
                   :resolution_id resolution-id
                   :task_ref (str "sha256:" (apply str (repeat 64 "7")))
                   :first_event_id event-id
                   :last_event_id event-id
                   :outcome (pr-str {:status :completed})
                   :usage (pr-str {:tokens 10})
                   :created_at now})))

(defn- add-tool-call!
  "Insert an external-effect receipt row (Step 2 fixture)."
  [db session-id event-id]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :tool_calls
                  {:id (str (random-uuid))
                   :session_id (str session-id)
                   :event_id event-id
                   :tool_id "tool-a"
                   :intent_id (str (random-uuid))
                   :request_ref nil
                   :response_ref nil
                   :outcome "completed"
                   :created_at now})))

(defn- add-model-call!
  "Insert a model external-effect receipt row (Step 2 fixture)."
  [db session-id event-id]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :model_calls
                  {:id (str (random-uuid))
                   :session_id (str session-id)
                   :event_id event-id
                   :model "fixture-model"
                   :request_ref nil
                   :response_ref nil
                   :input_tokens 5
                   :output_tokens 5
                   :total_cost 0.0
                   :outcome "completed"
                   :created_at now})))

(defn- rollback-fixture
  "Build the full stack: G42 is CURRENT (seed, 'active'), a real
  component promotion (promote!) makes G43 CURRENT over G42 ('retired'),
  a second operator session is created pinned to G43 (the rollback
  event anchor), and G43-anchored episode/receipt rows are inserted.
  Returns a map of every id the tests need."
  []
  (let [db (fresh-db)
        cas-root (temp-cas-root)
        cas (cas/->cas cas-root)
        seed-genome (:artifact/id
                     (cas/put-bytes! cas
                                     (.getBytes "seed genome body"
                                                StandardCharsets/UTF_8)
                                     {}))
        _ (seed-generation! db gen42 seed-genome)
        candidate-id (random-uuid)
        evaluation-id (random-uuid)
        candidate-genome (:artifact/id
                          (cas/put-bytes! cas
                                          (.getBytes "candidate genome body"
                                                     StandardCharsets/UTF_8)
                                          {}))
        sid1 (operator-session! db gen42 seed-genome)
        _ (add-candidate! db candidate-id gen42 seed-genome candidate-genome)
        _ (add-evaluation! db evaluation-id candidate-id gen42
                           {:eligible? true :reasons []})
        promotion-result (promote/promote!
                          {:store {:sqlite db :cas cas}
                           :resolution/id new-resolution
                           :event/session-id sid1}
                          {:candidate-id candidate-id
                           :evaluation-id evaluation-id
                           :expected-parent-generation gen42})
        g43 (:to promotion-result)
        sid2 (operator-session! db g43 candidate-genome)
        root-event-id (:event/id (first (event/events-for-session db sid2)))]
    (add-episode! db sid2 g43 candidate-genome new-resolution root-event-id)
    (add-tool-call! db sid2 root-event-id)
    (add-model-call! db sid2 root-event-id)
    {:db db
     :cas cas
     :generation/id gen42
     :to g43
     :target/genome-id seed-genome
     :event/session-id sid1
     :rollback/session-id sid2
     :resolution/id new-resolution}))

(defn- rollback-system
  "The promotion-system map for rollback! (the same contract promote!
  accepts — :resolution/id is part of the shared contract though
  rollback does not consume it)."
  [fx]
  {:store {:sqlite (:db fx) :cas (:cas fx)}
   :resolution/id (:resolution/id fx)
   :event/session-id (:rollback/session-id fx)})

(defn- rollback-request
  "A valid rollback! request: G43 (CURRENT) → G42, reason
  :canary-regression."
  [fx]
  {:from-generation (:to fx)
   :to-generation (:generation/id fx)
   :reason :canary-regression})

;; --- row helpers ------------------------------------------------------------

(defn- gen-row [db gen-id]
  (first (sqlite/query db ["SELECT * FROM generations WHERE id = ?" gen-id])))

(defn- session-row [db session-id]
  (first (sqlite/query db ["SELECT * FROM sessions WHERE id = ?"
                           (str session-id)])))

(defn- current-rows [db]
  (sqlite/query db ["SELECT * FROM generations WHERE current = 1"]))

(defn- promotion-rows [db]
  (sqlite/query db ["SELECT * FROM promotions"]))

(defn- tx-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- assert-state-unchanged!
  "Step 3 assertion: after a refused rollback, CURRENT, both generation
  states, the promotion rows, and the rollback event log are exactly
  as before."
  [db fx]
  (testing "CURRENT is still G43 (:active, current = 1), exactly one row"
    (let [rows (current-rows db)]
      (is (= 1 (count rows)))
      (is (= (:to fx) (:id (first rows))))
      (is (= "active" (:state (first rows))))))
  (testing "G42 is still :superseded ('retired'); G43 still :active"
    (is (= "retired" (:state (gen-row db (:generation/id fx)))))
    (is (= "active" (:state (gen-row db (:to fx))))))
  (testing "no rollback promotion row and no :promotion/rollback event"
    (is (= 1 (count (promotion-rows db))))
    (is (empty? (event/events-by-type db (:rollback/session-id fx)
                                      :promotion/rollback)))))

;; ============================================================================
;; Step 1 — rollback changes ONLY the generation chosen for future sessions
;; ============================================================================

(deftest step1-rollback-moves-current-back-and-updates-states
  (let [fx (rollback-fixture)
        db (:db fx)
        result (rollback/rollback! (rollback-system fx) (rollback-request fx))]
    (testing "the return contract"
      (is (= :rolled-back (:status result)))
      (is (= (:to fx) (:from result)))
      (is (= (:generation/id fx) (:to result))))
    (testing "CURRENT is back on G42, :active, exactly one current row (Invariant 6)"
      (let [rows (current-rows db)]
        (is (= 1 (count rows)))
        (is (= (:generation/id fx) (:id (first rows))))
        (is (= "active" (:state (first rows))))))
    (testing "G43 is :rolled-back and no longer current"
      (let [g43 (gen-row db (:to fx))]
        (is (= "rolled-back" (:state g43)))
        (is (= 0 (:current g43)))))
    (testing "G42 was reactivated :active with lineage intact"
      (let [g42 (gen-row db (:generation/id fx))]
        (is (= "active" (:state g42)))
        (is (= 1 (:current g42)))))
    (testing "selection-only: no new promotion row, no candidate/evaluation/session rewrites"
      (is (= 1 (count (promotion-rows db))))            ; only the :promoted row
      (is (= 2 (count (sqlite/query db ["SELECT id FROM generations"])))) ; G42 + G43
      (is (= 1 (count (sqlite/query db ["SELECT id FROM candidates"]))))
      (is (= 1 (count (sqlite/query db ["SELECT id FROM eval_runs"]))))
      (is (= 2 (count (sqlite/query db ["SELECT id FROM sessions"])))))
    (testing "the :promotion/rollback event is appended with the reason (Event Taxonomy)"
      (let [events (event/events-by-type db (:rollback/session-id fx)
                                         :promotion/rollback)]
        (is (= 1 (count events)))
        (is (= (:to fx) (get-in (first events) [:metadata :from])))
        (is (= (:generation/id fx) (get-in (first events) [:metadata :to])))
        (is (= :canary-regression (get-in (first events) [:metadata :reason])))))))

(deftest step1-stale-rollback-when-from-is-no-longer-current
  (let [fx (rollback-fixture)
        db (:db fx)
        r1 (rollback/rollback! (rollback-system fx) (rollback-request fx))
        r2 (rollback/rollback! (rollback-system fx) (rollback-request fx))]
    (testing "the first rollback succeeds; the second finds G43 no longer CURRENT"
      (is (= :rolled-back (:status r1)))
      (is (= :stale (:status r2)))
      (is (= (:generation/id fx) (:current r2)))
      (is (= (:to fx) (:expected r2))))
    (testing "the stale call changed nothing: CURRENT stays on G42, one event only"
      (is (= (:generation/id fx) (:id (current/current-generation db))))
      (is (= 1 (count (current-rows db))))
      (is (= 1 (count (event/events-by-type db (:rollback/session-id fx)
                                            :promotion/rollback)))))))

;; ============================================================================
;; Step 2 — all G43 data stays queryable (nothing deleted)
;; ============================================================================

(deftest step2-all-g43-data-remains-queryable
  (let [fx (rollback-fixture)
        db (:db fx)
        _ (rollback/rollback! (rollback-system fx) (rollback-request fx))
        sid2 (:rollback/session-id fx)]
    (testing "the promotion record that promoted G43 is still there (Invariant 5)"
      (let [rows (promotion-rows db)]
        (is (= 1 (count rows)))
        (is (= (:to fx) (:to_generation_id (first rows))))))
    (testing "G43 episodes remain queryable"
      (is (= 1 (count (sqlite/query db
                                    ["SELECT * FROM episodes WHERE generation_id = ?"
                                     (:to fx)])))))
    (testing "G43 external-effect receipts remain queryable"
      (is (= 1 (count (sqlite/query db
                                    ["SELECT * FROM tool_calls WHERE session_id = ?"
                                     (str sid2)]))))
      (is (= 1 (count (sqlite/query db
                                    ["SELECT * FROM model_calls WHERE session_id = ?"
                                     (str sid2)])))))
    (testing "the G43 session stays pinned and its event chain still verifies"
      (is (= (:to fx) (:generation_id (session-row db sid2))))
      (is (= "created" (:state (session-row db sid2))))
      (is (= 2 (count (event/events-for-session db sid2)))) ; root + rollback
      (is (true? (:valid? (event/verify-event-chain db sid2)))))
    (testing "the G43 generation row itself remains — selection-only, nothing deleted"
      (is (= "rolled-back" (:state (gen-row db (:to fx))))))))

;; ============================================================================
;; Step 3 — rollback refuses a target whose Genome fails integrity checks
;; ============================================================================

(deftest step3-rollback-refuses-missing-target-genome
  (let [fx (rollback-fixture)
        db (:db fx)
        _ (Files/deleteIfExists (cas/body-path (:cas fx) (:target/genome-id fx)))
        e (tx-error #(rollback/rollback! (rollback-system fx) (rollback-request fx)))]
    (is (= :store/cas-missing (:error/type (ex-data e))))
    (assert-state-unchanged! db fx)))

(deftest step3-rollback-refuses-corrupt-target-genome
  (let [fx (rollback-fixture)
        db (:db fx)
        _ (Files/write (cas/body-path (:cas fx) (:target/genome-id fx))
                       (.getBytes "corrupted body" StandardCharsets/UTF_8)
                       (make-array java.nio.file.OpenOption 0))
        e (tx-error #(rollback/rollback! (rollback-system fx) (rollback-request fx)))]
    (is (= :store/cas-corrupt (:error/type (ex-data e))))
    (assert-state-unchanged! db fx)))

;; ============================================================================
;; Step 4 — no compensating external actions, by construction
;; ============================================================================

(deftest step4-rollback-writes-no-external-effect-and-requires-no-dispatch-ns
  (let [fx (rollback-fixture)
        db (:db fx)
        counts (fn []
                 [(count (sqlite/query db ["SELECT id FROM tool_calls"]))
                  (count (sqlite/query db ["SELECT id FROM model_calls"]))
                  (count (sqlite/query db ["SELECT id FROM capability_leases"]))])
        before (counts)
        _ (rollback/rollback! (rollback-system fx) (rollback-request fx))
        after (counts)]
    (testing "no new external-effect receipt is written by the rollback"
      (is (= before after)))
    (testing "by construction: the rollback namespace requires no dispatch/provider/capability/runtime namespace"
      (let [aliases (set (keys (ns-aliases 'evoclj.promotion.rollback)))
            forbidden #{'evoclj.intent.dispatch
                        'evoclj.provider.registry
                        'evoclj.provider.protocol
                        'evoclj.capability.broker
                        'evoclj.runtime.executor
                        'evoclj.runtime.system}]
        (is (empty? (clojure.set/intersection aliases forbidden)))))))

;; ============================================================================
;; Boundary: input validation and typed error vocabulary
;; ============================================================================

(deftest rollback-request-and-system-are-validated
  (let [fx (rollback-fixture)
        invalid-request? (fn [req]
                           (= :promotion/invalid
                              (-> (tx-error #(rollback/rollback! (rollback-system fx) req))
                                  ex-data :error/type)))]
    (testing "unknown request keys are rejected (closed trust boundary)"
      (is (invalid-request? (assoc (rollback-request fx) :bogus 1))))
    (testing "missing from-generation is rejected"
      (is (invalid-request? (dissoc (rollback-request fx) :from-generation))))
    (testing "missing to-generation is rejected"
      (is (invalid-request? (dissoc (rollback-request fx) :to-generation))))
    (testing "a non-keyword reason is rejected"
      (is (invalid-request? (assoc (rollback-request fx) :reason "canary-regression"))))
    (testing "a system without a store is rejected"
      (is (= :promotion/system-invalid
             (-> (tx-error #(rollback/rollback! (dissoc (rollback-system fx) :store)
                                                (rollback-request fx)))
                 ex-data :error/type))))))

(deftest rollback-rejects-unknown-or-invalid-targets
  (testing "rolling back to a nonexistent generation fails before any write"
    (let [fx (rollback-fixture)
          db (:db fx)
          e (tx-error #(rollback/rollback! (rollback-system fx)
                                           (assoc (rollback-request fx)
                                                  :to-generation "generation-999")))]
      (is (= :promotion/generation-not-found (:error/type (ex-data e))))
      (assert-state-unchanged! db fx)))
  (testing "rolling back a generation to itself is rejected"
    (let [fx (rollback-fixture)
          db (:db fx)
          e (tx-error #(rollback/rollback! (rollback-system fx)
                                           (assoc (rollback-request fx)
                                                  :to-generation (:to fx))))]
      (is (= :promotion/rollback-invalid (:error/type (ex-data e))))
      (assert-state-unchanged! db fx)))
  (testing "rolling back to a target that is not :superseded is rejected"
    (let [fx (rollback-fixture)
          db (:db fx)
          _ (rollback/rollback! (rollback-system fx) (rollback-request fx))
          ;; now G43 is :rolled-back; try to roll back FROM G42 TO G43
          e (tx-error #(rollback/rollback! (rollback-system fx)
                                           {:from-generation (:generation/id fx)
                                            :to-generation (:to fx)
                                            :reason :operator-error}))]
      (is (= :promotion/rollback-target-invalid (:error/type (ex-data e))))
      (is (= (:generation/id fx) (:id (current/current-generation db)))))))
