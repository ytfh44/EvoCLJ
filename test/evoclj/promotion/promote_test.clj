(ns evoclj.promotion.promote-test
  "component tests: the atomic CURRENT compare-and-set promotion.

  promote! is the ONLY code path that changes the generations CURRENT
  pointer (evoclj.promotion.current/cas-current!): a promotion reads
  the candidate and its FINALIZED evaluation, consumes the stored
  :eligibility judgment verbatim (never recomputed — Step 4), compares
  CURRENT against the candidate's parent generation, and either moves
  the pointer in one BEGIN IMMEDIATE transaction or reports :stale.

  The task's numbered steps:

  - Step 1: happy path — promote! moves CURRENT G42 → G43, the old
    generation becomes :superseded (persisted 'retired', the component
    vocabulary for the 9.1 machine state), the new generation row is
    born :active with current = 1 and full lineage, the candidate
    becomes :promoted, a promotions row (Invariant 5) and a
    :promotion/promoted event exist.
  - Step 2: concurrency — C1 and C2 share parent G42; after C1
    promotes, C2's promote! returns :stale and CURRENT is untouched.
  - Step 3: failure injection — a mid-transaction throw (or
    pre-corrupted state) rolls back to EXACTLY ONE active CURRENT
    generation; a missing/non-finalized/mismatched/ineligible
    evaluation and a missing CAS genome all fail before any write.
  - Step 4: the eligibility decision comes from the evaluation row's
    stored :eligibility {:eligible? true} — flipping the stored
    judgment makes the identical request fail closed.

  Fresh temp databases are migrated from the classpath migrations and
  deleted after every test; each fixture candidate gets its own CAS
  genome body, finalized evaluation, and operator session."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.promote :as promote]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent CountDownLatch)))

;; --- shared fixtures -------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private seed-gen "generation-1")
(def ^:private retired-gen "generation-0")
(def ^:private parent-genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private parent-resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private new-resolution (str "sha256:" (apply str (repeat 64 "d"))))

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (Files/createTempFile
                "evoclj-promote-" ".db"
                (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-cas-root
  "A throwaway CAS root directory in the system temp dir."
  []
  (let [p (str (Files/createTempDirectory
                "evoclj-promote-cas-"
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
  "Insert the CURRENT (current = 1) seed generation row."
  [db]
  (sqlite/with-db [conn db]
    ;; Fleet P5/F + 011: FK targets for generations and sessions (artifacts/genomes before generations/sessions)
    (try (jdbc/insert! conn :artifacts {:hash parent-genome :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash parent-resolution :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash phenotype :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
    (jdbc/insert! conn :generations
                  {:id seed-gen
                   :genome_id parent-genome
                   :resolution_id parent-resolution
                   :parent_id nil
                   :state "active"
                   :current 1
                   :created_at now})))

(defn- add-retired-generation!
  "Insert a non-current generation row whose (id, genome_id) can back a
  candidate's composite parent FK (Database Invariant 8 fixtures)."
  [db]
  (sqlite/with-db [conn db]
    (try (jdbc/insert! conn :artifacts {:hash parent-genome :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash parent-resolution :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash phenotype :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
    (jdbc/insert! conn :generations
                  {:id retired-gen
                   :genome_id parent-genome
                   :resolution_id parent-resolution
                   :parent_id nil
                   :state "retired"
                   :current 0
                   :created_at now})))

(defn- add-mutation!
  "Insert the mutation row a candidate's mutation_id FK needs;
  returns the mutation id."
  [conn]
  (let [mutation-id (random-uuid)
        eid (str "sha256:" (apply str (repeat 64 "e")))]
    ;; P5/F: ensure artifact for evidence and genome
    (try (jdbc/insert! conn :artifacts {:hash parent-genome :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :artifacts {:hash eid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
    (jdbc/insert! conn :mutations
                  {:id (str mutation-id)
                   :parent_genome_id parent-genome
                   :hypothesis_id (str (random-uuid))
                   :evidence_id eid
                   :risk "parameter"
                   :ops (pr-str [])
                   :expected_effect (pr-str {})
                   :created_at now})
    mutation-id))

(defn- add-candidate!
  "Insert an EVALUATED (state 'eligible') candidate row for the given
  parent generation; returns the candidate id."
  [db candidate-id parent-generation-id genome-id]
  (sqlite/with-db [conn db]
    (let [mutation-id (add-mutation! conn)
          eid (str "sha256:" (apply str (repeat 64 "e")))]
      ;; P5/F: ensure FK targets for candidate
      (try (jdbc/insert! conn :artifacts {:hash genome-id :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :artifacts {:hash eid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :genomes {:id genome-id :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :genomes {:id parent-genome :created_at now}) (catch Exception _ nil))
      (jdbc/insert! conn :candidates
                    {:id (str candidate-id)
                     :parent_generation_id parent-generation-id
                     :parent_genome_id parent-genome
                     :genome_id genome-id
                     :mutation_id (str mutation-id)
                     :evidence_id eid
                     :risk "parameter"
                     :state "eligible"
                     :created_at now})))
  candidate-id)

(defn- add-evaluation!
  "Insert a FINALIZED eval_runs row carrying `eligibility` (the stored
  judgment promote! must consume verbatim); returns the evaluation id."
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
  "Create an operator session pinned to the seed generation and append
  its :session/created root event (the host's job — promote! anchors
  the :promotion/* event to this session). Returns the session id."
  [db]
  (let [sid (:session/id
             (session/create-session!
              db
              {:genome/id parent-genome
               :resolution/id parent-resolution
               :phenotype/id phenotype
               :generation/id seed-gen}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id seed-gen
                          :phenotype/id phenotype
                          :event/type :session/created
                          :prev/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- promotion-fixture
  "Build the full promotion stack and return a map of every id the
  tests need. `opts` keys:

      :n-candidates     n        ; each candidate gets its own CAS genome,
                                  ; finalized evaluation, and operator session
      :eligibility      <map>    ; overrides the stored final judgment
      :parent-generation <id>    ; a RETIRED non-current generation the
                                  ; candidate is a child of (CAS-loser fixtures)

  The single-candidate convenience keys (:candidate/id :evaluation/id
  :candidate/genome-id :event/session-id) point at the first candidate."
  ([] (promotion-fixture {}))
  ([{:keys [n-candidates eligibility parent-generation genome-body]}]
   (let [db (fresh-db)
         cas-root (temp-cas-root)
         cas (cas/->cas cas-root)
         _ (seed-generation! db)
         _ (sqlite/with-db [conn db]
             (try (jdbc/insert! conn :artifacts {:hash new-resolution :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil)))
         _ (when (and parent-generation (not= parent-generation seed-gen))
             (add-retired-generation! db))
         parent-gen-id (or parent-generation seed-gen)
         elig (or eligibility {:eligible? true :reasons []})
         candidates (mapv
                     (fn [i]
                       (let [candidate-id (random-uuid)
                             evaluation-id (random-uuid)
                             genome-id (:artifact/id
                                        (cas/put-bytes!
                                         cas
                                         (.getBytes (or genome-body (str "candidate genome body " i))
                                                    StandardCharsets/UTF_8)
                                         {}))
                             sid (operator-session! db)]
                         (add-candidate! db candidate-id parent-gen-id genome-id)
                         (add-evaluation! db evaluation-id candidate-id parent-gen-id elig)
                         {:candidate/id candidate-id
                          :evaluation/id evaluation-id
                          :candidate/genome-id genome-id
                          :event/session-id sid}))
                     (range (or n-candidates 1)))
         first-c (first candidates)]
     {:db db
      :cas cas
      :generation/id seed-gen
      :resolution/id new-resolution
      :candidate/id (:candidate/id first-c)
      :evaluation/id (:evaluation/id first-c)
      :candidate/genome-id (:candidate/genome-id first-c)
      :event/session-id (:event/session-id first-c)
      :candidates candidates})))

(defn- promotion-system
  "The promotion-system map for the fixture's first candidate."
  [fx]
  {:store {:sqlite (:db fx) :cas (:cas fx)}
   :resolution/id (:resolution/id fx)
   :event/session-id (:event/session-id fx)})

(defn- system-for
  "The promotion-system map for one specific fixture candidate."
  [fx candidate]
  {:store {:sqlite (:db fx) :cas (:cas fx)}
   :resolution/id (:resolution/id fx)
   :event/session-id (:event/session-id candidate)})

(defn- promote-request
  "A valid promote! request for the fixture's first candidate."
  [fx]
  {:candidate-id (:candidate/id fx)
   :evaluation-id (:evaluation/id fx)
   :expected-parent-generation (:generation/id fx)})

;; --- row helpers ------------------------------------------------------------

(defn- candidate-row [db candidate-id]
  (first (sqlite/query db ["SELECT * FROM candidates WHERE id = ?" (str candidate-id)])))

(defn- gen-row [db gen-id]
  (first (sqlite/query db ["SELECT * FROM generations WHERE id = ?" gen-id])))

(defn- current-rows [db]
  (sqlite/query db ["SELECT * FROM generations WHERE current = 1"]))

(defn- promotion-rows [db]
  (sqlite/query db ["SELECT * FROM promotions"]))

(defn- tx-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- deref-unwrap
  "Deref a future, rethrowing the cause of an ExecutionException so
  worker failures surface with their real type."
  [f]
  (try @f
       (catch java.util.concurrent.ExecutionException e
         (throw (or (.getCause e) e)))))

;; ============================================================================
;; Step 1 — happy path: CURRENT moves G42 → G43 inside one transaction
;; ============================================================================

(deftest step1-happy-path-promotes-and-moves-current
  (let [fx (promotion-fixture)
        db (:db fx)
        result (promote/promote! (promotion-system fx) (promote-request fx))]
    (testing "the return contract"
      (is (= :promoted (:status result)))
      (is (= (:generation/id fx) (:from result)))
      (is (string? (:to result)))
      (is (not= (:generation/id fx) (:to result))))
    (testing "CURRENT moved from the seed generation to the new generation"
      (is (= (:to result) (:id (current/current-generation db))))
      (is (= "active" (:state (current/current-generation db))))
      (is (= 1 (count (current-rows db)))))
    (testing "the old generation is :superseded (persisted 'retired'), no longer current"
      (let [old (gen-row db (:generation/id fx))]
        (is (= "retired" (:state old)))
        (is (= 0 (:current old)))))
    (testing "the new generation row carries complete lineage (Invariant 7, 8; GC 17)"
      (let [row (gen-row db (:to result))]
        (is (= (:candidate/genome-id fx) (:genome_id row)))
        (is (= (:resolution/id fx) (:resolution_id row)))
        (is (= (:generation/id fx) (:parent_id row)))
        (is (= "active" (:state row)))
        (is (= 1 (:current row)))))
    (testing "the candidate is :promoted"
      (is (= "promoted" (:state (candidate-row db (:candidate/id fx))))))
    (testing "a promotions row references exactly this finalized evaluation (Invariant 5)"
      (let [rows (promotion-rows db)
            row (first rows)]
        (is (= 1 (count rows)))
        (is (= "promoted" (:decision row)))
        (is (= (str (:evaluation/id fx)) (:evaluation_id row)))
        (is (= (str (:candidate/id fx)) (:candidate_id row)))
        (is (= (:generation/id fx) (:from_generation_id row)))
        (is (= (:to result) (:to_generation_id row)))))
    (testing "the :promotion/promoted event exists (Event Taxonomy)"
      (let [events (event/events-by-type db (:event/session-id fx) :promotion/promoted)]
        (is (= 1 (count events)))
        (is (= (:to result) (get-in (first events) [:metadata :to])))
        (is (= (:generation/id fx) (get-in (first events) [:metadata :from])))))
    (testing "current.clj owns the ONLY code path that changes CURRENT"
      (let [publics (set (map name (keys (ns-publics 'evoclj.promotion.current))))]
        (is (contains? publics "cas-current!"))
        (is (empty? (filter #(re-matches #".*(?:insert!|delete!|remove!).*" %) publics)))))))

;; ============================================================================
;; Step 2 — concurrency: two candidates share G42; exactly one wins
;; ============================================================================

(deftest step2-two-candidates-share-parent-one-wins-one-stale
  (let [fx (promotion-fixture {:n-candidates 2})
        db (:db fx)
        [c1 c2] (:candidates fx)
        gate (CountDownLatch. 1)
        worker (fn [candidate]
                 (.await gate)
                 (promote/promote! (system-for fx candidate)
                                   {:candidate-id (:candidate/id candidate)
                                    :evaluation-id (:evaluation/id candidate)
                                    :expected-parent-generation (:generation/id fx)}))
        t1 (future (worker c1))
        t2 (future (worker c2))]
    (.countDown gate)
    (let [r1 (deref-unwrap t1)
          r2 (deref-unwrap t2)
          results [r1 r2]
          promoted (first (filter #(= :promoted (:status %)) results))
          stale (first (filter #(= :stale (:status %)) results))
          winner-c (if (= :promoted (:status r1)) c1 c2)
          loser-c (if (= :promoted (:status r1)) c2 c1)]
      (testing "exactly one promotion wins; the other sees :stale"
        (is (= #{:promoted :stale} (set (map :status results)))))
      (testing "the stale caller reports the winner's generation as CURRENT"
        (is (= (:to promoted) (:current stale)))
        (is (= (:generation/id fx) (:expected stale))))
      (testing "CURRENT is the winner's new generation — the loser changed nothing"
        (let [rows (current-rows db)]
          (is (= 1 (count rows)))
          (is (= (:to promoted) (:id (first rows))))
          (is (= "active" (:state (first rows))))))
      (testing "the winner's candidate is :promoted; the loser's is :stale (9.1)"
        (is (= "promoted" (:state (candidate-row db (:candidate/id winner-c)))))
        (is (= "stale" (:state (candidate-row db (:candidate/id loser-c))))))
      (testing "one :promotion/promoted event, one :promotion/stale event"
        (is (= 1 (count (event/events-by-type db (:event/session-id winner-c)
                                              :promotion/promoted))))
        (is (= 1 (count (event/events-by-type db (:event/session-id loser-c)
                                              :promotion/stale))))))))

;; ============================================================================
;; Step 3 — failure injection: rollback leaves EXACTLY ONE active CURRENT
;; ============================================================================

(deftest step3-mid-transaction-throw-rolls-back-everything
  (let [fx (promotion-fixture)
        db (:db fx)
        system (assoc (promotion-system fx)
                      :failpoint (fn [] (throw (ex-info "injected failure"
                                                        {:error/type :test/injected}))))
        e (tx-error #(promote/promote! system (promote-request fx)))]
    (testing "the injected failure propagates"
      (is (= :test/injected (:error/type (ex-data e)))))
    (testing "EXACTLY ONE active CURRENT generation remains — the seed"
      (let [rows (current-rows db)]
        (is (= 1 (count rows)))
        (is (= seed-gen (:id (first rows))))
        (is (= "active" (:state (first rows))))))
    (testing "every write of the promoted path was rolled back"
      (is (empty? (promotion-rows db)))
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx)))))
      (is (= "active" (:state (gen-row db seed-gen))))
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))
    (testing "no promotion event was appended"
      (is (empty? (event/events-by-type db (:event/session-id fx) :promotion/promoted))))))

(deftest step3-pre-corrupted-state-fails-before-any-write
  (testing "a deleted evaluation row fails with :promotion/evaluation-not-found"
    (let [fx (promotion-fixture)
          db (:db fx)
          _ (sqlite/exec! db ["DELETE FROM eval_runs WHERE id = ?"
                              (str (:evaluation/id fx))])
          e (tx-error #(promote/promote! (promotion-system fx) (promote-request fx)))]
      (is (= :promotion/evaluation-not-found (:error/type (ex-data e))))
      (is (= 1 (count (current-rows db))))
      (is (empty? (promotion-rows db)))
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx)))))))
  (testing "a non-finalized evaluation is rejected (Invariant 4)"
    (let [fx (promotion-fixture)
          db (:db fx)
          _ (sqlite/exec! db ["UPDATE eval_runs SET status = 'running' WHERE id = ?"
                              (str (:evaluation/id fx))])
          e (tx-error #(promote/promote! (promotion-system fx) (promote-request fx)))]
      (is (= :promotion/evaluation-not-finalized (:error/type (ex-data e))))
      (is (= 1 (count (current-rows db))))
      (is (empty? (promotion-rows db)))))
  (testing "an evaluation for a DIFFERENT candidate is rejected (Invariant 5)"
    (let [fx (promotion-fixture {:n-candidates 2})
          db (:db fx)
          [c1 c2] (:candidates fx)
          e (tx-error #(promote/promote! (system-for fx c1)
                                         {:candidate-id (:candidate/id c1)
                                          :evaluation-id (:evaluation/id c2)
                                          :expected-parent-generation (:generation/id fx)}))]
      (is (= :promotion/evaluation-candidate-mismatch (:error/type (ex-data e))))
      (is (= 1 (count (current-rows db))))
      (is (empty? (promotion-rows db)))
      (is (= "eligible" (:state (candidate-row db (:candidate/id c1)))))))
  (testing "a genome absent from the CAS fails activation (Invariant 7)"
    (let [fx (promotion-fixture)
          db (:db fx)
          _ (Files/deleteIfExists (cas/body-path (:cas fx) (:candidate/genome-id fx)))
          e (tx-error #(promote/promote! (promotion-system fx) (promote-request fx)))]
      (is (= :store/cas-missing (:error/type (ex-data e))))
      (is (= 1 (count (current-rows db))))
      (is (empty? (promotion-rows db)))))
  (testing "a candidate that is not :evaluated cannot be promoted (9.1 evaluated-only)"
    (let [fx (promotion-fixture)
          db (:db fx)
          _ (sqlite/exec! db ["UPDATE candidates SET state = 'materialized' WHERE id = ?"
                              (str (:candidate/id fx))])
          e (tx-error #(promote/promote! (promotion-system fx) (promote-request fx)))]
      (is (= :promotion/candidate-state-invalid (:error/type (ex-data e))))
      (is (= 1 (count (current-rows db))))
      (is (empty? (promotion-rows db)))))
  (testing "an expected parent that disagrees with the candidate's lineage fails loudly (Invariant 8)"
    (let [fx (promotion-fixture)
          db (:db fx)
          e (tx-error #(promote/promote! (promotion-system fx)
                                         (assoc (promote-request fx)
                                                :expected-parent-generation retired-gen)))]
      (is (= :promotion/parent-mismatch (:error/type (ex-data e))))
      (is (= 1 (count (current-rows db))))
      (is (empty? (promotion-rows db)))
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx))))))))

(deftest step3-stale-when-the-parent-generation-is-no-longer-current
  (testing "a candidate whose parent is a RETIRED generation loses the
            compare CURRENT == candidate.parent and reports :stale
            without touching CURRENT"
    (let [fx (promotion-fixture {:parent-generation retired-gen})
          db (:db fx)
          result (promote/promote! (promotion-system fx) (promote-request fx))]
      (is (= :stale (:status result)))
      (is (= seed-gen (:current result)))
      (is (= (:generation/id fx) (:expected result)))
      (let [rows (current-rows db)]
        (is (= 1 (count rows)))
        (is (= seed-gen (:id (first rows)))))
      (testing "the losing candidate is marked :stale (9.1)"
        (is (= "stale" (:state (candidate-row db (:candidate/id fx))))))
      (testing "the :promotion/stale event is appended; no promotions row"
        (is (= 1 (count (event/events-by-type db (:event/session-id fx) :promotion/stale))))
        (is (empty? (promotion-rows db)))))))

;; ============================================================================
;; Step 4 — promotion consumes ONLY finalized eligibility data
;; ============================================================================

(deftest step4-promotion-consumes-only-finalized-eligibility
  (testing "the decision is the stored :eligibility map, never recomputed:
            flipping the stored judgment makes the IDENTICAL request fail
            closed with :promotion/ineligible, CURRENT untouched"
    (let [fx (promotion-fixture
              {:eligibility {:eligible? false
                             :reasons [{:dimension :hard
                                        :rule :hard-violation
                                        :detail "a hard gate failed"}]}})
          db (:db fx)
          e (tx-error #(promote/promote! (promotion-system fx) (promote-request fx)))]
      (is (= :promotion/ineligible (:error/type (ex-data e))))
      (testing "CURRENT is untouched"
        (let [rows (current-rows db)]
          (is (= 1 (count rows)))
          (is (= seed-gen (:id (first rows))))))
      (testing "the candidate stays :evaluated; nothing was written"
        (is (= "eligible" (:state (candidate-row db (:candidate/id fx)))))
        (is (empty? (promotion-rows db)))
        (is (empty? (event/events-by-type db (:event/session-id fx)
                                          :promotion/promoted))))))
  (testing "an eligible stored judgment promotes (baseline)"
    (let [fx (promotion-fixture)
          result (promote/promote! (promotion-system fx) (promote-request fx))]
      (is (= :promoted (:status result))))))

;; ============================================================================
;; Boundary: input validation at the module boundary
;; ============================================================================

(deftest promote-request-and-system-are-validated
  (let [fx (promotion-fixture)
        invalid-request? (fn [req]
                           (= :promotion/invalid
                              (-> (tx-error #(promote/promote! (promotion-system fx) req))
                                  ex-data :error/type)))]
    (testing "unknown request keys are rejected (closed trust boundary)"
      (is (invalid-request? (assoc (promote-request fx) :bogus 1))))
    (testing "a malformed candidate id is rejected"
      (is (invalid-request? (assoc (promote-request fx) :candidate-id "not-a-uuid"))))
    (testing "a missing expected parent generation is rejected"
      (is (invalid-request? (dissoc (promote-request fx) :expected-parent-generation))))
    (testing "a system without a store is rejected"
      (is (= :promotion/system-invalid
             (-> (tx-error #(promote/promote! (dissoc (promotion-system fx) :store)
                                              (promote-request fx)))
                 ex-data :error/type))))))

;; ============================================================================
;; SCI sandbox static recheck gate (Task: promote-gate heuristic)
;; ============================================================================

(deftest sci-sandbox-gate-rejects-dangerous-and-allows-safe-source
  (testing "a candidate program with dangerous interop is rejected by the gate"
    (let [r (promote/sci-sandbox-gate "(System/exit 0)")]
      (is (false? (:passed? r)))
      (is (seq (:violations r)))))
  (testing "a pure-function candidate program passes the gate"
    (let [r (promote/sci-sandbox-gate "(defn f [x] (+ x 1))")]
      (is (true? (:passed? r)))
      (is (empty? (:violations r)))))
  (testing "a multi-file genome fails when ANY program hits a red light"
    (let [r (promote/sci-sandbox-gate ["(defn f [x] (+ x 1))" "(Thread/sleep 0)"])]
      (is (false? (:passed? r)))
      (is (seq (:violations r)))))
  (testing "a multi-file genome passes only when EVERY program is safe"
    (let [r (promote/sci-sandbox-gate ["(defn f [x] (+ x 1))" "(defn g [y] (* y 2))"])]
      (is (true? (:passed? r)))
      (is (empty? (:violations r))))))

(deftest sci-sandbox-gate-blocks-promotion-of-dangerous-candidate
  (let [fx (promotion-fixture {:genome-body "(System/exit 0)"})
        db (:db fx)
        e (tx-error #(promote/promote! (promotion-system fx) (promote-request fx)))]
    (testing "the promotion is rejected with a typed sci-sandbox error"
      (is (= :promotion/sci-sandbox-failed (:error/type (ex-data e))))
      (is (= "sci sandbox recheck failed" (:reason (ex-data e))))
      (is (seq (:violations (ex-data e)))))
    (testing "CURRENT is untouched and nothing was written"
      (let [rows (current-rows db)]
        (is (= 1 (count rows)))
        (is (= seed-gen (:id (first rows)))))
      (is (empty? (promotion-rows db)))
      (is (= "eligible" (:state (candidate-row db (:candidate/id fx))))))))

(deftest sci-sandbox-gate-allows-safe-candidate-to-promote
  (let [fx (promotion-fixture {:genome-body "(defn f [x] (+ x 1))"})
        result (promote/promote! (promotion-system fx) (promote-request fx))]
    (testing "the safe candidate promotes normally; the gate does not interrupt"
      (is (= :promoted (:status result)))
      (is (= (:generation/id fx) (:from result)))
      (is (string? (:to result))))))