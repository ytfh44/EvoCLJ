(ns evoclj.store.migrate-test
  "Task 5.1 tests for the SQLite schema and migration runner.

  Step 1: a fresh temporary database applies all migrations once and
  ends with all 15 normative tables plus a recorded schema version.
  Step 2: a second apply is a safe no-op that verifies the version and
  leaves the schema undamaged. Step 3: the required unique constraints
  hold — generation id, per-session event sequence, the single CURRENT
  pointer row, and artifact hash. Step 4: lineage foreign keys are
  present AND actually enforced (the connection pragma is on), and the
  append-only event triggers reject updates/deletes. Step 5: a
  schema-version mismatch fails cleanly with a typed
  :store/schema-mismatch error and changes nothing.

  Temp databases live in the system temp directory (created via
  java.nio.file.Files/createTempFile) and are deleted after each test;
  every connection is closed by java.jdbc before cleanup runs."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private expected-tables
  "The 15 normative initial tables (Task 5.1)."
  #{"meta" "generations" "candidates" "mutations" "sessions" "events"
    "artifacts" "model_calls" "tool_calls" "episodes" "eval_runs"
    "eval_cases" "eval_results" "capability_leases" "promotions"})

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private g1 "generation-1")
(def ^:private g2 "generation-2")
(def ^:private hash1 (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private hash2 (str "sha256:" (apply str (repeat 64 "b"))))

(def ^:private db-paths (atom []))

(defn- temp-db-path
  "Create a throwaway SQLite file in the system temp dir."
  []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-migrate-" ".db"
                (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup!
  "Delete every temp db file created during this run."
  []
  (doseq [p @db-paths]
    (java.nio.file.Files/deleteIfExists
     (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- table-names
  "All table names present in the database (includes sqlite_sequence)."
  [db]
  (set (map :name
            (sqlite/query db
                          ["SELECT name FROM sqlite_master WHERE type = 'table'"]))))

(defn- current-rows
  "Rows with the CURRENT marker set."
  [db]
  (sqlite/query db ["SELECT id FROM generations WHERE current = 1"]))

(defn- meta-value
  "The value recorded for `key` in the meta table, or nil."
  [db key]
  (-> (sqlite/query db ["SELECT value FROM meta WHERE key = ?" key])
      first
      :value))

(defn- insert!
  "Insert `row` into `table` on a fresh connection with FK pragma on."
  [db table row]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn table row)))

(defn- insert-generation!
  [db id {:keys [current parent genome]}]
  (insert! db :generations
           {:id id
            :genome_id (or genome hash1)
            :resolution_id "resolution-1"
            :parent_id parent
            :state "active"
            :current (if current 1 0)
            :created_at now}))

(defn- insert-session!
  [db sid gen-id]
  (insert! db :sessions
           {:id sid
            :generation_id gen-id
            :genome_id hash1
            :resolution_id "resolution-1"
            :phenotype_id "phenotype-1"
            :state "created"
            :created_at now}))

(defn- insert-event!
  [db {:keys [session s gen type] :or {session "s1" gen g1 type ":session/created"}}]
  (insert! db :events
           {:session_id session
            :event_seq s
            :generation_id gen
            :phenotype_id "phenotype-1"
            :event_type type
            :payload "{}"
            :event_hash (str "event-hash-" session "-" s)
            :created_at now}))

(defn- insert-artifact!
  [db hash media size]
  (insert! db :artifacts
           {:hash hash :media_type media :size size :created_at now}))

(defn- insert-mutation!
  [db id]
  (insert! db :mutations
           {:id id
            :parent_genome_id hash1
            :hypothesis_id "hypothesis-1"
            :evidence_id hash1
            :risk ":parameter"
            :ops "[]"
            :expected_effect "{}"
            :created_at now}))

(defn- insert-candidate!
  [db {:keys [id parent parent-genome mutation] :or {id "candidate-1"}}]
  (insert! db :candidates
           {:id id
            :parent_generation_id parent
            :parent_genome_id (or parent-genome hash1)
            :genome_id hash2
            :mutation_id (or mutation "mutation-1")
            :evidence_id hash1
            :risk ":parameter"
            :state "materialized"
            :created_at now}))

(defn- insert-eval-run!
  [db id candidate-id]
  (insert! db :eval_runs
           {:id id
            :candidate_id candidate-id
            :parent_generation_id g1
            :profile_id ":default"
            :gates "[]"
            :summary "{}"
            :eligibility "{}"
            :created_at now}))

(defn- insert-promotion!
  [db from to eval-id]
  (insert! db :promotions
           {:id "promotion-1"
            :candidate_id "candidate-1"
            :evaluation_id eval-id
            :from_generation_id from
            :to_generation_id to
            :decision "promoted"
            :reason "{}"
            :created_at now}))

(defn- seed-lineage!
  "Insert the rows a valid promotion depends on."
  [db]
  (insert-generation! db g1 {})
  (insert-mutation! db "mutation-1")
  (insert-candidate! db {:parent g1}))

(defn- migrate-error
  "The ExceptionInfo thrown by migrate!, or nil when it succeeds."
  [db]
  (try (migrate/migrate! db)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

;; ============================================================================
;; Step 1 — a fresh database applies all migrations once
;; ============================================================================

(deftest fresh-database-applies-all-migrations
  (let [db (sqlite/spec (temp-db-path))]
    (is (= {:status :applied :version 4} (migrate/migrate! db)))
    (is (= 4 (migrate/current-version db)))
    (testing "all 15 normative tables exist"
      (is (every? (table-names db) expected-tables)))
    (testing "schema version and applied migrations are recorded in meta"
      (is (= 2 (count (sqlite/query db ["SELECT key FROM meta"]))))
      (is (= "4" (meta-value db "schema_version")))
      (is (= "001-init.sql 002-memory.sql 003-routing.sql 004-enrichment.sql 005-deploy.sql"
             (meta-value db "applied_migrations"))))
    (testing "003-routing.sql added the session routing audit columns"
      (let [cols (set (map :name (sqlite/query db
                                               ["PRAGMA table_info(sessions)"])))]
        (is (contains? cols "routing_deployment_version"))
        (is (contains? cols "routing_bucket"))))))

;; ============================================================================
;; Step 2 — applying again is a safe no-op
;; ============================================================================

(deftest second-apply-is-a-safe-noop
  (let [db (fresh-db)
        tables-before (table-names db)]
    (is (= {:status :noop :version 4} (migrate/migrate! db)))
    (testing "no duplicate/schema damage"
      (is (= tables-before (table-names db)))
      (is (= "4" (meta-value db "schema_version")))
      (is (= 2 (count (sqlite/query db ["SELECT key FROM meta"]))))
      ;; the migrated schema still works
      (insert-generation! db g1 {})
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))))

;; ============================================================================
;; Step 3 — required unique constraints
;; ============================================================================

(deftest generation-identifier-is-unique
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (is (thrown-with-msg? java.sql.SQLException #"UNIQUE constraint failed"
                          (insert-generation! db g1 {})))))

(deftest single-current-pointer-row
  (let [db (fresh-db)]
    (testing "the first CURRENT row is accepted"
      (insert-generation! db g1 {:current true})
      (is (= 1 (count (current-rows db)))))
    (testing "a second CURRENT row is rejected by the partial unique index"
      (is (thrown-with-msg? java.sql.SQLException #"UNIQUE constraint failed"
                            (insert-generation! db g2 {:current true}))))
    (testing "non-current rows are unrestricted"
      (insert-generation! db g2 {})
      (is (= 1 (count (current-rows db)))))))

(deftest event-sequence-unique-per-session
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (insert-session! db "s1" g1)
    (insert-event! db {:session "s1" :s 1})
    (testing "the same (session_id, event_seq) is rejected"
      (is (thrown-with-msg? java.sql.SQLException #"UNIQUE constraint failed"
                            (insert-event! db {:session "s1" :s 1
                                               :type ":session/started"}))))
    (testing "the same sequence number in a different session is allowed"
      (insert-session! db "s2" g1)
      (insert-event! db {:session "s2" :s 1})
      (is (= 2 (count (sqlite/query db ["SELECT id FROM events"])))))))

(deftest artifact-hash-is-unique
  (let [db (fresh-db)]
    (insert-artifact! db hash1 "application/edn" 10)
    (is (thrown-with-msg? java.sql.SQLException #"UNIQUE constraint failed"
                          (insert-artifact! db hash1 "application/edn" 20)))
    (testing "a different hash with identical metadata is fine"
      (insert-artifact! db hash2 "application/edn" 20)
      (is (= 2 (count (sqlite/query db ["SELECT hash FROM artifacts"])))))))

;; ============================================================================
;; Step 4 — lineage foreign keys are present AND enforced
;; ============================================================================

(deftest foreign-keys-are-enforced
  (let [db (fresh-db)]
    (testing "a candidate cannot reference a missing parent generation"
      (insert-mutation! db "mutation-1")
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (insert-candidate! db {:parent "no-such-generation"}))))
    (testing "a candidate's parent genome must agree with the generation record (Invariant 8)"
      (insert-generation! db g1 {})          ; genome_id = hash1
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (insert-candidate! db {:parent g1 :parent-genome hash2}))))
    (testing "a candidate cannot reference a missing mutation"
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (insert-candidate! db {:parent g1 :mutation "no-such-mutation"}))))
    (testing "a session cannot reference a missing generation"
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (insert-session! db "s1" "no-such-generation"))))
    (testing "an event cannot reference a missing session or generation"
      (insert-session! db "s1" g1)
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (insert-event! db {:session "s1" :s 1 :gen "no-such-generation"})))
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (insert-event! db {:session "no-such-session" :s 1 :gen g1}))))
    (testing "a promotion must reference an existing finalized evaluation (Invariant 5)"
      ;; g1 and mutation-1 already exist from the blocks above; materialize the
      ;; candidate and a valid target generation so ONLY the evaluation FK can
      ;; fail (re-inserting g1 here would trip generations.id uniqueness).
      (insert-candidate! db {:parent g1})
      (insert-generation! db g2 {:parent g1})
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (insert-promotion! db g1 g2 "no-such-evaluation"))))))

(deftest lineage-fks-allow-valid-inserts
  (let [db (fresh-db)]
    (seed-lineage! db)
    (insert-eval-run! db "evaluation-1" "candidate-1")
    (testing "a valid promotion chain inserts cleanly"
      (insert-generation! db g2 {:parent g1})
      (insert-promotion! db g1 g2 "evaluation-1")
      (is (= 1 (count (sqlite/query db ["SELECT id FROM promotions"])))))))

(deftest events-are-append-only
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (insert-session! db "s1" g1)
    (insert-event! db {:session "s1" :s 1})
    (testing "no UPDATE of the event log"
      (is (thrown-with-msg? java.sql.SQLException #"append-only"
                            (sqlite/exec! db ["UPDATE events SET payload = 'x' WHERE id = 1"]))))
    (testing "no DELETE from the event log"
      (is (thrown-with-msg? java.sql.SQLException #"append-only"
                            (sqlite/exec! db ["DELETE FROM events"]))))
    (testing "the row is still intact after both rejected attempts"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM events"])))))))

;; ============================================================================
;; Step 5 — schema-version check and clean failure on mismatch
;; ============================================================================

(deftest version-mismatch-fails-cleanly
  (let [db (fresh-db)]
    (sqlite/exec! db ["UPDATE meta SET value = '99' WHERE key = 'schema_version'"])
    (let [e (migrate-error db)]
      (is (some? e))
      (is (= :store/schema-mismatch (:error/type (ex-data e))))
      (is (= :version-ahead (:reason (ex-data e))))
      (is (= 99 (:actual (ex-data e)))))
    (testing "the failed attempt changed nothing"
      (is (= "99" (meta-value db "schema_version")))
      (is (= 0 (count (sqlite/query db ["SELECT id FROM generations"])))))))

(deftest unversioned-tables-fail-cleanly
  (let [db (sqlite/spec (temp-db-path))]
    ;; Tables without a version record must never be guessed at.
    (sqlite/exec! db ["CREATE TABLE generations (id TEXT PRIMARY KEY)"])
    (let [e (migrate-error db)]
      (is (some? e))
      (is (= :store/schema-mismatch (:error/type (ex-data e))))
      (is (= :unversioned-tables (:reason (ex-data e)))))
    (testing "the manual table was not touched"
      (is (= 1 (count (sqlite/query db ["SELECT name FROM sqlite_master WHERE name = 'generations'"])))))))

(deftest unrecorded-migration-fails-cleanly
  (let [db (fresh-db)]
    (sqlite/exec! db ["DELETE FROM meta WHERE key = 'applied_migrations'"])
    (let [e (migrate-error db)]
      (is (some? e))
      (is (= :store/schema-mismatch (:error/type (ex-data e))))
      (is (= :missing-migration-record (:reason (ex-data e)))))))

;; ============================================================================
;; Task 9.3 — an existing version-1 database upgrades additively
;; ============================================================================

(deftest version-1-database-upgrades-additively
  ;; Simulate a pre-Task-9.3 database: migrate a fresh db, then rewind
  ;; the 003-routing.sql effects (index + columns) and the meta records
  ;; back to version 1. The runner must then apply ONLY the pending
  ;; migration and bring the version forward to 2.
  (let [db (sqlite/spec (temp-db-path))
        _ (migrate/migrate! db)
        _ (sqlite/exec! db ["DROP INDEX sessions_routing_idx"])
        _ (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN routing_deployment_version"])
        _ (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN routing_bucket"])
        ;; 002-memory.sql effects are also rewound (feature R1): a v1
        ;; database predates the episodic_memory table entirely
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS episodic_memory"])
        ;; 004-enrichment.sql effects are rewound too (Foundation F3): a
        ;; v1 database predates the enrichments table entirely
        _ (sqlite/exec! db ["DROP TABLE enrichments"])
        ;; 005-deploy.sql effects are rewound too (S1-3): a v1 database
        ;; predates the deployment_decisions table entirely
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS deployment_decisions"])
        _ (sqlite/exec! db ["UPDATE meta SET value = '1' WHERE key = 'schema_version'"])
        _ (sqlite/exec! db ["UPDATE meta SET value = '001-init.sql'
                            WHERE key = 'applied_migrations'"])
        _ (sqlite/exec! db ["INSERT INTO generations (id, genome_id, resolution_id,
                                                      state, current, created_at)
                             VALUES ('g1', 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
                                     'sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc',
                                     'active', 0, '2025-01-01T00:00:00Z')"])
        _ (sqlite/exec! db ["INSERT INTO sessions (id, generation_id, genome_id,
                                                   resolution_id, phenotype_id,
                                                   state, created_at)
                             VALUES ('old-session', 'g1', 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
                                     'sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc',
                                     'sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
                                     'created', '2025-01-01T00:00:00Z')"])
        result (migrate/migrate! db)]
    (testing "only the pending migration runs; the version moves to 4"
      (is (= {:status :applied :version 4} result)))
    (testing "the old session row survives untouched with NULL routing columns"
      (let [row (first (sqlite/query db ["SELECT routing_deployment_version, routing_bucket
                                          FROM sessions WHERE id = 'old-session'"]))]
        (is (nil? (:routing_deployment_version row)))
        (is (nil? (:routing_bucket row)))))
    (testing "a third apply is a verified no-op"
      (is (= {:status :noop :version 4} (migrate/migrate! db))))))
