(ns evoclj.store.migrate-test
  "component tests for the SQLite schema and migration runner.

  Step 1: a fresh temporary database applies all migrations once and
  ends with all 17 normative tables plus a recorded schema version.
  Step 2: a second apply is a safe no-op that verifies the version and
  leaves the schema undamaged. Step 3: the required unique constraints
  hold — generation id, per-session event sequence, the single CURRENT
  pointer row, and artifact hash. Step 4: lineage foreign keys are
  present AND actually enforced (the connection pragma is on), and the
  append-only event triggers reject updates/deletes. Step 5: a
  schema-version mismatch fails cleanly with a typed
  :store/schema-mismatch error and changes nothing. Step 6: the B0
  version-reconciliation matrix — databases stamped by the previous
  build (5) with and without the 006 record, a version-3 upgrade, and
  a database stamped ahead of the code. Step 7: the classpath
  migration chain itself fails closed — validate-migration-chain!
  rejects gaps, a wrong starting number, duplicate numbers, or drift
  from latest-version BEFORE any database work, and every migrate!
  path validates the chain first. Step 8: the per-step incremental
  matrix — for every N→N+1 in the chain, a shaped version-N fixture
  upgrades through the real runner with that file's own schema
  invariants verified, zero data loss, and an idempotent re-run.

  Temp databases live in the system temp directory (created via
  java.nio.file.Files/createTempFile) and are deleted after each test;
  every connection is closed by java.jdbc before cleanup runs."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private expected-tables
  "The 19 normative tables (component + 006 session_bindings + 009 genomes)."
  #{"meta" "generations" "candidates" "mutations" "sessions" "events"
    "artifacts" "genomes" "model_calls" "tool_calls" "episodes" "eval_runs"
    "eval_cases" "eval_results" "capability_leases" "promotions"
    "session_bindings" "kernel_state" "causal_links"})

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

(defn- set-meta!
  "Overwrite the value recorded for `key` in the meta table."
  [db key value]
  (sqlite/exec! db ["UPDATE meta SET value = ? WHERE key = ?" value key]))

(defn- insert!
  "Insert `row` into `table` on a fresh connection with FK pragma on."
  [db table row]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn table row)))

(defn- insert-generation!
  [db id {:keys [current parent genome]}]
  (let [gid (or genome hash1)
        rid "resolution-1"]
    ;; P5/F: ensure FK targets exist (generations FK to genomes -> artifacts, resolution -> artifacts)
    (try (insert! db :artifacts {:hash gid :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (insert! db :artifacts {:hash rid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (insert! db :genomes {:id gid :created_at now}) (catch Exception _ nil))
    (insert! db :generations
             {:id id
              :genome_id gid
              :resolution_id rid
              :parent_id parent
              :state "active"
              :current (if current 1 0)
              :created_at now})))

(defn- insert-session!
  [db sid gen-id]
  (try (insert! db :artifacts {:hash hash1 :media_type "application/octet-stream" :size 0 :created_at now}) (catch Exception _ nil))
  (try (insert! db :artifacts {:hash "resolution-1" :media_type "application/octet-stream" :size 0 :created_at now}) (catch Exception _ nil))
  (try (insert! db :artifacts {:hash "phenotype-1" :media_type "application/octet-stream" :size 0 :created_at now}) (catch Exception _ nil))
  (try (insert! db :genomes {:id hash1 :created_at now}) (catch Exception _ nil))
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
  ;; P5/F: ensure FK targets exist for candidate FKs (genomes/artifacts)
  (let [pg (or parent-genome hash1)
        cg hash2
        eid hash1]
    (try (insert! db :artifacts {:hash pg :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (insert! db :artifacts {:hash cg :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (insert! db :artifacts {:hash eid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (insert! db :genomes {:id pg :created_at now}) (catch Exception _ nil))
    (try (insert! db :genomes {:id cg :created_at now}) (catch Exception _ nil))
    (insert! db :candidates
             {:id id
              :parent_generation_id parent
              :parent_genome_id pg
              :genome_id cg
              :mutation_id (or mutation "mutation-1")
              :evidence_id eid
              :risk ":parameter"
              :state "materialized"
              :created_at now})))

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
    (is (= {:status :applied :version 18} (migrate/migrate! db)))
    (is (= 18 (migrate/current-version db)))
    (testing "all 17 normative tables exist"
      (is (every? (table-names db) expected-tables)))
    (testing "schema version and applied migrations are recorded in meta"
      (is (= 2 (count (sqlite/query db ["SELECT key FROM meta"]))))
      (is (= "18" (meta-value db "schema_version")))
      (is (= "001-init.sql 002-memory.sql 003-routing.sql 004-enrichment.sql 005-deploy.sql 006-session-bindings.sql 007-singleton-current.sql 008-normalize-candidate.sql 009-cas-fk-existence.sql 010-promotion-outbox.sql 011-session-memory-fk.sql 012-commands.sql 013-capabilities.sql 014-code-image-deployment-execution.sql 015-principal.sql 016-resource-edn.sql 017-event-prev-causal-links.sql 018-work.sql"
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
    (is (= {:status :noop :version 18} (migrate/migrate! db)))
    (testing "no duplicate/schema damage"
      (is (= tables-before (table-names db)))
      (is (= "18" (meta-value db "schema_version")))
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
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY|TRIGGER|candidates parent_genome_id"
                            (insert-candidate! db {:parent g1 :parent-genome hash2}))))
    (testing "a candidate cannot reference a missing mutation"
      (is (thrown-with-msg? java.sql.SQLException #"(?i)FOREIGN KEY|TRIGGER|candidates parent_genome_id"
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
;; Step 6 — the version-reconciliation matrix (B0)
;;
;; latest-version tracks the six migration files on the classpath. The
;; interesting databases are the ones stamped by the PREVIOUS build
;; (schema_version = 5): one whose applied record already covers 006
;; (the field shape — the incremental branch finds no pending work and
;; merely brings the version record forward), and a true legacy one
;; whose applied record stops at 005. Before the bump that legacy shape
;; hit the version==latest verify branch and failed with
;; :missing-migration-record; post-bump the incremental branch is
;; reachable at latest and applies ONLY 006.
;; ============================================================================

(deftest stamped-five-with-full-record-is-brought-forward
  ;; Field shape: migrated under the previous build (stamped 5, all six
  ;; files applied and recorded). Rewind only the version stamp.
  (let [db (fresh-db)
        _ (set-meta! db "schema_version" "5")]
    (is (= {:status :noop :version 18} (migrate/migrate! db)))
    (testing "the version record was brought forward to 9"
      (is (= "18" (meta-value db "schema_version"))))
    (testing "the schema was not touched"
      (is (every? (table-names db) expected-tables)))))

(deftest true-legacy-stamped-five-applies-pending-006
  ;; True legacy shape: stamped 5 but the applied record stops at 005 —
  ;; session_bindings never made it onto disk. Regression anchor: this
  ;; exact shape used to throw :missing-migration-record; now 006
  ;; applies additively.
  (let [db (fresh-db)
        _ (insert-generation! db g1 {})
        _ (sqlite/exec! db ["DROP TABLE session_bindings"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS kernel_state"])
        _ (sqlite/exec! db ["DROP VIEW IF EXISTS candidates_normalized"])
        _ (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_insert"])
        _ (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_update"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS executions"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS deployments"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS code_images"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_code_image_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_deployment_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_execution_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_code_image_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_deployment_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_execution_idx"])
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN code_image_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN deployment_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN execution_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN code_image_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN deployment_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN execution_id"]) (catch Exception _ nil))
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS causal_links"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_from_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_to_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_type_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_prev_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_cause_idx"])
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN prev_event_id"]) (catch Exception _ nil))
        _ (set-meta! db "applied_migrations"
                     "001-init.sql 002-memory.sql 003-routing.sql 004-enrichment.sql 005-deploy.sql")
        _ (set-meta! db "schema_version" "5")
        result (migrate/migrate! db)]
    (testing "only 006-009 runs; status :applied at version 9"
      (is (= {:status :applied :version 18} result)))
    (testing "session_bindings exists again"
      (is (contains? (table-names db) "session_bindings")))
    (testing "the records agree with the classpath"
      (is (= "001-init.sql 002-memory.sql 003-routing.sql 004-enrichment.sql 005-deploy.sql 006-session-bindings.sql 007-singleton-current.sql 008-normalize-candidate.sql 009-cas-fk-existence.sql 010-promotion-outbox.sql 011-session-memory-fk.sql 012-commands.sql 013-capabilities.sql 014-code-image-deployment-execution.sql 015-principal.sql 016-resource-edn.sql 017-event-prev-causal-links.sql 018-work.sql"
             (meta-value db "applied_migrations")))
      (is (= "18" (meta-value db "schema_version"))))
    (testing "pre-existing data survives the additive upgrade"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))
    (testing "a follow-up apply is a verified no-op"
      (is (= {:status :noop :version 18} (migrate/migrate! db))))))

(deftest version-three-database-upgrades-additively-through-six
  ;; A version-3 database predates the enrichment store, deploy log,
  ;; and session bindings entirely: rewinding those tables plus the
  ;; meta records simulates one. 004-008 apply additively.
  (let [db (fresh-db)
        _ (insert-generation! db g1 {})
        _ (sqlite/exec! db ["DROP TABLE enrichments"])
        _ (sqlite/exec! db ["DROP TABLE deployment_decisions"])
        _ (sqlite/exec! db ["DROP TABLE session_bindings"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS kernel_state"])
        _ (sqlite/exec! db ["DROP VIEW IF EXISTS candidates_normalized"])
        _ (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_insert"])
        _ (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_update"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS executions"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS deployments"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS code_images"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_code_image_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_deployment_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_execution_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_code_image_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_deployment_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_execution_idx"])
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN code_image_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN deployment_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN execution_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN code_image_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN deployment_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN execution_id"]) (catch Exception _ nil))
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS causal_links"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_from_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_to_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_type_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_prev_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_cause_idx"])
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN prev_event_id"]) (catch Exception _ nil))
        _ (set-meta! db "applied_migrations"
                     "001-init.sql 002-memory.sql 003-routing.sql")
        _ (set-meta! db "schema_version" "3")
        result (migrate/migrate! db)]
    (testing "004-010 run; status :applied at version 10"
      (is (= {:status :applied :version 18} result)))
    (testing "the three later tables are back"
      (let [tables (table-names db)]
        (is (contains? tables "enrichments"))
        (is (contains? tables "deployment_decisions"))
        (is (contains? tables "session_bindings"))))
    (testing "pre-existing rows survive untouched"
      (let [row (first (sqlite/query db
                                     ["SELECT id FROM generations WHERE id = 'generation-1'"]))]
        (is (= g1 (:id row)))))
    (testing "the records agree with the classpath"
      (is (= "001-init.sql 002-memory.sql 003-routing.sql 004-enrichment.sql 005-deploy.sql 006-session-bindings.sql 007-singleton-current.sql 008-normalize-candidate.sql 009-cas-fk-existence.sql 010-promotion-outbox.sql 011-session-memory-fk.sql 012-commands.sql 013-capabilities.sql 014-code-image-deployment-execution.sql 015-principal.sql 016-resource-edn.sql 017-event-prev-causal-links.sql 018-work.sql"
             (meta-value db "applied_migrations")))
      (is (= "18" (meta-value db "schema_version"))))))

(deftest version-ahead-of-code-fails-cleanly
  ;; Exactly one past the new latest-version: still never guessed at.
  (let [db (fresh-db)
        _ (set-meta! db "schema_version" "19")
        e (migrate-error db)]
    (is (some? e))
    (is (= :store/schema-mismatch (:error/type (ex-data e))))
    (is (= :version-ahead (:reason (ex-data e))))
    (is (= 19 (:actual (ex-data e))))
    (testing "the failed attempt changed nothing"
      (is (= "19" (meta-value db "schema_version"))))))

(deftest mid-chain-failure-leaves-prior-version-intact
  ;; Fault path: a pending migration's SQL fails mid-chain (here 006's
  ;; CREATE TABLE collides with a same-named object left on disk). The
  ;; whole upgrade transaction must roll back — version stamp, applied
  ;; record, and pre-existing data keep their prior values — and the
  ;; failure surfaces as a typed :store/migration-error naming the file
  ;; that failed, not a raw driver exception.
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (sqlite/exec! db ["DROP TABLE session_bindings"])
    (sqlite/exec! db ["DROP TABLE IF EXISTS kernel_state"])
    (sqlite/exec! db ["DROP VIEW IF EXISTS candidates_normalized"])
    (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_insert"])
    (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_update"])
    (sqlite/exec! db ["DROP TABLE IF EXISTS executions"])
    (sqlite/exec! db ["DROP TABLE IF EXISTS deployments"])
    (sqlite/exec! db ["DROP TABLE IF EXISTS code_images"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_code_image_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_deployment_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_execution_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_code_image_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_deployment_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_execution_idx"])
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN code_image_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN deployment_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN execution_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN code_image_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN deployment_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN execution_id"]) (catch Exception _ nil))
    (sqlite/exec! db ["DROP TABLE IF EXISTS causal_links"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_from_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_to_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_type_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_prev_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_cause_idx"])
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN prev_event_id"]) (catch Exception _ nil))
    (sqlite/exec! db ["CREATE TABLE session_bindings (bogus TEXT)"])
    (set-meta! db "applied_migrations"
               "001-init.sql 002-memory.sql 003-routing.sql 004-enrichment.sql 005-deploy.sql")
    (set-meta! db "schema_version" "5")
    (let [e (migrate-error db)]
      (is (some? e) "migrate! must throw when a pending migration fails")
      (is (= :store/migration-error (:error/type (ex-data e))))
      (is (= "006-session-bindings.sql" (:migration/file (ex-data e))))
      (testing "the failed attempt changed nothing"
        (is (= "5" (meta-value db "schema_version")))
        (is (= "001-init.sql 002-memory.sql 003-routing.sql 004-enrichment.sql 005-deploy.sql"
               (meta-value db "applied_migrations")))
        (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"]))))
        (testing "the sabotaged object was not consumed by a partial apply"
          (is (= #{"bogus"}
                 (set (map :name (sqlite/query db
                                               ["PRAGMA table_info(session_bindings)"]))))))
        (testing "the database still upgrades once the obstruction is cleared"
          (sqlite/exec! db ["DROP TABLE session_bindings"])
          (is (= {:status :applied :version 18} (migrate/migrate! db)))
          (is (contains? (table-names db) "session_bindings"))
          (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))))))

;; ============================================================================
;; Step 7 — fail-closed migration chain integrity (B0)
;;
;; latest-version is a human-maintained constant while the migration file
;; set is ground truth; B0 happened precisely because nothing forced them
;; to agree. validate-migration-chain! enforces that agreement BEFORE any
;; database work: versions run 1..N contiguously (no gaps, no duplicate
;; numbers, correct start) and end exactly at latest-version. The real
;; classpath is validated inside migration-files, so every migrate! path
;; inherits the check.
;; ============================================================================

(def ^:private full-chain
  ["001-init.sql" "002-memory.sql" "003-routing.sql"
   "004-enrichment.sql" "005-deploy.sql" "006-session-bindings.sql"
   "007-singleton-current.sql" "008-normalize-candidate.sql" "009-cas-fk-existence.sql" "010-promotion-outbox.sql" "011-session-memory-fk.sql" "012-commands.sql" "013-capabilities.sql" "014-code-image-deployment-execution.sql" "015-principal.sql" "016-resource-edn.sql" "017-event-prev-causal-links.sql" "018-work.sql"])

(deftest latest-version-matches-the-migration-file-set
  ;; The three-way reconciliation pin: constant == file set == recorded
  ;; applied set (the applied-record string is pinned exactly by the
  ;; fresh-install test above).
  (is (= full-chain (vec (migrate/migration-files))))
  (is (= (count full-chain) migrate/latest-version))
  (is (= full-chain
         (migrate/validate-migration-chain! (migrate/migration-files)))
      "the real classpath chain validates cleanly"))

(deftest broken-migration-chains-fail-closed
  (let [chain-error
        (fn [files]
          (try (migrate/validate-migration-chain! files)
               nil
               (catch clojure.lang.ExceptionInfo e e)))]
    (testing "a gap in the middle of the chain"
      (let [e (chain-error ["001-init.sql" "002-memory.sql" "004-enrichment.sql"])]
        (is (some? e))
        (is (= :store/migration-chain-invalid (:error/type (ex-data e))))
        (is (= :chain-gap (:reason (ex-data e))))))
    (testing "a chain that does not start at 1"
      (let [e (chain-error ["003-routing.sql" "004-enrichment.sql"])]
        (is (= :store/migration-chain-invalid (:error/type (ex-data e))))
        (is (= :chain-start (:reason (ex-data e))))))
    (testing "duplicate version numbers"
      (let [e (chain-error ["001-init.sql" "001-duplicate.sql" "002-memory.sql"])]
        (is (= :store/migration-chain-invalid (:error/type (ex-data e))))
        (is (= :chain-duplicate (:reason (ex-data e))))))
    (testing "a chain whose top disagrees with latest-version"
      (let [e-short (chain-error (butlast full-chain))     ; tops out at 16
            e-long (chain-error (conj full-chain "019-beyond.sql"))] ; tops at 18
        (is (= :store/migration-chain-invalid (:error/type (ex-data e-short))))
        (is (= :latest-version-drift (:reason (ex-data e-short))))
        (is (= :latest-version-drift (:reason (ex-data e-long))))))
    (testing "an empty chain cannot silently satisfy a non-empty constant"
      (let [e (chain-error [])]
        (is (= :latest-version-drift (:reason (ex-data e))))))))

;; ============================================================================
;; Step 8 — per-step incremental migration matrix (B0)
;;
;; For every N→N+1 in the chain: a fixture shaped like version N (real
;; schema effects of later migrations rewound, meta records rewound)
;; upgrades through the real runner; the file that owns step N+1 gets its
;; OWN schema invariants verified post-upgrade, pre-existing rows survive,
;; and a re-run is an idempotent noop.
;; ============================================================================

(defn- rewind-to-version!
  "Shape a fully-migrated database into a version-N one: drop the
  schema effects of every migration AFTER N, then rewind both meta
  records to N. What remains is exactly what migrations 1..N produced."
  [db n]
  (when (< n 2) (sqlite/exec! db ["DROP TABLE episodic_memory"]))
  (when (< n 3)
    (try (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_routing_idx"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN routing_deployment_version"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN routing_bucket"]) (catch Exception _ nil)))
  (when (< n 4) (sqlite/exec! db ["DROP TABLE enrichments"]))
  (when (< n 5) (sqlite/exec! db ["DROP TABLE IF EXISTS deployment_decisions"]))
  (when (< n 6) (sqlite/exec! db ["DROP TABLE IF EXISTS session_bindings"]))
  (when (< n 7) (sqlite/exec! db ["DROP TABLE IF EXISTS kernel_state"]))
  (when (< n 9)
    (sqlite/exec! db ["DROP VIEW IF EXISTS candidates_normalized"])
    (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_insert"])
    (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_update"]))
  (when (< n 14)
    (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_code_image_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_deployment_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_execution_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_code_image_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_deployment_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_execution_idx"])
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN code_image_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN deployment_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN execution_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN code_image_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN deployment_id"]) (catch Exception _ nil))
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN execution_id"]) (catch Exception _ nil))
    (sqlite/exec! db ["DROP TABLE IF EXISTS executions"])
    (sqlite/exec! db ["DROP TABLE IF EXISTS deployments"])
    (sqlite/exec! db ["DROP TABLE IF EXISTS code_images"]))
  (when (< n 17)
    (sqlite/exec! db ["DROP TABLE IF EXISTS causal_links"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_from_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_to_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_type_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_prev_idx"])
    (sqlite/exec! db ["DROP INDEX IF EXISTS events_cause_idx"])
    (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN prev_event_id"]) (catch Exception _ nil)))
  (set-meta! db "applied_migrations" (str/join " " (take n full-chain)))
  (set-meta! db "schema_version" (str n)))

(deftest incremental-step-1-to-2-adds-session-scoped-memory
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (rewind-to-version! db 1)
    (is (= {:status :applied :version 18} (migrate/migrate! db)))
    (testing "002-memory.sql invariants hold after the incremental apply"
      (insert-session! db "s1" g1)
      (insert! db :episodic_memory
               {:session_id "s1" :memory_key "k" :content "{:v 1}" :created_at now})
      (is (thrown-with-msg? java.sql.SQLException #"UNIQUE constraint failed"
                            (insert! db :episodic_memory
                                     {:session_id "s1" :memory_key "k"
                                      :content "{:v 2}" :created_at now})))
      (is (= "{:v 1}" (:content (first (sqlite/query db
                                                     ["SELECT content FROM episodic_memory WHERE session_id = 's1'"]))))))
    (testing "no data loss"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))
    (testing "re-running is a verified noop"
      (is (= {:status :noop :version 18} (migrate/migrate! db))))))

(deftest incremental-step-2-to-3-adds-session-routing-audit
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (insert-session! db "old-s" g1)
    (rewind-to-version! db 2)
    (is (= {:status :applied :version 18} (migrate/migrate! db)))
    (testing "003-routing.sql invariants: additive audit columns, index present"
      (let [row (first (sqlite/query db
                                     ["SELECT routing_deployment_version, routing_bucket
                                       FROM sessions WHERE id = 'old-s'"]))]
        (is (nil? (:routing_deployment_version row)) "pre-routing rows stay NULL")
        (is (nil? (:routing_bucket row))))
      (sqlite/exec! db
                    ["INSERT INTO sessions (id, generation_id, genome_id, resolution_id,
                                            phenotype_id, state, created_at,
                                            routing_deployment_version, routing_bucket)
                      VALUES ('new-s', ?, ?, ?, ?, 'created', ?, 'v9', 42)"
                     g1 hash1 "resolution-1" "phenotype-1" now])
      (let [row (first (sqlite/query db
                                     ["SELECT routing_deployment_version, routing_bucket
                                       FROM sessions WHERE id = 'new-s'"]))]
        (is (= "v9" (:routing_deployment_version row)))
        (is (= 42 (:routing_bucket row))))
      (is (seq (sqlite/query db
                             ["SELECT name FROM sqlite_master
                               WHERE type = 'index' AND name = 'sessions_routing_idx'"]))))
    (testing "no data loss"
      (is (= 2 (count (sqlite/query db ["SELECT id FROM sessions"])))))
    (testing "re-running is a verified noop"
      (is (= {:status :noop :version 18} (migrate/migrate! db))))))

(deftest incremental-step-3-to-4-adds-append-only-enrichments
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (rewind-to-version! db 3)
    (is (= {:status :applied :version 18} (migrate/migrate! db)))
    (testing "004-enrichment.sql invariants: append-only discipline survives the upgrade"
      (insert! db :enrichments
               {:id "enr-1" :entity_kind ":genome" :entity_id "e1"
                :kind ":case/weight" :version 1 :payload_ref hash1 :created_at now})
      (is (thrown-with-msg? java.sql.SQLException #"append-only"
                            (sqlite/exec! db
                                          ["UPDATE enrichments SET version = 2 WHERE id = 'enr-1'"])))
      (is (thrown-with-msg? java.sql.SQLException #"append-only"
                            (sqlite/exec! db ["DELETE FROM enrichments"])))
      (is (thrown-with-msg? java.sql.SQLException #"UNIQUE constraint failed"
                            (insert! db :enrichments
                                     {:id "enr-2" :entity_kind ":genome" :entity_id "e1"
                                      :kind ":case/weight" :version 1
                                      :payload_ref hash1 :created_at now}))))
    (testing "no data loss"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))
    (testing "re-running is a verified noop"
      (is (= {:status :noop :version 18} (migrate/migrate! db))))))

(deftest incremental-step-4-to-5-adds-deployment-decision-log
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (rewind-to-version! db 4)
    (is (= {:status :applied :version 18} (migrate/migrate! db)))
    (testing "005-deploy.sql invariants: constrained decision vocabulary"
      (insert! db :deployment_decisions
               {:id "d1" :generation_id g1 :decision "deployed"
                :reason "canary-pass" :created_at now})
      (is (thrown-with-msg? java.sql.SQLException #"CHECK constraint failed"
                            (insert! db :deployment_decisions
                                     {:id "d2" :generation_id g1 :decision "bogus"
                                      :created_at now})))
      (is (= 1 (count (sqlite/query db ["SELECT id FROM deployment_decisions"])))))
    (testing "no data loss"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))
    (testing "re-running is a verified noop"
      (is (= {:status :noop :version 18} (migrate/migrate! db))))))

(deftest incremental-step-5-to-6-adds-durable-session-bindings
  (let [db (fresh-db)]
    (insert-generation! db g1 {})
    (insert-session! db "bound-s" g1)
    (rewind-to-version! db 5)
    (is (= {:status :applied :version 18} (migrate/migrate! db)))
    (testing "006-session-bindings.sql invariants after the incremental apply"
      (let [binding {:id "b1" :session_id "bound-s" :binding_type "skill"
                     :logical_id "[:skill \"debugging\"]" :revision_id hash1
                     :bundle_id "bundle-a" :state "active" :activated_at now}]
        (insert! db :session_bindings binding)
        (testing "at most one ACTIVE binding per (session, logical id)"
          (is (thrown-with-msg? java.sql.SQLException #"UNIQUE constraint failed"
                                (insert! db :session_bindings
                                         (assoc binding :id "b2" :bundle_id "bundle-b")))))
        (testing "an inactive sibling of the same logical id is fine"
          (insert! db :session_bindings
                   (assoc binding :id "b3" :state "inactive" :deactivated_at now))
          (is (= 2 (count (sqlite/query db ["SELECT id FROM session_bindings"])))))
        (testing "bindings still reference real sessions"
          (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                                (insert! db :session_bindings
                                         (assoc binding :id "b4"
                                                :session_id "no-such-session")))))))
    (testing "no data loss"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM sessions"]))))
      (is (= 1 (count (sqlite/query db ["SELECT id FROM generations"])))))
    (testing "re-running is a verified noop"
      (is (= {:status :noop :version 18} (migrate/migrate! db))))))

;; ============================================================================
;; component — an existing version-1 database upgrades additively
;; ============================================================================

(deftest version-1-database-upgrades-additively
  ;; Simulate a pre-Task-9.3 database: migrate a fresh db, then rewind
  ;; the 003-routing.sql effects (index + columns) and the meta records
  ;; back to version 1. The runner must then apply every pending
  ;; migration and bring the version forward to the true latest (8,
  ;; the top of the eight-file classpath chain).
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
        ;; 006-session-bindings.sql effects are rewound too (Phase 8): a v1
        ;; database predates the session_bindings table entirely
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS session_bindings"])
        ;; 007-singleton-current.sql effects are rewound (Fleet S1): a v1
        ;; database predates the kernel_state table entirely
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS kernel_state"])
        ;; 008-normalize-candidate.sql effects are rewound (Fleet S3): a v1
        ;; database predates the normalized view/triggers entirely
        _ (sqlite/exec! db ["DROP VIEW IF EXISTS candidates_normalized"])
        _ (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_insert"])
        _ (sqlite/exec! db ["DROP TRIGGER IF EXISTS candidates_no_mismatch_update"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS executions"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS deployments"])
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS code_images"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_code_image_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_deployment_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS sessions_execution_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_code_image_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_deployment_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_execution_idx"])
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN code_image_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN deployment_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE sessions DROP COLUMN execution_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN code_image_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN deployment_id"]) (catch Exception _ nil))
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN execution_id"]) (catch Exception _ nil))
        _ (sqlite/exec! db ["DROP TABLE IF EXISTS causal_links"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_from_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_to_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS causal_links_type_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_prev_idx"])
        _ (sqlite/exec! db ["DROP INDEX IF EXISTS events_cause_idx"])
        _ (try (sqlite/exec! db ["ALTER TABLE events DROP COLUMN prev_event_id"]) (catch Exception _ nil))
        _ (sqlite/exec! db ["UPDATE meta SET value = '1' WHERE key = 'schema_version'"])
        _ (sqlite/exec! db ["UPDATE meta SET value = '001-init.sql'
                            WHERE key = 'applied_migrations'"])
        ;; P5/F: ensure FK targets for generations inserted during rewind
        _ (sqlite/exec! db ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES ('sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', 'application/octet-stream', 0, '2025-01-01T00:00:00Z')"])
        _ (sqlite/exec! db ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES ('sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '2025-01-01T00:00:00Z')"])
        _ (sqlite/exec! db ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES ('sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', 'application/octet-stream', 0, '2025-01-01T00:00:00Z')"])
        _ (sqlite/exec! db ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES ('sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc', 'application/edn', 0, '2025-01-01T00:00:00Z')"])
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
    (testing "only the pending migration runs; the version moves to 9"
      (is (= {:status :applied :version 18} result)))
    (testing "the old session row survives untouched with NULL routing columns"
      (let [row (first (sqlite/query db ["SELECT routing_deployment_version, routing_bucket
                                          FROM sessions WHERE id = 'old-session'"]))]
        (is (nil? (:routing_deployment_version row)))
        (is (nil? (:routing_bucket row)))))
    (testing "a third apply is a verified no-op"
      (is (= {:status :noop :version 18} (migrate/migrate! db))))))