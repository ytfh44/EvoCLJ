(ns evoclj.store.current-singleton-test
  "Fleet S1 - kernel_state singleton guarantees.

  Proves that after 007-singleton-current.sql the CURRENT pointer is
  a singleton definition, not a predicate protocol:
  - at-most-one row enforced by CHECK(id=1) + PRIMARY KEY WITHOUT ROWID
  - deletion forbidden by trigger (definition-level exactly-one after seed)
  - sync: kernel_state current_generation -> generations.current via triggers
  - seed: migration seeds kernel_state from existing current=1 row"
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.current-store :as cs]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private g1 "generation-1")
(def ^:private g2 "generation-2")
(def ^:private hash1 (str "sha256:" (apply str (repeat 64 "a"))))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-singleton-" ".db" (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (Files/deleteIfExists (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db-path []
  (let [p (temp-db-path)
        spec (sqlite/spec p)]
    (migrate/migrate! spec)
    p))

(defn- insert-generation! [db id {:keys [current parent genome]}]
  (let [gid (or genome hash1)
        rid "resolution-1"]
    (sqlite/with-db [conn db]
      ;; P5/F: ensure FK targets for generations
      (try (jdbc/insert! conn :artifacts {:hash gid :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :artifacts {:hash rid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
      (try (jdbc/insert! conn :genomes {:id gid :created_at now}) (catch Exception _ nil))
      (jdbc/insert! conn :generations
                    {:id id
                     :genome_id gid
                     :resolution_id rid
                     :parent_id parent
                     :state "active"
                     :current (if current 1 0)
                     :created_at now}))))

(deftest kernel-state-table-exists
  (let [p (fresh-db-path)
        db (sqlite/spec p)]
    (testing "kernel_state table exists with WITHOUT ROWID singleton shape"
      (let [sql (first (sqlite/query db ["SELECT sql FROM sqlite_master WHERE type='table' AND name='kernel_state'"]))]
        (is (some? sql))
        (is (re-find #"WITHOUT ROWID" (:sql sql)))
        (is (re-find #"CHECK\s*\(id\s*=\s*1\)" (:sql sql)))
        (is (re-find #"current_generation" (:sql sql)))
        (is (re-find #"REFERENCES generations" (:sql sql)))))
    (testing "triggers exist"
      (let [trigs (set (map :name (sqlite/query db ["SELECT name FROM sqlite_master WHERE type='trigger'"])))]
        (is (contains? trigs "kernel_state_no_delete"))
        (is (contains? trigs "kernel_state_sync_current_after_insert"))
        (is (contains? trigs "kernel_state_sync_current_after_update"))))))

(deftest singleton-at-most-one
  (let [p (fresh-db-path)
        db (sqlite/spec p)]
    (insert-generation! db g1 {:current true})
    (insert-generation! db g2 {})
    (testing "first kernel_state insert succeeds"
      (sqlite/with-db [conn db]
        (jdbc/execute! conn ["INSERT INTO kernel_state (id, current_generation, updated_at) VALUES (1, ?, ?)" g1 now]))
      (is (= g1 (-> (sqlite/query db ["SELECT current_generation FROM kernel_state WHERE id=1"]) first :current_generation))))
    (testing "second row with id=1 violates PRIMARY KEY (at-most-one)"
      (is (thrown? java.sql.SQLException
                   (sqlite/with-db [conn db]
                     (jdbc/execute! conn ["INSERT INTO kernel_state (id, current_generation, updated_at) VALUES (1, ?, ?)" g2 now])))))
    (testing "id=2 violates CHECK(id=1)"
      (is (thrown? java.sql.SQLException
                   (sqlite/with-db [conn db]
                     (jdbc/execute! conn ["INSERT INTO kernel_state (id, current_generation, updated_at) VALUES (2, ?, ?)" g2 now])))))
    (testing "only one row remains"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM kernel_state"])))))))

(deftest singleton-deletion-forbidden
  (let [p (fresh-db-path)
        db (sqlite/spec p)]
    (insert-generation! db g1 {:current true})
    (insert-generation! db g2 {})
    (sqlite/with-db [conn db]
      (jdbc/execute! conn ["INSERT INTO kernel_state (id, current_generation, updated_at) VALUES (1, ?, ?)" g1 now]))
    (testing "DELETE is blocked by trigger"
      (is (thrown-with-msg? java.sql.SQLException #"singleton.*deletion forbidden"
                            (sqlite/with-db [conn db]
                              (jdbc/execute! conn ["DELETE FROM kernel_state WHERE id=1"])))))
    (testing "row still exists after blocked delete"
      (is (= 1 (count (sqlite/query db ["SELECT id FROM kernel_state"])))))))

(deftest singleton-syncs-generations-current
  (let [p (fresh-db-path)
        db (sqlite/spec p)]
    (insert-generation! db g1 {:current true})
    (insert-generation! db g2 {})
    (testing "INSERT into kernel_state syncs generations.current via trigger"
      (sqlite/with-db [conn db]
        (jdbc/execute! conn ["INSERT INTO kernel_state (id, current_generation, updated_at) VALUES (1, ?, ?)" g1 now]))
      (is (= 1 (-> (sqlite/query db ["SELECT current FROM generations WHERE id=?" g1]) first :current)))
      (is (= 0 (-> (sqlite/query db ["SELECT current FROM generations WHERE id=?" g2]) first :current))))
    (testing "UPDATE kernel_state moves the pointer and syncs predicate"
      (sqlite/with-db [conn db]
        (jdbc/execute! conn ["UPDATE kernel_state SET current_generation = ?, updated_at = ? WHERE id=1" g2 now]))
      (is (= 0 (-> (sqlite/query db ["SELECT current FROM generations WHERE id=?" g1]) first :current)))
      (is (= 1 (-> (sqlite/query db ["SELECT current FROM generations WHERE id=?" g2]) first :current)))
      (is (= g2 (-> (sqlite/query db ["SELECT current_generation FROM kernel_state WHERE id=1"]) first :current_generation))))
    (testing "foreign key: kernel_state cannot point to missing generation"
      (is (thrown-with-msg? java.sql.SQLException #"FOREIGN KEY"
                            (sqlite/with-db [conn db]
                              (jdbc/execute! conn ["UPDATE kernel_state SET current_generation = ? WHERE id=1" "no-such-generation"])))))))

(deftest singleton-seeded-from-existing-current
  (testing "real v6 -> 10 migration seeds kernel_state from existing current=1 row"
    (let [p (temp-db-path)
          db (sqlite/spec p)
          v6-files ["001-init.sql" "002-memory.sql" "003-routing.sql" "004-enrichment.sql" "005-deploy.sql" "006-session-bindings.sql"]
          split-statements @(ns-resolve 'evoclj.store.migrate 'split-statements)]
      ;; Build a real v6 DB by applying 001-006 exactly as migrate does
      (jdbc/with-db-transaction [conn (sqlite/spec db)]
        (sqlite/enable-foreign-keys! conn)
        (doseq [f v6-files]
          (let [sql (slurp (io/resource (str "migrations/" f)))
                stmts (split-statements sql)]
            (doseq [stmt stmts]
              (jdbc/execute! conn [stmt]))
            (let [applied (-> (jdbc/query conn ["SELECT value FROM meta WHERE key = ?" "applied_migrations"]) first :value)
                  updated (if (seq applied) (str applied " " f) f)]
              (jdbc/execute! conn ["INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)" "applied_migrations" updated]))))
        (jdbc/execute! conn ["INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)" "schema_version" "6"]))
      ;; v6 has no kernel_state table
      (is (= 0 (count (sqlite/query db ["SELECT name FROM sqlite_master WHERE type='table' AND name='kernel_state'"]))))
      ;; insert a generation with current=1 at v6
      (insert-generation! db g1 {:current true})
      (is (= 1 (-> (sqlite/query db ["SELECT current FROM generations WHERE id=?" g1]) first :current)))
      ;; migrate to 017 should create kernel_state and seed from existing current
      (let [result (migrate/migrate! db)]
        (is (= 19 (:version result))))
      (is (= g1 (-> (sqlite/query db ["SELECT current_generation FROM kernel_state WHERE id=1"]) first :current_generation)))
      (is (= 1 (-> (sqlite/query db ["SELECT current FROM generations WHERE id=?" g1]) first :current)))
      (is (= 1 (count (sqlite/query db ["SELECT id FROM kernel_state"])))))))

(deftest current-store-prefers-singleton
  (let [p (fresh-db-path)
        db (sqlite/spec p)]
    (insert-generation! db g1 {:current true})
    (insert-generation! db g2 {})
    (sqlite/with-db [conn db]
      (jdbc/execute! conn ["INSERT INTO kernel_state (id, current_generation, updated_at) VALUES (1, ?, ?)" g1 now]))
    (let [store (cs/make-current-store p)]
      (is (= g1 (:id (cs/current-generation store))))
      (sqlite/with-db [conn db]
        (jdbc/execute! conn ["UPDATE kernel_state SET current_generation = ?, updated_at = ? WHERE id=1" g2 now]))
      (is (= g2 (:id (cs/current-generation store)))))))

(deftest init-singleton-helper
  (let [p (fresh-db-path)
        db (sqlite/spec p)]
    (insert-generation! db g1 {:current true})
    ;; fresh DB after migrate has empty kernel_state because no generations at migration time
    (is (= 0 (count (sqlite/query db ["SELECT id FROM kernel_state"]))))
    (let [store (cs/make-current-store p)]
      (cs/init-singleton! store g1)
      (is (= g1 (-> (sqlite/query db ["SELECT current_generation FROM kernel_state WHERE id=1"]) first :current_generation)))
      ;; second call is no-op (INSERT OR IGNORE)
      (cs/init-singleton! store g1)
      (is (= 1 (count (sqlite/query db ["SELECT id FROM kernel_state"])))))))