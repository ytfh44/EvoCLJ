(ns evoclj.provider.memory-test
  "Tests for the kernel-owned :memory/kv provider (feature R1).

  The provider closes over a SQLite spec and reads/writes the
  episodic_memory table (migration 002) scoped to the requesting
  session. These tests exercise normalize-request and
  execute-request! directly with real temp databases."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [evoclj.provider.memory :as mem]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-mem-test-" ".db" (make-array FileAttribute 0)))]
    (let [db (sqlite/spec p)]
      (migrate/migrate! db)
      db)))

(defn- sid [n]
  (UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(def ^:private gen "mem-gen-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))

(defn- ensure-session! [db session-id]
  (sqlite/with-db [conn db]
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" genome])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" resolution])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" phenotype])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES (?, datetime('now'))" genome])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO generations (id, genome_id, resolution_id, parent_id, state, current, created_at) VALUES (?, ?, ?, NULL, 'active', 0, datetime('now'))" gen genome resolution])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO sessions (id, generation_id, genome_id, resolution_id, phenotype_id, state, created_at) VALUES (?, ?, ?, ?, ?, 'created', datetime('now'))" (str session-id) gen genome resolution phenotype])))


(defn- read-intent [session key & [limit]]
  {:intent/id (random-uuid)
   :intent/type :intent/memory-read
   :session/id session
   :phenotype/id "sha256:0000000000000000000000000000000000000000000000000000000000000000"
   :node/id :node/mem
   :cause/event-id 1
   :payload (cond-> {:memory/key key}
              limit (assoc :memory/limit limit))
   :budget {:max-steps 10}
   :metadata {}})

(defn- write-intent [session key content]
  {:intent/id (random-uuid)
   :intent/type :intent/memory-write
   :session/id session
   :phenotype/id "sha256:0000000000000000000000000000000000000000000000000000000000000000"
   :node/id :node/mem
   :cause/event-id 1
   :payload {:memory/key key :memory/content content}
   :budget {:max-steps 10}
   :metadata {}})

(defn- normalized [p intent]
  (evoclj.provider.protocol/normalize-request p intent))

(defn- execute [p intent]
  (evoclj.provider.protocol/execute-request!
   p (evoclj.provider.protocol/normalize-request p intent)))

(deftest write-then-read-round-trip
  (testing "a write then a read in the SAME session returns the EDN content"
    (let [db (temp-db)
          p (mem/memory-provider {:store db})
          s (sid 1)
          _ (ensure-session! db s)
          _ (execute p (write-intent s :note {:text "hello" :n 1}))
          out (execute p (read-intent s :note))]
      (is (true? (:memory/found out)))
      (is (= {:text "hello" :n 1} (:memory/content out))))))

(deftest missing-key-returns-found-false
  (testing "a read of an unwritten key reports found false with nil content"
    (let [db (temp-db)
          p (mem/memory-provider {:store db})
          out (execute p (read-intent (sid 1) :never-written))]
      (is (false? (:memory/found out)))
      (is (nil? (:memory/content out))))))

(deftest write-upserts-per-session-and-key
  (testing "a second write to the same (session, key) overwrites"
    (let [db (temp-db)
          p (mem/memory-provider {:store db})
          s (sid 1)
          _ (ensure-session! db s)
          _ (execute p (write-intent s :k {:v 1}))
          _ (execute p (write-intent s :k {:v 2}))
          out (execute p (read-intent s :k))]
      (is (= {:v 2} (:memory/content out))))))

(deftest sessions-are-isolated
  (testing "a session CANNOT read another session's memory (feature R2"
    (let [db (temp-db)
          p (mem/memory-provider {:store db})
          _ (ensure-session! db (sid 1))
          _ (ensure-session! db (sid 2))
          _ (execute p (write-intent (sid 1) :k "secret"))
          out (execute p (read-intent (sid 2) :k))]
      (is (false? (:memory/found out)))
      (is (nil? (:memory/content out))))))

(deftest normalized-request-shape
  (testing "normalize-request yields the canonical {:kind :memory :id key} resource"
    (let [db (temp-db)
          p (mem/memory-provider {:store db})
          n (normalized p (read-intent (sid 1) :k))]
      (is (= :memory/kv (:tool/id n)))
      (is (= {:kind :memory :id :k} (:resource n)))
      (is (= :read (get-in n [:args :memory/op]))))
    (let [db (temp-db)
          p (mem/memory-provider {:store db})
          n (normalized p (write-intent (sid 1) :k 42))]
      (is (= {:kind :memory :id :k} (:resource n)))
      (is (= :write (get-in n [:args :memory/op])))))
  (testing "the session id is threaded into the normalized args"
    (let [db (temp-db)
          p (mem/memory-provider {:store db})
          n (normalized p (write-intent (sid 7) :k 1))]
      (is (= (sid 7) (get-in n [:args :session/id]))))))

(deftest execution-count-bumps
  (testing "execute-request! bumps the execution counter"
    (let [db (temp-db)
          c (atom 0)
          p (mem/memory-provider {:store db :execution-count c})
          _ (ensure-session! db (sid 1))
          _ (execute p (write-intent (sid 1) :k 1))
          _ (execute p (read-intent (sid 1) :k))]
      (is (= 2 @c)))))

(deftest no-store-config-fails
  (testing "the provider requires a :store spec"
    (let [e (try (mem/memory-provider {}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :provider/config-invalid (:error/type (ex-data e)))))))