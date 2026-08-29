(ns evoclj.store.horizontal-sessions-test
  "Fleet horizontal — isomorphic gaps for Sessions/Enrichment/Memory.
  Verifies narrow handles, canonical states, FK existence, and raw-map rejection
  analogous to prior fleets R/S1/S2/P5."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.store.session :as session]
            [evoclj.store.session-states :as sstates]
            [evoclj.store.session-store :as ss]
            [evoclj.store.enrichment :as enrich]
            [evoclj.store.enrichment-store :as es]
            [evoclj.store.memory-store :as ms]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.cas :as cas]
            [evoclj.store.existence :as existence]
            [clojure.java.jdbc :as jdbc])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "horiz-" ".db" (make-array FileAttribute 0)))]
    (let [db (sqlite/spec p)] (migrate/migrate! db) db)))

(defn- temp-cas []
  (let [p (str (Files/createTempDirectory "horiz-cas-" (make-array FileAttribute 0)))] (cas/->cas p)))

(defn- sha [c] (str "sha256:" (apply str (repeat 64 c))))

(defn- seed-generation!
  [db genome resolution gen-id]
  (sqlite/with-db [conn db]
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" genome])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" resolution])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES (?, datetime('now'))" genome])
    (jdbc/insert! conn :generations {:id gen-id :genome_id genome :resolution_id resolution :parent_id nil :state "active" :current 0 :created_at "2025-01-01T00:00:00Z"})))

(defn- ensure-artifact!
  [db hash]
  (sqlite/with-db [conn db]
    (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" hash])
    (jdbc/execute! conn ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES (?, datetime('now'))" hash])))

;; ---------------------------------------------------------------------------
;; R: narrow handles — raw maps rejected
;; ---------------------------------------------------------------------------

(deftest session-store-rejects-raw-map
  (testing "raw {:sqlite ...} map is rejected (Fleet R)"
    (let [e (try (session/create-session! {:sqlite "x" :cas "y"} {:genome/id (sha "a") :resolution/id (sha "c") :phenotype/id (sha "b") :generation/id "g"}) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (= :store/session-invalid (:error/type (ex-data e))))
      (is (= :not-a-session-store (:reason (ex-data e)))))))

(deftest enrichment-store-rejects-raw-map
  (testing "enrichment raw map rejected"
    (let [cas (temp-cas)
          e (try (enrich/put-enrichment! {:sqlite "x" :cas cas} {:entity/kind :genome :entity/id "e" :kind :k :payload {}}) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (= :enrichment/store-invalid (:error/type (ex-data e)))))))

(deftest memory-store-rejects-raw-map
  (testing "MemoryStore handle required"
    (let [e (try (ms/memory-read {:sqlite "x"} (random-uuid) :k) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (= :store/memory-invalid (:error/type (ex-data e)))))))

;; ---------------------------------------------------------------------------
;; S2: canonical states single source
;; ---------------------------------------------------------------------------

(deftest session-states-canonical
  (testing "session-states is single source, session aliases it"
    (is (= sstates/session-states session/states))
    (is (= sstates/session-transitions session/transitions))
    (is (= sstates/terminal-states session/terminal-states))
    (is (sstates/session-state? :created))
    (is (not (sstates/session-state? :banana)))
    (is (= "created" (sstates/kw->db :created)))
    (is (= :created (sstates/db->kw "created")))
    (is (sstates/valid-transition? :created :resolving))
    (is (not (sstates/valid-transition? :created :running)))
    (is (= [:enum :budget-exhausted :cancelled :completed :created :failed :resolving :running :waiting] sstates/session-state-enum))))

;; ---------------------------------------------------------------------------
;; P5/F: FK existence
;; ---------------------------------------------------------------------------

(deftest session-fk-existence
  (testing "session creation with unknown generation fails"
    (let [db (temp-db)
          ss (ss/make-session-store db)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (session/create-session! ss {:genome/id (sha "a") :resolution/id (sha "c") :phenotype/id (sha "b") :generation/id "nope"})))))
  (testing "session pinned ids FK at rest — inserting without artifacts fails"
    (let [db (temp-db)
          ss (ss/make-session-store db)
          gen-id "g1"
          genome (sha "a")
          resolution (sha "c")
          phenotype (sha "b")]
      ;; seed generation with proper FKs
      (seed-generation! db genome resolution gen-id)
      (ensure-artifact! db phenotype)
      ;; happy path via SessionStore
      (let [s (session/create-session! ss {:genome/id genome :resolution/id resolution :phenotype/id phenotype :generation/id gen-id})]
        (is (= :created (:state s))))
      ;; raw SQL with bogus genome should fail FK
      (is (thrown? java.sql.SQLException
                   (sqlite/exec! db ["INSERT INTO sessions (id, generation_id, genome_id, resolution_id, phenotype_id, state, created_at) VALUES (?, ?, ?, ?, ?, 'created', datetime('now'))"
                                     (str (random-uuid)) gen-id (sha "f") resolution phenotype]))))))

(deftest memory-fk-to-session
  (testing "episodic_memory FK to sessions — write for unknown session fails"
    (let [db (temp-db)
          mem (ms/make-memory-store db)]
      (is (thrown? java.sql.SQLException
                   (ms/memory-write! mem (random-uuid) :k {:v 1})))))
  (testing "write then read via MemoryStore after session exists"
    (let [db (temp-db)
          ss (ss/make-session-store db)
          mem (ms/make-memory-store db)
          gen-id "g2"
          genome (sha "a")
          resolution (sha "c")
          phenotype (sha "b")]
      (seed-generation! db genome resolution gen-id)
      (ensure-artifact! db phenotype)
      (let [s (session/create-session! ss {:genome/id genome :resolution/id resolution :phenotype/id phenotype :generation/id gen-id})
            sid (:session/id s)]
        (ms/memory-write! mem sid :note {:text "hello"})
        (let [row (ms/memory-read mem sid :note)]
          (is (= "{:text \"hello\"}" (:content row))))))))

;; ---------------------------------------------------------------------------
;; SessionStore opaque — (:db handle) is nil
;; ---------------------------------------------------------------------------

(deftest session-store-opaque
  (testing "SessionStore does not expose :db via keyword access"
    (let [s (ss/make-session-store "x")]
      (is (nil? (:db s)))
      (is (nil? (:sqlite s))))))

(deftest enrichment-store-opaque
  (testing "EnrichmentStore does not expose :db/:sqlite via keyword"
    (let [cas (temp-cas) store (es/make-enrichment-store "x" cas)]
      (is (nil? (:db store)))
      (is (nil? (:sqlite store)))
      (is (nil? (:cas store))))))

;; ---------------------------------------------------------------------------
;; Enrichment via handle — put + latest
;; ---------------------------------------------------------------------------

(deftest enrichment-via-handle-roundtrip
  (let [db (temp-db) cas (temp-cas) store (es/make-enrichment-store db cas)]
    (let [rec (enrich/put-enrichment! store {:entity/kind :genome :entity/id (sha "a") :kind :case/weight :payload {:w 0.5} :cause nil})]
      (is (= 1 (:version rec)))
      (is (= {:w 0.5} (enrich/payload store rec)))
      (is (= rec (enrich/latest-enrichment store :genome (sha "a") :case/weight))))))