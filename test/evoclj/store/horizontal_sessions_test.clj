(ns evoclj.store.horizontal-sessions-test
  "Horizontal integration: narrow handles reject raw maps and preserve FK boundaries."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.jdbc :as jdbc]
            [evoclj.store.session :as session]
            [evoclj.store.session-store :as ss]
            [evoclj.store.enrichment :as enrich]
            [evoclj.store.enrichment-store :as es]
            [evoclj.store.memory-store :as mem]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]))

(defn- temp-db []
  (let [f (java.io.File/createTempFile "horiz-" ".db")
        p (.getAbsolutePath f)]
    (.delete f)
    (let [db (sqlite/spec p)]
      (migrate/migrate! db)
      db)))

(defn- temp-cas []
  (str (java.nio.file.Files/createTempDirectory
        "horiz-cas-"
        (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- sha [c]
  (str "sha256:" (apply str (repeat 64 c))))

(defn- seed-artifact!
  [db hash]
  (jdbc/insert! db :artifacts
                {:hash hash :media_type "application/octet-stream"
                 :size 0 :created_at "2025-01-01T00:00:00Z"})
  (jdbc/insert! db :genomes
                {:id hash :created_at "2025-01-01T00:00:00Z"}))

(defn- seed-generation!
  [db gen-id genome resolution]
  (seed-artifact! db genome)
  (seed-artifact! db resolution)
  (jdbc/insert! db :generations
                {:id gen-id :genome_id genome :resolution_id resolution
                 :parent_id nil :state "active" :current 0
                 :created_at "2025-01-01T00:00:00Z"}))

(defn- session-request
  [gen-id genome resolution phenotype]
  {:generation/id gen-id :genome/id genome :resolution/id resolution
   :phenotype/id phenotype})

;; ---------------------------------------------------------------------------
;; R: narrow handles — raw maps rejected
;; ---------------------------------------------------------------------------

(deftest session-store-rejects-raw-map
  (testing "Session requires a SessionStore handle at the boundary"
    (let [db (temp-db)
          raw {:sqlite db}
          request (session-request "g" (sha "a") (sha "c") (sha "b"))
          error (try
                  (session/create-session! raw request)
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (= :store/session-invalid (:error/type (ex-data error))))
      (is (= :not-a-session-store (:reason (ex-data error)))))))

(deftest enrichment-store-rejects-raw-map
  (testing "Enrichment requires an EnrichmentStore handle"
    (let [db (temp-db)
          cas-root (temp-cas)
          raw {:cas cas-root}
          request {:entity/kind :genome :entity/id (sha "a")
                   :kind :case/weight :payload {:w 0.5} :cause nil}
          error (try
                  (enrich/put-enrichment! raw request)
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (= :enrichment/store-invalid (:error/type (ex-data error)))))))

(deftest memory-store-rejects-raw-map
  (testing "Memory requires a MemoryStore handle"
    (let [db (temp-db)
          store (mem/make-memory-store db)
          error (try
                  (mem/memory-write! {:memory store} (random-uuid) :key "value")
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (= :store/memory-invalid (:error/type (ex-data error)))))))

;; ---------------------------------------------------------------------------
;; P5/F: FK existence
;; ---------------------------------------------------------------------------

(deftest session-fk-existence
  (testing "session creation requires an existing generation and pinned artifacts"
    (let [db (temp-db)
          gen-id "g-1"
          genome (sha "a")
          resolution (sha "c")
          phenotype (sha "b")
          store (ss/make-session-store db)
          request (session-request gen-id genome resolution phenotype)
          missing (try
                   (session/create-session! store request)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
      (is (= :store/generation-not-found (:error/type (ex-data missing))))
      (seed-artifact! db phenotype)
      (seed-generation! db gen-id genome resolution)
      (let [s (session/create-session! store request)]
        (is (uuid? (:session/id s)))
        (is (= gen-id (:generation/id s)))))))

(deftest memory-fk-to-session
  (testing "memory writes require a valid session id"
    (let [db (temp-db)
          gen-id "g-1"
          genome (sha "a")
          resolution (sha "c")
          phenotype (sha "b")
          store (ss/make-session-store db)
          request (session-request gen-id genome resolution phenotype)]
      (seed-artifact! db phenotype)
      (seed-generation! db gen-id genome resolution)
      (let [s (session/create-session! store request)
            memory-store (mem/make-memory-store db)]
        (is (seq (mem/memory-write! memory-store (:session/id s) :key {:text "hello"})))
        (is (some? (mem/memory-read memory-store (:session/id s) :key)))
        (try
          (mem/memory-write! memory-store (random-uuid) :key {:text "oops"})
          (is false "unknown session was unexpectedly accepted")
          (catch java.sql.SQLException _
            (is true "foreign key rejects unknown session")))))))

;; ---------------------------------------------------------------------------
;; Opaque handles
;; ---------------------------------------------------------------------------

(deftest session-store-opaque
  (let [store (ss/make-session-store (temp-db))]
    (is (nil? (:db store)))
    (is (nil? (:sqlite store)))))

(deftest enrichment-store-opaque
  (let [store (es/make-enrichment-store (temp-db) (temp-cas))]
    (is (nil? (:db store)))
    (is (nil? (:cas store)))))

(deftest enrichment-via-handle-roundtrip
  (testing "enrichment put, latest, and CAS payload use the opaque handle"
    (let [store (es/make-enrichment-store (temp-db) (temp-cas))
          request {:entity/kind :genome :entity/id (sha "a")
                   :kind :case/weight :payload {:w 0.5} :cause nil}
          rec (enrich/put-enrichment! store request)
          latest (enrich/latest-enrichment store :genome (sha "a") :case/weight)]
      (is (uuid? (:enrichment/id rec)))
      (is (= 1 (:version rec)))
      (is (= rec latest))
      (is (= {:w 0.5} (enrich/payload store rec))))))