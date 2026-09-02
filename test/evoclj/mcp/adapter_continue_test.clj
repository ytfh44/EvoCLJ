(ns evoclj.mcp.adapter-continue-test
  "A6 — MCP Tasks continue wiring: 2025 degrades to queued, 2026 keeps continuing but audited."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [evoclj.mcp.adapter :as adapter]
            [evoclj.store.command :as cmd]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; ---------------------------------------------------------------------------
;; helpers — temp db + session seed
;; ---------------------------------------------------------------------------

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [f (java.io.File/createTempFile "adapter-continue-test-" ".db")]
    (.deleteOnExit f)
    (let [p (.getAbsolutePath f)]
      (swap! db-paths conj p)
      p)))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try (io/delete-file p true) (catch Exception _ nil)))
  (reset! db-paths []))

(def ^:private gen-id "generation-1")
(def ^:private genome-id (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "c"))))

(defn- fresh-db []
  (let [db (temp-db-path)]
    (migrate/migrate! db)
    db))

(defn- seed-session! [db]
  (let [sid (random-uuid)
        now "2025-01-01T00:00:00Z"]
    (sqlite/with-db [conn db]
      (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen-id]))
        (try (jdbc/insert! conn :artifacts {:hash genome-id :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
        (try (jdbc/insert! conn :artifacts {:hash resolution-id :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
        (try (jdbc/insert! conn :artifacts {:hash phenotype-id :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
        (try (jdbc/insert! conn :genomes {:id genome-id :created_at now}) (catch Exception _ nil))
        (jdbc/insert! conn :generations
                      {:id gen-id
                       :genome_id genome-id
                       :resolution_id resolution-id
                       :parent_id nil
                       :state "active"
                       :current 1
                       :created_at now}))
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id gen-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :phenotype_id phenotype-id
                     :state "created"
                     :created_at now
                     :updated_at nil}))
    (event/append-event! db
                         {:session/id sid
                          :generation/id gen-id
                          :phenotype/id phenotype-id
                          :event/type :session/created
                          :prev/event-id nil
                          :payload-ref nil
                          :metadata {}
                          :created-at (java.util.Date/from (java.time.Instant/parse now))})
    sid))

;; ---------------------------------------------------------------------------
;; 1 — 2025 no longer throws :mcp/not-supported, returns queued
;; ---------------------------------------------------------------------------

(deftest continue-2025-queues-instead-of-throwing
  (let [a (adapter/adapter-2025)]
    (let [ret (adapter/continue a {:id 1 :op :test})]
      (is (map? ret))
      (is (= :queued (:status ret)) "2025 continue must degrade to :queued")
      (is (uuid? (:command-id ret)) "must carry a command-id")
      (is (= :mcp/continue (:cmd/type (:command ret))) "command type is :mcp/continue")
      (is (string? (:cmd/idempotency-key (:command ret))) "command has idempotency-key")
      (is (= :mcp-2025-11 (:adapter ret))))))

(deftest continue-2025-does-not-throw
  (let [a (adapter/adapter-2025)
        ret (try (adapter/continue a {:id 2}) (catch Exception e e))]
    (is (map? ret) "2025 continue must NOT throw :mcp/not-supported")
    (is (not (instance? Throwable ret)) "must not be an exception")))

;; ---------------------------------------------------------------------------
;; 2 — 2026 keeps :continuing but also audits via command
;; ---------------------------------------------------------------------------

(deftest continue-2026-keeps-continuing-but-audits
  (let [a (adapter/adapter-2026 {:ttl-ms 60000})]
    (let [ret (adapter/continue a {:id 99})]
      (is (= :continuing (:status ret)))
      (is (= :mcp-2026-07 (:adapter ret)))
      (is (uuid? (:command-id ret)) "2026 also carries a command-id for audit")
      (is (= :mcp/continue (:cmd/type (:command ret))))
      (is (= {:id 99} (:task ret))))))

;; ---------------------------------------------------------------------------
;; 3 — with a durable store the command is persisted
;; ---------------------------------------------------------------------------

(deftest continue-2025-persists-when-store-wired
  (let [db (fresh-db)
        _sid (seed-session! db)
        a (adapter/adapter-2025 {:store db})]
    (try
      (let [ret (adapter/continue a {:id 42 :op :with-store})]
        (is (= :queued (:status ret)))
        (let [row (cmd/fetch-command db (:command-id ret))]
          (is (some? row) "command was persisted to SQLite")
          (is (= :mcp/continue (:cmd/type row)))
          (is (= :queued (:cmd/state row)))))
      (finally (cleanup!)))))

(deftest continue-2026-persists-when-store-wired
  (let [db (fresh-db)
        _sid (seed-session! db)
        a (adapter/adapter-2026 {:store db :ttl-ms 60000})]
    (try
      (let [ret (adapter/continue a {:id 77})]
        (is (= :continuing (:status ret)))
        (let [row (cmd/fetch-command db (:command-id ret))]
          (is (some? row))
          (is (= :mcp/continue (:cmd/type row)))))
      (finally (cleanup!)))))
