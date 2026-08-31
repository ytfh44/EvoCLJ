(ns evoclj.environment.registry-async-test
  "A6 — refresh-async! via command queue (auditable, no raw future leak)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.source :as src]
            [evoclj.environment.static :as static]
            [evoclj.store.command :as cmd]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; ---------------------------------------------------------------------------
;; helpers — temp db + session seed (mirrors command_test seed)
;; ---------------------------------------------------------------------------

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [f (java.io.File/createTempFile "registry-async-test-" ".db")]
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
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}
                          :created-at (java.util.Date/from (java.time.Instant/parse now))})
    sid))

;; ---------------------------------------------------------------------------
;; 1 — refresh-async! returns a command map, not a raw future leak
;; ---------------------------------------------------------------------------

(deftest refresh-async-returns-command-not-future
  (let [registry (reg/create-registry)
        src (static/make-static-source :async/test {:hello 1})
        sid (reg/register-source! registry src)]
    (let [ret (reg/refresh-async! registry sid)]
      (is (map? ret) "refresh-async! must return a command map")
      (is (some? (:cmd/id ret)) "command map carries :cmd/id")
      (is (= :environment/refresh (:cmd/type ret)) "command type is :environment/refresh")
      (is (= :queued (:cmd/state ret)) "initial command state is :queued")
      (is (string? (:cmd/idempotency-key ret)) "has idempotency-key")
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:cmd/payload-ref ret)) "payload-ref is sha256")
      (is (not (future? ret)) "must NOT leak a raw future as return value")
      (is (= ret (:last-command @registry)) "registry :last-command holds the returned command")
      (is (seq (:command-queue @registry)) "command-queue is populated")
      (let [fut (:last-refresh-future @registry)]
        (is (future? fut) "background work is tracked as :last-refresh-future")
        (deref fut 5000 nil)
        (is (= 1 (get-in @registry [:per-source sid :seq])) "async refresh published (seq advanced)")))))

(deftest refresh-async-nil-source-id-also-auditable
  (let [registry (reg/create-registry)
        src (static/make-static-source :async/all {:x 2})]
    (reg/register-source! registry src)
    (let [ret (reg/refresh-async! registry)]
      (is (map? ret))
      (is (:cmd/id ret))
      (is (not (future? ret)))
      (let [fut (:last-refresh-future @registry)]
        (deref fut 5000 nil)
        (is (pos? (get-in @registry [:per-source :async/all :seq])))))))

;; ---------------------------------------------------------------------------
;; 2 — with a durable store the command is persisted and transitions
;; ---------------------------------------------------------------------------

(deftest refresh-async-persists-command-when-store-wired
  (let [db (fresh-db)
        _sid (seed-session! db)
        registry (reg/create-registry {:store db})
        src (static/make-static-source :async/durable {:durable true})
        sid (reg/register-source! registry src)]
    (try
      (let [ret (reg/refresh-async! registry sid)]
        (is (map? ret))
        (is (:cmd/id ret))
        (let [row (cmd/fetch-command db (:cmd/id ret))]
          (is (some? row) "command row was persisted to SQLite")
          (is (= :environment/refresh (:cmd/type row)))
          (is (string? (:cmd/idempotency-key row))))
        (let [fut (:last-refresh-future @registry)]
          (deref fut 5000 nil)
          (Thread/sleep 100)
          (let [row2 (cmd/fetch-command db (:cmd/id ret))]
            (is (= :succeeded (:cmd/state row2)) "command reaches :succeeded after refresh! completes"))))
      (finally
        (cleanup!)))))

(deftest refresh-async-command-lifecycle-on-throw-becomes-failed
  (let [db (fresh-db)
        _sid (seed-session! db)
        registry (reg/create-registry {:store db})
        src (static/make-static-source :async/good {:good true})
        _good-sid (reg/register-source! registry src)]
    (try
      (let [ret (reg/refresh-async! registry :async/missing)
            fut (:last-refresh-future @registry)]
        (try (deref fut 5000 nil) (catch Exception _ nil))
        (Thread/sleep 100)
        (let [row (cmd/fetch-command db (:cmd/id ret))]
          (is (= :failed (:cmd/state row)) "failed refresh (no such source) marks command :failed")))
      (finally (cleanup!)))))
