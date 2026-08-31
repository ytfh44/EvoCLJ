(ns evoclj.store.command-recovery-test
  "A5 — command recovery for orphaned queued/running commands
   (store/recovery.clj).

  When the process dies, a command may be left in :queued (submitted but
  never dispatched) or :running (dispatched but never settled). Recovery
  classifies both as crash residue and — following the store's \"report,
  not fabricate completion\" discipline — leaves :queued orphans queued
  for redelivery (idempotency_key UNIQUE de-duplicates a resubmit) and
  marks :running orphans :failed with {:error/type :recovery/orphaned}.
  Recovery NEVER fabricates :succeeded, and terminal commands
  (:succeeded) are untouched."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.command :as cmd]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.sqlite :as sqlite]))

(def ^:private db-paths (atom []))

(defn- temp-db-path
  []
  (let [f (java.io.File/createTempFile "command-recovery-test-" ".db")]
    (.deleteOnExit f)
    (let [p (.getAbsolutePath f)]
      (swap! db-paths conj p)
      p)))

(defn- cleanup!
  []
  (doseq [p @db-paths]
    (try (io/delete-file p true) (catch Exception _ nil)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  []
  (let [db (temp-db-path)]
    (migrate/migrate! db)
    db))

(def ^:private gen-id "generation-1")
(def ^:private genome-id (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "c"))))

(defn- seed-session!
  "Insert a generation + session suitable for command FK + event chain.
   Returns session-id UUID."
  [db]
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
    sid))

(defn- command-for
  "A valid command map pinned to `owner-sid` with optional overrides."
  [owner-sid overrides]
  (merge {:cmd/id (random-uuid)
          :cmd/type :tool/invoke
          :cmd/state :queued
          :cmd/idempotency-key (str "idem-" (random-uuid))
          :cmd/payload-ref (str "sha256:" (apply str (repeat 64 "a")))
          :cmd/owner-session-id owner-sid
          :cmd/created-at (java.util.Date.)}
         overrides))

;; ---------------------------------------------------------------------------
;; 1) orphaned :queued command remains queued after recover
;; ---------------------------------------------------------------------------

(deftest orphaned-queued-command-remains-queued
  (testing "find-orphaned-commands classifies only queued+running, never terminal"
    (let [db (fresh-db)
          sid (seed-session! db)
          queued (cmd/create-command! db (command-for sid {:cmd/state :queued}))
          running (cmd/create-command! db (command-for sid {:cmd/state :running}))
          done (cmd/create-command! db (command-for sid {:cmd/state :succeeded}))]
      (let [orphans (recovery/find-orphaned-commands db)]
        (is (= #{(:cmd/id queued) (:cmd/id running)}
               (set (map :cmd/id orphans)))
            "only queued and running commands are orphans; succeeded is not"))))
  (testing "recover-commands! leaves a queued orphan queued for redelivery"
    (let [db (fresh-db)
          sid (seed-session! db)
          q (cmd/create-command! db (command-for sid {:cmd/state :queued}))
          report (recovery/recover-commands! db)]
      (is (= [(:cmd/id q)] (:recovered-queued report)) "queued orphan reported for redelivery")
      (is (empty? (:recovered-running report)) "no running orphans recovered")
      (is (= :queued (:cmd/state (cmd/fetch-command db (:cmd/id q))))
          "row state untouched — still queued for redelivery")
      ;; the same idempotency_key can be resubmitted exactly once (deduped by UNIQUE)
      (let [resubmit (command-for sid {:cmd/idempotency-key (:cmd/idempotency-key q)
                                       :cmd/state :queued})]
        (try
          (cmd/create-command! db resubmit)
          (is false "resubmit with the reserved idempotency_key should be deduped")
          (catch clojure.lang.ExceptionInfo e
            (is (= :store/duplicate-command (:error/type (ex-data e)))
                "redelivery de-duplicated by idempotency_key UNIQUE")))))))

;; ---------------------------------------------------------------------------
;; 2) orphaned :running command becomes :failed with :recovery/orphaned
;; ---------------------------------------------------------------------------

(deftest orphaned-running-command-becomes-failed
  (testing "recover-commands! marks a running orphan failed with the recovery error"
    (let [db (fresh-db)
          sid (seed-session! db)
          r (cmd/create-command! db (command-for sid {:cmd/state :running}))
          report (recovery/recover-commands! db)]
      (is (= 1 (count (:recovered-running report))) "the running orphan was recovered")
      (is (= (:cmd/id r) (:cmd/id (first (:recovered-running report)))) "report names the orphan")
      (is (= recovery/orphan-command-error (:recovery/error (first (:recovered-running report))))
          "report carries the {:error/type :recovery/orphaned} marker")
      (is (empty? (:recovered-queued report)) "no queued orphans involved")
      (is (= :failed (:cmd/state (cmd/fetch-command db (:cmd/id r))))
          "running orphan row transitioned to :failed (reported, not fabricated)"))))

;; ---------------------------------------------------------------------------
;; 3) already succeeded command is untouched by recover
;; ---------------------------------------------------------------------------

(deftest succeeded-command-unaffected-by-recover
  (testing "terminal :succeeded command survives recovery unchanged"
    (let [db (fresh-db)
          sid (seed-session! db)
          done (cmd/create-command! db (command-for sid {:cmd/state :succeeded}))
          q (cmd/create-command! db (command-for sid {:cmd/state :queued}))
          report (recovery/recover-commands! db)]
      (is (= [(:cmd/id q)] (:recovered-queued report)) "only the queued orphan is touched")
      (is (empty? (:recovered-running report)))
      (is (= :succeeded (:cmd/state (cmd/fetch-command db (:cmd/id done))))
          "succeeded command row unchanged — recovery never fabricates or rewrites terminal success"))))