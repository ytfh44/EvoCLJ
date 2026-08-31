(ns evoclj.store.command-dispatch-test
  "A3 — command dispatch state-machine tests.

  Verifies Wolfram [W-20..W-24] via the store layer:

    queued -> {running,failed,cancelled}
    running -> {succeeded,failed,timed-out,cancelled}
    terminals have no outgoing (enforced via CHECK + code guards).

  Five required checks:
    1) dispatch queued->running succeeds
    2) dispatch non-queued (already running) fails with :store/invalid-transition
    3) succeed running->succeeded
    4) fail queued->failed
    5) terminal cannot transition (succeeded->running fails)"
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.command :as cmd]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; -- fixtures ---------------------------------------------------------------

(def ^:private db-paths (atom []))

(defn- temp-db-path
  []
  (let [f (java.io.File/createTempFile "command-dispatch-test-" ".db")]
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

(defn- command-for
  [owner-sid overrides]
  (merge {:cmd/id (random-uuid)
          :cmd/type :tool/invoke
          :cmd/state :queued
          :cmd/idempotency-key (str "idem-" (random-uuid))
          :cmd/payload-ref (str "sha256:" (apply str (repeat 64 "a")))
          :cmd/owner-session-id owner-sid
          :cmd/created-at (java.util.Date.)}
         overrides))

;; -- 1) dispatch queued -> running succeeds --------------------------------

(deftest dispatch-queued-to-running-succeeds
  (testing "dispatch-command! transitions queued -> running and returns updated row"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          before (cmd/fetch-command db cid)
          _ (is (= :queued (:cmd/state before)) "precondition: queued")
          dispatched (cmd/dispatch-command! db cid)
          after (cmd/fetch-command db cid)]
      (is (= :running (:cmd/state dispatched)) "dispatch return should be running")
      (is (= :running (:cmd/state after)) "persisted state should be running")
      (is (= cid (:cmd/id after)) "id unchanged")
      (testing "fetch-commands-by-state reflects transition"
        (is (empty? (cmd/fetch-commands-by-state db :queued)) "no queued left for this cid")
        (is (= 1 (count (cmd/fetch-commands-by-state db :running))) "one running")))))

;; -- 2) dispatch non-queued (already running) fails ------------------------

(deftest dispatch-non-queued-fails-with-invalid-transition
  (testing "dispatching an already-running command throws :store/invalid-transition"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          ex (try
               (cmd/dispatch-command! db cid)
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "second dispatch must throw")
      (is (= :store/invalid-transition (:error/type (ex-data ex))) "typed error")
      (is (= :running (:cmd/state (cmd/fetch-command db cid))) "state unchanged after failed dispatch"))))

;; -- 3) succeed running -> succeeded ---------------------------------------

(deftest succeed-running-to-succeeded
  (testing "succeed-command! transitions running -> succeeded"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          result-ref (str "sha256:" (apply str (repeat 64 "b")))
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          succeeded (cmd/succeed-command! db cid result-ref)
          after (cmd/fetch-command db cid)]
      (is (= :succeeded (:cmd/state succeeded)) "return should be succeeded")
      (is (= :succeeded (:cmd/state after)) "persisted should be succeeded")
      (is (= cid (:cmd/id after)) "id unchanged"))))

(deftest succeed-non-running-fails
  (testing "succeed-command! on a queued command (not running) throws :store/invalid-transition"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          ex (try
               (cmd/succeed-command! db cid (str "sha256:" (apply str (repeat 64 "c"))))
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "must throw when not running")
      (is (= :store/invalid-transition (:error/type (ex-data ex))))
      (is (= :queued (:cmd/state (cmd/fetch-command db cid))) "state unchanged"))))

;; -- 4) fail queued -> failed ----------------------------------------------

(deftest fail-queued-to-failed
  (testing "fail-command! transitions queued -> failed"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          failed (cmd/fail-command! db cid "boom")
          after (cmd/fetch-command db cid)]
      (is (= :failed (:cmd/state failed)))
      (is (= :failed (:cmd/state after))))))

(deftest fail-running-to-failed
  (testing "fail-command! transitions running -> failed"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          failed (cmd/fail-command! db cid "oops")
          after (cmd/fetch-command db cid)]
      (is (= :failed (:cmd/state failed)))
      (is (= :failed (:cmd/state after))))))

;; -- 5) terminal cannot transition -----------------------------------------

(deftest terminal-cannot-transition
  (testing "succeeded is terminal: dispatch must fail with :store/invalid-transition"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          _ (cmd/succeed-command! db cid (str "sha256:" (apply str (repeat 64 "d"))))
          _ (is (= :succeeded (:cmd/state (cmd/fetch-command db cid))) "precondition: succeeded")
          ex1 (try (cmd/dispatch-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex2 (try (cmd/succeed-command! db cid nil) nil (catch clojure.lang.ExceptionInfo e e))
          ex3 (try (cmd/fail-command! db cid "late") nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex1) "dispatch after succeeded must throw")
      (is (= :store/invalid-transition (:error/type (ex-data ex1))))
      (is (some? ex2) "succeed after succeeded must throw")
      (is (= :store/invalid-transition (:error/type (ex-data ex2))))
      (is (some? ex3) "fail after succeeded must throw")
      (is (= :store/invalid-transition (:error/type (ex-data ex3))))
      (is (= :succeeded (:cmd/state (cmd/fetch-command db cid))) "terminal state unchanged")))
  (testing "failed is terminal"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/fail-command! db cid "boom")
          _ (is (= :failed (:cmd/state (cmd/fetch-command db cid))))
          ex (try (cmd/dispatch-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex))
      (is (= :store/invalid-transition (:error/type (ex-data ex)))))))

;; -- fetch-commands-by-state helper ----------------------------------------

(deftest fetch-commands-by-state-returns-filtered
  (testing "fetch-commands-by-state filters correctly"
    (let [db (fresh-db)
          sid (seed-session! db)
          q1 (random-uuid)
          q2 (random-uuid)
          r1 (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id q1 :cmd/state :queued}))
          _ (cmd/create-command! db (command-for sid {:cmd/id q2 :cmd/state :queued}))
          _ (cmd/create-command! db (command-for sid {:cmd/id r1 :cmd/state :queued}))
          _ (cmd/dispatch-command! db r1)
          queued (cmd/fetch-commands-by-state db :queued)
          running (cmd/fetch-commands-by-state db :running)]
      (is (= 2 (count queued)) "two queued remain")
      (is (= 1 (count running)) "one running")
      (is (every? #(= :queued (:cmd/state %)) queued))
      (is (every? #(= :running (:cmd/state %)) running)))))
