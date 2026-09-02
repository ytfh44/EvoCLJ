(ns evoclj.store.command-timeout-test
  "A4 — command timeout/cancel transitions + deadline helper.

  Verifies Wolfram [W-20..W-24] A4 additions:

    queued -> {running,failed,cancelled}
    running -> {succeeded,failed,timed-out,cancelled}
    timed-out / cancelled are terminal (no outgoing)
    queued -> timed-out is NOT allowed

  Five checks (spec):
    1) timeout running->timed-out succeeds
    2) timeout queued->timed-out fails (invalid-transition)
    3) cancel queued->cancelled succeeds
    4) cancel running->cancelled succeeds
    5) terminal cannot timeout/cancel (and deadline-passed? helper)"
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.command :as cmd]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; -- fixtures (copied from command-dispatch-test for isolation) ---------------

(def ^:private db-paths (atom []))

(defn- temp-db-path
  []
  (let [f (java.io.File/createTempFile "command-timeout-test-" ".db")]
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
                          :prev/event-id nil
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

;; -- 1) timeout running -> timed-out succeeds --------------------------------

(deftest timeout-running-to-timed-out-succeeds
  (testing "timeout-command! transitions running -> timed-out and returns updated row"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          _ (is (= :running (:cmd/state (cmd/fetch-command db cid))) "precondition: running")
          timed (cmd/timeout-command! db cid)
          after (cmd/fetch-command db cid)]
      (is (= :timed-out (:cmd/state timed)) "timeout return should be timed-out")
      (is (= :timed-out (:cmd/state after)) "persisted state should be timed-out")
      (is (= cid (:cmd/id after)) "id unchanged"))))

;; -- 2) timeout queued -> fails (queued -> timed-out not in SM) --------------

(deftest timeout-queued-fails
  (testing "timeout-command! on a queued command throws :store/invalid-transition (SM: queued->{running,failed,cancelled} only)"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          ex (try (cmd/timeout-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "timeout on queued must throw")
      (is (= :store/invalid-transition (:error/type (ex-data ex))) "typed error")
      (is (= :queued (:cmd/state (cmd/fetch-command db cid))) "state unchanged after failed timeout"))))

;; -- 3) cancel queued -> cancelled succeeds ----------------------------------

(deftest cancel-queued-to-cancelled-succeeds
  (testing "cancel-command! transitions queued -> cancelled"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          cancelled (cmd/cancel-command! db cid)
          after (cmd/fetch-command db cid)]
      (is (= :cancelled (:cmd/state cancelled)) "return should be cancelled")
      (is (= :cancelled (:cmd/state after)) "persisted should be cancelled")
      (is (= cid (:cmd/id after))))))

;; -- 4) cancel running -> cancelled succeeds ---------------------------------

(deftest cancel-running-to-cancelled-succeeds
  (testing "cancel-command! transitions running -> cancelled"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          _ (is (= :running (:cmd/state (cmd/fetch-command db cid))) "precondition: running")
          cancelled (cmd/cancel-command! db cid)
          after (cmd/fetch-command db cid)]
      (is (= :cancelled (:cmd/state cancelled)))
      (is (= :cancelled (:cmd/state after))))))

;; -- 5) terminal cannot timeout or cancel ------------------------------------

(deftest terminal-cannot-timeout-or-cancel
  (testing "succeeded is terminal: timeout/cancel must fail"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          _ (cmd/succeed-command! db cid (str "sha256:" (apply str (repeat 64 "b"))))
          _ (is (= :succeeded (:cmd/state (cmd/fetch-command db cid))) "precondition: succeeded")
          ex1 (try (cmd/timeout-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex2 (try (cmd/cancel-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex3 (try (cmd/dispatch-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex4 (try (cmd/fail-command! db cid "late") nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex1) "timeout after succeeded must throw")
      (is (= :store/invalid-transition (:error/type (ex-data ex1))))
      (is (some? ex2) "cancel after succeeded must throw")
      (is (= :store/invalid-transition (:error/type (ex-data ex2))))
      (is (some? ex3) "dispatch after succeeded must throw")
      (is (some? ex4) "fail after succeeded must throw")
      (is (= :succeeded (:cmd/state (cmd/fetch-command db cid))) "terminal unchanged")))
  (testing "failed is terminal"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/fail-command! db cid "boom")
          _ (is (= :failed (:cmd/state (cmd/fetch-command db cid))))
          ex1 (try (cmd/timeout-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex2 (try (cmd/cancel-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex1))
      (is (= :store/invalid-transition (:error/type (ex-data ex1))))
      (is (some? ex2))
      (is (= :store/invalid-transition (:error/type (ex-data ex2))))
      (is (= :failed (:cmd/state (cmd/fetch-command db cid))))))
  (testing "timed-out is terminal"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid)
          _ (cmd/timeout-command! db cid)
          _ (is (= :timed-out (:cmd/state (cmd/fetch-command db cid))) "precondition: timed-out")
          ex1 (try (cmd/timeout-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex2 (try (cmd/cancel-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex3 (try (cmd/succeed-command! db cid nil) nil (catch clojure.lang.ExceptionInfo e e))
          ex4 (try (cmd/fail-command! db cid "late") nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex1) "second timeout must fail")
      (is (= :store/invalid-transition (:error/type (ex-data ex1))))
      (is (some? ex2) "cancel after timed-out must fail")
      (is (= :store/invalid-transition (:error/type (ex-data ex2))))
      (is (some? ex3))
      (is (some? ex4))
      (is (= :timed-out (:cmd/state (cmd/fetch-command db cid))) "terminal unchanged")))
  (testing "cancelled is terminal"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/state :queued}))
          _ (cmd/cancel-command! db cid)
          _ (is (= :cancelled (:cmd/state (cmd/fetch-command db cid))))
          ex1 (try (cmd/cancel-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex2 (try (cmd/timeout-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))
          ex3 (try (cmd/dispatch-command! db cid) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex1))
      (is (= :store/invalid-transition (:error/type (ex-data ex1))))
      (is (some? ex2))
      (is (= :store/invalid-transition (:error/type (ex-data ex2))))
      (is (some? ex3))
      (is (= :cancelled (:cmd/state (cmd/fetch-command db cid)))))))

;; -- 6) deadline-passed? helper ----------------------------------------------

(deftest deadline-passed-helper
  (testing "deadline-passed? true when deadline before now, false otherwise"
    (let [past (java.util.Date. (- (System/currentTimeMillis) 60000))
          future (java.util.Date. (+ (System/currentTimeMillis) 60000))
          now (java.util.Date.)
          cmd-past {:cmd/deadline past}
          cmd-future {:cmd/deadline future}
          cmd-none {}
          cmd-nil {:cmd/deadline nil}]
      (is (true? (cmd/deadline-passed? cmd-past now)) "past deadline should be passed")
      (is (false? (cmd/deadline-passed? cmd-future now)) "future deadline should not be passed")
      (is (false? (cmd/deadline-passed? cmd-none now)) "no deadline -> false")
      (is (false? (cmd/deadline-passed? cmd-nil now)) "nil deadline -> false")
      (is (false? (cmd/deadline-passed? cmd-past past)) "deadline == now -> not passed (strict <)")
      (is (true? (cmd/deadline-passed? {:cmd/deadline (java.time.Instant/now)} (java.util.Date. (+ (System/currentTimeMillis) 1000)))) "Instant deadline past -> true")))
  (testing "deadline-passed? integrates with persisted command deadline"
    (let [db (fresh-db)
          sid (seed-session! db)
          past (java.util.Date. (- (System/currentTimeMillis) 5000))
          cid (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid :cmd/deadline past}))
          fetched (cmd/fetch-command db cid)
          now (java.util.Date.)]
      (is (true? (cmd/deadline-passed? fetched now)) "fetched deadline should be detected as passed")
      (is (= (.getTime ^java.util.Date past) (.getTime ^java.util.Date (:cmd/deadline fetched))) "deadline round-trips"))))

;; -- 7) cancel after timeout not allowed; timeout after cancel not allowed ----

(deftest cancel-and-timeout-mutual-exclusion
  (testing "after timeout, cancel fails; after cancel, timeout fails"
    (let [db (fresh-db)
          sid (seed-session! db)
          cid1 (random-uuid)
          cid2 (random-uuid)
          _ (cmd/create-command! db (command-for sid {:cmd/id cid1 :cmd/state :queued}))
          _ (cmd/create-command! db (command-for sid {:cmd/id cid2 :cmd/state :queued}))
          _ (cmd/dispatch-command! db cid1)
          _ (cmd/dispatch-command! db cid2)
          _ (cmd/timeout-command! db cid1)
          _ (cmd/cancel-command! db cid2)
          ex1 (try (cmd/cancel-command! db cid1) nil (catch clojure.lang.ExceptionInfo e e))
          ex2 (try (cmd/timeout-command! db cid2) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex1) "cancel after timed-out must fail")
      (is (= :store/invalid-transition (:error/type (ex-data ex1))))
      (is (some? ex2) "timeout after cancelled must fail")
      (is (= :store/invalid-transition (:error/type (ex-data ex2)))))))
