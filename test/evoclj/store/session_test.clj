(ns evoclj.store.session-test
  "Session identity-contract tests (W2: Work owns lifecycle)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.work :as work-store]))

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [f (java.io.File/createTempFile "evoclj-session-test-" ".db")
        p (.getAbsolutePath f)]
    (.delete f)
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try
      (java.nio.file.Files/deleteIfExists
       (java.nio.file.Paths/get p (make-array String 0)))
      (catch Exception _ nil)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (try (f) (finally (cleanup!)))))

(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- seed-generation! [db]
  (artifact/ensure-artifact! db genome "application/octet-stream" 0)
  (artifact/ensure-artifact! db resolution "application/edn" 0)
  (artifact/ensure-artifact! db phenotype "application/edn" 0)
  (artifact/ensure-genome! db genome)
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :generations
                  {:id gen
                   :genome_id genome
                   :resolution_id resolution
                   :parent_id nil
                   :state "active"
                   :current 1
                   :created_at now})))

(defn- session-request
  [& overrides]
  (merge {:generation/id gen
          :genome/id genome
          :resolution/id resolution
          :phenotype/id phenotype}
         (apply merge overrides)))

(defn- tx-error [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest session-pins-identity-at-creation
  (let [db (fresh-db)
        _ (seed-generation! db)
        created-at (java.util.Date. 1700000000000)
        s (session/create-session! db (assoc (session-request) :created-at created-at))
        fetched (session/get-session db (:session/id s))]
    (is (instance? java.util.UUID (:session/id s)))
    (is (= gen (:generation/id s)))
    (is (= [genome resolution phenotype]
           [(:genome/id s) (:resolution/id s) (:phenotype/id s)]))
    (is (= created-at (:created-at s)))
    (is (not (contains? s :state)))
    (is (= [genome resolution phenotype gen]
           [(:genome/id fetched) (:resolution/id fetched)
            (:phenotype/id fetched) (:generation/id fetched)]))))

(deftest supplied-session-id-round-trips
  (let [db (fresh-db)
        _ (seed-generation! db)
        sid (java.util.UUID/randomUUID)
        s (session/create-session! db (assoc (session-request) :session/id sid))]
    (is (= sid (:session/id s)))
    (is (= s (session/find-session db sid)))
    (is (true? (session/session-exists? db sid)))))

(deftest unknown-session-is-not-found
  (let [db (fresh-db)
        sid (java.util.UUID/randomUUID)]
    (is (nil? (session/get-session db sid)))
    (is (nil? (session/find-session db sid)))
    (is (nil? (session/try-cancel-session! db sid)))
    (is (= :store/session-not-found
           (-> (tx-error #(session/get-session! db sid))
               ex-data :error/type)))))

(deftest create-request-is-validated
  (let [db (fresh-db)]
    (is (= :store/session-invalid
           (-> (tx-error #(session/create-session! db {}))
               ex-data :error/type)))
    (is (= :store/session-invalid
           (-> (tx-error #(session/create-session! db (assoc (session-request) :unexpected true)))
               ex-data :error/type)))
    (is (= :store/generation-not-found
           (-> (tx-error #(session/create-session! db (assoc (session-request)
                                                            :generation/id "missing-generation")))
               ex-data :error/type)))))

(deftest routing-is-persisted-with-the-session-pin
  (let [db (fresh-db)
        _ (seed-generation! db)
        s (session/create-session! db (assoc (session-request)
                                             :routing {:deployment-version "v1" :bucket 7}))
        fetched (session/get-session db (:session/id s))]
    (is (= {:deployment-version "v1" :bucket 7}
           (:routing fetched)))))

(deftest cancellation-delegates-to-root-work
  (let [db (fresh-db)
        _ (seed-generation! db)
        s (session/create-session! db (session-request))
        sid (:session/id s)
        work-id (java.util.UUID/randomUUID)
        _ (work-store/create-work! db {:work/id work-id
                                       :work/type :session/run
                                       :work/state :running
                                       :work/session-id sid})
        cancelled (session/try-cancel-session! db sid)]
    (is (= sid (:session/id cancelled)))
    (is (= :cancelled (:work/state (work-store/fetch-work db work-id))))
    (is (not (contains? (session/get-session db sid) :state)))))

(deftest work-lifecycle-does-not-change-session-identity
  (let [db (fresh-db)
        _ (seed-generation! db)
        s (session/create-session! db (session-request))
        sid (:session/id s)
        work-id (java.util.UUID/randomUUID)
        _ (work-store/create-work! db {:work/id work-id
                                       :work/type :session/run
                                       :work/state :queued
                                       :work/session-id sid})
        _ (work-store/dispatch-work! db work-id)
        _ (work-store/succeed-work! db work-id nil)
        fetched (session/get-session db sid)]
    (is (= :succeeded (:work/state (work-store/fetch-work db work-id))))
    (is (= [genome resolution phenotype gen]
           [(:genome/id fetched) (:resolution/id fetched)
            (:phenotype/id fetched) (:generation/id fetched)]))
    (is (not (contains? fetched :state)))))

(deftest session-descendants-follow-work-parentage
  (testing "list-descendants follows works.parent_work_id transitively"
    (let [db (fresh-db)
          _ (seed-generation! db)
          root-session (session/create-session! db (session-request))
          root-id (:session/id root-session)
          root-work-id (java.util.UUID/randomUUID)
          _ (work-store/create-work! db {:work/id root-work-id
                                         :work/type :session/run
                                         :work/state :succeeded
                                         :work/session-id root-id})
          child-session (session/create-session! db (session-request))
          child-id (:session/id child-session)
          child-work-id (java.util.UUID/randomUUID)
          _ (work-store/create-work! db {:work/id child-work-id
                                         :work/type :subagent/run
                                         :work/state :queued
                                         :work/session-id child-id
                                         :work/parent-work-id root-work-id})
          grandchild-session (session/create-session! db (session-request))
          grandchild-id (:session/id grandchild-session)
          grandchild-work-id (java.util.UUID/randomUUID)
          _ (work-store/create-work! db {:work/id grandchild-work-id
                                         :work/type :subagent/run
                                         :work/state :queued
                                         :work/session-id grandchild-id
                                         :work/parent-work-id child-work-id})
          descendants (set (session/list-descendants db root-id))]
      (is (= #{child-id grandchild-id} descendants)))))

(deftest session-api-has-no-state-transition-entrypoint
  (let [db (fresh-db)
        _ (seed-generation! db)
        s (session/create-session! db (session-request))]
    (is (nil? (ns-resolve 'evoclj.store.session 'transition-session!)))
    (is (not (contains? (session/get-session db (:session/id s)) :state)))))