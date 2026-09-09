(ns evoclj.control-test
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is]]
            [evoclj.control :as control]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.work :as work-store]))

(def ^:private generation "generation-1")
(def ^:private genome "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private phenotype "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(def ^:private resolution "resolution-1")
(def ^:private created-at "2025-01-01T00:00:00Z")

(defn- fresh-db
  []
  (let [file (java.io.File/createTempFile "evoclj-control-test-" ".sqlite")
        path (.getAbsolutePath file)]
    (.delete file)
    (migrate/migrate! (sqlite/spec path))
    [(sqlite/spec path) path]))

(defn- cleanup!
  [path]
  (try (.delete (java.io.File. path)) (catch Exception _ nil)))

(defn- seed-session!
  [db sid]
  (sqlite/with-db [conn db]
    (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" generation]))
      (doseq [[hash media-type size]
              [[genome "application/octet-stream" 64]
               [resolution "application/edn" 64]
               [phenotype "application/octet-stream" 64]]]
        (jdbc/insert! conn :artifacts {:hash hash
                                       :media_type media-type
                                       :size size
                                       :created_at created-at}))
      (jdbc/insert! conn :genomes {:id genome :created_at created-at})
      (jdbc/insert! conn :generations {:id generation
                                       :genome_id genome
                                       :resolution_id resolution
                                       :parent_id nil
                                       :state "active"
                                       :current 0
                                       :created_at created-at}))
    (jdbc/insert! conn :sessions {:id (str sid)
                                  :generation_id generation
                                  :genome_id genome
                                  :resolution_id resolution
                                  :phenotype_id phenotype
                                  :state "created"
                                  :created_at created-at}))
  sid)

(defn- make-work!
  [db sid & [parent-work-id]]
  (let [id (random-uuid)]
    (work-store/create-work!
     db
     (cond-> {:work/id id
              :work/type :subagent/run
              :work/state :queued
              :work/session-id sid
              :work/created-at (java.util.Date. 0)}
       parent-work-id (assoc :work/parent-work-id parent-work-id)))
    id))

(defn- root-event!
  [db sid]
  (event/append-event! db {:session/id sid
                           :generation/id generation
                           :phenotype/id phenotype
                           :event/type :session/created
                           :prev/event-id nil
                           :causal-links #{}
                           :payload-ref nil
                           :metadata {}}))

(defn- request
  [issuer target & [work-id]]
  (cond-> {:control/id (random-uuid)
           :control/issuer issuer
           :control/type :interrupt
           :control/idempotency-key (str "interrupt-" (random-uuid))
           :control/scope {:session/id target}
           :control/payload {:reason "operator-request"}
           :control/requested-at (java.util.Date. 0)}
    work-id (assoc-in [:control/scope :work/id] work-id)))

(defn- append-control!
  [db target root req]
  (control/append-control-event!
   db
   {:session/id target
    :generation/id generation
    :phenotype/id phenotype
    :prev/event-id (:event/id root)
    :causal-links #{}}
   req))

(defn- thrown-error-type
  [f]
  (:error/type
   (ex-data
    (try
      (f)
      nil
      (catch clojure.lang.ExceptionInfo e e)))))

(deftest control-request-is-an-append-only-event-fact
  (let [[db path] (fresh-db)
        sid (seed-session! db (random-uuid))]
    (try
      (let [root (root-event! db sid)
            req (request sid sid)
            persisted (append-control! db sid root req)]
        (is (= :control/requested (:event/type persisted)))
        (is (= (assoc req :control/status :requested)
               (control/control-event persisted)))
        (is (= [1 2] (mapv :event/seq (event/events-for-session db sid)))))
      (finally
        (cleanup! path)))))

(deftest identical-retries-replay-one-event
  (let [[db path] (fresh-db)
        sid (seed-session! db (random-uuid))]
    (try
      (let [root (root-event! db sid)
            req (request sid sid)
            first-event (append-control! db sid root req)
            replay (append-control! db sid root req)]
        (is (= (:event/id first-event) (:event/id replay)))
        (is (= (:event-hash first-event) (:event-hash replay)))
        (is (= 2 (count (event/events-for-session db sid)))))
      (finally
        (cleanup! path)))))

(deftest concurrent-identical-retries-have-one-winner
  (let [[db path] (fresh-db)
        sid (seed-session! db (random-uuid))]
    (try
      (let [root (root-event! db sid)
            req (request sid sid)
            results (->> (repeatedly 8 #(future (append-control! db sid root req)))
                         (mapv deref))]
        (is (= 1 (count (set (map :event/id results)))))
        (is (= 2 (count (event/events-for-session db sid)))))
      (finally
        (cleanup! path)))))

(deftest reused-identity-with-different-content-is-a-conflict
  (let [[db path] (fresh-db)
        sid (seed-session! db (random-uuid))]
    (try
      (let [root (root-event! db sid)
            req (request sid sid)]
        (append-control! db sid root req)
        (is (= :control/idempotency-conflict
               (thrown-error-type
                #(append-control! db sid root
                                  (assoc req :control/payload {:reason "different"})))))
        (is (= :control/idempotency-conflict
               (thrown-error-type
                #(append-control! db sid root
                                  (assoc req :control/id (random-uuid)))))))
      (finally
        (cleanup! path)))))

(deftest ancestor-controls-descendant-but-child-cannot-control-parent-or-sibling
  (let [[db path] (fresh-db)
        parent (seed-session! db (random-uuid))
        child (seed-session! db (random-uuid))
        sibling (seed-session! db (random-uuid))]
    (try
      (let [parent-work (make-work! db parent)
            child-work (make-work! db child parent-work)
            _sibling-work (make-work! db sibling parent-work)
            parent-root (root-event! db parent)
            child-root (root-event! db child)]
        (is (= :control/requested
               (:event/type
                (append-control! db child child-root
                                 (request parent child child-work)))))
        (is (= :control/scope-denied
               (thrown-error-type
                #(append-control! db parent parent-root
                                  (request child parent parent-work)))))
        (is (= :control/scope-denied
               (thrown-error-type
                #(append-control! db child child-root
                                  (request sibling child child-work)))))
        (is (= :control/work-scope-mismatch
               (thrown-error-type
                #(append-control! db child child-root
                                  (request parent child parent-work)))))
        (is (some? (work-store/fetch-work db child-work))))
      (finally
        (cleanup! path)))))

(deftest control-request-validation-fails-closed
  (let [[db path] (fresh-db)
        sid (seed-session! db (random-uuid))]
    (try
      (let [root (root-event! db sid)]
        (is (= :control/request-invalid
               (thrown-error-type
                #(append-control! db sid root
                                  (assoc (request sid sid) :control/type :pause)))))
        (is (= :control/issuer-not-found
               (thrown-error-type
                #(append-control! db sid root
                                  (request (random-uuid) sid)))))
        (is (= :control/work-not-found
               (thrown-error-type
                #(append-control! db sid root
                                  (request sid sid (random-uuid)))))))
      (finally
        (cleanup! path)))))
