(ns evoclj.control-test
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is]]
            [evoclj.control :as control]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

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
                                     :created_at created-at})
    (jdbc/insert! conn :sessions {:id (str sid)
                                  :generation_id generation
                                  :genome_id genome
                                  :resolution_id resolution
                                  :phenotype_id phenotype
                                  :state "created"
                                  :created_at created-at}))
  sid)

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
  [sid]
  {:control/id (random-uuid)
   :control/type :interrupt
   :control/idempotency-key "interrupt-1"
   :control/scope {:session/id sid}
   :control/payload {:reason "operator-request"}
   :control/requested-at (java.util.Date. 0)})

(deftest control-request-is-an-append-only-event-fact
  (let [[db path] (fresh-db)
        sid (seed-session! db (random-uuid))]
    (try
      (let [root (root-event! db sid)
            req (request sid)
            persisted (control/append-control-event!
                       db
                       {:session/id sid
                        :generation/id generation
                        :phenotype/id phenotype
                        :prev/event-id (:event/id root)
                        :causal-links #{}}
                       req)]
        (is (= :control/requested (:event/type persisted)))
        (is (= (assoc req :control/status :requested)
               (control/control-event persisted)))
        (is (= [1 2] (mapv :event/seq (event/events-for-session db sid)))))
      (finally
        (cleanup! path)))))

(deftest control-request-validation-fails-closed
  (let [[db path] (fresh-db)
        sid (seed-session! db (random-uuid))]
    (try
      (is (= :control/request-invalid
             (try
               (control/append-control-event!
                db
                {:session/id sid
                 :generation/id generation
                 :phenotype/id phenotype
                 :prev/event-id 1
                 :causal-links #{}}
                (assoc (request sid) :control/type :pause))
               nil
               (catch clojure.lang.ExceptionInfo e
                 (:error/type (ex-data e))))))
      (finally
        (cleanup! path)))))
