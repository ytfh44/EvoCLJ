(ns support.property.composition-property-test
  "Composition invariants: Work×Session product, Grant meet, Event refinement (100 rounds, shared DB for speed)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.set :as set]
            [evoclj.runtime.work :as work]
            [evoclj.capability.grant :as grant]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [clojure.java.jdbc :as jdbc])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID Date)))

(defspec work-session-product-collapse 100
  (prop/for-all [_ (gen/return nil)]
    (and (= 48 work/session-x-command-product)
         (= 7 (work/work-states-count))
         (work/acyclic?)
         (work/terminals-sink?)
         (work/queued->succeeded-path?)
         (work/queued->timed-out-path?)
         (= "Session×Command 48 states collapses to Work 7" (work/collapse-ratio)))))

(defspec grant-meet-product-composition 100
  (prop/for-all [a (gen/elements [#{:read} #{:write} #{:read :write} #{:list}])
                 b (gen/elements [#{:read} #{:write} #{:read :write} #{:list}])]
    (let [ga (grant/make-grant {:kind :filesystem :path "/a"} a)
          gb (grant/make-grant {:kind :filesystem :path "/a"} b)
          m (grant/meet ga gb)
          am (grant/action-set-meet a b)
          rm (grant/resource-meet (:resource ga) (:resource gb))]
      (if (nil? m) (nil? am) (and (= (:actions m) am) (= (:resource m) rm))))))

(def ^:private shared-composition-db
  (delay
    (let [p (str (Files/createTempFile "comp-prop-shared-" ".db" (make-array FileAttribute 0)))
          db (sqlite/spec p)]
      (migrate/migrate! db)
      (let [gen-id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            res-id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            phen-id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"]
        (artifact/ensure-artifact! db gen-id "application/octet-stream" 0)
        (artifact/ensure-artifact! db res-id "application/edn" 0)
        (artifact/ensure-artifact! db phen-id "application/edn" 0)
        (artifact/ensure-genome! db gen-id)
        (sqlite/with-db [conn db]
          (try (jdbc/insert! conn :generations {:id "gen-1" :genome_id gen-id :resolution_id res-id :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"}) (catch Exception _ nil))
          (try (jdbc/insert! conn :sessions {:id (str #uuid "00000000-0000-4000-a000-000000000000") :generation_id "gen-1" :genome_id gen-id :resolution_id res-id :phenotype_id phen-id :state "created" :created_at "2025-01-01T00:00:00Z"}) (catch Exception _ nil))
          (try (jdbc/insert! conn :sessions {:id (str #uuid "00000000-0000-4000-a000-000000000001") :generation_id "gen-1" :genome_id gen-id :resolution_id res-id :phenotype_id phen-id :state "created" :created_at "2025-01-01T00:00:00Z"}) (catch Exception _ nil))))
      {:db db :path p})))

(defn- clean-comp-events! [db]
  (try (sqlite/exec! db ["DELETE FROM causal_links"]) (catch Exception _ nil))
  (try (sqlite/exec! db ["DELETE FROM events"]) (catch Exception _ nil)))

(defn- temp-db2 []
  (let [p (str (Files/createTempFile "comp-prop2-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec p)]
    (migrate/migrate! db)
    (let [gen-id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          res-id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          phen-id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"]
      (artifact/ensure-artifact! db gen-id "application/octet-stream" 0)
      (artifact/ensure-artifact! db res-id "application/edn" 0)
      (artifact/ensure-artifact! db phen-id "application/edn" 0)
      (artifact/ensure-genome! db gen-id)
      (sqlite/with-db [conn db]
        (try (jdbc/insert! conn :generations {:id "gen-1" :genome_id gen-id :resolution_id res-id :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"}) (catch Exception _ nil))
        (try (jdbc/insert! conn :sessions {:id (str #uuid "00000000-0000-4000-a000-000000000000") :generation_id "gen-1" :genome_id gen-id :resolution_id res-id :phenotype_id phen-id :state "created" :created_at "2025-01-01T00:00:00Z"}) (catch Exception _ nil))
        (try (jdbc/insert! conn :sessions {:id (str #uuid "00000000-0000-4000-a000-000000000001") :generation_id "gen-1" :genome_id gen-id :resolution_id res-id :phenotype_id phen-id :state "created" :created_at "2025-01-01T00:00:00Z"}) (catch Exception _ nil))))
    {:db db :path p}))

(defspec event-refinement-prev-vs-causal-links 5
  (prop/for-all [_ (gen/return nil)]
    (let [{:keys [db path]} (temp-db2)
          now (Date.)
          sid-a #uuid "00000000-0000-4000-a000-000000000000"
          sid-b #uuid "00000000-0000-4000-a000-000000000001"
          e-a1 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                        :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}})
          e-b1 (event/append-event! db {:session/id sid-b :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                        :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}})
          e-a2 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                        :event/type :tool/invoke :prev/event-id (:event/id e-a1) :causal-links #{{:from (:event/id e-b1) :type :test}} :payload-ref nil :created-at now :metadata {}})
          events-a (event/events-for-session db sid-a)
          seqs (map :event/seq events-a)
          ok? (and (= [1 2] (vec seqs)) (= (:event/id e-a1) (:prev/event-id e-a2)) (= 1 (count (:causal-links e-a2))) (:valid? (event/verify-event-chain db sid-a)))]
      (try (clojure.java.io/delete-file path) (catch Exception _ nil))
      ok?)))
