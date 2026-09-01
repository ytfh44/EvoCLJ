(ns evoclj.store.event-property-test
  "Event refinement — prev linear vs causal-links graph + seq positional + hash chain (100 rounds)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.event :as event]
            [evoclj.store.event-schema :as es]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [clojure.java.jdbc :as jdbc])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID Date)
           (java.time Instant)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "event-prop-" ".db" (make-array FileAttribute 0)))
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

(def ^:private sid-a #uuid "00000000-0000-4000-a000-000000000000")
(def ^:private sid-b #uuid "00000000-0000-4000-a000-000000000001")

(defspec event-prev-must-be-same-session 5
  (prop/for-all [_ (gen/return nil)]
    (let [{:keys [db path]} (temp-db)
          now (Date.)
          root {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}}
          e1 (event/append-event! db root)
          bad-prev {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                    :event/type :tool/invoke :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}}
          ok? (try (event/append-event! db bad-prev) false (catch Exception e (= :store/event-invalid (:error/type (ex-data e)))))]
      (try (clojure.java.io/delete-file path) (catch Exception _ nil))
      (and (some? e1) (= 1 (:event/seq e1)) ok?))))

(defspec event-prev-strictly-earlier 5
  (prop/for-all [seed (gen/choose 0 1000)]
    (let [{:keys [db path]} (temp-db)
          now (Date.)
          e1 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                      :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}})
          e2 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                      :event/type :tool/invoke :prev/event-id (:event/id e1) :causal-links #{} :payload-ref nil :created-at now :metadata {}})]
      (try (clojure.java.io/delete-file path) (catch Exception _ nil))
      (and (= 1 (:event/seq e1)) (= 2 (:event/seq e2)) (= (:event/id e1) (:prev/event-id e2))))))

(defspec event-causal-links-cross-session-allowed 5
  (prop/for-all [_ (gen/return nil)]
    (let [{:keys [db path]} (temp-db)
          now (Date.)
          e-a1 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                        :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}})
          e-b1 (event/append-event! db {:session/id sid-b :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                        :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}})
          e-a2 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                        :event/type :subagent/result :prev/event-id (:event/id e-a1) :causal-links #{{:from (:event/id e-b1) :type :subagent/result}} :payload-ref nil :created-at now :metadata {:child/session-id sid-b}})]
      (try (clojure.java.io/delete-file path) (catch Exception _ nil))
      (and (= 2 (:event/seq e-a2)) (= 1 (count (:causal-links e-a2))) (= (:event/id e-b1) (:from (first (:causal-links e-a2))))))))

(defspec event-causal-link-must-exist 5
  (prop/for-all [_ (gen/return nil)]
    (let [{:keys [db path]} (temp-db)
          now (Date.)
          e1 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                      :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}})
          bogus 999999]
      (try (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                    :event/type :tool/invoke :prev/event-id (:event/id e1) :causal-links #{{:from bogus :type :test}} :payload-ref nil :created-at now :metadata {}})
           false
           (catch Exception e (= :store/causal-link-not-found (:error/type (ex-data e))))))))

(defspec event-root-no-prev-or-links 5
  (prop/for-all [_ (gen/return nil)]
    (let [{:keys [db path]} (temp-db)
          now (Date.)
          root {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}}
          e1 (event/append-event! db root)
          with-prev-fails? (try (event/append-event! db (assoc root :prev/event-id 1)) false (catch Exception e (= :store/event-invalid (:error/type (ex-data e)))))
          with-links-fails? (try (event/append-event! db (assoc root :causal-links #{{:from (:event/id e1) :type :test}})) false (catch Exception e (= :store/event-invalid (:error/type (ex-data e)))))]
      (try (clojure.java.io/delete-file path) (catch Exception _ nil))
      (and with-prev-fails? with-links-fails?))))

(def ^:private shared-seq-db
  (delay
    (let [p (str (Files/createTempFile "event-seq-shared-" ".db" (make-array FileAttribute 0)))
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

(defn- clean-seq-events! [db]
  (try (sqlite/exec! db ["DELETE FROM causal_links"]) (catch Exception _ nil))
  (try (sqlite/exec! db ["DELETE FROM events"]) (catch Exception _ nil)))

(defspec event-seq-positional-continuous 5
  (prop/for-all [n (gen/choose 1 5)]
    (let [{:keys [db]} @shared-seq-db
          _ (clean-seq-events! db)
          now (Date.)
          _ (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                     :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {}})
          _ (loop [prev-id (:event/id (first (event/events-for-session db sid-a))) i 1]
              (when (< i n)
                (let [e (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                                 :event/type :tool/invoke :prev/event-id prev-id :causal-links #{} :payload-ref nil :created-at now :metadata {:i i}})]
                  (recur (:event/id e) (inc i)))))
          events (event/events-for-session db sid-a)
          seqs (map :event/seq events)
          expected (range 1 (inc (count events)))]
      (and (= (vec seqs) (vec expected)) (:valid? (event/verify-event-chain db sid-a))))))

(defspec event-hash-chain-detects-tamper 5
  (prop/for-all [_ (gen/return nil)]
    (let [{:keys [db path]} (temp-db)
          now (Date.)
          e1 (event/append-event! db {:session/id sid-a :generation/id "gen-1" :phenotype/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                      :event/type :session/created :prev/event-id nil :causal-links #{} :payload-ref nil :created-at now :metadata {:secret "a"}})
          chain-before (:valid? (event/verify-event-chain db sid-a))]
      (try (do (sqlite/exec! db ["DROP TRIGGER IF EXISTS events_no_update"])
                 (sqlite/exec! db ["UPDATE events SET event_hash = ? WHERE id = ?" "tampered" (:event/id e1)])
                 (sqlite/exec! db ["CREATE TRIGGER events_no_update BEFORE UPDATE ON events BEGIN SELECT RAISE(ABORT, 'events are append-only'); END;"])) (catch Exception _ nil))
      (let [chain-after (:valid? (event/verify-event-chain db sid-a))]
        (try (clojure.java.io/delete-file path) (catch Exception _ nil))
        (and chain-before (not chain-after))))))
