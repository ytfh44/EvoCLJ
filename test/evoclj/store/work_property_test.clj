(ns evoclj.store.work-property-test
  "Work×Session product + Work 7-state SM composition (100 rounds per law)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [evoclj.runtime.work :as work]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.work :as work-store]
            [clojure.java.jdbc :as jdbc])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID Date)))

;; --- pure SM properties (no DB) --------------------------------------------

(defspec work-edges-legal 100
  (prop/for-all [from (gen/elements (vec work/work-states))
                 to   (gen/elements (vec work/work-states))]
    (let [valid? (work/valid-transition? from to)
          in-table? (contains? (get work/work-transitions from #{}) to)]
      (= valid? in-table?))))

(defspec work-acyclic 100
  (prop/for-all [_ (gen/return nil)]
    (work/acyclic?)))

(defspec work-terminals-sink 100
  (prop/for-all [_ (gen/return nil)]
    (work/terminals-sink?)))

(defspec work-queued-to-succeeded-path 100
  (prop/for-all [_ (gen/return nil)]
    (work/queued->succeeded-path?)))

(defspec work-queued-to-timed-out-path 100
  (prop/for-all [_ (gen/return nil)]
    (work/queued->timed-out-path?)))

(defspec work-product-collapse 100
  (prop/for-all [_ (gen/return nil)]
    (and (= 48 work/session-x-command-product)
         (= 7 (work/work-states-count))
         (= "Session×Command 48 states collapses to Work 7" (work/collapse-ratio))
         (let [{:keys [pass?]} (work/verify-work-sm)] pass?))))

(defspec work-random-walk-legal 100
  (prop/for-all [steps (gen/vector (gen/elements [:running :waiting :succeeded :failed :cancelled :timed-out]) 1 5)]
    (loop [state :queued
           remaining steps
           visited #{:queued}]
      (if (empty? remaining)
        true
        (let [next (first remaining)
              legal? (work/valid-transition? state next)]
          (if legal?
            (if (contains? visited next) false (recur next (rest remaining) (conj visited next)))
            (not legal?)))))))

;; --- DB-backed composition (with temp DB, 5 rounds to keep fast, but property style) ---

(defn- temp-db []
  (let [p (str (Files/createTempFile "work-prop-" ".db" (make-array FileAttribute 0)))
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
        (try (jdbc/insert! conn :sessions {:id (str #uuid "00000000-0000-4000-a000-000000000000") :generation_id "gen-1" :genome_id gen-id :resolution_id res-id :phenotype_id phen-id :state "created" :created_at "2025-01-01T00:00:00Z"}) (catch Exception _ nil))))
    {:db db :path p}))

(defspec work-cas-transition-composition 5
  (prop/for-all [seed (gen/choose 0 1000000)]
    (let [{:keys [db path]} (temp-db)
          sid #uuid "00000000-0000-4000-a000-000000000000"
          wid (UUID/randomUUID)
          _ (work-store/create-work! db {:work/id wid :work/type :prop :work/state :queued :work/session-id sid})
          before (work-store/fetch-work db wid)
          _ (work-store/dispatch-work! db wid)
          after (work-store/fetch-work db wid)
          ok? (and (= :queued (:work/state before)) (= :running (:work/state after)))]
      (try (clojure.java.io/delete-file path) (catch Exception _ nil))
      ok?)))

(defspec work-recovery-composition 5
  (prop/for-all [seed (gen/choose 0 100000)]
    (let [{:keys [db path]} (temp-db)
          sid #uuid "00000000-0000-4000-a000-000000000000"
          w1 (UUID/randomUUID)
          w2 (UUID/randomUUID)
          _ (work-store/create-work! db {:work/id w1 :work/type :prop :work/state :queued :work/session-id sid})
          _ (work-store/create-work! db {:work/id w2 :work/type :prop :work/state :queued :work/session-id sid})
          _ (work-store/dispatch-work! db w2)
          orphans-before (count (work-store/find-orphaned-works db))
          report (work-store/recover-works! db)
          orphans-after (count (work-store/find-orphaned-works db))
          w1-after (work-store/fetch-work db w1)
          w2-after (work-store/fetch-work db w2)]
      (try (clojure.java.io/delete-file path) (catch Exception _ nil))
      (and (= :queued (:work/state w1-after))
           (= :failed (:work/state w2-after))
           (= 2 orphans-before)
           (or (= 1 orphans-after) (= 1 (count (:recovered-queued report))))
           (not= :succeeded (:work/state w2-after))))))
