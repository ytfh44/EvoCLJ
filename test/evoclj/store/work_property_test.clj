(ns evoclj.store.work-property-test
  "Work×Session product + Work 7-state SM composition (100 rounds per law)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

;; --- DB CHECK alignment (works.state is the single source of truth) ---------
;;
;; Mirrors evoclj.evolution.candidate-states-test/db-check-aligned: the Work
;; db-state->kw mapping is pinned to the `works.state` CHECK parsed out of
;; resources/migrations/018-work.sql. Without this pin, a stray extra spelling
;; (e.g. the hyphen form "timed-out", which no migration admits and no reader
;; ever needs) can sit in the mapping unnoticed.

(defn- works-db-check-states
  "Parse the `works.state` CHECK constraint from 018-work.sql and return the
  set of DB strings it admits.

  Disambiguation: migrations may contain more than one `state IN (...)` CHECK
  (012-commands.sql admits a 6-value set sharing 'queued'/'running'/
  'timed_out'/'cancelled'). We read only 018-work.sql and select the CHECK
  whose inner list contains \"waiting\" — 'waiting' exists solely in the
  works vocabulary, so it uniquely identifies the works table's constraint
  (the same discriminator technique candidate-states-test uses with
  \"materialized\")."
  []
  (let [sql (try (slurp (io/resource "migrations/018-work.sql"))
                 (catch Exception _ ""))
        sql (if (str/blank? sql)
              (slurp "resources/migrations/018-work.sql")
              sql)
        matches (re-seq #"state\s+IN\s*\(([^)]+)\)" sql)
        works-inner (some (fn [[_ inner]]
                            (when (str/includes? inner "waiting") inner))
                          matches)]
    (when-not works-inner
      (throw (ex-info "Could not parse works.state CHECK constraint"
                      {:sql sql :matches matches})))
    (->> (re-seq #"'([^']+)'" works-inner)
         (map second)
         set)))

(deftest db-check-aligned
  (testing "db-state->kw keys are exactly the works.state DB CHECK values"
    (let [db-check (works-db-check-states)]
      (is (= db-check (set (keys work/db-state->kw)))
          (str "DB CHECK " db-check " vs db-state->kw keys "
               (set (keys work/db-state->kw))))
      (is (= 7 (count db-check)))
      (is (= #{"queued" "running" "waiting" "succeeded"
               "failed" "cancelled" "timed_out"}
             db-check))))
  (testing "kw->db-state is the inverse of db-state->kw"
    (is (= work/db-state->kw
           (into {} (map (fn [[k v]] [v k]) work/kw->db-state))))
    (is (= work/kw->db-state
           (into {} (map (fn [[k v]] [v k]) work/db-state->kw)))))
  (testing "persisted keywords round-trip through DB mapping"
    (doseq [[db kw] work/db-state->kw]
      (is (= db (get work/kw->db-state kw)))
      (is (= kw (get work/db-state->kw db)))))
  (testing "DB strings cover exactly the persisted keyword values"
    (is (= (works-db-check-states)
           (set (vals work/kw->db-state))))))
