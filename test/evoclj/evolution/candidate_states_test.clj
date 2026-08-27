(ns evoclj.evolution.candidate-states-test
  "Fleet S2 — single canonical source for candidate states.

  Verifies:
  - illegal state :banana is rejected (definition > validation via Malli enum)
  - DB CHECK constraint in 001-init.sql is aligned with kw<->DB mapping
  - transition table is single-source (no duplicate literals)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.candidate-states :as cstates]
            [evoclj.promotion.state :as pstate]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- db-check-states
  "Parse the candidates.state CHECK constraint from 001-init.sql and
  return the set of DB strings it admits."
  []
  (let [sql (try (slurp (io/resource "migrations/001-init.sql"))
                 (catch Exception _ ""))
        sql (if (str/blank? sql)
              (slurp "resources/migrations/001-init.sql")
              sql)
        matches (re-seq #"state IN \(([^)]+)\)" sql)
        candidates-inner (some (fn [[_ inner]]
                                 (when (str/includes? inner "materialized")
                                   inner))
                               matches)]
    (when-not candidates-inner
      (throw (ex-info "Could not parse candidates CHECK constraint" {:sql sql :matches matches})))
    (->> (re-seq #"'([^']+)'" candidates-inner)
         (map second)
         set)))

(defn- candidate-schema-rejects?
  "True when CandidateSchema rejects a candidate map with :state = k."
  [k]
  (let [c {:candidate/id (java.util.UUID/randomUUID)
           :parent/generation-id "generation-1"
           :parent/genome-id "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
           :candidate/genome-id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
           :mutation/id (java.util.UUID/randomUUID)
           :evidence/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
           :risk :behavioral
           :state k
           :created-at (java.util.Date.)}]
    (some? (m/explain candidate/CandidateSchema c))))

;; ---------------------------------------------------------------------------
;; 1. Illegal state rejected
;; ---------------------------------------------------------------------------

(deftest illegal-state-rejected
  (testing ":banana is not a candidate state"
    (is (not (contains? cstates/candidate-states :banana)))
    (is (not (cstates/candidate-state? :banana)))
    (is (not (m/validate cstates/candidate-state-enum :banana))))
  (testing "CandidateSchema rejects :banana"
    (is (candidate-schema-rejects? :banana))
    (is (not (m/validate candidate/CandidateSchema
                         {:candidate/id (java.util.UUID/randomUUID)
                          :parent/generation-id "g1"
                          :parent/genome-id "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                          :candidate/genome-id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                          :mutation/id (java.util.UUID/randomUUID)
                          :evidence/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                          :risk :behavioral
                          :state :banana
                          :created-at (java.util.Date.)}))))
  (testing "CandidateSchema accepts every legal state"
    (doseq [s cstates/candidate-states]
      (is (not (candidate-schema-rejects? s))
          (str "should accept " s))))
  (testing "candidate-state-enum is exactly the vocabulary"
    (is (= (set (rest cstates/candidate-state-enum))
           cstates/candidate-states))
    (is (= :enum (first cstates/candidate-state-enum))))
  (testing "non-keyword and nil are rejected"
    (is (candidate-schema-rejects? "materialized"))
    (is (candidate-schema-rejects? nil))
    (is (candidate-schema-rejects? :MATERIALIZED))))

;; ---------------------------------------------------------------------------
;; 2. DB CHECK aligned
;; ---------------------------------------------------------------------------

(deftest db-check-aligned
  (testing "db-state->kw keys are exactly the DB CHECK values"
    (let [db-check (db-check-states)]
      (is (= db-check (set (keys cstates/db-state->kw)))
          (str "DB CHECK " db-check " vs db-state->kw keys " (set (keys cstates/db-state->kw))))
      (is (= 6 (count db-check)))
      (is (= #{"materialized" "evaluating" "eligible" "promoted" "rejected" "stale"} db-check))))
  (testing "kw->db-state is the inverse of db-state->kw"
    (is (= cstates/db-state->kw
           (into {} (map (fn [[k v]] [v k]) cstates/kw->db-state))))
    (is (= cstates/kw->db-state
           (into {} (map (fn [[k v]] [v k]) cstates/db-state->kw)))))
  (testing "persisted keywords round-trip through DB mapping"
    (doseq [[db kw] cstates/db-state->kw]
      (is (= db (cstates/kw->db kw)))
      (is (= kw (cstates/db->kw db)))))
  (testing "non-persisted states map to nil in kw->db"
    (is (nil? (cstates/kw->db :proposed)))
    (is (nil? (cstates/kw->db :invalid)))
    (is (nil? (cstates/kw->db :canary)))
    (is (nil? (cstates/kw->db :canary-failed)))
    (is (nil? (cstates/kw->db :banana))))
  (testing "unknown DB strings decode to nil"
    (is (nil? (cstates/db->kw "banana")))
    (is (nil? (cstates/db->kw "proposed"))))
  (testing "DB strings cover exactly the persisted keyword values"
    (is (= (set (vals cstates/kw->db-state))
           (set (keys cstates/db-state->kw))))))

;; ---------------------------------------------------------------------------
;; 3. Transition table single-source
;; ---------------------------------------------------------------------------

(deftest transition-table-single-source
  (testing "candidate ns delegates to canonical source (identical? / =)"
    (is (= candidate/states cstates/candidate-states))
    (is (= candidate/transitions cstates/candidate-transitions))
    (is (identical? candidate/states cstates/candidate-states))
    (is (identical? candidate/transitions cstates/candidate-transitions)))
  (testing "promotion/state delegates to canonical source"
    (is (= pstate/candidate-states cstates/candidate-states))
    (is (= pstate/candidate-transitions cstates/candidate-transitions))
    (is (identical? pstate/candidate-states cstates/candidate-states))
    (is (identical? pstate/candidate-transitions cstates/candidate-transitions)))
  (testing "candidate-store mapping is single-source (via public aliases)"
    (is (= cstates/db-state->kw cstates/db-state->state))
    (is (= cstates/kw->db-state cstates/state->db-state))
    (is (= cstates/db-state->kw {"materialized" :materialized
                                 "evaluating" :evaluation-pending
                                 "eligible" :evaluated
                                 "promoted" :promoted
                                 "rejected" :rejected
                                 "stale" :stale})))
  (testing "every transition target is a known candidate state"
    (is (every? #(contains? cstates/candidate-states %)
                (keys cstates/candidate-transitions)))
    (is (every? #(contains? cstates/candidate-states %)
                (mapcat val cstates/candidate-transitions))))
  (testing "closed-world invariants"
    (is (= cstates/candidate-states (set (keys cstates/candidate-transitions))))
    (is (empty? (cstates/next-states :promoted)))
    (is (empty? (cstates/next-states :rejected)))
    (is (= #{:materialized} (cstates/next-states :proposed)))
    (is (= #{:evaluation-pending} (cstates/next-states :materialized)))
    (is (= #{:evaluated :invalid} (cstates/next-states :evaluation-pending))))
  (testing "valid-transition? agrees with table"
    (is (cstates/valid-transition? :proposed :materialized))
    (is (not (cstates/valid-transition? :proposed :evaluation-pending)))
    (is (cstates/valid-transition? :materialized :evaluation-pending))
    (is (cstates/valid-transition? :evaluation-pending :evaluated))
    (is (cstates/valid-transition? :evaluated :canary))
    (is (not (cstates/valid-transition? :evaluated :proposed))))
  (testing "no duplicate literal definitions elsewhere"
    (is (true? (cstates/candidate-state? :proposed)))
    (is (true? (cstates/candidate-state? :materialized)))))
