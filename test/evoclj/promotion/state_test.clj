(ns evoclj.promotion.state-test
  "Task 9.1 tests: generation states and candidate deployment states as
  PURE, closed transition tables (data, no SQL — Task 9.2 owns the
  CURRENT compare-and-set).

  The machine fragments this task owns:

  - generation states: :seed :active :superseded :rolled-back, with
    the normative edges :seed → :active, :active → :superseded,
    :active → :rolled-back (a generation that was never active is
    never rolled back; terminal states have no outgoing edges).
  - candidate deployment states (the M9 fragment of the Task 7.6
    candidate machine): :evaluated → #{:canary :promoted :rejected
    :stale}, :canary → #{:promoted :canary-failed}; :rejected
    :stale :canary-failed :promoted are terminal.

  THE KEY RULE, in the task's numbered order:

  - Step 1: only an evaluated, ELIGIBLE candidate may enter :canary
    or direct :promoted. 'Evaluated-only' is encoded structurally in
    the table (only :evaluated carries the canary/promotion edges);
    the eligibility gate is enforced by deployment-transition.
  - Step 2: an ineligible candidate can ONLY become :rejected —
    entry to :canary, :promoted, or even :stale fails with
    :promotion/ineligible.
  - Step 3: the transition tables are pure data and closed over the
    state vocabulary; unknown states fail with :promotion/unknown-state
    and transitions outside the table with :promotion/invalid-transition.

  Eligibility gates ENTRY only: once a candidate is in :canary, exit
  to :promoted/:canary-failed is decided by the canary guardrails
  (Task 9.4) and never re-checks the entry eligibility."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.promotion.schema :as schema]
            [evoclj.promotion.state :as state]
            [malli.core :as m]))

(defn- uuid
  "A fixed, readable UUID for fixture ids."
  [n]
  (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil
  when nothing is thrown."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

(def ^:private eligible
  "Finalized eligibility data for an eligible evaluation (Task 8.x
  shape: :reasons is empty exactly when :eligible? is true)."
  {:eligible? true :reasons []})

(def ^:private ineligible
  "Finalized eligibility data for an ineligible evaluation."
  {:eligible? false
   :reasons [{:dimension :hard :rule :hard-violation
              :detail "a hard gate failed"}]})

;; --- Step 1: eligible-only entry to :canary / direct promotion ---------------

(deftest step1-eligible-candidate-may-enter-canary-or-direct-promotion
  (testing "an evaluated, ELIGIBLE candidate may enter :canary"
    (is (= :canary (state/deployment-transition :evaluated eligible :canary))))
  (testing "an evaluated, ELIGIBLE candidate may be directly promoted"
    (is (= :promoted (state/deployment-transition :evaluated eligible :promoted))))
  (testing "an evaluated, ELIGIBLE candidate may become :stale (Task 9.2 CAS loser)"
    (is (= :stale (state/deployment-transition :evaluated eligible :stale))))
  (testing "the evaluated-only rule is structural: only :evaluated carries
            the canary/promotion edges"
    (is (= #{:canary :promoted :rejected :stale}
           (state/next-candidate-states :evaluated)))
    (is (not (contains? (state/next-candidate-states :evaluation-pending) :canary))))
  (testing "a candidate that has not reached :evaluated cannot enter deployment"
    (is (= :promotion/invalid-transition
           (thrown-error-type
            #(state/deployment-transition :evaluation-pending eligible :canary))))
    (is (= :promotion/invalid-transition
           (thrown-error-type
            #(state/deployment-transition :proposed eligible :promoted))))
    (is (= :promotion/invalid-transition
           (thrown-error-type
            #(state/deployment-transition :invalid eligible :canary))))))

(deftest step1-canary-exit-never-rechecks-eligibility
  (testing "eligibility gates ENTRY to :canary/:promoted, not exit from :canary
            (canary exit is decided by the Task 9.4 guardrails)"
    (is (= :promoted
           (state/deployment-transition :canary ineligible :promoted)))
    (is (= :canary-failed
           (state/deployment-transition :canary eligible :canary-failed)))))

;; --- Step 2: an ineligible candidate can ONLY become :rejected ---------------

(deftest step2-ineligible-candidate-can-only-become-rejected
  (testing "an ineligible evaluated candidate may become :rejected"
    (is (= :rejected (state/deployment-transition :evaluated ineligible :rejected))))
  (testing "an ineligible candidate cannot enter :canary"
    (is (= :promotion/ineligible
           (thrown-error-type
            #(state/deployment-transition :evaluated ineligible :canary)))))
  (testing "an ineligible candidate cannot be directly promoted"
    (is (= :promotion/ineligible
           (thrown-error-type
            #(state/deployment-transition :evaluated ineligible :promoted)))))
  (testing "an ineligible candidate cannot become :stale either — only :rejected"
    (is (= :promotion/ineligible
           (thrown-error-type
            #(state/deployment-transition :evaluated ineligible :stale)))))
  (testing "the eligibility rule as data: ineligible → only :rejected"
    (is (= #{:rejected} (state/eligible-deployment-states ineligible)))
    (is (= #{:canary :promoted :stale} (state/eligible-deployment-states eligible)))))

;; --- Step 3: pure, closed tables; unknown states/transitions fail -------------

(deftest step3-generation-table-is-pure-and-closed
  (testing "the generation table is plain data (no SQL, no side effects)"
    (is (map? state/generation-transitions))
    (is (every? set? (vals state/generation-transitions))))
  (testing "closed: the vocabulary covers exactly the table's states"
    (is (= state/generation-states (set (keys state/generation-transitions))))
    (is (every? #(contains? state/generation-states %)
                (apply concat (vals state/generation-transitions)))))
  (testing "the normative generation edges"
    (is (= #{:active} (state/next-generation-states :seed)))
    (is (= #{:superseded :rolled-back} (state/next-generation-states :active)))
    (is (empty? (state/next-generation-states :superseded)))
    (is (empty? (state/next-generation-states :rolled-back))))
  (testing "a generation that was never active is never rolled back"
    (is (= :promotion/invalid-transition
           (thrown-error-type #(state/generation-transition :seed :rolled-back))))
    (is (= :promotion/invalid-transition
           (thrown-error-type #(state/generation-transition :superseded :active))))
    (is (= :promotion/invalid-transition
           (thrown-error-type #(state/generation-transition :rolled-back :active)))))
  (testing "valid generation transitions return the target state"
    (is (= :active (state/generation-transition :seed :active)))
    (is (= :superseded (state/generation-transition :active :superseded)))
    (is (= :rolled-back (state/generation-transition :active :rolled-back))))
  (testing "unknown states fail with a typed error"
    (is (= :promotion/unknown-state
           (thrown-error-type #(state/generation-transition :bogus :active))))
    (is (= :promotion/unknown-state
           (thrown-error-type #(state/generation-transition :seed :bogus))))
    (is (= :promotion/unknown-state
           (thrown-error-type #(state/next-generation-states :canary))))))

(deftest step3-candidate-table-is-pure-and-closed
  (testing "the candidate table is plain data"
    (is (map? state/candidate-transitions))
    (is (every? set? (vals state/candidate-transitions))))
  (testing "closed: the vocabulary covers exactly the table's states"
    (is (= state/candidate-states (set (keys state/candidate-transitions))))
    (is (every? #(contains? state/candidate-states %)
                (apply concat (vals state/candidate-transitions)))))
  (testing "the base machine fragment (Task 7.6 + M8) is preserved"
    (is (= #{:materialized} (state/next-candidate-states :proposed)))
    (is (= #{:evaluation-pending} (state/next-candidate-states :materialized)))
    (is (= #{:evaluated :invalid} (state/next-candidate-states :evaluation-pending))))
  (testing "the deployment fragment (M9)"
    (is (= #{:canary :promoted :rejected :stale}
           (state/next-candidate-states :evaluated)))
    (is (= #{:promoted :canary-failed} (state/next-candidate-states :canary)))
    (is (empty? (state/next-candidate-states :rejected)))
    (is (empty? (state/next-candidate-states :stale)))
    (is (empty? (state/next-candidate-states :promoted)))
    (is (empty? (state/next-candidate-states :canary-failed))))
  (testing "deployment transitions outside the table fail"
    (is (= :promotion/invalid-transition
           (thrown-error-type
            #(state/deployment-transition :canary eligible :rejected))))
    (is (= :promotion/invalid-transition
           (thrown-error-type
            #(state/deployment-transition :canary-failed eligible :promoted))))
    (is (= :promotion/invalid-transition
           (thrown-error-type
            #(state/deployment-transition :promoted eligible :canary))))
    (is (= :promotion/invalid-transition
           (thrown-error-type
            #(state/deployment-transition :stale eligible :canary)))))
  (testing "unknown states fail with a typed error"
    (is (= :promotion/unknown-state
           (thrown-error-type #(state/next-candidate-states :bogus))))
    (is (= :promotion/unknown-state
           (thrown-error-type #(state/deployment-transition :bogus eligible :canary))))
    (is (= :promotion/unknown-state
           (thrown-error-type #(state/deployment-transition :evaluated eligible :bogus))))))

;; --- the Promotion record data contract (docs 'Detailed Public Data Contracts') --

(deftest promotion-record-contract
  (let [record {:promotion/id (uuid 1)
                :candidate/id (uuid 2)
                :evaluation/id (uuid 3)
                :from-generation "generation-1"
                :to-generation "generation-2"
                :decision :promoted
                :reason {:eligibility eligible}
                :created-at (java.util.Date.)}]
    (testing "a valid Promotion record satisfies the contract"
      (is (nil? (m/explain schema/PromotionSchema record))))
    (testing "a malformed Promotion record fails Malli validation"
      (is (some? (m/explain schema/PromotionSchema (dissoc record :decision))))
      (is (some? (m/explain schema/PromotionSchema (assoc record :candidate/id "not-a-uuid")))))
    (testing "the state-value schemas admit exactly the vocabulary"
      (is (nil? (m/explain schema/GenerationStateSchema :seed)))
      (is (nil? (m/explain schema/GenerationStateSchema :rolled-back)))
      (is (some? (m/explain schema/GenerationStateSchema :canary)))
      (is (nil? (m/explain schema/DeploymentStateSchema :canary)))
      (is (nil? (m/explain schema/DeploymentStateSchema :canary-failed)))
      (is (some? (m/explain schema/DeploymentStateSchema :evaluated)))
      (is (nil? (m/explain schema/EligibilitySchema eligible)))
      (is (nil? (m/explain schema/EligibilitySchema ineligible)))
      (is (some? (m/explain schema/EligibilitySchema {:eligible? :yes}))))))
