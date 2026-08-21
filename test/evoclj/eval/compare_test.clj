(ns evoclj.eval.compare-test
  "component tests: hard, utility, cost, and complexity metrics stay
  SEPARATE and comparison is LEXICOGRAPHIC (Global Constraint 14 —
  hard constraints dominate and are never collapsed into a
  compensating weighted score).

  The normative scenarios, in the task's numbered order:

  - Step 1 (no compensation): higher utility cannot rescue a hard
    safety violation, and a non-pass integrity side cannot be rescued
    either.
  - Step 2 (cost guardrail): utility improves but a cost metric
    regresses beyond the profile's :max-cost-regression — ineligible.
  - Step 3 (min-delta): a tiny/noisy utility improvement below the
    profile's :min-delta is not enough (and neither is a regression).
  - reason-data: eligibility returns explicit
    {:eligible? <bool> :reasons [{:dimension ... :rule ... :metric ...
    :detail ...} ...]} data; complexity is informational unless the
    profile guards it; thresholds are optional in the component profile
    contract and fall back to the canonical defaults.

  Step 5 (no Promotion coupling): this test asserts that neither the
  loaded namespaces nor evoclj.eval.compare's aliases touch
  evoclj.promotion.* — the comparison performs no promotion, ever."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.eval.compare :as compare]
            [evoclj.eval.metrics :as metrics]
            [evoclj.eval.profile :as profile]))

;; --- fixtures ----------------------------------------------------------------

(def ^:private base-summary
  "A fully passing evaluation summary — every section separate."
  {:hard {:safety {:parent 1.0 :candidate 1.0 :violations []}
          :integrity {:parent :pass :candidate :pass}}
   :utility {:task/success {:parent 0.72 :candidate 0.79}}
   :cost {:tokens/task {:parent 1200 :candidate 1212}
          :latency-ms {:parent 1500 :candidate 1500}}
   :complexity {:genome-bytes {:parent 18000 :candidate 18000}
                :graph-nodes {:parent 4 :candidate 4}}})

(defn- promotion-profile
  "A component profile carrying the component promotion thresholds."
  ([] (promotion-profile {}))
  ([overrides]
   (merge {:eval/profile-id :test/v1
           :evolution-set {:source :evals/evolution}
           :selection-set {:source :evals/selection :visibility :kernel-only}
           :audit-set {:source :evals/audit :visibility :operator-only}
           :repetitions 1
           :promotion {:strategy :paired-comparison
                       :min-delta 0.05
                       :max-cost-regression 1.10
                       :max-complexity-regression 1.25}}
          overrides)))

(defn- approx
  "Equality within 1e-9 (for double arithmetic)."
  [a b]
  (< (Math/abs (double (- a b))) 1e-9))

;; --- Step 1: hard violations are never compensated ----------------------------

(deftest hard-safety-violation-cannot-be-compensated-by-utility
  (let [summary (assoc-in base-summary [:hard :safety :violations]
                          [{:check :protected-path :path "eval/tamper.edn"}])
        ;; utility improvement is far beyond any min-delta — irrelevant
        summary (assoc-in summary [:utility :task/success]
                          {:parent 0.30 :candidate 0.95})
        outcome (compare/eligibility summary (promotion-profile))]
    (testing "ineligible with the hard reason and NOTHING else"
      (is (false? (:eligible? outcome)))
      (is (= [{:dimension :hard
               :rule :hard-violation
               :metric :safety
               :detail {:violations [{:check :protected-path
                                      :path "eval/tamper.edn"}]}}]
             (:reasons outcome))))))

(deftest hard-integrity-nonpass-cannot-be-compensated-by-utility
  (let [summary (assoc-in base-summary [:hard :integrity :candidate] :fail)
        summary (assoc-in summary [:utility :task/success]
                          {:parent 0.10 :candidate 0.99})
        outcome (compare/eligibility summary (promotion-profile))]
    (is (false? (:eligible? outcome)))
    (is (= [{:dimension :hard
             :rule :hard-violation
             :metric :integrity
             :detail {:parent :pass :candidate :fail}}]
           (:reasons outcome)))))

;; --- Step 2: cost regression guardrail ----------------------------------------

(deftest utility-improvement-cannot-exceed-cost-regression
  (let [summary (assoc-in base-summary [:cost :tokens/task]
                          {:parent 1200 :candidate 1500}) ; ratio 1.25 > 1.10
        outcome (compare/eligibility summary (promotion-profile))]
    (is (false? (:eligible? outcome)))
    (let [r (first (:reasons outcome))]
      (is (= :cost (:dimension r)))
      (is (= :max-cost-regression (:rule r)))
      (is (= :tokens/task (:metric r)))
      (is (= {:parent 1200 :candidate 1500
              :ratio 1.25 :max-cost-regression 1.10}
             (:detail r))))))

;; --- Step 3: min-delta — tiny/noisy improvement is not enough ------------------

(deftest tiny-improvement-below-min-delta-is-ineligible
  (let [summary (assoc-in base-summary [:utility :task/success]
                          {:parent 0.72 :candidate 0.73}) ; delta 0.01 < 0.05
        outcome (compare/eligibility summary (promotion-profile))]
    (is (false? (:eligible? outcome)))
    (let [r (first (:reasons outcome))]
      (is (= :utility (:dimension r)))
      (is (= :below-min-delta (:rule r)))
      (is (= :utility/total (:metric r)))
      (is (approx (:delta (:detail r)) 0.01))
      (is (= 0.05 (:min-delta (:detail r)))))))

(deftest utility-regression-is-below-min-delta-too
  (let [summary (assoc-in base-summary [:utility :task/success]
                          {:parent 0.72 :candidate 0.70})
        outcome (compare/eligibility summary (promotion-profile))]
    (is (false? (:eligible? outcome)))
    (is (= :below-min-delta (:rule (first (:reasons outcome)))))))

;; --- reason data: the passing baseline and the section rules -------------------

(deftest passing-summary-is-eligible-with-empty-reasons
  (let [outcome (compare/eligibility base-summary (promotion-profile))]
    (is (true? (:eligible? outcome)))
    (is (= [] (:reasons outcome)))))

(deftest complexity-is-informational-without-a-profile-guard
  (let [p (promotion-profile {:promotion {:strategy :paired-comparison
                                          :min-delta 0.05
                                          :max-cost-regression 1.10}})
        summary (assoc-in base-summary [:complexity :genome-bytes]
                          {:parent 18000 :candidate 90000})]
    (let [outcome (compare/eligibility summary p)]
      (is (true? (:eligible? outcome)))
      (is (= [] (:reasons outcome))))))

(deftest complexity-guard-makes-regression-ineligible
  (let [summary (assoc-in base-summary [:complexity :genome-bytes]
                          {:parent 18000 :candidate 90000}) ; ratio 5.0 > 1.25
        outcome (compare/eligibility summary (promotion-profile))]
    (is (false? (:eligible? outcome)))
    (let [r (first (:reasons outcome))]
      (is (= :complexity (:dimension r)))
      (is (= :max-complexity-regression (:rule r)))
      (is (= :genome-bytes (:metric r)))
      (is (> (:ratio (:detail r)) 1.25)))))

;; --- boundary validation --------------------------------------------------------

(deftest eligibility-validates-the-summary-contract
  (is (metrics/summary? base-summary))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"evaluation summary"
                        (compare/eligibility {:hard {} :utility {}}
                                             (promotion-profile)))))

(deftest eligibility-rejects-invalid-profiles
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"evaluation profile"
                        (compare/eligibility
                         base-summary
                         (assoc (promotion-profile) :repetitions 0)))))

;; --- metric math keeps sections separate ----------------------------------------

(deftest metric-math-keeps-sections-separate
  (testing "utility delta is the sum of per-metric candidate-parent deltas"
    (is (approx (metrics/utility-delta base-summary) 0.07)))
  (testing "cost regression ratios are candidate/parent"
    (let [regs (metrics/cost-regressions base-summary)
          tok (first (filter #(= :tokens/task (:metric %)) regs))]
      (is (approx (:ratio tok) 1.01))))
  (testing "zero-cost parents are guarded (no division by zero)"
    (is (= Double/POSITIVE_INFINITY
           (metrics/ratio {:parent 0 :candidate 5})))
    (is (= 1.0 (metrics/ratio {:parent 0 :candidate 0})))))

(deftest summarize-utility-derives-rates-from-a-paired-run
  (let [paired {:parent {:side/kind :parent :cases 3 :passed 2 :failed 1 :score 2.0}
                :candidate {:side/kind :candidate :cases 3 :passed 3 :failed 0 :score 3.0}
                :pairs [] :aggregate {}}]
    (is (approx (get-in (metrics/summarize-utility paired)
                        [:utility :task/success :parent])
                2/3))
    (is (= 1.0 (get-in (metrics/summarize-utility paired)
                       [:utility :task/success :candidate])))))

;; --- profile schema carries the component thresholds ------------------------------

(deftest profile-schema-carries-promotion-thresholds
  (is (profile/profile? (promotion-profile)))
  (is (profile/profile? profile/default-v1))
  (is (= 0.05 (get-in profile/default-v1 [:promotion :min-delta])))
  (is (= 1.10 (get-in profile/default-v1 [:promotion :max-cost-regression])))
  (testing "thresholds are optional in the contract — a bare profile still validates"
    (is (profile/profile?
         (promotion-profile {:promotion {:strategy :paired-comparison}}))))
  (testing "canonical defaults apply when a profile omits the thresholds"
    (let [outcome (compare/eligibility
                   base-summary
                   (promotion-profile {:promotion {:strategy :paired-comparison}}))]
      (is (true? (:eligible? outcome))))))

;; --- Step 5: no Promotion coupling ------------------------------------------------

(deftest comparison-namespace-never-calls-promotion
  ;; M9 created the evoclj.promotion.* namespaces (component), so the M8
  ;; 'no promotion namespace is loaded anywhere' (all-ns) guard is obsolete;
  ;; the durable guarantee is that THIS namespace never couples to promotion.
  (testing "evoclj.eval.compare requires no evoclj.promotion.* alias"
    (is (not-any? #(str/starts-with? (str (ns-name %)) "evoclj.promotion")
                  (vals (ns-aliases 'evoclj.eval.compare))))))
