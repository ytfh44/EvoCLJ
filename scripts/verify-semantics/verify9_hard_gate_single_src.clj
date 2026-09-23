(ns verify9-hard-gate-single-src
  "Semantic verification #9 — the hard gate has ONE rule implementation.

  Model: the G6 hard gate (evoclj.eval.core) and the eligibility decision
  (evoclj.eval.compare) both answer the same question — does this summary
  violate the profile's cost/complexity thresholds? If two implementations
  existed they could disagree; the claim is that the gate DERIVES its reasons
  from compare/thresholds-for + compare/guard-reason over the SAME
  metrics/cost-regressions / metrics/complexity-regressions records, so the
  two answers are equal by construction.

  Enumerated (finite domain): a summary whose cost ratio exceeds the profile
  max, one whose complexity ratio exceeds it, one that violates both, and one
  that violates neither — for each, the eligibility reasons must equal the
  reasons recomputed independently through guard-reason, item for item.

  Real code: evoclj.eval.compare/{thresholds-for,guard-reason,eligibility},
  evoclj.eval.metrics/{cost-regressions,complexity-regressions},
  evoclj.eval.statistics/promotion-checks (the Step-5 sample floor).
  Structural half: evoclj/eval/core.clj must not re-implement the ratio math
  (no `metrics/ratio`, no `:max-cost-regression` threshold literal)."
  (:require [clojure.string :as str]
            [evoclj.eval.compare :as compare]
            [evoclj.eval.core :as core]
            [evoclj.eval.metrics :as metrics]
            [evoclj.eval.profile :as profile]
            [evoclj.eval.statistics :as statistics]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(defn- summary
  "A schema-valid evaluation summary; the cost/complexity sections are the
  only ones the gate reads here."
  [cost complexity & [sample]]
  (cond-> {:hard {:task/success {:parent :pass :candidate :pass}}
           :utility {:task/success {:parent 0.5 :candidate 0.8}}
           :cost cost
           :complexity complexity}
    sample (assoc :sample sample)))

(defn- profile-with
  "The :default-v1 profile with explicit promotion thresholds."
  [promotion]
  (assoc profile/default-v1 :promotion (merge {:strategy :paired-comparison}
                                              promotion)))

;; ---------------------------------------------------------------------------
;; (a) behavioural: eligibility reasons == independently recomputed reasons
;; ---------------------------------------------------------------------------

(defn- independent-cost-reasons
  [summary profile]
  (compare/guard-reason :cost :max-cost-regression :max-cost-regression
                        (:max-cost-regression (compare/thresholds-for profile))
                        (metrics/cost-regressions summary)))

(defn- independent-complexity-reasons
  [summary profile]
  (compare/guard-reason :complexity :max-complexity-regression
                        :max-complexity-regression
                        (:max-complexity-regression (compare/thresholds-for profile))
                        (metrics/complexity-regressions summary)))

(def cases
  "The finite domain: [label summary profile expected-dimension]."
  [["cost over the max"
    (summary {:tokens {:parent 100 :candidate 150}} {:steps {:parent 1 :candidate 1}})
    (profile-with {:max-cost-regression 1.10 :max-complexity-regression 1.25})
    :cost]
   ["complexity over the max"
    (summary {:tokens {:parent 100 :candidate 105}} {:steps {:parent 1 :candidate 2}})
    (profile-with {:max-cost-regression 1.10 :max-complexity-regression 1.25})
    :complexity]
   ["both over the max (cost is checked first)"
    (summary {:tokens {:parent 100 :candidate 150}} {:steps {:parent 1 :candidate 2}})
    (profile-with {:max-cost-regression 1.10 :max-complexity-regression 1.25})
    :cost]
   ["neither over the max"
    (summary {:tokens {:parent 100 :candidate 105}} {:steps {:parent 1 :candidate 1}})
    (profile-with {:max-cost-regression 1.10 :max-complexity-regression 1.25})
    nil]
   ["complexity is informational when the profile omits the guard"
    (summary {:tokens {:parent 100 :candidate 105}} {:steps {:parent 1 :candidate 9}})
    (profile-with {:max-cost-regression 1.10})
    nil]])

(doseq [[label s p expected] cases]
  (let [decision (compare/eligibility s p)
        cost (independent-cost-reasons s p)
        complexity (when (:max-complexity-regression (compare/thresholds-for p))
                     (independent-complexity-reasons s p))
        expected-reasons (or (seq cost) (seq complexity) [])]
    (check! (str "eligibility reasons == guard-reason for: " label)
            (= expected-reasons (:reasons decision))
            (str "eligible? " (:eligible? decision)
                 (when (seq (:reasons decision))
                   (str " reasons " (mapv :rule (:reasons decision))))))
    (check! (str "the failing dimension is " (or expected :none) " for: " label)
            (= expected (some-> decision :reasons first :dimension))
            (str "short-circuit order: hard -> utility -> cost -> complexity -> sample"))))

;; The thresholds themselves come from ONE place: the profile's declaration
;; wins, the canonical default fills the rest.
(check! "thresholds-for merges the profile over the canonical defaults"
        (let [t (compare/thresholds-for (profile-with {:max-cost-regression 1.5}))]
          (and (= 1.5 (:max-cost-regression t))
               (= (:min-delta profile/default-promotion-thresholds) (:min-delta t))
               (nil? (:max-complexity-regression t))))
        "one threshold resolver (compare/thresholds-for)")

;; ---------------------------------------------------------------------------
;; (b) the Step-5 sample floor is enforced by the same pipeline
;; ---------------------------------------------------------------------------

(let [p (profile-with {:min-pairs 4 :max-candidate-failure-rate 0.25})
      thin (summary {:tokens {:parent 100 :candidate 100}}
                    {:steps {:parent 1 :candidate 1}}
                    {:n 2 :losses 0})
      lossy (summary {:tokens {:parent 100 :candidate 100}}
                     {:steps {:parent 1 :candidate 1}}
                     {:n 8 :losses 4})
      good (summary {:tokens {:parent 100 :candidate 100}}
                    {:steps {:parent 1 :candidate 1}}
                    {:n 8 :losses 1})]
  (check! "a sample below :min-pairs is ineligible"
          (= [:below-min-pairs] (mapv :rule (:reasons (compare/eligibility thin p))))
          "promotion-checks runs as the pipeline's final step")
  (check! "a candidate failure rate above the max is ineligible"
          (= [:above-max-candidate-failure-rate]
             (mapv :rule (:reasons (compare/eligibility lossy p))))
          "the failure-rate check reads the same :sample section")
  (check! "a sample meeting both floors is eligible"
          (true? (:eligible? (compare/eligibility good p)))
          "n = 8, losses = 1 against :min-pairs 4 / max rate 0.25")
  (check! "promotion-checks is the single rule source for the sample floor"
          (= (statistics/promotion-checks (:sample thin) p)
             (:reasons (compare/eligibility thin p)))
          "eligibility returns exactly what promotion-checks computed"))

;; ---------------------------------------------------------------------------
;; (c) structural: the gate does not re-implement the ratio rule
;; ---------------------------------------------------------------------------

(let [src (slurp "src/evoclj/eval/core.clj")]
  (check! "eval/core.clj does not re-implement the ratio math"
          (not (str/includes? src "metrics/ratio"))
          "the gate must consume compare/guard-reason, not metrics/ratio")
  (check! "eval/core.clj carries no numeric cost threshold"
          (not (re-find #":max-cost-regression\s+[0-9]" src))
          "the threshold value is read through compare/thresholds-for")
  (check! "eval/core.clj delegates to the compare rule"
          (and (str/includes? src "compare/guard-reason")
               (str/includes? src "compare/thresholds-for"))
          "the single rule implementation is compare/guard-reason"))

(println "VERIFY9 DONE")
