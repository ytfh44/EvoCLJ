(ns evoclj.eval.compare
  "Lexicographic comparison of an evaluation summary against a profile
  (component).

  The comparison pipeline keeps every section SEPARATE and decides in
  LEXICOGRAPHIC order — hard first, then utility, then cost, then
  complexity. A hard violation makes the candidate ineligible with NO
  compensation from utility/cost (Global Constraint 14); a utility
  delta below the profile's :min-delta is ineligible; a cost
  regression beyond the profile's :max-cost-regression is ineligible;
  complexity is informational unless the profile declares a
  :max-complexity-regression guard.

  (eligibility evaluation-summary profile) returns explicit reason
  data:

      {:eligible? true  :reasons []}
      {:eligible? false :reasons [{:dimension :hard  :rule :hard-violation
                                   :metric :safety
                                   :detail {:violations [...]}}
                                  ...]}

  A failing dimension SHORT-CIRCUITS: later dimensions are not even
  examined, so no downstream metric can mask an upstream failure. The
  result carries the failing dimension's reasons only.

  Step 5: this namespace performs the comparison only. It never calls
  Promotion code — no evoclj.promotion.* require exists or is
  permitted here (Promotion is Milestone 9's boundary)."
  (:require [evoclj.eval.metrics :as metrics]
            [evoclj.eval.profile :as profile]))

;; --- thresholds ---------------------------------------------------------------

(defn- thresholds-for
  "The effective promotion thresholds for a profile: the profile's own
  values when declared, the canonical defaults otherwise
  (evoclj.eval.profile/default-promotion-thresholds). The complexity
  guard is special: it applies ONLY when the profile declares it —
  complexity is informational otherwise."
  [profile]
  (let [p (:promotion profile)
        d profile/default-promotion-thresholds]
    {:min-delta (or (:min-delta p) (:min-delta d))
     :max-cost-regression (or (:max-cost-regression p)
                              (:max-cost-regression d))
     :max-complexity-regression (:max-complexity-regression p)}))

;; --- per-dimension decisions ----------------------------------------------------

(defn- utility-reason
  "The utility decision: the section's total delta must be >= the
  profile's :min-delta. A delta below the threshold (including a
  regression) is a :below-min-delta reason."
  [summary thresholds]
  (let [delta (metrics/utility-delta summary)
        min-delta (:min-delta thresholds)]
    (when (< delta min-delta)
      [{:dimension :utility
        :rule :below-min-delta
        :metric :utility/total
        :detail {:delta delta :min-delta min-delta}}])))

(defn- guard-reason
  "The cost/complexity decision for one section: every candidate/parent
  ratio must be <= the profile's max; a ratio beyond it is a
  :max-cost-regression / :max-complexity-regression reason. `max-key`
  names the threshold key carried in the :detail evidence."
  [dimension rule max-key section-max regressions]
  (into []
        (keep (fn [reg]
                (when (> (:ratio reg) section-max)
                  {:dimension dimension
                   :rule rule
                   :metric (:metric reg)
                   :detail (assoc (dissoc reg :metric)
                                  max-key (double section-max))})))
        regressions))

;; --- the lexicographic decision --------------------------------------------------

(defn eligibility
  "The lexicographic eligibility decision for one evaluation summary
  against one profile (component, Step 4).

  Pipeline (short-circuiting — a failing dimension ends the check):

    1. hard       — any hard violation => ineligible, no compensation.
    2. utility    — total delta >= :min-delta required.
    3. cost       — every candidate/parent ratio <= :max-cost-regression.
    4. complexity — informational, or guarded by
                    :max-complexity-regression when the profile
                    declares it.

  Returns {:eligible? <bool> :reasons [<reason maps>]}; :reasons is
  empty exactly when :eligible? is true. The profile must satisfy the
  component contract (evoclj.eval.profile/validate-profile!) and the
  summary the component contract (metrics/validate-summary!)."
  [summary profile]
  (profile/validate-profile! profile)
  (metrics/validate-summary! summary)
  (let [ths (thresholds-for profile)
        hard (metrics/hard-violations summary)]
    (cond
      (seq hard)
      {:eligible? false :reasons hard}

      :else
      (let [util (utility-reason summary ths)]
        (if (seq util)
          {:eligible? false :reasons util}
          (let [cost (guard-reason :cost :max-cost-regression
                                   :max-cost-regression
                                   (:max-cost-regression ths)
                                   (metrics/cost-regressions summary))]
            (if (seq cost)
              {:eligible? false :reasons cost}
              (let [max-cx (:max-complexity-regression ths)
                    cx (when max-cx
                         (guard-reason :complexity
                                       :max-complexity-regression
                                       :max-complexity-regression
                                       max-cx
                                       (metrics/complexity-regressions summary)))]
                (if (seq cx)
                  {:eligible? false :reasons cx}
                  {:eligible? true :reasons []})))))))))
