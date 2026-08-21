(ns evoclj.eval.metrics
  "Evaluation summary metrics (component).

  The NORMATIVE evaluation summary keeps every section SEPARATE —
  hard, utility, cost, and complexity never collapse into a single
  compensating scalar (Global Constraint 14). This namespace owns the
  summary contract and the pure metric math over it; the
  lexicographic DECISION lives in evoclj.eval.compare.

      {:hard {:safety {:parent 1.0 :candidate 1.0 :violations []}
              :integrity {:parent :pass :candidate :pass}}
       :utility {:task/success {:parent 0.72 :candidate 0.79}}
       :cost {:tokens/task {:parent 1200 :candidate 1260}
              :latency-ms {:parent 1500 :candidate 1580}}
       :complexity {:genome-bytes {:parent 18000 :candidate 18600}
                    :graph-nodes {:parent 4 :candidate 4}}}

  Section semantics:

  - :hard — safety/integrity/policy observations. A metric entry
    either carries a :violations vector (pass = empty) or a status
    pair (pass = both sides :pass). Any other shape is a hard
    failure.
  - :utility — higher-is-better metrics; the section's comparison
    value is the SUM of per-metric candidate-parent deltas.
  - :cost — lower-is-better metrics; the guardrail compares each
    metric's candidate/parent ratio against the profile's
    :max-cost-regression.
  - :complexity — informational by default; guarded only when the
    profile declares a :max-complexity-regression threshold.

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/summary-invalid (closed-map contract violation, Malli
  explanations)."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err]))

;; --- the normative summary contract -------------------------------------------

(def MetricEntrySchema
  "One metric entry: the parent and candidate values. A hard metric
  entry MAY additionally carry a :violations vector (pass = empty)."
  [:map {:closed true}
   [:parent any?]
   [:candidate any?]
   [:violations {:optional true} [:sequential any?]]])

(def NumericMetricEntrySchema
  "A numeric metric entry (utility/cost/complexity sections)."
  [:map {:closed true}
   [:parent number?]
   [:candidate number?]])

(def EvalSummarySchema
  "The NORMATIVE component evaluation summary contract (closed). Every
  section stays separate; no section is ever folded into another."
  [:map {:closed true}
   [:hard [:map-of keyword? MetricEntrySchema]]
   [:utility [:map-of keyword? NumericMetricEntrySchema]]
   [:cost [:map-of keyword? NumericMetricEntrySchema]]
   [:complexity [:map-of keyword? NumericMetricEntrySchema]]])

(defn summary?
  "True when `x` satisfies the normative EvalSummarySchema (closed-map
  contract)."
  [x]
  (boolean (m/validate EvalSummarySchema x)))

(defn validate-summary!
  "Validate `x` against the normative evaluation summary contract.
  Returns `x` on success; throws :eval/summary-invalid with humanized
  Malli explanations otherwise."
  [x]
  (when-not (summary? x)
    (let [expl (m/explain EvalSummarySchema x)]
      (throw (err/error :eval/summary-invalid
                        "value does not satisfy the evaluation summary contract"
                        {:errors (me/humanize expl)}))))
  x)

;; --- pure metric math over a summary -------------------------------------------

(defn delta
  "The candidate-parent delta for one numeric metric entry (positive =
  the candidate improved)."
  [{:keys [parent candidate]}]
  (double (- candidate parent)))

(defn ratio
  "The candidate/parent regression ratio for one numeric metric entry.
  Zero parents are guarded: 0/0 is 1.0 (no regression) and c/0 for
  c > 0 is positive infinity (an unbounded regression)."
  [{:keys [parent candidate]}]
  (cond
    (and (zero? parent) (zero? candidate)) 1.0
    (zero? parent) Double/POSITIVE_INFINITY
    :else (double (/ candidate parent))))

(defn utility-delta
  "The utility section's total delta: the sum of per-metric
  candidate-parent deltas. Every utility metric is higher-is-better in
  comparable units (task success rates), so the sum is the section's
  single comparison value."
  [summary]
  (reduce + 0.0 (map (comp delta val) (:utility summary))))

(defn hard-violations
  "Every hard-section violation as an explicit reason map:

      {:dimension :hard :rule :hard-violation :metric <metric>
       :detail <the offending entry>}

  A hard metric entry passes when it carries no violations AND both
  sides are :pass. Any other shape is a hard failure — nothing
  downstream compensates (Global Constraint 14)."
  [summary]
  (into []
        (keep (fn [[metric entry]]
                (if (contains? entry :violations)
                  (when (seq (:violations entry))
                    {:dimension :hard :rule :hard-violation
                     :metric metric
                     :detail {:violations (:violations entry)}})
                  (when-not (and (= :pass (:parent entry))
                                 (= :pass (:candidate entry)))
                    {:dimension :hard :rule :hard-violation
                     :metric metric
                     :detail {:parent (:parent entry)
                              :candidate (:candidate entry)}}))))
        (:hard summary)))

(defn- regressions
  "Per-metric candidate/parent regression records for one section:
  [{:metric <kw> :parent n :candidate n :ratio n} ...]."
  [section summary]
  (into []
        (map (fn [[metric entry]]
               {:metric metric
                :parent (:parent entry)
                :candidate (:candidate entry)
                :ratio (ratio entry)}))
        (get summary section)))

(defn cost-regressions
  "Per-metric cost regression records over the :cost section:
  [{:metric <kw> :parent n :candidate n :ratio n} ...]."
  [summary]
  (regressions :cost summary))

(defn complexity-regressions
  "Per-metric complexity regression records over the :complexity
  section: [{:metric <kw> :parent n :candidate n :ratio n} ...]."
  [summary]
  (regressions :complexity summary))

;; --- bridging the G5 paired runner ---------------------------------------------

(defn summarize-utility
  "The :utility section from a G5 paired run result (component): the
  task/success rate per side — the side's total score over its cases,
  normalized to a 0..1 rate. Only the utility section is derivable
  from the paired result here; hard/cost/complexity observations
  arrive from their own measurement points (gates, scheduler
  telemetry, genome stats) and are merged into the summary by the
  orchestrator (component)."
  [paired-result]
  (letfn [(rate [side]
            (let [n (max 1 (long (or (:cases side) 0)))]
              (double (/ (double (or (:score side) 0.0)) n))))]
    {:utility {:task/success {:parent (rate (:parent paired-result))
                              :candidate (rate (:candidate paired-result))}}}))
