(ns evoclj.context.eval
  "Three evaluation classes for context compression quality.

  Every compression run should be evaluated on three axes:
  1. RETENTION  — did the envelope preserve the information that future
     turns need? Measured by comparing residue and evidence coverage
     between the original context and the envelope.
  2. REGRESSION  — did compression cause any regression? Measured by
     checking whether the envelope's task/subgoal state is consistent
     with the original (via crosscheck).
  3. HALLUCINATION — did the model invent information not present in
     the original? Measured by checking whether every residue claim in
     the envelope can be traced to a source in the original context.

  In v0 the actual scoring is a stub: the eval module produces the
  eval record with PASS/FAIL/WARN status for each class. The host is
  responsible for filling in the scores (e.g. via an LLM judge or
  automated heuristics). The module validates that the scores are
  serializable and within bounds."
  (:require [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.crosscheck :as crosscheck]))

;; ---------------------------------------------------------------------------
;; Eval class keywords
;; ---------------------------------------------------------------------------

(def eval-retention
  "`:eval/retention` — Did the envelope preserve the information future
  turns need? Score: 0.0 (nothing preserved) to 1.0 (everything
  preserved)."
  :eval/retention)

(def eval-regression
  "`:eval/regression` — Did compression cause a regression in task or
  subgoal state? Score: 0.0 (total regression) to 1.0 (no regression)."
  :eval/regression)

(def eval-hallucination
  "`:eval/hallucination` — Did the model invent information not in the
  original? Score: 0.0 (pure hallucination) to 1.0 (no hallucination)."
  :eval/hallucination)

;; ---------------------------------------------------------------------------
;; Status keywords
;; ---------------------------------------------------------------------------

(def status-pass
  "`:status/pass` — The eval class meets the pass threshold."
  :status/pass)

(def status-warn
  "`:status/warn` — The eval class is below pass but above fail; review
  recommended."
  :status/warn)

(def status-fail
  "`:status/fail` — The eval class is below the fail threshold."
  :status/fail)

;; ---------------------------------------------------------------------------
;; Default thresholds
;; ---------------------------------------------------------------------------

(def ^:private default-thresholds
  {:retention  {:pass 0.8  :warn 0.5  :fail 0.0}
   :regression {:pass 0.9  :warn 0.7  :fail 0.0}
   :hallucination {:pass 0.9 :warn 0.7 :fail 0.0}})

;; ---------------------------------------------------------------------------
;; Threshold helpers
;; ---------------------------------------------------------------------------

(defn- threshold [class kind]
  (get-in default-thresholds [class kind] 0.0))

(defn- classify [class score]
  (let [pass (threshold class :pass)
        warn (threshold class :warn)]
    (cond
      (>= score pass) status-pass
      (>= score warn) status-warn
      :else status-fail)))

;; ---------------------------------------------------------------------------
;; Eval record
;; ---------------------------------------------------------------------------

(defn- eval-record
  [class score status details]
  {:eval/class class
   :eval/score score
   :eval/status status
   :eval/details (err/sanitize details)})

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn eval-retention-score
  "Score retention for `envelope` against `original-context`.

  In v0 this is a stub that returns the caller-supplied score. The host
  should fill this in by comparing residue/evidence coverage.

  `score` must be a number between 0.0 and 1.0.
  `details` is an optional map with scoring rationale.

  Returns an eval record map."
  ([envelope original-context score]
   (eval-retention-score envelope original-context score nil))
  ([envelope original-context score details]
   (envelope/validate-envelope envelope)
   (when-not (number? score)
     (throw (err/error :context/compression-invalid
                       "score must be a number"
                       {:score (err/sanitize score)})))
   (when (or (< score 0.0) (> score 1.0))
     (throw (err/error :context/compression-invalid
                       "score must be between 0.0 and 1.0"
                       {:score score})))
   (eval-record eval-retention score (classify :retention score) details)))

(defn eval-regression-score
  "Score regression for `envelope` against `original-context`.

   In v0 this is a stub. The host should fill this in by checking
   whether the envelope's structured fields are consistent with the
   original context.

   Returns an eval record map."
  ([envelope original-context score]
   (eval-regression-score envelope original-context score nil))
  ([envelope original-context score details]
   (envelope/validate-envelope envelope)
   (when-not (number? score)
     (throw (err/error :context/compression-invalid
                       "score must be a number"
                       {:score (err/sanitize score)})))
   (when (or (< score 0.0) (> score 1.0))
     (throw (err/error :context/compression-invalid
                       "score must be between 0.0 and 1.0"
                       {:score score})))
   (eval-record eval-regression score (classify :regression score) details)))

;; ---------------------------------------------------------------------------
;; Deprecated backward-compatible wrappers
;; ---------------------------------------------------------------------------

(defn eval-regression-score-deprecated
  "DEPRECATED: use `eval-regression-score` (2-arg) instead.

   The `todo` parameter is ignored; regression scoring no longer
   depends on a specific todo tool. Kept for backward compatibility
   during migration."
  ([envelope original-context todo score]
   (eval-regression-score envelope original-context score nil))
  ([envelope original-context todo score details]
   (eval-regression-score envelope original-context score details)))

(defn eval-hallucination-score
  "Score hallucination for `envelope` against `original-context`.

  In v0 this is a stub. The host should fill this in by tracing every
  residue claim in the envelope back to a source in the original
  context.

  Returns an eval record map."
  ([envelope original-context score]
   (eval-hallucination-score envelope original-context score nil))
  ([envelope original-context score details]
   (envelope/validate-envelope envelope)
   (when-not (number? score)
     (throw (err/error :context/compression-invalid
                       "score must be a number"
                       {:score (err/sanitize score)})))
   (when (or (< score 0.0) (> score 1.0))
     (throw (err/error :context/compression-invalid
                       "score must be between 0.0 and 1.0"
                       {:score score})))
   (eval-record eval-hallucination score (classify :hallucination score) details)))

(defn eval-summary
  "Combine multiple eval records into a summary map. `records` is a
  vector of eval record maps.

  Returns:
    {:eval/overall-status <best status across records>
     :eval/records [<record> ...]}
  The overall status is the WORST status across all records (fail < warn
  < pass)."
  [records]
  (when-not (coll? records)
    (throw (err/error :context/compression-invalid
                      "records must be a collection"
                      {:value (err/sanitize records)})))
  (let [statuses (map :eval/status records)
        worst (reduce (fn [acc s]
                        (cond
                          (= s status-fail) status-fail
                          (= s status-warn) (if (= acc status-pass) status-warn acc)
                          :else acc))
                      status-pass
                      statuses)]
    {:eval/overall-status worst
     :eval/records (vec records)}))

(defn passing?
  "True when `eval-summary` has overall status :status/pass."
  [summary]
  (= status-pass (:eval/overall-status summary)))

(defn failing?
  "True when `eval-summary` has overall status :status/fail."
  [summary]
  (= status-fail (:eval/overall-status summary)))