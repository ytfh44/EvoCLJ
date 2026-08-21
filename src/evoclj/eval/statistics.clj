(ns evoclj.eval.statistics
  "Descriptive statistics over repeated paired observations (component).

  The NORMATIVE interface:

      (summarize-paired-deltas pairs)
      ;; => {:n ... :mean-delta ... :median-delta ... :wins ... :losses ... :ties ...}

  `pairs` is the vector of RAW paired observations — one map per
  repetition of the parent/candidate comparison on the same case set
  and environment fixture (Global Constraint 13):

      [{:parent 0.72 :candidate 0.79}   ; delta +0.07 — candidate win
       {:parent 0.75 :candidate 0.70}   ; delta -0.05 — candidate loss
       {:parent 0.60 :candidate 0.60}]  ; delta  0.0  — tie

  Each pair's delta is candidate - parent, the same sign convention as
  evoclj.eval.paired's :case/outcome :delta. wins = pairs where the
  candidate is strictly better, losses = pairs where it is strictly
  worse, ties = pairs whose deltas are equal (including pairs where
  both sides scored zero — the G5 runner's finer :both-failed status
  stays in the raw pair records; this summary only distinguishes
  sign).

  DESCRIPTIVE ONLY — NO PROBABILITY OR CALIBRATION CLAIMS (Step 4):
  this namespace computes sample-descriptive statistics — count, mean,
  median, and win/loss/tie counts — and NOTHING ELSE. It does NOT
  compute p-values, confidence intervals, effect-size significance,
  posterior/calibration probabilities, or any other formal inferential
  claim, because no sampling model, independence, or distribution
  assumption is stated or verified here. The sample is what it is; the
  summary must never be read as a probability that the candidate is
  better. The output key set is exactly
  #{:n :mean-delta :median-delta :wins :losses :ties}; :mean-delta and
  :median-delta are nil for an empty sample.

  STORAGE AND RECOMPUTABILITY (Step 3): the pairs vector IS the
  durable raw observation record. Summaries are a pure deterministic
  function of it — summarizing twice over the same raw pairs yields
  identical results, and summarizing never consumes or mutates the
  input, so the raw observations stay available for later
  recomputation or re-analysis. pairs-artifact-ref derives the
  content-hash artifact reference (Global Constraint 21 — SQLite rows
  reference the hash, never duplicated payload bodies) that the
  persistence layer (component) stores instead of the body.

  STEP 5 — PROFILE-DECLARED SAMPLE REQUIREMENTS: promotion-checks
  exposes the checks for high-risk mutations as pure reason data in
  the component shape:

      (promotion-checks summary profile)
      ;; => []                                        ; every declared check passes
      ;; => [{:dimension :paired :rule :below-min-pairs ...} ...]

  The profile's :promotion block MAY declare :min-pairs (the minimum
  number of paired observations required) and
  :max-candidate-failure-rate (the maximum fraction of pairs the
  candidate may LOSE — losses / n). A check applies ONLY when the
  profile declares it, mirroring how evoclj.eval.compare treats the
  optional complexity guard. The component profile schema (closed map)
  does not yet carry these keys; extending the schema and wiring these
  checks into evoclj.eval.compare's lexicographic pipeline is component — here the checks are exposed as data with complete evidence, so
  that wiring is trivially additive.

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/statistics-pairs-invalid."
  (:require [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]))

;; --- input contract -----------------------------------------------------------

(defn- pairs-error
  [reason message value]
  (err/error :eval/statistics-pairs-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-pairs!
  "The raw paired observations must be a sequential collection of maps,
  each carrying numeric :parent and :candidate scores. Deltas are
  derived from the raw scores, never trusted from the caller — the
  summary stays recomputable from the stored observations."
  [pairs]
  (when-not (sequential? pairs)
    (throw (pairs-error :not-sequential
                        "paired observations must be a sequential collection"
                        pairs)))
  (doseq [pair pairs]
    (when-not (map? pair)
      (throw (pairs-error :pair-not-a-map
                          "paired observations must be a sequential collection of maps"
                          pair)))
    (when-not (and (number? (:parent pair)) (number? (:candidate pair)))
      (throw (pairs-error :non-numeric-score
                          "each paired observation must carry numeric :parent and :candidate scores"
                          pair))))
  pairs)

;; --- pure descriptive math -----------------------------------------------------

(defn- pair-delta
  "The paired delta for one observation: candidate - parent (positive =
  the candidate improved on that repetition)."
  [{:keys [parent candidate]}]
  (double (- candidate parent)))

(defn- median-delta
  "The sample median of the deltas: the middle value for an odd count,
  the mean of the two middle values for an even count; nil for an
  empty sample."
  [deltas]
  (let [sorted (sort deltas)
        n (count sorted)]
    (when (pos? n)
      (let [mid (quot n 2)]
        (if (odd? n)
          (nth sorted mid)
          (/ (+ (nth sorted (dec mid)) (nth sorted mid)) 2.0))))))

;; --- the normative interface -----------------------------------------------------

(defn summarize-paired-deltas
  "The NORMATIVE component interface: DESCRIPTIVE statistics over the
  raw paired observations.

      (summarize-paired-deltas pairs)
      ;; => {:n 3 :mean-delta 0.02 :median-delta 0.0
      ;;     :wins 1 :losses 1 :ties 1}

  Deterministic and pure: identical raw pairs always yield identical
  summaries, the input is never consumed or mutated, and the summary
  is recomputable from the stored raw observations at any time (Step
  3). The output is descriptive ONLY — no probability, confidence, or
  calibration claim is computed or implied (Step 4)."
  [pairs]
  (validate-pairs! pairs)
  (let [n (count pairs)
        deltas (mapv pair-delta pairs)]
    {:n n
     :mean-delta (when (pos? n) (double (/ (reduce + 0.0 deltas) n)))
     :median-delta (median-delta deltas)
     :wins (count (filter pos? deltas))
     :losses (count (filter neg? deltas))
     :ties (count (filter zero? deltas))}))

(defn pairs-artifact-ref
  "The content-hash artifact reference for the raw paired observations
  (Global Constraint 21): a deterministic digest over the EDN of the
  pairs vector. The persistence layer (component) stores THIS ref in
  SQLite rows instead of duplicating the payload body; the raw pairs
  remain available for recomputation, keyed by this ref."
  [pairs]
  (validate-pairs! pairs)
  (hash/text-digest (pr-str pairs)))

;; --- Step 5: profile-declared sample requirements --------------------------------

(defn- min-pairs-reason
  "The :min-pairs check: the sample must contain at least the profile's
  declared minimum number of paired observations (Step 5 — high-risk
  mutations need enough repetitions before any signal is read)."
  [summary min-pairs]
  (let [n (:n summary)]
    (when (< n min-pairs)
      [{:dimension :paired
        :rule :below-min-pairs
        :metric :pairs/n
        :detail {:n n :min-pairs min-pairs}}])))

(defn- failure-rate-reason
  "The :max-candidate-failure-rate check (Step 5): the candidate's
  observed failure rate — losses / n, the fraction of paired
  repetitions the candidate LOST — must not exceed the profile's
  declared maximum. Skipped on an empty sample (a rate over zero
  pairs is undefined)."
  [summary max-rate]
  (let [n (:n summary)
        losses (:losses summary)]
    (when (pos? n)
      (let [rate (double (/ losses n))]
        (when (> rate max-rate)
          [{:dimension :paired
            :rule :above-max-candidate-failure-rate
            :metric :candidate/failure-rate
            :detail {:losses losses
                     :n n
                     :failure-rate rate
                     :max-candidate-failure-rate max-rate}}])))))

(defn promotion-checks
  "The component Step 5 data checks for high-risk mutations, exposed as
  pure reason data in the component shape.

      (promotion-checks summary profile)
      ;; => []                                         ; every declared check passes
      ;; => [{:dimension :paired :rule :below-min-pairs
      ;;       :metric :pairs/n :detail {:n 3 :min-pairs 10}} ...]

  The profile's :promotion block MAY declare :min-pairs (the minimum
  number of paired observations) and :max-candidate-failure-rate (the
  maximum fraction of pairs the candidate may lose). A check applies
  ONLY when the profile declares it. Returns a vector of failing
  reason maps — empty exactly when every declared check passes. The
  reasons carry complete evidence (:detail), so wiring them into
  evoclj.eval.compare's lexicographic pipeline (component) is
  trivially additive."
  [summary profile]
  (let [p (or (:promotion profile) {})]
    (into []
          (concat (when-let [min-pairs (:min-pairs p)]
                    (min-pairs-reason summary min-pairs))
                  (when-let [max-rate (:max-candidate-failure-rate p)]
                    (failure-rate-reason summary max-rate))))))
