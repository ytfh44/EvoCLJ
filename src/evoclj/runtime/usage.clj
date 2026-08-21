(ns evoclj.runtime.usage
  "Standard usage accounting across the runtime (component).

  Design: a PURE, merge-based accumulator. Usage is an immutable map
  of monotonic counters plus attribution keys; samples are combined
  with `add`, never mutated. The runtime already owns stateful atoms
  where call counting must be atomic (evoclj.intent.dispatch's
  per-:cap/id usage atom feeding the pure capability policy); this
  namespace standardizes the SAMPLE and AGGREGATION layer on top of
  those sources as pure functions, so episodes, evidence packs, and
  evaluation can combine and attribute usage without shared state
  (Global Constraints 20, 22).

  Key vocabulary is aligned with what the runtime already emits:

    - :steps, :wall-ms    — the exact :usage keys evoclj.sci.execute
                            reports (SCI interruption checks + elapsed)
    - :total-cost, :cost  — episode cost keys evoclj.evolution.evidence
                            and evoclj.evolution.diagnose read (fallback
                            order :total-cost then :cost); both merge
                            monotonically here
    - :provider-calls     — the per-:cap/id call-count map reported by
                            evoclj.intent.dispatch, standardized as a
                            single counter total (per-capability detail
                            stays in the dispatch result's :usage map)
    - :tool-calls         — phenotype-level tool invocations
    - :model-input-tokens, :model-output-tokens, :model-cost-units /
      :provider-reported-cost, :network-bytes, :artifact-bytes — the
      component normative counters; :provider-reported-cost is accepted
      as a counter key in its own right, :model-cost-units is canonical
      (callers pick one; both are monotonic if both appear)

  Attribution: each sample optionally carries :session/id, :intent/id,
  :node/id (Global Constraint 20 — every externally visible effect
  must be attributable). `add` is left-biased on attribution: the
  first sample's origin is preserved, never overwritten or summed.

  Samples may also carry an :outcome map ({:status :completed ...}),
  which survives aggregation untouched and drives per-outcome and
  per-successful-task aggregation (`successful?` matches the success
  rule of evoclj.evolution.evidence: :status :completed)."

  (:require [clojure.set :as set]))

(def ^:const empty-usage
  "The identity element for `add`: no counters, no attribution."
  {})

(def counter-keys
  "Keys that MERGE BY SUMMING. Every component normative counter plus
  the vocabulary the runtime already emits (:steps, :wall-ms,
  :total-cost, :cost). Summing is what makes accumulation monotonic."
  #{:wall-ms
    :model-input-tokens
    :model-output-tokens
    :model-reasoning-tokens
    :model-cost-units
    :provider-reported-cost
    :provider-calls
    :tool-calls
    :network-bytes
    :steps
    :artifact-bytes
    :total-cost
    :cost})

(def attribution-keys
  "Keys that are PRESERVED, never summed: the originating Intent,
  session, and node (Global Constraint 20)."
  #{:session/id :intent/id :node/id})

(defn- counter? [k]
  (contains? counter-keys k))

(defn- attribution? [k]
  (contains? attribution-keys k))

(defn attributed
  "Attach an attribution map to a bare usage map. Only
  :session/id, :intent/id, :node/id are accepted; any other key is
  rejected (validated public boundary, Global Constraint 22). Never
  overwrites an origin already present."
  [usage attribution]
  (let [unknown (set/difference (set (keys attribution)) attribution-keys)]
    (when (seq unknown)
      (throw (ex-info "usage attribution carries unknown keys"
                      {:error/type :usage/attribution-invalid
                       :unknown-keys (vec unknown)})))
    (reduce (fn [m k]
              (if (and (some? (get attribution k))
                       (nil? (get m k)))
                (assoc m k (get attribution k))
                m))
            usage
            attribution-keys)))

(defn add
  "Merge two usage samples into one immutable map (pure; neither
  argument is modified). Counters sum — accumulation is monotonic.
  Attribution is left-biased: the first sample's :session/id,
  :intent/id, :node/id are preserved, missing origin slots are filled
  from the right sample. Non-counter, non-attribution keys (e.g.
  :outcome) follow standard merge: left wins, right fills gaps."
  [a b]
  (reduce (fn [m k]
            (if (counter? k)
              (update m k (fnil + 0) (get b k 0))
              (cond
                (attribution? k) (if (some? (get m k))
                                   m
                                   (if (some? (get b k))
                                     (assoc m k (get b k))
                                     m))
                :else (if (contains? m k)
                        m
                        (if (contains? b k)
                          (assoc m k (get b k))
                          m)))))
          a
          (set/union (set (keys a)) (set (keys b)))))

(defn aggregate
  "Reduce a collection of usage samples into one usage map. The empty
  collection aggregates to `empty-usage`."
  [samples]
  (reduce add empty-usage samples))

(defn aggregate-by-session
  "Per-case aggregation: group samples by their :session/id and
  aggregate each group. Samples without a :session/id group under nil."
  [samples]
  (reduce (fn [acc sample]
            (update acc (:session/id sample) add sample))
          {}
          samples))

(defn aggregate-by-outcome
  "Partition samples by their outcome's :status keyword (:completed,
  :failed, nil when absent) and aggregate each partition."
  [samples]
  (reduce (fn [acc sample]
            (update acc (get-in sample [:outcome :status]) add sample))
          {}
          samples))

(defn successful?
  "An outcome is a success when its :status is :completed — the same
  rule evoclj.evolution.evidence applies to episodes (:failed,
  :budget-exhausted, :cancelled are failures, i.e. evidence)."
  [sample]
  (= :completed (get-in sample [:outcome :status])))

(defn aggregate-successful
  "Per-successful-task aggregation: aggregate only the samples whose
  :outcome marks a completed (successful) task."
  [samples]
  (aggregate (filter successful? samples)))
