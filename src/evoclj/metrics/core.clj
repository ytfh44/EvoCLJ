(ns evoclj.metrics.core
  "Unified metric-record vocabulary shared across evaluation,
  evolution, and runtime (Foundation F2).

  A metric record is ONE timestamped observation of a measured value
  under a named scope:

      {:metric/id     #uuid\"...\"   ; fresh per record
       :metric/name   :task/success
       :metric/scope  :eval          ; :eval | :evolution | :runtime | ...
       :metric/scope-id \"run-7\"
       :metric/value  0.79
       :metric/unit   :rate
       :metric/at     #inst\"...\"}  ; wall-clock of capture

  This namespace is the generic DESCRIPTIVE vocabulary: it owns the
  record schema and the pure per-collection aggregates (mean, median,
  quantiles, ...). It makes NO inferential claim about a population —
  aggregates here are pure deterministic functions of the observed
  values exactly as stored. Any statement about confidence, uncertainty
  over an unknown population, or resampling lives in a SEPARATE
  namespace (evoclj.metrics.inference) that states its own assumptions
  and epistemics explicitly. The raw record remains the durable
  observation; a summary is always recomputable from it and never
  mutates the store.

  A collector is an atom holding a vector of metric records, so
  recording is a pure in-memory side effect bound to a single process:

      (def c (atom []))
      (collect-metric! c (record-metric :task/success :eval \"run-7\" 0.79 :rate))

  Error contract (Global Constraint 22 — plain serializable data):
  :metrics/invalid."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err]))

;; --- the metric-record contract -------------------------------------------------

(def MetricSchema
  "The closed metric-record contract. Every record carries a fresh
  :metric/id (uuid), a namespaced :metric/name, a :metric/scope and
  :metric/scope-id identifying WHERE the observation was made, a
  numeric :metric/value, a :metric/unit, and a capture instant
  :metric/at. Unknown keys are rejected — the record vocabulary is
  shared across subsystems, so shape drift is a contract violation."
  [:map {:closed true}
   [:metric/id uuid?]
   [:metric/name keyword?]
   [:metric/scope keyword?]
   [:metric/scope-id string?]
   [:metric/value number?]
   [:metric/unit keyword?]
   [:metric/at inst?]])

(defn- invalid-error
  [reason message value]
  (err/error :metrics/invalid message
             {:reason reason :value (err/sanitize value)}))

(defn validate-record!
  "Validate a metric record against MetricSchema. Returns the record
  unchanged on success; throws :metrics/invalid (with humanized Malli
  explanations) otherwise."
  [record]
  (if (m/validate MetricSchema record)
    record
    (let [expl (m/explain MetricSchema record)]
      (throw (invalid-error :schema-violation
                            "value is not a valid metric record"
                            {:record record
                             :errors (me/humanize expl)})))))

;; --- recording ------------------------------------------------------------------

(defn record-metric
  "Build a valid metric record.

      (record-metric :task/success :eval \"run-7\" 0.79 :rate)
      ;; => {:metric/id #uuid\"...\" :metric/name :task/success
      ;;     :metric/scope :eval :metric/scope-id \"run-7\"
      ;;     :metric/value 0.79 :metric/unit :rate :metric/at #inst\"...\"}

  Assigns a fresh :metric/id and stamps :metric/at with the current
  instant (the namespace's only IO). A malformed input — wrong type for
  :metric/value, non-keyword :metric/scope, etc. — throws
  :metrics/invalid. Validation is deliberately permissive here so callers
  can rely on every record constructed through this path satisfying the
  closed schema."
  [name scope scope-id value unit]
  (validate-record!
   {:metric/id (java.util.UUID/randomUUID)
    :metric/name name
    :metric/scope scope
    :metric/scope-id scope-id
    :metric/value value
    :metric/unit unit
    :metric/at (java.util.Date.)}))

(defn collect-metric!
  "Validate `record` and conj it onto the collector's vector (swap!).

  `collector` must be an atom holding a vector of metric records. The
  record is validated against the closed schema FIRST, so an invalid
  record never leaves a partial write. On success returns the record.
  Throws :metrics/invalid when the collector is not an atom holding a
  vector, or when the record is not a valid metric record."
  [collector record]
  (when-not (and (instance? clojure.lang.Atom collector)
                 (vector? @collector))
    (throw (invalid-error :collector-invalid
                          "collector must be an atom holding a vector of metric records"
                          collector)))
  (validate-record! record)
  (swap! collector conj record)
  record)

(defn metrics
  "The vector of ALL metric records currently held by `collector`."
  [collector]
  @collector)

(defn metrics-by-name
  "The vector of metric records held by `collector` whose :metric/name
  equals `name`, in insertion order."
  [collector name]
  (filterv #(= (:metric/name %) name) @collector))

;; --- pure descriptive aggregates -------------------------------------------------

(defn- validate-values!
  "The value vector must be sequential; for :mean and :sum every element
  must be numeric (that is what makes those aggregates defined). All
  other ops accept and operate on whatever is present."
  [values]
  (when-not (sequential? values)
    (throw (invalid-error :not-sequential
                          "aggregate values must be a sequential collection"
                          values)))
  values)

(defn- numeric-values!
  "The values must all be numeric (for :mean / :sum). Uses `some` so a
  non-numeric value of nil (falsy) is still detected and reported."
  [values]
  (when (some (complement number?) values)
    (throw (invalid-error :non-numeric-value
                          "mean/sum aggregate requires numeric values"
                          values)))
  values)

(defn aggregate
  "One descriptive aggregate over a vector of values.

      (aggregate [1 3 5] :median)  ;; => 3.0
      (aggregate [] :count)        ;; => 0
      (aggregate [] :mean)         ;; => nil

  `op` ∈ #{:mean :median :min :max :sum :count}. On an EMPTY vector,
  :count → 0 and every other op → nil. :median follows the same
  convention as evoclj.eval.statistics: the middle value for an odd
  count, the mean of the two middle values for an even count. :mean and
  :sum require every value to be numeric and throw :metrics/invalid
  otherwise. An unknown `op` returns nil."
  [values op]
  (validate-values! values)
  (let [n (count values)]
    (case op
      :count (long n)
      :sum (if (zero? n)
             nil
             (reduce + 0.0 (numeric-values! values)))
      :mean (if (zero? n)
              nil
              (double (/ (reduce + 0.0 (numeric-values! values)) n)))
      :median (when (pos? n)
                (let [sorted (sort values)
                      mid (quot n 2)]
                  (if (odd? n)
                    (double (nth sorted mid))
                    (double (/ (+ (nth sorted (dec mid)) (nth sorted mid)) 2.0)))))
      :min (when (pos? n) (first (sort values)))
      :max (when (pos? n) (last (sort values)))
      nil)))

(defn- quantile-at
  "The p-quantile of a SORTED ascending vector, by linear interpolation
  (R type-7 / NumPy default convention):

      position = p * (n - 1)                ; 0-based index, possibly fractional
      lower    = floor(position), upper = ceil(position)
      q        = sorted[lower] + (position - lower) * (sorted[upper] - sorted[lower])

  p = 0.0 clamps to sorted[0] (the min), p = 1.0 clamps to sorted[n-1]
  (the max). Requires n >= 1."
  [sorted p]
  (let [n (count sorted)
        position (* p (dec n))]
    (if (<= n 1)
      (double (first sorted))
      (let [lower (long (Math/floor position))
            upper (long (Math/ceil position))
            lo (nth sorted lower)
            hi (nth sorted upper)]
        (double (+ lo (* (- position lower) (- hi lo))))))))

(defn quantiles
  "The quantile points `[ { :p p :q q } ... ]` of the observed values.

      (quantiles [1 2 3 4 5] [0.5])
      ;; => [{:p 0.5 :q 3.0}]

  `ps` must be a sequential of numbers in [0, 1] (default
  [0.0 0.5 1.0]). Each :q is the linear-interpolation p-quantile of the
  SORTED values using position = p*(n-1) (R type-7 / NumPy default): the
  value is lerped between the floor and ceil neighbors of that 0-based
  position. p = 0 clamps to the min, p = 1 clamps to the max. For an
  EMPTY value vector every :q is nil. Throws :metrics/invalid on a
  non-sequential `ps` or an out-of-range p (outside [0, 1])."
  ([values] (quantiles values [0.0 0.5 1.0]))
  ([values ps]
   (validate-values! values)
   (when-not (sequential? ps)
     (throw (invalid-error :not-sequential
                           "quantile probabilities must be a sequential collection"
                           ps)))
   (doseq [p ps]
     (when-not (and (number? p) (<= 0.0 (double p) 1.0))
       (throw (invalid-error :out-of-range-p
                             "quantile probabilities must be numbers in [0, 1]"
                             p))))
   (let [sorted (vec (sort values))
         n (count sorted)]
     (mapv (fn [p]
             {:p (double p)
              :q (when (pos? n) (quantile-at sorted (double p)))})
           ps))))
