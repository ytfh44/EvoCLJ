(ns evoclj.metrics.inference
  "Sample-based inferential summaries over observed metric values
  (Foundation F2).

  EPISTEMIC LIMITS — READ BEFORE USE. Everything here is a sample-based
  ESTIMATE over the observed values with stated resampling or regression
  mechanics. It is NOT a calibrated probability claim about an unknown
  population:

  - bootstrap-ci resamples the OBSERVED sample with replacement. Its
    interval is a measure of the sampling variability of the observed
    MEAN under the observed EMPIRICAL distribution — not a confidence
    interval about a true population mean under any parametric model,
    and not a calibrated posterior.
  - trend-test fits a least-squares line to the observed series indexed
    by position. Its slope/intercept are deterministic fits to the
    observed data; :trend/up? is purely `slope > 0`. No autocorrelation,
    independence, or error-distribution assumption is asserted or
    verified.

  This mirrors the epistemic discipline of evoclj.eval.statistics: the
  RAW VALUES remain the durable record, and every summary above is a
  pure deterministic function of those observed values (given the stated
  RNG mechanics). Resampling with a fixed seed is deterministic and
  reproducible; changing the seed changes the resampling estimate. These
  numbers support descriptive reporting and variance-aware comparison of
  the observed sample — they never authorize a calibrated claim about a
  hidden population.

  Error contract (Global Constraint 22 — plain serializable data):
  :metrics/inference-invalid."
  (:require [evoclj.kernel.error :as err]))

;; --- shared input contract --------------------------------------------------------

(defn- inference-error
  [reason message value]
  (err/error :metrics/inference-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-values!
  "A sequential collection of numeric values. Used by both bootstrap-ci
  and trend-test."
  [values]
  (when-not (sequential? values)
    (throw (inference-error :not-sequential
                            "values must be a sequential collection of numbers"
                            values)))
  (when-some [bad (first (remove number? values))]
    (throw (inference-error :non-numeric-value
                            "values must all be numeric"
                            bad)))
  values)

;; --- bootstrap confidence interval -------------------------------------------------

(defn- validate-p!
  "p must be a number strictly inside (0, 1) — exclusive, so 0 and 1 are
  rejected (a degenerate interval level is a call error, not a valid
  request)."
  [p]
  (when-not (and (number? p)
                 (< 0.0 (double p) 1.0))
    (throw (inference-error :out-of-range-p
                            "bootstrap confidence level p must be a number in (0, 1) exclusive"
                            p)))
  p)

(defn- ci-impl
  "The bootstrap confidence interval of the sample mean with an explicit
  RNG. Samples WITH replacement, n draws per replication, 1000
  replications; :ci/lo and :ci/hi are the (1-p)/2 and 1-(1-p)/2
  quantiles of the replication means. A single-element series trivially
  yields lo = hi = the value while still returning the full map.
  `p` must be strictly inside (0, 1)."
  [values p ^java.util.Random rng]
  (let [values (vec values)
        n (count values)
        replications 1000
        lo-p (/ (- 1.0 p) 2.0)
        hi-p (- 1.0 lo-p)]
    (if (<= n 1)
      (let [v (double (first values))]
        {:ci/lo v :ci/hi v :ci/p (double p)
         :ci/n (long n) :ci/replications (long replications)})
      (let [means (into []
                        (map (fn [_]
                               (loop [i 0 sum 0.0]
                                 (if (< i n)
                                   (recur (inc i) (+ sum (double (nth values (.nextInt rng n)))))
                                   (/ sum n)))))
                        (range replications))
            sorted (vec (sort means))
            q (fn [prob] (let [;; linear interpolation over the sorted replication means
                               ;; (R type-7 / NumPy default): position = prob * (m-1)
                               m (count sorted)
                               pos (* prob (dec m))
                               lo (long (Math/floor pos))
                               hi (long (Math/ceil pos))]
                           (if (<= m 1)
                             (double (first sorted))
                             (double (+ (nth sorted lo)
                                        (* (- pos lo) (- (nth sorted hi) (nth sorted lo))))))))]
        {:ci/lo (q lo-p)
         :ci/hi (q hi-p)
         :ci/p (double p)
         :ci/n (long n)
         :ci/replications (long replications)}))))

(defn bootstrap-ci*
  "The bootstrap confidence interval of the sample mean with an explicit
  RNG.

      (bootstrap-ci* values 0.95 (java.util.Random. 42))
      ;; => {:ci/lo ... :ci/hi ... :ci/p 0.95 :ci/n ... :ci/replications 1000}

  Mechanics: sample WITH replacement, n draws per replication, 1000
  replications by default; :ci/lo and :ci/hi are the (1-p)/2 and
  1-(1-p)/2 linear-interpolation quantiles of the replication means.
  The interval is a property of the observed sample's empirical
  distribution, NOT a calibrated population-parameter claim (see the
  namespace epistemic limits). Throws :metrics/inference-invalid for an
  empty value vector, p outside (0,1) exclusive, or non-sequential /
  non-numeric values."
  [values p rng]
  (validate-values! values)
  (when-not (seq values)
    (throw (inference-error :empty-values
                            "bootstrap-ci requires at least one value"
                            values)))
  (validate-p! p)
  (ci-impl values p rng))

(defn bootstrap-ci
  "The bootstrap confidence interval of the sample mean. Same mechanics
  as bootstrap-ci* but uses the deterministic SEEDED default RNG
  (java.util.Random. 42), so two calls over the same values agree
  exactly (reproducibility)."
  ([values] (bootstrap-ci values 0.95))
  ([values p]
   (bootstrap-ci* values p (java.util.Random. 42))))

;; --- least-squares trend -------------------------------------------------------------

(defn trend-test
  "The ordinary least-squares linear trend over the observed series.

      (trend-test [1 2 3 4 5])
      ;; => {:trend/slope 1.0 :trend/intercept 1.0 :trend/up? true :trend/streak 1}

  Indexes the series by position x = 0..n-1 and fits slope b and
  intercept a over the points (x, values[x]) by least squares:

      xbar = mean(x),  ybar = mean(values)
      b    = Σ (x - xbar)(values[x] - ybar) / Σ (x - xbar)^2
      a    = ybar - b * xbar

  :trend/up? = slope > 0. :trend/streak is the length of the TRAILING
  run of successive values moving in the slope direction (>= when
  slope >= 0, <= when slope < 0; ties count) — the trailing plateau of
  consecutive EQUAL values, where a tie (delta 0) is the only step that
  extends the run backward. For an upward series that plateaus at the
  top, [1 2 3 3 3], the trailing run is the plateau [3 3 3] of length 3.
  An empty or single-element series degenerates to
  {:trend/slope 0.0 :trend/intercept 0.0 :trend/up? false
  :trend/streak 0}. Throws :metrics/inference-invalid on non-sequential
  or non-numeric input."
  [values]
  (validate-values! values)
  (let [values (vec values)
        n (count values)]
    (if (< n 2)
      {:trend/slope 0.0 :trend/intercept 0.0 :trend/up? false :trend/streak 0}
      (let [x (vec (range n))
            xbar (/ (* n (dec n)) 2.0 n)
            ybar (/ (reduce + 0.0 values) n)
            sxx (reduce + 0.0 (map #(let [d (- % xbar)] (* d d)) x))
            sxy (reduce + 0.0 (map (fn [xi yi] (* (- xi xbar) (- (double yi) ybar)))
                                   x values))
            b (if (zero? sxx) 0.0 (/ sxy sxx))
            a (- ybar (* b xbar))
            up? (pos? b)
            ;; Trailing run: walk backward from the last value while each
            ;; successive value HOLDS within the slope direction. A tie
            ;; (delta 0) is the only delta that counts toward the run — the
            ;; trailing plateau of consecutive EQUAL values. A strict step
            ;; toward the trend (the last actual change) terminates the run.
            streak (let [last-val (double (nth values (dec n)))]
                     (loop [i (dec n) len 1]
                       (if (and (pos? i)
                                (== (double (nth values (dec i))) last-val))
                         (recur (dec i) (inc len))
                         len)))]
        {:trend/slope (double b)
         :trend/intercept (double a)
         :trend/up? up?
         :trend/streak (long streak)}))))
