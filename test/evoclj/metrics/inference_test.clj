(ns evoclj.metrics.inference-test
  "Foundation F2 tests: sample-based inferential summaries (bootstrap CI
  and least-squares trend).

  These are estimates over the OBSERVED values with stated resampling /
  regression mechanics — NOT calibrated probability claims about an
  unknown population. The bootstrap runs are made deterministic by the
  fixed seed, so the specific numbers asserted here are stable; changing
  the seed changes the resampling estimate."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.metrics.inference :as inference]))

(defn- throws-inference-invalid?
  "True when `f` throws an ExceptionInfo whose :error/type is
  :metrics/inference-invalid — the typed error contract for this
  namespace."
  [f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= :metrics/inference-invalid (:error/type (ex-data e))))))

(defn- approx
  [a b]
  (< (Math/abs (double (- a b))) 1e-9))

;; --- bootstrap confidence interval ------------------------------------------------

(deftest bootstrap-ci-is-an-interval-inside-observed-range
  (let [values [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]
        ci (inference/bootstrap-ci values)]
    (testing "lo <= hi"
      (is (<= (:ci/lo ci) (:ci/hi ci))))
    (testing "the interval lies within the observed [min, max]"
      (is (<= (apply min values) (:ci/lo ci) (:ci/hi ci) (apply max values))))
    (testing "the confidence level and the sample size are recorded"
      (is (approx (:ci/p ci) 0.95))
      (is (= 10 (:ci/n ci))))
    (testing "1000 replications ran"
      (is (= 1000 (:ci/replications ci))))))

(deftest bootstrap-ci-constant-series-is-degenerate
  (let [ci (inference/bootstrap-ci [5 5 5 5 5])]
    (testing "a constant series collapses to the constant value"
      (is (approx (:ci/lo ci) 5.0))
      (is (approx (:ci/hi ci) 5.0)))))

(deftest bootstrap-ci-is-deterministic-under-seeded-default
  (let [values [0.1 0.2 0.3 0.4 0.5]
        a (inference/bootstrap-ci values)
        b (inference/bootstrap-ci values)]
    (testing "two calls with the seeded default agree exactly"
      (is (= a b)))))

(deftest bootstrap-ci-different-seed-gives-different-result
  (let [values [0.1 0.2 0.3 0.4 0.5 0.6 0.7]
        a (inference/bootstrap-ci* values 0.95 (java.util.Random. 42))
        b (inference/bootstrap-ci* values 0.95 (java.util.Random. 7))]
    (testing "different seeds give (almost surely) different intervals"
      (is (not= a b))))
  (testing "the same explicit seed reproduces the same interval"
    (is (= (inference/bootstrap-ci* [0.1 0.2 0.3] 0.95 (java.util.Random. 11))
           (inference/bootstrap-ci* [0.1 0.2 0.3] 0.95 (java.util.Random. 11))))))

(deftest bootstrap-ci-higher-p-is-wider
  (let [values [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]]
    (testing "p=0.99 spans a wider interval than p=0.5"
      (is (<= (- (:ci/hi (inference/bootstrap-ci values 0.5))
                 (:ci/lo (inference/bootstrap-ci values 0.5)))
              (- (:ci/hi (inference/bootstrap-ci values 0.99))
                 (:ci/lo (inference/bootstrap-ci values 0.99))))))
    (testing "the p=0.5 interval is nested inside the p=0.99 interval"
      (is (<= (:ci/lo (inference/bootstrap-ci values 0.99))
              (:ci/lo (inference/bootstrap-ci values 0.5))))
      (is (>= (:ci/hi (inference/bootstrap-ci values 0.99))
              (:ci/hi (inference/bootstrap-ci values 0.5)))))))

(deftest bootstrap-ci-single-value-series
  (let [ci (inference/bootstrap-ci [7.0])]
    (testing "a single value gives lo = hi = the value still as a full map"
      (is (approx (:ci/lo ci) 7.0))
      (is (approx (:ci/hi ci) 7.0))
      (is (= [7.0 7.0] [(:ci/lo ci) (:ci/hi ci)]))
      (is (= 1 (:ci/n ci)))
      (is (= 1000 (:ci/replications ci)))
      (is (approx (:ci/p ci) 0.95)))))

(deftest bootstrap-ci-is-within-observed-range-in-general
  ;; The bootstrap resamples only the observed values, so every
  ;; replication mean — and hence every quantile of them — lies within
  ;; the observed [min, max].
  (let [values [0.2 0.4 0.6 0.8 1.0 1.2]
        ci (inference/bootstrap-ci values)]
    (is (<= (apply min values) (:ci/lo ci) (:ci/hi ci) (apply max values)))))

(deftest bootstrap-ci-validates-input
  (testing "an empty value vector throws"
    (is (throws-inference-invalid? #(inference/bootstrap-ci []))))
  (testing "p <= 0 throws"
    (is (throws-inference-invalid? #(inference/bootstrap-ci [1 2 3] 0.0))))
  (testing "p >= 1 throws"
    (is (throws-inference-invalid? #(inference/bootstrap-ci [1 2 3] 1.0))))
  (testing "non-sequential values throw"
    (is (throws-inference-invalid? #(inference/bootstrap-ci 42))))
  (testing "non-numeric values throw"
    (is (throws-inference-invalid? #(inference/bootstrap-ci [1 "x" 3])))))

;; --- least-squares trend ----------------------------------------------------------

(deftest trend-test-increasing-series
  (let [t (inference/trend-test [1 2 3 4 5])]
    (testing "positive slope and up? true"
      (is (pos? (:trend/slope t)))
      (is (true? (:trend/up? t))))
    (testing "exact slope and intercept for an exact line"
      (is (approx (:trend/slope t) 1.0))
      (is (approx (:trend/intercept t) 1.0)))))

(deftest trend-test-decreasing-series
  (let [t (inference/trend-test [5 4 3 2 1])]
    (testing "negative slope and up? false"
      (is (neg? (:trend/slope t)))
      (is (false? (:trend/up? t))))
    (testing "symmetric exact fit"
      (is (approx (:trend/slope t) -1.0))
      (is (approx (:trend/intercept t) 5.0)))))

(deftest trend-test-flat-series
  (let [t (inference/trend-test [3 3 3 3 3])]
    (testing "slope is ~0 and up? false"
      (is (< (Math/abs (:trend/slope t)) 1e-9))
      (is (false? (:trend/up? t))))))

(deftest trend-test-plateau-streak
  (let [t (inference/trend-test [1 2 3 3 3])]
    (testing "the trend is upward"
      (is (true? (:trend/up? t))))
    (testing "the trailing plateau [3 3 3] is the trailing streak of length 3"
      (is (= 3 (:trend/streak t))))))

(deftest trend-test-degenerate-series
  (testing "an empty series is the degenerate zero trend"
    (is (= {:trend/slope 0.0 :trend/intercept 0.0 :trend/up? false :trend/streak 0}
           (inference/trend-test []))))
  (testing "a single-element series is the degenerate zero trend"
    (is (= {:trend/slope 0.0 :trend/intercept 0.0 :trend/up? false :trend/streak 0}
           (inference/trend-test [5])))))

(deftest trend-test-validates-input
  (testing "non-sequential input throws"
    (is (throws-inference-invalid? #(inference/trend-test 42))))
  (testing "non-numeric values throw"
    (is (throws-inference-invalid? #(inference/trend-test [1 2 "x"])))))
