(ns evoclj.metrics.core-test
  "Foundation F2 tests: the unified metric-record vocabulary and its
  pure descriptive aggregates.

  The metric record is ONE timestamped observation of a measured value
  under a named scope, validated against a closed Malli schema. The
  aggregates here are DESCRIPTIVE ONLY — pure deterministic functions of
  the observed values; no inferential claim (confidence, p-value,
  calibration) is computed or implied (that discipline lives in
  evoclj.metrics.inference)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.metrics.core :as metrics]))

(defn- throws-invalid?
  "True when `f` throws an ExceptionInfo whose :error/type is
  :metrics/invalid — the typed error contract for this namespace."
  [f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= :metrics/invalid (:error/type (ex-data e))))))

;; --- record shape and identity ---------------------------------------------------

(deftest record-metric-builds-a-valid-record
  (let [r (metrics/record-metric :task/success :eval "run-7" 0.79 :rate)]
    (testing "the record carries fresh uuid and capture instant"
      (is (uuid? (:metric/id r)))
      (is (inst? (:metric/at r))))
    (testing "the record satisfies the closed schema"
      (is (metrics/validate-record! r)))
    (testing "the supplied fields round-trip exactly"
      (is (= :task/success (:metric/name r)))
      (is (= :eval (:metric/scope r)))
      (is (= "run-7" (:metric/scope-id r)))
      (is (= 0.79 (:metric/value r)))
      (is (= :rate (:metric/unit r))))))

(deftest record-metric-assigns-fresh-uuids
  (is (not= (:metric/id (metrics/record-metric :a :s "1" 1.0 :u))
            (:metric/id (metrics/record-metric :a :s "1" 1.0 :u)))))

(deftest validate-record-rejects-invalid-record
  (testing "a non-map is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metric record"
                          (metrics/validate-record! 42))))
  (testing "a missing required key is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metric record"
                          (metrics/validate-record! {:metric/name :x}))))
  (testing "a non-numeric value is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metric record"
                          (metrics/validate-record!
                           {:metric/id (java.util.UUID/randomUUID)
                            :metric/name :x :metric/scope :s
                            :metric/scope-id "1" :metric/value "oops"
                            :metric/unit :u :metric/at (java.util.Date.)}))))
  (testing "an unknown extra key violates the CLOSED schema"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metric record"
                          (metrics/validate-record!
                           {:metric/id (java.util.UUID/randomUUID)
                            :metric/name :x :metric/scope :s
                            :metric/scope-id "1" :metric/value 1.0
                            :metric/unit :u :metric/at (java.util.Date.)
                            :metric/extra 1})))))

(deftest record-metric-validates-its-arguments
  (testing "a non-keyword scope throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metric record"
                          (metrics/record-metric :x "not-kw" "1" 1.0 :u))))
  (testing "a non-numeric value throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metric record"
                          (metrics/record-metric :x :s "1" "no" :u)))))

;; --- collector add / read / by-name ----------------------------------------------

(deftest collector-add-read-and-filter
  (let [c (atom [])
        r1 (metrics/record-metric :fitness :evolution "gen-3" 12.5 :score)
        r2 (metrics/record-metric :fitness :evolution "gen-3" 14.0 :score)
        r3 (metrics/record-metric :latency-ms :runtime "pod-2" 88 :ms)]
    (testing "collect-metric! returns the record and appends it"
      (is (= r1 (metrics/collect-metric! c r1)))
      (is (= r2 (metrics/collect-metric! c r2)))
      (is (= r3 (metrics/collect-metric! c r3))))
    (testing "metrics reads the full insertion-ordered vector"
      (is (= [r1 r2 r3] (metrics/metrics c))))
    (testing "metrics-by-name filters on :metric/name in insertion order"
      (is (= [r1 r2] (metrics/metrics-by-name c :fitness)))
      (is (= [r3] (metrics/metrics-by-name c :latency-ms)))
      (is (= [] (metrics/metrics-by-name c :missing))))))

(deftest collector-rejects-bad-state-and-bad-record
  (let [c (atom [])]
    (testing "a missing / non-atom collector throws"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"collector"
                            (metrics/collect-metric! nil
                                                     (metrics/record-metric :x :s "1" 1.0 :u))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"collector"
                            (metrics/collect-metric! "not-an-atom"
                                                     (metrics/record-metric :x :s "1" 1.0 :u)))))
    (testing "an atom not holding a vector throws"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"collector"
                            (metrics/collect-metric! (atom 42)
                                                     (metrics/record-metric :x :s "1" 1.0 :u)))))
    (testing "an invalid record is rejected and not written"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"metric record"
                            (metrics/collect-metric! c {:metric/name :x})))
      (is (= [] (metrics/metrics c))))))

;; --- descriptive aggregates ------------------------------------------------------

(defn- approx
  [a b]
  (< (Math/abs (double (- a b))) 1e-9))

(deftest aggregate-all-ops
  (let [values [1 2 3 4 5]]
    (testing "sum"
      (is (approx (metrics/aggregate values :sum) 15.0)))
    (testing "mean"
      (is (approx (metrics/aggregate values :mean) 3.0)))
    (testing "median (odd count = middle value)"
      (is (approx (metrics/aggregate values :median) 3.0)))
    (testing "median (even count = mean of two middles)"
      (is (approx (metrics/aggregate [1 2 3 4] :median) 2.5)))
    (testing "min / max"
      (is (approx (metrics/aggregate values :min) 1.0))
      (is (approx (metrics/aggregate values :max) 5.0)))
    (testing "count"
      (is (= 5 (metrics/aggregate values :count))))))

(deftest aggregate-empty-values
  (testing "count -> 0"
    (is (= 0 (metrics/aggregate [] :count))))
  (testing "every other op -> nil"
    (doseq [op [:mean :median :min :max :sum]]
      (is (nil? (metrics/aggregate [] op))
          (str "op " op " should be nil on empty")))))

(deftest aggregate-rejects-non-numeric-for-mean-and-sum
  (testing "mean over non-numeric values throws"
    (is (throws-invalid? #(metrics/aggregate [1 "a" 3] :mean))))
  (testing "sum over non-numeric values (including a falsey nil) throws"
    (is (throws-invalid? #(metrics/aggregate [1 nil 3] :sum))))
  (testing "non-sequential values throw"
    (is (throws-invalid? #(metrics/aggregate 42 :sum)))))

(deftest aggregate-unknown-op
  (is (nil? (metrics/aggregate [1 2 3] :bogus))))

;; --- quantiles -------------------------------------------------------------------

(deftest quantiles-endpoints-and-midpoint
  (let [qs (metrics/quantiles [1 2 3 4 5] [0.0 0.5 1.0])]
    (testing "p=0 clamps to the min, p=1 to the max"
      (is (approx (:q (first qs)) 1.0))
      (is (approx (:q (last qs)) 5.0)))
    (testing "p=0.5 falls within [min, max]"
      (is (<= 1.0 (:q (second qs)) 5.0)))))

(deftest quantiles-linear-interpolation
  (testing "p=0.25 of [0 4] interpolates to 1.0 (position 0.25*(2-1)=0.25)"
    (is (approx (:q (first (metrics/quantiles [0 4] [0.25]))) 1.0)))
  (testing "p=0.5 of [0 4] = 2.0"
    (is (approx (:q (first (metrics/quantiles [0 4] [0.5]))) 2.0)))
  (testing "result carries :p and :q keys"
    (is (= [0.25 0.5] (mapv :p (metrics/quantiles [0 4] [0.25 0.5]))))))

(deftest quantiles-empty-values-and-default
  (testing "empty values -> every :q is nil"
    (is (every? #(nil? (:q %)) (metrics/quantiles []))))
  (testing "default ps = [0.0 0.5 1.0]"
    (is (= [0.0 0.5 1.0] (mapv :p (metrics/quantiles [1 2 3]))))))

(deftest quantiles-reject-invalid-input
  (testing "non-sequential ps throws"
    (is (throws-invalid? #(metrics/quantiles [1 2 3] 0.5))))
  (testing "p < 0 throws"
    (is (throws-invalid? #(metrics/quantiles [1 2 3] [-0.1]))))
  (testing "p > 1 throws"
    (is (throws-invalid? #(metrics/quantiles [1 2 3] [1.1]))))
  (testing "non-numeric p throws"
    (is (throws-invalid? #(metrics/quantiles [1 2 3] [:half]))))
  (testing "non-sequential values throw"
    (is (throws-invalid? #(metrics/quantiles 42)))))
