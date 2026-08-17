(ns evoclj.eval.judge-drift-test
  (:require [clojure.test :refer :all]
            [evoclj.eval.judge-drift :refer [drift-score baseline]]))

(defn approx=
  "Assert that `actual` and `expected` scalars are within `tol` (default 1e-9)
  of each other. Returns true on success, false otherwise."
  ([actual expected]
   (approx= actual expected 1e-9))
  ([actual expected tol]
   (< (Math/abs (- (double actual) (double expected))) tol)))

(defn map-approx=
  "Assert two number-valued maps are element-wise within `tol` (default 1e-9)
  of each other, treating absent keys as 0.0."
  ([actual expected]
   (map-approx= actual expected 1e-9))
  ([actual expected tol]
   (let [ks (distinct (concat (keys actual) (keys expected)))]
     (every? (fn [k]
               (< (Math/abs (- (double (get actual k 0.0))
                               (double (get expected k 0.0))))
                  tol))
             ks))))

(deftest test-empty-history-drift-zero
  (testing "Empty history => baseline falls back to current => drift is 0."
    (is (approx= (drift-score [] {:equivalent 0.8 :not-equivalent 0.2}) 0.0))))

(deftest test-identical-to-baseline-drift-near-zero
  (testing "Current distribution matches the historical baseline => drift ~ 0."
    (let [hist [{:equivalent 0.6 :not-equivalent 0.4}
                {:equivalent 0.7 :not-equivalent 0.3}
                {:equivalent 0.8 :not-equivalent 0.2}]]
      ;; baseline mean = {:equivalent 0.7 :not-equivalent 0.3}
      (is (map-approx= (baseline hist) {:equivalent 0.7 :not-equivalent 0.3}))
      (is (approx= (drift-score hist {:equivalent 0.7 :not-equivalent 0.3}) 0.0)))))

(deftest test-clear-drift-correct-value
  (testing "A clear shift yields drift > 0 with the exact expected L1 distance."
    (let [hist [{:equivalent 0.8 :not-equivalent 0.2}]]
      ;; baseline mean = {:equivalent 0.8 :not-equivalent 0.2}
      ;; current       = {:equivalent 0.3 :not-equivalent 0.7}
      ;; L1 = |0.8-0.3| + |0.2-0.7| = 0.5 + 0.5 = 1.0
      (is (approx= (drift-score hist {:equivalent 0.3 :not-equivalent 0.7}) 1.0))
      (is (> (drift-score hist {:equivalent 0.3 :not-equivalent 0.7}) 0.0)))))

(deftest test-window-size-bounds-history
  (testing "Only the trailing N=5 histories contribute to the baseline."
    (let [hist (vec (repeat 7 {:equivalent 1.0 :not-equivalent 0.0}))]
      ;; all identical => baseline identical => drift 0 regardless of windowing
      (is (approx= (drift-score hist {:equivalent 1.0 :not-equivalent 0.0}) 0.0))
      ;; baseline over trailing 5 identical distributions
      (is (map-approx= (baseline hist) {:equivalent 1.0 :not-equivalent 0.0})))))

(deftest test-partial-key-overlap
  (testing "Keys absent in one side are treated as 0 when computing L1."
    (let [hist [{:equivalent 1.0}]
          ;; baseline = {:equivalent 1.0}; current = {:not-equivalent 1.0}
          ;; L1 = |1.0-0| + |0-1.0| = 2.0
          cur {:not-equivalent 1.0}]
      (is (map-approx= (baseline hist) {:equivalent 1.0}))
      (is (approx= (drift-score hist cur) 2.0)))))
