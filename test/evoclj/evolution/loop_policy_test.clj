(ns evoclj.evolution.loop-policy-test
  "Tests for `evoclj.evolution.loop-policy/decide-continue?`.

  Each deftest builds the MINIMAL history and config needed to exercise
  one decision branch, then asserts both the `:decision` keyword and a
  stable substring of the `:reason`."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.loop-policy :as lp]))

(defn- gen
  "Construct one generation-summary map."
  [utility parent-utility]
  {:generation/id (str "generation-" utility)
   :utility utility
   :parent/utility parent-utility})

(deftest test-stop-max-gen
  (testing "stops when history length reaches max-generations"
    (let [config {:max-generations 3}
          history [(gen 0.50 0.40)
                   (gen 0.60 0.50)
                   (gen 0.70 0.60)]
          result (lp/decide-continue? history config)]
      (is (= :stop-max-gen (:decision result)))
      (is (re-find #"reached max-generations 3" (:reason result))))))

(deftest test-stop-regression
  (testing "stops when the latest generation scored below its parent"
    (let [config {:max-generations 20
                  :stop-on-regression? true}
          history [(gen 0.80 0.70)
                   (gen 0.60 0.80)]   ;; utility 0.60 < parent/utility 0.80
          result (lp/decide-continue? history config)]
      (is (= :stop-regression (:decision result)))
      (is (re-find #"latest generation regressed vs parent" (:reason result))))))

(deftest test-stop-plateau
  (testing "stops when the trailing window shows no improvement"
    (let [config {:plateau-window 3
                  :min-improvement 0.05}
          ;; window of 3, utilities 0.70/0.71/0.71 → spread 0.01 < 0.05
          ;; latest util 0.71 == parent/utility 0.71, so NO regression
          ;; fires first and the plateau branch is reached.
          history [(gen 0.70 0.60)
                   (gen 0.71 0.70)
                   (gen 0.71 0.71)]
          result (lp/decide-continue? history config)]
      (is (= :stop-plateau (:decision result)))
      (is (re-find #"utility plateau over window 3" (:reason result))))))

(deftest test-continue
  (testing "continues when still improving and within budget"
    (let [config {:max-generations 20
                  :stop-on-regression? true
                  :min-improvement 0.01}
          history [(gen 0.60 0.50)
                   (gen 0.72 0.60)]   ;; improving: 0.72 > 0.60
          result (lp/decide-continue? history config)]
      (is (= :continue (:decision result)))
      (is (re-find #"improving or within budget" (:reason result))))))
