(ns evoclj.eval.cost-guard-test
  "Pure cost hard-stop guard tests (Task E2).

   The guard is a PURE function: no IO, no state. Each test asserts a
   single strict-comparison boundary or the invalid-input contract. The
   error type is the boundary-crossing contract from
   evoclj.kernel.error (Global Constraint 22): a stable
   :error/type keyword carried in ex-data."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.cost-guard :as cg]
            [evoclj.kernel.error :as err]))

(deftest cumulative-cost-below-threshold-continues
  (testing "cost 0.5 with threshold 1.0 stays under budget"
    (is (= :continue (cg/should-stop? 0.5 1.0)))))

(deftest cumulative-cost-equal-to-threshold-continues
  (testing "equality is NOT a stop — the comparison is strictly greater-than"
    (is (= :continue (cg/should-stop? 1.0 1.0)))))

(deftest cumulative-cost-above-threshold-stops
  (testing "cost 1.5 with threshold 1.0 exceeds budget and stops"
    (is (= :stop (cg/should-stop? 1.5 1.0)))))

(deftest zero-cost-continues
  (testing "zero cost against a zero threshold is not strictly greater"
    (is (= :continue (cg/should-stop? 0.0 0.0)))))

(deftest negative-cost-below-zero-threshold-continues
  (testing "a negative cost stays below a zero threshold"
    (is (= :continue (cg/should-stop? -1.0 0.0)))))

(defn- capture-ex-info
  "Call `thunk` and return its ex-data if it throws
  clojure.lang.ExceptionInfo, else return nil."
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(deftest non-number-cumulative-cost-throws
  (testing "a non-numeric cumulative cost is rejected with a typed error"
    (let [data (capture-ex-info #(cg/should-stop? "x" 1.0))]
      (is (= :eval/cost-guard-invalid (:error/type data))))))

(deftest non-number-threshold-throws
  (testing "a non-numeric threshold is rejected with a typed error"
    (let [data (capture-ex-info #(cg/should-stop? 1.0 "x"))]
      (is (= :eval/cost-guard-invalid (:error/type data))))))
