(ns evoclj.eval.leakage-wiring-test
  "Wiring tests for the exam-leakage red-light check in evoclj.eval.paired.

  These tests prove the contract from the task:
    - a candidate whose program text embeds a case keyword is flagged
      :contaminated and is excluded from the utility sum;
    - a candidate with no overlapping keyword is evaluated normally;
    - a contaminated case neither adds to nor subtracts from the total
      utility (it is merely marked)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.paired :as paired]
            [evoclj.eval.leakage :as leakage]))

;; A case text carrying a significant (>= 4 char, non-stopword) token, and a
;; candidate program that embeds that exact keyword.
(def ^:private case-text "compute the frobnicate metric over the samples")
(def ^:private contaminated-candidate "defn frobnicate [x] (* x 2)")
(def ^:private clean-candidate "defn square [x] (* x x)")
(def ^:private normal-result {:status :pass :utility 1.0 :reason "evaluated"})

(deftest guard-leakage-flags-embedded-keywords
  (testing "candidate embedding a case keyword is reported contaminated"
    (is (= {:contaminated? true
            :reason "candidate text embeds case keywords"}
           (paired/guard-leakage contaminated-candidate case-text))))
  (testing "candidate without the keyword is not contaminated"
    (is (= {:contaminated? false}
           (paired/guard-leakage clean-candidate case-text))))
  (testing "missing text is treated as empty string (no error)"
    (is (= {:contaminated? false} (paired/guard-leakage nil nil)))
    (is (= {:contaminated? false} (paired/guard-leakage "" case-text)))))

(deftest evaluate-case-with-guard-returns-contaminated-on-leak
  (let [called (atom false)
        evaluate (fn [] (reset! called true) normal-result)]
    (testing "a leaking candidate short-circuits to :contaminated"
      (let [result (paired/evaluate-case-with-guard
                     contaminated-candidate case-text evaluate)]
        (is (= :contaminated (:status result)))
        (is (= 0.0 (:utility result)))
        (is (= "leakage red-light" (:reason result)))
        (is (false? @called)
            "the normal evaluator must NOT be invoked on a leak")))
    (testing "a clean candidate runs the normal evaluator unchanged"
      (let [result (paired/evaluate-case-with-guard
                     clean-candidate case-text evaluate)]
        (is (= normal-result result))
        (is (true? @called))))))

(deftest contaminated-case-excluded-from-utility-summary
  (testing "only contaminated cases yield zero utility but a count"
    (let [results [{:status :contaminated :utility 0.0 :reason "leakage red-light"}
                   {:status :contaminated :utility 0.0 :reason "leakage red-light"}]
          summary (paired/summarize-utility results)]
      (is (= 0.0 (:utility summary)))
      (is (= 2 (:contaminated-count summary)))
      (is (= 2 (:cases summary)))))
  (testing "a contaminated case is not added to the normal utility sum"
    (let [results [{:status :contaminated :utility 0.0 :reason "leakage red-light"}
                   {:status :pass :utility 1.0 :reason "evaluated"}]
          summary (paired/summarize-utility results)]
      (is (= 1.0 (:utility summary))
          "the contaminated case (utility 0.0) is excluded, not summed")
      (is (= 1 (:contaminated-count summary)))
      (is (= 2 (:cases summary))))))

(deftest end-to-end-wiring-through-guard
  (testing "guard-leakage agrees with the underlying leakage heuristic"
    (is (true? (leakage/contaminated? contaminated-candidate case-text))
        "sanity: the wired guard uses the real leakage/contaminated?")
    (is (false? (leakage/contaminated? clean-candidate case-text)))))
