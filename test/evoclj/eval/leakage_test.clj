(ns evoclj.eval.leakage-test
  "Tests for evoclj.eval.leakage (coarse contamination heuristic)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.leakage :as leakage]))

(deftest contaminated-leaks-case-keyword
  (testing "candidate containing a significant case token is contaminated"
    (let [case-text "compute the frobnicate metric over the samples"
          candidate "defn frobnicate [x] (* x 2)"]
      (is (true? (leakage/contaminated? candidate case-text))))))

(deftest not-contaminated-when-unrelated
  (testing "candidate with no overlapping significant token is clean"
    (let [case-text "compute the frobnicate metric"
          candidate "defn square [x] (* x x)"]
      (is (false? (leakage/contaminated? candidate case-text))))))

(deftest stopwords-do-not-trigger
  (testing "a case made only of stopwords yields no significant token"
    (let [case-text "the and for with this that return def let when"
          candidate "return the result for this and that"]
      (is (false? (leakage/contaminated? candidate case-text))))))

(deftest extract-tokens-returns-significant-set
  (testing "extract-tokens drops short words and stopwords"
    (is (= #{"over" "frobnicate" "metric" "compute" "samples"}
           (leakage/extract-tokens "compute the Frobnicate metric over samples")))))
