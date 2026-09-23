(ns evoclj.evolution.dag-test
  (:require [clojure.test :refer [deftest is]]
            [evoclj.evolution.dag :as dag]))

(deftest frontier-is-bounded-and-stable
  (let [xs [{:id "b" :score 3} {:id "a" :score 3} {:id "c" :score 1}]
        out (dag/bounded-frontier xs {:k 2 :eval-budget 3 :score-fn :score})]
    (is (= ["a" "b"] (mapv :id (:selected out))))
    (is (= ["c"] (mapv :id (:unselected out))))
    (is (= 3 (:evaluated out)))))

(deftest frontier-tie-breaks-by-digest-then-id
  (let [out (dag/bounded-frontier
             [{:id "z" :digest "digest-b" :score 1}
              {:id "a" :digest "digest-a" :score 1}
              {:id "a-2" :digest "digest-a" :score 1}]
             {:k 3 :eval-budget 3 :score-fn :score})]
    (is (= ["a" "a-2" "z"] (mapv :id (:selected out))))))
