(ns evoclj.evolution.pareto-test
  "Tests for `evoclj.evolution.pareto` (S3-1).

  The Pareto archive tracks non-dominated candidates across
  generations. Dominance: A dominates B when A >= B in ALL objectives
  and > B in AT LEAST ONE. Lower cost/complexity is better; higher
  success is better."

  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.pareto :as pareto]))

;; --- helpers ------------------------------------------------------------------

(defn- scores
  [task-success cost complexity]
  {:task/success task-success
   :cost cost
   :complexity complexity})

;; --- create-pareto-archive ----------------------------------------------------

(deftest test-create-pareto-archive
  (testing "returns an empty vector"
    (is (empty? (pareto/create-pareto-archive)))))

;; --- add-candidate! / frontier -------------------------------------------------

(deftest test-add-candidate-empty-archive
  (testing "adding to an empty archive returns a singleton frontier"
    (let [archive (pareto/add-candidate! (pareto/create-pareto-archive)
                                         (scores 0.8 10.0 5.0))]
      (is (= 1 (count (pareto/frontier archive))))
      (is (= (scores 0.8 10.0 5.0) (first (pareto/frontier archive)))))))

(deftest test-add-candidate-dominated-discard
  (testing "a dominated candidate is discarded"
    (let [archive (-> (pareto/create-pareto-archive)
                      (pareto/add-candidate! (scores 0.8 10.0 5.0))
                      (pareto/add-candidate! (scores 0.7 20.0 8.0)))]
      ;; second is worse in success, higher cost, higher complexity
      (is (= 1 (count (pareto/frontier archive))))
      (is (= (scores 0.8 10.0 5.0) (first (pareto/frontier archive)))))))

(deftest test-add-candidate-dominates-removes-existing
  (testing "a new candidate that dominates an existing one removes the old"
    (let [archive (-> (pareto/create-pareto-archive)
                      (pareto/add-candidate! (scores 0.7 20.0 8.0))
                      (pareto/add-candidate! (scores 0.9 5.0 2.0)))]
      (is (= 1 (count (pareto/frontier archive))))
      (is (= (scores 0.9 5.0 2.0) (first (pareto/frontier archive)))))))

(deftest test-add-candidate-non-dominated-keeps-both
  (testing "non-dominated candidates are both kept"
    (let [archive (-> (pareto/create-pareto-archive)
                      (pareto/add-candidate! (scores 0.8 10.0 5.0))
                      (pareto/add-candidate! (scores 0.7 5.0 8.0)))]
      (is (= 2 (count (pareto/frontier archive))))
      (is (some #(= (scores 0.8 10.0 5.0) %) (pareto/frontier archive)))
      (is (some #(= (scores 0.7 5.0 8.0) %) (pareto/frontier archive))))))

(deftest test-frontier-returns-copy
  (testing "frontier returns a vector snapshot"
    (let [archive (pareto/add-candidate! (pareto/create-pareto-archive)
                                         (scores 0.5 0.0 0.0))
          f (pareto/frontier archive)]
      (is (vector? f))
      (is (= 1 (count f))))))

(deftest test-add-candidate-atom
  (testing "add-candidate! on an atom swaps! and returns the atom"
    (let [archive (atom (pareto/create-pareto-archive))
          scores-a (scores 0.8 10.0 5.0)
          scores-b (scores 0.7 20.0 8.0)]
      (is (identical? archive (pareto/add-candidate! archive scores-a)))
      (is (= 1 (count (pareto/frontier @archive))))
      (is (identical? archive (pareto/add-candidate! archive scores-b)))
      (is (= 1 (count (pareto/frontier @archive)))))))
