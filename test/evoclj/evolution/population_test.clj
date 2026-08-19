(ns evoclj.evolution.population-test
  "Tests for `evoclj.evolution.population` (S3-1).

  The Population is a plain map; these tests verify creation, addition,
  selection, and size without any store or IO dependency."

  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.population :as population]))

;; --- shared fixtures ----------------------------------------------------------

(defn- candidate
  "Build a minimal candidate record map for testing."
  [id]
  {:candidate/id id
   :parent/generation-id "generation-1"
   :candidate/genome-id (str "sha256:" (apply str (repeat 64 "a")))})

;; --- create-population --------------------------------------------------------

(deftest test-create-population
  (testing "create-population returns a map with the right keys"
    (let [pop (population/create-population "generation-1")]
      (is (uuid? (:id pop)))
      (is (= "generation-1" (:generation/id pop)))
      (is (vector? (:candidates pop)))
      (is (= 0 (population/size pop)))
      (is (map? (:evaluations pop))))))

;; --- add-candidate! -----------------------------------------------------------

(deftest test-add-candidate!
  (testing "add-candidate! appends a candidate and returns the updated population"
    (let [pop (population/create-population "generation-1")
          c1 (candidate (java.util.UUID/randomUUID))
          c2 (candidate (java.util.UUID/randomUUID))
          pop' (population/add-candidate! pop c1)]
      (is (= 1 (population/size pop')))
      (is (= [c1] (:candidates pop')))
      (testing "a second add appends"
        (let [pop'' (population/add-candidate! pop' c2)]
          (is (= 2 (population/size pop'')))
          (is (= [c1 c2] (:candidates pop''))))))))

(deftest test-add-candidate-with-eval-summary
  (testing "add-candidate! with eval-summary stores the summary in :evaluations"
    (let [pop (population/create-population "generation-1")
          c (candidate (java.util.UUID/randomUUID))
          summary {:summary {:utility {:task/success {:candidate 0.8}}}}
          pop' (population/add-candidate! pop c summary)]
      (is (= 1 (population/size pop')))
      (is (= {(:candidate/id c) summary} (:evaluations pop'))))))

(deftest test-add-candidate!-is-pure-when-not-atom
  (testing "add-candidate! does not mutate the original population map"
    (let [pop (population/create-population "generation-1")
          c (candidate (java.util.UUID/randomUUID))
          pop' (population/add-candidate! pop c)]
      (is (= 0 (population/size pop)))
      (is (= 1 (population/size pop'))))))

(deftest test-add-candidate!-atom-side-effect
  (testing "add-candidate! on an atom swaps! and returns the atom"
    (let [pop (atom (population/create-population "generation-1"))
          c (candidate (java.util.UUID/randomUUID))]
      (is (identical? pop (population/add-candidate! pop c)))
      (is (= 1 (population/size @pop))))))

;; --- size ---------------------------------------------------------------------

(deftest test-size
  (testing "size returns the number of candidates"
    (let [pop (reduce (fn [p _] (population/add-candidate! p (candidate (java.util.UUID/randomUUID))))
                      (population/create-population "generation-1")
                      (range 5))]
      (is (= 5 (population/size pop))))))

;; --- candidates-for-breeding --------------------------------------------------

(deftest test-candidates-for-breeding-empty
  (testing "returns [] when population is empty"
    (let [pop (population/create-population "generation-1")]
      (is (empty? (population/candidates-for-breeding pop))))))

(deftest test-candidates-for-breeding-default
  (testing "returns the current population size by default"
    (let [pop (reduce (fn [p _] (population/add-candidate! p (candidate (java.util.UUID/randomUUID))))
                      (population/create-population "generation-1")
                      (range 4))]
      (is (= 4 (count (population/candidates-for-breeding pop)))))))

(deftest test-candidates-for-breeding-count
  (testing "returns exactly :count candidates when supplied"
    (let [pop (reduce (fn [p _] (population/add-candidate! p (candidate (java.util.UUID/randomUUID))))
                      (population/create-population "generation-1")
                      (range 5))]
      (is (= 2 (count (population/candidates-for-breeding pop {:count 2})))))))

(deftest test-candidates-for-breeding-tournament-size
  (testing "does not fail when tournament-size is supplied"
    (let [pop (reduce (fn [p _] (population/add-candidate! p (candidate (java.util.UUID/randomUUID))))
                      (population/create-population "generation-1")
                      (range 3))]
      (is (vector? (population/candidates-for-breeding pop {:tournament-size 2}))))))

(deftest test-candidates-for-breeding-prefers-evaluated
  (testing "candidates with eval summaries are preferred over unevaluated ones"
    (let [pop (-> (population/create-population "generation-1")
                  (population/add-candidate! (candidate (java.util.UUID/randomUUID))
                                             {:summary {:utility {:task/success {:candidate 0.9}}}})
                  (population/add-candidate! (candidate (java.util.UUID/randomUUID)))
                  (population/add-candidate! (candidate (java.util.UUID/randomUUID))))]
      ;; With default tournament size 3, the evaluated candidate should
      ;; always win because it has a better fitness proxy.
      (let [winners (population/candidates-for-breeding pop {:count 10})
            evaluated-ids (into #{} (map :candidate/id)
                                (filter #(get-in pop [:evaluations (:candidate/id %)])
                                        (:candidates pop)))]
        (is (every? #(contains? evaluated-ids (:candidate/id %)) winners))))))
