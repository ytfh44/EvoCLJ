(ns evoclj.eval.harness-registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.harness-registry :as reg]))

(def ^:private fixture-set
  {:set/id :math-basics
   :set/source :fixture
   :set/version "1.0.0"
   :set/path "test/fixtures/harness/math_basics.edn"
   :set/generation-scoped? false})

(def ^:private gen-set
  {:set/id :gen-algebra
   :set/source :generator
   :set/version "0.3.1"
   :set/path "gen://algebra/42"
   :set/generation-scoped? true})

(deftest register-then-list-contains
  (testing "after register-set the record is present in list-sets"
    (let [registry (reg/register-set [] fixture-set)
          listed (reg/list-sets registry)]
      (is (= 1 (count listed)))
      (is (= fixture-set (first listed))))))

(deftest get-set-hit-and-miss
  (testing "get-set finds an existing id and returns nil for a missing id"
    (let [registry (reg/register-set [] fixture-set)]
      (is (= fixture-set (reg/get-set registry :math-basics)))
      (is (nil? (reg/get-set registry :does-not-exist))))))

(deftest active-sets-filters-generation-scoped
  (testing "active-sets keeps only :set/generation-scoped? false records"
    (let [registry (-> []
                       (reg/register-set fixture-set)
                       (reg/register-set gen-set))
          active (reg/active-sets registry)]
      (is (= 2 (count registry)))
      (is (= 1 (count active)))
      (is (= fixture-set (first active)))
      (is (not-any? :set/generation-scoped? active)))))

(deftest register-duplicate-id
  (testing "registering the same :set/id twice appends both records"
    (let [v2 (assoc fixture-set :set/version "1.0.1")
          registry (-> []
                       (reg/register-set fixture-set)
                       (reg/register-set v2))]
      (is (= 2 (count registry)))
      ;; get-set returns the most recently registered (last) record
      (is (= "1.0.1" (:set/version (reg/get-set registry :math-basics)))))))
