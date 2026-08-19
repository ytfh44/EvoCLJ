(ns evoclj.evolution.meta-evolution-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.meta-evolution :as meta-evo]
            [evoclj.evolution.meta-schema :as ms]))

(deftest validate-meta-genome-accepts-valid
  (testing "a valid meta-genome passes validation"
    (let [mg {:meta/params []
              :meta/fitness 0.0
              :meta/generation-id "meta-1"}]
      (is (= mg (ms/validate-meta-genome mg))))))

(deftest validate-meta-genome-rejects-invalid
  (testing "an invalid meta-genome throws :evolution/meta-invalid"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"meta-genome does not satisfy schema"
                          (ms/validate-meta-genome {:meta/params "bad"})))))

(deftest create-meta-genome-empty
  (testing "create-meta-genome returns an empty valid meta-genome"
    (let [mg (meta-evo/create-meta-genome)]
      (is (vector? (:meta/params mg)))
      (is (string? (:meta/generation-id mg))))))

(deftest create-meta-genome-from-config
  (testing "extracts prompts from system config"
    (let [config {:evolution/system {:diagnostician {:system-prompt "d1"}
                                     :mutator {:system-prompt "m1"}}}
          mg (meta-evo/create-meta-genome config)]
      (is (= 2 (count (:meta/params mg))))
      (is (= :diagnostician (-> mg :meta/params first :prompt/type))))))

(deftest prompt-mutation-changes-text
  (testing "prompt-mutator returns a new meta-genome"
    (let [m (meta-evo/prompt-mutator)
          mg {:meta/params [{:prompt/type :mutator :prompt/text "improve"}]}]
      (is (not= mg (meta-evo/mutate-meta m mg))))))

(deftest weight-mutation-clamps
  (testing "weight-mutator keeps value within bounds"
    (let [m (meta-evo/weight-mutator)
          mg {:meta/params [{:weight/name :w1 :weight/value 0.5 :weight/min 0.0 :weight/max 1.0}]}]
      (let [result (meta-evo/mutate-meta m mg)]
        (is (<= 0.0 (:weight/value (first (:meta/params result)))))
        (is (>= 1.0 (:weight/value (first (:meta/params result)))))))))

(deftest policy-mutation-flips-boolean
  (testing "policy-mutator flips boolean values"
    (let [m (meta-evo/policy-mutator)
          mg {:meta/params [{:policy/name :p1 :policy/key :k1 :policy/value true}]}
          result (meta-evo/mutate-meta m mg)]
      (is (= false (-> result :meta/params first :policy/value))))))

(deftest evaluate-meta-genome-stub
  (testing "evaluate-meta-genome returns the meta-genome with fitness"
    (let [mg {:meta/params [] :meta/fitness 0.42}]
      (is (= 0.42 (:meta/fitness (meta-evo/evaluate-meta-genome mg)))))))

(deftest evaluate-meta-genome-override
  (testing "evaluate-meta-genome can override fitness"
    (let [mg {:meta/params []}]
      (is (= 0.99 (:meta/fitness (meta-evo/evaluate-meta-genome mg {:fitness 0.99})))))))
