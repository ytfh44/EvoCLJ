(ns evoclj.evolution.invariant-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.invariant :as invariant]))

(def ^:private ref-a "evidence-a")
(def ^:private ref-b "replay-b")
(def ^:private ref-c "adversarial-c")

(def ^:private proposal
  {:proposal/id "proposal-1"
   :proposer "proposer-1"
   :scope :runtime
   :risk :medium
   :version 1
   :registry/revision "revision-1"
   :predicate {:predicate/type :dsl
               :dsl {:op :equals :path [:state] :value :safe}}
   :evidence/refs [ref-a]
   :replay/refs [ref-b]
   :adversarial/refs [ref-c]})

(defn- run [id kind & {:keys [gate deterministic fresh passed model-policy]
                       :or {deterministic true fresh false passed true
                            model-policy :recorded}}]
  (invariant/run {:run/id id
                  :proposal/id "proposal-1"
                  :kind kind
                  :result-ref (case kind :replay ref-b :adversarial ref-c (str "result-" id))
                  :result {:gate/id (if (= gate :g3) :G3-deterministic-suites (or gate :G3-deterministic-suites))
                           :status (if passed :pass :fail)
                           :details-ref (str "details-" id)
                           :model/policy model-policy
                           :deterministic? deterministic
                           :fresh-model? fresh
                           :passed? passed}}))

(deftest proposal-is-closed-data-only
  (doseq [key [:fn :function :code :source :eval :eval-string]]
    (testing (str "forbidden key " key)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"executable"
                            (invariant/proposal (assoc proposal key "not executable")))))))

(deftest pure-status-transitions-are-closed
  (is (= :approved (invariant/transition :proposed :approved)))
  (is (= :active (invariant/transition :approved :active)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"not admissible"
                        (invariant/transition :proposed :active)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"unknown"
                        (invariant/transition :unknown :approved))))

(deftest recorded-replay-qualifies-but-fresh-model-does-not
  (let [recorded (run "run-recorded" :replay)
        fresh (run "run-fresh" :replay :fresh true)]
    (is (true? (:activation-qualified? recorded)))
    (is (false? (:activation-qualified? fresh)))))

(deftest approval-requires-independent-complete-evidence
  (let [replay (run "run-replay" :replay)
        adversarial (run "run-adversarial" :adversarial :gate :g3)
        counterexample (run "run-counterexample" :counterexample :passed false)]
    (is (= :approved
           (:status (invariant/approval proposal
                                         {:reviewer "reviewer-1"
                                          :replay/ref ref-b
                                          :adversarial/ref ref-c}
                                         [replay adversarial]))))
    (testing "the proposer cannot review its own proposal"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"proposer"
                            (invariant/approval proposal
                                                  {:reviewer "proposer-1"
                                                   :replay/ref ref-b
                                                   :adversarial/ref ref-c}
                                                  [replay adversarial]))))
    (testing "a counterexample blocks approval even when the positive runs pass"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"evidence"
                            (invariant/approval proposal
                                                  {:reviewer "reviewer-1"
                                                   :replay/ref ref-b
                                                   :adversarial/ref ref-c}
                                                  [replay adversarial counterexample]))))
    (testing "missing evidence is rejected before approval"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"evidence"
                            (invariant/approval (assoc proposal :evidence/refs [])
                                                  {:reviewer "reviewer-1"
                                                   :replay/ref ref-b
                                                   :adversarial/ref ref-c}
                                                  [replay adversarial]))))))

(deftest fresh-model-cannot-satisfy-approval
  (let [fresh-replay (run "run-fresh" :replay :fresh true)
        adversarial (run "run-adversarial" :adversarial :gate :g3)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"evidence"
                          (invariant/approval proposal
                                                {:reviewer "reviewer-1"
                                                 :replay/ref ref-b
                                                 :adversarial/ref ref-c}
                                                [fresh-replay adversarial])))))
