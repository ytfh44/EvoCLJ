(ns evoclj.evolution.scheduler-test
  "Tests for `evoclj.evolution.scheduler` (component).

  The unit tests drive `run-cycles!` with an INJECTED mock
  `run-generation` fn (the one-generation step), so the loop wiring and
  its integration with `loop-policy/decide-continue?` are verified
  WITHOUT running the heavy propose→eval→promote path. The mock returns
  a controlled sequence of per-generation `:utility` values; from the
  call count and the returned advisor map we assert exactly how many
  generations ran and which `:final-decision` terminated the loop.

  A second unit test exercises `make-generation-runner` (the production
  wiring that reuses the public evolve/eval/promote APIs) with those
  three public APIs `with-redefs`-stubbed, proving the composition calls
  the right APIs and returns the right summary, again without a live
  store.

  Integration note: a real `:demo` provision (a `clojure -X/-T` state
  setup) is intentionally NOT performed here — the project forbids
  `clojure -T…` (it would write an unrelated `tools/` dir) and the demo
  state requires CLI host wiring that is out of scope for this unit
  suite. The spec sanctions this exact fallback: \"用注入的 mock
  评估/晋升 fn 验证循环逻辑正确即可\"."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.scheduler :as scheduler]
            [evoclj.evolution.core :as evolution]
            [evoclj.eval.core :as eval-core]
            [evoclj.promotion.promote :as promote]))

;; --- mock one-generation step ------------------------------------------------

(defn- counting-run-generation
  "A mock `run-generation`: returns `:utility` from `utilities` in order
  (repeating the last value past the end, so a plateau/regression
  scenario can be extended as needed), and records the call count in the
  returned `:calls` atom. Each step also returns a `:generation/id`."
  [utilities]
  (let [xs (vec utilities)
        calls (atom 0)]
    {:calls calls
     :fn (fn []
           (swap! calls inc)
           (let [i (min (dec @calls) (dec (count xs)))]
             {:generation/id (str "generation-" @calls)
              :utility (double (nth xs i))}))}))


;; --- cost-guard wiring (component) ---------------------------------------------

(defn- cost-run-generation
  "A mock `run-generation` that returns `:utility` and `:cost` from
  `values` in order (repeating the last value past the end), recording
  the call count in `:calls`."
  [values]
  (let [xs (vec values)
        calls (atom 0)]
    {:calls calls
     :fn (fn []
           (swap! calls inc)
           (let [i (min (dec @calls) (dec (count xs)))]
             {:generation/id (str "generation-" @calls)
              :utility (double (nth xs i))
              :cost (double (nth xs i))}))}))

(deftest test-run-cycles-stops-on-cost
  (testing "loop terminates with :stop-cost when cumulative cost exceeds threshold"
    ;; utilities always improve so policy does not stop first; cumulative
    ;; cost after 3 generations (0.3 + 0.5 + 0.7 = 1.5) exceeds 1.0.
    (let [rg (cost-run-generation [0.3 0.5 0.7])
          result (scheduler/run-cycles!
                   (:fn rg)
                   {:max-generations 20}
                   :cost-guard {:threshold 1.0})]
      (is (= :stop-cost (:final-decision result)))
      (is (= 3 (:cycles result)))
      (is (= 3 (count (:generations result))))
      (is (re-find #"exceeds threshold" (:stop-reason result))))))

(deftest test-run-cycles-continues-when-cost-below-threshold
  (testing "loop continues to max-cycles when cost stays below threshold"
    ;; max-generations is high so policy never stops; max-cycles is 2 so
    ;; the safety cap terminates after two generations with cumulative
    ;; cost 0.3 still below the 1.0 threshold.
    (let [rg (cost-run-generation [0.1 0.2])
          result (scheduler/run-cycles!
                   (:fn rg)
                   {:max-generations 100}
                   :max-cycles 2
                   :cost-guard {:threshold 1.0})]
      (is (= :stop-max-cycles (:final-decision result)))
      (is (= 2 (:cycles result))))))

(deftest test-run-cycles-cost-guard-no-threshold-is-ignored
  (testing "a cost-guard map without :threshold does not stop the loop"
    (let [rg (cost-run-generation [0.5 0.6 0.7])
          result (scheduler/run-cycles!
                   (:fn rg)
                   {:max-generations 3}
                   :cost-guard {})]
      (is (= :stop-max-gen (:final-decision result)))
      (is (= 3 (:cycles result))))))

(deftest test-run-cycles-no-cost-guard-unchanged
  (testing "without :cost-guard the loop follows policy and cap only"
    (let [rg (counting-run-generation [0.50 0.60 0.70])
          result (scheduler/run-cycles!
                   (:fn rg)
                   {:max-generations 3})]
      (is (= :stop-max-gen (:final-decision result)))
      (is (= 3 (:cycles result)))
      (is (= 3 (count (:generations result)))))))
;; --- loop termination integration --------------------------------------------

(deftest test-run-cycles-stops-at-max-gen
  (testing "loop terminates with :stop-max-gen when history reaches max-generations"
    (let [rg (counting-run-generation [0.50 0.60 0.70])
          result (scheduler/run-cycles! (:fn rg) {:max-generations 3})]
      (is (= :stop-max-gen (:final-decision result)))
      (is (= 3 (:cycles result)))
      (is (= 3 (count (:generations result))))
      (is (= 3 (deref (:calls rg))))
      (is (re-find #"reached max-generations 3" (:stop-reason result))))))

(deftest test-run-cycles-stops-on-regression
  (testing "loop terminates with :stop-regression when the latest gen regresses"
    (let [rg (counting-run-generation [0.80 0.60])
          result (scheduler/run-cycles!
                   (:fn rg)
                   {:max-generations 20 :stop-on-regression? true})]
      (is (= :stop-regression (:final-decision result)))
      (is (= 2 (:cycles result)))
      (is (= 2 (deref (:calls rg))))
      (is (re-find #"regressed vs parent" (:stop-reason result))))))

(deftest test-run-cycles-stops-on-plateau
  (testing "loop terminates with :stop-plateau once the trailing window stops improving"
    ;; The policy checks the trailing window of size min(plateau-window,
    ;; count); with two near-equal utilities the spread (< min-improvement)
    ;; already trips the plateau branch at cycle 2 (window size 2), which
    ;; is the earliest it can fire (a window needs >= 2 points).
    (let [rg (counting-run-generation [0.70 0.70 0.70])
          result (scheduler/run-cycles!
                   (:fn rg)
                   {:max-generations 20 :plateau-window 3 :min-improvement 0.05})]
      (is (= :stop-plateau (:final-decision result)))
      (is (= 2 (:cycles result)))
      (is (re-find #"utility plateau over window 2" (:stop-reason result))))))

(deftest test-run-cycles-continues-until-cap
  (testing "loop keeps advancing (policy returns :continue) until the safety cap"
    (let [rg (counting-run-generation [0.60 0.72 0.85])
          result (scheduler/run-cycles!
                   (:fn rg)
                   {:max-generations 20 :stop-on-regression? true
                    :min-improvement 0.01}
                   :max-cycles 2)]
      ;; the ONLY stop was the safety cap, proving the policy said
      ;; :continue each iteration and the loop advanced normally.
      (is (= :stop-max-cycles (:final-decision result)))
      (is (= 2 (:cycles result)))
      (is (= 2 (count (:generations result))))
      ;; parent/utility is auto-filled from the previous generation's utility
      (let [gens (:generations result)]
        (is (= (:utility (first gens)) (:parent/utility (second gens))))))))

(deftest test-run-cycles-fills-parent-utility
  (testing "run-cycles! fills :parent/utility from the prior generation's :utility"
    (let [rg (counting-run-generation [0.40 0.55 0.65])
          result (scheduler/run-cycles! (:fn rg) {:max-generations 3})
          gens (:generations result)]
      (is (= 0.0 (:parent/utility (first gens))))   ; first gen: seed default
      (is (= 0.40 (:parent/utility (second gens))))
      (is (= 0.55 (:parent/utility (nth gens 2)))))))

(deftest test-run-cycles-with-mock-evolution-pipeline
  (testing "a mock evolve/eval/promote pipeline drives the loop and terminates cleanly"
    ;; NOTE: real :demo provision (clojure -X/-T state) is intentionally
    ;; skipped — see the ns docstring for the sanitized fallback. We inject
    ;; a mock that mimics one generation's evolve→eval→promote outcome.
    (let [state (atom {:gen 0})
          run-gen (fn []
                    (swap! state update :gen inc)
                    (let [gen (:gen @state)
                          util (double (+ 0.4 (* 0.1 gen)))]
                      {:generation/id (str "generation-" gen)
                       :utility util}))
          result (scheduler/run-cycles!
                   run-gen
                   {:max-generations 4 :min-improvement 0.01})]
      (is (= :stop-max-gen (:final-decision result)))
      (is (= 4 (:cycles result)))
      (is (= 4 (count (:generations result))))
      (is (every? #(contains? % :parent/utility) (:generations result)))
      (is (= 0.5 (:utility (first (:generations result)))))
      (is (= 0.8 (:utility (last (:generations result))))))))

;; --- make-generation-runner wiring (public APIs reused) ----------------------

(deftest test-make-generation-runner-composes-apis
  (testing "make-generation-runner wires propose/eval/promote and returns the summary"
    (let [proposed (atom 0) evaluated (atom 0) promoted (atom 0)
          cand-id (java.util.UUID/randomUUID)
          candidate {:candidate/id cand-id
                     :parent/generation-id "generation-1"
                     :state :evaluation-pending}
          eval-record {:candidate/id cand-id
                       :evaluation/id (java.util.UUID/randomUUID)
                       :parent/generation-id "generation-1"
                       :summary {:utility {:task/success {:parent 0.6
                                                          :candidate 0.8}}}
                       :eligibility {:eligible? true}}]
      (with-redefs [evolution/propose-candidates!
                    (fn [_ _] (swap! proposed inc) [candidate])
                    eval-core/evaluate-candidate!
                    (fn [_ _ _] (swap! evaluated inc) eval-record)
                    promote/promote!
                    (fn [_ _] (swap! promoted inc)
                      {:status :promoted :from "generation-1"
                       :to "generation-2"})]
        (let [run-gen (scheduler/make-generation-runner
                        {:evolution-system {}
                         :evaluator {}
                         :promotion-system {}
                         :profile-id :default-v1
                         :current-generation-id (fn [] "generation-1")
                         :candidates-for-generation (fn [_] [candidate])})
              summary (run-gen)]
          (is (= 1 @proposed))
          (is (= 1 @evaluated))
          (is (= 1 @promoted))
          (is (= "generation-2" (:generation/id summary)))
          (is (= 0.8 (:utility summary))))))))

(deftest test-make-generation-runner-no-promote-skips
  (testing "with :no-promote? the runner records a skip instead of promoting"
    (let [proposed (atom 0) evaluated (atom 0) promoted (atom 0)
          cand-id (java.util.UUID/randomUUID)
          candidate {:candidate/id cand-id
                     :parent/generation-id "generation-1"
                     :state :evaluation-pending}
          eval-record {:candidate/id cand-id
                       :evaluation/id (java.util.UUID/randomUUID)
                       :parent/generation-id "generation-1"
                       :summary {:utility {:task/success {:parent 0.6
                                                          :candidate 0.8}}}
                       :eligibility {:eligible? true}}]
      (with-redefs [evolution/propose-candidates!
                    (fn [_ _] (swap! proposed inc) [candidate])
                    eval-core/evaluate-candidate!
                    (fn [_ _ _] (swap! evaluated inc) eval-record)
                    promote/promote!
                    (fn [_ _] (swap! promoted inc) {:status :promoted})]
        (let [run-gen (scheduler/make-generation-runner
                        {:evolution-system {}
                         :evaluator {}
                         :promotion-system {}
                         :profile-id :default-v1
                         :no-promote? true
                         :current-generation-id (fn [] "generation-1")
                         :candidates-for-generation (fn [_] [candidate])})
              summary (run-gen)]
          (is (= 1 @proposed))
          (is (= 1 @evaluated))
          (is (= 0 @promoted))   ; promote! must NOT be called
          (is (= "generation-1" (:generation/id summary)))
          (is (= 0.8 (:utility summary))))))))
