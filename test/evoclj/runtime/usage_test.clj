(ns evoclj.runtime.usage-test
  "component tests for standard usage accounting.

  usage.clj standardizes usage accounting across the runtime: a pure,
  merge-based accumulator over immutable usage maps. Each usage sample
  carries monotonic counters (wall-ms, model tokens, provider/tool call
  counts, SCI steps, artifact bytes) plus attribution keys
  (:session/id, :intent/id, :node/id — Global Constraint 20). The key
  vocabulary reuses what the runtime already emits:

    - :steps and :wall-ms, the exact keys evoclj.sci.execute reports
    - :total-cost / :cost, the episode cost keys evoclj.evolution.*
      reads (fallback order :total-cost then :cost)
    - provider call counts, extending the per-:cap/id call-count map
      evoclj.intent.dispatch reports as a single :provider-calls total

  The three normative steps, in the task's numbered order:

  - Step 1: usage accumulates MONOTONICALLY within a session — merges
    never decrease a counter and wall-ms totals >= their parts.
  - Step 2: child/provider usage is attributed to the originating
    Intent/session/node — merging attributed samples preserves the
    origin and never overwrites it.
  - Step 3: evaluation can aggregate usage per case (by session) and
    per successful task (outcome :status :completed, the same success
    rule evoclj.evolution.evidence applies)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.runtime.usage :as u]))

(def s1 (random-uuid))
(def s2 (random-uuid))
(def n1 (random-uuid))
(def n2 (random-uuid))

(defn sample
  ([] (u/attributed
       {:wall-ms 120
        :model-input-tokens 400
        :model-output-tokens 250
        :model-cost-units 1.5
        :provider-calls 2
        :tool-calls 3
        :network-bytes 1024
        :steps 500
        :artifact-bytes 4096}
       {:session/id s1 :intent/id (random-uuid) :node/id n1}))
  ([session-id node-id]
   (u/attributed
    {:wall-ms 120
     :model-input-tokens 400
     :model-output-tokens 250
     :provider-calls 2
     :tool-calls 3
     :steps 500}
    {:session/id session-id :intent/id (random-uuid) :node/id node-id})))

;; ============================================================================
;; Step 1 — monotonic accumulation within a session
;; ============================================================================

(deftest merge-is-monotonic-within-a-session
  (let [a (sample)
        b (sample)
        c (u/add a b)]
    (testing "merging never decreases any counter"
      (doseq [k u/counter-keys]
        (is (>= (get c k 0) (get a k 0))
            (str k " must not decrease vs first sample"))
        (is (>= (get c k 0) (get b k 0))
            (str k " must not decrease vs second sample"))))
    (testing "repeated merges only increase"
      (let [twice (u/add c c)]
        (doseq [k u/counter-keys]
          (is (>= (get twice k 0) (get c k 0))))))
    (testing "wall-ms total >= the parts"
      (is (>= (:wall-ms c) (:wall-ms a)))
      (is (>= (:wall-ms c) (:wall-ms b)))
      (is (= (+ (:wall-ms a) (:wall-ms b)) (:wall-ms c))))))

(deftest empty-usage-is-the-merge-identity
  (let [a (sample)]
    (is (= a (u/add a u/empty-usage)))
    (is (= a (u/add u/empty-usage a)))
    (is (= u/empty-usage (u/add u/empty-usage u/empty-usage)))))

(deftest merge-is-pure
  (let [a (sample)
        b (sample)
        before-a a
        before-b b
        _ (u/add a b)]
    (is (= before-a a) "add must not mutate its arguments")
    (is (= before-b b) "add must not mutate its arguments")))

(deftest merge-is-associative-and-commutative
  (let [a (sample)
        b (sample)
        c (u/attributed
           {:wall-ms 7 :model-input-tokens 9 :steps 11}
           {:session/id s2 :intent/id (random-uuid) :node/id n2})
        counters (fn [m] (select-keys m u/counter-keys))]
    (is (= (u/add (u/add a b) c)
           (u/add a (u/add b c)))
        "associativity")
    (is (= (counters (u/add a b)) (counters (u/add b a)))
        "counter sum is commutative")
    (is (= (:session/id a) (:session/id (u/add a b)))
        "attribution is left-biased, not commutative")
    (is (= (:wall-ms (u/add a b))
           (+ (:wall-ms a) (:wall-ms b))))))

(deftest sci-usage-vocabulary-merges-directly
  (testing "the exact :usage shape evoclj.sci.execute reports merges in"
    (let [sci-usage {:steps 500 :wall-ms 120}
          a (u/attributed sci-usage {:session/id s1 :node/id n1})
          merged (u/add a a)]
      (is (= 1000 (:steps merged)))
      (is (= 240 (:wall-ms merged))))))

(deftest episode-cost-keys-merge
  (testing ":total-cost / :cost episode keys (evoclj.evolution.*) are counters"
    (let [a (u/add {:total-cost 55} {:total-cost 45})]
      (is (= 100 (:total-cost a))))
    (let [b (u/add {:cost 3} {:cost 4})]
      (is (= 7 (:cost b))))))

;; ============================================================================
;; Step 2 — attribution to the originating Intent/session/node
;; ============================================================================

(deftest attribution-preserves-origin-across-merges
  (let [a (sample s1 n1)
        b (sample s2 n2)
        c (u/add a b)]
    (testing "the merged map keeps the first sample's origin"
      (is (= s1 (:session/id c)))
      (is (= n1 (:node/id c))))
    (testing "origin keys are never summed or overwritten"
      (is (some? (:session/id c)))
      (is (some? (:intent/id c)))
      (is (some? (:node/id c)))
      (is (= #{s1} (set [(:session/id c)]))))))

(deftest attributed-fills-missing-origin
  (let [bare {:wall-ms 5 :steps 3}
        with-attribution (u/attributed bare {:session/id s1 :intent/id n1 :node/id n1})]
    (is (= bare (select-keys with-attribution u/counter-keys)))
    (is (= s1 (:session/id with-attribution)))
    (is (= n1 (:intent/id with-attribution)))
    (is (= n1 (:node/id with-attribution)))
    (is (= with-attribution (u/attributed with-attribution {:session/id s2}))
        "attributed never overwrites an existing origin")))

(deftest attributed-rejects-unknown-attribution-keys
  (is (thrown? clojure.lang.ExceptionInfo
               (u/attributed {:steps 1} {:session/id s1 :not/an-attribution-key 1}))))

;; ============================================================================
;; Step 3 — aggregation per case and per successful task
;; ============================================================================

(deftest aggregate-combines-a-collection-of-samples
  (let [samples [(sample s1 n1) (sample s1 n1) (sample s2 n2)]
        total (u/aggregate samples)]
    (is (= (+ 120 120 120) (:wall-ms total)))
    (is (= (+ 400 400 400) (:model-input-tokens total)))
    (is (= (+ 2 2 2) (:provider-calls total)))
    (is (= (+ 500 500 500) (:steps total)))))

(deftest aggregate-by-session-groups-per-case
  (let [samples [(sample s1 n1) (sample s1 n1) (sample s2 n2)]
        by-session (u/aggregate-by-session samples)]
    (is (= #{s1 s2} (set (keys by-session))))
    (is (= 240 (:wall-ms (get by-session s1)))
        "both s1 samples land in one per-case bucket")
    (is (= 120 (:wall-ms (get by-session s2))))
    (is (= s1 (:session/id (get by-session s1)))
        "the bucket keeps the origin it was attributed to")))

(deftest aggregate-by-outcome-partitions-samples
  (let [ok (assoc (sample s1 n1) :outcome {:status :completed :score 0.9})
        failed (assoc (sample s1 n1) :outcome {:status :failed :score nil})
        by-outcome (u/aggregate-by-outcome [ok failed ok])]
    (is (= #{:completed :failed} (set (keys by-outcome))))
    (is (= 240 (:wall-ms (get by-outcome :completed))))
    (is (= 120 (:wall-ms (get by-outcome :failed))))))

(deftest aggregate-successful-covers-only-successful-tasks
  (let [ok (assoc (sample s1 n1) :outcome {:status :completed :score 0.9})
        failed (assoc (sample s2 n2) :outcome {:status :failed :score nil})
        successes (u/aggregate-successful [ok failed ok])]
    (is (u/successful? ok))
    (is (not (u/successful? failed)))
    (is (= 240 (:wall-ms successes)))
    (is (= 800 (:model-input-tokens successes)))
    (is (= 4 (:provider-calls successes)))
    (is (= s1 (:session/id successes))
        "the successful aggregate keeps the successful sample's origin")))
