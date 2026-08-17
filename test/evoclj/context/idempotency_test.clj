(ns evoclj.context.idempotency-test
  (:require [clojure.test :as t]
            [evoclj.context.idempotency :as idemp]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.error :as err]))

;; ---------------------------------------------------------------------------
;; envelope helpers
;; ---------------------------------------------------------------------------

(defn- base-envelope [version task residue evidence]
  {:envelope/version version
   :envelope/created-at "2026-08-17T00:00:00Z"
   :envelope/window {:window/from 0 :window/to 10}
   :envelope/tokens-before 5000
   :envelope/tokens-after 300
   :envelope/compressor {:compressor/model "test-model"
                         :compressor/prompt "compress"}
   :envelope/task task
   :envelope/subgoals []
   :envelope/residue residue
   :envelope/evidence evidence})

(defn- minimal-residue [id text]
  {:residue/id id
   :residue/kind :constraint
   :residue/text text
   :residue/source "test"
   :residue/at "2026-08-17T00:00:00Z"})

(defn- minimal-evidence [id text]
  {:evidence/id id
   :evidence/kind :test-pass
   :evidence/text text
   :evidence/at "2026-08-17T00:00:00Z"})

;; ---------------------------------------------------------------------------
;; idempotent-merge
;; ---------------------------------------------------------------------------

(t/deftest idempotent-merge-accumulates-residue-and-evidence
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        a-residue [(minimal-residue 1 "constraint from a")]
        a-evidence [(minimal-evidence 1 "evidence from a")]
        b-task {:task/id "task-1" :task/status :in-progress :task/description "Task"}
        b-residue [(minimal-residue 2 "new discovery from b")]
        b-evidence [(minimal-evidence 2 "new observation from b")]
        a (base-envelope 1 a-task a-residue a-evidence)
        b (base-envelope 1 b-task b-residue b-evidence)
        merged (idemp/idempotent-merge a b)]
    (t/is (= 2 (count (:envelope/residue merged))))
    (t/is (some #(= "constraint from a" (:residue/text %)) (:envelope/residue merged)))
    (t/is (some #(= "new discovery from b" (:residue/text %)) (:envelope/residue merged)))
    (t/is (= 2 (count (:envelope/evidence merged))))
    (t/is (some #(= "evidence from a" (:evidence/text %)) (:envelope/evidence merged)))
    (t/is (some #(= "new observation from b" (:evidence/text %)) (:envelope/evidence merged)))
    (envelope/validate-envelope merged)))

(t/deftest idempotent-merge-throws-when-b-drops-task
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        a (base-envelope 1 a-task [] [])
        b (base-envelope 1 nil [] [])]
    (try
      (idemp/idempotent-merge a b)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/idempotency-violation (:error/type (ex-data e))))))))

(t/deftest idempotent-merge-throws-when-versions-differ
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        a (base-envelope 1 a-task [] [])
        b (base-envelope 2 a-task [] [])]
    (try
      (idemp/idempotent-merge a b)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/idempotency-violation (:error/type (ex-data e))))
        (t/is (= 1 (:a-version (ex-data e))))
        (t/is (= 2 (:b-version (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; residue-preserved? / evidence-preserved?
;; ---------------------------------------------------------------------------

(t/deftest residue-preserved-true-when-all-present
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        r1 (minimal-residue 1 "text alpha")
        r2 (minimal-residue 2 "text beta")
        a (base-envelope 1 a-task [r1 r2] [])
        result-task {:task/id "task-1" :task/status :in-progress :task/description "Task"}
        result (base-envelope 1 result-task [r1 r2 (minimal-residue 3 "new")] [])]
    (t/is (idemp/residue-preserved? a result))))

(t/deftest residue-preserved-false-when-text-missing
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        r1 (minimal-residue 1 "alpha only in a")
        a (base-envelope 1 a-task [r1] [])
        result-task {:task/id "task-1" :task/status :in-progress :task/description "Task"}
        result (base-envelope 1 result-task [(minimal-residue 2 "different text")] [])]
    (t/is (not (idemp/residue-preserved? a result)))))

(t/deftest evidence-preserved-true-when-all-present
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        e1 (minimal-evidence 1 "evidence alpha")
        e2 (minimal-evidence 2 "evidence beta")
        a (base-envelope 1 a-task [] [e1 e2])
        result-task {:task/id "task-1" :task/status :in-progress :task/description "Task"}
        result (base-envelope 1 result-task [] [e1 e2 (minimal-evidence 3 "new")])]
    (t/is (idemp/evidence-preserved? a result))))

(t/deftest evidence-preserved-false-when-text-missing
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        e1 (minimal-evidence 1 "alpha only in a")
        a (base-envelope 1 a-task [] [e1])
        result-task {:task/id "task-1" :task/status :in-progress :task/description "Task"}
        result (base-envelope 1 result-task [] [(minimal-evidence 2 "different evidence")])]
    (t/is (not (idemp/evidence-preserved? a result)))))

;; ---------------------------------------------------------------------------
;; core-fields-preserved?
;; ---------------------------------------------------------------------------

(t/deftest core-fields-preserved-true-when-b-carries-all
  (let [a {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 300
           :envelope/compressor {:compressor/model "m" :compressor/prompt "p"}
           :envelope/task {:task/id "t1" :task/status :pending :task/description "Task"}
           :envelope/residue []
           :envelope/evidence []}
        b (assoc a :envelope/tokens-after 200)]
    (t/is (idemp/core-fields-preserved? a b))))

(t/deftest core-fields-preserved-false-when-b-drops-field
  (let [a {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 300
           :envelope/compressor {:compressor/model "m" :compressor/prompt "p"}
           :envelope/task {:task/id "t1" :task/status :pending :task/description "Task"}
           :envelope/residue []
           :envelope/evidence []}
        b (dissoc a :envelope/tokens-after)]
    (t/is (not (idemp/core-fields-preserved? a b)))))

(t/deftest core-fields-preserved-false-when-b-drops-task
  (let [a {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 300
           :envelope/compressor {:compressor/model "m" :compressor/prompt "p"}
           :envelope/task {:task/id "t1" :task/status :pending :task/description "Task"}
           :envelope/residue []
           :envelope/evidence []}
        b (assoc a :envelope/task nil)]
    (t/is (not (idemp/core-fields-preserved? a b)))))

(t/deftest core-fields-preserved-true-when-both-task-nil
  (let [a {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 300
           :envelope/compressor {:compressor/model "m" :compressor/prompt "p"}
           :envelope/task nil
           :envelope/residue []
           :envelope/evidence []}
        b (assoc a :envelope/tokens-after 200)]
    (t/is (idemp/core-fields-preserved? a b))))

;; ---------------------------------------------------------------------------
;; idempotency-report
;; ---------------------------------------------------------------------------

(t/deftest idempotency-report-all-keys-and-lost-residue
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        r1 (minimal-residue 1 "present in both")
        r2 (minimal-residue 2 "present only in a")
        a (base-envelope 1 a-task [r1 r2] [])
        result-task {:task/id "task-1" :task/status :in-progress :task/description "Task"}
        r3 (minimal-residue 3 "new in result")
        result (base-envelope 1 result-task [r1 r3] [])
        report (idemp/idempotency-report a a result)]
    (t/is (contains? report :idempotency/valid?))
    (t/is (contains? report :idempotency/residue-preserved?))
    (t/is (contains? report :idempotency/evidence-preserved?))
    (t/is (contains? report :idempotency/core-fields-preserved?))
    (t/is (contains? report :idempotency/version-match?))
    (t/is (contains? report :idempotency/lost-residue-texts))
    (t/is (contains? report :idempotency/lost-evidence-texts))
    (t/is (false? (:idempotency/valid? report)))
    (t/is (false? (:idempotency/residue-preserved? report)))
    (t/is (= ["present only in a"] (:idempotency/lost-residue-texts report)))
    (t/is (= [] (:idempotency/lost-evidence-texts report)))))

(t/deftest idempotency-report-valid-when-all-checks-pass
  (let [a-task {:task/id "task-1" :task/status :pending :task/description "Task"}
        r1 (minimal-residue 1 "alpha")
        e1 (minimal-evidence 1 "beta")
        a (base-envelope 1 a-task [r1] [e1])
        result-task {:task/id "task-1" :task/status :in-progress :task/description "Task"}
        result (base-envelope 1 result-task
                             [r1 (minimal-residue 2 "new")]
                             [e1 (minimal-evidence 2 "new")])
        report (idemp/idempotency-report a a result)]
    (t/is (true? (:idempotency/valid? report)))
    (t/is (true? (:idempotency/residue-preserved? report)))
    (t/is (true? (:idempotency/evidence-preserved? report)))
    (t/is (true? (:idempotency/core-fields-preserved? report)))
    (t/is (true? (:idempotency/version-match? report)))
    (t/is (= [] (:idempotency/lost-residue-texts report)))
    (t/is (= [] (:idempotency/lost-evidence-texts report)))))

(t/run-tests)