(ns evoclj.context.envelope-test
  (:require [clojure.test :as t]
            [evoclj.context.compression.envelope :as env]
            [evoclj.context.compression.error :as err]))

;; --------------------------------------------------------------------
;; Helpers
;; --------------------------------------------------------------------

(defn- make-minimal-envelope []
  {:envelope/version 1
   :envelope/created-at "2026-08-17T00:00:00Z"
   :envelope/window {:window/from 0 :window/to 10}
   :envelope/tokens-before 5000
   :envelope/tokens-after 300
   :envelope/compressor {:compressor/model "test-model"
                         :compressor/prompt "compress"}
   :envelope/task {:task/id "task-1"
                   :task/status :pending
                   :task/description "Test task"}
   :envelope/subgoals []
   :envelope/residue [{:residue/id 1
                       :residue/kind :constraint
                       :residue/text "must use pr-str"
                       :residue/source "user"
                       :residue/at "2026-08-17T00:00:00Z"}]
   :envelope/evidence [{:evidence/id 1
                        :evidence/kind :test-pass
                        :evidence/text "all tests green"
                        :evidence/at "2026-08-17T00:00:00Z"}]})

;; --------------------------------------------------------------------
;; Tests: make-envelope / EDN round-trip
;; --------------------------------------------------------------------

(t/deftest make-envelope-round-trips-through-edn
  (let [original (env/make-envelope
                 {:task {:task/id "t1" :task/status :in-progress
                         :task/description "do the thing"}
                  :subgoals [{:subgoal/id "sg1"
                              :subgoal/status :pending
                              :subgoal/description "subgoal one"
                              :subgoal/parent "t1"}]
                  :residue [{:residue/id 1
                             :residue/kind :constraint
                             :residue/text "no mutation"
                             :residue/source "design"
                             :residue/at "2026-08-17T10:00:00Z"}]
                  :evidence [{:evidence/id 1
                              :evidence/kind :observation
                              :evidence/text "logs show spike"
                              :evidence/at "2026-08-17T10:00:00Z"}]
                  :created-at "2026-08-17T10:00:00Z"
                  :window {:window/from 0 :window/to 5}
                  :tokens-before 1000
                  :tokens-after 100
                  :compressor {:compressor/model "gpt-4"
                               :compressor/prompt "summarize"}})
        serialized (env/envelope->edn original)
        deserialized (env/edn->envelope serialized)]
    (t/is (= original deserialized))))

;; --------------------------------------------------------------------
;; Tests: validate-envelope rejects malformed input
;; --------------------------------------------------------------------

(t/deftest validate-envelope-rejects-missing-envelope-version
  (let [bad (dissoc (make-minimal-envelope) :envelope/version)]
    (try
      (env/validate-envelope bad)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/compression-invalid (:error/type (ex-data e))))))))

(t/deftest validate-envelope-rejects-non-string-residue-text
  (let [bad (assoc (make-minimal-envelope)
                   :envelope/residue
                   [{:residue/id 1
                     :residue/kind :constraint
                     :residue/text 12345
                     :residue/source "test"
                     :residue/at "2026-08-17T00:00:00Z"}])]
    (try
      (env/validate-envelope bad)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/compression-invalid (:error/type (ex-data e))))))))

;; --------------------------------------------------------------------
;; Tests: merge-envelopes
;; --------------------------------------------------------------------

(t/deftest merge-envelopes-accumulates-residue
  (let [a {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 300
           :envelope/compressor {:compressor/model "m1" :compressor/prompt "p"}
           :envelope/task {:task/id "task-1"
                           :task/status :pending
                           :task/description "Original task"}
           :envelope/subgoals []
           :envelope/residue [{:residue/id 1
                               :residue/kind :constraint
                               :residue/text "constraint from a"
                               :residue/source "a"
                               :residue/at "2026-08-17T00:00:00Z"}]
           :envelope/evidence []}
        b {:envelope/version 1
           :envelope/created-at "2026-08-17T01:00:00Z"
           :envelope/window {:window/from 11 :window/to 20}
           :envelope/tokens-before 5300
           :envelope/tokens-after 200
           :envelope/compressor {:compressor/model "m2" :compressor/prompt "q"}
           :envelope/task {:task/id "task-1"
                           :task/status :in-progress
                           :task/description "Updated task"}
           :envelope/subgoals []
           :envelope/residue [{:residue/id 2
                               :residue/kind :discovery
                               :residue/text "found a new api"
                               :residue/source "b"
                               :residue/at "2026-08-17T01:00:00Z"}]
           :envelope/evidence []}
        merged (env/merge-envelopes a b)]
    (t/is (= 2 (count (:envelope/residue merged))))
    (t/is (= "constraint from a" (->> merged :envelope/residue (map :residue/text) first)))
    (t/is (= "found a new api" (->> merged :envelope/residue (map :residue/text) second)))))

(t/deftest merge-envelopes-throws-when-b-drops-task-id
  (let [a {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 300
           :envelope/compressor {:compressor/model "m1" :compressor/prompt "p"}
           :envelope/task {:task/id "task-1"
                           :task/status :pending
                           :task/description "Original task"}
           :envelope/subgoals []
           :envelope/residue []
           :envelope/evidence []}
        b {:envelope/version 1
           :envelope/created-at "2026-08-17T01:00:00Z"
           :envelope/window {:window/from 11 :window/to 20}
           :envelope/tokens-before 5300
           :envelope/tokens-after 200
           :envelope/compressor {:compressor/model "m2" :compressor/prompt "q"}
           :envelope/task nil
           :envelope/subgoals []
           :envelope/residue []
           :envelope/evidence []}]
    (try
      (env/merge-envelopes a b)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/idempotency-violation (:error/type (ex-data e))))))))

(t/deftest merge-envelopes-b-wins-task-status
  (let [a {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 300
           :envelope/compressor {:compressor/model "m1" :compressor/prompt "p"}
           :envelope/task {:task/id "task-1"
                           :task/status :pending
                           :task/description "Task"}
           :envelope/subgoals []
           :envelope/residue []
           :envelope/evidence []}
        b {:envelope/version 1
           :envelope/created-at "2026-08-17T01:00:00Z"
           :envelope/window {:window/from 11 :window/to 20}
           :envelope/tokens-before 5300
           :envelope/tokens-after 200
           :envelope/compressor {:compressor/model "m2" :compressor/prompt "q"}
           :envelope/task {:task/id "task-1"
                           :task/status :completed
                           :task/description "Task"}
           :envelope/subgoals []
           :envelope/residue []
           :envelope/evidence []}
        merged (env/merge-envelopes a b)]
    (t/is (= :completed (:task/status (:envelope/task merged))))))

(t/deftest envelope-tokens-returns-tokens-after
  (let [e {:envelope/version 1
           :envelope/created-at "2026-08-17T00:00:00Z"
           :envelope/window {:window/from 0 :window/to 10}
           :envelope/tokens-before 5000
           :envelope/tokens-after 999
           :envelope/compressor {:compressor/model "m" :compressor/prompt "p"}
           :envelope/task nil
           :envelope/subgoals []
           :envelope/residue []
           :envelope/evidence []}]
    (t/is (= 999 (env/envelope-tokens e)))))

(t/run-tests)
