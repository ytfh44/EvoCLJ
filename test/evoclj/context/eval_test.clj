(ns evoclj.context.eval-test
  (:require [clojure.test :as t]
            [evoclj.context.eval :as ev]
            [evoclj.context.envelope :as envelope]))

;; ---------------------------------------------------------------------------
;; envelope helper
;; ---------------------------------------------------------------------------

(defn- sample-envelope []
  (envelope/make-envelope
   {:task {:task/id "t1" :task/status :completed :task/description "done"}
    :subgoals []
    :residue []
    :evidence []
    :version 1
    :created-at "2026-08-17T00:00:00Z"
    :window {:window/from 0 :window/to 10}
    :tokens-before 5000
    :tokens-after 300
    :compressor {:compressor/model "test-model"
                 :compressor/prompt "compress"}}))

;; ---------------------------------------------------------------------------
;; eval-retention-score
;; ---------------------------------------------------------------------------

(t/deftest retention-score-within-bounds
  (let [record (ev/eval-retention-score (sample-envelope) "ctx" 0.9)]
    (t/is (= :eval/retention (:eval/class record)))
    (t/is (= 0.9 (:eval/score record)))
    (t/is (= :status/pass (:eval/status record)))))

(t/deftest retention-score-below-warn-threshold
  (let [record (ev/eval-retention-score (sample-envelope) "ctx" 0.3)]
    (t/is (= :status/fail (:eval/status record)))))

(t/deftest retention-score-in-warn-band
  (let [record (ev/eval-retention-score (sample-envelope) "ctx" 0.6)]
    (t/is (= :status/warn (:eval/status record)))))

(t/deftest retention-score-throws-on-non-number
  (try
    (ev/eval-retention-score (sample-envelope) "ctx" "bad")
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

(t/deftest retention-score-throws-on-out-of-range
  (try
    (ev/eval-retention-score (sample-envelope) "ctx" 1.5)
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

;; ---------------------------------------------------------------------------
;; eval-regression-score
;; ---------------------------------------------------------------------------

(t/deftest regression-score-within-bounds
  (let [record (ev/eval-regression-score (sample-envelope) "ctx" {} 0.95)]
    (t/is (= :eval/regression (:eval/class record)))
    (t/is (= 0.95 (:eval/score record)))
    (t/is (= :status/pass (:eval/status record)))))

(t/deftest regression-score-throws-on-non-number
  (try
    (ev/eval-regression-score (sample-envelope) "ctx" {} nil)
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

;; ---------------------------------------------------------------------------
;; eval-hallucination-score
;; ---------------------------------------------------------------------------

(t/deftest hallucination-score-within-bounds
  (let [record (ev/eval-hallucination-score (sample-envelope) "ctx" 0.95)]
    (t/is (= :eval/hallucination (:eval/class record)))
    (t/is (= 0.95 (:eval/score record)))
    (t/is (= :status/pass (:eval/status record)))))

(t/deftest hallucination-score-below-fail
  (let [record (ev/eval-hallucination-score (sample-envelope) "ctx" 0.2)]
    (t/is (= :status/fail (:eval/status record)))))

;; ---------------------------------------------------------------------------
;; eval-summary
;; ---------------------------------------------------------------------------

(t/deftest eval-summary-worst-status-wins
  (let [records [(ev/eval-retention-score (sample-envelope) "ctx" 0.9)
                 (ev/eval-regression-score (sample-envelope) "ctx" {} 0.3)
                 (ev/eval-hallucination-score (sample-envelope) "ctx" 0.8)]
        summary (ev/eval-summary records)]
    (t/is (= :status/fail (:eval/overall-status summary)))
    (t/is (= 3 (count (:eval/records summary))))))

(t/deftest eval-summary-all-pass
  (let [records [(ev/eval-retention-score (sample-envelope) "ctx" 0.9)
                 (ev/eval-regression-score (sample-envelope) "ctx" {} 0.95)
                 (ev/eval-hallucination-score (sample-envelope) "ctx" 0.9)]
        summary (ev/eval-summary records)]
    (t/is (= :status/pass (:eval/overall-status summary)))))

(t/deftest passing?-and-failing?-helpers
  (let [pass-summary (ev/eval-summary [(ev/eval-retention-score (sample-envelope) "ctx" 0.9)])
        fail-summary (ev/eval-summary [(ev/eval-retention-score (sample-envelope) "ctx" 0.1)])]
    (t/is (true? (ev/passing? pass-summary)))
    (t/is (false? (ev/failing? pass-summary)))
    (t/is (false? (ev/passing? fail-summary)))
    (t/is (true? (ev/failing? fail-summary)))))

(t/deftest eval-summary-throws-on-non-collection
  (try
    (ev/eval-summary "not a collection")
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

(t/run-tests)