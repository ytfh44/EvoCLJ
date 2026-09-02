(ns evoclj.context.apply-test
  (:require [clojure.test :as t]
            [evoclj.context.compression.apply :as apply]
            [evoclj.context.compression.envelope :as envelope]
            [evoclj.context.compression.error :as err]))

;; ---------------------------------------------------------------------------
;; envelope helper
;; ---------------------------------------------------------------------------

(defn- sample-envelope []
  (envelope/make-envelope
   {:task {:task/id "t1" :task/status :completed :task/description "done"}
    :subgoals [{:subgoal/id "sg1" :subgoal/status :completed
                :subgoal/description "step one" :subgoal/parent "t1"}]
    :residue [{:residue/id 1 :residue/kind :constraint
               :residue/text "must not break X" :residue/source "user"
               :residue/at "2026-08-17T00:00:00Z"}]
    :evidence [{:evidence/id 1 :evidence/kind :observation
                :evidence/text "saw it work" :evidence/at "2026-08-17T00:00:00Z"}]
    :version 1
    :created-at "2026-08-17T00:00:00Z"
    :window {:window/from 0 :window/to 10}
    :tokens-before 5000
    :tokens-after 300
    :compressor {:compressor/model "test-model"
                 :compressor/prompt "compress"}}))

;; ---------------------------------------------------------------------------
;; apply-envelope
;; ---------------------------------------------------------------------------

(t/deftest apply-envelope-concatenates-envelope-and-tail
  (let [env (sample-envelope)
        tail "the fresh tail of the conversation"
        result (apply/apply-envelope env tail)]
    (t/is (string? result))
    (t/is (clojure.string/starts-with? result (envelope/envelope->edn env)))
    (t/is (clojure.string/ends-with? result tail))))

(t/deftest apply-envelope-uses-default-separator
  (let [env (sample-envelope)
        tail "tail"
        result (apply/apply-envelope env tail)]
    (t/is (clojure.string/includes? result "\ntail"))))

(t/deftest apply-envelope-respects-custom-separator
  (let [env (sample-envelope)
        tail "tail"
        result (apply/apply-envelope env tail " | ")]
    (t/is (clojure.string/includes? result " | tail"))))

(t/deftest apply-envelope-throws-on-malformed-envelope
  (try
    (apply/apply-envelope {} "tail")
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

(t/deftest apply-envelope-throws-on-non-string-tail
  (let [env (sample-envelope)]
    (try
      (apply/apply-envelope env nil)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/apply-invalid (:error/type (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; envelope-prefix
;; ---------------------------------------------------------------------------

(t/deftest envelope-prefix-returns-serialized-envelope
  (let [env (sample-envelope)
        prefix (apply/envelope-prefix env)]
    (t/is (= (envelope/envelope->edn env) prefix))))

(t/deftest envelope-prefix-throws-on-malformed-envelope
  (try
    (apply/envelope-prefix {})
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

;; ---------------------------------------------------------------------------
;; applied-context-length
;; ---------------------------------------------------------------------------

(t/deftest applied-context-length-estimates-size
  (let [env (sample-envelope)
        tail "hello world"
        len (apply/applied-context-length env tail)]
    (t/is (pos? len))
    (t/is (> len (count tail)))))

(t/deftest applied-context-length-throws-on-non-string-tail
  (let [env (sample-envelope)]
    (try
      (apply/applied-context-length env nil)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/apply-invalid (:error/type (ex-data e))))))))

(t/deftest applied-context-length-matches-actual-length
  (let [env (sample-envelope)
        tail "the quick brown fox"
        actual (count (apply/apply-envelope env tail))
        estimated (apply/applied-context-length env tail)]
    (t/is (= actual estimated))))

(t/run-tests)