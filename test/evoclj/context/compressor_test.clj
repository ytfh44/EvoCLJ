(ns evoclj.context.compressor-test
  (:require [clojure.test :as t]
            [evoclj.context.compressor :as comp]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.error :as err]))

;; ---------------------------------------------------------------------------
;; test data
;; ---------------------------------------------------------------------------

(defn- sample-summary []
  {:task {:task/id "t1" :task/status :in-progress :task/description "build the thing"}
   :subgoals [{:subgoal/id "sg1" :subgoal/status :completed
               :subgoal/description "step one" :subgoal/parent "t1"}]
   :residue [{:residue/id 1 :residue/kind :constraint
              :residue/text "must not break X" :residue/source "user"
              :residue/at "2026-08-17T00:00:00Z"}]
   :evidence [{:evidence/id 1 :evidence/kind :observation
               :evidence/text "src/evoclj/context/compressor.clj"
               :evidence/at "2026-08-17T00:00:00Z"}]})

(defn- canned-response [summary]
  (pr-str (select-keys summary [:task :subgoals :residue :evidence])))

;; ---------------------------------------------------------------------------
;; build-prompt
;; ---------------------------------------------------------------------------

(t/deftest build-prompt-returns-string
  (t/is (string? (comp/build-prompt (sample-summary)))))

(t/deftest build-prompt-embeds-summary
  (let [prompt (comp/build-prompt (sample-summary))
        summary-str (pr-str (sample-summary))]
    (t/is (clojure.string/includes? prompt summary-str))))

(t/deftest build-prompt-throws-on-non-map-summary
  (try
    (comp/build-prompt "not a map")
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

;; ---------------------------------------------------------------------------
;; compress with mock caller
;; ---------------------------------------------------------------------------

(t/deftest compress-returns-valid-envelope
  (let [result (comp/compress (sample-summary)
                              (comp/mock-call (fn [_] (canned-response (sample-summary))))
                              :model "test-model")]
    ;; validate-envelope returns the envelope on success, throws on failure
    (t/is (map? (envelope/validate-envelope result)))
    (t/is (= "test-model" (get-in result [:envelope/compressor :compressor/model])))))

(t/deftest compress-preserves-task-and-subgoals
  (let [result (comp/compress (sample-summary)
                              (comp/mock-call (fn [_] (canned-response (sample-summary))))
                              :model "test-model")]
    (t/is (= "t1" (get-in result [:envelope/task :task/id])))
    (t/is (= 1 (count (:envelope/subgoals result))))
    (t/is (= "sg1" (get-in result [:envelope/subgoals 0 :subgoal/id])))))

(t/deftest compress-preserves-residue-and-evidence
  (let [result (comp/compress (sample-summary)
                              (comp/mock-call (fn [_] (canned-response (sample-summary))))
                              :model "test-model")]
    (t/is (= 1 (count (:envelope/residue result))))
    (t/is (= 1 (count (:envelope/evidence result))))
    (t/is (= "must not break X"
             (get-in result [:envelope/residue 0 :residue/text])))))

(t/deftest compress-records-tokens-before-and-after
  (let [result (comp/compress (sample-summary)
                              (comp/mock-call (fn [_] (canned-response (sample-summary))))
                              :model "test-model"
                              :tokens-before 5000)]
    (t/is (pos? (:envelope/tokens-before result)))
    (t/is (pos? (:envelope/tokens-after result)))))

(t/deftest compress-throws-on-malformed-model-response
  (try
    (comp/compress (sample-summary)
                   (comp/mock-call (fn [_] "not valid edn"))
                   :model "test-model")
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/compression-invalid (:error/type (ex-data e)))))))

(t/deftest compress-uses-default-model-when-not-provided
  (let [result (comp/compress (sample-summary)
                              (comp/mock-call (fn [_] (canned-response (sample-summary)))))]
    (t/is (= "unknown" (get-in result [:envelope/compressor :compressor/model])))))

(t/deftest mock-call-returns-canned-response
  (let [caller (comp/mock-call (fn [prompt] (str "RESPONSE TO: " (count prompt) " chars")))
        response (caller "hello")]
    (t/is (clojure.string/starts-with? response "RESPONSE TO:"))))

(t/run-tests)