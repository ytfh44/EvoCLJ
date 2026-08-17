(ns evoclj.context.compacter-test
  (:require [clojure.test :as t]
            [evoclj.context.compacter :as compacter]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.registry :as registry]
            [evoclj.context.footer :as footer]))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defn mock-call [_]
  (pr-str {:task {:task/id "t1" :task/status :completed :task/description "done"}
           :subgoals []
           :residue []
           :evidence []}))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(t/deftest default-compacter-returns-envelope-and-footer
  (let [c (compacter/->DefaultCompacter mock-call)
        short-context "short"
        result (compacter/run short-context c {:token-threshold 10000})]
    (t/is (map? (:envelope result)))
    (t/is (string? (:footer result)))
    (t/is (envelope/valid-envelope? (:envelope result)))))

(t/deftest default-compacter-compresses-when-threshold-exceeded
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; 5000 chars / 4 = 1250 tokens, threshold 1000 => compress
        long-context (apply str (repeat 1000 "x"))
        result (compacter/run long-context c {:token-threshold 1000})]
    (t/is (envelope/valid-envelope? (:envelope result)))
    ;; footer should mention the task id from the envelope
    (t/is (string? (:footer result)))
    (t/is (not (str/blank? (:footer result))))))

(t/deftest run-with-custom-compacter
  (let [custom (reify compacter/Compacter
                 (compress [_ context opts]
                   {:envelope (envelope/make-envelope
                                {:task {:task/id "custom" :task/status :completed
                                        :task/description "custom"}
                                 :subgoals []
                                 :residue []
                                 :evidence []
                                 :version 1
                                 :created-at "2026-08-17T00:00:00Z"
                                 :window {:window/from 0 :window/to 10}
                                 :tokens-before 100
                                 :tokens-after 50
                                 :compressor {:compressor/model "custom"
                                              :compressor/prompt "p"}})
                    :footer "custom footer"}))
        result (compacter/run "ctx" custom {})]
    (t/is (= "custom" (get-in (:envelope result) [:envelope/task :task/id])))
    (t/is (= "custom footer" (:footer result)))))

(t/deftest no-compression-returns-original-token-count
  (let [c (compacter/->DefaultCompacter mock-call)
        ctx "hi"
        result (compacter/run ctx c {:token-threshold 10000})]
    (t/is (= (:envelope/tokens-before (:envelope result))
             (:envelope/tokens-after (:envelope result))))))

(t/deftest compacter-respects-model-option
  (let [c (compacter/->DefaultCompacter mock-call)
        long-context (apply str (repeat 1000 "x"))
        result (compacter/run long-context c {:token-threshold 1000 :model "gpt-4"})]
    (t/is (= "gpt-4"
             (get-in (:envelope result) [:envelope/compressor :compressor/model])))))

(t/run-tests)
