(ns evoclj.context.integration-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [evoclj.context.compression.compacter :as compacter]
            [evoclj.context.compression.apply :as apply]
            [evoclj.context.compression.envelope :as envelope]
            [evoclj.context.loop :as loop]
            [evoclj.context.compression.eval :as eval]
            [evoclj.context.compression.archivers :as archivers]
            [evoclj.context.registry :as registry]
            [evoclj.context.token-estimator :as token-estimator]
            [evoclj.context.trigger :as trigger]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- make-context [n]
  (str/join "\n" (mapv #(str "Line " % ": some content here") (range n))))

(defn- make-compacter []
  (compacter/->DefaultCompacter
    (fn [_]
      (pr-str {:task {:task/id "integration-task" :task/status :in-progress
                      :task/description "Integration test compression"}
               :subgoals []
               :residue []
               :evidence []}))))

;; ---------------------------------------------------------------------------
;; End-to-end loop tests
;; ---------------------------------------------------------------------------

(t/deftest full-loop-preserves-information
  (let [context (make-context 100)
        comp (make-compacter)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})
        applied (:context result)
        env (:envelope result)
        footer (:footer result)]
    ;; Applied context is envelope + fresh tail (no footer)
    (t/is (str/starts-with? applied "#:envelope{"))
    ;; Footer contains the compression marker
    (t/is (str/includes? footer "[CONTEXT COMPRESSION]"))
    ;; Envelope shows compression happened
    (t/is (pos? (:envelope/tokens-before env)))
    (t/is (< (:envelope/tokens-after env) (:envelope/tokens-before env)))))

(t/deftest recompress-idempotent
  (let [context (make-context 50)
        comp (make-compacter)
        result1 (loop/recompress! context comp
                   {:model "test-model"
                    :token-threshold 10
                    :marker "[CONTEXT COMPRESSION]"})
        applied1 (:context result1)
        result2 (loop/recompress! applied1 comp
                   {:model "test-model"
                    :token-threshold 10
                    :marker "[CONTEXT COMPRESSION]"})
        applied2 (:context result2)]
    ;; Both should start with envelope and have footer with marker
    (t/is (str/starts-with? applied1 "#:envelope{"))
    (t/is (str/starts-with? applied2 "#:envelope{"))
    (t/is (str/includes? (:footer result1) "[CONTEXT COMPRESSION]"))
    (t/is (str/includes? (:footer result2) "[CONTEXT COMPRESSION]"))
    ;; The second compression should not grow unboundedly
    (t/is (< (count applied2) (* 2 (count applied1))))))

(t/deftest compress-and-apply-completes
  (let [context (make-context 30)
        comp (make-compacter)
        result (loop/compress-and-apply context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})]
    (t/is (string? result))
    (t/is (str/starts-with? result "#:envelope{"))))

(t/deftest eval-scores-on-compressed-context
  (let [context (make-context 100)
        comp (make-compacter)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})
        env (:envelope result)
        eval-records [(eval/eval-retention-score env context 0.9)
                      (eval/eval-regression-score env context 0.9)
                      (eval/eval-hallucination-score env context 0.9)]
        summary (eval/eval-summary eval-records)]
    (t/is (= 3 (count eval-records)))
    (t/is (keyword? (:eval/overall-status summary)))))

(t/deftest archiver-reports-included-in-footer
  (registry/clear-registry!)
  (let [archiver (archivers/todo-archiver
                   [{:todo/id :t1 :todo/status :completed :todo/description "done"}])
        _ (registry/register! archiver)
        context (make-context 50)
        comp (make-compacter)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})
        applied (:context result)
        footer (:footer result)]
    ;; Applied context is just envelope + fresh tail
    (t/is (str/starts-with? applied "#:envelope{"))
    ;; Archiver reports are in the footer
    (t/is (str/includes? footer "[TOOL ARCHIVES]"))
    (t/is (str/includes? footer "Todo list snapshot"))
    (registry/clear-registry!)))

(t/deftest multiple-iterations-complete
  (let [context (make-context 100)
        comp (make-compacter)
        result (loop/compress-and-apply context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"
                  :iterations 3})]
    (t/is (string? result))
    (t/is (str/starts-with? result "#:envelope{"))))

(t/deftest context-can-be-read-back
  (let [context (make-context 20)
        comp (make-compacter)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})
        applied (:context result)]
    ;; The applied context should be readable as a string
    (t/is (string? applied))
    (t/is (pos? (count applied)))))

(t/deftest envelope-roundtrip
  (let [context (make-context 30)
        comp (make-compacter)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})
        env (:envelope result)
        _ (envelope/validate-envelope env)]
    (t/is (map? env))
    (t/is (number? (:envelope/tokens-before env)))
    (t/is (number? (:envelope/tokens-after env)))))

(t/deftest footer-contains-compression-marker
  (let [context (make-context 40)
        comp (make-compacter)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})
        applied (:context result)
        footer (:footer result)]
    ;; Applied context is envelope + fresh tail (no footer marker)
    (t/is (str/starts-with? applied "#:envelope{"))
    ;; Footer contains the compression marker
    (t/is (str/includes? footer "[CONTEXT COMPRESSION]"))))

(t/deftest compression-loop-with-eval
  (let [context (make-context 100)
        comp (make-compacter)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})
        env (:envelope result)
        eval-records [(eval/eval-retention-score env context 0.9)
                      (eval/eval-regression-score env context 0.9)
                      (eval/eval-hallucination-score env context 0.9)]
        summary (eval/eval-summary eval-records)]
    (t/is (= 3 (count eval-records)))
    (t/is (keyword? (:eval/overall-status summary)))
    ;; Context is envelope + fresh tail; footer has marker
    (t/is (str/starts-with? (:context result) "#:envelope{"))
    (t/is (str/includes? (:footer result) "[CONTEXT COMPRESSION]"))))

(t/deftest cooldown-prevents-recompression
  (let [context (make-context 50)
        comp (make-compacter)
        result1 (loop/recompress! context comp
                  {:model "test-model"
                   :token-threshold 10
                   :cooldown-tokens 1000
                   :marker "[CONTEXT COMPRESSION]"})
        savings (or (get-in result1 [:trigger/last-savings])
                    (- (count context) (count (:context result1))))
        result2 (loop/recompress! (:context result1) comp
                  {:model "test-model"
                   :token-threshold 10
                   :cooldown-tokens 1000
                   :trigger/last-savings savings
                   :marker "[CONTEXT COMPRESSION]"})]
    ;; Second compression should be noop because savings < cooldown-tokens
    (t/is (= (:context result1) (:context result2)))))

(t/deftest custom-token-estimator-affects-trigger
  (let [context (str (apply str (repeat 100 "abc ")))
        ;; With char-count estimator (4 chars/token): 400 chars / 4 = 100 tokens
        ;; With word-count estimator: 100 words = 100 tokens
        char-est (token-estimator/char-count-estimator 4)
        word-est (token-estimator/cl100k-estimator)
        trigger-config-char {:trigger/token-threshold 50
                             :trigger/token-estimator char-est}
        trigger-config-word {:trigger/token-threshold 50
                             :trigger/token-estimator word-est}
        result-char (trigger/should-compress? context trigger-config-char)
        result-word (trigger/should-compress? context trigger-config-word)]
    (t/is (= (:trigger/token-count result-char) 100))
    (t/is (= (:trigger/token-count result-word) 100))
    (t/is (:trigger/compressed? result-char))
    (t/is (:trigger/compressed? result-word))))

(t/deftest service-usage-affects-tokens-after
  (let [comp (compacter/->DefaultCompacter
               (fn [_]
                 {:text (pr-str {:residue [] :evidence []})
                  :usage {:input-tokens 123 :output-tokens 456}}))
        context (make-context 100)
        result (loop/recompress! context comp
                 {:model "test-model"
                  :token-threshold 10
                  :marker "[CONTEXT COMPRESSION]"})]
    (t/is (= 456 (:envelope/tokens-after (:envelope result))))))

(t/run-tests)
