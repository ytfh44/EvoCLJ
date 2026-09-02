(ns evoclj.context.loop-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [evoclj.context.loop :as loop]
            [evoclj.context.compression.envelope :as envelope]
            [evoclj.context.compression.compacter :as compacter]
            [evoclj.context.compression.apply :as apply]
            [evoclj.context.compression.idempotency :as idempotency]
            [evoclj.context.registry :as registry]))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defn mock-call [_]
  (pr-str {:residue [{:residue/id 1 :residue/kind :constraint
                       :residue/text "must not break X"
                       :residue/source "user"
                       :residue/at "2026-08-17T00:00:00Z"}]
           :evidence [{:evidence/id 1 :evidence/kind :observation
                        :evidence/text "src/evoclj/context/loop.clj"
                        :evidence/at "2026-08-17T00:00:00Z"}]}))

;; ---------------------------------------------------------------------------
;; extract-envelope
;; ---------------------------------------------------------------------------

(t/deftest extract-envelope-returns-nil-when-no-envelope
  (let [result (loop/extract-envelope "plain text without envelope")]
    (t/is (nil? (:envelope result)))
    (t/is (= "plain text without envelope" (:fresh-tail result)))))

(t/deftest extract-envelope-parses-valid-envelope-prefix
  (let [env (envelope/make-envelope
              {:task {:task/id "t1" :task/status :in-progress :task/description "task"}
               :subgoals []
               :residue []
               :evidence []
               :version 1
               :created-at "2026-08-17T00:00:00Z"
               :window {:window/from 0 :window/to 10}
               :tokens-before 100
               :tokens-after 50
               :compressor {:compressor/model "test" :compressor/prompt "p"}})
        context-str (str (envelope/envelope->edn env) "\n\n[CONTEXT COMPRESSION]\n--- Task ---\nt1 [:in-progress] task\n\n<fresh tail here>")
        result (loop/extract-envelope context-str)]
    (t/is (map? (:envelope result)))
    (t/is (= 1 (:envelope/version (:envelope result))))
    (t/is (str/includes? (:fresh-tail result) "<fresh tail here>"))))

(t/deftest extract-envelope-returns-full-input-on-invalid-edn
  (let [result (loop/extract-envelope "{:not a valid edn")]
    (t/is (nil? (:envelope result)))
    (t/is (= "{:not a valid edn" (:fresh-tail result)))))

;; ---------------------------------------------------------------------------
;; extract-fresh-tail (via full context)
;; ---------------------------------------------------------------------------

(t/deftest extract-fresh-tail-strips-footer
  (let [env (envelope/make-envelope
              {:task {:task/id "t1" :task/status :in-progress :task/description "task"}
               :subgoals []
               :residue []
               :evidence []
               :version 1
               :created-at "2026-08-17T00:00:00Z"
               :window {:window/from 0 :window/to 10}
               :tokens-before 100
               :tokens-after 50
               :compressor {:compressor/model "test" :compressor/prompt "p"}})
        context-str (str (envelope/envelope->edn env) "\n\n[CONTEXT COMPRESSION]\n--- Task ---\nt1\n\n<fresh tail>")]
    (t/is (= "<fresh tail>" (loop/extract-fresh-tail context-str)))))

(t/deftest extract-fresh-tail-returns-unchanged-when-no-footer
  (let [context-str "plain text without envelope"]
    (t/is (= "plain text without envelope" (loop/extract-fresh-tail context-str)))))

(t/deftest extract-fresh-tail-returns-empty-when-only-footer
  (let [env (envelope/make-envelope
              {:task {:task/id "t1" :task/status :in-progress :task/description "task"}
               :subgoals []
               :residue []
               :evidence []
               :version 1
               :created-at "2026-08-17T00:00:00Z"
               :window {:window/from 0 :window/to 10}
               :tokens-before 100
               :tokens-after 50
               :compressor {:compressor/model "test" :compressor/prompt "p"}})
        context-str (str (envelope/envelope->edn env) "\n\n[CONTEXT COMPRESSION]\n--- Task ---\nt1\n\n")]
    (t/is (= "" (loop/extract-fresh-tail context-str)))))

;; ---------------------------------------------------------------------------
;; recompress!
;; ---------------------------------------------------------------------------

(t/deftest recompress-compresses-fresh-context
  (let [c (compacter/->DefaultCompacter mock-call)
        context-str "some fresh context that needs compression"
        result (loop/recompress! context-str c {:token-threshold 1})]
    (t/is (map? (:envelope result)))
    (t/is (string? (:footer result)))
    (t/is (string? (:context result)))
    ;; Applied context is envelope + fresh tail (no footer marker)
    (t/is (str/starts-with? (:context result) "#:envelope{"))
    ;; Footer contains the compression marker
    (t/is (str/includes? (:footer result) "[CONTEXT COMPRESSION]"))))

(t/deftest recompress-preserves-previous-envelope-on-second-pass
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; First compression
        context1 "first batch of context"
        result1 (loop/recompress! context1 c {:token-threshold 1})
        context2 (:context result1)
        ;; Second compression: append more context to the fresh tail
        context2-with-more (str context2 "\n\nmore context arrived")
        result2 (loop/recompress! context2-with-more c {:token-threshold 1})]
    (t/is (map? (:envelope result2)))
    ;; The merged envelope should contain residue from both compressions
    (let [residue-texts (map :residue/text (:envelope/residue (:envelope result2)))]
      (t/is (some #{"must not break X"} residue-texts)))))

(t/deftest recompress-is-idempotent-when-no-new-content
  (let [c (compacter/->DefaultCompacter mock-call)
        context-str (str "first context\n\nsecond context")
        result1 (loop/recompress! context-str c {:token-threshold 10})
        result2 (loop/recompress! (:context result1) c {:token-threshold 10})]
    ;; Both should produce valid envelopes
    (t/is (map? (:envelope result1)))
    (t/is (map? (:envelope result2)))))

(t/deftest compress-and-apply-returns-context-string
  (let [c (compacter/->DefaultCompacter mock-call)
        context-str "some context"
        result (loop/compress-and-apply context-str c {:token-threshold 10})]
    (t/is (string? result))
    ;; When no compression needed, returns original context unchanged
    (t/is (= "some context" result))))

(t/deftest loop-roundtrip-preserves-residue
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; Build an initial context with a known residue
        initial-envelope (envelope/make-envelope
                           {:task {:task/id "t1" :task/status :completed :task/description "task"}
                            :subgoals []
                            :residue [{:residue/id 1 :residue/kind :constraint
                                        :residue/text "old constraint"
                                        :residue/source "user"
                                        :residue/at "2026-08-17T00:00:00Z"}]
                            :evidence []
                            :version 1
                            :created-at "2026-08-17T00:00:00Z"
                            :window {:window/from 0 :window/to 10}
                            :tokens-before 100
                            :tokens-after 50
                            :compressor {:compressor/model "test" :compressor/prompt "p"}})
        footer (str "[CONTEXT COMPRESSION]\nTask: t1\n\n")
        initial-context (str (envelope/envelope->edn initial-envelope) "\n" footer "new tail content")
        result (loop/recompress! initial-context c {:token-threshold 10})]
    (let [residue-texts (map :residue/text (:envelope/residue (:envelope result)))]
      (t/is (some #{"old constraint"} residue-texts)))))

(t/run-tests)
