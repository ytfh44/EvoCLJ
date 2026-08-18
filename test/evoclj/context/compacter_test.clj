(ns evoclj.context.compacter-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [evoclj.context.compacter :as compacter]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.registry :as registry]
            [evoclj.context.footer :as footer]
            [evoclj.context.crosscheck :as crosscheck]
            [evoclj.context.eval :as eval]))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defn mock-call [_]
  ;; The structured path expects ONLY :residue and :evidence in the response.
  (pr-str {:residue [{:residue/id 1 :residue/kind :constraint
                       :residue/text "must not break X"
                       :residue/source "user"
                       :residue/at "2026-08-17T00:00:00Z"}]
           :evidence [{:evidence/id 1 :evidence/kind :observation
                        :evidence/text "src/evoclj/context/compacter.clj"
                        :evidence/at "2026-08-17T00:00:00Z"}]}))

(defn mock-call-with-residue [_]
  (pr-str {:residue [{:residue/id 1 :residue/kind :decision
                       :residue/text "chose approach A"
                       :residue/source "user"
                       :residue/at "2026-08-17T00:00:00Z"}]
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
    (t/is (not (nil? (:envelope/version (:envelope result)))))))

(t/deftest default-compacter-compresses-when-threshold-exceeded
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; 5000 chars / 4 = 1250 tokens, threshold 1000 => compress
        long-context (apply str (repeat 5000 "x"))
        result (compacter/run long-context c {:token-threshold 1000})]
    (t/is (map? (:envelope result)))
    (t/is (string? (:footer result)))
    (t/is (not (str/blank? (:footer result))))
    ;; The structured path should have populated residue/evidence from the LLM
    (t/is (pos? (count (:envelope/residue (:envelope result)))))))

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

(t/deftest default-compacter-retains-previous-residue
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; 5000 chars / 4 = 1250 tokens, threshold 1000 => compress
        long-context (apply str (repeat 5000 "x"))
        previous-envelope (envelope/make-envelope
                            {:task {:task/id "prev" :task/status :completed
                                    :task/description "previous"}
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
                             :compressor {:compressor/model "prev"
                                          :compressor/prompt "p"}})
        result (compacter/run long-context c {:token-threshold 1000
                                               :previous-envelope previous-envelope})]
    ;; The new envelope should contain both the previous residue and the new LLM residue
    (let [residue-texts (map :residue/text (:envelope/residue (:envelope result)))]
      (t/is (some #{"old constraint"} residue-texts))
      (t/is (some #{"must not break X"} residue-texts)))))

(t/deftest default-compacter-includes-archiver-reports-in-footer
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; 5000 chars / 4 = 1250 tokens, threshold 1000 => compress
        long-context (apply str (repeat 5000 "x"))
        ;; Register a fake archiver
        archiver (reify registry/CompacterArchive
                   (archive-manifest [_]
                     {:archiver/id :test/archiver
                      :archiver/description "test archiver"
                      :archiver/serialized {:state :active}}))
        _ (registry/register! archiver)
        result (compacter/run long-context c {:token-threshold 1000})]
    (t/is (str/includes? (:footer result) "test archiver"))
    (registry/clear-registry!)))

(t/deftest default-compacter-crosscheck-records-mismatches
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; 5000 chars / 4 = 1250 tokens, threshold 1000 => compress
        long-context (apply str (repeat 5000 "x"))
        structured-sections {:tasks [{:task/id "t1" :task/status :completed
                                       :task/description "authoritative"}]
                             :subgoals []}
        result (compacter/run long-context c {:token-threshold 1000
                                               :structured-sections structured-sections})]
    ;; The envelope task should be corrected to match authoritative state
    (t/is (= "t1" (get-in (:envelope result) [:envelope/task :task/id])))
    (t/is (= :completed (get-in (:envelope result) [:envelope/task :task/status])))
    ;; mismatches should be empty because the model didn't return :task
    (t/is (vector? (:mismatches result)))))

(t/deftest default-compacter-eval-summary-when-requested
  (let [c (compacter/->DefaultCompacter mock-call)
        ;; Use a context that shares words with the envelope's task/description
        ;; and the mock residue/evidence so heuristics score well.
        long-context (str (apply str (repeat 200 "compression "))
                          (apply str (repeat 200 "must not break X "))
                          (apply str (repeat 200 "src/evoclj/context/loop.clj ")))
        result (compacter/run long-context c {:token-threshold 100
                                               :eval? true})]
    (t/is (map? (:eval result)))
    (t/is (keyword? (get-in (:eval result) [:eval/overall-status])))))

(t/run-tests)
