(ns evoclj.ptc.codemode-test
  "P7 slit: CodeModeOrchestrator fail-safe and :code parsing.

  - disabled / missing :ptc throws :ptc/not-enabled
  - enabled delegates to TraditionalOrchestrator (no code execution yet)
  - Traditional path unchanged when disabled
  - dialect parses :code once (INV-05) and ModelResponse carries it"
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.runtime.orchestrator :as orch]
            [evoclj.provider.dialect :as dialect]
            [evoclj.provider.request :as request]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- error-type [e]
  (:error/type (ex-data e)))

;; ---------------------------------------------------------------------------
;; Orchestrator fail-safe
;; ---------------------------------------------------------------------------

(deftest codemode-disabled-throws
  (testing "missing :ptc fails safe (false default)"
    (let [cm (orch/->CodeModeOrchestrator)
          executor {}]
      (try
        (orch/orchestrate cm executor nil nil {:intent/type :intent/model-call :payload {:model/id "test/model"}} [])
        (is false "should have thrown :ptc/not-enabled")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e)))
          (is (= "PTC is disabled" (.getMessage e)))))))
  (testing "explicit :ptc {:enabled? false} throws"
    (let [cm (orch/->CodeModeOrchestrator)
          executor {:ptc {:enabled? false}}]
      (try
        (orch/orchestrate cm executor nil nil {:intent/type :intent/model-call :payload {:model/id "test/model"}} [])
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e)))))))
  (testing "disabled also gates non-model intents (fail-safe is global)"
    (let [cm (orch/->CodeModeOrchestrator)
          executor {:ptc {:enabled? false}}]
      (try
        (orch/orchestrate cm executor nil nil {:intent/type :intent/other :payload {}} [])
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e))))))))

(deftest codemode-enabled-delegates
  (testing "enabled delegates to TraditionalOrchestrator — no :ptc/not-enabled"
    (let [cm (orch/->CodeModeOrchestrator)
          executor {:ptc {:enabled? true :language :sci-clojure}
                    :stores {:tool-catalog (atom []) :sqlite nil :cas nil}
                    :dispatch {:registry nil}}]
      ;; Stub the low-level host calls so TraditionalOrchestrator can run without a real DB.
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id "fake-id"})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "fake-ref")
                    evoclj.intent.dispatch/dispatch! (fn [_ _] {:result/status :ok
                                                                 :value {:model/output {:text "hello"}}
                                                                 :authorization {:decision :allow}})]
        (let [result (orch/orchestrate cm executor {:session/id "s1"} {:event/id "cause"} {:intent/type :intent/model-call
                                                                                            :payload {:model/id "fixture/echo"
                                                                                                      :messages [{:role :user :content "hi"}]
                                                                                                      :tools []
                                                                                                      :options {:max-tool-rounds 0}}} [])]
          (is (map? result) "enabled should delegate and return a step map")
          (is (contains? result :last-event))
          (is (contains? result :outputs)))))))

(deftest traditional-unchanged
  (testing "TraditionalOrchestrator still works without :ptc (no fail-safe gate)"
    (let [t (orch/->TraditionalOrchestrator)
          executor {:stores {:tool-catalog (atom []) :sqlite nil :cas nil}
                    :dispatch {:registry nil}}]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id "fake-id"})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "fake-ref")
                    evoclj.intent.dispatch/dispatch! (fn [_ _] {:result/status :ok
                                                                 :value {:model/output {:text "traditional"}}
                                                                 :authorization {:decision :allow}})]
        (let [result (orch/orchestrate t executor {:session/id "s1"} {:event/id "cause"} {:intent/type :intent/model-call
                                                                                            :payload {:model/id "fixture/echo"
                                                                                                      :messages [{:role :user :content "hi"}]
                                                                                                      :tools []
                                                                                                      :options {:max-tool-rounds 0}}} [])]
          (is (map? result))
          (is (= :ok (:outcome result))))))))

;; ---------------------------------------------------------------------------
;; :code block parsing (INV-05 single impl)
;; ---------------------------------------------------------------------------

(deftest code-block-parsing
  (testing "openai response with fenced clojure block yields :code"
    (let [raw {:choices [{:message {:content "hi\n```clojure\n(+ 1 2)\n```"}}]
               :usage {:prompt_tokens 1 :completion_tokens 2}}]
      (let [parsed (dialect/parse-openai-response {} raw)]
        (is (= {:language :clojure :source "(+ 1 2)"} (:code parsed)))
        (is (= "hi\n```clojure\n(+ 1 2)\n```" (get-in parsed [:model/output :text]))))))
  (testing "openai blank language defaults to :clojure and trims source"
    (let [raw {:choices [{:message {:content "```\n  hello  \n```"}}]
               :usage {}}]
      (is (= {:language :clojure :source "hello"} (:code (dialect/parse-openai-response {} raw))))))
  (testing "openai no block yields no :code"
    (let [raw {:choices [{:message {:content "just text"}}] :usage {}}]
      (is (nil? (:code (dialect/parse-openai-response {} raw))))))
  (testing "anthropic response with fenced block yields :code"
    (let [raw {:content [{:type "text" :text "hi\n```python\nprint(1)\n```"}]
               :usage {:input_tokens 1 :output_tokens 2}}]
      (is (= {:language :python :source "print(1)"} (:code (dialect/parse-anthropic-response raw))))))
  (testing "anthropic no block yields no :code"
    (let [raw {:content [{:type "text" :text "no code here"}] :usage {}}]
      (is (nil? (:code (dialect/parse-anthropic-response raw))))))
  (testing "ModelResponse malli validates :code"
    (is (m/validate request/ModelResponse {:text "hi" :code {:language :clojure :source "x"}}))
    (is (m/validate request/ModelResponse {:text "hi"}))
    (is (not (m/validate request/ModelResponse {:text "hi" :code {:language "oops" :source 123}})))))

(deftest code-carried-but-not-executed
  (testing "parse preserves :code without execution (pure EDN)"
    (let [raw {:choices [{:message {:content "```clojure\n(+ 1 2)\n```"}}] :usage {}}
          parsed (dialect/parse-openai-response {} raw)]
      (is (= "(+ 1 2)" (get-in parsed [:code :source])))
      ;; Ensure no eval happened: source is still a string, not a number.
      (is (string? (get-in parsed [:code :source]))))))
