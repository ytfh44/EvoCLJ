(ns evoclj.ptc.codemode-test
  "P7 slit: CodeModeOrchestrator fail-safe and :code parsing.

  - disabled / missing :ptc throws :ptc/not-enabled
  - enabled delegates to TraditionalOrchestrator (no code execution yet)
  - Traditional path unchanged when disabled
  - dialect parses :code only from code_execution tool_use (INV-05) and ModelResponse carries it"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [cheshire.core :as json]
            [evoclj.runtime.orchestrator :as orch]
            [evoclj.provider.dialect :as dialect]
            [evoclj.provider.request :as request]
            [evoclj.sci.computation :as computation]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- error-type [e]
  (:error/type (ex-data e)))

(defn- raw-openai-with-code
  "Helper: raw OpenAI response with code_execution tool_call carrying code-str.
  language defaults to :clojure when not supplied. source-key controls whether
  the tool argument uses :code or :source."
  ([code-str] (raw-openai-with-code code-str {}))
  ([code-str {:keys [language source-key] :or {language "clojure" source-key :code}}]
   {:choices [{:message {:content "hi"
                         :tool_calls [{:id "call_1"
                                       :type "function"
                                       :function {:name "code_execution"
                                                  :arguments (json/generate-string
                                                              (cond-> {source-key code-str}
                                                                language (assoc :language language)))}}]}}]
    :usage {:prompt_tokens 1 :completion_tokens 2}}))

(defn- raw-anthropic-with-code
  "Helper: raw Anthropic response with code_execution tool_use carrying code-str."
  ([code-str] (raw-anthropic-with-code code-str {}))
  ([code-str {:keys [language source-key] :or {language "clojure" source-key :code}}]
   {:content [{:type "tool_use"
               :id "toolu_1"
               :name "code_execution"
               :input (cond-> {source-key code-str}
                        language (assoc :language language))}]
    :usage {:input_tokens 1 :output_tokens 2}}))

;; ---------------------------------------------------------------------------
;; Orchestrator fail-safe
;; ---------------------------------------------------------------------------

(deftest codemode-disabled-throws
  (testing "missing :ptc fails safe (false default)"
    (let [comp (computation/make-computation {})
          cm (orch/->CodeModeOrchestrator comp)
          executor {}]
      (try
        (orch/orchestrate cm executor nil nil {:intent/type :intent/model-call :payload {:model/id "test/model"}} [])
        (is false "should have thrown :ptc/not-enabled")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e)))
          (is (= "PTC is disabled" (.getMessage e)))))))
  (testing "explicit :ptc {:enabled? false} throws"
    (let [comp (computation/make-computation {})
          cm (orch/->CodeModeOrchestrator comp)
          executor {:ptc {:enabled? false}}]
      (try
        (orch/orchestrate cm executor nil nil {:intent/type :intent/model-call :payload {:model/id "test/model"}} [])
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e)))))))
  (testing "disabled also gates non-model intents (fail-safe is global)"
    (let [comp (computation/make-computation {})
          cm (orch/->CodeModeOrchestrator comp)
          executor {:ptc {:enabled? false}}]
      (try
        (orch/orchestrate cm executor nil nil {:intent/type :intent/other :payload {}} [])
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e))))))))

(deftest codemode-enabled-delegates
  (testing "enabled delegates to TraditionalOrchestrator — no :ptc/not-enabled"
    (let [comp (computation/make-computation {})
          cm (orch/->CodeModeOrchestrator comp)
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
;; :code block parsing (INV-05 single impl — tool_use only, fence != :code)
;; ---------------------------------------------------------------------------

(deftest code-block-parsing
  (testing "fenced text does NOT yield :code for openai"
    (let [raw {:choices [{:message {:content "hi\n```clojure\n(+ 1 2)\n```"}}]
               :usage {:prompt_tokens 1 :completion_tokens 2}}]
      (is (nil? (:code (dialect/parse-openai-response {} raw)))
          "fenced block in text must not produce :code; only code_execution tool_use does")))
  (testing "openai no block yields no :code"
    (let [raw {:choices [{:message {:content "just text"}}] :usage {}}]
      (is (nil? (:code (dialect/parse-openai-response {} raw))))))
  (testing "openai code_execution tool_use yields :code"
    (let [raw {:choices [{:message {:content "hi"
                                    :tool_calls [{:id "call_1"
                                                  :type "function"
                                                  :function {:name "code_execution"
                                                             :arguments "{\"code\":\"(+ 1 2)\"}"}}]}}]
               :usage {:prompt_tokens 1 :completion_tokens 2}}]
      (is (= {:language :clojure :source "(+ 1 2)"} (:code (dialect/parse-openai-response {} raw))))))
  (testing "openai code_execution with :source/:language variant yields :code"
    (let [raw {:choices [{:message {:content "hi"
                                    :tool_calls [{:id "call_1"
                                                  :type "function"
                                                  :function {:name "code_execution"
                                                             :arguments "{\"source\":\"print(1)\",\"language\":\"python\"}"}}]}}]
               :usage {}}]
      (is (= {:language :python :source "print(1)"} (:code (dialect/parse-openai-response {} raw))))))
  (testing "openai blank language defaults to :clojure and trims source via tool_use"
    (let [raw {:choices [{:message {:content "hi"
                                    :tool_calls [{:id "call_1"
                                                  :type "function"
                                                  :function {:name "code_execution"
                                                             :arguments "{\"code\":\"  hello  \"}"}}]}}]
               :usage {}}]
      (is (= {:language :clojure :source "hello"} (:code (dialect/parse-openai-response {} raw))))))
  (testing "openai non-code tool does not yield :code"
    (let [raw {:choices [{:message {:content "hi"
                                    :tool_calls [{:id "call_1"
                                                  :type "function"
                                                  :function {:name "web_search"
                                                             :arguments "{\"query\":\"hi\"}"}}]}}]
               :usage {}}]
      (is (nil? (:code (dialect/parse-openai-response {} raw))))))
  (testing "fenced text does NOT yield :code for anthropic"
    (let [raw {:content [{:type "text" :text "hi\n```python\nprint(1)\n```"}]
               :usage {:input_tokens 1 :output_tokens 2}}]
      (is (nil? (:code (dialect/parse-anthropic-response raw)))
          "fenced block in text must not produce :code; only code_execution tool_use does")))
  (testing "anthropic no block yields no :code"
    (let [raw {:content [{:type "text" :text "no code here"}] :usage {}}]
      (is (nil? (:code (dialect/parse-anthropic-response raw))))))
  (testing "anthropic code_execution tool_use yields :code"
    (let [raw {:content [{:type "tool_use" :id "toolu_1" :name "code_execution" :input {:code "(+ 1 2)"}}]
               :usage {:input_tokens 1 :output_tokens 2}}]
      (is (= {:language :clojure :source "(+ 1 2)"} (:code (dialect/parse-anthropic-response raw))))))
  (testing "anthropic code_execution with :source/:language variant yields :code"
    (let [raw {:content [{:type "tool_use" :id "toolu_1" :name "code_execution" :input {:language "python" :source "print(1)"}}]
               :usage {:input_tokens 1 :output_tokens 2}}]
      (is (= {:language :python :source "print(1)"} (:code (dialect/parse-anthropic-response raw))))))
  (testing "anthropic non-code tool does not yield :code"
    (let [raw {:content [{:type "tool_use" :id "toolu_1" :name "other_tool" :input {:code "(+ 1 2)"}}]
               :usage {}}]
      (is (nil? (:code (dialect/parse-anthropic-response raw))))))
  (testing "ModelResponse malli validates :code"
    (is (m/validate request/ModelResponse {:text "hi" :code {:language :clojure :source "x"}}))
    (is (m/validate request/ModelResponse {:text "hi"}))
    (is (not (m/validate request/ModelResponse {:text "hi" :code {:language "oops" :source 123}})))))

(deftest code-carried-but-not-executed
  (testing "parse preserves :code without execution (pure EDN) via tool_use"
    (let [raw {:choices [{:message {:content "hi"
                                    :tool_calls [{:id "call_1"
                                                  :type "function"
                                                  :function {:name "code_execution"
                                                             :arguments "{\"code\":\"(+ 1 2)\"}"}}]}}]
               :usage {}}
          parsed (dialect/parse-openai-response {} raw)]
      (is (= "(+ 1 2)" (get-in parsed [:code :source])))
      ;; Ensure no eval happened: source is still a string, not a number.
      (is (string? (get-in parsed [:code :source])))))
  (testing "anthropic code_execution also carried as string without execution"
    (let [raw {:content [{:type "tool_use" :id "toolu_1" :name "code_execution" :input {:code "(+ 1 2)"}}]
               :usage {}}
          parsed (dialect/parse-anthropic-response raw)]
      (is (= "(+ 1 2)" (get-in parsed [:code :source])))
      (is (string? (get-in parsed [:code :source]))))))
