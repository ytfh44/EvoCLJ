(ns evoclj.provider.dialect-test
  "Tests for the pure EDN dialect layer (evoclj.provider.dialect):
  request-side extra params (reasoning effort/toggle, server-side
  search modes, extra-params), response-side parsing (DeepSeek
  interleaved reasoning_content extraction, Anthropic content
  blocks), and cost estimation from the models.dev pricing table."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.provider.dialect :as d]))

(def ^:private deepseek-dialect
  {:interleaved :reasoning_content
   :reasoning-options [{:type :toggle}
                       {:type :effort :values ["low" "high" "max"]}]
   :server-side-search :off
   :extra-params {}})

(def ^:private searchable-dialect
  {:interleaved :none
   :reasoning-options []
   :server-side-search :web-search-options
   :extra-params {:vendor_flag true}})

(deftest request-extra-reasoning
  (testing "effort reasoning emits reasoning_effort"
    (is (= {:reasoning_effort "high"}
           (d/openai-request-extra deepseek-dialect
                                   {:reasoning {:mode :effort :level "high"}}))))
  (testing "toggle reasoning without a dialect param is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"does not support"
                          (d/openai-request-extra deepseek-dialect
                                                  {:reasoning {:mode :toggle}}))))
  (testing "toggle reasoning with a dialect param emits it"
    (is (= {:thinking true}
           (d/openai-request-extra
            (assoc deepseek-dialect :reasoning-toggle-param :thinking)
            {:reasoning {:mode :toggle :enabled true}})))))

(deftest request-extra-server-side-search
  (testing "web-search-options mode emits web-search-options"
    (let [extra (d/openai-request-extra searchable-dialect
                                        {:server-side-search true})]
      (is (contains? extra :web-search-options))
      (is (= :medium (get-in extra [:web-search-options :search_context_size])))))
  (testing "web-search-tool mode appends a web_search tool"
    (let [dialect (assoc deepseek-dialect :server-side-search :web-search-tool)
          extra (d/openai-request-extra dialect
                                        {:server-side-search true
                                         :tools [{:type "function"
                                                  :function {:name "f"}}]})]
      (is (= #{"web_search" "function"}
             (set (map :type (:tools extra)))))))
  (testing "off dialect ignores the request flag"
    (is (= {} (d/openai-request-extra deepseek-dialect
                                      {:server-side-search true})))))

(deftest request-extra-params
  (testing "dialect extra-params merge under and win"
    (is (= {:vendor_flag true :reasoning_effort "low"}
           (d/openai-request-extra (assoc searchable-dialect
                                          :extra-params {:vendor_flag true :reasoning_effort "x"})
                                   {:reasoning {:mode :effort :level "low"}})))))

(deftest reasoning-mode-selection
  (testing "effort wins when both exist; values are collected"
    (is (= {:mode :effort :levels ["high" "low" "max"]}
           (d/reasoning-options->mode deepseek-dialect))))
  (testing "no options yields nil"
    (is (nil? (d/reasoning-options->mode {:interleaved :none}))))
  (testing "toggle-only yields toggle"
    (is (= {:mode :toggle}
           (d/reasoning-options->mode {:reasoning-options [{:type :toggle}]})))))

(deftest parse-openai-response-text-and-reasoning
  (testing "DeepSeek interleaved reasoning is extracted separately"
    (let [resp {:id "x"
               :choices [{:index 0
                          :message {:role "assistant"
                                    :content "final answer"
                                    :reasoning_content "think step by step"}
                          :finish_reason "stop"}]
               :usage {:prompt_tokens 12 :completion_tokens 8 :total_tokens 20}}
          parsed (d/parse-openai-response deepseek-dialect resp)]
      (is (= {:text "final answer" :reasoning "think step by step"}
             (:model/output parsed)))
      (is (= {:input-tokens 12 :output-tokens 8} (:usage parsed))))
  (testing "no interleaved dialect keeps reasoning out of the output"
    (let [resp {:choices [{:message {:content "plain"}}]}
          parsed (d/parse-openai-response {:interleaved :none} resp)]
      (is (= {:text "plain"} (:model/output parsed)))))
  (testing "missing choices is a typed output error"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"no usable first choice"
                          (d/parse-openai-response deepseek-dialect
                                                   {:choices []})))))

(deftest parse-anthropic-response-blocks
  (testing "text blocks concatenate; usage maps tokens"
    (let [resp {:content [{:type "text" :text "Hello "}
                          {:type "tool_use" :id "t1" :name "x"}
                          {:type "text" :text "world"}],
               :usage {:input_tokens 5 :output_tokens 7}}
          parsed (d/parse-anthropic-response resp)]
      (is (= {:text "Hello world"} (:model/output parsed)))
      (is (= {:input-tokens 5 :output-tokens 7} (:usage parsed))))))

(deftest cost-estimation
  (testing "flat rates"
    (is (= 1.0 (d/estimate-cost {:input 0.1 :output 0.2}
                                {:input-tokens 5 :output-tokens 2.5})))
    ;; 5*0.1 + 2.5*0.2 = 0.5 + 0.5 = 1.0
    )
  (testing "DeepSeek-style reasoning rate applies to output tokens"
    (is (= 0.56 (d/estimate-cost {:input 0.14 :output 0.28 :reasoning 0.28}
                                 {:input-tokens 1 :output-tokens 1.5})))
    ;; 1*0.14 + 1.5*0.28 = 0.56 (reasoning rate unused in v1)
    )
  (testing "unknown pricing yields nil"
    (is (nil? (d/estimate-cost nil {:input-tokens 1 :output-tokens 1})))))

(deftest provider-result-shape
  (testing "cost units attach only when known"
    (is (= {:model/output {:text "hi"}
            :usage {:model-input-tokens 3 :model-output-tokens 4}
            :model-cost-units 0.7}
           (d/provider-result {:text "hi"}
                              {:input-tokens 3 :output-tokens 4}
                              0.7)))
    (is (not (contains? (d/provider-result {:text "hi"}
                                            {:input-tokens 3 :output-tokens 4}
                                            nil)
                        :model-cost-units))))))
