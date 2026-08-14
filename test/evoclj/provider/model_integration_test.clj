(ns evoclj.provider.model-integration-test
  "Integration tests for the real-model path: model registry -> broker
  dispatch -> capability lease (kind :model) -> llm node -> provider.

  All HTTP is fake (local HttpServer); the model registry is built
  from a small catalog index with an explicit API key, so no
  environment variables or network are involved."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.capability.lease :as lease]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.model-registry :as mreg]
            [evoclj.provider.protocol :as proto]
            [evoclj.runtime.node :as node]
            [evoclj.runtime.nodes.llm :as llm])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.net InetSocketAddress)))

(def ^:private fake-index
  {"deepseek/deepseek-v4-flash"
   {:model/id "deepseek/deepseek-v4-flash"
    :model/provider :deepseek
    :model/style :openai-compatible
    :model/status :supported
    :model/base-url "http://placeholder"
    :model/api-key-env "DEEPSEEK_API_KEY"
    :model/dialect {:interleaved :reasoning_content
                    :reasoning-options [{:type :toggle}]
                    :server-side-search :off
                    :extra-params {}}
    :model/cost {:input 0.14 :output 0.28}}
   "anthropic/claude-opus-4-7"
   {:model/id "anthropic/claude-opus-4-7"
    :model/provider :anthropic
    :model/style :anthropic
    :model/status :supported
    :model/base-url "http://placeholder"
    :model/api-key-env "ANTHROPIC_API_KEY"
    :model/dialect {:interleaved :none :reasoning-options []
                    :server-side-search :off :extra-params {}}
    :model/cost {:input 5 :output 25}}})

(def ^:private openai-response
  "{\"id\":\"x\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"hi from deepseek\",\"reasoning_content\":\"hmm\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":4,\"completion_tokens\":3}}")

(defn- start-openai-server
  []
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)]
    (.createContext server "/chat/completions"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [bytes (.getBytes openai-response "UTF-8")]
                          (.sendResponseHeaders exchange 200 (count bytes))
                          (with-open [os (.getResponseBody exchange)]
                            (.write os bytes))))))
    (.start server)
    {:server server
     :base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))}))

(def ^:private servers (atom []))

(use-fixtures :each
  (fn [f]
    (reset! servers [])
    (f)
    (doseq [{:keys [server]} @servers] (.stop server 0))))

(defn- base-url-rewrite
  "Point the fake index entries at the live fake server."
  [fx]
  (update-vals fake-index #(assoc % :model/base-url (:base-url fx))))

(defn- phenotype-id
  []
  "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(deftest registry-build
  (testing "configured models get live providers; missing keys are flagged"
    (let [fx (start-openai-server)
          _ (swap! servers conj fx)
          reg (mreg/build-model-registry
               (base-url-rewrite fx)
               {:registry/api-keys {:deepseek "sk-ds"}})]
      (is (some? (:provider (mreg/lookup reg "deepseek/deepseek-v4-flash"))))
      (is (= :api-key-missing (:reason (mreg/lookup reg "anthropic/claude-opus-4-7"))))
      (is (= ["deepseek/deepseek-v4-flash"]
             (mreg/configured-models reg))))))

(deftest lease-model-matching
  (testing "exact and prefix model grants"
    (let [lease-fn (fn [id]
                     {:cap/id (java.util.UUID/randomUUID)
                      :subject {:phenotype/id (phenotype-id)}
                      :resource {:kind :model :id id}
                      :actions #{:invoke}
                      :constraints {}
                      :issued-at (java.util.Date. 0)
                      :expires-at (java.util.Date. 9999999999999)})]
      (is (true? (lease/resource-covers?
                  (lease-fn "deepseek/deepseek-v4-flash")
                  {:kind :model :id "deepseek/deepseek-v4-flash"} :invoke)))
      (is (true? (lease/resource-covers?
                  (lease-fn "deepseek/*")
                  {:kind :model :id "deepseek/deepseek-v4-flash"} :invoke)))
      (is (false? (lease/resource-covers?
                   (lease-fn "deepseek/deepseek-v4-flash")
                   {:kind :model :id "deepseek/other"} :invoke)))
      (is (false? (lease/resource-covers?
                   (lease-fn "deepseek/deepseek-v4-flash")
                   {:kind :tool :id :fixture/echo} :invoke))))))

(deftest dispatch-model-call
  (testing "authorized model call executes and returns the provider value"
    (let [fx (start-openai-server)
          _ (swap! servers conj fx)
          reg (atom {:fixture/echo nil}) ;; dummy provider registry
          model-reg (mreg/build-model-registry
                     (base-url-rewrite fx)
                     {:registry/api-keys {:deepseek "sk-ds"}})
          ctx (dispatch/make-broker-context
               {:registry reg
                :model-registry model-reg
                :leases [{:cap/id (java.util.UUID/randomUUID)
                          :subject {:phenotype/id (phenotype-id)}
                          :resource {:kind :model :id "deepseek/*"}
                          :actions #{:invoke}
                          :constraints {}
                          :issued-at (java.util.Date. 0)
                          :expires-at (java.util.Date. 9999999999999)}]})
          i (intent/model-call
             (java.util.UUID/randomUUID) (phenotype-id) :node/planner 7
             {:model/id "deepseek/deepseek-v4-flash"
              :messages [{:role :user :content "hello"}]}
             {:wall-ms 5000})
          result (dispatch/dispatch! ctx i)]
      (is (= :ok (:result/status result)))
      (is (= "hi from deepseek" (get-in result [:value :model/output :text])))
      (is (= "hmm" (get-in result [:value :model/output :reasoning])))
      (is (some? (:authorization result)))))
  (testing "denied without a model lease"
    (let [fx (start-openai-server)
          _ (swap! servers conj fx)
          model-reg (mreg/build-model-registry
                     (base-url-rewrite fx)
                     {:registry/api-keys {:deepseek "sk-ds"}})
          ctx (dispatch/make-broker-context
               {:registry (atom {}) :model-registry model-reg})
          i (intent/model-call
             (java.util.UUID/randomUUID) (phenotype-id) :node/planner 7
             {:model/id "deepseek/deepseek-v4-flash"
              :messages [{:role :user :content "hello"}]}
             {:wall-ms 5000})
          result (dispatch/dispatch! ctx i)]
      (is (= :error (:result/status result)))
      (is (= :capability/denied (:error/type result)))))
  (testing "unconfigured model reports the reason"
    (let [fx (start-openai-server)
          _ (swap! servers conj fx)
          model-reg (mreg/build-model-registry (base-url-rewrite fx) {})
          ctx (dispatch/make-broker-context
               {:registry (atom {}) :model-registry model-reg})
          i (intent/model-call
             (java.util.UUID/randomUUID) (phenotype-id) :node/planner 7
             {:model/id "deepseek/deepseek-v4-flash"
              :messages [{:role :user :content "hello"}]}
             {:wall-ms 5000})
          result (dispatch/dispatch! ctx i)]
      (is (= :provider/not-configured (:error/type result)))
      (is (= :api-key-missing (get-in result [:error/data :reason]))))))

(deftest llm-node-emits-model-intent
  (testing "the handler resolves the alias via the compiled resolution"
    (let [handler (llm/llm-handler)
          runtime-state {:session/id (java.util.UUID/randomUUID)
                         :phenotype/id (phenotype-id)
                         :node/id :node/planner
                         :outputs []
                         :compiled {:resolution {:models {:planner {:alias :reasoning/high
                                                                    :provider :deepseek
                                                                    :provider-model "deepseek/deepseek-v4-flash"
                                                                    :adapter-version "1"}}}}}
          node {:node/type :llm :model :planner :system "Be brief" :next :node/emit}
          input-event {:event/id 9 :event/type :node/started :payload {:op :ask}}
          transition (node/step handler runtime-state node input-event)
          [i] (:intents transition)]
      (is (= :continue (:transition/status transition)))
      (is (= "deepseek/deepseek-v4-flash" (get-in i [:payload :model/id])))
      (is (= [{:role :system :content "Be brief"}
              {:role :user :content "{:op :ask}"}]
             (get-in i [:payload :messages])))
      (is (= :intent/model-call (:intent/type i)))))
  (testing "unresolved alias fails closed"
    (let [handler (llm/llm-handler)
          runtime-state {:session/id (java.util.UUID/randomUUID)
                         :phenotype/id (phenotype-id)
                         :node/id :node/planner
                         :outputs []
                         :compiled {:resolution {:models {}}}}
          node {:node/type :llm :model :ghost :next :node/emit}
          input-event {:event/id 9 :event/type :node/started :payload "x"}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"unresolved"
                            (node/step handler runtime-state node input-event))))))
