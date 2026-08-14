(ns evoclj.provider.anthropic-test
  "Tests for the Anthropic provider adapter (evoclj.provider.anthropic)
  against a local fake /v1/messages endpoint: describe contract,
  normalize-request validation, end-to-end execution (system prompt,
  user/assistant turns, max-tokens, usage, cost), HTTP error mapping,
  and the execution counter."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.provider.anthropic :as an]
            [evoclj.provider.protocol :as proto]
            [malli.core :as m])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.net InetSocketAddress)))

(def ^:private claude-entry
  {:model/id "anthropic/claude-opus-4-7"
   :model/dialect {:interleaved :none :reasoning-options []
                   :server-side-search :off :extra-params {}}
   :model/cost {:input 5 :output 25}})

(def ^:private success-response
  "{\"id\":\"msg_1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"hello from claude\"},{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"x\"}],\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":15,\"output_tokens\":9}}")

(defn- make-server
  [handler-fn]
  (let [requests (atom [])
        server (HttpServer/create (InetSocketAddress. 0) 0)]
    (.createContext server "/v1/messages"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [body (slurp (.getRequestBody exchange))
                              [status resp] (handler-fn body)]
                          (swap! requests conj body)
                          (let [bytes (.getBytes resp "UTF-8")]
                            (.sendResponseHeaders exchange status (count bytes))
                            (with-open [os (.getResponseBody exchange)]
                              (.write os bytes)))))))
    (.start server)
    {:server server
     :base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
     :requests requests}))

(def ^:private servers (atom []))

(use-fixtures :each
  (fn [f]
    (reset! servers [])
    (f)
    (doseq [{:keys [server]} @servers] (.stop server 0))))

(defn- provider-for
  [fixture & {:keys [entries] :as _opts}]
  (swap! servers conj fixture)
  (an/anthropic-provider
   {:provider/id :anthropic
    :base-url (:base-url fixture)
    :api-key "sk-ant-test"
    :model-entries (or entries {"anthropic/claude-opus-4-7" claude-entry})
    :timeout-ms 5000}))

(defn- model-intent
  [& {:keys [model messages options] :as _opts}]
  {:intent/id (java.util.UUID/randomUUID)
   :intent/type :intent/model-call
   :session/id (java.util.UUID/randomUUID)
   :phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   :node/id :node/planner
   :cause/event-id 1
   :payload {:model/id (or model :claude-opus-4-7)
             :messages (or messages [{:role :user :content "hi"}])
             :options options}
   :budget {:wall-ms 5000}
   :metadata {}})

(deftest describe-and-normalize
  (let [fx (make-server (fn [_] [200 success-response]))
        p (provider-for fx)
        d (proto/describe p)]
    (is (= :model/anthropic (:tool/id d)))
    (is (= :model-call (:effect d)))
    (is (= {:safe? true} (:retry d)))
    (testing "known model normalizes with the canonical resource"
      (let [n (proto/normalize-request p (model-intent))]
        (is (= {:kind :model :id "anthropic/claude-opus-4-7" :provider :anthropic}
               (:resource n)))))
    (testing "unknown model fails closed"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"not served"
                            (proto/normalize-request p (model-intent :model :gpt-9)))))
    (testing "unknown options fail closed"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"unsupported model-call option"
                            (proto/normalize-request p (model-intent :options {:seed 1})))))))

(deftest execute-end-to-end
  (testing "system prompt, turns, options, usage and cost"
    (let [fx (make-server (fn [_] [200 success-response]))
          p (provider-for fx)
          n (proto/normalize-request
             p (model-intent
                :messages [{:role :system :content "Be terse"}
                           {:role :user :content "hi"}
                           {:role :assistant :content "previous"}]
                :options {:temperature 0.7 :max-tokens 2048}))
          result (proto/execute-request! p n)]
      (is (= {:text "hello from claude"} (:model/output result)))
      (is (= {:model-input-tokens 15 :model-output-tokens 9}
             (select-keys (:usage result) [:model-input-tokens :model-output-tokens])))
      (is (< (Math/abs (- 300.0 (double (:model-cost-units result)))) 1e-9))
      (let [req (json/parse-string (first @(:requests fx)) true)]
        (is (= "claude-opus-4-7" (:model req)))
        (is (= 2048 (:max_tokens req)))
        (is (= 0.7 (:temperature req)))
        (is (= "Be terse" (:system req)))
        (is (= [{:content "hi" :role "user"}
                {:content "previous" :role "assistant"}]
               (:messages req))))
      (is (m/validate an/ModelCallOutputSchema result)))))

(deftest error-mapping
  (testing "429 is transient"
    (let [fx (make-server (fn [_] [429 "{\"type\":\"error\"}"]))
          p (provider-for fx)
          n (proto/normalize-request p (model-intent))]
      (is (try
            (proto/execute-request! p n)
            false
            (catch clojure.lang.ExceptionInfo e
              (= :provider/transient-error (:error/type (ex-data e))))))))
  (testing "500 is transient"
    (let [fx (make-server (fn [_] [500 "boom"]))
          p (provider-for fx)
          n (proto/normalize-request p (model-intent))]
      (is (try
            (proto/execute-request! p n)
            false
            (catch clojure.lang.ExceptionInfo e
              (= :provider/transient-error (:error/type (ex-data e))))))))
  (testing "400 is a hard model-error"
    (let [fx (make-server (fn [_] [400 "{\"type\":\"error\"}"]))
          p (provider-for fx)
          n (proto/normalize-request p (model-intent))]
      (is (try
            (proto/execute-request! p n)
            false
            (catch clojure.lang.ExceptionInfo e
              (= :provider/model-error (:error/type (ex-data e))))))))
  (testing "malformed JSON is a model-error"
    (let [fx (make-server (fn [_] [200 "not json"]))
          p (provider-for fx)
          n (proto/normalize-request p (model-intent))]
      (is (try
            (proto/execute-request! p n)
            false
            (catch clojure.lang.ExceptionInfo e
              (= :provider/model-error (:error/type (ex-data e)))))))))
