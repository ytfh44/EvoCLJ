(ns evoclj.provider.openai-test
  "Tests for the OpenAI-compatible provider adapter
  (evoclj.provider.openai) against a local fake chat/completions
  endpoint: describe contract, normalize-request validation,
  execute-request! end to end (dialect params in the wire request,
  interleaved reasoning + usage + cost in the result), HTTP error
  mapping (429/5xx transient, 4xx model-error, malformed JSON), and
  the execution counter."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.provider.openai :as oa]
            [evoclj.provider.protocol :as proto]
            [malli.core :as m])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.net InetSocketAddress)))

(def ^:private deepseek-entry
  {:model/id "deepseek/deepseek-v4-flash"
   :model/dialect {:interleaved :reasoning_content
                   :reasoning-options [{:type :toggle}
                                       {:type :effort :values ["low" "high" "max"]}]
                   :server-side-search :off
                   :extra-params {}}
   :model/cost {:input 0.14 :output 0.28}})

(def ^:private searchable-entry
  (assoc deepseek-entry
         :model/id "deepseek/deepseek-search"
         :model/dialect (assoc (get-in deepseek-entry [:model/dialect])
                               :server-side-search :web-search-options)))

(def ^:private success-response
  "{\"id\":\"x\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"hello\",\"reasoning_content\":\"thought\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":5,\"total_tokens\":15}}")

(defn- make-server
  "Start a fake chat/completions server. handler-fn receives the
  request body string and returns [status response-body]; the server
  records every request body in an atom."
  [handler-fn]
  (let [requests (atom [])
        server (HttpServer/create (InetSocketAddress. 0) 0)]
    (.createContext server "/chat/completions"
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
  "A provider bound to the given fake server; registers the server
  for cleanup. entries defaults to the deepseek entry."
  [fixture & {:keys [entries] :as _opts}]
  (swap! servers conj fixture)
  (oa/openai-compatible-provider
   {:provider/id :deepseek
    :base-url (:base-url fixture)
    :api-key "test-key"
    :model-entries (or entries {"deepseek/deepseek-v4-flash" deepseek-entry})
    :timeout-ms 5000}))

(defn- model-intent
  "A fully-attributed :intent/model-call for the deepseek model."
  [& {:keys [model messages options] :as _opts}]
  {:intent/id (java.util.UUID/randomUUID)
   :intent/type :intent/model-call
   :session/id (java.util.UUID/randomUUID)
   :phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   :node/id :node/planner
   :cause/event-id 1
   :payload {:model/id (or model :deepseek-v4-flash)
             :messages (or messages [{:role :user :content "hi"}])
             :options options}
   :budget {:wall-ms 5000}
   :metadata {}})

(deftest describe-contract
  (let [fx (make-server (fn [_] [200 success-response]))
        p (provider-for fx)
        d (proto/describe p)]
    (is (= :model/deepseek (:tool/id d)))
    (is (= :model-call (:effect d)))
    (is (= {:safe? true} (:retry d)))
    (is (m/validate oa/ModelCallInputSchema
                    {:model/id :deepseek-v4-flash
                     :messages [{:role :user :content "x"}]}))))

(deftest normalize-validates
  (let [fx (make-server (fn [_] [200 success-response]))
        p (provider-for fx)]
    (testing "known model normalizes to the canonical model resource"
      (let [n (proto/normalize-request p (model-intent))]
        (is (= {:kind :model
                :id "deepseek/deepseek-v4-flash"
                :provider :deepseek}
               (:resource n)))
        (is (= "deepseek/deepseek-v4-flash" (:model/id n)))))
    (testing "unknown model fails closed"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"not served"
                            (proto/normalize-request p
                                                     (model-intent :model :gpt-9)))))
    (testing "bad messages fail closed"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"messages"
                            (proto/normalize-request p
                                                     (model-intent :messages "nope")))))
    (testing "unknown options fail closed"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"unsupported model-call option"
                            (proto/normalize-request p
                                                     (model-intent :options {:frequency_penalty 1.0})))))))
(deftest execute-end-to-end
  (testing "dialect params reach the wire; reasoning/usage/cost return"
    (let [fx (make-server (fn [_] [200 success-response]))
          p (provider-for fx)
          n (proto/normalize-request p (model-intent
                                        :options {:temperature 0.5
                                                  :reasoning {:mode :effort :level "high"}}))
          result (proto/execute-request! p n)]
      (is (= {:text "hello" :reasoning "thought"}
             (:model/output result)))
      (is (= {:model-input-tokens 10 :model-output-tokens 5}
             (select-keys (:usage result) [:model-input-tokens :model-output-tokens])))
      (is (< (Math/abs (- 2.8 (double (:model-cost-units result)))) 1e-9))
      (let [req (json/parse-string (first @(:requests fx)) true)]
        (is (= "deepseek-v4-flash" (:model req)))
        (is (= 0.5 (:temperature req)))
        (is (= "high" (:reasoning_effort req)))
        (is (= "hi" (get-in req [:messages 0 :content])))
        (is (= "user" (get-in req [:messages 0 :role]))))
      (is (m/validate oa/ModelCallOutputSchema result))))
  (testing "execution counter bumps exactly once per call"
    (let [counter (atom 0)
          fx (make-server (fn [_] [200 success-response]))
          p (oa/openai-compatible-provider
             {:provider/id :deepseek
              :base-url (:base-url fx)
              :api-key "k"
              :model-entries {"deepseek/deepseek-v4-flash" deepseek-entry}
              :execution-count counter
              :timeout-ms 5000})
          n (proto/normalize-request p (model-intent))]
      (swap! servers conj fx)
      (proto/execute-request! p n)
      (proto/execute-request! p n)
      (is (= 2 @counter)))))

(deftest server-side-search-modes
  (testing "web-search-options mode adds web_search_options to the body"
    (let [fx (make-server (fn [_] [200 success-response]))
          p (provider-for fx :entries {"deepseek/deepseek-search" searchable-entry})
          n (proto/normalize-request p (model-intent :model :deepseek-search
                                                     :options {:server-side-search true}))]
      (proto/execute-request! p n)
      (let [req (json/parse-string (first @(:requests fx)) true)]
        (is (= "medium" (get-in req [:web_search_options :search_context_size]))))))
  (testing "web-search-tool mode appends a web_search tool"
    (let [fx (make-server (fn [_] [200 success-response]))
          entry (assoc deepseek-entry
                       :model/id "deepseek/deepseek-tool"
                       :model/dialect (assoc (get-in deepseek-entry [:model/dialect])
                                             :server-side-search :web-search-tool))
          p (provider-for fx :entries {"deepseek/deepseek-tool" entry})
          n (proto/normalize-request p (model-intent :model :deepseek-tool
                                                     :options {:server-side-search true}))]
      (proto/execute-request! p n)
      (let [req (json/parse-string (first @(:requests fx)) true)]
        (is (some #(= "web_search" (:type %)) (:tools req)))))))

(deftest http-error-mapping
  (testing "429 is transient (retryable)"
    (let [fx (make-server (fn [_] [429 "{\"error\":{\"message\":\"rate\"}}"]))
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
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"500"
                            (proto/execute-request! p n)))))
  (testing "400 is a hard model-error"
    (let [fx (make-server (fn [_] [400 "{\"error\":{\"message\":\"bad\"}}"]))
          p (provider-for fx)
          n (proto/normalize-request p (model-intent))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"rejected the request"
                            (proto/execute-request! p n)))))
  (testing "malformed JSON body is a model-error"
    (let [fx (make-server (fn [_] [200 "not json at all"]))
          p (provider-for fx)
          n (proto/normalize-request p (model-intent))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"malformed JSON"
                            (proto/execute-request! p n))))))
