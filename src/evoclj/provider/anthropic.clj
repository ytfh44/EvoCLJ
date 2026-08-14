(ns evoclj.provider.anthropic
  "Anthropic messages-API model provider adapter (post-v0 extension 1).

  One adapter instance serves ONE Anthropic-compatible endpoint for
  the model ids that endpoint hosts, built on the official
  anthropic-java client (com.anthropic:anthropic-java). The API key
  and client are closed over (Global Constraint 19); the boundary
  carries only plain validated EDN (Global Constraint 22) — the raw
  HTTP response body is read through the SDK raw-response API and
  converted to EDN inside execute-request!.

  Protocol contract mirrors evoclj.provider.openai: describe returns
  the :model/<provider> tool descriptor with :effect :model-call and
  :retry {:safe? true}; normalize-request validates the model id and
  messages and returns the canonical {:kind :model :id ...} resource
  BEFORE authorization; execute-request! builds the SDK request
  (model, max_tokens, system prompt, user/assistant messages, vendor
  additionalProperties), calls the endpoint, parses the raw JSON via
  evoclj.provider.dialect/parse-anthropic-response, and returns the
  canonical provider result with usage and cost.

  Anthropic content blocks: only text blocks are returned as :text;
  tool_use blocks are ignored in v1. Reasoning (thinking blocks) is
  not interleaved into the output in v1."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.provider.dialect :as dialect]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m])
  (:import (com.anthropic.client.okhttp AnthropicOkHttpClient)
           (com.anthropic.core JsonValue)
           (com.anthropic.errors AnthropicServiceException AnthropicIoException)
           (com.anthropic.models.messages MessageCreateParams
                                          MessageCreateParams$Body)))

(def ModelCallInputSchema
  "The model-call input contract (same shape as the OpenAI adapter)."
  [:map {:closed false}
   [:model/id keyword?]
   [:messages [:vector :map]]
   [:options {:optional true} :map]])

(def ModelCallOutputSchema
  "The model-call output contract: text output and usage counters."
  [:map {:closed true}
   [:model/output [:map {:closed false}
                   [:text string?]
                   [:reasoning {:optional true} string?]]]
   [:usage [:map {:closed true}
            [:model-input-tokens :int]
            [:model-output-tokens :int]]]
   [:model-cost-units {:optional true} double?]])

(defn- descriptor-for
  [provider-id]
  {:tool/id (keyword "model" (name provider-id))
   :effect :model-call
   :input-schema ModelCallInputSchema
   :output-schema ModelCallOutputSchema
   :required-action :invoke
   :retry {:safe? true}})

(defn- edn->json
  "EDN-safe data to JSON-compatible Java values (same rules as the
  OpenAI adapter)."
  [x]
  (cond
    (or (nil? x) (true? x) (false? x) (number? x) (string? x)) x
    (keyword? x) (name x)
    (symbol? x) (str x)
    (map? x) (into {} (map (fn [[k v]] [(edn->json k) (edn->json v)])) x)
    (or (vector? x) (set? x) (list? x)) (mapv edn->json x)
    :else (throw (err/error :provider/input-invalid
                            "options must be plain EDN-safe data"
                            {:reason :not-edn-safe :value (err/sanitize x)}))))

(defn- model-request-name
  "The wire model id (the part after the provider prefix)."
  [model-id]
  (second (str/split model-id #"/")))

(defn- supported-option?
  "v1 supported call options: temperature and max-tokens."
  [k]
  (contains? #{:temperature :max-tokens :max-tool-rounds} k))

(defn- build-params
  "Build the SDK MessageCreateParams: system messages become the
  system prompt, user/assistant messages become message params,
  supported options map to builder methods, and any dialect extra
  params merge via additionalProperties."
  [request]
  (let [opts (or (:options request) {})
        messages (:messages request)
        system-prompt (->> (filter #(= :system (:role %)) messages)
                           (map :content)
                           (str/join "\n"))
        turns (remove #(= :system (:role %)) messages)
        b (-> (MessageCreateParams$Body/builder)
              (.model (model-request-name (:model/id request)))
              (.maxTokens (long (or (:max-tokens opts) 1024))))
        b (if (seq system-prompt)
            (.system b system-prompt)
            b)
        b (if-let [t (:temperature opts)]
            (.temperature b (double t))
            b)
        b (reduce (fn [b m]
                    (case (:role m)
                      :user (.addUserMessage b (str (:content m)))
                      :assistant (.addAssistantMessage b (str (:content m)))
                      (throw (err/error :provider/input-invalid
                                        (str "unsupported message role " (:role m))
                                        {:reason :unsupported-role :role (:role m)}))))
                  b turns)
        b (reduce (fn [b [k v]]
                    (.putAdditionalProperty b (str/replace (name k) "-" "_")
                                            (JsonValue/from (edn->json v))))
                  b (get-in request [:options :extra-params] {}))
        params (.build (.body (MessageCreateParams/builder) (.build b)))]
    params))

(defn- read-body
  [^java.io.InputStream is]
  (let [sb (StringBuilder.)]
    (with-open [r (java.io.BufferedReader. (java.io.InputStreamReader. is "UTF-8"))]
      (loop [line (.readLine r)]
        (when line
          (.append sb line)
          (recur (.readLine r)))))
    (str sb)))

(defn- execute-raw!
  "One messages-API call through the SDK raw-response API. SDK
  errors map by status: 429/5xx and IO errors are transient."
  [client params]
  (try
    (let [resp (.create (.withRawResponse (.messages client)) params)
          status (.statusCode resp)
          body (read-body (.body resp))]
      {:http/status status :http/body body})
    (catch AnthropicServiceException e
      (let [code (try (.statusCode e) (catch Exception _ 0))]
        (if (or (= code 429) (>= code 500))
          (throw (err/error :provider/transient-error
                            (str "model endpoint " code ": " (.getMessage e))
                            {:status :http-error :http-code code}))
          (throw (err/error :provider/model-error
                            (str "model endpoint rejected the request: HTTP " code)
                            {:status :http-error :http-code code})))))
    (catch AnthropicIoException e
      (throw (err/error :provider/transient-error
                        (str "model endpoint IO error: " (.getMessage e))
                        {:status :io-error})))))

(defn- parse-http-response!
  [raw]
  (let [status (:http/status raw)]
    (when-not (<= 200 status 299)
      (if (>= status 500)
        (throw (err/error :provider/transient-error
                          (str "model endpoint HTTP " status)
                          {:status :http-error :http-code status}))
        (throw (err/error :provider/model-error
                          (str "model endpoint HTTP " status)
                          {:status :http-error :http-code status}))))
    (let [parsed (try
                   (json/parse-string (:http/body raw) true)
                   (catch Exception e
                     (throw (err/error :provider/model-error
                                       "model endpoint returned malformed JSON"
                                       {:reason :bad-json
                                        :message (str (.getMessage e))}))))]
      (dialect/parse-anthropic-response parsed))))

(defn anthropic-provider
  "Build one Anthropic-compatible provider.

  opts: :provider/id (keyword), :base-url (string), :api-key
  (string, closed over), :model-entries (slice of the models.dev
  index this endpoint serves), :timeout-ms (default 60000),
  :execution-count (atom, optional)."
  [opts]
  (let [{provider-id :provider/id base-url :base-url api-key :api-key
         model-entries :model-entries timeout-ms :timeout-ms
         execution-count :execution-count} opts
        timeout-ms (or timeout-ms 60000)
        served (set (keys model-entries))
        client (-> (AnthropicOkHttpClient/builder)
                   (.baseUrl base-url)
                   (.apiKey api-key)
                   (.timeout (java.time.Duration/ofMillis timeout-ms))
                   (.maxRetries 0)
                   (.build))
        execution-count (or execution-count (atom 0))
        describe-map (descriptor-for provider-id)]
    (reify proto/Provider
      (describe [_] describe-map)
      (normalize-request [_ intent]
        (let [payload (:payload intent)
              model-id (get payload :model/id)
              full-id (if (keyword? model-id)
                        (str (name provider-id) "/" (name model-id))
                        model-id)]
          (when-not (contains? served full-id)
            (throw (err/error :provider/input-invalid
                              (str "model " full-id " is not served by this endpoint")
                              {:reason :model-not-served :model/id full-id})))
          (when-not (and (vector? (:messages payload))
                         (every? map? (:messages payload)))
            (throw (err/error :provider/input-invalid
                              "model-call payload must carry a :messages vector of maps"
                              {:reason :messages-invalid
                               :value (err/sanitize (:messages payload))})))
          (doseq [k (keys (or (:options payload) {}))]
            (when-not (supported-option? k)
              (throw (err/error :provider/input-invalid
                                (str "unsupported model-call option " k)
                                {:reason :unknown-option :option k}))))
          {:model/id full-id
           :resource {:kind :model :id full-id :provider provider-id}
           :request {:model/id full-id
                     :messages (:messages payload)
                     :options (:options payload)}}))
      (execute-request! [_ authorized-request]
        (when-not (and (map? authorized-request) (:request authorized-request))
          (throw (err/error :provider/request-invalid
                            "execute-request! requires a normalized model request"
                            {:value (err/sanitize authorized-request)})))
        (swap! execution-count inc)
        (let [request (:request authorized-request)
              entry (get model-entries (:model/id request))
              params (build-params request)
              raw (execute-raw! client params)
              parsed (parse-http-response! raw)
              usage (:usage parsed)
              cost (dialect/estimate-cost (:model/cost entry) usage)]
          (dialect/provider-result (:model/output parsed) usage cost))))))
