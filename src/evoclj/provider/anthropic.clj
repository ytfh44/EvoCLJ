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
  (model, max_tokens, system prompt, user/assistant messages, and the
  tool declaration), calls the endpoint, parses the raw JSON via
  provider.request/parse-response (single dispatch point), and returns
  the canonical provider result with usage and cost.

  TOOLS. A :tools vector on the payload is declared to the endpoint the
  same way evoclj.provider.openai declares it — provider.request/wire-tools
  is the single EDN->wire shape — and the SDK's own builder carries it
  (MessageCreateParams$Body.Builder.tools(List<ToolUnion>)), rather than
  the putAdditionalProperty escape hatch. Tool RESULTS come back as a
  user-role turn carrying tool_result content blocks, which is how
  Anthropic expresses them: there is no \"tool\" role. A :tool-role
  message that arrives without that shape is still refused with
  :unsupported-role rather than silently dropped."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.provider.dialect :as dialect]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.request :as request]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m])
  (:import (com.anthropic.client.okhttp AnthropicOkHttpClient)
           (com.anthropic.core JsonValue)
           (com.anthropic.errors AnthropicServiceException AnthropicIoException)
           (com.anthropic.models.messages ContentBlockParam MessageCreateParams
                                          MessageCreateParams$Body
                                          Tool Tool$InputSchema ToolUnion
                                          ToolUnion$Companion
                                          ToolResultBlockParam)))

(def ModelCallInputSchema
  "The model-call input contract (same shape as the OpenAI adapter)."
  [:map {:closed false}
   [:model/id keyword?]
   [:options {:optional true} :map]
   ;; the tool declarations to offer; same shape openai.clj accepts
   [:tools {:optional true} [:vector :map]]])

(def ModelCallOutputSchema
  "The model-call output contract: text output, optional tool-calls,
   and usage counters."
  [:map {:closed true}
   [:model/output [:map {:closed false}
                   [:text string?]
                   [:reasoning {:optional true} string?]]]
   [:tool-calls {:optional true}
    [:vector [:map {:closed false}
              [:tool/call-id string?]
              [:tool/name string?]
              [:tool/arguments :map]]]]
   [:usage [:map {:closed true}
            [:model-input-tokens :int]
            [:model-output-tokens :int]
            [:model-reasoning-tokens {:optional true} :int]]]
   [:model-cost-units {:optional true} double?]])

(defn- descriptor-for
  [provider-id]
  {:tool/id (keyword "model" (name provider-id))
   :effect :model-call
   :input-schema ModelCallInputSchema
   :output-schema ModelCallOutputSchema
   :required-action :invoke
   :retry {:safe? true}})

(defn- model-request-name
  "The wire model id (the part after the provider prefix)."
  [model-id]
  (second (str/split model-id #"/")))

(defn- tool-union
  "One wire tool declaration as the SDK's ToolUnion.

  provider.request/wire-tools is the single EDN->wire definition, shared
  with openai.clj, and it emits the OpenAI function shape
  {:type \"function\" :function {:name … :description … :parameters …}};
  the internal :tool id is stripped there, as it must never reach the
  wire. Only the SDK wrapper is provider-specific: Anthropic takes a raw
  JSON Schema under :input_schema, so :parameters is attached as the
  schema's own members (with the required `type: object`) rather than
  being reshaped."
  [wire]
  (let [{:keys [name description parameters]} (:function wire)
        schema (-> (Tool$InputSchema/builder)
                   (.type (JsonValue/from "object"))
                   (.putAllAdditionalProperties
                    (into {}
                          (map (fn [[k v]]
                                 [(request/edn->json k)
                                  (JsonValue/from (request/edn->json v))]))
                          (or parameters {})))
                   (.build))
        tool (-> (Tool/builder)
                 (.name name)
                 (.description (or description ""))
                 (.inputSchema schema)
                 (.build))]
    (.ofTool ^ToolUnion$Companion ToolUnion/Companion tool)))

(defn- tool-result-blocks
  "The tool_result content blocks for one :tool turn. Anthropic has no
  \"tool\" role: a tool result is a USER-role turn whose content blocks
  are tool_result, each naming the tool_use id it answers. A turn may
  carry either one {:tool-call-id … :content …} or a vector of them."
  [m]
  (let [content (:content m)
        results (if (and (sequential? content) (not (map? (first content))))
                  content
                  [{:tool-call-id (:tool-call-id m) :content content}])]
    (mapv (fn [{:keys [tool-call-id content]}]
            (-> (ToolResultBlockParam/builder)
                (.toolUseId (str tool-call-id))
                (.content (str content))
                (.build)))
          results)))


(defn- build-params
  "Build the SDK MessageCreateParams: system messages become the
  system prompt, user/assistant/tool messages become message params (a
  tool result is a user turn of tool_result content blocks), and
  :tools is declared through the SDK's own tools builder.
  Delegates edn->json and wire-tools to provider.request."
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
        b (if (seq (:tools request))
            (.tools b (mapv tool-union (request/wire-tools (:tools request))))
            b)
        b (reduce (fn [b m]
                    (case (:role m)
                      :user (.addUserMessage b (str (:content m)))
                      :assistant (.addAssistantMessage b (str (:content m)))
                      :tool (.addUserMessageOfBlockParams b (tool-result-blocks m))
                      (throw (err/error :provider/input-invalid
                                        (str "unsupported message role " (:role m))
                                        {:reason :unsupported-role :role (:role m)}))))
                  b turns)
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
  "Validate the raw HTTP result and parse the JSON body into EDN:
  non-2xx statuses become typed errors; malformed JSON becomes
  :provider/model-error. Delegates to provider.request/parse-response
  (single dispatch point)."
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
      (request/parse-response :anthropic nil parsed))))

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
            (when-not (request/supported-option? :anthropic k)
              (throw (err/error :provider/input-invalid
                                (str "unsupported model-call option " k)
                                {:reason :unknown-option :option k}))))
          (when (and (:tools payload)
                     (not (and (vector? (:tools payload))
                               (every? map? (:tools payload)))))
            (throw (err/error :provider/input-invalid
                              "model-call payload :tools must be a vector of maps"
                              {:reason :tools-invalid
                               :value (err/sanitize (:tools payload))})))
          {:model/id full-id
           :resource {:kind :model :id full-id :provider provider-id}
           :request {:model/id full-id
                     :messages (:messages payload)
                     :options (:options payload)
                     :tools (:tools payload)}}))
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
              cost (dialect/estimate-cost (:model/cost entry) usage)
              result (dialect/provider-result (:model/output parsed) usage cost)]
          (if (:tool-calls parsed)
            (assoc result :tool-calls (:tool-calls parsed))
            result))))))
