(ns evoclj.provider.openai
  "OpenAI-compatible model provider adapter (post-v0 extension 1).

  One adapter instance serves ONE OpenAI-compatible endpoint
  (openai.com, deepseek, azure-openai, groq, lmstudio, ollama, ...)
  for the model ids that endpoint hosts. It is built on the official
  openai-java client (com.openai:openai-java) with a base-url
  override — the same pattern the SDK documents for OpenAI-compatible
  servers — plus dialect support (evoclj.provider.dialect):

    - custom headers and query params (azure api-key / api-version)
    - additionalProperties for vendor-specific request fields
      (reasoning_effort, enable_thinking, ...)
    - server-side search (web_search_options / web_search tool)
    - interleaved reasoning extraction (DeepSeek reasoning_content)

  The adapter is a kernel-owned host object (Global Constraint 19):
  the API key, base URL, and client are closed over and NEVER appear
  in describe / normalize-request / execute-request! output. All
  boundary values are plain validated EDN (Global Constraint 22);
  the SDK objects (OpenAIClient, ChatCompletion) never cross the
  provider boundary — execute-request! reads the raw HTTP response
  body through the SDK raw-response API and converts it to EDN
  before anything is returned.

  The three protocol methods:

    1. describe — a tool descriptor with :tool/id :model/<provider>
       (one registration per endpoint, NOT per model), :effect
       :model-call, :retry {:safe? true} (a model call has no
       side effect beyond cost, so the dispatcher may retry a
       transient failure), and the model-call input/output schemas.
    2. normalize-request — validates the payload (:model/id must be
       served by this endpoint, :messages must be EDN-safe), and
       returns the canonical resource {:kind :model :id <model-id>}
       BEFORE authorization (Global Constraint 9: the lease check
       sees the canonical model id, never a display name).
    3. execute-request! — builds the SDK request from the
       authorized EDN request + the model dialect, calls the
       endpoint, parses the raw JSON response, and returns
       {:model/output {:text ... :reasoning ...}
        :usage {:model-input-tokens ... :model-output-tokens ...
                :model-cost-units ...}}.

  Error contract: :provider/input-invalid (:reason :model-not-served,
  :messages-invalid, :unsupported-role, :unknown-option),
  :provider/request-invalid (unnormalized request),
  :provider/transient-error (HTTP 429/5xx, IO/timeout — the
  dispatcher may retry because the descriptor declares :retry
  {:safe? true}), :provider/model-error (4xx or malformed
  response), :provider/output-invalid (response failed schema
  validation — never model-visible data)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.provider.dialect :as dialect]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m])
  (:import (com.openai.client.okhttp OpenAIOkHttpClient)
           (com.openai.core JsonValue)
           (com.openai.errors OpenAIIoException OpenAIServiceException)
           (com.openai.models.chat.completions ChatCompletionCreateParams
                                              ChatCompletionCreateParams$Body
                                              ChatCompletionUserMessageParam
                                              ChatCompletionSystemMessageParam
                                              ChatCompletionAssistantMessageParam
                                              ChatCompletionToolMessageParam)))

;; --- schemas ------------------------------------------------------------------

(def ModelCallInputSchema
  "The model-call input contract for this adapter: the model id (a
  keyword in the intent payload, matching a models.dev id), the
  messages vector (role/content maps), and optional call options.
  Open to further keys; EDN-safety is enforced at the boundary."
  [:map {:closed false}
   [:model/id keyword?]
   [:messages [:vector :map]]
   [:options {:optional true} :map]])

(def ModelCallOutputSchema
  "The model-call output contract: the model output (text, plus an
  optional interleaved reasoning trace) and usage counters. Closed:
  no field may be missing, renamed, or extended."
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
            [:model-reasoning-tokens {:optional true} :int]]]]
   [:model-cost-units {:optional true} double?]])

(defn- descriptor-for
  "The tool descriptor for one endpoint: one :tool/id per provider,
  :effect :model-call (an external read-like effect: the dispatcher
  requires no idempotency key and may retry transient failures
  because the descriptor declares :retry {:safe? true}), and the
  model-call schemas."
  [provider-id]
  {:tool/id (keyword "model" (name provider-id))
   :effect :model-call
   :input-schema ModelCallInputSchema
   :output-schema ModelCallOutputSchema
   :required-action :invoke
   :retry {:safe? true}})

;; --- EDN -> SDK conversion -----------------------------------------------------

(defn- edn->json
  "Convert EDN-safe data into JSON-compatible Java values for the
  SDK: keywords/symbols become strings (names only), sets become
  vectors, maps keep keyword-free string keys. Scalar values pass
  through. Throws :provider/input-invalid on anything non-EDN."
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

(defn- message->param
  "Convert one EDN message into the SDK message param. Roles:
  :system/:user/:assistant carry :content; :assistant may also carry
  :tool-calls (the model's own tool-call declarations, serialized as
  raw JSON via additionalProperties — the SDK's typed tool-call
  builder is a sealed union, so raw is the portable path); :tool
  messages carry :tool-call-id + :content and become
  ChatCompletionToolMessageParam. Any other role is rejected."
  [msg]
  (let [role (:role msg)
        content (:content msg)]
    (when-not (and (keyword? role) (string? content))
      (throw (err/error :provider/input-invalid
                        "message must carry :role keyword and :content string"
                        {:reason :messages-invalid :value (err/sanitize msg)})))
    (case role
      :system (.build (.content (ChatCompletionSystemMessageParam/builder) content))
      :user (.build (.content (ChatCompletionUserMessageParam/builder) content))
      :assistant (let [b (.content (ChatCompletionAssistantMessageParam/builder) content)]
                   (if-let [tool-calls (:tool-calls msg)]
                     (.build (.putAdditionalProperty
                              b "tool_calls" (JsonValue/from (edn->json tool-calls))))
                     (.build b)))
      :tool (do
              (when-not (and (string? (:tool-call-id msg))
                             (not (str/blank? (:tool-call-id msg))))
                (throw (err/error :provider/input-invalid
                                  "tool message must carry a :tool-call-id string"
                                  {:reason :messages-invalid :value (err/sanitize msg)})))
              (.build (.toolCallId
                       (.content (ChatCompletionToolMessageParam/builder) content)
                       (:tool-call-id msg))))
      (throw (err/error :provider/input-invalid
                        (str "unsupported message role " role)
                        {:reason :unsupported-role :role role})))))

(defn- model-request-name
  "The wire model id: the part after the provider prefix of the
  full models.dev id (deepseek/deepseek-v4-flash ->
  deepseek-v4-flash)."
  [model-id]
  (second (str/split model-id #"/")))

(defn- supported-option?
  "The v1 supported call options. :max-tool-rounds is a
  scheduler-level option (the model tool-calling loop bound) — the
  adapter validates it but never serializes it to the wire."
  [k]
  (contains? #{:temperature :max-tokens :seed :reasoning
              :server-side-search :max-tool-rounds} k))

(defn- wire-tools
  "The wire tools declaration from the payload :tools vector:
  each entry {:name :description :parameters :tool} becomes the
  OpenAI function-tool shape; the internal :tool id (the mapping
  back to the EvoCLJ tool) is stripped before serialization."
  [tools]
  (mapv (fn [t]
          (cond-> {:type "function"
                   :function {:name (:name t)
                              :description (or (:description t) "")
                              :parameters (or (:parameters t) {})}}
            (contains? t :tool) (assoc :tool/id (:tool t))))
        tools))

(defn- build-params
  "Build the SDK ChatCompletionCreateParams from the authorized EDN
  request: the body is assembled on the SDK Body builder (model,
  messages, supported options via builder methods, dialect extras
  and the tools declaration via putAdditionalProperty) and attached
  to the outer params."
  [request dialect]
  (let [opts (or (:options request) {})
        extra (dialect/openai-request-extra dialect opts)
        extra (if (seq (:tools request))
                (assoc extra :tools (mapv #(dissoc % :tool/id)
                                          (wire-tools (:tools request))))
                extra)
        b (-> (ChatCompletionCreateParams$Body/builder)
              (.model (model-request-name (:model/id request))))
        b (reduce (fn [b m] (.addMessage b (message->param m)))
                  b (:messages request))
        b (if-let [t (:temperature opts)]
            (.temperature b (double t)) b)
        b (if-let [mt (:max-tokens opts)]
            (.maxTokens b (long mt)) b)
        b (if-let [s (:seed opts)]
            (.seed b (long s)) b)
        b (reduce (fn [b [k v]]
                    (.putAdditionalProperty b (str/replace (name k) "-" "_")
                                            (JsonValue/from (edn->json v))))
                  b extra)
        params (.build (.body (ChatCompletionCreateParams/builder) (.build b)))]
    params))

(defn- json-params
  "The body params that describe the normalized request: the model,
  message count, and options — plain data, no SDK objects."
  [request]
  {:model/id (:model/id request)
   :messages (count (:messages request))
   :options (select-keys (or (:options request) {})
                         [:temperature :max-tokens :seed :reasoning
                          :server-side-search])})

;; --- response handling ----------------------------------------------------------

(defn- read-body
  "Read an HttpResponse body InputStream to a string."
  [^java.io.InputStream is]
  (let [sb (StringBuilder.)]
    (with-open [r (java.io.BufferedReader. (java.io.InputStreamReader. is "UTF-8"))]
      (loop [line (.readLine r)]
        (when line
          (.append sb line)
          (recur (.readLine r)))))
    (str sb)))

(defn- execute-raw!
  "Perform one chat-completions call through the SDK raw-response
  API: returns {:http/status <int> :http/body <json string>}. SDK
  errors are mapped: OpenAIServiceException carries the HTTP status —
  429/5xx and IO/timeout become :provider/transient-error (the
  dispatcher may retry), other statuses become :provider/model-error."
  [client params]
  (try
    (let [resp (.create (.completions (.withRawResponse (.chat client)))
                        params)
          status (.statusCode resp)
          body (read-body (.body resp))]
      {:http/status status :http/body body})
    (catch OpenAIServiceException e
      (let [code (try (.statusCode e) (catch Exception _ 0))]
        (if (or (= code 429) (>= code 500))
          (throw (err/error :provider/transient-error
                            (str "model endpoint " code ": " (.getMessage e))
                            {:status :http-error :http-code code}))
          (throw (err/error :provider/model-error
                            (str "model endpoint rejected the request: HTTP " code)
                            {:status :http-error :http-code code})))))
    (catch OpenAIIoException e
      (throw (err/error :provider/transient-error
                        (str "model endpoint IO error: " (.getMessage e))
                        {:status :io-error})))))

(defn- parse-http-response!
  "Validate the raw HTTP result and parse the JSON body into EDN:
  non-2xx statuses become typed errors; malformed JSON becomes
  :provider/model-error."
  [raw dialect]
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
      (dialect/parse-openai-response dialect parsed))))

;; --- the provider ----------------------------------------------------------------

(defn openai-compatible-provider
  "Build one OpenAI-compatible provider.

  opts:
    :provider/id   keyword (the models.dev provider id, e.g. :deepseek)
    :base-url      string (the endpoint base, e.g.,
                   https://api.deepseek.com)
    :api-key       string (closed over; never described)
    :headers       {string string} extra request headers
                   (azure api-key header, etc.)
    :query-params  {string string} extra query params
                   (azure api-version, etc.)
    :model-entries {full-model-id -> models.dev index entry}
                   the slice of the catalog this endpoint serves
    :timeout-ms    int (default 60000)
    :execution-count atom (optional; bumped once per execute, so
                   tests can assert the provider really ran)
    :azure?        bool — when true the client uses the Azure
                   service-version/path-mode defaults (openai-java
                   native Azure support); the operator still supplies
                   the per-resource base-url and api-key
    :api-key-env   string (optional; recorded for diagnostics only
                   — never the key itself)
  Returns a Provider satisfying evoclj.provider.protocol."
  [opts]
  (let [{provider-id :provider/id base-url :base-url api-key :api-key
         headers :headers query-params :query-params model-entries :model-entries
         timeout-ms :timeout-ms execution-count :execution-count
         azure? :azure? api-key-env :api-key-env} opts
        timeout-ms (or timeout-ms 60000)
        served (set (keys model-entries))
        client-builder (-> (OpenAIOkHttpClient/builder)
                           (.baseUrl base-url)
                           (.timeout (java.time.Duration/ofMillis timeout-ms))
                           (.maxRetries 0))
        client-builder (if api-key
                         (.apiKey client-builder api-key)
                         client-builder)
        client-builder (reduce-kv (fn [b k v] (.putHeader b k v))
                                  client-builder (or headers {}))
        client-builder (reduce-kv (fn [b k v] (.putQueryParam b k v))
                                  client-builder (or query-params {}))
        client-builder (if azure?
                         (-> client-builder
                             (.azureServiceVersion (com.openai.azure.AzureOpenAIServiceVersion/getV2024_06_01))
                             (.azureUrlPathMode com.openai.azure.AzureUrlPathMode/UNIFIED))
                         client-builder)
        client (.build client-builder)
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
              dialect (:model/dialect entry)
              params (build-params request dialect)
              raw (execute-raw! client params)
              parsed (parse-http-response! raw dialect)
              usage (:usage parsed)
              cost (dialect/estimate-cost (:model/cost entry) usage)
              result (dialect/provider-result (:model/output parsed) usage cost)]
          (if (:tool-calls parsed)
            (assoc result :tool-calls (:tool-calls parsed))
            result))))))

(defn served-models
  "The full model ids an endpoint serves (for diagnostics)."
  [provider]
  (keys (:model-entries @(atom nil))))
