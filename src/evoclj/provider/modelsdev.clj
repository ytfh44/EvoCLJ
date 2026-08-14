(ns evoclj.provider.modelsdev
  "models.dev catalog service (post-v0 extension 1 — real model providers).

  This namespace turns the community-maintained model catalog at
  https://models.dev/api.json (anomalyco/models.dev) into a
  kernel-owned, refreshable, cacheable model index. It is the
  data root for every real LLM provider: which models exist, which
  API style each provider speaks (OpenAI-compatible vs Anthropic
  messages), which base URL and API-key environment variable to use,
  and which dialect quirks a model has.

  THE CATALOG IS FETCHED AT STARTUP, NOT EMBEDDED: refresh-catalog!
  pulls the current api.json over HTTPS, validates its shape, writes
  it atomically into the state directory, and builds a normalized
  index. On a network failure the previously cached copy is used
  (status :catalog/cached); with neither network nor cache the
  catalog is unavailable (:catalog/unavailable) and real model
  resolution fails closed — fixture providers keep working.

  CLASSIFICATION (the JVM answer to Vercel AI SDK): models.dev
  labels every provider with the AI SDK provider package it is
  consumed through (the :npm field, e.g. \"@ai-sdk/openai-compatible\"
  or \"@ai-sdk/anthropic\"). That JS package cannot run here, so this
  namespace maps the label to one of the two API styles EvoCLJ
  implements (evoclj.provider.openai, evoclj.provider.anthropic):

    :openai-compatible — /chat/completions style REST, driven by the
      official openai-java client with a base-url override
    :anthropic         — /v1/messages style REST, driven by the
      official anthropic-java client

  Many providers whose primary SDK label is something else (azure,
  mistral, xai, groq, togetherai, cerebras, fireworks-ai, deepinfra,
  perplexity, google/gemini, nvidia, novita-ai, lmstudio, ollama,
  ...) expose an OpenAI-compatible endpoint in practice — the user
  explicitly treats these as dialects of the OpenAI API rather than
  separate formats — so style-overrides maps them onto
  :openai-compatible too. Every model ends up with a :model/status:

    :supported     — style AND base-url are known; usable when the
                     host has the API key (env check happens in the
                     host, never here — the catalog is pure data)
    :needs-config  — style known but no base URL; an operator may
                     supply one via the :catalog/base-urls config
    :unsupported   — no OpenAI-compatible or Anthropic-compatible
                     endpoint is known; listed so operators can see
                     the full catalog honestly

  DIALECT MARKERS: each model entry carries a :model/dialect map
  derived from the catalog plus the built-in override table:

    {:interleaved :reasoning-content}   — DeepSeek-style reasoning
      field inside chat-completion messages (the catalog
      :interleaved {:field \"reasoning_content\"} marker)
    :reasoning-options [..]             — how the model toggles
      reasoning (toggle vs effort levels), normalized keywords
    :server-side-search :off|:web-search-options|:web-search-tool
      — :web-search-options uses openai-java WebSearchOptions
        (native OpenAI chat-completions web search); :web-search-tool
        injects a tools-based web_search declaration for compatible
        endpoints that speak that dialect
    :extra-params {..}                  — additionalProperties the
        adapter must merge into every chat-completions request body

  Error contract: :catalog/fetch-failed (network/timeout/HTTP status;
  :status carries the code), :catalog/parse-invalid (malformed JSON
  or schema violations), :catalog/cache-invalid (cache read failure),
  :catalog/config-invalid (bad catalog config). refresh-catalog!
  never throws on a stale/absent cache when the fetch fails: it
  reports the situation as data (:status :catalog/unavailable) so the
  host can decide (fail-closed for model resolution)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [malli.core :as m])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest
                           HttpResponse HttpResponse$BodyHandlers)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files Path StandardCopyOption)
           (java.time Duration Instant)))

;; --- config and schemas ------------------------------------------------------

(def CatalogConfigSchema
  "The catalog configuration contract: where to fetch, where to cache,
  how fresh to keep the copy, and the operator override tables. All
  paths are optional host decisions; the defaults live in
  evoclj.kernel.system."
  [:map {:closed false}
   [:catalog/url string?]
   [:catalog/cache-dir string?]
   [:catalog/ttl-hours int?]
   [:catalog/timeout-ms {:optional true} int?]
   [:catalog/base-urls {:optional true} [:map-of keyword? string?]]
   [:catalog/style-overrides {:optional true} [:map-of keyword? keyword?]]
   [:catalog/dialect-overrides {:optional true} [:map-of keyword? :map]]])

(defn validate-config!
  "Validate catalog config; throws :catalog/config-invalid carrying a
  serializable Malli explanation."
  [config]
  (when-not (m/validate CatalogConfigSchema config)
    (throw (err/error :catalog/config-invalid
                      "models.dev catalog config failed schema validation"
                      {:value (err/sanitize config)
                       :explanation (err/sanitize (m/explain CatalogConfigSchema config))})))
  config)

;; --- well-known endpoint tables ---------------------------------------------

(def ^:private well-known-openai-base-urls
  "Base URLs for providers whose OpenAI-compatible endpoint is
  public knowledge and does not appear in the catalog :api field
  (the catalog only carries :api for some providers). Entries are the
  full base URL including any /v1 path, exactly as openai-java
  baseUrl option expects."
  {"openai" "https://api.openai.com/v1"
   "mistral" "https://api.mistral.ai/v1"
   "xai" "https://api.x.ai/v1"
   "groq" "https://api.groq.com/openai/v1"
   "togetherai" "https://api.together.xyz/v1"
   "cerebras" "https://api.cerebras.ai/v1"
   "fireworks-ai" "https://api.fireworks.ai/inference/v1"
   "deepinfra" "https://api.deepinfra.com/v1/openai"
   "perplexity" "https://api.perplexity.ai"
   "openrouter" "https://openrouter.ai/api/v1"
   "google" "https://generativelanguage.googleapis.com/v1beta/openai"
   "nvidia" "https://integrate.api.nvidia.com/v1"
   "novita-ai" "https://api.novita.ai/v3/openai"
   "upstage" "https://api.upstage.ai/v1/solar"
   "moonshotai" "https://api.moonshot.cn/v1"
   "zhipuai" "https://open.bigmodel.cn/api/paas/v4"
   "siliconflow" "https://api.siliconflow.cn/v1"
   "alibaba" "https://dashscope.aliyuncs.com/compatible-mode/v1"
   "lmstudio" "http://localhost:1234/v1"
   "ollama" "http://localhost:11434/v1"
   "github-copilot" "https://models.github.ai/inference"})

(def ^:private style-overrides
  "Provider -> :openai-compatible | :anthropic overrides for providers
  whose primary models.dev SDK label is a proprietary package but
  which expose an OpenAI-compatible (or Anthropic-compatible) REST
  endpoint in practice. Azure OpenAI is the canonical example: it is
  an OpenAI-compatible dialect with its own auth headers and an
  api-version query parameter — handled by openai-java native Azure
  support. :needs-config providers still need an operator-supplied
  base URL (azure endpoints are per-resource; vertex/bedrock need
  cloud credentials)."
  {"azure" :openai-compatible
   "mistral" :openai-compatible
   "xai" :openai-compatible
   "groq" :openai-compatible
   "togetherai" :openai-compatible
   "cerebras" :openai-compatible
   "fireworks-ai" :openai-compatible
   "deepinfra" :openai-compatible
   "perplexity" :openai-compatible
   "google" :openai-compatible
   "google-vertex" :openai-compatible
   "amazon-bedrock" :openai-compatible
   "nvidia" :openai-compatible
   "novita-ai" :openai-compatible
   "upstage" :openai-compatible
   "moonshotai" :openai-compatible
   "zhipuai" :openai-compatible
   "siliconflow" :openai-compatible
   "alibaba" :openai-compatible
   "lmstudio" :openai-compatible
   "ollama" :openai-compatible
   "github-copilot" :openai-compatible
   "google-vertex-anthropic" :anthropic
   "amazon-bedrock-anthropic" :anthropic})

(def ^:private anthropic-well-known-base-urls
  {"anthropic" "https://api.anthropic.com"})

(def ^:private dialect-defaults
  "Built-in per-provider dialect defaults merged under the catalog
  data. :server-side-search — :off (default), :web-search-options
  (openai-java native WebSearchOptions; openai supports it for its
  web-search-capable chat models), :web-search-tool (tools-based
  web_search declaration for compatible endpoints that speak that
  dialect). :extra-params merge into every request body via
  additionalProperties."
  {"openai" {:server-side-search :web-search-options}
   "perplexity" {:server-side-search :web-search-tool}})

;; --- fetching ----------------------------------------------------------------

(defn- http-get
  "GET url with a bounded timeout; returns the body as a string.
  Throws :catalog/fetch-failed on network errors, timeouts, or a
  non-2xx status (carrying :status/:http-code)."
  [url timeout-ms]
  (let [client (-> (HttpClient/newBuilder)
                   (.followRedirects HttpClient$Redirect/NORMAL)
                   (.connectTimeout (Duration/ofMillis timeout-ms))
                   (.build))]
    (try
      (let [req (-> (HttpRequest/newBuilder (URI. url))
                    (.header "User-Agent" "evoclj-modelsdev/1")
                    (.header "Accept" "application/json")
                    (.timeout (Duration/ofMillis timeout-ms))
                    (.GET)
                    (.build))
            resp (.send client req (HttpResponse$BodyHandlers/ofString))]
        (if (<= 200 (.statusCode resp) 299)
          (.body resp)
          (throw (err/error :catalog/fetch-failed
                            (str "models.dev returned HTTP " (.statusCode resp))
                            {:status :http-error :http-code (.statusCode resp)}))))
      (catch Exception e
        (throw (err/error :catalog/fetch-failed
                          (str "models.dev fetch failed: " (.getMessage e))
                          {:status :network-error}))))))

;; --- parsing and validation --------------------------------------------------

(def ^:private ProviderEntrySchema
  [:map {:closed false}
   [:id string?]
   [:env [:vector string?]]
   [:npm {:optional true} string?]
   [:name {:optional true} string?]
   [:api {:optional true} string?]
   [:models [:map-of keyword? :map]]])

(def ^:private ModelEntrySchema
  [:map {:closed false}
   [:id string?]
   [:name {:optional true} string?]
   [:reasoning {:optional true} boolean?]
   [:reasoning_options {:optional true} [:vector :map]]
   [:tool_call {:optional true} boolean?]
   [:interleaved {:optional true} [:or :map boolean?]]
   [:structured_output {:optional true} boolean?]
   [:temperature {:optional true} boolean?]
   [:attachment {:optional true} boolean?]
   [:knowledge {:optional true} string?]
   [:release_date {:optional true} string?]
   [:last_updated {:optional true} string?]
   [:modalities {:optional true} :map]
   [:limit {:optional true} :map]
   [:cost {:optional true} :map]])

(defn parse-catalog
  "Parse and validate a raw api.json body into a normalized catalog:

    {:catalog/fetched-at <inst>
     :catalog/providers {<provider-id> {<provider-entry>}}
     :catalog/models    {<model-id> {<normalized-model-entry>}}}

  Throws :catalog/parse-invalid when the body is not valid JSON or
  fails the schema contract. Validation never coerces: a provider or
  model entry that does not match is rejected, not silently dropped."
  [body fetched-at]
  (let [parsed (try
                 (json/parse-string body true)
                 (catch Exception e
                   (throw (err/error :catalog/parse-invalid
                                     "models.dev api.json is not valid JSON"
                                     {:reason :bad-json
                                      :message (str (.getMessage e))}))))]
    (when-not (map? parsed)
      (throw (err/error :catalog/parse-invalid
                        "models.dev api.json must be a provider map"
                        {:reason :not-a-map})))
    (doseq [[pid entry] parsed]
      (when-not (and (map? entry) (m/validate ProviderEntrySchema entry))
        (throw (err/error :catalog/parse-invalid
                          (str "provider entry " pid " failed schema validation")
                          {:provider pid
                           :explanation (err/sanitize
                                         (m/explain ProviderEntrySchema entry))}))))
    (doseq [[pid entry] parsed
            [mid model] (:models entry)]
      (when-not (m/validate ModelEntrySchema model)
        (throw (err/error :catalog/parse-invalid
                          (str "model entry " mid " of provider " pid
                               " failed schema validation")
                          {:provider pid :model mid
                           :explanation (err/sanitize
                                         (m/explain ModelEntrySchema model))}))))
    {:catalog/fetched-at fetched-at
     :catalog/providers parsed
     :catalog/models {}}))

;; --- normalization -----------------------------------------------------------

(defn- npm-style
  "Map a models.dev :npm package label to an EvoCLJ API style
  keyword, or nil when the label is neither OpenAI-compatible nor
  Anthropic."
  [npm]
  (cond
    (or (= npm "@ai-sdk/openai-compatible")
        (= npm "@ai-sdk/openai")) :openai-compatible
    (= npm "@ai-sdk/anthropic") :anthropic
    :else nil))

(defn- provider-style
  "The API style EvoCLJ will use for a provider: the npm-derived
  style, overridden by the built-in style-overrides table and the
  operator :catalog/style-overrides config. Returns
  :openai-compatible, :anthropic, or nil."
  [provider-id npm overrides]
  (or (get overrides (keyword provider-id))
      (get style-overrides (name provider-id))
      (npm-style npm)))

(defn- resolve-base-url
  "Base URL for a provider: operator config override > catalog :api
  field > well-known table > nil."
  [provider-id provider-entry config]
  (or (get-in config [:catalog/base-urls (keyword provider-id)])
      (:api provider-entry)
      (get well-known-openai-base-urls (name provider-id))
      (get anthropic-well-known-base-urls (name provider-id))))

(defn- normalize-reasoning-options
  "Normalize the catalog :reasoning_options vector into keywords:
  {:type :toggle} and {:type :effort :values [low high max]}."
  [options]
  (mapv (fn [o]
          (cond-> {:type (keyword (:type o))}
            (:values o) (assoc :values (vec (:values o)))))
        options))

(defn- normalize-dialect
  "Build the :model/dialect map for one model: interleaved reasoning
  field, reasoning options, server-side search mode, and extra
  request params, from the catalog entry plus provider dialect
  defaults and operator overrides."
  [provider-id model config]
  (let [defaults (or (get dialect-defaults (name provider-id)) {})
        overrides (get-in config [:catalog/dialect-overrides (keyword provider-id)] {})
        interleaved (or (:interleaved overrides)
                        (:interleaved model))
        field (cond
                (and (map? interleaved) (:field interleaved))
                (keyword (str/replace (:field interleaved) "-" "_"))
                (true? interleaved) :interleaved
                :else :none)]
    {:interleaved field
     :reasoning-options (or (:reasoning-options overrides)
                            (normalize-reasoning-options (:reasoning_options model))
                            [])
     :server-side-search (or (:server-side-search overrides)
                             (:server-side-search defaults)
                             :off)
     :extra-params (merge (:extra-params defaults) (:extra-params overrides))}))

(defn normalize-model-index
  "Build the full model index {:model/id -> entry} from a parsed
  catalog, resolving style, base URLs, and dialect markers per model.

  config may carry :catalog/base-urls, :catalog/style-overrides,
  and :catalog/dialect-overrides."
  [parsed config]
  (into {}
        (for [[provider-id provider] (:catalog/providers parsed)
              [mid _] (:models provider)
              :let [npm (:npm provider)
                    style (provider-style provider-id npm config)
                    base-url (resolve-base-url provider-id provider config)
                    model (get-in parsed [:catalog/providers provider-id :models mid])
                    status (cond
                             (and style base-url) :supported
                             style :needs-config
                             :else :unsupported)]]
          [(str (name provider-id) "/" (name mid))
           (merge {:model/dialect (normalize-dialect provider-id model config)}
                  {:model/id (str (name provider-id) "/" (name mid))
                   :model/provider provider-id
                   :model/name (:name model (:id model))
                   :model/style style
                   :model/status status
                   :model/base-url base-url
                   :model/api-key-env (when (seq (:env provider)) (first (:env provider)))
                   :model/capabilities {:reasoning (boolean (:reasoning model))
                                        :tools (boolean (:tool_call model))
                                        :structured-output (boolean (:structured_output model))
                                        :temperature (boolean (:temperature model))
                                        :attachment (boolean (:attachment model))}
                   :model/limits (cond-> {}
                                    (:limit model) (assoc :context (get-in model [:limit :context])
                                                          :output (get-in model [:limit :output])))
                   :model/cost (:cost model)
                   :model/knowledge (:knowledge model)
                   :model/release-date (:release_date model)})])))

;; --- caching ------------------------------------------------------------------

(defn- cache-meta-path
  [cache-dir]
  (java.nio.file.Paths/get cache-dir (make-array String 0)))

(defn write-cache!
  "Atomically persist a fetched catalog under cache-dir:
  api.json (the raw body, byte-identical) plus meta.edn recording
  :catalog/fetched-at and :catalog/source-url. Writes go through a
  temp file + atomic move so a crash can never leave a partial
  catalog. Returns {:catalog/cache-dir ... :catalog/cache-file ...}."
  [cache-dir body fetched-at source-url]
  (let [dir (java.nio.file.Paths/get cache-dir (make-array String 0))
        _ (Files/createDirectories dir (make-array java.nio.file.attribute.FileAttribute 0))
        body-file (.resolve dir "api.json")
        meta-file (.resolve dir "meta.edn")
        tmp-body (.resolve dir (str "api.json.tmp-" (System/nanoTime)))
        tmp-meta (.resolve dir (str "meta.edn.tmp-" (System/nanoTime)))]
    (try
      (Files/writeString tmp-body body StandardCharsets/UTF_8
                        (make-array java.nio.file.OpenOption 0))
      (Files/writeString tmp-meta
                         (pr-str {:catalog/fetched-at fetched-at
                                  :catalog/source-url source-url})
                         StandardCharsets/UTF_8
                         (make-array java.nio.file.OpenOption 0))
      ;; REPLACE_EXISTING: a previous startup's cache is overwritten
      ;; (Windows move does not overwrite without it)
      (Files/move tmp-body body-file
                  (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
      (Files/move tmp-meta meta-file
                  (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
      {:catalog/cache-dir cache-dir :catalog/cache-file (str body-file)}
      (catch Exception e
        (throw (err/error :catalog/cache-invalid
                          (str "failed to write catalog cache: " (.getMessage e))
                          {:cache-dir cache-dir}))))))

(defn read-cache
  "Read the cached catalog: {:catalog/fetched-at <inst>
  :catalog/source-url <str> :catalog/body <raw json str>} or nil when
  no cache exists. A corrupt cache throws :catalog/cache-invalid."
  [cache-dir]
  (let [dir (java.nio.file.Paths/get cache-dir (make-array String 0))
        body-file (.resolve dir "api.json")
        meta-file (.resolve dir "meta.edn")]
    (when (Files/exists body-file (make-array java.nio.file.LinkOption 0))
      (try
        (let [meta (if (Files/exists meta-file (make-array java.nio.file.LinkOption 0))
                     (read-string (Files/readString meta-file StandardCharsets/UTF_8))
                     {})]
          {:catalog/fetched-at (:catalog/fetched-at meta)
           :catalog/source-url (:catalog/source-url meta)
           :catalog/body (Files/readString body-file StandardCharsets/UTF_8)})
        (catch Exception e
          (throw (err/error :catalog/cache-invalid
                            (str "catalog cache is corrupt: " (.getMessage e))
                            {:cache-dir cache-dir})))))))

;; --- the startup entry point ---------------------------------------------------

(defn- cache-result
  "Build the :catalog/cached or :catalog/unavailable result from the
  cached copy (or its absence)."
  [config cache-dir cached-attempt]
  (let [cached (try
                 (read-cache cache-dir)
                 (catch Exception e
                   {:error (err/error-data e)}))]
    (if (and (map? cached) (:catalog/body cached))
      (let [parsed (try
                     (parse-catalog (:catalog/body cached)
                                    (or (:catalog/fetched-at cached)
                                        (java.util.Date.)))
                     (catch Exception e
                       {:error (err/error-data e)}))]
        (if (:catalog/providers parsed)
          {:catalog/status :catalog/cached
           :catalog/data (assoc parsed
                                :catalog/models
                                (normalize-model-index parsed config))}
          {:catalog/status :catalog/unavailable
           :catalog/error (or (:error parsed) (:error cached))}))
      {:catalog/status :catalog/unavailable
       :catalog/error (or (:error cached) cached-attempt)})))

(defn refresh-catalog!
  "The startup entry point: refresh the catalog, never throw on an
  unreachable or malformed source.

  Returns {:catalog/status :catalog/fresh | :catalog/cached |
           :catalog/unavailable
           :catalog/data {:catalog/fetched-at ... :catalog/providers ...
                          :catalog/models <model index>}
           :catalog/error <typed error data when unavailable>}

  :catalog/fresh  — fetched, parsed, and validated from
                    :catalog/url; the cache is updated.
  :catalog/cached — the fetch failed (network, timeout, or a body
                    that failed validation); the cached copy was
                    used (its fetched-at is reported).
  :catalog/unavailable — no network and no cache: model resolution
                    must fail closed."
  [config]
  (validate-config! config)
  (let [url (:catalog/url config)
        timeout-ms (or (:catalog/timeout-ms config) 30000)
        cache-dir (:catalog/cache-dir config)
        attempt (try
                  {:status :fresh
                   :body (http-get url timeout-ms)}
                  (catch Exception e
                    {:status :fetch-failed
                     :error (err/error-data e)}))]
    (if (= :fresh (:status attempt))
      (let [fetched-at (java.util.Date.)
            parsed (try
                     (parse-catalog (:body attempt) fetched-at)
                     (catch Exception e
                       {:error (err/error-data e)}))]
        (if (:catalog/providers parsed)
          (do
            (write-cache! cache-dir (:body attempt) fetched-at url)
            {:catalog/status :catalog/fresh
             :catalog/data (assoc parsed
                                  :catalog/models
                                  (normalize-model-index parsed config))})
          ;; fetched but unparseable — fall back to the cache rather
          ;; than failing startup on a transient upstream glitch
          (cache-result config cache-dir (:error parsed))))
      (cache-result config cache-dir (:error attempt)))))

;; --- lookup --------------------------------------------------------------------

(defn lookup-model
  "Look up one model in the index by its full id
  (deepseek/deepseek-v4-flash); nil when unknown."
  [index model-id]
  (get index model-id))

(defn list-models
  "All model entries, optionally filtered by :model/style or
  :model/status; sorted by id for stable output."
  [index & {:keys [style status provider]}]
  (->> (vals index)
       (filter (fn [m]
                 (and (or (nil? style) (= style (:model/style m)))
                      (or (nil? status) (= status (:model/status m)))
                      (or (nil? provider) (= provider (:model/provider m))))))
       (sort-by :model/id)
       vec))
