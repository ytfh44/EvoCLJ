(ns evoclj.provider.request
  "Unified ModelRequest / ModelResponse and build/parse helpers.

  Single source for EDN-safe conversion, wire tool shaping, and
  per-provider option allowlists. Providers delegate here so
  edn->json and supported-option? are defined once (INV-05).
  Parse dispatch is the single entry point (parse-response
  provider raw) and fixes the anthropic tool_use dropping.
  :code is parsed but not executed (carried through as {:language :source})."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.provider.dialect :as dialect]))

;; --- ModelRequest / ModelResponse contracts ---------------------------------

(def ModelRequest
  "Canonical ModelRequest shape shared by all providers.
  :model/id is the fully-qualified models.dev id (string
  '<provider>/<name>'), :messages is the EDN message vector,
  :tools is the optional wire tool vector, :options holds
  provider-specific call options."
  [:map
   [:model/id string?]
   [:messages [:vector :map]]
   [:tools {:optional true} [:vector :map]]
   [:options {:optional true} :map]
   [:dialect {:optional true} :map]])

(def ModelResponse
  "Canonical ModelResponse EDN produced by parse-response.
  :text is the concatenated visible text, :reasoning is an
  optional interleaved reasoning trace, :tool-calls is the
  optional vector of parsed tool calls, :usage carries token
  counters, :code is an optional parsed code block
  {:language <keyword> :source <string>} (parsed but not executed)."
  [:map
   [:text string?]
   [:reasoning {:optional true} string?]
   [:tool-calls {:optional true}
    [:vector [:map
              [:tool/call-id string?]
              [:tool/name string?]
              [:tool/arguments :map]]]]
   [:usage {:optional true} :map]
   [:code {:optional true} [:map [:language keyword?] [:source string?]]]])

;; --- EDN -> JSON (single definition) ----------------------------------------

(defn edn->json
  "Convert EDN-safe data into JSON-compatible Java values for
  SDK additionalProperties: keywords/symbols become strings (names
  only), sets become vectors, maps keep keyword-free string keys.
  Throws :provider/input-invalid on anything non-EDN."
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

;; --- wire-tools (single definition) -----------------------------------------

(defn wire-tools
  "The wire tools declaration from the payload :tools vector:
  each entry {:name :description :parameters :tool} becomes the
  OpenAI function-tool shape; the internal :tool id is stripped
  before serialization. Shared by openai-compatible endpoints."
  [tools]
  (mapv (fn [t]
          (cond-> {:type "function"
                   :function {:name (:name t)
                              :description (or (:description t) "")
                              :parameters (or (:parameters t) {})}}
            (contains? t :tool) (assoc :tool/id (:tool t))))
        tools))

;; --- per-provider option allowlist ------------------------------------------

(def ^:private supported-options
  {:openai #{:temperature :max-tokens :seed :reasoning
             :server-side-search :max-tool-rounds}
   :anthropic #{:temperature :max-tokens :max-tool-rounds}})

(defn supported-option?
  "True when option k is supported for provider (keyword
  :openai or :anthropic)."
  [provider k]
  (contains? (get supported-options provider #{}) k))

(defn supported-options-for
  "Return the allowed option set for provider."
  [provider]
  (get supported-options provider #{}))

;; --- build helpers (thin EDN wire builders used by providers) ---------------

(defn build-request
  "Build provider-specific wire EDN params from a ModelRequest.

  provider is :openai or :anthropic. request is
  {:model/id string :messages [...] :tools [...] :options {...}}.
  dialect is the models.dev dialect map (only used for :openai).

  Returns an EDN wire map that the provider SDK builder can
  translate to its SDK params. Pure EDN, no SDK objects."
  [provider request dialect]
  (case provider
    :openai
    (let [opts (or (:options request) {})
          extra (dialect/openai-request-extra dialect opts)
          extra (if (seq (:tools request))
                  (assoc extra :tools (mapv #(dissoc % :tool/id)
                                            (wire-tools (:tools request))))
                  extra)]
      {:model (second (str/split (:model/id request) #"/"))
       :messages (:messages request)
       :opts opts
       :extra extra})
    :anthropic
    (let [opts (or (:options request) {})
          messages (:messages request)
          system-prompt (->> (filter #(= :system (:role %)) messages)
                             (map :content)
                             (str/join "\n"))
          turns (remove #(= :system (:role %)) messages)]
      {:model (second (str/split (:model/id request) #"/"))
       :system system-prompt
       :turns (vec turns)
       :opts opts
       :extra-params (get-in request [:options :extra-params] {})})
    (throw (err/error :provider/input-invalid
                      (str "unknown provider " provider)
                      {:provider provider}))))

;; --- parse dispatch (single entry point) ------------------------------------

(defn parse-response
  "Single dispatch point: parse a decoded JSON response map into
  the canonical provider result.

  provider is :openai or :anthropic.
  For :openai, dialect supplies the interleaved reasoning field.
  raw is the decoded JSON map (cheshire parse with keyword keys).

  Returns {:model/output {:text ... :reasoning ...}
           :usage {:input-tokens ... :output-tokens ... :reasoning-tokens ...}
           :tool-calls [...]?} mirroring dialect outputs; the
  Anthropic path now retains tool_use blocks as :tool-calls
  (fixing the earlier dropping)."
  ([provider raw]
   (parse-response provider nil raw))
  ([provider dialect raw]
   (case provider
     :openai (dialect/parse-openai-response dialect raw)
     :anthropic (dialect/parse-anthropic-response raw)
     (throw (err/error :provider/input-invalid
                       (str "unknown provider for parse " provider)
                       {:provider provider})))))
