(ns evoclj.provider.dialect
  "OpenAI-compatible and Anthropic dialect layer (pure EDN).

  The user requirement: OpenAI-compatible APIs have many dialects —
  DeepSeek returns reasoning in an interleaved reasoning_content
  field, some endpoints offer server-side search, others accept
  vendor-specific request params. Vercel AI SDK (JS) hides these
  differences; on the JVM we implement the classification ourselves.
  This namespace is the pure-data heart of that classification:
  every transform takes EDN and returns EDN, with no SDK objects,
  no network, no host state (Global Constraint 22 — only validated
  Clojure data crosses module boundaries).

  REQUEST SIDE — openai-request-extra turns a dialect spec and the
  caller request options into the extra body params that must be
  merged into a chat-completions request:

    {:reasoning {:mode :effort :level \"high\"}
     :server-side-search true
     :tools [...]}
    -> {:reasoning_effort \"high\"
        :web-search-options {:search_context_size :medium}
        :tools [...]}

  RESPONSE SIDE — parse-openai-response / parse-anthropic-response
  turn a decoded JSON response map into the canonical provider
  result:

    {:model/output {:text \"...\"
                    :reasoning \"...\"}   ; only when the dialect
                                        ; interleaves reasoning
     :usage {:input-tokens 10 :output-tokens 5}}

  The interleaved field name comes from the models.dev catalog
  (:interleaved {:field \"reasoning_content\"} for DeepSeek-style
  models) and is normalized to a keyword (:reasoning_content). The
  reasoning text is extracted from the assistant message under that
  field and NEVER merged into the visible :text (the model output
  boundary stays clean: text is text, reasoning is reasoning).

  COST — estimate-cost computes USD cost units from the models.dev
  per-model pricing table: input_tokens * prompt rate +
  output_tokens * completion rate (reasoning tokens are billed at
  the output rate when a :reasoning rate exists, matching DeepSeek
  pricing semantics).

  No secrets, no fns, no host objects: this namespace is safe to
  unit test exhaustively offline."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

;; --- request side -------------------------------------------------------------

(def ^:private openai-web-search-options-default
  {:search_context_size :medium})

(defn openai-request-extra
  "Compute the extra chat-completions body params for one request.

  `dialect` is the :model/dialect map from the models.dev index:
  {:interleaved ... :reasoning-options [...] :server-side-search
   :off|:web-search-options|:web-search-tool :extra-params {...}}.

  `request-opts` is the caller request options map:
  {:reasoning {:mode :toggle|:effort :level \"high\"}
   :server-side-search bool?
   :tools [tool-maps]}.

  Returns the map of extra params to merge into the request body
  (keys may be keywords or strings, matching how the adapter
  serializes them). Rules:

  - :reasoning {:mode :effort :level L} emits :reasoning_effort L
    (the OpenAI-compatible standard accepted by DeepSeek, Qwen,
    GLM and friends). A :toggle mode emits :reasoning <bool> only
    when the dialect declares :reasoning-toggle-param; otherwise it
    is rejected as :dialect/unsupported-reasoning-mode.
  - :server-side-search true with dialect :web-search-options
    emits :web-search-options; with :web-search-tool it appends a
    {:type \"web_search\"} tool to the request tools.
  - :extra-params from the dialect merge under everything and win
    on conflicts (operator-provided dialect knowledge is final)."
  [dialect request-opts]
  (let [dialect (or dialect {})
        opts (or request-opts {})
        reasoning (:reasoning opts)
        search? (and (:server-side-search opts)
                     (not= :off (:server-side-search dialect)))
        extra (or (:extra-params dialect) {})
        base (cond-> (when (seq (:tools opts)) {:tools (vec (:tools opts))})
                (and reasoning (= :effort (:mode reasoning)) (:level reasoning))
                (assoc :reasoning_effort (:level reasoning))
                (and reasoning (= :toggle (:mode reasoning))
                      (:reasoning-toggle-param dialect))
                (assoc (:reasoning-toggle-param dialect)
                       (boolean (:enabled reasoning true)))
                (and search? (= :web-search-options
                                (:server-side-search dialect)))
                (assoc :web-search-options
                       (merge openai-web-search-options-default
                              (:web-search-options opts)))
                (and search? (= :web-search-tool
                                (:server-side-search dialect)))
                (update :tools (fnil conj []) {:type "web_search"}))]
    (if (and reasoning (= :toggle (:mode reasoning))
             (not (:reasoning-toggle-param dialect)))
      (throw (ex-info "dialect does not support a reasoning toggle"
                      {:error/type :dialect/unsupported-reasoning-mode
                       :dialect dialect}))
      (merge extra base))))

(defn reasoning-options->mode
  "Pick a reasoning mode from the dialect reasoning-options:
  :effort when an effort option exists (its values as :levels),
  :toggle when a toggle option exists, else nil."
  [dialect]
  (let [opts (:reasoning-options dialect [])]
    (cond
      (some #(= :effort (:type %)) opts)
      {:mode :effort
       :levels (vec (sort (set (some :values (filter #(= :effort (:type %)) opts)))))}
      (some #(= :toggle (:type %)) opts) {:mode :toggle}
      :else nil)))

;; --- response side ------------------------------------------------------------

(defn- parse-tool-arguments
  "Parse a tool-call arguments JSON string into a map; malformed or
  non-map arguments become an empty map (the call still executes
  with no args — the tool's own input schema rejects bad shapes)."
  [s]
  (if (and (string? s) (seq s))
    (try
      (let [parsed (json/parse-string s true)]
        (if (map? parsed) parsed {}))
      (catch Exception _ {}))
    {}))

(defn parse-openai-response
  "Parse a decoded OpenAI-compatible chat-completions response map
  into the canonical provider result.

  `dialect` supplies the interleaved reasoning field name (as a
  keyword, e.g. :reasoning_content) — the text under that field in
  the first choice message becomes :model/output :reasoning. A nil
  or :none interleaved dialect extracts no reasoning.

  Returns {:model/output {:text <str> :reasoning <str or absent>}
           :usage {:input-tokens <int> :output-tokens <int>}}.
  Throws :provider/output-invalid when the response has no choices
  or the first choice has no message."
  [dialect response]
  (let [dialect (or dialect {})
        interleaved (:interleaved dialect :none)
        choices (:choices response)
        choice (first choices)
        message (when (map? choice) (:message choice))
        text (when (map? message) (:content message))
        reasoning (when (and (not= :none interleaved) (map? message))
                    (or (get message interleaved)
                        (get message (name interleaved))))
        raw-tools (when (map? message) (:tool_calls message))
        tool-calls (when (vector? raw-tools)
                     (keep (fn [tc]
                             (when (and (map? tc) (map? (:function tc)))
                               {:tool/call-id (:id tc)
                                :tool/name (get-in tc [:function :name])
                                :tool/arguments (parse-tool-arguments
                                                 (get-in tc [:function :arguments]))}))
                           raw-tools))
        tool-calls (seq tool-calls)
        usage (:usage response)]
    (when-not (and (vector? choices) (seq choices) (map? message))
      (throw (ex-info "openai response has no usable first choice"
                      {:error/type :provider/output-invalid
                       :response (select-keys response [:id :choices])})))
    (cond-> {:model/output (cond-> {:text (or text "")}
                              (and reasoning (not (str/blank? (str reasoning))))
                              (assoc :reasoning (str reasoning)))
             :usage {:input-tokens (get-in usage [:prompt_tokens] 0)
                     :output-tokens (get-in usage [:completion_tokens] 0)}}
      tool-calls (assoc :tool-calls (vec tool-calls))
      (map? usage) (assoc :model/raw-usage (select-keys usage
                                                 [:prompt_tokens
                                                  :completion_tokens
                                                  :total_tokens
                                                  :prompt_tokens_details
                                                  :completion_tokens_details])))))

(defn tool-calls->wire
  "Convert the parsed tool-call records [{:tool/call-id :tool/name
  :tool/arguments <map>} ...] into the OpenAI wire shape for an
  assistant message: [{:id <str> :type \"function\" :function
  {:name <str> :arguments <json-string>}} ...] — the exact shape
  the follow-up request must echo back."
  [tool-calls]
  (mapv (fn [tc]
          {:id (:tool/call-id tc)
           :type "function"
           :function {:name (:tool/name tc)
                      :arguments (json/generate-string (:tool/arguments tc))}})
        tool-calls))

(defn parse-anthropic-response
  "Parse a decoded Anthropic messages response map into the
  canonical provider result: content blocks are concatenated (text
  blocks only; tool_use blocks are ignored in v1), usage carries
  input/output tokens. No reasoning extraction: Anthropic reasoning
  lives in the thinking block and is not interleaved in v1."
  [response]
  (let [content (:content response)
        texts (keep (fn [b] (when (and (map? b) (= "text" (:type b)))
                               (:text b)))
                    (if (vector? content) content []))
        usage (:usage response)]
    {:model/output {:text (apply str texts)}
     :usage {:input-tokens (get-in usage [:input_tokens] 0)
             :output-tokens (get-in usage [:output_tokens] 0)}}))

;; --- cost ----------------------------------------------------------------------

(defn estimate-cost
  "Estimate USD cost units for one call from the models.dev pricing
  table {:input <rate> :output <rate> :reasoning <rate or nil>}:
  input_tokens * input rate + output_tokens * output rate. The
  :reasoning rate is ignored in v1 because OpenAI-compatible chat
  responses do not report the reasoning-token split; it becomes
  usable once providers expose it (TODO: completion_tokens_details
  reasoning_tokens). Returns a plain number; nil pricing yields nil
  (unknown cost)."
  [pricing usage]
  (when (and (map? pricing) (map? usage))
    (let [input-rate (:input pricing)
          output-rate (:output pricing)
          input-tokens (or (:input-tokens usage) 0)
          output-tokens (or (:output-tokens usage) 0)]
      (when (or input-rate output-rate)
        (+ (* input-tokens (double (or input-rate 0)))
           (* output-tokens (double (or output-rate 0))))))))

(defn provider-result
  "Assemble the canonical provider result map for a model call:
  output + usage + cost. `cost` is the estimate-cost result (nil
  when pricing is unknown)."
  [output usage cost]
  (cond-> {:model/output output
           :usage {:model-input-tokens (get usage :input-tokens 0)
                   :model-output-tokens (get usage :output-tokens 0)}}
    (some? cost) (assoc :model-cost-units (double cost))))
