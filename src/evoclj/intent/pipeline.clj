(ns evoclj.intent.pipeline
  "EffectPipeline combinator — the single implementation of the
  validate -> lookup -> normalize -> authorize -> execute (with retry)
  -> validate-output pipeline.

  The pipeline is the ONLY place that turns an Intent into a provider
  effect. It handles both intent families through one function:

  - :intent/model-call — resolved through the kernel-owned model registry
    (payload :model/id), normalized to the canonical {:kind :model ...}
    resource.
  - :intent/tool-call, :intent/memory-read, :intent/memory-write —
    resolved through the kernel-owned provider registry under a tool id.
    Memory intents resolve the fixed :memory/kv provider.

  Tool intents capture a CallBinding (ToolSurface current entry ->
  capture-tool-binding -> CallBinding) before normalization; the frozen
  descriptor is reused for authorization, execution, and output
  validation (D_normalize = D_authorize = D_execute = D_validate).
  Model intents have no CallBinding; the effect journal still records
  the transition with nil binding-derived fields.

  Execution is retried only when the descriptor declares
  :retry {:safe? true} and the provider throws the typed transient
  signal :provider/transient-error (or an MCP transient family:
  :mcp/timeout, :mcp/transport-error, :mcp/protocol-error — transport
  vs timeout classification is preserved, both are transient for retry).
  An ambiguous outcome (:provider/call-ambiguous — the request was sent
  and the remote effect MAY have committed but the definitive result
  never came back) is NEVER retried, even for a safe descriptor, and
  fails closed as :effect/ambiguous.

  Returns a typed result map (see evoclj.intent.dispatch for the
  contract). Runtime outcomes are returned as typed error results;
  host-side bugs throw typed ExceptionInfo."
  (:require [evoclj.binding.call :as binding]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.constraint :as constraint]
            [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]
            [evoclj.provider.model-registry :as model-registry]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m])
  (:import (java.nio.charset StandardCharsets)))

;; --- shared error classification (INV-05: single implementation) -----------
;;
;; This is the ONLY place that defines which error families are retryable
;; (transient) vs fail-closed ambiguous. Both provider and MCP families are
;; covered here so callers (dispatch, mcp.client, provider.request) have
;; one authoritative predicate and no duplicated sets.

(def ^:private provider-transient-type :provider/transient-error)
(def ^:private provider-ambiguous-type :provider/call-ambiguous)

(def ^:private mcp-transient-types
  "MCP transport-family signals. Timeout stays its OWN category and is
  never folded into transport; both are transient for retry."
  #{:mcp/timeout :mcp/transport-error :mcp/protocol-error})

(defn transient-error?
  "Return true when the thrown value is a retryable transient failure.
  Covers both :provider/transient-error and the MCP transient families."
  [t]
  (and (instance? clojure.lang.ExceptionInfo t)
       (let [etype (:error/type (ex-data t))]
         (or (= provider-transient-type etype)
             (contains? mcp-transient-types etype)))))

(defn ambiguous-error?
  "Return true when the thrown value is the AMBIGUOUS outcome signal
  :provider/call-ambiguous."
  [t]
  (and (instance? clojure.lang.ExceptionInfo t)
       (= provider-ambiguous-type (:error/type (ex-data t)))))

(defn transient-error-type?
  "Return true when error-type keyword is a retryable transient family."
  [error-type]
  (or (= provider-transient-type error-type)
      (contains? mcp-transient-types error-type)))

(defn ambiguous-error-type?
  "Return true when error-type is the ambiguous family."
  [error-type]
  (= provider-ambiguous-type error-type))
;; --- byte accounting (C3: :max-bytes measures bytes, not calls) -----------
;;
;; Per-provider-kind derivation rule (documented here, implemented in
;; value-bytes below):
;;   - model providers (anthropic/openai/modelsdev): the execute-request!
;;     value is {:model/output {...} :usage {:model-input-tokens N
;;     :model-output-tokens M ...} ...} — token counters times
;;     bytes-per-token (4 UTF-8 bytes/token avg) is the consumed-byte
;;     estimate. Values carrying a token map under :model/usage, or bare
;;     :input-tokens/:output-tokens-style counters at the top level, use
;;     the same estimate.
;;   - string values (a provider echoing raw text): the UTF-8 byte length
;;     of the string itself.
;;   - byte arrays: alength (raw byte count).
;;   - everything else (fixture echo {:text ...}, memory KV, MCP
;;     {:value ... :audit ...}, or a model value with no token counters):
;;     the UTF-8 byte count of the pr-str serialization — the actual
;;     serialized size that crossed the provider boundary.

(def ^:private bytes-per-token
  "Concrete byte estimate for one model token (UTF-8 avg). Used to turn
  token counters into a byte quota the :max-bytes descriptor can compare."
  4)

(def ^:private model-token-keys
  "The token counters recognized on a provider result :usage map. Summing
  all present counters (times bytes-per-token) is the consumed-byte
  estimate for a model call."
  [:input-tokens :output-tokens :reasoning-tokens
   :model-input-tokens :model-output-tokens :model-reasoning-tokens
   :prompt-tokens :completion-tokens])

(defn- token-source-maps
  "Candidate token-counter maps on a provider result value, in priority
  order: the :usage map (model providers), :model/usage (alt key), then
  the value itself (bare top-level counters like :output-tokens)."
  [value]
  (when (map? value)
    (filter map? [(:usage value) (:model/usage value) value])))

(defn- model-token-count
  "Sum of the recognized token counters on a value's token maps; nil when
  the value carries no recognized numeric counter (so non-model values
  fall back to byte-serialization rather than miscounting)."
  [value]
  (let [counters (mapcat (fn [m]
                           (keep (fn [k] (when (number? (get m k)) (get m k)))
                                 model-token-keys))
                         (token-source-maps value))]
    (when (seq counters) (reduce + 0 counters))))

(defn value-bytes
  "The consumed-byte estimate for ONE provider result value (see the
  per-provider-kind rule above): strings measure UTF-8 length, byte
  arrays measure alength, token-carrying maps measure tokens times
  bytes-per-token, everything else measures the UTF-8 bytes of pr-str."
  [value]
  (cond
    (string? value)
    (long (count (.getBytes ^String value StandardCharsets/UTF_8)))
    (bytes? value)
    (long (alength ^bytes value))
    :else
    (if-let [tokens (model-token-count value)]
      (long (* bytes-per-token tokens))
      (long (count (.getBytes (pr-str value) StandardCharsets/UTF_8))))))


(defn- attach-binding-audit
  [result binding]
  (binding/attach-audit-to-result result binding))

(defn- enrich-value-audit
  [value binding]
  (if (and (map? value) (contains? value :audit))
    (update value :audit merge (binding/binding->audit binding))
    value))

(defn- effect-journal
  [binding intent decision final-status]
  {:effect/proposed {:intent/id (:intent/id intent)}
   :effect/authorized (or decision {:decision :none})
   :effect/call-started {:idempotency/key (get-in intent [:metadata :idempotency/key])
                         :revision/seq (when binding (:revision/seq binding))
                         :binding/id (when binding (:binding/id binding))}
   :effect/final final-status})

(defn- final-status-for
  [result]
  (if (= :ok (:result/status result))
    :effect/committed
    (if (= :effect/ambiguous (:error/type result))
      :effect/ambiguous
      :effect/rejected)))

(defn- attach-journal
  [result binding intent decision]
  (assoc result :effect-journal
         (effect-journal binding intent decision (final-status-for result))))

(defn- result-error
  [intent type message data authorization usage]
  {:result/status :error
   :error/type type
   :error/message message
   :intent/id (:intent/id intent)
   :authorization authorization
   :usage usage
   :error/data (err/sanitize data)})

(defn- result-ok
  [intent value authorization usage]
  {:result/status :ok
   :intent/id (:intent/id intent)
   :value value
   :authorization authorization
   :usage usage})

(defn- normalize-request!
  [broker-context provider intent]
  (try
    {:normalized (proto/normalize-request provider intent)}
    (catch clojure.lang.ExceptionInfo e
      (if (= :provider/input-invalid (:error/type (ex-data e)))
        {:error-result (result-error intent :provider/input-invalid
                                     (ex-message e)
                                     (dissoc (ex-data e) :error/type)
                                     nil @(:usage broker-context))}
        {:error-result (result-error intent :provider/execution-failed
                                     "provider normalize-request failed unexpectedly"
                                     {:cause (err/error-data e)}
                                     nil @(:usage broker-context))}))
    (catch Throwable t
      {:error-result (result-error intent :provider/execution-failed
                                   "provider normalize-request failed unexpectedly"
                                   {:cause (err/error-data t)}
                                   nil @(:usage broker-context))})))

(defn- execute-with-retry!
  [broker-context provider descriptor decision normalized]
  (let [max-attempts (:max-attempts broker-context)
        safe? (get-in descriptor [:retry :safe?])
        usage-atom (:usage broker-context)
        lease-id (:lease-id decision)]
    (loop [attempt 1]
      (swap! usage-atom constraint/bump-calls lease-id)
      (let [outcome (try
                      {:value (proto/execute-request! provider normalized)}
                      (catch clojure.lang.ExceptionInfo e
                        (cond
                          (ambiguous-error? e) {:ambiguous e}
                          (transient-error? e) {:transient e}
                          :else {:failed e}))
                      (catch Throwable t
                        {:failed t}))]
        (cond
          (contains? outcome :value)
          (let [value (:value outcome)]
            ;; Bytes accumulate ONLY on a successful provider return: each
            ;; yielded value's serialized/token size is added to the :bytes
            ;; counter. Calls (above) accumulate on EVERY attempt, retries
            ;; included. The two dimensions are independent.
            (swap! usage-atom constraint/add-bytes lease-id (value-bytes value))
            {:ok value})

          (contains? outcome :ambiguous)
          {:error-type :effect/ambiguous
           :error-message (ex-message (:ambiguous outcome))
           :error-data {:cause (err/error-data (:ambiguous outcome))
                        :attempt attempt}}

          (contains? outcome :transient)
          (if (and safe? (< attempt max-attempts))
            (recur (inc attempt))
            {:error-type provider-transient-type
             :error-message (ex-message (:transient outcome))
             :error-data {:cause (err/error-data (:transient outcome))
                          :attempt attempt}})

          :else
          (let [t (:failed outcome)]
            {:error-type :provider/execution-failed
             :error-message (if (instance? clojure.lang.ExceptionInfo t)
                              (ex-message t)
                              (str "provider execute-request! threw "
                                   (.getName (class t))))
             :error-data {:cause (err/error-data t)
                          :attempt attempt}}))))))

(defn- validate-output!
  [intent descriptor decision value usage]
  (if (and (boundary/edn-safe? value)
           (m/validate (:output-schema descriptor) value))
    (result-ok intent value decision usage)
    (result-error intent :provider/output-invalid
                  "provider output failed output-schema validation"
                  {:output (err/sanitize value)
                   :explanation (err/sanitize
                                 (m/explain (:output-schema descriptor) value))}
                  decision usage)))

;; --- lookup helpers --------------------------------------------------------

(defn- dispatch-model
  [broker-context intent]
  (let [usage-atom (:usage broker-context)
        model-id (get-in intent [:payload :model/id])
        full-id (if (keyword? model-id) (name model-id) model-id)
        registry (:model-registry broker-context)
        emit (fn [result decision]
               (attach-journal result nil intent decision))]
    (if-not registry
      (emit (result-error intent :provider/not-found
                           "no model registry in the broker context"
                           {:model/id full-id :reason :no-model-registry}
                           nil @usage-atom)
            nil)
      (let [entry (model-registry/lookup registry full-id)]
        (cond
          (nil? entry)
          (emit (result-error intent :provider/not-found
                               (str "unknown model " full-id)
                               {:model/id full-id :reason :unknown-model}
                               nil @usage-atom)
                nil)

          (nil? (:provider entry))
          (emit (result-error intent :provider/not-configured
                               (str "model " full-id " is not configured: " (:reason entry))
                               {:model/id full-id :reason (:reason entry)}
                               nil @usage-atom)
                nil)

          :else
          (let [provider (:provider entry)
                descriptor (proto/describe provider)
                normalized-step (normalize-request! broker-context provider intent)]
            (if-let [error-result (:error-result normalized-step)]
              (emit error-result nil)
              (let [normalized (:normalized normalized-step)
                    decision (broker/authorize
                              {:intent intent
                               :normalized-request normalized
                               :leases (:leases broker-context)
                               :usage @usage-atom
                               :now ((:now broker-context))})]
                (if (= :deny (:decision decision))
                  (emit (result-error intent :capability/denied
                                       "intent denied by the capability broker"
                                       {:reason (:reason decision)}
                                       decision @usage-atom)
                        decision)
                  (let [execution (execute-with-retry!
                                   broker-context provider descriptor
                                   decision normalized)]
                    (if-let [value (:ok execution)]
                      (emit (validate-output! intent descriptor decision
                                               value @usage-atom)
                            decision)
                      (emit (result-error intent (:error-type execution)
                                          (:error-message execution)
                                          (:error-data execution)
                                          decision @usage-atom)
                            decision))))))))))))

(defn- dispatch-tool
  [broker-context intent tool-id require-idempotency-key?]
  (let [usage-atom (:usage broker-context)
        entry (registry/lookup (:registry broker-context) tool-id)
        emit (fn [result binding decision]
               (attach-journal (attach-binding-audit result binding)
                               binding intent decision))]
    (if-not entry
      (if (registry/removed? (:registry broker-context) tool-id)
        (result-error intent :provider/tool-removed
                      (str "tool " tool-id " was registered and has been removed")
                      {:tool/id tool-id}
                      nil @usage-atom)
        (result-error intent :provider/not-found
                      (str "no provider registered for tool " tool-id)
                      {:tool/id tool-id}
                      nil @usage-atom))
      (let [provider (:provider entry)
            freshness (or (:freshness broker-context) :best-effort)
            binding (binding/capture-tool-binding entry {:freshness freshness})
            stale? (:binding/stale? binding)]
        (if (and stale? (= freshness :required))
          (let [err-result (result-error intent :provider/freshness-required
                                         "descriptor is stale and freshness :required blocks execution"
                                         {:tool/id tool-id
                                          :freshness freshness
                                          :revision/seq (:revision/seq binding)
                                          :reason :stale-descriptor}
                                         nil @usage-atom)]
            (emit err-result binding nil))
          (let [frozen-descriptor (:binding/descriptor binding)
                normalized-step (normalize-request! broker-context provider intent)]
            (if-let [error-result (:error-result normalized-step)]
              (emit error-result binding nil)
              (let [normalized (:normalized normalized-step)
                    binding* (assoc binding :binding/normalized normalized :contract/normalized normalized)
                    decision (broker/authorize
                              {:intent intent
                               :normalized-request normalized
                               :leases (:leases broker-context)
                               :usage @usage-atom
                               :now ((:now broker-context))})
                    binding** (assoc binding* :binding/decision decision :contract/decision decision)]
                (if (= :deny (:decision decision))
                  (emit
                   (result-error intent :capability/denied
                                 "intent denied by the capability broker"
                                 {:reason (:reason decision)}
                                 decision @usage-atom)
                   binding** decision)
                  (if (and require-idempotency-key?
                           (not= :pure (:effect frozen-descriptor))
                           (not= :model-call (:effect frozen-descriptor))
                           (nil? (get-in intent [:metadata :idempotency/key])))
                    (emit
                     (result-error intent :intent/idempotency-key-missing
                                   "non-pure writes require an idempotency key in :metadata before execution"
                                   {:tool/id tool-id :effect (:effect frozen-descriptor)}
                                   decision @usage-atom)
                     binding** decision)
                    (let [execution (execute-with-retry!
                                     broker-context provider frozen-descriptor
                                     decision normalized)]
                      (if-let [value (:ok execution)]
                        (let [tool-error? (binding/tool-error? value)
                              enriched-value (enrich-value-audit value binding**)]
                          (if tool-error?
                            (emit (result-ok intent enriched-value decision @usage-atom) binding** decision)
                            (let [ok-result (validate-output! intent frozen-descriptor decision
                                                               enriched-value @usage-atom)]
                              (emit ok-result binding** decision))))
                        (emit
                         (result-error intent (:error-type execution)
                                       (:error-message execution)
                                       (:error-data execution)
                                       decision @usage-atom)
                         binding** decision)))))))))))))

;; --- public pipeline -------------------------------------------------------

(defn pipeline
  "Execute intent through the single EffectPipeline combinator.

  The pipeline implements the normative order:

    validate intent -> lookup provider -> normalize resource
    -> authorize -> execute once/retry per policy -> validate output

  broker-context is the map produced by evoclj.intent.dispatch/make-broker-context.
  intent is a v0 Intent. Both :intent/model-call and :intent/tool-call
  (plus :intent/memory-read / :intent/memory-write) are handled via the
  tool-id lookup branch; model intents resolve through the model registry
  while tool/memory intents resolve through the provider registry.

  Returns a typed result map; throws typed ExceptionInfo for host-side
  bugs (malformed intent, malformed broker context)."
  [broker-context intent]
  (intent-schema/validate-intent intent)
  (case (:intent/type intent)
    :intent/tool-call
    (dispatch-tool broker-context intent
                   (get-in intent [:payload :tool/id]) true)
    :intent/memory-read
    (dispatch-tool broker-context intent :memory/kv false)
    :intent/memory-write
    (dispatch-tool broker-context intent :memory/kv false)
    :intent/model-call (dispatch-model broker-context intent)
    (result-error intent :intent/unsupported-dispatch
                  "the v0 dispatcher executes :intent/tool-call, :intent/memory-read, :intent/memory-write, and :intent/model-call intents only"
                  {:intent/type (:intent/type intent)}
                  nil @(:usage broker-context))))

;; Map-shaped entry point for callers that prefer a single argument.
(defn pipeline-map
  "Map-shaped alias: (pipeline-map {:intent intent :broker-context ctx})
  delegates to (pipeline ctx intent). Accepts either :broker-context or
  :surface as the context key for compatibility."
  [{:keys [intent broker-context surface computation]}]
  (let [ctx (or broker-context surface computation)]
    (pipeline ctx intent)))
