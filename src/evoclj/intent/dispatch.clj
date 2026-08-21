(ns evoclj.intent.dispatch
  "The effectful intent dispatcher (component).

  dispatch! is the ONLY kernel code that turns an authorized Intent
  into a real provider effect. It implements the NORMATIVE dispatcher
  order (component Step 5), exactly:

    1. validate intent     — evoclj.intent.schema/validate-intent
    2. lookup provider     — evoclj.provider.registry
    3. normalize resource  — provider normalize-request, BEFORE
                             authorization (Global Constraint 9: a
                             visible action/tool never grants resource
                             authority, so coverage is decided on the
                             canonical resource)
    4. authorize           — evoclj.capability.broker/authorize, PURE,
                             with a usage atom for call counts
    5. execute once/retry  — execute-request!, retried ONLY when the
                             descriptor declares :retry {:safe? true}
                             and the provider reports a TRANSIENT
                             error (:provider/transient-error); a
                             non-pure write (:effect not :pure) must
                             carry an idempotency key in
                             :metadata {:idempotency/key ...} before
                             execution.
    6. validate output     — the provider's result value is validated
                             against the descriptor's :output-schema
                             (and the EDN-safe boundary, Global
                             Constraint 22) BEFORE it is visible
                             anywhere; malformed output is
                             :provider/output-invalid, never accepted
                             as model-visible data.

  Returns a typed result map:

    {:result/status :ok :intent/id ... :value ...
     :authorization {...} :usage {...}}
    {:result/status :error :error/type ... :intent/id ...
     :error/message ... :authorization ... :usage ... :error/data ...}

  Runtime outcomes (unknown tool, denied request, input/output
  validation failures, transient or hard provider failures) are
  RETURNED as typed error results. Host-side bugs — a malformed
  intent (:intent/schema-invalid), a malformed broker context
  (:broker/context-invalid), malformed leases or usage
  (:capability/schema-invalid) — THROW typed ExceptionInfo instead:
  garbage never authorizes and never hides a caller bug (the same
  contract as evoclj.capability.broker).

  Usage accounting: :usage is an atom mapping a lease's :cap/id to the
  number of calls already consumed under it, exactly the shape the
  pure policy consumes. Authorization reads the atom once; each
  execute-request! ATTEMPT consumes one call under the authorizing
  lease BEFORE the provider runs (provider-call-started, Transaction
  Boundaries step 5), so a retried attempt consumes budget too and
  the reported :usage is the post-dispatch snapshot.

  The effect protocol of the Transaction Boundaries section is the
  natural extension point: the pipeline state (validated intent,
  normalized request, authorization decision, idempotency key,
  attempt outcomes) is exactly what the durable persistence steps
  (persist intent proposed -> normalized request -> authorization
  decision -> provider-call-started with idempotency key -> completed
  result OR ambiguous outcome) need in Milestone 5; each step below is
  a separate function so those steps slot in without restructuring.

  Generic Tool/Call Binding: dispatch now uses evoclj.binding.call/CallBinding
  to freeze the descriptor snapshot. The flow is:
    ToolSurface current entry -> capture-tool-binding -> CallBinding -> normalize -> authorize -> execute -> validate.
  Cross-persistence/audit writes only pure data {:binding/id ... :tool/id ... :revision/id ... :revision/seq ...}."
  (:require [evoclj.binding.call :as binding]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.schema :as capability-schema]
            [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]
            [evoclj.provider.model-registry :as model-registry]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; --- constants and context -------------------------------------------------

(def ^:private default-max-attempts 2)
(def ^:private transient-error-type :provider/transient-error)

(defn- validate-lease-collection!
  [leases]
  (try
    (doseq [l leases]
      (capability-schema/validate-lease l))
    (catch clojure.lang.ExceptionInfo e
      (throw (err/error :broker/context-invalid
                        "broker context leases must be valid capability leases"
                        {:cause (err/error-data e)})))))

(defn make-broker-context
  "Build and validate the broker context map dispatch! consumes.

  `config` keys:

  - :registry     (REQUIRED) the provider registry atom from
                  evoclj.provider.registry/create-registry.
  - :leases       the collection of CapabilityLease values granted to
                  the dispatched phenotype (default [] — nothing is
                  granted).
  - :usage        an atom mapping a lease's :cap/id to calls already
                  consumed (default a fresh (atom {})). This is the
                  usage atom that keeps authorization pure while the
                  dispatcher records call counts.
  - :now          a zero-argument fn returning the #inst decision
                  instant (default (fn [] (java.util.Date.))), so a
                  test can pin the clock with (constantly t).
  - :max-attempts the maximum number of execute-request! attempts per
                  dispatch, including the first (default 2 = one
                  retry). Retries happen only for providers declaring
                  :retry {:safe? true}.
  - :freshness    descriptor freshness policy :required | :best-effort | :pinned
                  (default :best-effort). :required fails closed when the
                  binding is stale, :best-effort proceeds with stale? true in
                  binding audit, :pinned never refreshes.

  Returns a closed map. Malformed input throws
  :broker/context-invalid."
  [{:keys [registry leases usage now max-attempts model-registry freshness]}]
  (when-not (instance? clojure.lang.Atom registry)
    (throw (err/error :broker/context-invalid
                      "broker context requires a provider registry atom"
                      {:value (err/sanitize registry)})))
  (let [leases (or leases [])
        usage (or usage (atom {}))
        now (or now (fn [] (java.util.Date.)))
        max-attempts (or max-attempts default-max-attempts)
        model-registry (when model-registry
                         (when-not (instance? clojure.lang.Atom model-registry)
                           (throw (err/error :broker/context-invalid
                                             "model-registry must be an atom or nil"
                                             {:value (err/sanitize model-registry)})))
                         model-registry)
        freshness (or freshness :best-effort)]
    (when-not (binding/valid-freshness? freshness)
      (throw (err/error :broker/context-invalid
                        "freshness must be :required, :best-effort, or :pinned"
                        {:value (err/sanitize freshness)})))
    (validate-lease-collection! leases)
    (when-not (instance? clojure.lang.Atom usage)
      (throw (err/error :broker/context-invalid
                        "usage must be an atom mapping :cap/id to call counts"
                        {:value (err/sanitize usage)})))
    (when-not (fn? now)
      (throw (err/error :broker/context-invalid
                        "now must be a zero-argument fn returning an #inst"
                        {:value (err/sanitize now)})))
    (when-not (and (int? max-attempts) (pos? max-attempts))
      (throw (err/error :broker/context-invalid
                        "max-attempts must be a positive integer"
                        {:value (err/sanitize max-attempts)})))
    {:registry registry
     :leases leases
     :usage usage
     :now now
     :max-attempts max-attempts
     :model-registry model-registry
     :freshness freshness}))

;; --- freshness / binding helpers ------------------------------------------

(defn- stale-binding?
  "True when binding is stale for the given freshness policy.
   Delegates to generic binding helper."
  [descriptor freshness]
  (binding/stale? descriptor freshness))

(defn- attach-binding-audit
  "Enrich a dispatch result with binding audit and persisted data.
   Delegates to generic binding helper so dispatcher does not directly
   mention MCP keys."
  [result binding]
  (binding/attach-audit-to-result result binding))

(defn- enrich-value-audit
  "When provider value is {:value ... :audit ...}, merge binding audit
   into its :audit so the envelope itself carries the snapshot info."
  [value binding]
  (if (and (map? value) (contains? value :audit))
    (update value :audit merge (binding/binding->audit binding))
    value))

(defn- effect-journal
  "Lightweight effect journal: proposed->authorized->call-started(with idempotency/key+revision-seq)
   ->committed/rejected/ambiguous."
  [binding intent decision final-status]
  {:effect/proposed {:intent/id (:intent/id intent)}
   :effect/authorized {:decision (:decision decision) :lease-id (:lease-id decision)}
   :effect/call-started {:idempotency/key (get-in intent [:metadata :idempotency/key])
                         :revision/seq (:revision/seq binding)
                         :binding/id (:binding/id binding)}
   :effect/final final-status})

;; --- result construction ---------------------------------------------------

(defn- result-error
  "Build a typed error result. `data` is sanitized before embedding so
  the result is always plain serializable EDN (Global Constraint 22);
  `authorization` is the (possibly nil) broker decision; `usage` is
  the usage atom snapshot at the point of failure."
  [intent type message data authorization usage]
  {:result/status :error
   :error/type type
   :error/message message
   :intent/id (:intent/id intent)
   :authorization authorization
   :usage usage
   :error/data (err/sanitize data)})

(defn- result-ok
  "Build the typed :ok result after the provider value passed output
  validation."
  [intent value authorization usage]
  {:result/status :ok
   :intent/id (:intent/id intent)
   :value value
   :authorization authorization
   :usage usage})

;; --- pipeline steps --------------------------------------------------------

(defn- normalize-request!
  "Step 3: run provider normalize-request, turning the user-facing
  request into the CANONICAL resource descriptor authorization is
  decided on (Global Constraint 9). Returns {:normalized <request>} on
  success or {:error-result <result>} when the provider rejects the
  input (:provider/input-invalid) or misbehaves
  (:provider/execution-failed)."
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

(defn- transient-error?
  "True when the thrown value is the provider's declared TRANSIENT
  failure signal: an ExceptionInfo whose ex-data carries :error/type
  :provider/transient-error. Only this signal is ever retried, and
  only for providers declaring :retry {:safe? true}."
  [t]
  (and (instance? clojure.lang.ExceptionInfo t)
       (= transient-error-type (:error/type (ex-data t)))))

(defn- execute-with-retry!
  "Step 5: execute the authorized, normalized request once, or retry
  per policy.

  Each attempt consumes one call under the authorizing lease (the
  usage atom is incremented BEFORE the provider runs — the effect
  protocol's provider-call-started step). A transient provider error
  is retried only when the descriptor declares :retry {:safe? true}
  and attempts remain; otherwise the transient failure is reported as
  a typed error result. Any non-transient failure is
  :provider/execution-failed. Returns {:ok <value>} or
  {:error-type ... :error-message ... :error-data ...}."
  [broker-context provider descriptor decision normalized]
  (let [max-attempts (:max-attempts broker-context)
        safe? (get-in descriptor [:retry :safe?])
        usage-atom (:usage broker-context)
        lease-id (:lease-id decision)]
    (loop [attempt 1]
      (swap! usage-atom update lease-id (fnil inc 0))
      (let [outcome (try
                      {:value (proto/execute-request! provider normalized)}
                      (catch clojure.lang.ExceptionInfo e
                        (if (transient-error? e)
                          {:transient e}
                          {:failed e}))
                      (catch Throwable t
                        {:failed t}))]
        (cond
          (contains? outcome :value)
          {:ok (:value outcome)}

          (contains? outcome :transient)
          (if (and safe? (< attempt max-attempts))
            (recur (inc attempt))
            {:error-type transient-error-type
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
  "Step 6: validate the provider's result value against the
  descriptor's :output-schema — after the EDN-safe boundary gate
  (Global Constraint 22). Returns the typed :ok result, or
  :provider/output-invalid carrying the sanitized output and a
  serializable Malli explanation (the invalid value is never accepted
  as model-visible data)."
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

;; --- the dispatcher --------------------------------------------------------

(defn- dispatch-model-call!
  "Dispatch an :intent/model-call through the broker pipeline: resolve
  the full model id in the kernel-owned model registry, normalize the
  request to the canonical {:kind :model ...} resource, authorize
  against the model lease, and execute. Unknown models, unconfigured
  providers (no API key / unsupported style), and denied requests are
  typed error results — nothing executes without a matching lease."
  [broker-context intent]
  (let [usage-atom (:usage broker-context)
        model-id (get-in intent [:payload :model/id])
        full-id (if (keyword? model-id) (name model-id) model-id)
        registry (:model-registry broker-context)]
    (if-not registry
      (result-error intent :provider/not-found
                    "no model registry in the broker context"
                    {:model/id full-id :reason :no-model-registry}
                    nil @usage-atom)
      (let [entry (model-registry/lookup registry full-id)]
        (cond
          (nil? entry)
          (result-error intent :provider/not-found
                        (str "unknown model " full-id)
                        {:model/id full-id :reason :unknown-model}
                        nil @usage-atom)

          (nil? (:provider entry))
          (result-error intent :provider/not-configured
                        (str "model " full-id " is not configured: " (:reason entry))
                        {:model/id full-id :reason (:reason entry)}
                        nil @usage-atom)

          :else
          (let [provider (:provider entry)
                descriptor (proto/describe provider)
                normalized-step (normalize-request! broker-context provider intent)]
            (if-let [error-result (:error-result normalized-step)]
              error-result
              (let [normalized (:normalized normalized-step)
                    decision (broker/authorize
                              {:intent intent
                               :normalized-request normalized
                               :leases (:leases broker-context)
                               :usage @usage-atom
                               :now ((:now broker-context))})]
                (if (= :deny (:decision decision))
                  (result-error intent :capability/denied
                                "intent denied by the capability broker"
                                {:reason (:reason decision)}
                                decision @usage-atom)
                  (let [execution (execute-with-retry!
                                   broker-context provider descriptor
                                   decision normalized)]
                    (if-let [value (:ok execution)]
                      (validate-output! intent descriptor decision
                                        value @usage-atom)
                      (result-error intent (:error-type execution)
                                    (:error-message execution)
                                    (:error-data execution)
                                    decision @usage-atom))))))))))))

(defn- dispatch-registered!
  "Dispatch an intent whose provider is resolved by the kernel-owned
  provider registry under `tool-id`, through the full broker pipeline in
  the NORMATIVE order (validate intent -> lookup provider -> normalize
  resource -> authorize -> execute once/retry per policy -> validate
  output). Shared by :intent/tool-call and the :intent/memory-read /
  :intent/memory-write branches (feature R1).

  Every memory intent resolves the SAME :memory/kv provider; the
  tool-call branch resolves the :payload :tool/id. When
  `require-idempotency-key?` is true (tool-call writes) a non-pure,
  non-model-call effect is refused without a :metadata
  {:idempotency/key ...}; the memory branch does NOT enforce it: an
  episodic memory write through the kernel's own memory nodes is
  bounded by the lease's :max-calls and is upserted (INSERT OR
  REPLACE) by the provider, and no model-requested external write is
  involved.

  Step 1 (CallBinding): capture is checked BEFORE normalize
  and the descriptor snapshot is frozen for the entire effect.
  D_normalize = D_authorize = D_execute = D_validate is enforced by
  capturing CallBinding once and reusing the same frozen-descriptor for all later steps; inline
  refresh inside provider execute-request! is forbidden after
  call-started.

  Freshness: :required fails closed as :provider/freshness-required when
  stale; :best-effort proceeds with :binding/stale? true and audit marks
  stale; :pinned never considers stale.

  ToolSurface entry -> capture-tool-binding -> CallBinding -> normalize -> authorize -> execute -> validate.

  Returns the typed dispatch result (see the namespace docstring)."
  [broker-context intent tool-id require-idempotency-key?]
  (let [usage-atom (:usage broker-context)
        entry (registry/lookup (:registry broker-context) tool-id)]
    (if-not entry
      (result-error intent :provider/not-found
                    (str "no provider registered for tool " tool-id)
                    {:tool/id tool-id}
                    nil @usage-atom)
      (let [provider (:provider entry)
            freshness (or (:freshness broker-context) :best-effort)
            ;; Capture CallBinding: ToolSurface current entry -> capture -> CallBinding
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
            (attach-binding-audit err-result binding))
          (let [frozen-descriptor (:binding/descriptor binding)
                normalized-step (normalize-request! broker-context provider intent)]
            (if-let [error-result (:error-result normalized-step)]
              (attach-binding-audit error-result binding)
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
                  (attach-binding-audit
                   (result-error intent :capability/denied
                                 "intent denied by the capability broker"
                                 {:reason (:reason decision)}
                                 decision @usage-atom)
                   binding**)
                  (if (and require-idempotency-key?
                           (not= :pure (:effect frozen-descriptor))
                           (not= :model-call (:effect frozen-descriptor))
                           (nil? (get-in intent [:metadata :idempotency/key])))
                    (attach-binding-audit
                     (result-error intent :intent/idempotency-key-missing
                                   "non-pure writes require an idempotency key in :metadata before execution"
                                   {:tool/id tool-id :effect (:effect frozen-descriptor)}
                                   decision @usage-atom)
                     binding**)
                    (let [execution (execute-with-retry!
                                     broker-context provider frozen-descriptor
                                     decision normalized)]
                      (if-let [value (:ok execution)]
                        (let [tool-error? (binding/tool-error? value)
                              enriched-value (enrich-value-audit value binding**)]
                          (if tool-error?
                            (attach-binding-audit (result-ok intent enriched-value decision @usage-atom) binding**)
                            (let [ok-result (validate-output! intent frozen-descriptor decision
                                                               enriched-value @usage-atom)]
                              (attach-binding-audit ok-result binding**))))
                        (attach-binding-audit
                         (result-error intent (:error-type execution)
                                       (:error-message execution)
                                       (:error-data execution)
                                       decision @usage-atom)
                         binding**)))))))))))))

(defn dispatch!
  "Execute intent through the broker pipeline in the NORMATIVE order
  (component Step 5): validate intent -> lookup provider -> normalize
  resource -> authorize -> execute once/retry per policy -> validate
  output -> return a typed result. See the namespace docstring for the
  result contract and the effect-protocol extension points.

  broker-context is a map built by make-broker-context. intent is a
  validated v0 Intent (a malformed intent throws
  :intent/schema-invalid; :intent/tool-call, :intent/memory-read, and
  :intent/memory-write execute through the provider registry,
  :intent/model-call executes through the model
  registry, and every other intent type fails closed with
  :intent/unsupported-dispatch)."
  [broker-context intent]
  (intent-schema/validate-intent intent)
  (case (:intent/type intent)
    :intent/tool-call
    (dispatch-registered! broker-context intent
                          (get-in intent [:payload :tool/id]) true)
    :intent/memory-read
    (dispatch-registered! broker-context intent :memory/kv false)
    :intent/memory-write
    (dispatch-registered! broker-context intent :memory/kv false)
    :intent/model-call (dispatch-model-call! broker-context intent)
    (result-error intent :intent/unsupported-dispatch
                  "the v0 dispatcher executes :intent/tool-call, :intent/memory-read, :intent/memory-write, and :intent/model-call intents only"
                  {:intent/type (:intent/type intent)}
                  nil @(:usage broker-context))))
