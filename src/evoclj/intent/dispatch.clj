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

  Usage accounting: :usage is an atom mapping a lease's :cap/id to an
  entry {:calls N :bytes B} — the calls and bytes already consumed
  under it, exactly the shape the pure policy consumes. Authorization
  reads the atom once; each execute-request! ATTEMPT consumes one call
  under the authorizing lease BEFORE the provider runs (provider-call-started,
  Transaction Boundaries step 5), and each successful provider return
  adds its value's byte size to :bytes. A retried attempt consumes call
  budget too, and the reported :usage is the post-dispatch snapshot.

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
            [evoclj.capability.core :as capability]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.schema :as capability-schema]
            [evoclj.intent.pipeline :as pipeline]
            [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]
            [evoclj.provider.model-registry :as model-registry]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.subagent :as subagent]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))
;; --- constants and context -------------------------------------------------

(def ^:private default-max-attempts 2)


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
  - :usage        an atom mapping a lease's :cap/id to an entry
                  {:calls N :bytes B} already consumed (default a fresh
                  (atom {})). This is the usage atom that keeps
                  authorization pure while the dispatcher records both
                  call and byte counters.
  - :now          a zero-argument fn returning the #inst decision
                  instant (default (fn [] (java.util.Date.))), so a
                  test can pin the clock with (constantly t).
  - :max-attempts the maximum number of execute-request! attempts per
                  dispatch, including the first (default 2 = one
                  retry). Retries happen only for providers declaring
                  :retry {:safe? true}.
  - :requested-capabilities optional Requested capability categories
                  carried by the compiled Genome.
  - :effects       optional static Effects carried by the compiled topology.
                  When either is supplied, both must be supplied and satisfy
                  Effects ⊆ Requested; Granted is derived from :leases.
  - :freshness     descriptor freshness policy :required | :best-effort | :pinned
                  (default :best-effort). :required fails closed when the
                  binding is stale, :best-effort proceeds with stale? true in
                  binding audit, :pinned never refreshes.

  Returns a closed map. Malformed input throws
  :broker/context-invalid."
  [{:keys [registry leases usage now max-attempts model-registry
           requested-capabilities effects freshness db]}]
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
                        "usage must be an atom mapping :cap/id to {:calls int :bytes int}"
                        {:value (err/sanitize usage)})))
    (when-not (fn? now)
      (throw (err/error :broker/context-invalid
                        "now must be a zero-argument fn returning an #inst"
                        {:value (err/sanitize now)})))
    (when-not (and (int? max-attempts) (pos? max-attempts))
      (throw (err/error :broker/context-invalid
                        "max-attempts must be a positive integer"
                        {:value (err/sanitize max-attempts)})))
    (when (not= (some? effects) (some? requested-capabilities))
      (throw (err/error :broker/context-invalid
                        "effects and requested-capabilities must be supplied together"
                        {:reason :capability-lattice-incomplete})))
    (when (and effects requested-capabilities)
      (try
        (capability/validate-effect-lattice!
         effects
         requested-capabilities
         (capability/granted-effects leases))
        (catch clojure.lang.ExceptionInfo e
          (throw (err/error :broker/context-invalid
                            "broker context carries an invalid capability lattice"
                            {:reason :capability-lattice-invalid
                             :cause (err/error-data e)})))))
    {:registry registry
     :leases leases
     :usage usage
     :now now
     :max-attempts max-attempts
     :model-registry model-registry
     :requested-capabilities requested-capabilities
     :effects effects
     :freshness freshness
     :db db}))

;; --- freshness / binding helpers ------------------------------------------
;; Pipeline delegates to binding/capture-tool-binding via evoclj.intent.pipeline

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
  "The single canonical effect journal (INV-05 — one implementation).
   Records the Transaction Boundaries protocol transition:

     proposed -> authorized -> call-started(with idempotency/key +
     revision/seq + binding/id) -> final(:effect/committed |
     :effect/rejected | :effect/ambiguous).

   It is attached to EVERY dispatch result so the durable persistence /
   audit steps (Milestone 5) have one consistent journal shape across
   all production paths (tool-call, memory-*, model-call). `binding`
   may be nil (model-call has no CallBinding) — call-started then omits
   the binding-derived fields but keeps the idempotency key. `decision`
   is the broker decision; for pre-authorization failures it is nil and
   the journal records :decision :none."
  [binding intent decision final-status]
  {:effect/proposed {:intent/id (:intent/id intent)}
   :effect/authorized (or decision {:decision :none})
   :effect/call-started {:idempotency/key (get-in intent [:metadata :idempotency/key])
                         :revision/seq (when binding (:revision/seq binding))
                         :binding/id (when binding (:binding/id binding))}
   :effect/final final-status})

(defn- final-status-for
  "Map a typed dispatch result to its journal final status. A successful
   execution finalizes as :effect/committed; an ambiguous outcome
   (typed :effect/ambiguous) finalizes as :effect/ambiguous; every other
   error finalizes as :effect/rejected."
  [result]
  (if (= :ok (:result/status result))
    :effect/committed
    (if (= :effect/ambiguous (:error/type result))
      :effect/ambiguous
      :effect/rejected)))

(defn- attach-journal
  "Attach the canonical effect journal to a typed result, deriving the
   final status from the result itself."
  [result binding intent decision]
  (assoc result :effect-journal
         (effect-journal binding intent decision (final-status-for result))))

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

;; --- pipeline steps (single impl lives in evoclj.intent.pipeline) --------
;;
;; The EffectPipeline combinator owns validate -> lookup -> normalize ->
;; authorize -> execute (with retry) -> validate-output as ONE function.
;; This namespace keeps only thin forwarding aliases for the error
;; classification predicates so INV-05 (single implementation) is
;; satisfied: no duplicated transient/ambiguous sets.

(defn- transient-error?
  "Forward to the single pipeline predicate (INV-05)."
  [t]
  (pipeline/transient-error? t))

(defn- ambiguous-error?
  "Forward to the single pipeline predicate (INV-05)."
  [t]
  (pipeline/ambiguous-error? t))

;; The private normalize / execute / validate helpers are owned by
;; evoclj.intent.pipeline and are not duplicated here.

;; --- the dispatcher (thin wrappers delegating to pipeline) -----------------

(defn- dispatch-model-call!
  "Deprecated forwarding wrapper. Delegates to evoclj.intent.pipeline/pipeline.
  Kept for backward compatibility; new code should call pipeline/pipeline
  directly."
  [broker-context intent]
  (pipeline/pipeline broker-context intent))

(defn- dispatch-registered!
  "Deprecated forwarding wrapper. Delegates to evoclj.intent.pipeline/pipeline.
  Kept for backward compatibility; the tool-id and idempotency flag are
  derived from the intent inside the pipeline, so the extra args are
  accepted but ignored beyond the delegation."
  [broker-context intent _tool-id _require-idempotency-key?]
  (pipeline/pipeline broker-context intent))

(defn- requested-effect-denial
  "Return a typed denial when a runtime intent asks for an effect that
  is not in the compiled Genome's Requested set. Exact lease coverage is
  still checked by the broker after this lattice gate."
  [broker-context intent]
  (let [requested (:requested-capabilities broker-context)
        effect (capability/intent-effect intent)]
    (when (and requested effect (not (contains? requested effect)))
      (attach-journal
       (result-error intent :capability/denied
                     "intent effect was not declared in Requested"
                     {:reason :capability/not-requested
                      :effect effect
                      :requested requested}
                     nil @(:usage broker-context))
       nil intent nil))))

(defn- dispatch-agent-spawn-tool!
  "Handle :intent/tool-call where :tool/id is :agent/spawn (S6 broker tool).
  Extracts :task from :args and spawns via subagent/spawn-subagent! using
  the intent's :session/id as parent. Depth/budget caps are enforced by
  spawn-subagent! itself."
  [broker-context intent]
  (let [db (:db broker-context)]
    (if-not db
      (result-error intent :intent/dispatch-invalid
                    "agent/spawn tool requires :db in broker context"
                    {:tool/id :agent/spawn}
                    nil @(:usage broker-context))
      (try
        (let [args (get-in intent [:payload :args])
              parent-id (:session/id intent)
              task (:task args)
              child-spec (merge {:task task} (dissoc args :task))
              res (subagent/spawn-subagent! db parent-id child-spec (:leases broker-context))
              child-id (:child/session-id res)]
          (attach-journal
           (result-ok intent {:child/session-id child-id
                              :child/capabilities (:child/capabilities res)}
                      nil @(:usage broker-context))
           nil intent nil))
        (catch clojure.lang.ExceptionInfo e
          (let [edata (ex-data e)]
            (attach-journal
             (result-error intent (or (:error/type edata) :intent/dispatch-failed)
                           (.getMessage e)
                           edata
                           nil @(:usage broker-context))
             nil intent nil)))
        (catch Exception e
          (attach-journal
           (result-error intent :intent/dispatch-failed
                         (.getMessage e)
                         {:cause (.getMessage e)}
                         nil @(:usage broker-context))
           nil intent nil))))))

(defn- dispatch-agent-status-tool!
  "Handle :intent/tool-call where :tool/id is :agent/status."
  [broker-context intent]
  (let [db (:db broker-context)]
    (if-not db
      (result-error intent :intent/dispatch-invalid
                    "agent/status tool requires :db in broker context"
                    {:tool/id :agent/status}
                    nil @(:usage broker-context))
      (try
        (let [args (get-in intent [:payload :args])
              sid-str (:session-id args)
              sid (try (evoclj.genome.types/session-id sid-str) (catch Exception _ sid-str))
              sess (try (evoclj.store.session/get-session db sid) (catch Exception _ nil))]
          (if-not sess
            (attach-journal
             (result-ok intent {:found false :reason :session-not-found :session/id sid}
                        nil @(:usage broker-context))
             nil intent nil)
            (attach-journal
             (result-ok intent {:found true
                                :session/id (:session/id sess)
                                :state (:state sess)
                                :depth (try (subagent/subagent-depth db sid) (catch Exception _ nil))
                                :children (try (subagent/child-session-ids db sid) (catch Exception _ []))}
                        nil @(:usage broker-context))
             nil intent nil)))
        (catch clojure.lang.ExceptionInfo e
          (let [edata (ex-data e)]
            (attach-journal
             (result-error intent (or (:error/type edata) :intent/dispatch-failed)
                           (.getMessage e)
                           edata
                           nil @(:usage broker-context))
             nil intent nil)))
        (catch Exception e
          (attach-journal
           (result-error intent :intent/dispatch-failed
                         (.getMessage e)
                         {:cause (.getMessage e)}
                         nil @(:usage broker-context))
           nil intent nil))))))

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
  (if-let [denial (requested-effect-denial broker-context intent)]
    denial
    (case (:intent/type intent)
      :intent/tool-call
      (let [tool-id (get-in intent [:payload :tool/id])]
        (cond
          (= :agent/spawn tool-id) (dispatch-agent-spawn-tool! broker-context intent)
          (= :agent/status tool-id) (dispatch-agent-status-tool! broker-context intent)
          :else (dispatch-registered! broker-context intent tool-id true)))
      :intent/memory-read
      (dispatch-registered! broker-context intent :memory/kv false)
      :intent/memory-write
      (dispatch-registered! broker-context intent :memory/kv false)
      :intent/model-call (dispatch-model-call! broker-context intent)
      :intent/subagent-spawn
      (let [db (:db broker-context)]
        (if-not db
          (result-error intent :intent/dispatch-invalid
                        "subagent spawn requires :db in broker context"
                        {:intent/type (:intent/type intent)}
                        nil @(:usage broker-context))
          (try
            (let [parent-id (get-in intent [:payload :parent/session-id])
                  child-spec (get-in intent [:payload :child/spec])
                  res (subagent/spawn-subagent! db parent-id (or child-spec {}) (:leases broker-context))
                  child-id (:child/session-id res)]
              (attach-journal
               (result-ok intent {:child/session-id child-id
                                  :child/capabilities (:child/capabilities res)}
                          nil @(:usage broker-context))
               nil intent nil))
            (catch clojure.lang.ExceptionInfo e
              (let [edata (ex-data e)]
                (attach-journal
                 (result-error intent (or (:error/type edata) :intent/dispatch-failed)
                               (.getMessage e)
                               edata
                               nil @(:usage broker-context))
                 nil intent nil)))
            (catch Exception e
              (attach-journal
               (result-error intent :intent/dispatch-failed
                             (.getMessage e)
                             {:cause (.getMessage e)}
                             nil @(:usage broker-context))
               nil intent nil)))))
      (result-error intent :intent/unsupported-dispatch
                    "the v0 dispatcher executes :intent/tool-call, :intent/memory-read, :intent/memory-write, :intent/model-call, and :intent/subagent-spawn intents only"
                    {:intent/type (:intent/type intent)}
                    nil @(:usage broker-context)))))
