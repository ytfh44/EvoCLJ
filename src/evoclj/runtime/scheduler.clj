(ns evoclj.runtime.scheduler
  "component — deterministic single-session scheduler and step budget.

  run-session! executes ONE session against the phenotype topology the
  executor carries, in strict FIFO (v0 has no concurrency):

    (run-session! executor session-id task-input)
    ;; => {:status :completed | :failed | :budget-exhausted
    ;;     :session/id <uuid>
    ;;     :output-ref <sha256 or nil>
    ;;     :error/artifact-ref <sha256 or nil>   ; :failed only
    ;;     :episode/id nil                        ; component materializes
    ;;                                            ;   episodes
    ;;     :event/count n}

  THE EXECUTOR MAP (normative for component, designed here):

    {:phenotype <Phenotype from evoclj.runtime.phenotype/instantiate>
     :stores {:sqlite <migrated db>   ; the OPENED sqlite store
              :cas <CAS root>}        ; the OPENED content-addressed store
     :dispatch <broker context from evoclj.intent.dispatch/make-broker-context>}

  The test constructs it directly; Integrant assembly is component
  The scheduler opens nothing and closes nothing: the stores and the
  broker context belong to the host and arrive open.

  SESSION PINNING (Global Constraint 2 — never assume, always read):
  run-session! reads the session from the store and verifies its
  pinned :genome/id, :resolution/id, and :phenotype/id agree with the
  executor's compiled genome (evoclj.compiler.core returns
  :compiled/genome-id, :compiled/resolution-id,
  :compiled/phenotype-id) before touching anything. A disagreement is
  :scheduler/pin-mismatch — the scheduler refuses to run a session
  against the wrong phenotype (the session's pinned identity is the
  store's contract, not the executor's claim).

  SESSION STATE MACHINE: the store's normative machine (component) is
  authoritative. The task text abbreviates the scheduler's transitions
  as :created → :running → :completed | :failed | :budget-exhausted;
  the store has no :created → :running edge and no :running →
  :completed edge (component: :created → :resolving → :running ↔
  :waiting → :completed), so run-session! walks :created → :resolving
  → :running, then :running → :failed | :budget-exhausted directly, or
  :running → :waiting → :completed for a successful run (two
  compare-and-set hops). Only sessions in :created are accepted;
  anything else is :scheduler/session-invalid :reason :not-created.

  CAUSAL ANCHORING: every session's causal chain opens with a
  :session/created root event appended by the host at creation time
  (evoclj.store.event/root-event-types — the scheduler never fabricates
  a root). run-session! requires that the FIRST stored event is
  :session/created and chains :session/started to it; every subsequent
  event chains to the event appended immediately before it, so the log
  is a single linear causal chain (each event's :cause/event-id is the
  previous event's :event/id) and every step's events are persisted
  BEFORE the scheduler advances to the next node (component Step 3).

  EXECUTION (component Steps 1-4): starting at the topology's :entry,
  each visit builds the per-session runtime-state contract of
  evoclj.runtime.node, steps the node's handler, and persists:

    :node/started → :node/completed (with the step's :outputs as a
    CAS artifact) → for each emitted intent, the intent effect
    transaction: :intent/proposed → (evoclj.intent.dispatch!) →
    :intent/authorized → :provider/call-started →
    :provider/call-completed (value as a CAS artifact) on success,
    :intent/denied for a broker denial (no provider event — the
    provider never ran), or :intent/failed for any other dispatch
    failure. Every provider result is fed back into the session's
    accumulated :outputs (component: dispatch and feed results back);
    the next node's input payload is the most recently accumulated
    output (the entry node receives the task-input). A denied or
    failed intent is a node-level outcome — the session continues.

  BUDGET (Step 2): the topology's :limits {:max-steps N} bounds node
  visits. When the visit count reaches N the run halts BEFORE the
  (N+1)-th node is stepped, transitions the session to
  :budget-exhausted, and persists :session/budget-exhausted carrying
  the limit, the steps consumed, and the accumulated outputs as a CAS
  artifact (:output-ref — failures are evidence, not discarded
  traces).

  LOOPS (component): a :loop node's iteration counter travels in the
  scheduler's per-session runtime-state as :loop-state, a map of loop
  node id -> iteration count — SESSION-LOCAL DATA, never a SCI global
  var (Global Constraint 23). The scheduler builds :loop-state fresh
  for every run-session! and threads it through the visit loop; each
  time a :loop node's handler chooses to iterate (its :continue
  transition's :next leads to the node's :body) the scheduler
  increments that loop node's counter before the next visit, so two
  sessions on ONE phenotype can never see each other's counters. The
  loop handler's :max-iterations cap returns a :failed transition
  typed :loop/max-iterations-exceeded; the scheduler recognizes that
  error type and routes it to the SAME :budget-exhausted outcome as
  the step budget (:session/budget-exhausted event recording the
  {:max-iterations N} limit and the accumulated outputs as a CAS
  artifact) — the typed budget outcome chosen for component, so an
  unbounded predicate is a budget outcome, not a session failure.

  FAILURE (Step 4): an unhandled node failure — a handler :failed
  transition, or ANY exception thrown while resolving, stepping, or
  processing the node's intents — fails the session: the serializable
  error payload is stored as a CAS artifact, :node/failed is appended
  (its :payload-ref is the artifact), the session transitions to
  :failed, and :session/failed is appended carrying the artifact ref
  in its metadata (:error/artifact-ref). Scheduler-level run failures
  (a :continue transition with no successor, a :next pointing at an
  undeclared node) fail the session the same way with a :scheduler/*
  error data map. Errors in the STORE or the BROKER CONTEXT
  themselves (host infrastructure failures) propagate as typed
  ExceptionInfo; recovery of a session left mid-run is component's job.

  Error contract (Global Constraint 22 — plain serializable data):
  :scheduler/executor-invalid (:reason distinguishes :not-a-map,
  :phenotype-missing, :phenotype-id-invalid, :compiled-missing,
  :topology-missing, :entry-missing, :nodes-missing, :stores-invalid,
  :sqlite-missing, :cas-missing, :dispatch-invalid),
  :scheduler/session-invalid (:reason :not-found, :not-created,
  :missing-root-event), :scheduler/pin-mismatch (:reason :genome,
  :resolution, :phenotype), and :scheduler/task-input-invalid
  (:reason :not-edn-safe — nothing non-EDN crosses this boundary).

   BINDINGS (WO-B1): before a session leaves :created the scheduler
   restores its durable bindings' runtime state through
   evoclj.store.binding/restore! (mount/context registries may be
   carried on the executor's :stores; verification is fail-closed — an
   unverifiable pinned binding refuses the run with typed
   :store/binding-invalid). During model-call rounds the durable
   bindings query degrades with a COUNTED typed event
   (:scheduler/bindings-degraded, sanitized error data) instead of the
   former silent swallow."
  (:require [evoclj.capability.core :as capability]
            [evoclj.genome.types :as types]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.dialect :as dialect]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.runtime.node :as node]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.binding :as binding-store]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session])
  (:import (java.nio.charset StandardCharsets)))

;; --- executor trust boundary ------------------------------------------------

(defn- executor-error
  "A :scheduler/executor-invalid ExceptionInfo carrying the
  distinguishing :reason and the sanitized offending value."
  [reason message value]
  (err/error :scheduler/executor-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-executor!
  "Validate the executor trust boundary: a map wiring a live phenotype
  (canonical :phenotype/id, a :compiled genome carrying a compiled
  :topology), the opened :stores (:sqlite + :cas), and a :dispatch
  broker context. Every failure throws :scheduler/executor-invalid
  with a distinguishing :reason (host-side bug; garbage never runs)."
  [executor]
  (when-not (map? executor)
    (throw (executor-error :not-a-map "executor must be a map" executor)))
  (let [phenotype (:phenotype executor)]
    (when-not (map? phenotype)
      (throw (executor-error :phenotype-missing
                             "executor must carry a :phenotype map"
                             phenotype)))
    (when-not (types/artifact-id? (:phenotype/id phenotype))
      (throw (executor-error :phenotype-id-invalid
                             "executor phenotype must carry a canonical :phenotype/id"
                             (:phenotype/id phenotype))))
    (when-not (map? (:compiled phenotype))
      (throw (executor-error :compiled-missing
                             "executor phenotype must carry a :compiled genome"
                             (:compiled phenotype))))
    (let [topology (:topology (:compiled phenotype))]
      (when-not (map? topology)
        (throw (executor-error :topology-missing
                               "compiled genome must carry a compiled :topology"
                               topology)))
      (when-not (keyword? (:entry topology))
        (throw (executor-error :entry-missing
                               "compiled topology must declare a keyword :entry"
                               (:entry topology))))
      (when-not (map? (:nodes topology))
        (throw (executor-error :nodes-missing
                               "compiled topology must carry a :nodes map"
                               (:nodes topology))))))
  (let [stores (:stores executor)]
    (when-not (map? stores)
      (throw (executor-error :stores-invalid
                             "executor must carry a :stores map"
                             stores)))
    (when-not (contains? stores :sqlite)
      (throw (executor-error :sqlite-missing
                             "executor :stores must carry the :sqlite handle"
                             stores)))
    (when-not (contains? stores :cas)
      (throw (executor-error :cas-missing
                             "executor :stores must carry the :cas handle"
                             stores))))
  (when-not (map? (:dispatch executor))
    (throw (executor-error :dispatch-invalid
                           "executor must carry a :dispatch broker context"
                           (:dispatch executor))))
  executor)

;; --- artifact persistence (Global Constraint 21) -----------------------------

(defn- put-payload!
  "Store an EDN payload by content hash under the executor's CAS and
  return its canonical artifact id."
  [executor value]
  (:artifact/id
   (cas/put-bytes! (:cas (:stores executor))
                   (.getBytes (pr-str value) StandardCharsets/UTF_8)
                   {})))

(defn- payload-ref
  "A CAS artifact id for `value`, or nil when value is empty (nothing
  to persist — empty payloads are not materialized)."
  [executor value]
  (when (seq value)
    (put-payload! executor value)))

(defn- outputs-ref
  "A CAS artifact id for the accumulated session :outputs (nil when
  empty)."
  [executor outputs]
  (payload-ref executor outputs))

;; --- the append-only causal log ---------------------------------------------

(defn- append-event!
  "Append one event to the session's append-only log, chained to
  `cause-event-id`, with the session's pinned identity (read from the
  store — never assumed) on every row (Global Constraint 20). Returns
  the persisted event."
  [executor pin cause-event-id type payload-ref metadata]
  (event/append-event!
   (:sqlite (:stores executor))
   {:session/id (:session/id pin)
    :generation/id (:generation/id pin)
    :phenotype/id (:phenotype/id pin)
    :event/type type
    :cause/event-id cause-event-id
    :payload-ref payload-ref
    :metadata metadata}))

(defn- event-count
  "The number of events appended to the session's log so far, minus
  its pre-existing :session/created root — i.e., the count appended by
  this run-session! call."
  [executor pin]
  (max 0 (dec (count (event/events-for-session (:sqlite (:stores executor))
                                               (:session/id pin))))))

;; --- the intent effect transaction (Transaction Boundaries) ------------------

(defn- dispatch-intent!
  "Persist one validated intent's effect protocol through the broker
  and feed the result back.

  The scheduler persists :intent/proposed (chained to the
  :node/completed that proposed it), then calls
  evoclj.intent.dispatch! ONCE (the v0 broker is a single call; its
  internal normalize/authorize/execute steps cannot be interleaved
  with persistence), then persists the observable outcome:

  - success    :intent/authorized → :provider/call-started (with the
                idempotency key when the intent carries one) →
                :provider/call-completed (result value as a CAS
                artifact); the result is fed back into :outputs.
  - denied     :intent/denied with the broker's :reason — NO provider
                events (a denied intent never reaches a provider).
  - other      :intent/failed with the dispatch error record as a CAS
                artifact (:payload-ref).

  A denied or failed intent is a node-level outcome: the session
  continues. Returns {:last-event <the final event appended>
  :outputs <the updated accumulated outputs>
  :outcome :ok | :denied | :failed}."
  [executor pin cause intent outputs]
  (let [proposed (append-event! executor pin cause :intent/proposed nil
                                {:intent/id (:intent/id intent)
                                 :intent/type (:intent/type intent)
                                 :node/id (:node/id intent)})
        result (dispatch/dispatch! (:dispatch executor) intent)]
    (if (= :ok (:result/status result))
      (let [authorization (:authorization result)
            tool-id (get-in intent [:payload :tool/id])
            authorized (append-event!
                        executor pin (:event/id proposed) :intent/authorized nil
                        {:intent/id (:intent/id intent)
                         :intent/type (:intent/type intent)
                         :authorization {:decision (:decision authorization)
                                         :lease-id (:lease-id authorization)}})
            started (append-event!
                     executor pin (:event/id authorized) :provider/call-started nil
                     {:intent/id (:intent/id intent)
                      :tool/id tool-id
                      :idempotency/key (get-in intent [:metadata :idempotency/key])})
            value-ref (put-payload! executor (:value result))
            completed (append-event!
                       executor pin (:event/id started) :provider/call-completed value-ref
                       {:intent/id (:intent/id intent)
                        :tool/id tool-id
                        :result/status :ok})]
        {:last-event completed
         :outputs (conj outputs (:value result))
         :outcome :ok})
      (if (= :capability/denied (:error/type result))
        {:last-event (append-event!
                      executor pin (:event/id proposed) :intent/denied nil
                      {:intent/id (:intent/id intent)
                       :intent/type (:intent/type intent)
                       :error/type :capability/denied
                       :reason (get-in result [:error/data :reason])})
         :outputs outputs
         :outcome :denied}
        {:last-event (append-event!
                      executor pin (:event/id proposed) :intent/failed
                      (put-payload! executor (dissoc result :usage))
                      {:intent/id (:intent/id intent)
                       :intent/type (:intent/type intent)
                       :error/type (:error/type result)})
         :outputs outputs
         :outcome :failed}))))


(def ^:private max-tool-rounds-default 4)

(defn- tool-map-of
  "The wire function-name -> declaration map from a model-call
  payload :tools vector (each {:name ... :tool <kw> ...})."
  [intent]
  (into {}
        (map (fn [t] [(:name t) t]))
        (get-in intent [:payload :tools])))

(defn- record-bindings-degradation!
  "WO-B1: a failing durable-bindings query is NEVER silent. Append one
  typed causal event (:scheduler/bindings-degraded) carrying fully
  sanitized error data; the number of such events on the session's log
  IS the degradation counter (durable, attributable, queryable). If
  even this event cannot be appended the failure propagates — fail
  closed rather than degrade invisibly."
  [executor pin cause t]
  (append-event! executor pin cause :scheduler/bindings-degraded nil
                 {:degradation :bindings-fetch
                  :error (err/error-data t)}))

(defn- fetch-bindings
  "Current active ContextBindings for the session (from durable store).
   WO-B1: a missing table or failing query no longer swallows silently —
   the Throwable is recorded as a typed degradation event and the run
   degrades to [] (the session continues without bindings, but the
   degradation is counted on the causal log)."
  [executor pin cause]
  (try
    (binding-store/active-bindings (:sqlite (:stores executor)) (:session/id pin))
    (catch Throwable t
      (record-bindings-degradation! executor pin cause t)
      [])))

(defn- restore-session-runtime!
  "WO-B1 production wiring: before a session leaves :created, republish
  its durable bindings' runtime state into the executor's registries.

  Failure discipline (same vocabulary as fetch-bindings):
    - an INV-02 VERDICT (:store/binding-invalid — the pinned bundle can
      no longer be verified to exist) is fail-closed and ABORTS the run
      with that typed error; the session stays :created;
    - any other Throwable (e.g. the bindings table itself unreadable)
      is recorded as a typed degradation event and the run continues
      without restored runtime state — degraded and counted, never
      silent."
  [executor pin root]
  (let [{:keys [cas registry mount-registry context-store]} (:stores executor)]
    (try
      (binding-store/restore! (:sqlite (:stores executor)) (:session/id pin)
                              (cond-> {}
                                cas (assoc :cas cas)
                                registry (assoc :registry registry)
                                mount-registry (assoc :mount-registry mount-registry)
                                context-store (assoc :context-store context-store)))
      (catch clojure.lang.ExceptionInfo e
        (if (= :store/binding-invalid (:error/type (ex-data e)))
          (throw e)
          (do (record-bindings-degradation! executor pin (:event/id root) e)
              nil)))
      (catch Throwable e
        (record-bindings-degradation! executor pin (:event/id root) e)
        nil))))

(defn- base-call-from-intent*
  "Extract BaseModelCall from an intent, tolerating both new
  :base/messages & legacy :messages shapes."
  [intent]
  (try
    (assembler/base-call-from-intent intent)
    (catch Throwable _
      {:base/messages (or (get-in intent [:payload :base/messages])
                          (get-in intent [:payload :messages]) [])
       :requested-tools (or (get-in intent [:payload :requested-tools])
                            (get-in intent [:payload :tools]) [])
       :options (or (get-in intent [:payload :options]) {})
       :model/id (get-in intent [:payload :model/id])})))

(defn- tool-call-intent
  "A validated :intent/tool-call for one model-requested tool call,
  attributed to the same session/phenotype/node as the model-call
  intent and chained to the causal event id. Carries a fresh
  idempotency key: the tool may be a non-pure write and every
  model-requested execution is its own request."
  [intent cause tool-call tool-id]
  (intent/tool-call
   (:session/id intent)
   (:phenotype/id intent)
   (:node/id intent)
   cause
   {:tool/id tool-id :args (:tool/arguments tool-call)}
   (:budget intent)))

(defn- tool-result-msg
  "The :role :tool message fed back to the model for one executed
  tool call: the provider value on success, a short error text on
  denial/failure (the model sees the failure as data and may react)."
  [tool-call outcome value]
  {:role :tool
   :tool-call-id (:tool/call-id tool-call)
   :content (if (= :ok outcome)
              (pr-str value)
              (str "error: " (name outcome)))})

(defn- execute-tool-calls!
  "Execute every model-requested tool call through the broker (each
  its own :intent/tool-call with full event persistence) and collect
  the tool-result messages. Unknown tool names fail the session with
  :scheduler/unknown-tool (the tool-map is host-declared; the model
  cannot invent tools)."
  [executor pin cause intent calls]
  (loop [acc {:last-event cause :outputs [] :tool-msgs []}
         calls (seq calls)]
    (if-let [{:keys [call tool-id]} (first calls)]
      (let [cause-id (if (map? (:last-event acc))
                       (:event/id (:last-event acc))
                       (:last-event acc))
            step (dispatch-intent!
                  executor pin cause-id
                  (tool-call-intent intent cause-id call tool-id)
                  (:outputs acc))
            value (peek (:outputs step))]
        (recur {:last-event (:last-event step)
                :outputs (:outputs step)
                :tool-msgs (conj (:tool-msgs acc)
                                 (tool-result-msg call (:outcome step) value))}
               (next calls)))
      acc)))

(defn- dispatch-with-tools!
  "Dispatch one intent with the model tool-calling loop (post-v0
  extension 1), now routed through the trusted RequestAssembler.

  Why not in the provider? The provider only knows the wire request
  (messages/tools JSON). The scheduler must also know the wire
  tool-name -> EvoCLJ tool binding mapping to execute tool calls.
  If dynamic ToolSurface tools were only visible inside the provider,
  the model would see tool A but the scheduler's dispatch-tool loop
  would not know how to execute A (or vice versa): visible-to-model
  and executable-by-scheduler would diverge. The Assembler is trusted
  runtime code that produces BOTH sides together: the wire :tools
  vector for the model AND the :tool-map {wire-name -> tool binding}
  for the scheduler. It is the single place that merges BaseModelCall
  + dynamic context + dynamic tool catalog so the two views stay
  consistent.

  Pinning: the whole loop pins its ToolCatalogBinding. Round 1 sees
  A/B/C; if a LiveSource refresh publishes D between rounds, round 2
  still works on the same pinned snapshot. Only the next independent
  model call (new LLM node visit) sees the new catalog. Implemented
  by capturing the binding once at loop start and reusing it.

  Context rebuild: unlike tools, Context MUST be reassembled each
  round. Round 1's model may call activate_skill, the tool execution
  creates a new ContextBinding, and round 2 must see the newly
  activated Skill instructions. Therefore the Assembler is called
  every round with fresh SessionBindings/CAS; it cannot just append
  the tool result to the original messages and re-call."
  [executor pin cause intent outputs]
  (if (not= :intent/model-call (:intent/type intent))
    ;; Non-model intents go directly through the broker (no assembler).
    (let [cause-id (if (map? cause) (:event/id cause) cause)]
      (dispatch-intent! executor pin cause-id intent outputs))
    ;; Model-call path: trusted assembler owns the wire shape.
    (let [rounds (get-in intent [:payload :options :max-tool-rounds]
                         max-tool-rounds-default)
          base-call (base-call-from-intent* intent)
          ;; Pin the tool catalog once for the whole loop.
          initial-tools (or (:requested-tools base-call)
                            (get-in intent [:payload :tools]) [])
          ;; Allow tests / dynamic hosts to supply a live catalog atom
          ;; via executor's :tool-catalog. When present, pin its
          ;; current snapshot; otherwise pin the base-call's own tools.
          live-catalog (try (when-let [a (get-in executor [:stores :tool-catalog])]
                               (when (instance? clojure.lang.Atom a) @a))
                            (catch Throwable _ nil))
          pinned-source (or live-catalog initial-tools)
          ;; S14 (pin→provider resolution ENFORCED, not merely
          ;; implemented): the wire catalog the assembler will advertise to
          ;; the model must not carry a SILENT DANGLING TOOL REFERENCE. Before
          ;; the loop runs, resolve every pinned tool id against the
          ;; kernel-owned provider registry and FAIL CLOSED on a dangling id
          ;; (:provider/catalog-unresolved-tool) — a catalog consumer either
          ;; gets a fully-resolved reference or a typed error, never a partial
          ;; reference with a dangling id handed to the model/broker. Guarded
          ;; so a host that carries no registry atom (pure broker test) is
          ;; still safe: no registry, no S14 enforcement gate.
          _ (when-let [provider-registry (:registry (:dispatch executor))]
              (registry/resolve-tool-catalog provider-registry pinned-source))
          pinned (try (assembler/capture-tool-catalog-binding pinned-source)
                      (catch Throwable _
                        (assembler/capture-tool-catalog-binding initial-tools)))]
      (loop [current-base-call base-call
             current-intent intent
             cause cause
             outputs outputs
             rounds rounds
             pinned pinned]
        (let [cause-id (if (map? cause) (:event/id cause) cause)
              bindings (fetch-bindings executor pin cause-id)
              cas (:cas (:stores executor))
              prepared (try
                         (assembler/assemble current-base-call
                                             {:session-bindings bindings
                                              :tool-catalog/binding pinned
                                              :cas cas
                                              :history ""})
                         (catch Throwable _ nil))
              tool-map (if prepared (:tool-map prepared) (tool-map-of current-intent))
              effective-intent (if prepared
                                 (-> current-intent
                                     (assoc-in [:payload :messages] (:messages prepared))
                                     (assoc-in [:payload :tools] (:tools prepared))
                                     ;; keep base fields so a refreshed
                                     ;; intent still carries the original
                                     ;; lightweight declaration as well
                                     (assoc-in [:payload :base/messages] (:base/messages current-base-call))
                                     (assoc-in [:payload :requested-tools] (:requested-tools current-base-call)))
                                 current-intent)
              step (dispatch-intent! executor pin cause-id effective-intent outputs)
              value (peek (:outputs step))
              tool-calls (when (= :intent/model-call (:intent/type current-intent))
                           (:tool-calls value))]
          (if (and (seq tool-calls) (pos? rounds) (seq tool-map))
            (let [calls (mapv (fn [tc]
                                (if-let [t (get tool-map (:tool/name tc))]
                                  {:call tc :tool-id (:tool t)}
                                  (throw (err/error :scheduler/unknown-tool
                                                    (str "model requested unknown tool "
                                                         (:tool/name tc))
                                                    {:tool/name (:tool/name tc)}))))
                              tool-calls)
                  executed (execute-tool-calls! executor pin (:last-event step) effective-intent calls)
                  assistant-msg {:role :assistant
                                 :content (get-in value [:model/output :text] "")
                                 :tool-calls (dialect/tool-calls->wire tool-calls)}
                  ;; Build the next raw conversation history from the
                  ;; prepared's messages (which already include any
                  ;; injected context) plus the new assistant + tool
                  ;; results. On the next loop the assembler will
                  ;; re-inject fresh context (replacing the stale
                  ;; leading system message) so a binding created by
                  ;; the just-executed tool is visible.
                  prepared-msgs (if prepared (:messages prepared)
                                  (get-in effective-intent [:payload :messages]))
                  next-messages (into (vec prepared-msgs)
                                      (cons assistant-msg (:tool-msgs executed)))
                  next-base-call (assoc current-base-call
                                         :base/messages next-messages
                                         :messages next-messages)]
              (recur next-base-call
                     (assoc effective-intent :payload (assoc (:payload effective-intent) :messages next-messages))
                     (:last-event executed)
                     (:outputs executed)
                     (dec rounds)
                     pinned))
            step))))))

;; --- terminal session outcomes ----------------------------------------------

(defn- fail-session!
  "Fail the session (component Step 4): store the serializable error
  payload as a CAS artifact, append :node/failed (chained to `cause`,
  :payload-ref = the artifact), transition the session to :failed, and
  append :session/failed carrying the artifact ref in its metadata
  (:error/artifact-ref). Returns the run result map."
  [executor pin cause node-id step error-data outputs]
  (let [error-ref (put-payload! executor error-data)
        node-failed (append-event! executor pin cause :node/failed error-ref
                                   {:node/id node-id
                                    :step step
                                    :error/type (:error/type error-data)})]
    (session/transition-session! (:sqlite (:stores executor))
                                 (:session/id pin) :running :failed nil)
    (append-event! executor pin (:event/id node-failed) :session/failed error-ref
                   {:error/artifact-ref error-ref
                    :error/type (:error/type error-data)})
    {:status :failed
     :session/id (:session/id pin)
     :output-ref (outputs-ref executor outputs)
     :error/artifact-ref error-ref
     :episode/id nil}))

(defn- budget-exhaust!
  "Halt the run when the topology's :max-steps budget is consumed
  (component Step 2): transition the session to :budget-exhausted and
  append :session/budget-exhausted carrying the limit, the steps
  consumed, and the accumulated outputs as a CAS artifact. Returns the
  run result map."
  [executor pin cause outputs limits steps]
  (let [out-ref (outputs-ref executor outputs)]
    (session/transition-session! (:sqlite (:stores executor))
                                 (:session/id pin) :running :budget-exhausted nil)
    (append-event! executor pin cause :session/budget-exhausted out-ref
                   {:limits limits :steps steps :output/ref out-ref})
    {:status :budget-exhausted
     :session/id (:session/id pin)
     :output-ref out-ref
     :error/artifact-ref nil
     :episode/id nil}))
(defn- validate-effect-lattice!
  "Enforce PLT5 before a session leaves :created. Static Effects come
  from the compiled topology; Requested comes from the compiled genome
  manifest; Granted comes from the kernel-owned broker lease snapshot.
  Direct scheduler fixtures without a manifest retain the topology's
  Effects as their explicit Requested set."
  [executor topology]
  (let [compiled (get-in executor [:phenotype :compiled])
        effects (or (:effects compiled)
                    (:effects topology)
                    (capability/topology-effects topology))
        declared? (or (contains? compiled :requested-capabilities)
                      (contains? (:manifest compiled)
                                 :capabilities/requested))
        requested (if declared?
                    (or (:requested-capabilities compiled)
                        (get-in compiled
                                [:manifest :capabilities/requested]))
                    effects)
        granted (capability/granted-effects
                 (get-in executor [:dispatch :leases]))]
    (if declared?
      (capability/validate-effect-lattice! effects requested granted)
      (capability/validate-effect-lattice! effects requested))))

;; --- the scheduler ----------------------------------------------------------

(defn run-session!
  "Execute ONE session against the executor's phenotype topology
  (deterministic single-session FIFO — v0 has no concurrency).

  Reads the session's pinned genome/resolution/phenotype from the
  store and verifies them against the executor's compiled genome
  (never assumes the pin), walks the compiled topology from :entry,
  steps each node's handler, dispatches every emitted intent through
  evoclj.intent.dispatch! (the broker), feeds the provider results
  back into the accumulated :outputs, and persists EVERY transition
  via evoclj.store.event/append-event! (node/started, node/completed,
  intent/proposed, intent/authorized | intent/denied,
  provider/call-started, provider/call-completed, ...) and
  evoclj.store.session/transition-session! (:created → :resolving →
  :running → :waiting → :completed | :failed | :budget-exhausted —
  the store's normative state machine). The topology's :limits
  {:max-steps N}
  halts overlong runs as :budget-exhausted; an unhandled node failure
  fails the session with the error payload preserved as a CAS artifact
  ref in the :session/failed event metadata.

  See the namespace docstring for the executor map shape, the returned
  result map, and the error contract (:scheduler/executor-invalid,
  :scheduler/session-invalid, :scheduler/pin-mismatch,
  :scheduler/task-input-invalid)."
  [executor session-id task-input]
  (validate-executor! executor)
  (when-not (boundary/edn-safe? task-input)
    (throw (err/error :scheduler/task-input-invalid
                      "task-input must be plain EDN-safe data (Global Constraint 22)"
                      {:reason :not-edn-safe
                       :value (err/sanitize task-input)})))
  (let [db (:sqlite (:stores executor))
        pin (session/get-session db session-id)]
    (when-not pin
      (throw (err/error :scheduler/session-invalid
                        "no session with this id"
                        {:reason :not-found
                         :session/id session-id})))
    (when-not (= :created (:state pin))
      (throw (err/error :scheduler/session-invalid
                        "run-session! starts only sessions in :created"
                        {:reason :not-created
                         :session/id (:session/id pin)
                         :state (:state pin)})))
    ;; pin verification: the session's pinned identity IS the store's
    ;; contract — the executor must agree with it, never the reverse
    (let [compiled (:compiled (:phenotype executor))]
      (when-not (= (:genome/id pin) (:compiled/genome-id compiled))
        (throw (err/error :scheduler/pin-mismatch
                          "session pin disagrees with the executor's compiled genome"
                          {:reason :genome
                           :session/genome-id (:genome/id pin)
                           :executor/genome-id (:compiled/genome-id compiled)})))
      (when-not (= (:resolution/id pin) (:compiled/resolution-id compiled))
        (throw (err/error :scheduler/pin-mismatch
                          "session pin disagrees with the executor's compiled resolution"
                          {:reason :resolution
                           :session/resolution-id (:resolution/id pin)
                           :executor/resolution-id (:compiled/resolution-id compiled)})))
      (when-not (= (:phenotype/id pin) (:phenotype/id (:phenotype executor)))
        (throw (err/error :scheduler/pin-mismatch
                          "session pin disagrees with the executor's phenotype"
                          {:reason :phenotype
                           :session/phenotype-id (:phenotype/id pin)
                           :executor/phenotype-id (:phenotype/id (:phenotype executor))}))))
    (let [topology (get-in executor [:phenotype :compiled :topology])
          compiled (:compiled (:phenotype executor))
          declared-requested (cond
                               (contains? compiled :requested-capabilities)
                               (:requested-capabilities compiled)
                               (contains? (:manifest compiled)
                                          :capabilities/requested)
                               (get-in compiled
                                       [:manifest :capabilities/requested]))
          entry (:entry topology)
          limits (or (:limits topology) {})
          max-steps (:max-steps limits)
          root (first (event/events-for-session db (:session/id pin)))
          lattice (validate-effect-lattice! executor topology)
          dispatch-context (assoc (:dispatch executor)
                                  :effects (:effects lattice)
                                  :requested-capabilities (:requested lattice))
          executor (assoc executor :dispatch dispatch-context)]
      (when-not (= :session/created (:event/type root))
        (throw (err/error :scheduler/session-invalid
                          "session causal chain must open with a :session/created root event"
                          {:reason :missing-root-event
                           :session/id (:session/id pin)
                           :first-event (:event/type root)})))

      ;; the store's state machine has no :created → :running edge;
      ;; the :resolving hop is the normative path (component)
      ;; WO-B1 production wiring: restore the session's durable bindings
      ;; into the executor's runtime registries BEFORE the session leaves
      ;; :created (see restore-session-runtime! for the failure
      ;; discipline — verdicts abort typed, infrastructure degrades).
      (restore-session-runtime! executor pin root)
      (session/transition-session! db (:session/id pin) :created :resolving nil)
      (session/transition-session! db (:session/id pin) :resolving :running nil)
      (let [started (append-event! executor pin (:event/id root) :session/started
                                   (put-payload! executor task-input)
                                   {:entry entry})
            outcome
            (loop [node-id entry
                   input-event {:event/id (:event/id started)
                                :event/type :session/started
                                :payload task-input}
                   outputs []
                   steps 0
                   last-event started
                   loop-state {}]
              (if (and max-steps (>= steps max-steps))
                (budget-exhaust! executor pin (:event/id last-event)
                                 outputs limits steps)
                (let [node (get (:nodes topology) node-id)]
                  (if-not node
                    (fail-session! executor pin (:event/id last-event)
                                   node-id (inc steps)
                                   {:error/type :scheduler/node-not-found
                                    :error/message "topology :next references an undeclared node"
                                    :node/id node-id}
                                   outputs)
                    (let [started-event (append-event!
                                         executor pin (:event/id last-event)
                                         :node/started nil
                                         {:node/id node-id
                                          :step (inc steps)
                                          :node/type (:node/type node)})
                          runtime-state {:session/id (:session/id pin)
                                         :phenotype/id (:phenotype/id pin)
                                         :node/id node-id
                                         :outputs outputs
                                         :sci-runtime (:sci-runtime (:phenotype executor))
                                         :compiled (:compiled (:phenotype executor))
                                         :loop-state loop-state}
                          stepped (try
                                    {:transition
                                     (node/validate-transition!
                                      (node/step ((node/handler-for (:node/type node)))
                                                 runtime-state node input-event))}
                                    (catch Throwable t
                                      {:failed-outcome
                                       (fail-session! executor pin (:event/id started-event)
                                                      node-id (inc steps)
                                                      (err/error-data t) outputs)}))]
                      (if-let [failed-outcome (:failed-outcome stepped)]
                        failed-outcome
                        (let [transition (:transition stepped)]
                          (case (:transition/status transition)
                            :failed
                            (if (= :loop/max-iterations-exceeded
                                   (:error/type (:error transition)))
                              ;; component typed budget outcome: a :loop
                              ;; node whose iteration count reached
                              ;; :max-iterations is a budget outcome,
                              ;; routed to the same :budget-exhausted
                              ;; session state as the step budget
                              (budget-exhaust! executor pin
                                                (:event/id started-event)
                                                outputs
                                                {:max-iterations
                                                 (:max-iterations node)}
                                                (inc steps))
                              (fail-session! executor pin (:event/id started-event)
                                             node-id (inc steps)
                                             (:error transition) outputs))

                            :complete
                            (let [out-ref (outputs-ref executor (:outputs transition))
                                  completed (append-event!
                                             executor pin (:event/id started-event)
                                             :node/completed out-ref
                                             {:node/id node-id
                                              :step (inc steps)
                                              :transition/status :complete})]
                              ;; the store's state machine has no
                              ;; :running → :completed edge (component:
                              ;; :running ↔ :waiting → :completed), so
                              ;; completion walks :running → :waiting →
                              ;; :completed
                              (session/transition-session! db (:session/id pin)
                                                           :running :waiting nil)
                              (session/transition-session! db (:session/id pin)
                                                           :waiting :completed nil)
                              (append-event! executor pin (:event/id completed)
                                             :session/completed out-ref
                                             {:status :completed
                                              :output/ref out-ref})
                              {:status :completed
                               :session/id (:session/id pin)
                               :output-ref out-ref
                               :error/artifact-ref nil
                               :episode/id nil})

                            :continue
                            (let [completed (append-event!
                                             executor pin (:event/id started-event)
                                             :node/completed
                                             (payload-ref executor (:outputs transition))
                                             {:node/id node-id
                                              :step (inc steps)
                                              :transition/status :continue
                                              :next (:next transition)})
                                  dispatch-result
                                  (try
                                    (reduce (fn [{:keys [last-event outputs]} intent]
                                              (dispatch-with-tools!
                                               executor pin
                                               (:event/id last-event)
                                               intent outputs))
                                            {:last-event completed
                                             :outputs (into outputs (:outputs transition))}
                                            (:intents transition))
                                    (catch Throwable t
                                      {:failed-outcome
                                       (fail-session! executor pin (:event/id completed)
                                                      node-id (inc steps)
                                                      (err/error-data t) outputs)}))]
                              (if-let [failed-outcome (:failed-outcome dispatch-result)]
                                failed-outcome
                                (let [{:keys [last-event outputs]} dispatch-result
                                      ;; component: the loop counter travels in
                                      ;; runtime-state. Each time a :loop node
                                      ;; chooses to iterate (its :continue
                                      ;; transition's :next leads to its :body)
                                      ;; the scheduler increments that loop
                                      ;; node's counter for the next visit.
                                      loop-state
                                      (if (and (= :loop (:node/type node))
                                               (= (:body node)
                                                  (first (:next transition))))
                                        (update loop-state node-id (fnil inc 0))
                                        loop-state)]
                                  (if-let [nxt (first (:next transition))]
                                    (recur nxt
                                           {:event/id (:event/id last-event)
                                            :event/type (:event/type last-event)
                                            :payload (peek outputs)}
                                           outputs (inc steps) last-event
                                           loop-state)
                                    (fail-session! executor pin (:event/id last-event)
                                                   node-id (inc steps)
                                                   {:error/type :scheduler/dangling-run
                                                    :error/message "a :continue transition carries no successor"
                                                    :node/id node-id}
                                                   outputs)))))))))))))]
        (assoc outcome :event/count (event-count executor pin))))))
