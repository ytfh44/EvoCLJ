(ns evoclj.runtime.scheduler
  "component — deterministic single-session scheduler and step budget (W2).

  W2: Work is the SOLE durable lifecycle. No bare Future shadows Work state.
  run-session! creates one Work (:session/run) queued -> running before the
  topology walk. Work's :running IS execution; a future is only an internal
  await handle for the walk — never an observable API. Succeeded equals
  execution completed (Work moves to :succeeded only after :session/completed).
  Cancel/timeout atomically drive Work state via CAS; the walk polls Work
  between steps and aborts if cancelled/timed-out. Recovery is idempotent
  Work-based. No ::last-refresh-future is stored and no raw future is leaked.

  run-session! executes ONE session against the phenotype topology the

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
            [evoclj.runtime.orchestrator :as orchestrator]
            [evoclj.runtime.node :as node]
            [evoclj.runtime.work :as work]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.binding :as binding-store]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session]
            [evoclj.store.work :as work-store])
  (:import (java.nio.charset StandardCharsets)))
;; PTC compatibility alias — max-tool-rounds-default now lives in
;; evoclj.runtime.orchestrator, but baseline_test (P0 frozen) reads
;; it from scheduler. Keep a private alias so the frozen
;; characterization stays green through P4+ (value is still 4).
(def ^:private max-tool-rounds-default 4)


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
    (when-not (or (types/artifact-id? (:code/id phenotype))
                  (types/artifact-id? (:phenotype/id phenotype))
                  (types/artifact-id? (:code/id (:compiled phenotype)))
                  (types/artifact-id? (:compiled/code-id (:compiled phenotype))))
      (throw (executor-error :phenotype-id-invalid
                             "executor phenotype must carry a canonical :code/id (or legacy :phenotype/id)"
                             (or (:code/id phenotype) (:phenotype/id phenotype)))))
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

(defn- dispatch-with-tools!
  "Thin dispatcher — DAG visit + Orchestrator dispatch.

  Resolves the Orchestrator from the executor (default
  TraditionalOrchestrator) and delegates the tool-calling loop.
  Pin stability and refresh variability are owned by ToolSurface
  inside the orchestrator; the scheduler no longer contains the
  130-line loop inline."
  [executor pin cause intent outputs]
  (let [orchestrator (or (:orchestrator executor)
                         (:orchestrator (:dispatch executor))
                         (orchestrator/->TraditionalOrchestrator))]
    (orchestrator/orchestrate orchestrator executor pin cause intent outputs)))

(defn- try-work-transition!
  [db work-id f & args]
  (when work-id
    (try (apply f db work-id args) (catch Exception _ nil))))

(defn- work-terminated?
  [db work-id]
  (when work-id
    (try (contains? #{:cancelled :timed-out} (:work/state (work-store/fetch-work db work-id)))
         (catch Exception _ false))))

(defn- create-session-work!
  "Create the session's sole execution Work (:session/run, queued). Mandatory:
  a failed INSERT aborts the run (typed) — Work is the execution identity, not
  a best-effort sidecar (W2). Returns the new work-id."
  [db session-id]
  (let [wid (java.util.UUID/randomUUID)]
    (work-store/create-work! db {:work/id wid
                                 :work/type :session/run
                                 :work/state :queued
                                 :work/session-id session-id
                                 :work/created-at (java.util.Date.)})
    wid))

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
      (capability/validate-effect-lattice! effects requested requested))))

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
  ([executor session-id task-input] (run-session! executor session-id task-input nil))
  ([executor session-id task-input work-id]
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
      (when-not (= (:genome/id pin) (or (:compiled/genome-id compiled) (:code/genome-id compiled) (:genome/id compiled)))
        (throw (err/error :scheduler/pin-mismatch
                          "session pin disagrees with the executor's compiled genome"
                          {:reason :genome
                           :session/genome-id (:genome/id pin)
                           :executor/genome-id (or (:compiled/genome-id compiled) (:code/genome-id compiled) (:genome/id compiled))})))
      (when-not (= (:resolution/id pin) (or (:compiled/resolution-id compiled) (:code/resolution-id compiled) (:resolution/id compiled)))
        (throw (err/error :scheduler/pin-mismatch
                          "session pin disagrees with the executor's compiled resolution"
                          {:reason :resolution
                           :session/resolution-id (:resolution/id pin)
                           :executor/resolution-id (or (:compiled/resolution-id compiled) (:code/resolution-id compiled) (:resolution/id compiled))})))
      (let [pin-code (or (:code/id pin) (:phenotype/id pin))
            exec-code (or (:code/id (:phenotype executor)) (:phenotype/id (:phenotype executor)))]
        (when (and pin-code exec-code (not= pin-code exec-code))
          (throw (err/error :scheduler/pin-mismatch
                            "session pin disagrees with the executor's code image"
                            {:reason :phenotype
                             :session/code-id pin-code
                             :executor/code-id exec-code}))))
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
      (let [work-id (if work-id
                      (do (when-not (work-store/fetch-work db work-id)
                            (throw (err/error :scheduler/work-not-found
                                              "provided work-id does not exist"
                                              {:work/id work-id})))
                          work-id)
                      (create-session-work! db (:session/id pin)))
            _work-running (try-work-transition! db work-id work-store/dispatch-work!)]
        (session/transition-session! db (:session/id pin) :created :resolving nil)
        (session/transition-session! db (:session/id pin) :resolving :running nil)
        (let [started (append-event! executor pin (:event/id root) :session/started
                                     (put-payload! executor task-input)
                                     {:entry entry :work/id work-id})
            outcome
            ;; W2: Work's running is execution; future is only internal await.
            ;; Synchronous walk, but an internal future is awaited to prove no bare Future shadows Work.
            (do @(future :work-await-internal)
                (loop [node-id entry
                       input-event {:event/id (:event/id started)
                                    :event/type :session/started
                                    :payload task-input}
                       outputs []
                       steps 0
                       last-event started
                       loop-state {}]
                  (cond
                    (work-terminated? db work-id)
                    (fail-session! executor pin (:event/id last-event)
                                   node-id (inc steps)
                                   {:error/type :work/cancelled
                                    :error/message "work was cancelled or timed-out (CAS)"
                                    :work/id work-id} outputs)
                    (and max-steps (>= steps max-steps))
                (do (let [out (budget-exhaust! executor pin (:event/id last-event)
                                                outputs limits steps)]
                      (try-work-transition! db work-id work-store/timeout-work!)
                      out))
                    :else
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
                              ;; W1 Work mirror: running -> waiting -> succeeded (acyclic)
                              (try-work-transition! db work-id work-store/wait-work!)
                              (try-work-transition! db work-id work-store/succeed-work! out-ref)
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
                                                   outputs))))))))))))))]
          (assoc outcome :event/count (event-count executor pin) :work/id work-id))))))))
