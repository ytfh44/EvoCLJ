(ns evoclj.runtime.node
  "Node handler protocol and pure transitions (component).

  A node handler computes ONE pure transition for one node of a
  compiled Genome graph:

    (step handler runtime-state node input-event)
    ;; => {:transition/status :continue
    ;;     :outputs [...]
    ;;     :intents [...]
    ;;     :next [:node/x]}

  The transition is pure DATA: handlers NEVER call providers or the
  capability broker (Global Constraint 8 — every external effect must
  cross the kernel-owned Intent/Capability Broker, so a handler only
  EMITS a validated Intent; the component scheduler dispatches it), and
  only validated, fully serializable Clojure data crosses this
  boundary (Global Constraint 22). Attribution is a parameter, never
  guessed: the intents a handler constructs carry :session/id,
  :phenotype/id, and :node/id from runtime-state and :cause/event-id
  from the input-event (Global Constraint 20 — every externally
  visible effect is attributable).

  THE TRANSITION SHAPE (normative):

    :continue — the node advanced the session; :next holds the
      successor node ids (the compiled node's single :next wrapped in
      a vector — the [:node/x] shape — empty when the node declares
      none) and :outputs holds the values this step produced (the
      scheduler accumulates them).
    :complete — the session is finished; :outputs carries the final
      session result (for :emit, the accumulated outputs) and there is
      no :next. The scheduler must complete the session here.
    :failed   — the step failed as runtime data (a program error, an
      evolvable decision the handler rejects); :error carries
      serializable error data. The scheduler must fail the session
      and preserve the error artifact.

  One shared Malli schema (TransitionSchema) validates EVERY handler
  result (component Step 3): every handler ends by passing its result
  through validate-transition!, which enforces the schema and requires
  every :intents entry to be a FULLY validated Intent
  (evoclj.intent.schema/IntentSchema), so no un-validated intent
  request can cross this boundary. The scheduler (component) should
  validate again as defense in depth.

  RUNTIME-STATE CONTRACT (the per-session map the scheduler injects;
  designed here, normative for component):

    {:session/id #uuid \"...\"          ; REQUIRED attribution
     :phenotype/id \"sha256:<64 hex>\"  ; REQUIRED attribution
     :node/id :node/x                  ; REQUIRED attribution (the
                                       ;   node being stepped)
     :outputs [...]                    ; REQUIRED accumulated session
                                       ;   outputs (a vector; the
                                       ;   :emit handler completes
                                       ;   with them)
     :sci-runtime {...}                ; the phenotype's isolated SCI
                                       ;   runtime map (required for
                                       ;   :sci and :loop nodes only)
     :budget {:wall-ms 1000}           ; OPTIONAL default intent budget
                                       ;   (default-budget when absent)
     :limits {...}                     ; OPTIONAL SCI limits for :sci
                                       ;   invocations
     :loop-state {node-id n}           ; OPTIONAL per-session loop
                                       ;   counters (component): a map
                                       ;   of :loop node id -> iteration
                                       ;   count, threaded by the
                                       ;   scheduler in its visit loop;
                                       ;   the :loop handler reads its
                                       ;   count here — LOOP STATE IS
                                       ;   SESSION-LOCAL DATA, never a
                                       ;   SCI global var (Global
                                       ;   Constraint 23)
     ...}                              ; other keys (e.g. :providers)
                                       ;   pass through untouched —
                                       ;   handlers never read them

  INPUT-EVENT CONTRACT:

    {:event/id <pos-int>               ; REQUIRED the causal event id
                                       ;   (:cause/event-id attribution
                                       ;   for emitted intents)
     :event/type <keyword>             ; the event type (informational)
     :payload <EDN>}                   ; the node's input value (the
                                       ;   :sci program input, the
                                       ;   :tool args); nil when absent

  REGISTRY: node-type keyword -> trusted handler constructor. The
  syntax set mirrors evoclj.compiler.topology/syntax-node-types and
  equals evoclj.compiler.topology/executable-node-types: every known
  v0 type has a handler. Definition > validation: only executable types
  are representable via compile, and handler-for resolves a constructor
  for every syntax type; unknown types throw :node/unknown-type.
  (The :route reservation was removed — it declared only a single :next
  edge with no branch attributes, carrying no semantics a plain edge
  lacks; see syntax-node-types.)
  handler-for resolves a constructor; the scheduler steps with
  (node/step ((node/handler-for (:node/type node))) runtime-state
  node input-event).

  Error contract (Global Constraint 22 — plain serializable data):
  :node/runtime-invalid (malformed runtime-state; :reason
  distinguishes :not-a-map, :session-id-invalid,
  :phenotype-id-invalid, :node-id-invalid, :outputs-invalid,
  :budget-invalid, :sci-runtime-missing), :node/input-invalid
  (malformed input-event or tool args; :reason :not-a-map,
  :event-id-invalid, :args-invalid), :node/invalid (malformed node or
  evolvable decision; :reason :not-a-map, :type-mismatch,
  :missing-required-key, :invalid-attribute, :invalid-program-output,
  :invalid-intent-request, :unknown-intent-type),
  :node/transition-invalid (a handler result that fails the shared
  schema), and :node/unknown-type.

  Load order: the handler namespaces (evoclj.runtime.nodes.*) require
  THIS namespace (their reify needs the protocol var at compile time),
  so they are required at the bottom of the file — after the protocol
  and every cross-referenced var are defined — and the registry is
  assembled from their constructors. Clojure's loader returns an
  in-progress namespace immediately, so no load-order cycle occurs."
  (:require [evoclj.intent.schema :as intent-schema]
            [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; --- the handler protocol ---------------------------------------------------

(defprotocol NodeHandler
  (step [handler runtime-state node input-event]
    "Compute ONE pure transition for `node` (a compiled node map) in
    the per-session `runtime-state`, caused by `input-event`. Returns
    a transition map validated by evoclj.runtime.node/TransitionSchema
    (see the namespace docstring for the :continue / :complete /
    :failed shapes). The implementation MUST be pure: it never calls
    providers, the broker, the store, or any effectful host code
    (Global Constraints 8, 20, 22)."))

;; --- shared defaults and type set ------------------------------------------

(def default-budget
  "The v0 default intent budget ({:wall-ms 1000}, matching the SCI
  default wall-clock budget) used when runtime-state carries no
  :budget."
  {:wall-ms 1000})

(def syntax-node-types
  "The v0 syntax node type set, mirroring
  evoclj.compiler.topology/syntax-node-types (definition). Every known
  type has a handler — the :route reservation was removed (it declared
  only a single :next edge with no branch attributes, no semantics a
  plain edge lacks). A test asserts syntax sets stay equal across
  compiler and runtime so Definition > validation holds."
  #{:llm :sci :tool :loop :emit :memory/read :memory/write})

(def executable-node-types
  "The subset of syntax-node-types the runtime can execute today
  (handler exists), mirroring
  evoclj.compiler.topology/executable-node-types. Every syntax type has
  a handler, so this equals syntax-node-types; the separate def stays
  so Definition > validation (only executable types are representable
  via compile) keeps a named executable side."
  #{:llm :sci :tool :loop :emit :memory/read :memory/write})


;; --- the shared transition schema ------------------------------------------

(def TransitionSchema
  "The ONE shared transition schema validating EVERY handler result
  (component Step 3): a :multi dispatching on :transition/status.

  - :continue — :next is a vector of successor node ids (possibly
    empty when the node declares none), :outputs the values this step
    produced (the scheduler accumulates them).
  - :complete — terminal; :outputs carries the final session result
    (for :emit, the accumulated outputs), no :next.
  - :failed — :error carries serializable error data (e.g. from
    evoclj.kernel.error/error-data); the scheduler fails the session
    and preserves the error artifact.

  Every map is closed; :intents entries must be FULLY validated
  Intents (evoclj.intent.schema/IntentSchema — attribution included),
  so a raw un-validated intent request can never cross this boundary
  (Global Constraint 22)."
  [:multi {:dispatch :transition/status}
   [:continue
    [:map {:closed true}
     [:transition/status [:= :continue]]
     [:outputs [:vector any?]]
     [:intents [:vector intent-schema/IntentSchema]]
     [:next [:vector keyword?]]]]
   [:complete
    [:map {:closed true}
     [:transition/status [:= :complete]]
     [:outputs [:vector any?]]
     [:intents [:vector intent-schema/IntentSchema]]]]
   [:failed
    [:map {:closed true}
     [:transition/status [:= :failed]]
     [:outputs [:vector any?]]
     [:intents [:vector intent-schema/IntentSchema]]
     [:error [:map {:closed false}]]]]])

(defn validate-transition!
  "Validate `x` against TransitionSchema, the one shared schema for
  every node handler result.

  First the EDN-safe boundary gate (Global Constraint 22), then the
  schema. Returns x unchanged when it is a valid transition (never
  coerces); otherwise throws :node/transition-invalid carrying the
  sanitized input under :value and a fully serializable Malli
  explanation under :explanation.

  Every handler ends with this call, so a handler bug fails at the
  handler; the component scheduler should validate again as defense in
  depth."
  [x]
  (when-not (boundary/edn-safe? x)
    (throw (err/error :node/transition-invalid
                      "transition must be plain EDN-safe data (Global Constraint 22)"
                      {:value (err/sanitize x)})))
  (if (m/validate TransitionSchema x)
    x
    (throw (err/error :node/transition-invalid
                      "node handler result failed the shared transition schema"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize (m/explain TransitionSchema x))}))))

;; --- trust-boundary validation ---------------------------------------------

(defn validate-runtime-state!
  "Validate the per-session runtime-state contract (see the namespace
  docstring): a map carrying uuid :session/id, canonical
  :phenotype/id, keyword :node/id, and vector :outputs, with an
  optional schema-valid :budget. Other keys (:sci-runtime, :limits,
  :providers, ...) pass through untouched — handlers never read
  providers. Throws :node/runtime-invalid with a distinguishing
  :reason."
  [runtime-state]
  (when-not (map? runtime-state)
    (throw (err/error :node/runtime-invalid
                      "runtime-state must be a map"
                      {:reason :not-a-map
                       :value (err/sanitize runtime-state)})))
  (let [sid (:session/id runtime-state)]
    (when-not (uuid? sid)
      (throw (err/error :node/runtime-invalid
                        "runtime-state must carry a uuid :session/id"
                        {:reason :session-id-invalid
                         :value (err/sanitize sid)}))))
  (when-not (intent-schema/phenotype-id? (:phenotype/id runtime-state))
    (throw (err/error :node/runtime-invalid
                      "runtime-state must carry a canonical :phenotype/id"
                      {:reason :phenotype-id-invalid
                       :value (err/sanitize (:phenotype/id runtime-state))})))
  (when-not (keyword? (:node/id runtime-state))
    (throw (err/error :node/runtime-invalid
                      "runtime-state must carry a keyword :node/id"
                      {:reason :node-id-invalid
                       :value (err/sanitize (:node/id runtime-state))})))
  (when-not (vector? (:outputs runtime-state))
    (throw (err/error :node/runtime-invalid
                      "runtime-state must carry a vector of accumulated :outputs"
                      {:reason :outputs-invalid
                       :value (err/sanitize (:outputs runtime-state))})))
  (when (contains? runtime-state :budget)
    (when-not (m/validate intent-schema/BudgetSchema (:budget runtime-state))
      (throw (err/error :node/runtime-invalid
                        "runtime-state :budget must be a valid intent budget"
                        {:reason :budget-invalid
                         :value (err/sanitize (:budget runtime-state))}))))
  runtime-state)

(defn validate-input-event!
  "Validate the causal input-event contract: a map carrying an integer
  :event/id (the :cause/event-id attribution for emitted intents). The
  :payload (the node's input value) is optional and may be any EDN
  value. Throws :node/input-invalid with a distinguishing :reason.
  Handlers that ignore their input (:emit) need not call this."
  [input-event]
  (when-not (map? input-event)
    (throw (err/error :node/input-invalid
                      "input-event must be a map"
                      {:reason :not-a-map
                       :value (err/sanitize input-event)})))
  (when-not (int? (:event/id input-event))
    (throw (err/error :node/input-invalid
                      "input-event must carry an integer :event/id"
                      {:reason :event-id-invalid
                       :value (err/sanitize (:event/id input-event))})))
  input-event)

(def ^:private handler-required-keys
  "Per-handler required node keys, mirroring the compiler's
  evoclj.compiler.topology/required-keys for the implemented types."
  {:sci #{:program}
   :tool #{:tool}
   :loop #{:body :exit :until :max-iterations}
   :emit #{}
   :memory/read #{:memory}
   :memory/write #{:memory}})

(def ^:private handler-attribute-keys
  "Node keys whose value must be a keyword when present (mirrors the
  compiler's attribute rule)."
  [:program :tool :next :exit :body :until :memory])

(defn validate-node!
  "Validate the compiled node map for `expected-type` (the handler's
  node type): a map whose :node/type matches, carrying the type's
  required keys, with keyword-valued attributes. Throws
  :node/invalid with a distinguishing :reason (host-side bug; a
  malformed node never reaches a handler body)."
  [node expected-type]
  (when-not (map? node)
    (throw (err/error :node/invalid
                      "node must be a map"
                      {:reason :not-a-map
                       :value (err/sanitize node)})))
  (when-not (= expected-type (:node/type node))
    (throw (err/error :node/invalid
                      "node :node/type does not match the handler type"
                      {:reason :type-mismatch
                       :node/type (:node/type node)
                       :expected expected-type})))
  (doseq [k (get handler-required-keys expected-type)]
    (when-not (contains? node k)
      (throw (err/error :node/invalid
                        "node missing required key"
                        {:reason :missing-required-key
                         :node/type expected-type
                         :key k}))))
  (doseq [k handler-attribute-keys]
    (when (and (contains? node k) (not (keyword? (get node k))))
      (throw (err/error :node/invalid
                        "node attribute must be a keyword"
                        {:reason :invalid-attribute
                         :node/type expected-type
                         :key k
                         :value (err/sanitize (get node k))}))))
  (when (and (= :loop expected-type) (contains? node :next))
    (throw (err/error :node/invalid
                      "a :loop node must use :exit; :next is not a Region edge"
                      {:reason :loop-next-forbidden
                       :node/type expected-type
                       :key :next})))
  node)

(defn successor
  "The transition's normal successor vector for a compiled node:
  sequential nodes use :next, while a :loop uses its Region :exit.
  The controlled :body edge is selected only by the loop handler."
  [node]
  (if-let [nxt (if (= :loop (:node/type node))
                 (:exit node)
                 (:next node))]
    [nxt]
    []))

;; --- registry ---------------------------------------------------------------

;; The handler namespaces (evoclj.runtime.nodes.*) require THIS
;; namespace — their reify needs the NodeHandler protocol var at
;; compile time — so they are required here, after the protocol and
;; every cross-referenced var above, and the registry is assembled from
;; their constructors. Clojure's loader returns an in-progress
;; namespace immediately, so no load-order cycle occurs.
(require '[evoclj.runtime.nodes.emit :as emit])
(require '[evoclj.runtime.nodes.llm :as llm])
(require '[evoclj.runtime.nodes.loop :as loop])
(require '[evoclj.runtime.nodes.memory :as memory])
(require '[evoclj.runtime.nodes.sci :as sci])
(require '[evoclj.runtime.nodes.tool :as tool])

(def node-handler-registry
  "The trusted registry: v0 node type keyword -> handler constructor
  (a 0-ary fn returning a NodeHandler). Every v0 type is implemented:
  :emit, :sci, :tool, :loop, :llm, :memory/read, and :memory/write
  (:loop landed in component, :llm in post-v0 extension 1, :memory/*
  in feature R1; the :route reservation was removed — single-:next edge
  with no branch attributes, no semantics beyond a plain edge)."
  {:emit emit/emit-handler
   :sci sci/sci-handler
   :tool tool/tool-handler
   :loop loop/loop-handler
   :llm llm/llm-handler
   :memory/read memory/read-handler
   :memory/write memory/write-handler})

(defn handler-for
  "Resolve the trusted handler constructor for `node-type` (a v0 node
  type keyword): every member of the registry resolves to its
  constructor fn (call it with no args to build the handler:
  ((handler-for :sci))). Anything else — including the removed :route
  reservation — throws :node/unknown-type: only executable types are
  representable, so an unhandled type can never arrive via a compiled
  topology.

  The scheduler steps a node with
  (node/step ((node/handler-for (:node/type node))) runtime-state node
  input-event)."
  [node-type]
  (if (contains? node-handler-registry node-type)
    (get node-handler-registry node-type)
    (throw (err/error :node/unknown-type
                      "node type is not part of the v0 node type set"
                      {:node/type (err/sanitize node-type)}))))
