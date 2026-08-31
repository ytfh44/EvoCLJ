(ns evoclj.tool.specs
  "Single source of truth for the Tool value object (D1).

  Unifies the three previously duplicated Tool descriptor definitions
  (provider registry, MCP codec / stable-descriptor, intent payload)
  into one normative contract. All other namespaces delegate to the
  schemas and validators defined here, satisfying INV-05 (single
  implementation) and GC-22 (EDN-safe validation).

  Contract C-Tool (D1):

    Tool {:tool/id keyword | [string string]
          :tool/audience #{:model :genome}
          :input-schema malli-schema (REAL, fail-closed)
          :output-schema malli-schema (REAL, fail-closed)
          :required-action keyword
          :lease/resource {:kind keyword :id any}}

  Provider descriptors extend the core Tool with the normative
  component fields (:effect, :retry, :version, and optional MCP
  provenance fields). The core Tool constraints still apply: :tool/id
  accepts a keyword for local tools or a [server-id remote-name]
  tuple for MCP composite tools, and :input-schema / :output-schema
  must be REAL schemas (fail-closed, never nil / :any / garbage).

  INV-05: json-schema->malli remains the single implementation in
  evoclj.mcp.codec and is not duplicated here; this namespace only
  reuses its REAL-schema predicate.

  GC-22: all descriptors validated through this namespace remain plain
  EDN-safe data (m/explain is sanitized, no Java objects leak)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.codec :as codec]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; Tool identity
;; ---------------------------------------------------------------------------

(def ToolIdSchema
  "Tool identifier: either a keyword for local/static tools
  (e.g. :fixture/echo, :evolution/evidence) or a two-element
  vector of strings for MCP composite tools ([server-id remote-name]).

  The vector form is the stable composite introduced in M12; it
  guarantees tools from different MCP servers never alias even when
  they share a remote name. Both forms are EDN-safe."
  [:or keyword? [:tuple string? string?]])

(defn tool-id?
  "True when x is a valid tool identifier (keyword or [string string] vector)."
  [x]
  (m/validate ToolIdSchema x))

;; ---------------------------------------------------------------------------
;; REAL-schema gate (fail-closed, delegates to codec)
;; ---------------------------------------------------------------------------

(def RealInputSchema
  "A REAL input schema (fail-closed): must be an explicit declared
  Malli schema, never nil / :any / missing. Delegates to
  evoclj.mcp.codec/real-schema? so there is exactly one predicate
  (INV-05)."
  [:fn codec/real-schema?])

(def RealOutputSchema
  "A REAL output schema (fail-closed): same rule as RealInputSchema,
  delegates to codec/real-schema?."
  [:fn codec/real-schema?])

;; ---------------------------------------------------------------------------
;; Core Tool value object (C-Tool contract)
;; ---------------------------------------------------------------------------

(def Tool
  "The canonical Tool value object (C-Tool / D1).

  Required keys:
    :tool/id          - keyword or [string string] composite (ToolIdSchema)
    :input-schema     - REAL Malli schema (fail-closed)
    :output-schema    - REAL Malli schema (fail-closed)
    :required-action  - keyword (e.g. :invoke)

  Optional keys:
    :tool/audience    - set of #{:model :genome} (where the tool is visible)
    :lease/resource   - {:kind keyword :id any} canonical resource
    :effect           - keyword (provider effect, e.g. :pure / :remote)
    :retry            - {:safe? boolean} (auto-retry safety)
    :version          - number (descriptor version)

  The map is closed in the core sense; provider descriptors add the
  optional MCP provenance fields defined in ToolDescriptorSchema."
  [:map {:closed false}
   [:tool/id ToolIdSchema]
   [:tool/audience {:optional true} [:set [:enum :model :genome]]]
   [:input-schema RealInputSchema]
   [:output-schema RealOutputSchema]
   [:required-action keyword?]
   [:lease/resource {:optional true} [:map [:kind keyword?] [:id any?]]]
   [:effect {:optional true} keyword?]
   [:retry {:optional true} [:map {:closed true} [:safe? boolean?]]]
   [:version {:optional true} number?]])

;; ---------------------------------------------------------------------------
;; Provider descriptor contract (normative, closed)
;; ---------------------------------------------------------------------------

(def ToolDescriptorSchema
  "The v0 tool descriptor contract (normative, component): a closed
  map of the five required fields plus optional :retry and :version
  blocks and optional MCP extension fields consumed by the MCP provider
  bridge and generic binding. Lifecycle MCP fields (generation,
  last-refreshed, captured-at) are allowed here for validation but
  generic dispatch no longer depends on them; they are treated as
  MCP-specific provenance in binding.

  Required:
    :tool/id         - keyword or [string string] composite (ToolIdSchema)
    :effect          - keyword
    :input-schema    - REAL Malli schema (fail-closed, delegates to codec/real-schema?)
    :output-schema   - REAL Malli schema (fail-closed, delegates to codec/real-schema?)
    :required-action - keyword

  Optional provider/MCP provenance fields are explicitly enumerated so
  the top level remains closed (fail-closed on unknown keys)."
  [:map {:closed true}
   [:tool/id ToolIdSchema]
   [:effect keyword?]
   [:input-schema RealInputSchema]
   [:output-schema RealOutputSchema]
   [:required-action keyword?]
   [:tool/audience {:optional true} [:set [:enum :model :genome]]]
   [:lease/resource {:optional true} [:map [:kind keyword?] [:id any?]]]
   [:retry {:optional true} [:map {:closed true} [:safe? boolean?]]]
   [:version {:optional true} number?]
   [:tool/description {:optional true} string?]
   [:tool/parameters {:optional true} any?]
   [:tool/budget {:optional true} [:map {:closed false} [:max-calls {:optional true} pos-int?]]]
   [:mcp/connection-id {:optional true} keyword?]
   [:mcp/server-id {:optional true} string?]
   [:mcp/last-refreshed {:optional true} any?]
   [:mcp/generation {:optional true} int?]
   [:mcp/captured-at {:optional true} any?]
   [:provider/input-schema {:optional true} any?]
   [:provider/output-schema {:optional true} any?]
   [:mcp/input-schema {:optional true} any?]
   [:mcp/output-schema {:optional true} any?]
   [:mcp/input-schema-json {:optional true} any?]
   [:mcp/output-schema-json {:optional true} any?]
   [:mcp/schema-source {:optional true} [:enum :malli :json-schema-fallback]]
   [:mcp/name {:optional true} string?]
   [:mcp/title {:optional true} string?]
   [:mcp/description {:optional true} string?]
   [:mcp/status {:optional true} keyword?]
   [:mcp/removed-at {:optional true} int?]
   [:mcp/retry-safe? {:optional true} boolean?]
   [:mcp/output-schema-kind {:optional true} keyword?]])

;; ---------------------------------------------------------------------------
;; ToolSurface
;; ---------------------------------------------------------------------------

(def ToolSurface
  "A ToolSurface (environment surface of type :tools): a stable,
  versioned collection of tool descriptors keyed by ToolId.

  Shape:
    {:surface/type :tools
     :surface/id any
     :entries {ToolId -> ToolDescriptorSchema}
     :revision/id (optional) string
     :revision/seq (optional) int}

  The surface is the single source of truth for the per-round
  visible-to-model / executable-by-scheduler tool set."
  [:map
   [:surface/type [:= :tools]]
   [:surface/id any?]
   [:entries [:map-of ToolIdSchema ToolDescriptorSchema]]
   [:revision/id {:optional true} [:maybe string?]]
   [:revision/seq {:optional true} int?]])

(def ToolSurfaceSchema
  "Alias for ToolSurface (kept for backward compatibility with
  callers that reference ToolSurfaceSchema by name)."
  ToolSurface)

;; ---------------------------------------------------------------------------
;; Validation helpers (fail-closed, EDN-safe)
;; ---------------------------------------------------------------------------

(defn- ensure-schema-value!
  "Throw :provider/descriptor-invalid when s is not a valid Malli
  schema value (e.g. 42, [:map [:text]], or an unregistered keyword),
  carrying the offending field name. This is the second gate after
  the REAL-schema check: REAL ensures the value is present and not
  :any, while this gate ensures the value is a well-formed Malli
  schema (m/schema does not throw)."
  [label s]
  (try
    (m/schema s)
    (catch clojure.lang.ExceptionInfo _
      (throw (err/error :provider/descriptor-invalid
                        (str label " is not a valid Malli schema value")
                        {:reason :invalid-schema :field label
                         :value (err/sanitize s)})))))

(defn validate-descriptor
  "Validate x as a v0 tool descriptor (normative shape, component).

  Returns x unchanged when it is well-formed; validation never
  coerces or rewrites values. Otherwise throws
  :provider/descriptor-invalid carrying a fully serializable Malli
  explanation (safe for pr-str / clojure.edn read-string
  round-tripping).

  This is the single implementation; all other namespaces delegate
  here (INV-05). The :input-schema / :output-schema gates are
  fail-closed and delegate to evoclj.mcp.codec/real-schema? via the
  RealInputSchema / RealOutputSchema schemas."
  [x]
  (when-not (m/validate ToolDescriptorSchema x)
    (throw (err/error :provider/descriptor-invalid
                      "provider descriptor failed schema validation"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize (m/explain ToolDescriptorSchema x))})))
  (ensure-schema-value! :input-schema (:input-schema x))
  (ensure-schema-value! :output-schema (:output-schema x))
  x)

(defn validate-tool
  "Validate x as a core Tool value object (C-Tool contract).

  Returns x unchanged when valid; otherwise throws
  :provider/descriptor-invalid with a sanitized explanation."
  [x]
  (when-not (m/validate Tool x)
    (throw (err/error :provider/descriptor-invalid
                      "tool failed schema validation"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize (m/explain Tool x))})))
  (ensure-schema-value! :input-schema (:input-schema x))
  (ensure-schema-value! :output-schema (:output-schema x))
  x)

;; ---------------------------------------------------------------------------
;; Code execution tool (P9) — single source C-Tool definition
;; ---------------------------------------------------------------------------

(def code-execution-tool-id
  "Canonical ToolId for the CodeMode sandbox execution tool."
  :ptc/code-execution)

(def code-execution-tool
  "Canonical C-Tool value object for code_execution. Reused by
  ToolSurface and Assembler for declaration; no duplication elsewhere.
  The SCI sandbox executes the code with injected toolFns (P8)."
  {:tool/id code-execution-tool-id
   :input-schema [:map
                  [:code string?]
                  [:language {:optional true} string?]]
   :output-schema [:map
                   [:value any?]
                   [:status {:optional true} keyword?]]
   :required-action :ptc/code-execution
   :effect :pure
   :tool/audience #{:model}})

(def code-execution-wire-tool
  "Wire declaration for code_execution as seen by the model
  (OpenAI function-tool shape plus :tool id for scheduler routing).
  Single wire shape derived from code-execution-tool (INV-05)."
  {:name "code_execution"
   :description "Execute SCI Clojure code with toolFns"
   :parameters {:type "object"
                :properties {:code {:type "string"
                                    :description "SCI Clojure source code to execute"}
                             :language {:type "string"
                                        :description "Language identifier, must be sci-clojure"}}
                :required ["code"]}
   :tool code-execution-tool-id})

 (defn code-execution-wire-tool?
   "True when m is the code_execution wire declaration."
   [m]
   (and (map? m)
        (= "code_execution" (:name m))
        (= code-execution-tool-id (:tool m))))

;; ---------------------------------------------------------------------------
;; Agent tool surface (S6) — broker-executable :agent/spawn + :agent/status
;; ---------------------------------------------------------------------------

(def agent-spawn-tool-id
  "Broker tool id for spawning a subagent session."
  :agent/spawn)

(def agent-status-tool-id
  "Broker tool id for querying subagent status."
  :agent/status)

(def agent-spawn-tool
  "Canonical C-Tool / provider descriptor for :agent/spawn.

  INPUT is the tool's model-facing args: {:task string :capabilities [any]}.
  :required-action is :invoke (capability-gated via broker). :effect is :pure
  for idempotency semantics — spawn is persisted via session create + event,
  and depth/budget caps are enforced fail-closed.
  :tool/budget {:max-calls 10} mirrors the assignment surface."
  {:tool/id agent-spawn-tool-id
   :tool/description "Spawn a subagent session"
   :tool/parameters {:type "object"
                     :properties {:task {:type "string"
                                        :description "The task text for the child session"}
                                 :capabilities {:type "array"
                                                :description "Requested capability hints (optional)"
                                                :items {:type "string"}}}
                     :required ["task"]}
   :tool/budget {:max-calls 10}
   :effect :pure
   :input-schema [:map {:closed true}
                  [:task string?]
                  [:capabilities {:optional true} [:vector any?]]]
   :output-schema [:map {:closed false}
                   [:child/session-id uuid?]
                   [:child/capabilities {:optional true} [:vector :map]]]
   :required-action :invoke
   :lease/resource {:kind :tool :id agent-spawn-tool-id}
   :tool/audience #{:model}})

(def agent-status-tool
  "Canonical C-Tool / provider descriptor for :agent/status."
  {:tool/id agent-status-tool-id
   :tool/description "Query subagent status"
   :tool/parameters {:type "object"
                     :properties {:session-id {:type "string"
                                              :description "Child session id (uuid string)"}}
                     :required ["session-id"]}
   :effect :pure
   :input-schema [:map {:closed true}
                  [:session-id string?]]
   :output-schema [:map {:closed false}
                   [:session/id {:optional true} uuid?]
                   [:state {:optional true} keyword?]]
   :required-action :invoke
   :lease/resource {:kind :tool :id agent-status-tool-id}
   :tool/audience #{:model}})

(def agent-spawn-wire-tool
  "Wire declaration for :agent/spawn (OpenAI function-tool shape + :tool id)."
  {:name "agent_spawn"
   :description "Spawn a subagent session"
   :parameters (:tool/parameters agent-spawn-tool)
   :tool agent-spawn-tool-id})

(def agent-status-wire-tool
  "Wire declaration for :agent/status."
  {:name "agent_status"
   :description "Query subagent status"
   :parameters (:tool/parameters agent-status-tool)
   :tool agent-status-tool-id})

(defn agent-tool?
  "True when m is one of the :agent/spawn or :agent/status wire declarations."
  [m]
  (and (map? m)
       (or (= agent-spawn-tool-id (:tool m))
           (= agent-status-tool-id (:tool m)))))
