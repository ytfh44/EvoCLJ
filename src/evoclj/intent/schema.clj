(ns evoclj.intent.schema
  "Malli schemas for the v0 Intent ABI (component).

  An Intent is the ONLY way evolvable code requests an effect: a
  validated, immutable, plain-data map. The base shape and the six v0
  intent types are normative:

    {:intent/id #uuid \"...\"
     :intent/type :intent/tool-call
     :session/id #uuid \"...\"
     :phenotype/id \"sha256:...\"
     :node/id :node/tool
     :cause/event-id 17
     :payload {...}
     :budget {:wall-ms 1000}
     :metadata {}}

  Global Constraint 20 makes every externally visible effect
  attributable to session, phenotype, node, intent, authorization
  decision, and outcome; the attribution fields here are therefore
  REQUIRED — never optional, never invented by a constructor or
  normalizer. Global Constraint 22 keeps only validated Clojure data on
  this boundary: validate-intent rejects anything that is not EDN-safe
  (raw Java objects, lazy sequences, records, functions) BEFORE schema
  checking, reusing the SCI boundary predicate (evoclj.sci.boundary).

  Validation never coerces: a valid intent is returned unchanged; any
  failure throws :intent/schema-invalid (or :intent/not-edn-safe) with
  a fully serializable Malli explanation (safe for pr-str / clojure.edn
  read-string round-tripping)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [evoclj.tool.specs :as tool.specs]
            [malli.core :as m]))

;; --- identifiers ------------------------------------------------------------

(def ^:private sha256-id-re #"^sha256:[0-9a-f]{64}$")

(defn phenotype-id?
  "True when x is a canonical PhenotypeId: a \"sha256:<64 hex>\" string,
  the same content-addressed form used for Genome/Resolution/Artifact
  IDs in evoclj.genome.types."
  [x]
  (and (string? x) (boolean (re-matches sha256-id-re x))))

(def PhenotypeIdSchema
  "A canonical content-addressed PhenotypeId string."
  [:fn phenotype-id?])

;; --- intent type and budget -------------------------------------------------

(def IntentTypeSchema
  "The v0 intent types extended with subagent lifecycle intents.
  The original six v0 types plus :intent/subagent-spawn,
  :intent/subagent-result, and :intent/subagent-cancel. An intent of any
  other :intent/type is rejected at the boundary."
  [:enum :intent/model-call :intent/tool-call :intent/memory-read
   :intent/memory-write :intent/finish :intent/fail
   :intent/subagent-spawn :intent/subagent-result :intent/subagent-cancel])

(def ^:private NonNegIntSchema
  "A non-negative integer (budgets, limits)."
  [:and :int [:fn (fn [x] (not (neg? x)))]])

(def BudgetSchema
  "A v0 budget: :wall-ms (required, non-negative) plus optional
  :max-steps, open to further budget keys. A negative :wall-ms is
  rejected: an intent can never request a negative budget."
  [:map {:closed false}
   [:wall-ms NonNegIntSchema]
   [:max-steps {:optional true} NonNegIntSchema]])

;; --- per-type payload contracts --------------------------------------------

(def PayloadModelCallSchema
  "A model-call payload: the model referenced by its full models.dev
  id (a string like \"deepseek/deepseek-v4-flash\", or a keyword
  accepted for compatibility), the messages as a vector of message
  maps, and an optional :tools vector of function-tool declarations
  (each {:name :description :parameters :tool} — :tool is the
  internal mapping back to the EvoCLJ tool id, stripped before
  serialization). Open to further keys."
  [:map {:closed false}
   [:model/id [:or keyword? string?]]
   [:messages [:vector :map]]
   [:tools {:optional true} [:vector :map]]])

(def PayloadToolCallSchema
  "A tool-call payload: the tool referenced by its identifier and its
  argument map (the M3 fixture shape {:tool/id :fixture/echo :args {...}}
  or the MCP composite shape {:tool/id [\"server-a\" \"read_file\"] :args {...}}).
  :tool/id delegates to evoclj.tool.specs/ToolIdSchema (keyword or
  [string string] tuple, D1) so there is exactly one identifier contract.
  Open to further keys."
  [:map {:closed false}
   [:tool/id tool.specs/ToolIdSchema]
   [:args :map]])

(def PayloadMemoryReadSchema
  "A memory-read payload: the episodic memory key, with an optional
  non-negative result limit. Episodic memory reads stay distinct from
  procedural Genome changes (Global Constraint 10)."
  [:map {:closed false}
   [:memory/key keyword?]
   [:memory/limit {:optional true} NonNegIntSchema]])

(def PayloadMemoryWriteSchema
  "A memory-write payload: the episodic memory key and its content (any
  EDN-safe value; EDN-safety is enforced by the boundary gate)."
  [:map {:closed false}
   [:memory/key keyword?]
   [:memory/content any?]])
(def PayloadFinishSchema
  "A finish payload carrying the task result value."
  [:map {:closed false}
   [:value any?]])

(def PayloadFailSchema
  "A fail payload carrying a human-readable message and an optional
  value."
  [:map {:closed false}
   [:message string?]
   [:value {:optional true} any?]])

(def ^:private cas-ref-re #"^sha256:[0-9a-f]{64}$")

(defn cas-ref?
  "True when x is a canonical CAS reference: a \"sha256:<64 hex>\" string,
  the same content-addressed form used for Phenotype/Artifact IDs."
  [x]
  (and (string? x) (boolean (re-matches cas-ref-re x))))

(def CasRefSchema
  "A canonical content-addressed CAS reference string (sha256:...)."
  [:fn cas-ref?])

(def PayloadSubagentSpawnSchema
  "A subagent-spawn payload: the parent session id, an open child spec
  map (e.g. {:genome/id string? :task any?}), and a vector of
  CapabilityLease maps (may be empty). All keys use malli
  string/uuid/keyword/vector/map checks only (GC-22: EDN-safe, no raw
  objects). The map is open to further keys."
  [:map {:closed false}
   [:parent/session-id uuid?]
   [:child/spec [:map {:closed false}]]
   [:child/capabilities [:vector [:map {:closed false}]]]])

(def PayloadSubagentResultSchema
  "A subagent-result payload: parent and child session ids plus a
  result CAS reference (sha256:...). The map is open to further keys.
  GC-22: EDN-safe malli checks only."
  [:map {:closed false}
   [:parent/session-id uuid?]
   [:child/session-id uuid?]
   [:result/cas-ref CasRefSchema]])

(def PayloadSubagentCancelSchema
  "A subagent-cancel payload: the target session id and a cancel reason
  drawn from #{:user-request :parent-cancel :timeout}. The map is open
  to further keys. GC-22: EDN-safe malli checks only."
  [:map {:closed false}
   [:target/session-id uuid?]
   [:reason [:enum :user-request :parent-cancel :timeout]]])

;; --- the full intent schema -------------------------------------------------

(defn- intent-map-schema
  "Build the full closed intent map schema for one v0 type with its
  payload schema. The top level is closed: no base field may be
  missing, renamed, or extended."
  [type payload-schema]
  [:map {:closed true}
   [:intent/id uuid?]
   [:intent/type [:= type]]
   [:session/id uuid?]
   [:phenotype/id PhenotypeIdSchema]
   [:node/id keyword?]
   [:cause/event-id int?]
   [:payload payload-schema]
   [:budget BudgetSchema]
   [:metadata [:map {:closed false}]]])

(def ModelCallIntentSchema
  (intent-map-schema :intent/model-call PayloadModelCallSchema))

(def ToolCallIntentSchema
  (intent-map-schema :intent/tool-call PayloadToolCallSchema))

(def MemoryReadIntentSchema
  (intent-map-schema :intent/memory-read PayloadMemoryReadSchema))

(def MemoryWriteIntentSchema
  (intent-map-schema :intent/memory-write PayloadMemoryWriteSchema))

(def FinishIntentSchema
  (intent-map-schema :intent/finish PayloadFinishSchema))

(def FailIntentSchema
  (intent-map-schema :intent/fail PayloadFailSchema))

(def SubagentSpawnIntentSchema
  (intent-map-schema :intent/subagent-spawn PayloadSubagentSpawnSchema))

(def SubagentResultIntentSchema
  (intent-map-schema :intent/subagent-result PayloadSubagentResultSchema))

(def SubagentCancelIntentSchema
  (intent-map-schema :intent/subagent-cancel PayloadSubagentCancelSchema))

(def IntentSchema
  "The v0 Intent ABI: a :multi schema dispatching on :intent/type, so an
  unknown intent type is rejected and each type's payload is validated
  against its own contract. The base shape is normative; nothing here
  grants, leases, or authorizes anything — an intent is a request, and
  only the kernel-owned capability broker may turn it into an effect."
  [:multi {:dispatch :intent/type}
   [:intent/model-call ModelCallIntentSchema]
   [:intent/tool-call ToolCallIntentSchema]
   [:intent/memory-read MemoryReadIntentSchema]
   [:intent/memory-write MemoryWriteIntentSchema]
   [:intent/finish FinishIntentSchema]
   [:intent/fail FailIntentSchema]
   [:intent/subagent-spawn SubagentSpawnIntentSchema]
   [:intent/subagent-result SubagentResultIntentSchema]
   [:intent/subagent-cancel SubagentCancelIntentSchema]])

;; --- validation entry point ------------------------------------------------

(defn validate-intent
  "Validate x as a v0 Intent.

  First the EDN-safe boundary gate (Global Constraint 22): x must be
  plain, fully realized EDN data — raw Java objects, lazy sequences,
  records, and functions are rejected with :intent/not-edn-safe before
  any schema checking. Then x is validated against IntentSchema.

  Returns x unchanged when it is a structurally valid intent; validation
  never coerces or rewrites values. Otherwise throws an ExceptionInfo
  with :error/type :intent/schema-invalid whose ex-data carries the
  sanitized input under :value and a fully serializable Malli
  explanation under :explanation."
  [x]
  (when-not (boundary/edn-safe? x)
    (throw (err/error :intent/not-edn-safe
                      "intent must be plain EDN-safe data (Global Constraint 22)"
                      {:value (err/sanitize x)})))
  (if (m/validate IntentSchema x)
    x
    (throw (err/error :intent/schema-invalid
                      "intent failed schema validation"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize (m/explain IntentSchema x))}))))
