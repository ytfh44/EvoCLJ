(ns evoclj.evolution.mutation-schema
  "Malli schemas for the Mutation IR (Task 7.3).

  The Mutation is the evolution subsystem's declarative successor
  language: an immutable, closed-map IR that names its parent Genome,
  the evidence pack and hypothesis it answers, its risk class, a
  NON-EMPTY bounded :ops vector, and the expected effect it claims.
  The normative shape (the plan's Task 7.3 interface):

      {:mutation/id #uuid
       :parent/genome-id \"sha256:...\"
       :hypothesis/id #uuid
       :evidence/id \"sha256:...\"
       :risk :behavioral
       :ops [{:op :set-edn
              :file \"skills/debugging.edn\"
              :path [:workflow :before-edit]
              :expect/hash \"sha256:...\"
              :value [:reproduce :localize]}]
       :expected-effect {:primary-metric :task/success
                         :direction :increase}}

  The initial operation set is exactly thirteen ops:

      :set-edn :delete-edn          ; EDN value navigation
      :insert-text :replace-text
      :delete-text                  ; bounded text edits
      :replace-form :insert-form
      :delete-form                  ; source-preserving Clojure form edits
      :add-node :remove-node
      :add-edge :remove-edge
      :update-node                  ; topology graph edits

  Step 2 precondition (normative): every DESTRUCTIVE/REPLACE op
  (:set-edn :delete-edn :replace-text :delete-text :replace-form
  :delete-form :remove-node :remove-edge :update-node) REQUIRES
  :expect/hash — the expected \"sha256:<64 hex>\" preimage digest of the
  target file — so a stale patch can never silently apply to a
  different parent. Pure-ADD ops (:insert-text :insert-form :add-node
  :add-edge) carry :expect/hash only optionally, and an optional value
  must still be canonical when present.

  Closed maps everywhere: unknown keys at a trust boundary are
  rejected (:mutation/invalid for the envelope, :mutation/op-invalid
  for an op) with a humanized Malli explanation. Validation never
  coerces values — a valid mutation is returned unchanged."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.schema :as gschema]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(defn- schema-error!
  "Throw a typed error carrying a humanized Malli explanation."
  [error-type kind expl]
  (throw (err/error error-type
                    (str kind " does not satisfy the mutation contract")
                    {:errors (me/humanize expl)})))

(def HashSchema
  "A canonical content hash — the \"sha256:<64 hex>\" convention of
  evoclj.genome.types. Used for :parent/genome-id, :evidence/id, and
  every op's :expect/hash preimage digest."
  [:fn types/artifact-id?])

(def ExpectedEffectSchema
  "The effect a mutation claims on its primary metric: the metric plus
  the direction it should move."
  [:map {:closed true}
   [:primary-metric keyword?]
   [:direction [:enum :increase :decrease]]])

(def EdnPathSchema
  "An EDN navigation path: a non-empty vector of keywords, strings, or
  positive integers selecting a nested value."
  [:vector {:min 1} [:or keyword? string? pos-int?]])

(def AnchorSchema
  "A bounded text anchor: an exact source string or a 1-based line
  offset. Replacement/insertion must match an explicitly bounded
  source range, never an unconstrained global string replace."
  [:or string? pos-int?])

(def FormSelectorSchema
  "A selector for a Clojure form in a source file: a keyword, symbol,
  string, or a non-empty vector path of the same."
  [:or keyword? symbol? string?
   [:vector {:min 1} [:or keyword? symbol? string? pos-int?]]])

(def NodeSchema
  "The node payload of an :add-node op. The full node shape is
  validated later by the topology compiler; the op must at least name
  the node."
  [:map [:node/id keyword?]])

(def EdgeSchema
  "The edge payload of :add-edge / :remove-edge: a directed edge
  between two declared node ids."
  [:map [:from keyword?] [:to keyword?]])

(def SetEdnOpSchema
  "Set the EDN value at :path inside :file. Replaces existing content,
  so :expect/hash is REQUIRED (Step 2)."
  [:map {:closed true}
   [:op [:enum :set-edn]]
   [:file string?]
   [:path EdnPathSchema]
   [:expect/hash HashSchema]
   [:value :any]])

(def DeleteEdnOpSchema
  "Delete the EDN value at :path inside :file. Destructive: :expect/hash
  is REQUIRED."
  [:map {:closed true}
   [:op [:enum :delete-edn]]
   [:file string?]
   [:path EdnPathSchema]
   [:expect/hash HashSchema]])

(def InsertTextOpSchema
  "Insert :text before/after :anchor in :file. Pure addition: no
  preimage digest is required, but an optional :expect/hash must be
  canonical."
  [:map {:closed true}
   [:op [:enum :insert-text]]
   [:file string?]
   [:position [:enum :before :after]]
   [:anchor AnchorSchema]
   [:text string?]
   [:expect/hash {:optional true} HashSchema]])

(def ReplaceTextOpSchema
  "Replace the text bounded by :anchor with :text in :file. Replace:
  :expect/hash is REQUIRED (a bounded source range/hash, never a
  global string replace)."
  [:map {:closed true}
   [:op [:enum :replace-text]]
   [:file string?]
   [:anchor AnchorSchema]
   [:text string?]
   [:expect/hash HashSchema]])

(def DeleteTextOpSchema
  "Delete the text bounded by :anchor in :file. Destructive:
  :expect/hash is REQUIRED."
  [:map {:closed true}
   [:op [:enum :delete-text]]
   [:file string?]
   [:anchor AnchorSchema]
   [:expect/hash HashSchema]])

(def ReplaceFormOpSchema
  "Replace the form selected by :selector with :form in a Clojure
  source file. Replace: :expect/hash is REQUIRED."
  [:map {:closed true}
   [:op [:enum :replace-form]]
   [:file string?]
   [:selector FormSelectorSchema]
   [:form :any]
   [:expect/hash HashSchema]])

(def InsertFormOpSchema
  "Insert :form before/after the form selected by :selector in a
  Clojure source file. Pure addition: :expect/hash optional."
  [:map {:closed true}
   [:op [:enum :insert-form]]
   [:file string?]
   [:selector FormSelectorSchema]
   [:position [:enum :before :after]]
   [:form :any]
   [:expect/hash {:optional true} HashSchema]])

(def DeleteFormOpSchema
  "Delete the form selected by :selector in a Clojure source file.
  Destructive: :expect/hash is REQUIRED."
  [:map {:closed true}
   [:op [:enum :delete-form]]
   [:file string?]
   [:selector FormSelectorSchema]
   [:expect/hash HashSchema]])

(def AddNodeOpSchema
  "Add :node to a topology module. Pure addition: :expect/hash
  optional."
  [:map {:closed true}
   [:op [:enum :add-node]]
   [:file string?]
   [:node NodeSchema]
   [:expect/hash {:optional true} HashSchema]])

(def RemoveNodeOpSchema
  "Remove the node :node/id from a topology module. Destructive:
  :expect/hash is REQUIRED."
  [:map {:closed true}
   [:op [:enum :remove-node]]
   [:file string?]
   [:node/id keyword?]
   [:expect/hash HashSchema]])

(def AddEdgeOpSchema
  "Add :edge to a topology module. Pure addition: :expect/hash
  optional."
  [:map {:closed true}
   [:op [:enum :add-edge]]
   [:file string?]
   [:edge EdgeSchema]
   [:expect/hash {:optional true} HashSchema]])

(def RemoveEdgeOpSchema
  "Remove :edge from a topology module. Destructive: :expect/hash is
  REQUIRED."
  [:map {:closed true}
   [:op [:enum :remove-edge]]
   [:file string?]
   [:edge EdgeSchema]
   [:expect/hash HashSchema]])

(def UpdateNodeOpSchema
  "Update :update/keys of :node/id with :value in a topology module.
  Replaces existing content: :expect/hash is REQUIRED."
  [:map {:closed true}
   [:op [:enum :update-node]]
   [:file string?]
   [:node/id keyword?]
   [:update/keys [:vector {:min 1} keyword?]]
   [:value map?]
   [:expect/hash HashSchema]])

(def OpSchema
  "The polymorphic op contract: dispatches on :op over the thirteen
  variants. An unknown or missing :op has no entry and is rejected."
  [:multi {:dispatch :op}
   [:set-edn SetEdnOpSchema]
   [:delete-edn DeleteEdnOpSchema]
   [:insert-text InsertTextOpSchema]
   [:replace-text ReplaceTextOpSchema]
   [:delete-text DeleteTextOpSchema]
   [:replace-form ReplaceFormOpSchema]
   [:insert-form InsertFormOpSchema]
   [:delete-form DeleteFormOpSchema]
   [:add-node AddNodeOpSchema]
   [:remove-node RemoveNodeOpSchema]
   [:add-edge AddEdgeOpSchema]
   [:remove-edge RemoveEdgeOpSchema]
   [:update-node UpdateNodeOpSchema]])

(def MutationSchema
  "The closed Mutation envelope (Global Constraints 4, 5, 6): parent
  Genome id, evidence and hypothesis provenance, a bounded risk class
  from the normative risk enum, a NON-EMPTY bounded :ops vector, and
  the expected effect. :risk follows the plan's R0-R4 classes as
  declared by evoclj.genome.schema/RiskClassSchema."
  [:map {:closed true}
   [:mutation/id uuid?]
   [:parent/genome-id [:fn types/genome-id?]]
   [:hypothesis/id uuid?]
   [:evidence/id [:fn types/artifact-id?]]
   [:risk gschema/RiskClassSchema]
   [:ops [:vector {:min 1} OpSchema]]
   [:expected-effect ExpectedEffectSchema]])

(defn validate-op
  "Validate a single mutation op. Returns the op unchanged, or throws
  :mutation/op-invalid."
  [op]
  (if-let [expl (m/explain OpSchema op)]
    (schema-error! :mutation/op-invalid "mutation op" expl)
    op))

(defn validate-mutation
  "Validate the Mutation envelope and every op's shape (including the
  Step 2 :expect/hash requirement). Returns the mutation unchanged, or
  throws :mutation/invalid.

  Path and policy preconditions (Step 3) live in
  evoclj.evolution.mutation/validate-mutation."
  [mutation]
  (if-let [expl (m/explain MutationSchema mutation)]
    (schema-error! :mutation/invalid "mutation" expl)
    mutation))
