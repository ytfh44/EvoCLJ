(ns evoclj.provider.registry
  "Provider registration and lookup (Task 4.3).

  The registry is the kernel-owned collection of registered providers
  (Global Constraint 19: the authority root is kernel-owned, never
  agent-mutable). Registration is fail-closed: a provider may be
  registered only once per :tool/id, its descriptor must be
  well-formed, and the object must actually satisfy the Provider
  protocol. A malformed registration is rejected with a typed error
  and changes nothing.

  The descriptor contract is normative (Task 4.3):

    {:tool/id :fixture/echo
     :effect :pure
     :input-schema [:map [:text :string]]
     :output-schema [:map [:text :string]]
     :required-action :invoke
     :retry {:safe? true}}

  :retry is OPTIONAL: automatic retries are allowed only when a
  provider declares :retry {:safe? true} (Task 4.5), so a descriptor
  without it is simply never auto-retried. :input-schema and
  :output-schema are validated as Malli schema VALUES at registration
  time so a malformed contract fails before any request is served.

  register! derives the stored descriptor from (describe provider):
  the provider is the single authority on its own contract, so the
  registry never has to reconcile two possibly-diverging copies.
  lookup returns the entry {:descriptor ... :provider ...} for a tool
  id, or nil when no such tool is registered — the broker (Task
  4.4/4.5) decides what an unknown or visible-but-ungranted tool
  means for authorization (Global Constraint 9: merely registering a
  tool never authorizes it)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [malli.core :as m]))

;; --- the normative descriptor contract -------------------------------------

(def ToolDescriptorSchema
  "The v0 tool descriptor contract (normative, Task 4.3): a closed
  map of the five required fields plus optional :retry and :version
  blocks. The top level is closed — no field may be missing, renamed,
  or extended beyond these, except for optional :mcp/* extension
  fields consumed by the MCP provider bridge. :input-schema and
  :output-schema are only required to be present here; they are
  separately validated as Malli schema VALUES by validate-descriptor."
  [:map {:closed true}
   [:tool/id keyword?]
   [:effect keyword?]
   [:input-schema any?]
   [:output-schema any?]
   [:required-action keyword?]
   [:retry {:optional true} [:map {:closed true} [:safe? boolean?]]]
   [:version {:optional true} number?]
   [:mcp/connection-id {:optional true} keyword?]
   [:mcp/server-id {:optional true} string?]
   [:mcp/last-refreshed {:optional true} any?]
   [:mcp/generation {:optional true} int?]
   [:mcp/captured-at {:optional true} any?]])

(defn- ensure-schema-value!
  "Throw :provider/descriptor-invalid when s is not a valid Malli
  schema value (e.g. 42, [:map [:text]], or an unregistered keyword),
  carrying the offending field name."
  [label s]
  (try
    (m/schema s)
    (catch clojure.lang.ExceptionInfo _
      (throw (err/error :provider/descriptor-invalid
                        (str label " is not a valid Malli schema value")
                        {:reason :invalid-schema :field label
                         :value (err/sanitize s)})))))

(defn validate-descriptor
  "Validate x as a v0 tool descriptor (normative shape, Task 4.3).

  Returns x unchanged when it is well-formed; validation never
  coerces or rewrites values. Otherwise throws
  :provider/descriptor-invalid carrying a fully serializable Malli
  explanation (safe for pr-str / clojure.edn read-string
  round-tripping)."
  [x]
  (when-not (m/validate ToolDescriptorSchema x)
    (throw (err/error :provider/descriptor-invalid
                      "provider descriptor failed schema validation"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize (m/explain ToolDescriptorSchema x))})))
  (ensure-schema-value! :input-schema (:input-schema x))
  (ensure-schema-value! :output-schema (:output-schema x))
  x)

;; --- the registry ----------------------------------------------------------

(defn create-registry
  "Create a fresh, empty provider registry. The registry is a
  kernel-owned mutable component (Global Constraint 19); the
  descriptors it stores are plain validated data (Global Constraint
  22), while the provider instances are host objects that never cross
  the boundary."
  []
  (atom {}))

(defn register!
  "Register `provider` under the :tool/id its describe returns.

  Derives the stored descriptor from (describe provider), validates it
  against the normative descriptor contract, and rejects the
  registration with a typed error when:

  - the object does not satisfy Provider     -> :provider/not-a-provider
  - the descriptor is malformed              -> :provider/descriptor-invalid
  - the :tool/id is already registered       -> :provider/duplicate-tool-id

  Returns the registered :tool/id. A failed registration changes
  nothing."
  [registry provider]
  (when-not (satisfies? proto/Provider provider)
    (throw (err/error :provider/not-a-provider
                      "registered object must satisfy the Provider protocol"
                      {:value (err/sanitize provider)})))
  (let [descriptor (validate-descriptor (proto/describe provider))
        tool-id (:tool/id descriptor)]
    (swap! registry
           (fn [m]
             (when (contains? m tool-id)
               (throw (err/error :provider/duplicate-tool-id
                                 (str "tool " tool-id " is already registered")
                                 {:tool/id tool-id})))
             (assoc m tool-id {:descriptor descriptor :provider provider})))
    tool-id))

(defn lookup
  "Return the registry entry for `tool-id`: {:descriptor ... :provider
  ...}, or nil when no such tool is registered. Pure read; the broker
  (Task 4.4/4.5) decides what an unregistered or
  visible-but-ungranted tool means for authorization."
  [registry tool-id]
  (get @registry tool-id))
