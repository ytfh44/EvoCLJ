(ns evoclj.provider.registry
  "Provider registration and lookup (component).

  The registry is the kernel-owned collection of registered providers
  (Global Constraint 19: the authority root is kernel-owned, never
  agent-mutable). Registration is fail-closed: a provider may be
  registered only once per :tool/id, its descriptor must be
  well-formed, and the object must actually satisfy the Provider
  protocol. A malformed registration is rejected with a typed error
  and changes nothing.

  The descriptor contract is normative (component):

    {:tool/id :fixture/echo
     :effect :pure
     :input-schema [:map [:text :string]]
     :output-schema [:map [:text :string]]
     :required-action :invoke
     :retry {:safe? true}}

  :retry is OPTIONAL: automatic retries are allowed only when a
  provider declares :retry {:safe? true} (component), so a descriptor
  without it is simply never auto-retried. :input-schema and
  :output-schema are validated as Malli schema VALUES at registration
  time so a malformed contract fails before any request is served.

  register! derives the stored descriptor from (describe provider):
  the provider is the single authority on its own contract, so the
  registry never has to reconcile two possibly-diverging copies.
  lookup returns the entry {:descriptor ... :provider ...} for a tool
  id, or nil when no such tool is registered — the broker (component/4.5) decides what an unknown or visible-but-ungranted tool
  means for authorization (Global Constraint 9: merely registering a
  tool never authorizes it)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [malli.core :as m]))

;; --- forward declarations --------------------------------------------------

(declare lookup)

;; --- the normative descriptor contract -------------------------------------

(def ToolDescriptorSchema
  "The v0 tool descriptor contract (normative, component): a closed
  map of the five required fields plus optional :retry and :version
  blocks. The top level is closed — no field may be missing, renamed,
  or extended beyond these, except for optional MCP extension fields
  consumed by the MCP provider bridge and generic binding. Lifecycle
  MCP fields (generation, last-refreshed, captured-at) are allowed here
  for validation but generic dispatch no longer depends on them;
  they are treated as MCP-specific provenance in binding."
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
  "Validate x as a v0 tool descriptor (normative shape, component).

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

(def ^:private removed-key
  "Namespaced tombstone key. Stored inside the (deliberately flat)
  registry atom so it can NEVER collide with a registered tool-id (all
  tool-ids are caller-chosen keywords, and this key is namespaced to
  evoclj.provider.registry). Keeping the atom flat preserves the
  long-standing contract that `(get @registry tool-id)` yields an entry
  and that tests may `(swap! reg assoc tool-id entry)` directly."
  ::removed)

(defn create-registry
  "Create a fresh, empty provider registry. The registry is a
  kernel-owned mutable component (Global Constraint 19); the
  descriptors it stores are plain validated data (Global Constraint
  22), while the provider instances are host objects that never cross
  the boundary.

  The atom is a flat map: tool-id -> {:descriptor ... :provider ...},
  plus a single namespaced tombstone set under `removed-key` recording
  the tool-ids that were REGISTERED and then later UNREGISTERED. That
  tombstone lets the dispatcher distinguish a REMOVED tool (was
  registered, then taken away) from a tool that NEVER existed — they
  are different failure classes and must be reported with different
  typed errors (M19: :provider/tool-removed vs :provider/not-found)."
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
             ;; (Re-)registering clears any prior removal tombstone: the tool
             ;; is live again, so it must NOT be misreported as removed (M19).
             (-> m
                 (assoc tool-id {:descriptor descriptor :provider provider})
                 (update removed-key (fnil disj #{}) tool-id))))
    tool-id))

(defn unregister!
  "Remove the provider registered under `tool-id` (if any) and record the
  tool-id in the tombstone set so that a LATER reference to it is reported
  as `:provider/tool-removed` rather than the generic
  `:provider/not-found` (M19 — tool-removal semantics recovery,
  fail-closed and typed).

  - a tool that was registered: removed from the registry, tool-id added to
    the tombstone set.
  - a tool that was NEVER registered: no entry change, but the tool-id is
    still added to the tombstone set (the caller believes it removed it, so
    any future reference must report removal, never a silent nil
    passthrough).
  - a tool that is ALREADY removed: idempotent — the tombstone is unchanged.

  Returns the removed tool-id. Fail-closed: this never throws on a missing
  entry; it always leaves the registry in a consistent state and is safe
  to call concurrently with register!/lookup."
  [registry tool-id]
  (swap! registry
         (fn [m]
           (-> m
               (dissoc tool-id)
               (update removed-key (fnil conj #{}) tool-id))))
  tool-id)

(defn lookup
  "Return the registry entry for `tool-id`: {:descriptor ... :provider
  ...}, or nil when no such tool is registered. Pure read; the broker
  (component/4.5) decides what an unregistered or
  visible-but-ungranted tool means for authorization."
  [registry tool-id]
  (get @registry tool-id))

(defn removed?
  "True when `tool-id` was registered and later unregistered (M19). A tool
  that has never existed returns false here — use lookup for presence."
  [registry tool-id]
  (contains? (get @registry removed-key #{}) tool-id))

(defn lookup-or-removed
  "Discriminate the three reference outcomes for `tool-id` (M19):

    [:present <entry>]   the tool is currently registered
    [:removed tool-id]   the tool was registered then unregistered (tombstone)
    [:absent  tool-id]   the tool was never registered

  Pure read; calls `lookup` internally so the dispatch path can branch on
  removed vs absent without a second deref."
  [registry tool-id]
  (if-let [entry (lookup registry tool-id)]
    [:present entry]
    (if (removed? registry tool-id)
      [:removed tool-id]
      [:absent tool-id])))
