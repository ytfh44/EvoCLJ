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
  tool never authorizes it).

  S14 (pin→provider resolution enforced): a tool catalog pins a set of
  tool ids that a consumer will advertise and execute. `resolve-tool-catalog`
  resolves EVERY pinned id to its registered provider entry (a resolved
  reference — never a dangling id). A pinned id that is ABSENT (never
  registered) or REMOVED (registered then unregistered) fails closed with a
  typed :provider/catalog-unresolved-tool. The resolution is deterministic
  (sorted ids, sorted result), typed, and fail-closed — a catalog consumer
  either gets a fully-resolved reference map or a typed error, never a
  partial map with a dangling id left in."
  (:require [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [evoclj.tool.specs :as tool.specs]
            [malli.core :as m]))

;; --- forward declarations --------------------------------------------------

(declare lookup)

;; --- the normative descriptor contract (single source of truth: evoclj.tool.specs) ---

(def ToolDescriptorSchema
  "Deprecated alias - use evoclj.tool.specs/ToolDescriptorSchema. Kept for
  backward compatibility; this var forwards to the single source of truth
  (D1, INV-05) and must not be redefined."
  tool.specs/ToolDescriptorSchema)

(defn validate-descriptor
  "Validate x as a v0 tool descriptor (normative shape, component).

  Delegates to evoclj.tool.specs/validate-descriptor (single
  implementation, INV-05). Returns x unchanged when well-formed;
  otherwise throws :provider/descriptor-invalid with a sanitized
  explanation. Kept as the public entry point for callers that
  require evoclj.provider.registry/validate-descriptor."
  [x]
  (tool.specs/validate-descriptor x))

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

;; --- S14: tool catalog pin→provider resolution -----------------------------

(defn- tool-id-of-reference
  "The tool id a single catalog reference pins. A keyword reference is a
  bare tool id; a map reference carries :tool (the wire form — e.g. the
  llm-mutator tool catalog entries {:name ... :tool :evolution/evidence})
  or :tool/id (the assembler form). Anything that does not pin a keyword
  tool id is :provider/catalog-invalid — a malformed catalog reference is
  NEVER silently skipped (partial resolution of a dangling catalog is
  forbidden)."
  [ref]
  (let [id (cond
             (keyword? ref) ref
             (and (map? ref) (contains? ref :tool)) (:tool ref)
             (and (map? ref) (contains? ref :tool/id)) (:tool/id ref)
             :else (throw (err/error :provider/catalog-invalid
                                     "tool catalog entry must be a keyword tool-id or a map carrying :tool / :tool/id"
                                     {:value (err/sanitize ref)})))]
    (when-not (keyword? id)
      (throw (err/error :provider/catalog-invalid
                        "a catalog tool reference must resolve to a keyword tool-id"
                        {:value (err/sanitize ref) :tool/id id})))
    id))

(defn catalog-tool-ids
  "The distinct tool ids pinned by a `catalog`, in deterministic sorted
  order (INV-05 — single implementation of the catalog-shape grammar; the
  resolution path calls this and nothing else re-derives it).

  `catalog` is a collection of tool references: a bare keyword tool id, or
  a map carrying :tool (wire form) / :tool/id (assembler form). Duplicate
  pins collapse. A malformed catalog (not a collection, or an entry that
  does not pin a keyword tool id) throws :provider/catalog-invalid.

  The returned sorted-set is the deterministic iteration order
  `resolve-tool-catalog` resolves in, so equal catalogs always produce
  equal results."
  [catalog]
  (when-not (coll? catalog)
    (throw (err/error :provider/catalog-invalid
                      "tool catalog must be a collection of tool references"
                      {:value (err/sanitize catalog)})))
  (reduce (fn [acc ref]
            (conj acc (tool-id-of-reference ref)))
          (sorted-set)
          catalog))

(defn resolve-tool-catalog
  "Resolve EVERY tool id pinned in `catalog` against `registry`.

  This is the S14 pin→provider enforcement point: a tool catalog (a
  collection of pinned tool references, see `catalog-tool-ids`) that a
  consumer advertises and executes must not carry a SILENT DANGLING TOOL
  REFERENCE — every pinned id must resolve to a real registered provider.

  Returns a deterministic (sorted) map tool-id -> registry entry
  {:descriptor ... :provider ...}:
  - the RESOLVED REFERENCE a consumer gets, never a dangling id;
  - identical (catalog, registry) always yields an identical map.

  Fail-closed: a pinned tool id that is ABSENT (never registered) or
  REMOVED (registered then unregistered) throws a typed
  :provider/catalog-unresolved-tool carrying the :unresolved vector
  ([{:tool/id <id> :status :absent|:removed} ...]) and the :tool/ids. A
  successful result is never partial — it contains only tool ids that were
  present, each mapped to a full registry entry.

  Throws ExceptionInfo with a stable :error/type:
    :provider/catalog-invalid          — a malformed catalog entry.
    :provider/catalog-unresolved-tool  — a pinned tool id without a
                                         resolvable provider (absent/removed)."
  [registry catalog]
  (let [ids (catalog-tool-ids catalog)
        outcomes (mapv (fn [id]
                         (let [[status value] (lookup-or-removed registry id)]
                           [id status value]))
                       ids)
        unresolved (->> outcomes
                        (keep (fn [[id status _]]
                                (when-not (= :present status)
                                  {:tool/id id :status status})))
                        vec)]
    (when (seq unresolved)
      (throw (err/error :provider/catalog-unresolved-tool
                        "tool catalog pins one or more tool ids that do not resolve to a registered provider"
                        {:unresolved unresolved
                         :tool/ids (mapv :tool/id unresolved)})))
    (into (sorted-map)
          (map (fn [[id _ entry]] [id entry]))
          outcomes)))
