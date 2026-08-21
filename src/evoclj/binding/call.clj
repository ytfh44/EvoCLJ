(ns evoclj.binding.call
  "Generic CallBinding - immutable, one per dispatch effect.

  Wraps: immutable descriptor, provider handle, revision-id,
  revision/seq, logical tool-id, source provenance, binding/id.

  Guarantees D_normalize = D_authorize = D_execute = D_validate by
  freezing the descriptor snapshot before normalize. The binding is the
  single source of truth for the dispatch pipeline.

  Freshness policy:
    :required    - stale binding fails closed as :provider/freshness-required
    :best-effort - stale allowed but binding :binding/stale? true and audit marks stale
    :pinned      - never treat as stale, revision pinned.

  Cross-persistence/audit boundary only writes pure data:
    {:binding/id ... :tool/id ... :revision/id ... :revision/seq ...}
  No live objects cross the boundary."
  (:require [evoclj.provider.protocol :as proto]
            [malli.core :as m]))

(def freshness-values #{:required :best-effort :pinned})
(def FreshnessSchema [:enum :required :best-effort :pinned])

(def CallBindingSchema
  [:map
   [:binding/id uuid?]
   [:tool/id keyword?]
   [:revision/id {:optional true} [:maybe string?]]
   [:revision/seq int?]
   [:source/id {:optional true} any?]
   [:binding/descriptor map?]
   [:binding/provider {:optional true} any?]
   [:binding/freshness FreshnessSchema]
   [:binding/stale? boolean?]
   [:binding/captured-at int?]])

(defn valid-freshness? [v] (contains? freshness-values v))

(defn- extract-descriptor-provider
  "Extract [descriptor provider] from various entry shapes:
  - Provider instance -> [(describe provider) provider]
  - {:descriptor ... :provider ...} registry entry
  - {:binding/descriptor ... :binding/provider ...} binding-like
  - descriptor map with :tool/id -> [descriptor nil]
  - nil -> throws"
  [entry]
  (cond
    (nil? entry)
    (throw (ex-info "cannot extract descriptor from nil entry" {}))

    (satisfies? proto/Provider entry)
    [(proto/describe entry) entry]

    (and (map? entry) (contains? entry :binding/descriptor))
    [(:binding/descriptor entry) (:binding/provider entry)]

    (and (map? entry) (contains? entry :descriptor) (contains? entry :provider))
    [(:descriptor entry) (:provider entry)]

    (and (map? entry) (contains? entry :tool/id))
    [entry nil]

    (and (map? entry) (:descriptor entry))
    [(:descriptor entry) (:provider entry)]

    :else
    (throw (ex-info "cannot extract descriptor from entry" {:entry entry}))))

(defn generation
  "Extract generation from descriptor, binding or contract. Prefers
  :revision/seq, then :mcp/generation, then :contract/generation. Defaults to 0.
  Kept for MCP compatibility; generic code should use :revision/seq."
  [x]
  (or (:revision/seq x)
      (:mcp/generation x)
      (:contract/generation x)
      (:binding/revision-seq x)
      0))

(defn captured-at
  "Extract captured-at from descriptor or binding."
  [x]
  (or (:binding/captured-at x)
      (:contract/captured-at x)
      (:mcp/captured-at x)
      (:mcp/last-refreshed x)))

(defn stale?
  "True when descriptor/binding is stale for given freshness.
  Stale is defined as:
   - if freshness is :pinned -> never stale
   - else if descriptor contains :mcp/last-refreshed -> nil means stale (MCP compat)
   - else if descriptor/binding has :revision/id -> nil means stale (generic)
   - else if no revision and no mcp field -> stale (conservative, matches old fixture behavior)
  An explicit :stale? in opts overrides this computation when capturing."
  ([x]
   (stale? x :best-effort))
  ([x freshness]
   (if (= freshness :pinned)
     false
     (cond
       (contains? x :mcp/last-refreshed)
       (nil? (:mcp/last-refreshed x))

       (contains? x :revision/id)
       (nil? (:revision/id x))

       (contains? x :binding/stale?)
       (boolean (:binding/stale? x))

       (contains? x :contract/stale?)
       (boolean (:contract/stale? x))

       :else
       ;; no provenance: treat as stale for best-effort (matches old fixture behavior where :mcp/last-refreshed missing => stale)
       true)))
  ([x freshness revision-id]
   (if (= freshness :pinned)
     false
     (if (contains? x :mcp/last-refreshed)
       (nil? (:mcp/last-refreshed x))
       (nil? revision-id)))))

(defn validate-binding
  "Validate binding against CallBindingSchema, throwing on invalid."
  [b]
  (when-not (m/validate CallBindingSchema b)
    (throw (ex-info "invalid CallBinding" {:explanation (m/explain CallBindingSchema b) :binding b})))
  b)

(defn- coerce-revision-seq [v descriptor]
  (cond
    (some? v) (int v)
    (some? (:revision/seq descriptor)) (int (:revision/seq descriptor))
    (some? (:mcp/generation descriptor)) (int (:mcp/generation descriptor))
    :else 0))

(defn capture-tool-binding
  "Create a CallBinding from a ToolSurface current entry (or provider registry entry).

  Entry may be:
   - a Provider instance (ToolEntry, fixture provider, etc.)
   - a registry entry {:descriptor ... :provider ...}
   - a descriptor map {:tool/id ...}
   - a binding-like map

  Opts map may contain:
   :freshness    - :required | :best-effort | :pinned (default :best-effort)
   :revision/id  - content identity string (sha256:...) or nil
   :revision/seq - monotonic seq int (or :mcp/generation for compat)
   :source/id    - provenance identifier
   :binding/id   - explicit uuid (default random-uuid)
   :captured-at  - millis (default System/currentTimeMillis or descriptor's captured-at)
   :stale?       - explicit stale override
   :provider     - explicit provider handle override
   :descriptor   - explicit descriptor override (for tests)

  Returns an immutable CallBinding map. Descriptor is snapshotted as-is.
  Provider handle is stored live but never included in persisted data."
  ([entry]
   (capture-tool-binding entry {}))
  ([entry opts]
   (capture-tool-binding entry opts nil))
  ([entry opts _extra]
   ;; _extra is ignored, kept for contract/capture arity compat where normalized/decision were separate args
   (let [freshness (or (:freshness opts) :best-effort)
         _ (when-not (valid-freshness? freshness)
             (throw (ex-info "invalid freshness" {:freshness freshness :allowed freshness-values})))
         ;; allow opts to override descriptor/provider directly
         descriptor-override (:descriptor opts)
         provider-override (:provider opts)
         [base-descriptor base-provider] (if descriptor-override
                                           [descriptor-override (or provider-override (:provider opts))]
                                           (try
                                             (extract-descriptor-provider entry)
                                             (catch Exception e
                                               (throw (ex-info "capture-tool-binding: cannot extract descriptor" {:entry entry :cause e})))))
         descriptor (or descriptor-override base-descriptor)
         provider (or provider-override base-provider (:provider opts))
         tool-id (or (:tool/id opts) (:tool/id descriptor) (:binding/tool-id opts))
         _ (when-not (keyword? tool-id)
             (throw (ex-info "capture-tool-binding requires :tool/id keyword" {:tool-id tool-id :descriptor descriptor})))
         revision-id (or (:revision/id opts) (:revision/id descriptor) (:mcp/revision-id descriptor) nil)
         ;; revision-seq: prefer explicit opts, then descriptor's revision/seq, then mcp/generation, then 0
         revision-seq (coerce-revision-seq (or (:revision/seq opts) (:revision/id opts) nil) descriptor)
         ;; Actually revision-seq should be from opts :revision/seq or descriptor's :mcp/generation
         revision-seq (cond
                        (contains? opts :revision/seq) (int (:revision/seq opts))
                        (contains? opts :revision-seq) (int (:revision-seq opts))
                        (some? (:revision/seq descriptor)) (int (:revision/seq descriptor))
                        (some? (:mcp/generation descriptor)) (int (:mcp/generation descriptor))
                        (some? (:contract/generation descriptor)) (int (:contract/generation descriptor))
                        :else 0)
         source-id (or (:source/id opts) (:source/id descriptor) (:mcp/server-id descriptor) (:mcp/connection-id descriptor) nil)
         captured (or (:captured-at opts) (:binding/captured-at opts) (:mcp/captured-at descriptor) (:mcp/last-refreshed descriptor) (System/currentTimeMillis))
         ;; stale computation: explicit :stale? in opts overrides
         stale (if (contains? opts :stale?)
                 (boolean (:stale? opts))
                 (if (contains? descriptor :mcp/last-refreshed)
                   (and (not= freshness :pinned) (nil? (:mcp/last-refreshed descriptor)))
                   ;; generic: if we have a concrete revision-id, not stale; otherwise stale
                   (and (not= freshness :pinned) (nil? revision-id))))
         ;; Also handle case where descriptor came from MCP but has no revision: use mcp field
         ;; For fixture with no mcp/last-refreshed and no revision, stale true (matches old)
         binding-id (or (:binding/id opts) (:id opts) (:contract/id opts) (random-uuid))
         binding {:binding/id binding-id
                  :tool/id tool-id
                  :revision/id revision-id
                  :revision/seq (int revision-seq)
                  :source/id source-id
                  :binding/descriptor descriptor
                  :binding/provider provider
                  :binding/freshness freshness
                  :binding/stale? (boolean stale)
                  :binding/captured-at (long captured)
                  ;; aliases for MCP/contract compat - keeps existing tests green while generic code uses :binding/* and :revision/*
                  :contract/id binding-id
                  :contract/generation (int revision-seq)
                  :contract/descriptor descriptor
                  :contract/freshness freshness
                  :contract/stale? (boolean stale)
                  :contract/captured-at (long captured)
                  :mcp/generation (int revision-seq)
                  :mcp/stale? (boolean stale)
                  :mcp/freshness freshness}]
     (validate-binding (select-keys binding [:binding/id :tool/id :revision/id :revision/seq :source/id :binding/descriptor :binding/provider :binding/freshness :binding/stale? :binding/captured-at]))
     binding)))

(defn capture
  "Alias for capture-tool-binding for contract compatibility.
  Supports arities:
   (capture descriptor freshness)
   (capture descriptor normalized decision freshness)
   (capture descriptor normalized decision freshness opts)"
  ([descriptor freshness]
   (capture descriptor nil nil freshness {}))
  ([descriptor normalized decision freshness]
   (capture descriptor normalized decision freshness {}))
  ([descriptor normalized decision freshness opts]
   (let [base (capture-tool-binding descriptor (merge {:freshness freshness} opts))]
     (cond-> base
       (some? normalized) (assoc :contract/normalized normalized :binding/normalized normalized)
       (some? decision) (assoc :contract/decision decision :binding/decision decision)))))

(defn freeze
  "Alias for capture."
  [& args]
  (apply capture args))

(defn binding->audit
  "Project binding generation/staleness into an audit map fragment.
  Includes both generic and MCP-compatible keys for backward compatibility."
  [binding]
  {:binding/id (:binding/id binding)
   :tool/id (:tool/id binding)
   :revision/id (:revision/id binding)
   :revision/seq (:revision/seq binding)
   :binding/stale? (:binding/stale? binding)
   :binding/freshness (:binding/freshness binding)
   ;; MCP/contract compat
   :mcp/generation (:revision/seq binding)
   :mcp/stale? (:binding/stale? binding)
   :mcp/freshness (:binding/freshness binding)
   :contract/id (:binding/id binding)
   :contract/generation (:revision/seq binding)
   :contract/stale? (:binding/stale? binding)})

(defn contract->audit
  "Alias for binding->audit, kept for contract delegation."
  [binding]
  (binding->audit binding))

(defn binding->persisted
  "Pure data for cross-persistence/audit boundary. No live objects."
  [binding]
  {:binding/id (:binding/id binding)
   :tool/id (:tool/id binding)
   :revision/id (:revision/id binding)
   :revision/seq (:revision/seq binding)})

(defn binding->pure-data
  "Alias for binding->persisted."
  [binding]
  (binding->persisted binding))

(defn tool-surface->binding
  "Convenience: capture a binding from a ToolSurface and tool-id.
  Looks up entry in (:entries surface) and delegates to capture-tool-binding."
  ([tool-surface tool-id]
   (tool-surface->binding tool-surface tool-id {}))
  ([tool-surface tool-id opts]
   (let [entries (:entries tool-surface)
         entry (get entries tool-id)]
     (when-not entry
       (throw (ex-info "tool not found in ToolSurface" {:tool/id tool-id :surface/id (:surface/id tool-surface)})))
     (let [rev-id (or (:revision/id opts) (:revision/id tool-surface))
           rev-seq (or (:revision/seq opts) (:revision/seq tool-surface) 0)
           source-id (or (:source/id opts) (:surface/id tool-surface))]
       (capture-tool-binding entry (merge {:freshness (or (:freshness opts) :best-effort)
                                           :revision/id rev-id
                                           :revision/seq rev-seq
                                           :source/id source-id}
                                          opts))))))

(defn stale-binding?
  "True when binding is stale."
  [binding]
  (boolean (:binding/stale? binding)))

(defn mcp-tool-error?
  "True when provider value indicates an MCP tool error (:mcp/tool-status :error).
   Kept here so generic dispatch does not directly mention MCP keys."
  [value]
  (= :error (:mcp/tool-status (:value value))))

(defn tool-error?
  "Generic alias for mcp-tool-error? — lets generic dispatch avoid mentioning MCP."
  [value]
  (mcp-tool-error? value))

(defn attach-audit-to-result
  "Enrich a dispatch result with binding audit and persisted data.
   Keeps MCP/contract compat keys for existing tests while writing only pure
   data to :persisted. Used by generic dispatcher so it does not directly
   mention MCP keys."
  [result binding]
  (let [fragment (binding->audit binding)
        persisted (binding->persisted binding)]
    (-> result
        (assoc :binding/id (:binding/id binding)
               :tool/id (:tool/id binding)
               :revision/id (:revision/id binding)
               :revision/seq (:revision/seq binding)
               :binding/stale? (:binding/stale? binding)
               :contract/id (:binding/id binding)
               :contract/generation (:revision/seq binding)
               :contract/stale? (:binding/stale? binding)
               :mcp/generation (:revision/seq binding)
               :audit (merge (:audit result) fragment)
               :persisted persisted)
        (update :audit merge fragment))))
