(ns evoclj.mcp.contract
  "CallContract — immutable, one per dispatch effect (Step 1).

  Guarantees D_normalize = D_authorize = D_execute = D_validate by
  freezing the descriptor snapshot at generation n before normalize.
  Effect start (normalize) forbids further reset! of the descriptor
  atom; refresh must happen BEFORE call-started.

  Freshness policy:
    :required    — stale descriptor fails closed as :provider/freshness-required
    :best-effort — stale allowed but contract :stale? true and audit marks stale
    :pinned      — never treat as stale, generation pinned."
  (:require [malli.core :as m]))

(def freshness-values #{:required :best-effort :pinned})

(def FreshnessSchema [:enum :required :best-effort :pinned])

(def CallContractSchema
  [:map
   [:contract/id uuid?]
   [:contract/generation int?]
   [:contract/descriptor map?]
   [:contract/normalized {:optional true} any?]
   [:contract/decision {:optional true} any?]
   [:contract/freshness FreshnessSchema]
   [:contract/stale? boolean?]
   [:contract/captured-at {:optional true} int?]])

(defn valid-freshness? [v] (contains? freshness-values v))

(defn generation
  "Extract :mcp/generation from a descriptor (or contract). Defaults to 0."
  [x]
  (or (:mcp/generation x)
      (:contract/generation x)
      0))

(defn captured-at [descriptor]
  (or (:mcp/captured-at descriptor) (:mcp/last-refreshed descriptor)))

(defn stale?
  "True when descriptor is stale: :mcp/last-refreshed is nil.
   :pinned freshness never counts as stale."
  ([descriptor] (nil? (:mcp/last-refreshed descriptor)))
  ([descriptor freshness]
   (and (not= freshness :pinned)
        (nil? (:mcp/last-refreshed descriptor)))))

(defn capture
  "Freeze a CallContract from a descriptor snapshot.
  freshness must be :required/:best-effort/:pinned.
  Options map may contain :stale? and :id."
  ([descriptor freshness]
   (capture descriptor nil nil freshness {}))
  ([descriptor normalized decision freshness]
   (capture descriptor normalized decision freshness {}))
  ([descriptor normalized decision freshness {:keys [stale? id captured-at]}]
   (when-not (valid-freshness? freshness)
     (throw (ex-info "invalid freshness" {:freshness freshness :allowed freshness-values})))
   (let [gen (generation descriptor)
         cid (or id (random-uuid))
         now (or captured-at (System/currentTimeMillis))]
     (cond-> {:contract/id cid
              :contract/generation gen
              :contract/descriptor descriptor
              :contract/freshness freshness
              :contract/stale? (boolean stale?)
              :contract/captured-at now}
       (some? normalized) (assoc :contract/normalized normalized)
       (some? decision)   (assoc :contract/decision decision)))))

(defn freeze
  "Alias for capture — freeze descriptor snapshot into contract."
  [& args]
  (apply capture args))

(defn validate-contract
  "Validate contract against CallContractSchema, throwing on invalid."
  [c]
  (when-not (m/validate CallContractSchema c)
    (throw (ex-info "invalid CallContract" {:explanation (m/explain CallContractSchema c)})))
  c)

(defn contract->audit
  "Project contract generation/staleness into an audit map fragment."
  [contract]
  {:mcp/generation (:contract/generation contract)
   :mcp/stale? (:contract/stale? contract)
   :mcp/freshness (:contract/freshness contract)
   :contract/id (:contract/id contract)})
