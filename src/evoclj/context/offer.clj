(ns evoclj.context.offer
  "ContextOffer — catalog's activatable context.

  A Catalog contains Offers. Each Offer is the activatable description
  of a context (e.g., a skill) at a specific revision. The Offer itself
  is immutable and points to an artifact via :offer/revision-id.

  Offer shape:
    {:offer/logical-id [:skill \"debugging\"]  ; logical identifier
     :offer/revision-id \"sha256:…\"           ; content identity, CAS key
     :offer/bundle-id \"bundle:…\"             ; bundle that publishes this offer
     :offer/name \"debugging\"                  ; human name
     :offer/description \"…\"                   ; short description}

  CatalogProjection is a map logical-id -> Offer (current view) or a
  function logical-id -> Offer. The materializer uses bindings for
  content, not the catalog's current revision, so Offers are only for
  discovery, not for materialization."
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(defn offer?
  "True when x is a valid ContextOffer map."
  [x]
  (and (map? x)
       (vector? (:offer/logical-id x))
       (string? (:offer/revision-id x))
       (types/artifact-id? (:offer/revision-id x))
       (string? (:offer/bundle-id x))
       (seq (:offer/bundle-id x))))

(defn make-offer
  "Create a ContextOffer. Requires :logical-id vector, :revision-id sha256 string,
  :bundle-id string. Optional :name and :description strings."
  [{:keys [logical-id revision-id bundle-id name description]}]
  (when-not (and (vector? logical-id) (seq logical-id))
    (throw (err/error :context/offer-invalid "logical-id must be non-empty vector" {:logical-id logical-id})))
  (when-not (types/artifact-id? revision-id)
    (throw (err/error :context/offer-invalid "revision-id must be sha256:<64 hex>" {:revision-id revision-id})))
  (when-not (and (string? bundle-id) (seq bundle-id))
    (throw (err/error :context/offer-invalid "bundle-id must be non-empty string" {:bundle-id bundle-id})))
  (cond-> {:offer/logical-id logical-id
           :offer/revision-id revision-id
           :offer/bundle-id bundle-id}
    name (assoc :offer/name name)
    description (assoc :offer/description description)))

(defn catalog-projection
  "Build a CatalogProjection map from a collection of offers.
  Returns map logical-id -> Offer (last wins on duplicate)."
  [offers]
  (into {} (map (fn [o] [(:offer/logical-id o) o]) offers)))

(defn current-offer
  "Lookup current Offer for logical-id in catalog projection.
  Catalog may be a map or a function. Returns Offer or nil."
  [catalog logical-id]
  (cond
    (map? catalog) (get catalog logical-id)
    (fn? catalog) (catalog logical-id)
    :else nil))
