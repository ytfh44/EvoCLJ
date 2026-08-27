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
       (seq (:offer/bundle-id x))
       (let [d (:offer/descriptor x)]
         (or (nil? d)
             (and (map? d)
                  (contains? #{:cas-leaf :cas-tree-file} (:type d)))))))

(defn make-offer
  "Create a ContextOffer. Requires :logical-id vector, :revision-id sha256 string,
  :bundle-id string. Optional :name and :description strings.
  Optional :descriptor is a materializer descriptor (WO-S1) that the
  materializer routes on when a binding is created from this offer."
  [{:keys [logical-id revision-id bundle-id name description descriptor]}]
  (when-not (and (vector? logical-id) (seq logical-id))
    (throw (err/error :context/offer-invalid "logical-id must be non-empty vector" {:logical-id logical-id})))
  (when-not (types/artifact-id? revision-id)
    (throw (err/error :context/offer-invalid "revision-id must be sha256:<64 hex>" {:revision-id revision-id})))
  (when-not (and (string? bundle-id) (seq bundle-id))
    (throw (err/error :context/offer-invalid "bundle-id must be non-empty string" {:bundle-id bundle-id})))
  (when (some? descriptor)
    (when-not (and (map? descriptor)
                   (contains? #{:cas-leaf :cas-tree-file} (:type descriptor)))
      (throw (err/error :context/offer-invalid "invalid :descriptor" {:descriptor descriptor}))))
  (cond-> {:offer/logical-id logical-id
           :offer/revision-id revision-id
           :offer/bundle-id bundle-id}
    name (assoc :offer/name name)
    description (assoc :offer/description description)
    (some? descriptor) (assoc :offer/descriptor descriptor)))

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
