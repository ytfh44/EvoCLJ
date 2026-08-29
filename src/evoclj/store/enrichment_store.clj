(ns evoclj.store.enrichment-store
  "Fleet R horizontal — narrow opaque handle for enrichment rows.

  Only this namespace may do jdbc on enrichments
  (Fleet R: make illegal authority unrepresentable). Business namespaces
  (e.g. evoclj.store.enrichment) must receive an EnrichmentStore, not a
  raw {:sqlite :cas} map.

  The handle is opaque via deftype — it does NOT expose :db or :cas via
  keyword access. This closes the raw-map authority gap analogous to
  CandidateStore."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

(deftype EnrichmentStore [db cas])

(defn make-enrichment-store
  "Constructor for the narrow EnrichmentStore handle. `db` is SQLite path/spec, `cas` is CAS root/config."
  [db cas]
  (when (nil? db)
    (throw (err/error :enrichment/store-invalid
                      "EnrichmentStore requires a non-nil db"
                      {:reason :sqlite-missing})))
  (when (nil? cas)
    (throw (err/error :enrichment/store-invalid
                      "EnrichmentStore requires a non-nil cas"
                      {:reason :cas-missing})))
  (->EnrichmentStore db cas))

(defn db-of [^EnrichmentStore s] (.-db ^EnrichmentStore s))
(defn cas-of [^EnrichmentStore s] (.-cas ^EnrichmentStore s))
;; Note: db-of/cas-of are for the enrichment business namespace only;
;; they are not exported as generic escape hatches (package-private via doc).