(ns evoclj.store.enrichment
  "Foundation F3 — versioned derived-metadata store (the enrichment
  layer).

  Genomes, candidates, and cases are immutable and content-addressed.
  Derived metadata — case weights, curriculum difficulty, generated
  docs, hall-of-fame flags, lab-notebook narratives — must be attached
  WITHOUT mutating the entity. This namespace is that attachment: an
  append-only, versioned 'enrichment' table whose rows reference a DERIVED
  fact on an immutable entity.

  Fleet R horizontal (narrow handle): `store` is an EnrichmentStore
  handle (evoclj.store.enrichment-store/make-enrichment-store), not a
  raw {:sqlite :cas} map. The handle is opaque (deftype) — (:sqlite store)
  is nil. Raw maps are rejected with :enrichment/store-invalid
  (definition > validation). For backward compat a raw {:sqlite :cas} map
  is NOT accepted; callers must migrate to the handle.

  VERSIONING: :version is allocated inside a single write transaction as
  max(version)+1 per (entity/kind, entity/id, kind), with a busy_timeout
  so a contended write waits for SQLite's write lock (the same pattern
  evoclj.evolution.candidate uses for its materialization)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.enrichment-store :as es]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; --- public contracts ----------------------------------------------------------

(def PutRequestSchema
  [:map {:closed true}
   [:entity/kind keyword?]
   [:entity/id string?]
   [:kind keyword?]
   [:payload :map]
   [:cause [:maybe string?]]
   [:created-at {:optional true} [:fn inst?]]])

(def EnrichmentSchema
  [:map {:closed true}
   [:enrichment/id uuid?]
   [:entity/kind keyword?]
   [:entity/id string?]
   [:kind keyword?]
   [:version pos-int?]
   [:payload-ref string?]
   [:cause [:maybe string?]]
   [:created-at [:fn inst?]]])

(defn- schema-error!
  [kind expl]
  (throw (err/error :enrichment/invalid
                    (str kind " does not satisfy the enrichment contract")
                    {:errors (me/humanize expl)})))

(defn- validate-request!
  [request]
  (when-let [expl (m/explain PutRequestSchema request)]
    (schema-error! "put-enrichment! request" expl))
  request)

(defn- validate-record!
  [rec]
  (when-let [expl (m/explain EnrichmentSchema rec)]
    (schema-error! "enrichment" expl))
  rec)

(defn- validate-store!
  "Validate that store is an EnrichmentStore handle. Raw maps are rejected."
  [store]
  (when-not (instance? evoclj.store.enrichment_store.EnrichmentStore store)
    (throw (err/error :enrichment/store-invalid
                      "store must be an EnrichmentStore handle (evoclj.store.enrichment-store/make-enrichment-store)"
                      {:reason :not-an-enrichment-store :value (err/sanitize store)})))
  store)

(defn- db-of [store] (.-db ^evoclj.store.enrichment_store.EnrichmentStore store))
(defn- cas-of [store] (.-cas ^evoclj.store.enrichment_store.EnrichmentStore store))

;; --- timestamps ----------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (err/error :enrichment/invalid
                                       "created-at must be an inst, Instant, or ISO-8601 string"
                                       {:created-at ts})))]
    (.format timestamp-fmt inst)))

(defn- set-busy-timeout!
  [db ms]
  (let [^java.sql.Connection conn (:connection db)]
    (with-open [stmt (.createStatement conn)]
      (.execute stmt (str "PRAGMA busy_timeout = " ms)))))

(defn- kw->db
  [k]
  (if-let [ns (namespace k)]
    (str ns "/" (name k))
    (name k)))

(defn- row->enrichment
  [row]
  {:enrichment/id (UUID/fromString (:id row))
   :entity/kind (keyword (:entity_kind row))
   :entity/id (:entity_id row)
   :kind (keyword (:kind row))
   :version (:version row)
   :payload-ref (:payload_ref row)
   :cause (:cause_ref row)
   :created-at (Date/from (Instant/parse (:created_at row)))})

;; --- the single write path ------------------------------------------------------

(defn put-enrichment!
  [store request]
  (validate-store! store)
  (validate-request! request)
  (let [db (db-of store)
        cas-root (cas-of store)
        ts (canonical-timestamp (:created-at request))
        entity-kind (kw->db (:entity/kind request))
        entity-id (:entity/id request)
        kind (kw->db (:kind request))
        put-result (cas/put-bytes! cas-root
                                   (.getBytes (pr-str (:payload request))
                                              java.nio.charset.StandardCharsets/UTF_8)
                                   {:media-type "application/edn"})
        payload-ref (:artifact/id put-result)]
    (when-not (cas/exists? cas-root payload-ref)
      (throw (err/error :enrichment/invalid
                        "payload write was not persisted by the CAS"
                        {:payload-ref payload-ref})))
    (sqlite/with-db [conn db]
      (set-busy-timeout! conn 10000)
      (let [next-version (-> (jdbc/query conn
                                         ["SELECT COALESCE(MAX(version), 0) + 1 AS version
                                           FROM enrichments
                                           WHERE entity_kind = ? AND entity_id = ? AND kind = ?"
                                          entity-kind entity-id kind])
                             first :version)
            enrichment-id (str (UUID/randomUUID))]
        (jdbc/insert! conn :enrichments
                      {:id enrichment-id
                       :entity_kind entity-kind
                       :entity_id entity-id
                       :kind kind
                       :version next-version
                       :payload_ref payload-ref
                       :cause_ref (:cause request)
                       :created_at ts})
        (validate-record!
         (row->enrichment
          (first (jdbc/query conn
                             ["SELECT * FROM enrichments WHERE id = ?"
                              enrichment-id]))))))))

(defn enrichments
  [store entity-kind entity-id kind]
  (validate-store! store)
  (->> (sqlite/query (db-of store)
                     ["SELECT * FROM enrichments
                       WHERE entity_kind = ? AND entity_id = ? AND kind = ?
                       ORDER BY version ASC"
                      (kw->db entity-kind) entity-id (kw->db kind)])
       (mapv (fn [row] (validate-record! (row->enrichment row))))))

(defn latest-enrichment
  [store entity-kind entity-id kind]
  (validate-store! store)
  (->> (sqlite/query (db-of store)
                     ["SELECT * FROM enrichments
                       WHERE entity_kind = ? AND entity_id = ? AND kind = ?
                       ORDER BY version DESC
                       LIMIT 1"
                      (kw->db entity-kind) entity-id (kw->db kind)])
       first
       some-> row->enrichment
       validate-record!))

(defn payload
  [store enrichment]
  (validate-store! store)
  (let [payload-ref (:payload-ref enrichment)]
    (when-not (cas/exists? (cas-of store) payload-ref)
      (throw (err/error :enrichment/payload-missing
                        "the enrichment payload artifact is absent from the CAS"
                        {:payload-ref payload-ref})))
    (edn/read-string
     (String. (cas/get-bytes (cas-of store) payload-ref)
              java.nio.charset.StandardCharsets/UTF_8))))