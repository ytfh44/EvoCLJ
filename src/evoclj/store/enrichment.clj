(ns evoclj.store.enrichment
  "Foundation F3 — versioned derived-metadata store (the enrichment
  layer).

  Genomes, candidates, and cases are immutable and content-addressed.
  Derived metadata — case weights, curriculum difficulty, generated
  docs, hall-of-fame flags, lab-notebook narratives — must be attached
  WITHOUT mutating the entity. This namespace is that attachment: an
  append-only, versioned 'enrichment' table whose rows reference a DERIVED
  fact on an immutable entity.

  THE ENRICHMENT RECORD (docs 'Foundation F3'):

      {:enrichment/id uuid?
       :entity/kind keyword?
       :entity/id string?            ; content address or stable entity id
       :kind keyword?                ; the derived-metadata class
       :version pos-int?             ; per (entity/kind, entity/id, kind)
       :payload-ref string?          ; \"sha256:<64 hex>\" content address
       :cause [:maybe string?]       ; optional provenance reference
       :created-at inst?}

  A row's :payload-ref is a content address into the filesystem CAS
  (resources/migrations/004-enrichment.sql, Global Constraint 21): the
  derived :payload map body is put into the CAS as pr-str EDN bytes and
  the row stores ONLY the reference — the body is never duplicated in
  SQLite. :cause carries an optional provenance reference
  (e.g. the event id / artifact id / stable id whose computation produced
  this enrichment).

  VERSIONING: :version is allocated inside a single write transaction as
  max(version)+1 per (entity/kind, entity/id, kind), with a busy_timeout
  so a contended write waits for SQLite's write lock (the same pattern
  evoclj.evolution.candidate uses for its materialization). The database
  UNIQUE (entity_kind, entity_id, kind, version) plus the append-only
  triggers in 004-enrichment.sql make each version immutable at the schema
  level: a stray UPDATE/DELETE fails loudly (Database Invariant 10,
  mirrored from the events table).

  `store` is the executor :stores map {:sqlite <db> :cas <CAS root or
  config>}, exactly as in evoclj.evolution.candidate — this namespace
  writes only enrichment ROWS and never opens or closes a connection.
  The :cas value is passed through to evoclj.store.cas, which accepts
  either a bare root path or a config map (e.g. (cas/->cas root)).

  Error contract (Global Constraint 22 — plain serializable data):
  :enrichment/store-invalid (:reason :not-a-map :sqlite-missing
  :cas-missing), :enrichment/invalid (contract violation, Malli
  explanations), :enrichment/payload-missing (:payload-ref when the
  referenced artifact is absent from the CAS)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; --- public contracts ----------------------------------------------------------

(def PutRequestSchema
  "The put-enrichment! input contract (closed). Identifies the immutable
  entity (:entity/kind + :entity/id) and the derived-metadata class
  (:kind), the :payload EDN map to attach, an optional :cause provenance
  string, and an optional :created-at (defaults to now). Unknown keys are
  rejected: trust boundaries use closed maps."
  [:map {:closed true}
   [:entity/kind keyword?]
   [:entity/id string?]
   [:kind keyword?]
   [:payload :map]
   [:cause [:maybe string?]]
   [:created-at {:optional true} [:fn inst?]]])

(def EnrichmentSchema
  "The public Enrichment record contract map returned by
  put-enrichment! and the read queries."
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
  "Throw :enrichment/invalid with a humanized Malli explanation."
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
  "Validate the executor :stores map {:sqlite ... :cas ...} (the shape
  evoclj.evolution.candidate defines). This namespace writes rows AND
  reads/writes the CAS for :payload bodies, so both handles are required."
  [store]
  (when-not (map? store)
    (throw (err/error :enrichment/store-invalid
                      "store must be the executor :stores map {:sqlite ... :cas ...}"
                      {:reason :not-a-map :value (err/sanitize store)})))
  (when-not (contains? store :sqlite)
    (throw (err/error :enrichment/store-invalid
                      "store must carry the :sqlite handle"
                      {:reason :sqlite-missing})))
  (when-not (contains? store :cas)
    (throw (err/error :enrichment/store-invalid
                      "store must carry the :cas handle"
                      {:reason :cas-missing})))
  store)

;; --- timestamps ----------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  "Canonical ISO-8601 UTC string for a :created-at value (a
  java.util.Date, a java.time.Instant, or an ISO-8601 string). nil means
  now."
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
  "Set SQLite's busy_timeout on the open connection carried by `db`
  (the spec-with-connection map sqlite/with-db binds). A contended
  write waits for SQLite's write lock instead of failing with
  SQLITE_BUSY — the same pattern evoclj.evolution.candidate uses for its
  materialization."
  [db ms]
  (let [^java.sql.Connection conn (:connection db)]
    (with-open [stmt (.createStatement conn)]
      (.execute stmt (str "PRAGMA busy_timeout = " ms)))))

(defn- kw->db
  "The full keyword string stored in the entity_kind / kind columns:
  namespace + slash + name (e.g. :entity/kind is stored as
  entity/kind). clojure.core/name would drop the namespace, so the
  full string is built explicitly — the same convention evoclj.store.event
  uses for event_type — and read back with (keyword s)."
  [k]
  (if-let [ns (namespace k)]
    (str ns "/" (name k))
    (name k)))

;; --- row mapping ----------------------------------------------------------------

(defn- row->enrichment
  "Convert an enrichments DB row into the public Enrichment record."
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
  "Attach one derived-metadata enrichment to an immutable entity and
  return the persisted Enrichment record (:version = max+1 per
  (entity/kind, entity/id, kind)).

  The :payload EDN map is written to the CAS as pr-str UTF-8 bytes via
  evoclj.store.cas/put-bytes! (Global Constraint 21: the row stores only
  the content-address :payload-ref, never the body); the resulting
  content hash is validated with cas/exists?. The :version is allocated
  inside a single write transaction (sqlite/with-db + busy_timeout) as
  max(version)+1 across the (entity_kind, entity_id, kind) triple, so
  concurrent writers serialize and each appended version is unique. The
  row is then inserted and read back, validated against EnrichmentSchema.

  Typed errors: :enrichment/store-invalid, :enrichment/invalid
  (closed-map contract violation, non-map payload, Malli explanations)."
  [store request]
  (validate-store! store)
  (validate-request! request)
  (let [db (:sqlite store)
        cas-root (:cas store)
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
  "Every Enrichment record attached to (entity-kind, entity-id) of the
  derived-metadata class `kind`, in ascending :version order (never
  lazy). Read-only."
  [store entity-kind entity-id kind]
  (validate-store! store)
  (->> (sqlite/query (:sqlite store)
                     ["SELECT * FROM enrichments
                       WHERE entity_kind = ? AND entity_id = ? AND kind = ?
                       ORDER BY version ASC"
                      (kw->db entity-kind) entity-id (kw->db kind)])
       (mapv (fn [row] (validate-record! (row->enrichment row))))))

(defn latest-enrichment
  "The highest :version Enrichment record for (entity-kind, entity-id,
  kind), or nil when none is attached. Read-only."
  [store entity-kind entity-id kind]
  (validate-store! store)
  (->> (sqlite/query (:sqlite store)
                     ["SELECT * FROM enrichments
                       WHERE entity_kind = ? AND entity_id = ? AND kind = ?
                       ORDER BY version DESC
                       LIMIT 1"
                      (kw->db entity-kind) entity-id (kw->db kind)])
       first
       some-> row->enrichment
       validate-record!))

(defn payload
  "Read the derived-metadata EDN map back from the CAS via a record's
  :payload-ref (evoclj.store.cas/get-bytes + clojure.edn/read-string).

  Throws :enrichment/payload-missing when the referenced artifact is
  absent from the CAS; :enrichment/store-invalid when `store` is not the
  executor :stores map."
  [store enrichment]
  (validate-store! store)
  (let [payload-ref (:payload-ref enrichment)]
    (when-not (cas/exists? (:cas store) payload-ref)
      (throw (err/error :enrichment/payload-missing
                        "the enrichment payload artifact is absent from the CAS"
                        {:payload-ref payload-ref})))
    (edn/read-string
     (String. (cas/get-bytes (:cas store) payload-ref)
              java.nio.charset.StandardCharsets/UTF_8))))
