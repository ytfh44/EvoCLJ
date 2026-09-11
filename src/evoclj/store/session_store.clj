(ns evoclj.store.session-store
  "Fleet R horizontal — narrow opaque handle for session rows.

  Only this namespace may do jdbc on the sessions table
  (Fleet R: make illegal authority unrepresentable — definition >
  validation). Business namespaces (e.g. evoclj.store.session)
  must receive a SessionStore, not a raw sqlite spec or {:sqlite ...} map.

  The handle is opaque via deftype — it does NOT expose :db or :sqlite
  via keyword access; (:db handle) is nil. No db-of escape is provided.

  S1 Singleton (Fleet S1): sessions are pinned to a generation whose
  CURRENT pointer is the kernel_state singleton (see
  evoclj.store.current-store). This handle does not move CURRENT itself;
  it only enforces FK existence at write time (Fleet P5/F) via
  VerifiedDigest and DB FKs (011).

  P5/F FK existence (Fleet P5/F): genome/phenotype/resolution references
  are existence proofs (VerifiedDigest) at the app boundary and FOREIGN
  KEYs at rest (011). Raw payload_ref strings are not proofs and are
  rejected where a proof is required (existence/ensure-proof).

  Session is immutable pin — no state machine. Lifecycle is Work."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.existence :as existence]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; Opaque handle — deftype so (:db handle) is nil
;; ---------------------------------------------------------------------------

(deftype SessionStore [db])

(defn make-session-store
  "Constructor for the narrow SessionStore handle. `db` is a SQLite
  path string or java.jdbc spec. The handle is opaque — it does not
  expose :db or :sqlite via keyword access."
  [db]
  (when (nil? db)
    (throw (err/error :store/session-invalid
                      "SessionStore requires a non-nil db"
                      {:reason :sqlite-missing})))
  (->SessionStore db))

;; ---------------------------------------------------------------------------
;; Shared helpers (single source — only this ns does jdbc on sessions)
;; ---------------------------------------------------------------------------

(defn- invalid-timestamp
  "Build the typed failure for a timestamp that is not a Date, Instant,
  or ISO-8601 string. The shared coercion + ISO formatting lives in
  evoclj.store.sqlite/canonical-timestamp; only the error type stays
  namespace-local."
  [ts]
  (err/error :store/session-invalid
             "timestamp must be an inst, Instant, or ISO-8601 string"
             {:timestamp ts}))

(defn- row->session
  "Convert a sessions DB row into the public Session contract map."
  [row]
  {:session/id (UUID/fromString (:id row))
   :generation/id (:generation_id row)
   :genome/id (:genome_id row)
   :resolution/id (:resolution_id row)
   :phenotype/id (:phenotype_id row)
   :created-at (Date/from (Instant/parse (:created_at row)))
   :routing (when (some? (:routing_deployment_version row))
              {:deployment-version (:routing_deployment_version row)
               :bucket (:routing_bucket row)})})

(defn- proof->digest
  [x]
  (when x
    (existence/digest-of (existence/ensure-proof x))))

;; ---------------------------------------------------------------------------
;; Narrow operations — the ONLY jdbc on sessions
;; ---------------------------------------------------------------------------

(defn insert-session!
  "Insert a pinned session row via SessionStore. Validates FK existence
  via optional VerifiedDigest proofs when supplied (P5/F): if the caller
  supplies :genome/existence-proof etc they are unwrapped to digests;
  otherwise raw ids are used but FK at rest (011) still enforces existence.
  Returns the inserted row map."
  [^SessionStore store request]
  (when-not (instance? SessionStore store)
    (throw (err/error :store/session-invalid
                      "insert-session! requires a SessionStore"
                      {:reason :not-a-session-store})))
  (let [db (.-db ^SessionStore store)
        sid (or (:session/id request) (UUID/randomUUID))
        genome-proof (:genome/existence-proof request)
        resolution-proof (:resolution/existence-proof request)
        phenotype-proof (:phenotype/existence-proof request)
        genome-digest (proof->digest genome-proof)
        resolution-digest (proof->digest resolution-proof)
        phenotype-digest (proof->digest phenotype-proof)
        ts (sqlite/canonical-timestamp (:created-at request) invalid-timestamp)]
    (sqlite/with-db [conn db]
      (sqlite/set-busy-timeout! conn 10000)
      (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?"
                                         (:generation/id request)]))
        (throw (err/error :store/generation-not-found
                          "cannot pin a session to an unknown generation"
                          {:generation/id (:generation/id request)})))
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id (:generation/id request)
                     :genome_id (or genome-digest (:genome/id request))
                     :resolution_id (or resolution-digest (:resolution/id request))
                     :phenotype_id (or phenotype-digest (:phenotype/id request))
                     :routing_deployment_version (:deployment-version (:routing request))
                     :routing_bucket (:bucket (:routing request))
                     :created_at ts}))
    (str sid)))

(defn find-session
  "Find session by id via SessionStore, or nil."
  [^SessionStore store session-id]
  (when-not (instance? SessionStore store)
    (throw (err/error :store/session-invalid
                      "find-session requires a SessionStore"
                      {:reason :not-a-session-store})))
  (some-> (first (sqlite/query (.-db ^SessionStore store)
                               ["SELECT * FROM sessions WHERE id = ?"
                                (str (types/session-id session-id))]))
          row->session))