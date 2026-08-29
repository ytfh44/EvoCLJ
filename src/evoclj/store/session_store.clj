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

  S2 Canonical states (Fleet S2): state vocabulary, transitions, and DB
  mapping are defined in evoclj.store.session-states (definition >
  validation); this store validates against it and delegates mapping.
  No duplicate literal sets here."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.session-states :as sstates]
            [evoclj.store.existence :as existence]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
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

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (err/error :store/session-invalid
                                       "timestamp must be an inst, Instant, or ISO-8601 string"
                                       {:timestamp ts})))]
    (.format timestamp-fmt inst)))

(defn- set-busy-timeout!
  [db ms]
  (let [^java.sql.Connection conn (:connection db)]
    (with-open [stmt (.createStatement conn)]
      (.execute stmt (str "PRAGMA busy_timeout = " ms)))))

(def ^:private db-state->state sstates/db-state->kw)
(def ^:private state->db-state sstates/kw->db-state)

(defn- row->session
  "Convert a sessions DB row into the public Session contract map."
  [row]
  {:session/id (UUID/fromString (:id row))
   :generation/id (:generation_id row)
   :genome/id (:genome_id row)
   :resolution/id (:resolution_id row)
   :phenotype/id (:phenotype_id row)
   :state (keyword (:state row))
   :created-at (Date/from (Instant/parse (:created_at row)))
   :routing (when (some? (:routing_deployment_version row))
              {:deployment-version (:routing_deployment_version row)
               :bucket (:routing_bucket row)})})

(defn- proof->digest
  [x]
  (existence/digest-of (existence/ensure-proof x)))

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
        sid (UUID/randomUUID)
        ts (canonical-timestamp (:created-at request))
        routing (:routing request)
        ;; P5/F: if proofs supplied, unwrap and validate; otherwise use raw
        genome-id (if-let [p (:genome/existence-proof request)] (proof->digest p) (:genome/id request))
        resolution-id (if-let [p (:resolution/existence-proof request)] (proof->digest p) (:resolution/id request))
        phenotype-id (if-let [p (:phenotype/existence-proof request)] (proof->digest p) (:phenotype/id request))]
    (sqlite/with-db [conn db]
      (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" (:generation/id request)]))
        (throw (err/error :store/generation-not-found
                          "cannot pin a session to an unknown generation"
                          {:generation/id (:generation/id request)})))
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id (:generation/id request)
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :phenotype_id phenotype-id
                     :state (name :created)
                     :routing_deployment_version (:deployment-version routing)
                     :routing_bucket (:bucket routing)
                     :created_at ts}))
    sid))

(defn find-session
  "Find session by id via SessionStore, or nil."
  [^SessionStore store session-id]
  (when-not (instance? SessionStore store)
    (throw (err/error :store/session-invalid
                      "find-session requires a SessionStore"
                      {:reason :not-a-session-store})))
  (some-> (first (sqlite/query (.-db ^SessionStore store) ["SELECT * FROM sessions WHERE id = ?" (str (types/session-id session-id))]))
          row->session))

(defn transition-session!
  "CAS state transition via SessionStore. Returns updated session."
  [^SessionStore store session-id expected-state new-state]
  (when-not (instance? SessionStore store)
    (throw (err/error :store/session-invalid
                      "transition-session! requires a SessionStore"
                      {:reason :not-a-session-store})))
  (when-not (sstates/session-state? new-state)
    (throw (err/error :session/invalid-transition
                      "target state not in closed session vocabulary"
                      {:expected-state expected-state :new-state new-state})))
  (when-not (sstates/valid-transition? expected-state new-state)
    (throw (err/error :session/invalid-transition
                      "not an edge of the session state machine"
                      {:session/id (types/session-id session-id)
                       :expected-state expected-state
                       :new-state new-state})))
  (let [sid (types/session-id session-id)
        key (str sid)
        ts (canonical-timestamp nil)
        db (.-db ^SessionStore store)]
    (sqlite/with-db [conn db]
      (set-busy-timeout! conn 10000)
      (let [cnt (first (jdbc/execute! conn
                                        ["UPDATE sessions SET state = ?, updated_at = ? WHERE id = ? AND state = ?"
                                         (name new-state) ts key (name expected-state)]))]
        (when-not (= 1 cnt)
          (let [row (first (jdbc/query conn ["SELECT state FROM sessions WHERE id = ?" key]))]
            (if row
              (throw (err/error :session/invalid-transition
                                "session is not in the expected state"
                                {:session/id sid
                                 :expected-state expected-state
                                 :new-state new-state
                                 :actual-state (keyword (:state row))}))
              (throw (err/error :store/session-not-found
                                "no session with this id"
                                {:session/id sid})))))))
    (find-session store sid)))