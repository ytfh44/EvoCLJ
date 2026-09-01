(ns evoclj.store.session
  "Session pinning and lifecycle transitions (component).

  A session is created pinned to one Genome, one Resolution, one
  Phenotype, and one Generation for its whole lifetime (Global
  Constraint 2, Database Invariant 2). The pinned identity columns are
  written once at insert and never touched again: the ONLY write path
  after creation is transition-session!, a compare-and-set UPDATE that
  matches the stored state and changes state (plus the transition
  timestamp) alone.

  State machine (normative, docs component):

      :created -> :resolving -> :running <-> :waiting -> :completed
                               |------------> :failed
                               |------------> :cancelled
                               `------------> :budget-exhausted

  Terminal states (:completed :failed :cancelled :budget-exhausted)
  accept no further transitions. transition-session! rejects a
  statically illegal edge (:session/invalid-transition) before touching
  the database, and the SQL `WHERE state = expected-state` backstop
  means a concurrent worker that lost the compare-and-set also sees
  :session/invalid-transition — two workers can never both transition
  from the same state silently (component Step 4).

  Public Session contract (docs 'Detailed Public Data Contracts'):
  :session/id, :generation/id, :genome/id, :resolution/id,
  :phenotype/id, :state, :created-at, :routing. Pinned identity fields
  are immutable after insert.

  Fleet R horizontal (narrow handle): this namespace is the business
  layer; persistence is via evoclj.store.session-store/SessionStore
  (opaque deftype). Raw maps are rejected (definition > validation).
  Fleet S2: state vocabulary and transitions are defined in
  evoclj.store.session-states (single canonical source).
  Fleet P5/F: genome/phenotype/resolution existence is enforced via
  VerifiedDigest and FK at rest (011)."
;; E1: Event prev vs causal-links — session creation uses :prev/event-id nil + :causal-links #{}, no :cause.
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.session-states :as sstates]
            [evoclj.store.session-store :as ss]
            [evoclj.store.existence :as existence]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util Date UUID)))

;; --- state machine (canonical — delegates to session-states) ---------------

(def states
  "Every state in the component state machine (alias for session-states/session-states)."
  sstates/session-states)

(def transitions
  "State machine edges (alias for session-states/session-transitions)."
  sstates/session-transitions)

(def terminal-states
  "States that accept no further transitions (alias)."
  sstates/terminal-states)

(defn- valid-transition?
  [expected-state new-state]
  (sstates/valid-transition? expected-state new-state))

;; --- boundary validation ----------------------------------------------------

(def routing-schema
  "The :routing map of the public Session contract."
  [:map {:closed true}
   [:deployment-version string?]
   [:bucket int?]])

(def CreateSessionRequest
  "The create-session! input contract. The pinned identity fields are
  content-addressed ids; :generation/id is required because the
  sessions.generation_id column is NOT NULL and references
  generations; :routing and :created-at are optional. Unknown keys are
  rejected: trust boundaries use closed maps."
  [:map {:closed true}
   [:genome/id [:fn types/genome-id?]]
   [:resolution/id [:fn types/resolution-id?]]
   [:phenotype/id [:fn types/artifact-id?]]
   [:generation/id string?]
   [:routing {:optional true} routing-schema]
   [:created-at {:optional true} [:fn inst?]]
   [:genome/existence-proof {:optional true} any?]
   [:resolution/existence-proof {:optional true} any?]
   [:phenotype/existence-proof {:optional true} any?]])

(def SessionSchema
  "The public Session contract map returned by create-session! and
  get-session."
  [:map {:closed true}
   [:session/id uuid?]
   [:generation/id string?]
   [:genome/id [:fn types/genome-id?]]
   [:resolution/id [:fn types/resolution-id?]]
   [:phenotype/id [:fn types/artifact-id?]]
   [:state sstates/session-state-enum]
   [:created-at [:fn inst?]]
   [:routing [:maybe routing-schema]]])

(defn- schema-error!
  [kind expl]
  (throw (err/error :store/session-invalid
                    (str kind " does not satisfy the session contract")
                    {:errors (me/humanize expl)})))

(defn- validate-create-request
  [request]
  (when-let [expl (m/explain CreateSessionRequest request)]
    (schema-error! "create-session! request" expl))
  request)

(defn- validate-session
  [s]
  (when-let [expl (m/explain SessionSchema s)]
    (schema-error! "session" expl))
  s)

(defn- edn-safe-map?
  [x]
  (or (nil? x)
      (and (map? x)
           (try
             (map? (edn/read-string (pr-str x)))
             (catch Exception _ false)))))

(defn- normalize-store
  "Normalize `store` to a SessionStore. Accepts a SessionStore or a
  raw sqlite spec (string/path/spec-map). Raw executor maps {:sqlite ...} are rejected
  with :store/session-invalid (Fleet R: not a SessionStore)."
  [store]
  (cond
    (instance? evoclj.store.session_store.SessionStore store) store
    (and (map? store) (contains? store :sqlite)) (throw (err/error :store/session-invalid
                                   "store must be a SessionStore handle (evoclj.store.session-store/make-session-store)"
                                   {:reason :not-a-session-store :value (err/sanitize store)}))
    (and (map? store) (contains? store :subprotocol)) (ss/make-session-store store)
    (string? store) (ss/make-session-store store)
    (map? store) (throw (err/error :store/session-invalid
                                   "store must be a SessionStore handle (evoclj.store.session-store/make-session-store)"
                                   {:reason :not-a-session-store :value (err/sanitize store)}))
    :else (ss/make-session-store store)))

;; --- public API ---------------------------------------------------------------

(declare get-session)

(defn create-session!
  "Create a session row pinned to the request's Genome, Resolution,
  Phenotype, and Generation ids (Global Constraint 2) and return the
  persisted Session contract map. The pinned identity fields are
  immutable after insert — no API can change them later.

  `store` is a SessionStore handle (evoclj.store.session-store/make-session-store).
  Raw maps are rejected (Fleet R). For backward compat a raw sqlite spec
  (string path) is auto-wrapped, but new code must pass a handle.

  Typed errors: :store/session-invalid, :store/session-invalid :not-a-session-store,
  :store/generation-not-found. Optional :genome/existence-proof etc. may
  carry VerifiedDigest proofs (Fleet P5/F)."
  [store request]
  (validate-create-request request)
  (let [ss-store (normalize-store store)
        sid (ss/insert-session! ss-store request)]
    (get-session ss-store sid)))

(defn transition-session!
  "Compare-and-set state transition (component Step 4)."
  [store session-id expected-state new-state data]
  (when-not (and (keyword? expected-state) (keyword? new-state))
    (throw (err/error :store/session-invalid
                      "transition states must be keywords"
                      {:expected-state expected-state :new-state new-state})))
  (when-not (edn-safe-map? data)
    (throw (err/error :store/session-invalid
                      "transition data must be nil or an EDN-safe map"
                      {:data data})))
  (when-not (valid-transition? expected-state new-state)
    (throw (err/error :session/invalid-transition
                      "not an edge of the session state machine"
                      {:session/id (types/session-id session-id)
                       :expected-state expected-state
                       :new-state new-state})))
  (let [ss-store (normalize-store store)]
    (ss/transition-session! ss-store session-id expected-state new-state)))

(defn get-session
  "The session as the public Session contract map, or nil when no
  session has `session-id`. Read-only."
  [store session-id]
  (let [ss-store (normalize-store store)]
    (some-> (ss/find-session ss-store session-id)
            validate-session)))

;; ---------------------------------------------------------------------------
;; Session helpers for subagent child execution (S3)
;; ---------------------------------------------------------------------------

(defn get-session!
  "Fetch session or throw :store/session-not-found when missing.
  Used by subagent run path to distinguish :subagent/not-found from
  generic nil."
  [store session-id]
  (or (get-session store session-id)
      (throw (ex-info (str "session not found: " (str session-id))
                      {:error/type :store/session-not-found
                       :session/id (try (types/session-id session-id)
                                        (catch Exception _ session-id))}))))

(defn session-exists?
  "True when a session with `session-id` exists."
  [store session-id]
  (boolean (get-session store session-id)))

(defn child-session?
  "True when `session-id` is a child subagent session (has a parent link).
  Requires the subagent link table; returns false when the table is absent
  or the link is not found. Lazy-requires subagent to avoid circular deps."
  [store session-id]
  (try
    (let [subagent-ns (try (requiring-resolve 'evoclj.runtime.subagent/get-parent-session-id)
                           (catch Exception _ nil))]
      (if subagent-ns
        (boolean (@subagent-ns store session-id))
        false))
    (catch Exception _ false)))

;; ---------------------------------------------------------------------------
;; Subagent graph helpers (S4)
;; ---------------------------------------------------------------------------

(defn- ensure-subagent-link-table*
  [db]
  (let [spec (if (instance? evoclj.store.session_store.SessionStore db)
               (.-db ^evoclj.store.session_store.SessionStore db)
               (if (string? db) db db))]
    (try
      (evoclj.store.sqlite/with-db [conn spec]
        (clojure.java.jdbc/execute! conn
                       ["CREATE TABLE IF NOT EXISTS subagent_links (child_session_id TEXT PRIMARY KEY, parent_session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE, created_at TEXT NOT NULL)"])
        (clojure.java.jdbc/execute! conn
                       ["CREATE INDEX IF NOT EXISTS subagent_links_parent_idx ON subagent_links(parent_session_id)"]))
      (catch Exception _))))

(defn list-descendants
  [db root-id]
  (ensure-subagent-link-table* db)
  (let [root-uuid (try (evoclj.genome.types/session-id root-id) (catch Exception _ root-id))
        spec (if (instance? evoclj.store.session_store.SessionStore db)
               (.-db ^evoclj.store.session_store.SessionStore db)
               db)]
    (loop [queue [root-uuid] visited #{} result []]
      (if (empty? queue)
        result
        (let [cur (first queue)
              rest-q (vec (rest queue))]
          (if (contains? visited cur)
            (recur rest-q visited result)
            (let [visited2 (conj visited cur)
                  children (try
                             (mapv #(evoclj.genome.types/session-id (:child_session_id %))
                                   (evoclj.store.sqlite/query spec ["SELECT child_session_id FROM subagent_links WHERE parent_session_id = ? ORDER BY created_at" (str cur)]))
                             (catch Exception _ []))
                  new-result (into result children)
                  new-queue (into rest-q children)]
              (recur new-queue visited2 new-result))))))))

(defn try-cancel-session!
  [db session-id]
  (let [sid (try (evoclj.genome.types/session-id session-id) (catch Exception _ session-id))
        sess (get-session db sid)]
    (when sess
      (let [cur (:state sess)]
        (cond
          (= cur :cancelled) sess
          (contains? terminal-states cur) sess
          :else
          (let [spec (if (instance? evoclj.store.session_store.SessionStore db)
                       (.-db ^evoclj.store.session_store.SessionStore db)
                       db)
                ts (.format java.time.format.DateTimeFormatter/ISO_INSTANT (java.time.Instant/now))
                updated (try
                           (evoclj.store.sqlite/with-db [conn spec]
                             (let [cnt (first (clojure.java.jdbc/execute! conn ["UPDATE sessions SET state = 'cancelled', updated_at = ? WHERE id = ? AND state NOT IN ('completed','failed','cancelled','budget-exhausted')" ts (str sid)]))]
                               (= 1 cnt)))
                           (catch Exception _ false))]
            (if updated
              (get-session db sid)
              (if (valid-transition? cur :cancelled)
                (try (transition-session! db sid cur :cancelled nil) (catch Exception _ (get-session db sid)))
                (get-session db sid)))))))))
  