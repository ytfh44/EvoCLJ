(ns evoclj.store.session
  "Session pinning - immutable context with no runtime state machine (W2).

  A session is created pinned to one Genome, one Resolution, one
  Phenotype, and one Generation for its whole lifetime (Global
  Constraint 2, Database Invariant 2). The pinned identity columns are
  written once at insert and never touched again - Session carries NO
  runtime state machine. The durable lifecycle is Work
  (queued/running/waiting/succeeded/failed/cancelled/timed-out) in
  evoclj.store.work / evoclj.runtime/work.

  Public Session contract (docs 'Detailed Public Data Contracts'):
  :session/id, :generation/id, :genome/id, :resolution/id,
  :phenotype/id, :created-at, :routing. Pinned identity fields
  are immutable after insert.

  Fleet R horizontal (narrow handle): this namespace is the business
  layer; persistence is via evoclj.store.session-store/SessionStore
  (opaque deftype). Raw maps are rejected (definition > validation).
  Fleet S2: Session is immutable pin; lifecycle is Work.
  Fleet P5/F: genome/phenotype/resolution existence is enforced via
  VerifiedDigest and FK at rest (011).
  W1 (Work unified lifecycle): Session is immutable context (pin:
  Genome/Resolution/CodeImage/Deployment/Generation). The durable
  lifecycle is Work (queued/running/waiting/succeeded/failed/cancelled/timed-out)
  in evoclj.store.work / evoclj.runtime/work."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.session-store :as ss]
            [evoclj.store.existence :as existence]
            [evoclj.store.work :as work-store]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util Date UUID)))

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
   [:session/id {:optional true} uuid?]
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
  get-session. Immutable pin - no state machine."
  [:map {:closed true}
   [:session/id uuid?]
   [:generation/id string?]
   [:genome/id [:fn types/genome-id?]]
   [:resolution/id [:fn types/resolution-id?]]
   [:phenotype/id [:fn types/artifact-id?]]
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
  "True when x is nil or a map of plain EDN-safe data (Global
  Constraint 22). Recursive pre-materialization check via
  evoclj.sci.boundary/edn-safe?: lazy seqs, records, functions and
  other non-data are rejected WITHOUT being realized or serialized."
  [x]
  (or (nil? x)
      (and (map? x) (boundary/edn-safe? x))))

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
  "Create a new session with pinned Genome/Resolution/Phenotype/Generation.
  Returns the public Session contract map (immutable pin, no state machine)."
  [store request]
  (validate-create-request request)
  (let [store (normalize-store store)
        sid (or (:session/id request) (UUID/randomUUID))]
    (ss/insert-session! store (assoc request :session/id sid))
    (get-session store sid)))

(defn get-session
  "The session as the public Session contract map, or nil when no
  session with this id exists."
  [store session-id]
  (let [store (normalize-store store)
        sid (try (types/session-id session-id) (catch Exception _ session-id))]
    (ss/find-session store sid)))

(defn try-cancel-session!
  "Cancel a session by marking its root Work as cancelled via Work store.
  Session is immutable pin - cancellation is driven by Work lifecycle.
  Returns the session map if found, nil otherwise."
  [store session-id]
  (let [sid (try (types/session-id session-id) (catch Exception _ session-id))
        sess (get-session store sid)
        db-spec (if (instance? evoclj.store.session_store.SessionStore store)
                  (ss/db-of store)
                  store)]
    (when sess
      (let [works (try (evoclj.store.work/list-works db-spec sid) (catch Exception _ []))]
        (when (seq works)
          (let [root-work (first works)]
            (evoclj.store.work/cancel-work! db-spec (:work/id root-work))))))
    sess))

(defn find-session
  "Find session by id via sqlite spec, or nil."
  [db session-id]
  (get-session db session-id))

(defn list-descendants
  "Return all descendant session ids (UUIDs) transitively spawned from
  `root-id` via Work graph (works.parent_work_id).

  Delegates to evoclj.store.work/list-descendants, the single owner of
  session-graph traversal (INV-05). Kept as a public entry point because
  this namespace is the business layer callers already hold."
  [db root-id]
  (work-store/list-descendants db root-id))

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
  "True when `session-id` is a child subagent session (has a parent Work).
  Parentage is resolved only from Work.parent_work_id; no event or helper-table
  fallback is consulted."
  [store session-id]
  (try
    (boolean (work-store/get-parent-session-id store session-id))
    (catch Exception _ false)))