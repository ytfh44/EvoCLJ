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
  (:require [clojure.edn :as edn]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.session-states :as sstates]
            [evoclj.store.session-store :as ss]
            [evoclj.store.existence :as existence])
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
  