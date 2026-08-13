(ns evoclj.store.session
  "Session pinning and lifecycle transitions (Task 5.4).

  A session is created pinned to one Genome, one Resolution, one
  Phenotype, and one Generation for its whole lifetime (Global
  Constraint 2, Database Invariant 2). The pinned identity columns are
  written once at insert and never touched again: the ONLY write path
  after creation is transition-session!, a compare-and-set UPDATE that
  matches the stored state and changes state (plus the transition
  timestamp) alone.

  State machine (normative, docs Task 5.4):

      :created → :resolving → :running ↔ :waiting → :completed
                               ├──────────────→ :failed
                               ├──────────────→ :cancelled
                               └──────────────→ :budget-exhausted

  Terminal states (:completed :failed :cancelled :budget-exhausted)
  accept no further transitions. transition-session! rejects a
  statically illegal edge (:session/invalid-transition) before touching
  the database, and the SQL `WHERE state = expected-state` backstop
  means a concurrent worker that lost the compare-and-set also sees
  :session/invalid-transition — two workers can never both transition
  from the same state silently (Task 5.4 Step 4).

  Public Session contract (docs 'Detailed Public Data Contracts'):
  :session/id, :generation/id, :genome/id, :resolution/id,
  :phenotype/id, :state, :created-at, :routing. Pinned identity fields
  are immutable after insert.

  Known deviation: the Task 5.1 sessions schema defines no data or
  routing column, and Task 5.4 may not touch migrations, so the
  transition `data` argument and the optional :routing input are
  validated at the module boundary (Global Constraint 22) but NOT
  persisted; get-session reports :routing as nil. Terminal-state
  classification is therefore driven by :state, which IS persisted.
  Persisting data/routing is a schema change for a later task. The
  Database Invariant 2 guarantee is enforced at the application layer
  (the only write path is the state CAS) because the schema committed
  in Task 5.1 has no sessions trigger and migrations are out of scope
  here."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; --- state machine (normative) ---------------------------------------------

(def states
  "Every state in the Task 5.4 state machine."
  #{:created :resolving :running :waiting
    :completed :failed :cancelled :budget-exhausted})

(def transitions
  "State machine edges: source state → the set of allowed target
  states. Terminal states have no entry, and therefore no outgoing
  edges, so a transition FROM :completed/:failed/:cancelled/
  :budget-exhausted is always invalid."
  {:created #{:resolving}
   :resolving #{:running}
   :running #{:waiting :failed :cancelled :budget-exhausted}
   :waiting #{:running :completed}})

(def terminal-states
  "States that accept no further transitions."
  #{:completed :failed :cancelled :budget-exhausted})

(defn- valid-transition?
  "True when (expected-state → new-state) is an edge of the state
  machine. Unknown states and terminal sources fail."
  [expected-state new-state]
  (contains? (get transitions expected-state #{}) new-state))

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
   [:created-at {:optional true} [:fn inst?]]])

(def SessionSchema
  "The public Session contract map returned by create-session! and
  get-session."
  [:map {:closed true}
   [:session/id uuid?]
   [:generation/id string?]
   [:genome/id [:fn types/genome-id?]]
   [:resolution/id [:fn types/resolution-id?]]
   [:phenotype/id [:fn types/artifact-id?]]
   [:state keyword?]
   [:created-at [:fn inst?]]
   [:routing [:maybe routing-schema]]])

(defn- schema-error!
  "Throw :store/session-invalid with a humanized Malli explanation."
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
  "True when x is nil or a map that round-trips through
  pr-str / clojure.edn read-string, so no function, Java object, or
  lazy sequence can cross the transition boundary (Global Constraint
  22)."
  [x]
  (or (nil? x)
      (and (map? x)
           (try
             (map? (edn/read-string (pr-str x)))
             (catch Exception _ false)))))

;; --- persistence helpers ------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  "Canonical ISO-8601 UTC string for a timestamp value (a
  java.util.Date, java.time.Instant, or ISO-8601 string); nil means
  now."
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
   :routing nil})

(defn- set-busy-timeout!
  "Set SQLite's busy_timeout on the open connection carried by `db`.

  `db` is the spec-with-connection map that evoclj.store.sqlite/with-db
  binds: java.jdbc 0.7.12 stores the live Connection under :connection
  (see add-connection / db-find-connection). The pragma must run
  through raw JDBC because java.jdbc's execute! routes PRAGMA
  statements through PreparedStatement.executeUpdate, and sqlite-jdbc
  classifies this PRAGMA as a query and throws a driver error (Query
  returns results). The setting applies to the same connection the
  compare-and-set UPDATE below runs on, so a contended UPDATE waits for
  a concurrent writer's commit instead of failing with SQLITE_BUSY."
  [db ms]
  (let [^java.sql.Connection conn (:connection db)]
    (with-open [stmt (.createStatement conn)]
      (.execute stmt (str "PRAGMA busy_timeout = " ms)))))

;; --- public API ---------------------------------------------------------------

(declare get-session)

(defn create-session!
  "Create a session row pinned to the request's Genome, Resolution,
  Phenotype, and Generation ids (Global Constraint 2) and return the
  persisted Session contract map. The pinned identity fields are
  immutable after insert — no API can change them later.

  Typed errors: :store/session-invalid (contract violation, including
  unknown keys — the trust boundary is a closed map),
  :store/generation-not-found (no generation row with that id). The
  optional :routing input is validated but not persisted (see the
  namespace docstring)."
  [store request]
  (validate-create-request request)
  (let [sid (UUID/randomUUID)
        ts (canonical-timestamp (:created-at request))]
    (sqlite/with-db [conn store]
      (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?"
                                         (:generation/id request)]))
        (throw (err/error :store/generation-not-found
                          "cannot pin a session to an unknown generation"
                          {:generation/id (:generation/id request)})))
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id (:generation/id request)
                     :genome_id (:genome/id request)
                     :resolution_id (:resolution/id request)
                     :phenotype_id (:phenotype/id request)
                     :state (name :created)
                     :created_at ts}))
    (get-session store sid)))

(defn transition-session!
  "Compare-and-set state transition (Task 5.4 Step 4).

  Sets the session's state to `new-state` ONLY when the stored state
  is exactly `expected-state`, in one atomic UPDATE, and returns the
  updated Session contract map. `data` is transition metadata (nil or
  an EDN-safe map), validated at the boundary (Global Constraint 22)
  but not persisted — the Task 5.1 schema has no data column (see the
  namespace docstring).

  Typed errors:
    :store/session-invalid      — non-keyword states or non-EDN data
    :store/session-not-found    — no session with this id
    :session/invalid-transition — (expected-state → new-state) is not
      an edge of the state machine, or the stored state is not
      `expected-state` (including a concurrent worker that already won
      the compare-and-set)."
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
  (let [sid (types/session-id session-id)
        key (str sid)
        ts (canonical-timestamp nil)]
    (sqlite/with-db [conn store]
      ;; busy_timeout makes a contended UPDATE wait for SQLite's write
      ;; lock instead of failing with SQLITE_BUSY, so a racing worker
      ;; observes 0 affected rows (and :session/invalid-transition)
      ;; rather than a raw driver error.
      (set-busy-timeout! conn 10000)
      (let [count (first (jdbc/execute! conn
                                        ["UPDATE sessions
                                          SET state = ?, updated_at = ?
                                          WHERE id = ? AND state = ?"
                                         (name new-state) ts key (name expected-state)]))]
        (when-not (= 1 count)
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
    (get-session store sid)))

(defn get-session
  "The session as the public Session contract map, or nil when no
  session has `session-id`. Read-only."
  [store session-id]
  (some-> (first (sqlite/query store ["SELECT * FROM sessions WHERE id = ?"
                                      (str (types/session-id session-id))]))
          row->session
          validate-session))
