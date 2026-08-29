(ns evoclj.store.session-states
  "Single canonical source for session state vocabulary, transitions,
  DB mapping, and Malli schemas (Fleet S2 horizontal — definition > validation).

  This namespace DEFINES the states. Every other namespace VALIDATES
  against it — no duplicate literal sets, transition tables, or DB
  mappings elsewhere (definition > validation). The DB CHECK constraint
  in resources/migrations/001-init.sql and the session lifecycle docs
  are the persisted projection of this definition.

  VOCABULARY (8 states, closed):

    :created           — pinned, not yet resolving
    :resolving         — resolving generation/phenotype
    :running           — active execution
    :waiting           — paused waiting for input
    :completed         — terminal success
    :failed            — terminal failure
    :cancelled         — terminal cancellation
    :budget-exhausted  — terminal budget exhaustion

  DB MAPPING (8 persisted states, matches sessions.state CHECK):

    'created'          <-> :created
    'resolving'        <-> :resolving
    'running'          <-> :running
    'waiting'          <-> :waiting
    'completed'        <-> :completed
    'failed'           <-> :failed
    'cancelled'        <-> :cancelled
    'budget-exhausted' <-> :budget-exhausted

  TRANSITIONS (closed machine):

    :created          -> #{:resolving}
    :resolving        -> #{:running}
    :running          -> #{:waiting :failed :cancelled :budget-exhausted}
    :waiting          -> #{:running :completed}
    :completed, :failed, :cancelled, :budget-exhausted -> #{}")

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(def session-states
  "Closed session state vocabulary (8 states)."
  #{:created :resolving :running :waiting
    :completed :failed :cancelled :budget-exhausted})

(def session-transitions
  "Closed session transition table (definition). Terminal states map to #{}."
  {:created #{:resolving}
   :resolving #{:running}
   :running #{:waiting :failed :cancelled :budget-exhausted}
   :waiting #{:running :completed}
   :completed #{}
   :failed #{}
   :cancelled #{}
   :budget-exhausted #{}})

(def terminal-states
  "States that accept no further transitions."
  #{:completed :failed :cancelled :budget-exhausted})

;; ---------------------------------------------------------------------------
;; DB mapping (single source; mirrors sessions.state CHECK)
;; ---------------------------------------------------------------------------

(def db-state->kw
  "DB string -> keyword. All 8 states are persisted."
  {"created" :created
   "resolving" :resolving
   "running" :running
   "waiting" :waiting
   "completed" :completed
   "failed" :failed
   "cancelled" :cancelled
   "budget-exhausted" :budget-exhausted})

(def kw->db-state
  "Keyword -> DB string. Every session state is persistable."
  (into {} (map (fn [[db kw]] [kw db]) db-state->kw)))

;; Legacy alias names
(def db-state->state db-state->kw)
(def state->db-state kw->db-state)

;; ---------------------------------------------------------------------------
;; Malli enum (definition > validation)
;; ---------------------------------------------------------------------------

(def session-state-enum
  "Malli [:enum ...] vector derived from session-states (single source
  for SessionSchema :state validation — :banana is rejected)."
  (into [:enum] (sort session-states)))

;; ---------------------------------------------------------------------------
;; Pure helpers (no I/O, no SQL)
;; ---------------------------------------------------------------------------

(defn session-state?
  "True when kw is in the closed session vocabulary."
  [kw]
  (contains? session-states kw))

(defn db-state?
  "True when s is a persisted DB string."
  [s]
  (contains? db-state->kw s))

(defn kw->db
  "Keyword -> DB string, or nil when not persisted."
  [kw]
  (get kw->db-state kw))

(defn db->kw
  "DB string -> keyword, or nil for unknown strings."
  [s]
  (get db-state->kw s))

(defn next-states
  "The allowed successors of state, or nil when unknown."
  [state]
  (get session-transitions state))

(defn valid-transition?
  "True when from -> to is an edge of the closed table."
  [from to]
  (contains? (get session-transitions from #{}) to))

(defn valid-state?
  "Alias for session-state?"
  [kw]
  (session-state? kw))