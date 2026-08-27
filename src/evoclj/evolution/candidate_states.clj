(ns evoclj.evolution.candidate-states
  "Single canonical source for candidate state vocabulary, transitions,
  DB mapping, and Malli schemas (Fleet S2 — definition > validation).

  This namespace DEFINES the states. Every other namespace VALIDATES
  against it — no duplicate literal sets, transition tables, or DB
  mappings elsewhere (definition > validation). The DB CHECK constraint
  in resources/migrations/001-init.sql is the persisted projection of
  this definition.

  VOCABULARY (10 states, closed):

    :proposed             — pre-persistence (no DB row)
    :materialized         — DB 'materialized'
    :evaluation-pending   — DB 'evaluating'
    :evaluated            — DB 'eligible'  (M8)
    :invalid              — no DB value    (M8, terminal)
    :canary               — canary rollout (M9, no 5.1 DB value)
    :promoted             — DB 'promoted'  (M9, terminal)
    :rejected             — DB 'rejected'  (M9, terminal)
    :stale                — DB 'stale'     (M9, terminal)
    :canary-failed        — canary failed  (M9, terminal, no 5.1 DB value)

  DB MAPPING (6 persisted states, matches 001-init.sql CHECK):

    'materialized' ↔ :materialized
    'evaluating'   ↔ :evaluation-pending
    'eligible'     ↔ :evaluated
    'promoted'     ↔ :promoted
    'rejected'     ↔ :rejected
    'stale'        ↔ :stale

  :proposed, :invalid, :canary, :canary-failed have no DB string
  (nil from kw->db-state). Unknown DB strings decode to nil.

  TRANSITIONS (closed machine):

    :proposed            → #{:materialized}
    :materialized        → #{:evaluation-pending}
    :evaluation-pending  → #{:evaluated :invalid}
    :evaluated           → #{:canary :promoted :rejected :stale}
    :canary              → #{:promoted :canary-failed}
    :invalid, :rejected, :stale, :promoted, :canary-failed → #{}"
  )

;; ---------------------------------------------------------------------------
;; Vocabulary
;; ---------------------------------------------------------------------------

(def candidate-states
  "Closed candidate state vocabulary (10 states)."
  #{:proposed :materialized :evaluation-pending :evaluated :invalid
    :canary :promoted :rejected :stale :canary-failed})

(def candidate-transitions
  "Closed candidate transition table (definition). Terminal states map to #{}."
  {:proposed #{:materialized}
   :materialized #{:evaluation-pending}
   :evaluation-pending #{:evaluated :invalid}
   :evaluated #{:canary :promoted :rejected :stale}
   :canary #{:promoted :canary-failed}
   :invalid #{}
   :rejected #{}
   :stale #{}
   :promoted #{}
   :canary-failed #{}})

;; ---------------------------------------------------------------------------
;; DB mapping (single source; mirrors 001-init.sql CHECK)
;; ---------------------------------------------------------------------------

(def db-state->kw
  "DB string → keyword. Only the six persisted states have entries."
  {"materialized" :materialized
   "evaluating"   :evaluation-pending
   "eligible"     :evaluated
   "promoted"     :promoted
   "rejected"     :rejected
   "stale"        :stale})

(def kw->db-state
  "Keyword → DB string. Non-persisted states (e.g. :proposed, :invalid,
  :canary, :canary-failed) map to nil; :evaluation-pending maps to \"evaluating\"."
  (into {} (map (fn [[db kw]] [kw db]) db-state->kw)))

;; Legacy alias names expected by callers that used the old private vars
(def db-state->state db-state->kw)
(def state->db-state kw->db-state)

;; ---------------------------------------------------------------------------
;; Malli enum (definition > validation)
;; ---------------------------------------------------------------------------

(def candidate-state-enum
  "Malli [:enum ...] vector derived from candidate-states (single source
  for CandidateSchema :state validation — :banana is rejected)."
  (into [:enum] (sort candidate-states)))

;; ---------------------------------------------------------------------------
;; Pure helpers (no I/O, no SQL)
;; ---------------------------------------------------------------------------

(defn candidate-state?
  "True when kw is in the closed candidate vocabulary."
  [kw]
  (contains? candidate-states kw))

(defn db-state?
  "True when s is a persisted DB string (a key of db-state->kw)."
  [s]
  (contains? db-state->kw s))

(defn kw->db
  "Keyword → DB string, or nil when not persisted (e.g. :proposed)."
  [kw]
  (get kw->db-state kw))

(defn db->kw
  "DB string → keyword, or nil for unknown strings."
  [s]
  (get db-state->kw s))

(defn next-states
  "The allowed successors of state (table row), or nil when unknown."
  [state]
  (get candidate-transitions state))

(defn valid-transition?
  "True when from → to is an edge of the closed table."
  [from to]
  (contains? (get candidate-transitions from #{}) to))

(defn valid-state?
  "Alias for candidate-state?"
  [kw]
  (candidate-state? kw))
