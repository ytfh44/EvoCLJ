(ns evoclj.promotion.state
  "component — generation states and candidate deployment states as
  PURE, CLOSED transition tables.

  This namespace is data plus pure functions: it holds no SQL, holds
  no reference to any CURRENT pointer, and makes no writes anywhere
  (Global Constraint 15 keeps promotion a separate subsystem; component
  owns the atomic compare-and-set and its transaction). Callers read
  the tables, ask what a state may transition to, or validate one
  transition; every entry point is a pure function of its arguments.

  GENERATION STATES (component, normative): :seed :active :superseded
  :rolled-back, with the edges :seed → :active, :active → :superseded,
  :active → :rolled-back. A generation that was never active is never
  rolled back; :superseded and :rolled-back are terminal.

  CANDIDATE DEPLOYMENT STATES (component, normative): :rejected :stale
  :canary :promoted :canary-failed. They extend the component candidate
  machine from evolution/candidate.clj (:proposed → :materialized →
  :evaluation-pending; M8 appends :evaluation-pending → #{:evaluated
  :invalid}); the M9 deployment edges are :evaluated → #{:canary
  :promoted :rejected :stale} and :canary → #{:promoted
  :canary-failed}. :rejected, :stale, :promoted, and :canary-failed
  are terminal.

  THE KEY RULE (Steps 1-2, normative): only an evaluated, ELIGIBLE
  candidate may enter :canary or direct :promoted. 'Evaluated-only' is
  encoded structurally — only :evaluated carries the canary/promotion
  edges, so a candidate that has not reached :evaluated has no path
  into deployment. 'Eligible-only' is enforced by
  `deployment-transition` against the candidate's FINALIZED eligibility
  data (:eligibility {:eligible? bool :reasons [...]} from the
  evaluator, component): an ineligible evaluated candidate can ONLY
  become :rejected — :canary, :promoted, and even :stale fail with
  :promotion/ineligible. Eligibility gates ENTRY only: a candidate
  already in :canary exits to :promoted/:canary-failed per the canary
  guardrails (component) and never re-checks entry eligibility.

  ERROR CONTRACT (Global Constraint 22 — plain serializable data):
  :promotion/unknown-state (a state outside the vocabulary),
  :promotion/invalid-transition (not an edge of the closed table),
  :promotion/ineligible (an ineligible candidate attempted a
  deployment entry it may not take)."
  (:require [evoclj.kernel.error :as err]))

;; --- the state vocabularies ----------------------------------------------------

(def generation-states
  "The NORMATIVE component generation states."
  #{:seed :active :superseded :rolled-back})

(def deployment-states
  "The NORMATIVE component candidate terminal/deployment states."
  #{:rejected :stale :canary :promoted :canary-failed})

(def candidate-states
  "The full candidate state vocabulary: the component base machine plus
  the M8 evaluation outcomes (:evaluated :invalid) and the M9
  deployment states."
  (into #{} (concat [:proposed :materialized :evaluation-pending
                     :evaluated :invalid]
                    deployment-states)))

;; --- the pure, closed transition tables ---------------------------------------

(def generation-transitions
  "The closed generation transition table (data, no SQL). A generation
  is born :seed, becomes :active, and then either :superseded (a newer
  generation won the CURRENT compare-and-set) or :rolled-back (component — future selection only, never reversing external effects,
  Global Constraint 18). :superseded and :rolled-back are terminal:
  nothing transitions out of them."
  {:seed #{:active}
   :active #{:superseded :rolled-back}
   :superseded #{}
   :rolled-back #{}})

(def candidate-transitions
  "The closed candidate state machine (data, no SQL): the component
  base fragment (:proposed → :materialized → :evaluation-pending,
  realized by evolution/candidate.clj), M8's evaluation outcomes
  (:evaluation-pending → #{:evaluated :invalid}), and the M9
  deployment fragment. :evaluated is the ONLY entry point into the
  deployment states; :rejected :stale :promoted :canary-failed (and
  :invalid) are terminal."
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

;; --- typed errors ---------------------------------------------------------------

(defn- unknown-state!
  "Throw :promotion/unknown-state for a state outside the vocabulary."
  [vocabulary state]
  (throw (err/error :promotion/unknown-state
                    "state is outside the state vocabulary"
                    {:state state
                     :vocabulary (vec (sort vocabulary))})))

(defn- invalid-transition!
  "Throw :promotion/invalid-transition for a from→to pair that is not
  an edge of the closed table."
  [from to]
  (throw (err/error :promotion/invalid-transition
                    "not an edge of the closed transition table"
                    {:from from :to to})))

;; --- table access (pure) --------------------------------------------------------

(defn next-generation-states
  "The allowed generation transitions out of `state` (the table row),
  or a :promotion/unknown-state error when `state` is not a generation
  state."
  [state]
  (when-not (contains? generation-states state)
    (unknown-state! generation-states state))
  (generation-transitions state))

(defn next-candidate-states
  "The allowed candidate transitions out of `state` (the table row),
  or a :promotion/unknown-state error when `state` is not a candidate
  state."
  [state]
  (when-not (contains? candidate-states state)
    (unknown-state! candidate-states state))
  (candidate-transitions state))

;; --- transition validation (pure) -------------------------------------------------

(defn generation-transition
  "Validate a generation transition `from` → `to` against the closed
  generation table. Returns `to` on success; throws
  :promotion/unknown-state for a state outside the vocabulary or
  :promotion/invalid-transition for a pair that is not an edge."
  [from to]
  (when-not (contains? generation-states from)
    (unknown-state! generation-states from))
  (when-not (contains? generation-states to)
    (unknown-state! generation-states to))
  (when-not (contains? (generation-transitions from) to)
    (invalid-transition! from to))
  to)

(defn eligible-deployment-states
  "THE KEY RULE as data: the deployment states an :evaluated candidate
  may enter given its FINALIZED eligibility judgment (component shape
  {:eligible? bool :reasons [...]}).

  - :eligible? true  → #{:canary :promoted :stale} — may start a
    canary rollout, promote directly, or become :stale (the component
    sibling that lost the CURRENT compare-and-set).
  - :eligible? false → #{:rejected} — an ineligible candidate can ONLY
    become :rejected (component Step 2). Missing or non-true :eligible?
    is treated as ineligible (fail-closed): judgment is never guessed."
  [eligibility]
  (if (true? (:eligible? eligibility))
    #{:canary :promoted :stale}
    #{:rejected}))

(defn deployment-transition
  "Validate a candidate deployment transition `candidate-state` → `to`
  against the closed candidate machine, applying the eligibility gate
  (Steps 1-2). Returns `to` on success; throws:

  - :promotion/unknown-state — `candidate-state` or `to` is outside
    the candidate vocabulary;
  - :promotion/invalid-transition — not an edge of the closed table
    (including any entry into deployment from a state other than
    :evaluated — the evaluated-only rule is structural);
  - :promotion/ineligible — an :evaluated candidate whose finalized
    eligibility is false attempted :canary, :promoted, or :stale,
    when only :rejected is permitted.

  `eligibility` is the finalized evaluator judgment consumed
  verbatim; it is consulted ONLY on the :evaluated → deployment entry
  edges — a candidate already in :canary exits per the component
  guardrails (:promoted / :canary-failed) and never re-checks entry
  eligibility."
  [candidate-state eligibility to]
  (when-not (contains? candidate-states candidate-state)
    (unknown-state! candidate-states candidate-state))
  (when-not (contains? candidate-states to)
    (unknown-state! candidate-states to))
  (when-not (contains? (candidate-transitions candidate-state) to)
    (invalid-transition! candidate-state to))
  (when (and (= :evaluated candidate-state)
             (not (contains? (eligible-deployment-states eligibility) to)))
    (throw (err/error :promotion/ineligible
                      "an ineligible candidate may only become :rejected"
                      {:candidate-state candidate-state
                       :to to
                       :eligibility (err/sanitize eligibility)})))
  to)
