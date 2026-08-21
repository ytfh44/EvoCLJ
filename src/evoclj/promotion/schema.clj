(ns evoclj.promotion.schema
  "Malli schemas for the component state vocabulary and the Promotion
  record (docs 'Detailed Public Data Contracts').

  The state-value schemas are derived from the pure transition-table
  vocabulary in evoclj.promotion.state — a single source of truth for
  the states (Global Constraint 17: every promoted generation retains
  complete lineage, so the states and the Promotion record must agree
  with the machine that admits them).

  The Promotion record contract:

      {:promotion/id uuid?
       :candidate/id uuid?
       :evaluation/id uuid?
       :from-generation stable-id?   ; string, e.g. \"generation-1\"
       :to-generation stable-id?
       :decision keyword?            ; :canary :promoted :rejected
                                    ; :stale :canary-failed (component :rolled-back)
       :reason map?                  ; the finalized decision context
       :created-at inst?}

  Eligibility data is the evaluator's finalized judgment (component):
  {:eligible? boolean? :reasons [<reason maps>]} — :reasons is empty
  exactly when :eligible? is true. Promotion consumes this finalized
  data and NEVER re-computes evaluator judgment (component Step 4)."
  (:require [evoclj.promotion.state :as state]
            [malli.core :as m]))

;; --- state-value schemas ------------------------------------------------------

(def GenerationStateSchema
  "The four generation states (component): :seed :active :superseded
  :rolled-back."
  (into [:enum] (sort state/generation-states)))

(def DeploymentStateSchema
  "The five candidate terminal/deployment states (component): :rejected
  :stale :canary :promoted :canary-failed."
  (into [:enum] (sort state/deployment-states)))

(def CandidateStateSchema
  "The full candidate state vocabulary — the component base machine
  (:proposed :materialized :evaluation-pending), the M8 evaluation
  outcomes (:evaluated :invalid), and the M9 deployment states. A
  Candidate record's :state is validated against this."
  (into [:enum] (sort state/candidate-states)))

(def EligibilitySchema
  "The evaluator's finalized eligibility judgment (component). :reasons
  is empty exactly when :eligible? is true; a non-empty :reasons means
  ineligible. Promotion treats this map as immutable finalized data."
  [:map {:closed true}
   [:eligible? boolean?]
   [:reasons vector?]])

;; --- the Promotion record ------------------------------------------------------

(def PromotionSchema
  "The public Promotion record contract (docs 'Detailed Public Data
  Contracts'): one Promotion references exactly one finalized
  Evaluation (Database Invariant 5), names the generation pair the
  compare-and-set moved between, and records the decision and its
  reason. :from-generation / :to-generation are stable generation ids
  (strings, matching the generations table)."
  [:map {:closed true}
   [:promotion/id uuid?]
   [:candidate/id uuid?]
   [:evaluation/id uuid?]
   [:from-generation string?]
   [:to-generation string?]
   [:decision keyword?]
   [:reason map?]
   [:created-at [:fn inst?]]])
