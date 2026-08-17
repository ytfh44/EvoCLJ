(ns evoclj.promotion.rollback-compensation
  "Task 9.5 follow-up — external-effect compensation manifest.

  When a generation is rolled back (see evoclj.promotion.rollback, which
  is SELECTION-ONLY per Global Constraint 18), the already-committed
  external effects of that generation are NOT reversed by the rollback
  machinery. This namespace produces a READ-ONLY, fail-soft
  compensation manifest: a plain list of the external-effect events that
  the rolled-back generation had already emitted to the outside world.

  PURPOSE (intentional half-open loop): the manifest is input for
  HUMAN / downstream reconciliation — it tells an operator exactly which
  external effects this generation produced so they can be audited,
  compensated, or accepted deliberately. It does NOT attempt automatic
  reversal (GC18: rollback must never claim to undo committed external
  effects). The decision to actually compensate is a separately
  authorized operator/agent task, never this code.

  This is a PURE function: no IO, no randomness, deterministic. Given the
  same generation-id and events it always returns the same manifest, with
  only the matching events listed in the order supplied. Missing fields
  are filled with nil rather than throwing (fail-soft).")

;; --- constants ---------------------------------------------------------------

(def ^:const external-effect-types
  "The :event/type values that denote an external-world effect (as
  opposed to an internal bookkeeping event). These are the events a
  reconciliation operator cares about after a rollback:
    :intent/invoked    — a tool was invoked (side-effecting call)
    :intent/llm-completed — a model was called (external compute/cost)
    :memory/write      — a memory was written (persisted externally)
    :effect/emitted    — an output was emitted (surfaced to the world)"
  #{:intent/invoked
    :intent/llm-completed
    :memory/write
    :effect/emitted})

(def ^:const manifest-fields
  "The projection of each matching event we surface in the manifest.
  `:timestamp` falls back to `:created-at` when the canonical `:timestamp`
  key is absent (defensive, fail-soft)."
  [:event/type :session/id :phenotype/id :intent/id :timestamp])

;; --- helpers -----------------------------------------------------------------

(defn- external-effect?
  "True iff `event` is an external-effect event belonging to `generation-id`.
  Matching requires both: the event's :event/type is in
  external-effect-types AND its :generation/id equals generation-id."
  [generation-id event]
  (and (contains? external-effect-types (:event/type event))
       (= generation-id (:generation/id event))))

(defn- project-event
  "Project one matching event down to the manifest entry, filling missing
  keys with nil (fail-soft, no exceptions)."
  [event]
  {:event/type  (:event/type event)
   :session/id  (:session/id event)
   :phenotype/id (:phenotype/id event)
   :intent/id   (:intent/id event)
   :timestamp   (or (:timestamp event) (:created-at event))})

;; --- public API --------------------------------------------------------------

(defn compensation-manifest
  "Build the external-effect compensation manifest for `generation-id`
  from a vector of `events`.

  Returns a map:
    {:generation/id <str>
     :effects      [{:event/type ... :session/id ... :phenotype/id ...
                     :intent/id ... :timestamp ...} ...]
     :count        <int>}

  Only events whose :event/type is in external-effect-types AND whose
  :generation/id equals `generation-id` are included. Non-matching
  events (wrong type, wrong generation, or both) are excluded. Field
  values missing from an event are filled with nil rather than throwing
  (fail-soft). The function is pure: no IO, no random, deterministic."
  [generation-id events]
  (let [effects (->> (or events [])
                     (filter (partial external-effect? generation-id))
                     (mapv project-event))]
    {:generation/id generation-id
     :effects       effects
     :count         (count effects)}))
