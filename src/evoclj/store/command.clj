(ns evoclj.store.command
  "Malli schemas for the async Command contract (A1).

  `CommandSchema` validates the durable async command row before it is
  written to the `commands` table (migration 012). The shape mirrors
  GC-21 (payloads live in the CAS, rows carry a sha256: reference) and
  the AsyncCommand state machine [W-20..W-24]:

    queued -> {running,failed,cancelled}
    running -> {succeeded,failed,timed-out,cancelled}

  No DB operations are exposed here (A2 owns `create-command!`); this
  namespace is schema + pure validation helpers only, mirroring the
  style of `evoclj.store.event-schema` and `evoclj.store.schema`."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(def ^:private sha256-re #"^sha256:[0-9a-f]{64}$")

(def ^:private allowed-states
  "The six AsyncCommand states (kebab-case keywords)."
  #{:queued :running :succeeded :failed :timed-out :cancelled})

(def CommandState
  "Malli enum for :cmd/state."
  [:enum :queued :running :succeeded :failed :timed-out :cancelled])

(def CommandSchema
  "Closed Malli map for a durable async command.

  Required keys:
    :cmd/id                uuid?
    :cmd/type              keyword?
    :cmd/state             CommandState
    :cmd/idempotency-key   string? (non-empty, unique per DB constraint)
    :cmd/payload-ref       string? sha256: CAS reference (GC-21)
    :cmd/owner-session-id  uuid?
    :cmd/created-at        inst?

  Optional keys:
    :cmd/parent-cmd-id     uuid? (causal parent command)
    :cmd/continuation-edn  any EDN value (stored as TEXT EDN in DB)
    :cmd/deadline          inst? (expiry for timed-out)"
  [:map {:closed true}
   [:cmd/id uuid?]
   [:cmd/type keyword?]
   [:cmd/state CommandState]
   [:cmd/idempotency-key [:and string? [:fn {:error/message "must be non-empty"} #(seq %)]]]
   [:cmd/payload-ref [:and string? [:re sha256-re]]]
   [:cmd/owner-session-id uuid?]
   [:cmd/created-at [:fn {:error/message "must be an inst"} #(inst? %)]]
   [:cmd/parent-cmd-id {:optional true} [:maybe uuid?]]
   [:cmd/continuation-edn {:optional true} :any]
   [:cmd/deadline {:optional true} [:maybe [:fn {:error/message "must be an inst"} #(inst? %)]]]])

(defn command?
  "True when `x` satisfies CommandSchema."
  [x]
  (m/validate CommandSchema x))

(defn command-state?
  "True when `s` is one of the six allowed command states."
  [s]
  (contains? allowed-states s))

(defn validate-command
  "Validate `cmd` against CommandSchema. Returns `cmd` unchanged, or
  throws :store/command-invalid carrying a humanized Malli explanation."
  [cmd]
  (if-let [expl (m/explain CommandSchema cmd)]
    (throw (err/error :store/command-invalid
                      "command does not satisfy the Command contract"
                      {:errors (me/humanize expl)
                       :explain expl}))
    cmd))

(defn explain-command
  "Return a humanized Malli explanation for `cmd`, or nil when valid."
  [cmd]
  (when-let [expl (m/explain CommandSchema cmd)]
    (me/humanize expl)))
