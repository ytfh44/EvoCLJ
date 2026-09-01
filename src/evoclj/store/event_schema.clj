(ns evoclj.store.event-schema
  "Malli schemas for the public Event contract (E1; the `Event`
  contract in docs/implementation-plan.md after the E1 split).

  E1 splits the overloaded `Event.cause` into two orthogonal axes:
  * `:prev/event-id` — linear predecessor inside the SAME session
    (the log's hash chain). `nil` only for the v0 root set
    `#{:session/created}` which opens a session; every other event
    must carry a prev that is the immediate predecessor
    `(seq = prev-seq + 1)` in the same session.
  * `:causal-links` — semantic causality graph edges that MAY cross
    sessions (e.g. child terminal -> parent result). Each edge is
    `{:from <event-id> :type <keyword>}` where `:from` may be any
    prior event (any session) and `:to` is implicitly the appended
    event.

  `AppendRequestSchema` validates what callers may hand to
  `evoclj.store.event/append-event!`. Legacy `:cause/event-id` is
  still accepted as a deprecated alias for `:prev/event-id` when the
  latter is absent (same-session only, for backward compat during the
  wave), but new code must use `:prev/event-id` + `:causal-links`.

  `EventSchema` validates the persisted event returned by the store:
  includes `:prev/event-id`, `:causal-links` (set, possibly empty),
  `:prev-hash`, `:event-hash`, etc. Deprecated `:cause/event-id` is
  tolerated as an optional mirror of `:prev/event-id` for compat but
  not required.

  Both validators throw `:store/event-invalid` carrying a humanized
  Malli explanation."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(def event-hash-re
  "Canonical content-address form of an event hash: sha256:<64 hex>."
  #"^sha256:[0-9a-f]{64}$")

(def CausalLinkSchema
  "One causal edge supplied at append time: {:from <event-id> :type <keyword>}.
  `:to` is implicitly the event being appended."
  [:map {:closed true}
   [:from pos-int?]
   [:type keyword?]])

(def EventSchema
  "The persisted Event contract (implementation-plan `Event` after E1)."
  [:map {:closed true}
   [:event/id pos-int?]
   [:event/seq pos-int?]
   [:session/id uuid?]
   [:generation/id string?]
   [:phenotype/id [:fn types/artifact-id?]]
   [:event/type keyword?]
   [:prev/event-id [:maybe pos-int?]]
   [:causal-links {:optional true} [:set CausalLinkSchema]]
   [:cause/event-id {:optional true} [:maybe pos-int?]]
   [:payload-ref [:maybe [:fn types/artifact-id?]]]
   [:prev-hash [:maybe [:and string? [:re event-hash-re]]]]
   [:event-hash [:and string? [:re event-hash-re]]]
   [:created-at [:fn inst?]]
   [:metadata :map]])

(def AppendRequestSchema
  "The append-event! input contract (E1)."
  [:map {:closed true}
   [:session/id [:fn types/session-id?]]
   [:generation/id string?]
   [:phenotype/id [:fn types/artifact-id?]]
   [:event/type keyword?]
   [:prev/event-id {:optional true} [:maybe pos-int?]]
   [:causal-links {:optional true} [:set CausalLinkSchema]]
   [:cause/event-id {:optional true} [:maybe pos-int?]]
   [:payload-ref {:optional true} [:maybe [:fn types/artifact-id?]]]
   [:metadata {:optional true} :map]
   [:created-at {:optional true} [:fn inst?]]])

(defn validate-event
  "Validate an Event value against EventSchema. Returns the value
  unchanged, or throws :store/event-invalid with a humanized Malli
  explanation in its ex-data."
  [event]
  (if-let [expl (m/explain EventSchema event)]
    (throw (err/error :store/event-invalid
                      "event does not satisfy the public Event contract"
                      {:errors (me/humanize expl)}))
    event))

(defn validate-append-request
  "Validate an append-event! request against AppendRequestSchema.
  Returns the request unchanged, or throws :store/event-invalid with a
  humanized Malli explanation in its ex-data."
  [request]
  (if-let [expl (m/explain AppendRequestSchema request)]
    (throw (err/error :store/event-invalid
                      "append request does not satisfy the event append contract"
                      {:errors (me/humanize expl)}))
    request))
