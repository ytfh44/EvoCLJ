(ns evoclj.store.event-schema
  "Malli schemas for the public Event contract (Task 5.3; the `Event`
  contract in docs/implementation-plan.md).

  `AppendRequestSchema` validates what callers may hand to
  evoclj.store.event/append-event!: a session pin (:session/id,
  :generation/id, :phenotype/id), an :event/type from the Event
  Taxonomy, an optional :cause/event-id that must reference an EARLIER
  event in the SAME session (root events excepted — the v0 root set is
  #{:session/created}, defined and documented in
  evoclj.store.event/root-event-types), an optional :payload-ref
  content address (Global Constraint 21: rows reference, they never
  duplicate payloads), small :metadata, and an optional :created-at
  (defaults to now, stored canonicalized as ISO-8601 UTC). Unknown
  keys are rejected: trust boundaries use closed maps.

  `EventSchema` validates the persisted event returned by the store:
  the contract's :event/id, :event/seq, :prev-hash, :event-hash and
  :created-at fields, with :session/id as a #uuid value and
  :created-at as an inst.

  Both validators throw :store/event-invalid carrying a humanized
  Malli explanation."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(def event-hash-re
  "Canonical content-address form of an event hash: sha256:<64 hex>."
  #"^sha256:[0-9a-f]{64}$")

(def EventSchema
  "The persisted Event contract (implementation-plan `Event`)."
  [:map {:closed true}
   [:event/id pos-int?]
   [:event/seq pos-int?]
   [:session/id uuid?]
   [:generation/id string?]
   [:phenotype/id [:fn types/artifact-id?]]
   [:event/type keyword?]
   [:cause/event-id [:maybe pos-int?]]
   [:payload-ref [:maybe [:fn types/artifact-id?]]]
   [:prev-hash [:maybe [:and string? [:re event-hash-re]]]]
   [:event-hash [:and string? [:re event-hash-re]]]
   [:created-at [:fn inst?]]
   [:metadata :map]])

(def AppendRequestSchema
  "The append-event! input contract."
  [:map {:closed true}
   [:session/id [:fn types/session-id?]]
   [:generation/id string?]
   [:phenotype/id [:fn types/artifact-id?]]
   [:event/type keyword?]
   [:cause/event-id [:maybe pos-int?]]
   [:payload-ref [:maybe [:fn types/artifact-id?]]]
   [:metadata :map]
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
