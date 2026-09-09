(ns evoclj.control
  "Typed control requests persisted as facts in the append-only event log.

  Control persistence is deliberately not a second store: the event log
  owns ordering and durability, while this namespace owns the control
  payload contract. Acceptance and execution are later facts, not
  mutations of a request event."
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.event :as event]
            [evoclj.store.event-schema :as event-schema]
            [malli.core :as m]
            [malli.error :as me]))

(def ControlTypeSchema
  [:enum :append :redirect :replace-goal :interrupt])

(def ControlScopeSchema
  [:map {:closed true}
   [:session/id [:fn types/session-id?]]
   [:work/id {:optional true} pos-int?]])

(def ControlRequestSchema
  "A command-shaped control request before the log records it."
  [:map {:closed true}
   [:control/id uuid?]
   [:control/type ControlTypeSchema]
   [:control/idempotency-key [:and string? [:fn not-empty]]]
   [:control/scope ControlScopeSchema]
   [:control/payload :map]
   [:control/requested-at [:fn inst?]]])

(def ControlEventSchema
  "The durable control fact carried in an Event metadata map."
  [:map {:closed true}
   [:control/id uuid?]
   [:control/type ControlTypeSchema]
   [:control/idempotency-key [:and string? [:fn not-empty]]]
   [:control/scope ControlScopeSchema]
   [:control/payload :map]
   [:control/requested-at [:fn inst?]]
   [:control/status [:enum :requested :accepted :rejected :applied :superseded]]])

(def AppendControlRequestSchema
  "Event context plus one typed control request."
  [:map {:closed true}
   [:session/id [:fn types/session-id?]]
   [:generation/id string?]
   [:phenotype/id [:fn types/artifact-id?]]
   [:prev/event-id [:maybe pos-int?]]
   [:causal-links {:optional true} [:set event-schema/CausalLinkSchema]]
   [:control ControlRequestSchema]])

(defn- validate!
  [schema value error-type message]
  (if-let [explanation (m/explain schema value)]
    (throw (err/error error-type message
                      {:errors (me/humanize explanation)}))
    value))

(defn validate-control-request
  "Validate a command-shaped control request."
  [request]
  (validate! ControlRequestSchema request
            :control/request-invalid
            "control request does not satisfy the control contract"))

(defn validate-control-event
  "Validate the durable control fact stored in Event metadata."
  [control-event]
  (validate! ControlEventSchema control-event
            :control/event-invalid
            "control event does not satisfy the durable control contract"))

(defn append-control-event!
  "Append a :control/requested fact to the existing Event log.

  `request` is recorded with status :requested. The event log remains
  the only persistence path, including sequence and hash-chain facts."
  [store context request]
  (validate-control-request request)
  (validate! AppendControlRequestSchema
             (assoc context :control request)
             :control/request-invalid
             "control append context does not satisfy the control contract")
  (let [event-request (assoc context
                             :event/type :control/requested
                             :metadata (assoc request :control/status :requested)
                             :causal-links (or (:causal-links context) #{}))
        persisted (event/append-event! store event-request)]
    (validate-control-event (:metadata persisted))
    persisted))

(defn control-event
  "Extract and validate the control fact from a persisted Event."
  [persisted-event]
  (event-schema/validate-event persisted-event)
  (let [control (:metadata persisted-event)]
    (validate-control-event control)
    control))
