(ns evoclj.control
  "Typed control requests persisted as facts in the append-only event log.

  Control persistence is deliberately not a second store: the event log
  owns ordering and durability, while this namespace owns the control
  payload contract. Acceptance and execution are later facts, not
  mutations of a request event."
  (:require [clojure.edn :as edn]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.event :as event]
            [evoclj.store.event-schema :as event-schema]
            [evoclj.store.sqlite :as sqlite]
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

(defn- request-matches-event?
  [request persisted-control]
  (= request (dissoc persisted-control :control/status)))

(defn- controls-on-connection
  [conn session-id]
  (mapv (fn [{:keys [id payload]}]
          (let [metadata (try
                           (edn/read-string (or payload "{}"))
                           (catch Exception e
                             (throw (err/error :control/event-invalid
                                               "control event metadata is not readable EDN"
                                               {:event/id id
                                                :cause (.getMessage e)}))))]
            (validate-control-event metadata)
            {:event/id id :control metadata}))
        (sqlite/query-raw! conn
                           "SELECT id, payload FROM events
                            WHERE session_id = ? AND event_type = ?
                            ORDER BY event_seq ASC"
                           [(str (types/session-id session-id))
                            "control/requested"])))

(defn- matching-control
  [controls request]
  (filter (fn [{:keys [control]}]
            (or (= (:control/id request) (:control/id control))
                (= (:control/idempotency-key request)
                   (:control/idempotency-key control))))
          controls))

(defn- replay-or-conflict!
  [controls request]
  (when-let [match (first (matching-control controls request))]
    (if (request-matches-event? request (:control match))
      match
      (throw (err/error :control/idempotency-conflict
                        "control id or idempotency key already names a different request"
                        {:control/id (:control/id request)
                         :control/idempotency-key (:control/idempotency-key request)
                         :existing-event/id (:event/id match)})))))

(defn append-control-event!
  "Append a :control/requested fact to the existing Event log.

  `request` is recorded with status :requested. The event log remains
  the only persistence path, including sequence and hash-chain facts.
  Retries with the same control identity and identical request replay the
  original event; a reused identity with different content is rejected."
  [store context request]
  (validate-control-request request)
  (validate! AppendControlRequestSchema
             (assoc context :control request)
             :control/request-invalid
             "control append context does not satisfy the control contract")
  (when-not (= (types/session-id (:session/id context))
               (types/session-id (get-in request [:control/scope :session/id])))
    (throw (err/error :control/scope-mismatch
                      "control scope must name the event session"
                      {:event/session-id (:session/id context)
                       :control/session-id (get-in request [:control/scope :session/id])})))
  (let [event-request (assoc context
                             :event/type :control/requested
                             :metadata (assoc request :control/status :requested)
                             :causal-links (or (:causal-links context) #{}))
        _ (event-schema/validate-append-request event-request)
        result (sqlite/with-write-tx [conn store]
                 (if-let [existing (replay-or-conflict!
                                    (controls-on-connection conn (:session/id context))
                                    request)]
                   {:replay/event-id (:event/id existing)}
                   {:event (event/append-event-on-conn! conn event-request)}))]
    (if-let [persisted (:event result)]
      (do
        (validate-control-event (:metadata persisted))
        persisted)
      (let [event-id (:replay/event-id result)
            replayed (some #(when (= event-id (:event/id %)) %)
                           (event/events-for-session store (:session/id context)))]
        (when-not replayed
          (throw (err/error :control/replay-missing
                            "idempotent control event disappeared before replay"
                            {:event/id event-id})))
        (validate-control-event (:metadata replayed))
        replayed))))

(defn control-event
  "Extract and validate the control fact from a persisted Event."
  [persisted-event]
  (event-schema/validate-event persisted-event)
  (let [control (:metadata persisted-event)]
    (validate-control-event control)
    control))
