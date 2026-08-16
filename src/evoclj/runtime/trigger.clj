(ns evoclj.runtime.trigger
  "Foundation F6 — data-driven triggers over the append-only event stream.

  The event store (evoclj.store.event) is append-only and hash-chained:
  triggers are NOT written back into the events table. Instead the
  runtime/CLI layer evaluates pure, data-only trigger rules against a
  provided sequence of events (or a metrics context) and fires a
  kernel-side action registry. This namespace supplies BOTH halves of
  that substrate:

    1. The MATCHER — pure rule evaluation over :event rules (count
       events of a type within a window and compare to a threshold) and
       :metric rules (compare a metrics value to a threshold). Rules are
       pure data (Global Constraint 22): they carry no functions, only
       threshold / comparator / window values.

    2. The ACTION REGISTRY — an atom mapping action keywords to handler
       functions. Handlers live ONLY in the registry atom (kernel-side
       state); rule data never contains a function. `run-actions!`
       dispatches each fired rule to its handler with per-fired-rule
       isolation: an unknown action id or a throwing handler produces an
       :error entry without stopping the other actions.

  DATA contract: a trigger rule

      {:trigger/id <uuid>
       :trigger/name <keyword>
       :trigger/kind :event | :metric
       :trigger/event-type <keyword>     ; kind :event
       :trigger/metric-name <keyword>    ; kind :metric
       :trigger/rule {:threshold <number>
                      :comparator :gt|:gte|:lt|:lte|:eq
                      :window <optional int>}
       :trigger/action <keyword>}

  Matching semantics:
    :event  — count events of :trigger/event-type within the last
              :window events of the provided event sequence (default
              window = all events; :window <= 0 is treated as all);
              apply :comparator to (count vs threshold).
    :metric — apply :comparator to (metric-value vs threshold) where the
              value is read from a metrics context map (name-keyword ->
              number).

  A fired rule carries, in :trigger/context, the observed slice: for
  event rules {:event/type ... :count ...}, for metric rules
  {:metric/name ... :metric/value ...}.

  ERROR CONTRACT (Global Constraint 22 — plain serializable data):
  :trigger/invalid — a malformed rule, context, registry argument, or
  non-numeric metric value; :trigger/action-not-found — an unknown
  action id, surfaced ONLY as an :error ENTRY inside run-actions!
  results and never thrown by run-actions!."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)))

(def ^:private fired-timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(def TriggerRuleSchema
  "A single data-only trigger rule (closed trust boundary). :trigger/rule
  is itself closed: threshold is a number, comparator one of
  :gt/:gte/:lt/:lte/:eq, and the optional :window an int."
  [:map {:closed true}
   [:trigger/id uuid?]
   [:trigger/name keyword?]
   [:trigger/kind [:enum :event :metric]]
   [:trigger/event-type {:optional true} keyword?]
   [:trigger/metric-name {:optional true} keyword?]
   [:trigger/rule [:map {:closed true}
                   [:threshold number?]
                   [:comparator [:enum :gt :gte :lt :lte :eq]]
                   [:window {:optional true} int?]]]
   [:trigger/action keyword?]])

(def FiredRuleSchema
  "The fired-rule map `evaluate` returns (closed). :trigger/context is
  a required key carrying the observed slice as a map."
  [:map {:closed true}
   [:trigger/id uuid?]
   [:trigger/name keyword?]
   [:trigger/kind keyword?]
   [:trigger/fired-at string?]
   [:trigger/context :map]])

(def ActionResultSchema
  "One per-fired-rule action result returned by `run-actions!` (closed)."
  [:map {:closed true}
   [:trigger/id uuid?]
   [:trigger/name keyword?]
   [:action/id keyword?]
   [:action/status [:enum :ok :error]]
   [:action/result {:optional true} :any]
   [:error/type {:optional true} keyword?]
   [:error/message {:optional true} string?]])

(defn- trigger-error!
  "Throw :trigger/invalid with a humanized Malli explanation."
  [kind expl]
  (throw (err/error :trigger/invalid
                    (str kind " does not satisfy the trigger contract")
                    {:errors (me/humanize expl)})))

(defn- validate-rule!
  "Validate a rule against TriggerRuleSchema. Returns the rule
  unchanged or throws :trigger/invalid."
  [rule]
  (when-let [expl (m/explain TriggerRuleSchema rule)]
    (trigger-error! "rule" expl))
  rule)

(defn- validate-fn
  "Return the comparator predicate fn for a validated comparator key.
  Only called after :comparator has satisfied the closed enum."
  [comparator]
  (case comparator
    :gt  >
    :gte >=
    :lt  <
    :lte <=
    :eq  =))

(defn- event-rule-count
  "The count of events of `event-type` within the last `window` events of
  `events`. A nil window means all events; :window <= 0 is treated as
  all (per the tae). Unknown event-type yields a count of 0."
  [event-type window events]
  (let [scoped (if (and window (pos-int? window))
                 (let [n (count events)]
                   (if (<= window n)
                     (subvec (vec events) (- n window))
                     events))
                 events)]
    (count (filter #(= event-type (:event/type %)) scoped))))

(defn match-event-rule
  "Evaluate ONE :event trigger rule against a sequential `events`
  collection and return whether it fired (boolean).

  Semantics: count events with :event/type equal to the rule's
  :trigger/event-type within the last :window events of `events`
  (default window = all events; :window <= 0 treated as all), then
  apply the rule's :comparator to (count vs :threshold). An event type
  absent from the sequence yields a count of 0, so an :gt/:gte rule over
  a missing type never fires while an :eq 0 rule fires.

  `events` may be any sequential of maps carrying at least
  :event/type (public Event maps satisfy this). Throws :trigger/invalid
  on a malformed rule (closed schema) — events themselves are not
  schema-validated, an event without :event/type simply does not match."
  [rule events]
  (let [rule (validate-rule! rule)
        kind (:trigger/kind rule)
        rule-map (:trigger/rule rule)
        comparator (validate-fn (:comparator rule-map))]
    (case kind
      :event (let [count (event-rule-count (:trigger/event-type rule)
                                           (:window rule-map)
                                           events)]
               (comparator count (:threshold rule-map)))
      :metric (throw (err/error :trigger/invalid
                                "match-event-rule requires an :event kind rule"
                                {:trigger/kind kind})))))

(defn match-metric-rule
  "Evaluate ONE :metric trigger rule against a numeric `value` and
  return whether it fired (boolean): apply the rule's :comparator to
  (value vs :threshold). Throws :trigger/invalid when the rule is
  malformed or `value` is not a number (a non-numeric metric cannot be
  compared)."
  [rule value]
  (let [rule (validate-rule! rule)]
    (when-not (number? value)
      (throw (err/error :trigger/invalid
                        "metric rule value must be a number"
                        {:value value})))
    (let [rule-map (:trigger/rule rule)
          comparator (validate-fn (:comparator rule-map))]
      (case (:trigger/kind rule)
        :metric (comparator (double value) (:threshold rule-map))
        :event (throw (err/error :trigger/invalid
                                 "match-metric-rule requires a :metric kind rule"
                                 {:trigger/kind (:trigger/kind rule)}))))))

(defn- fired-context
  "The observed slice carried by a fired rule's :trigger/context: for
  event kind, {:event/type <event-type> :count <observed count>}; for
  metric kind, {:metric/name <metric-name> :metric/value <observed
  value>}."
  [rule events metrics]
  (case (:trigger/kind rule)
    :event {:event/type (:trigger/event-type rule)
            :count (event-rule-count (:trigger/event-type rule)
                                     (:window (:trigger/rule rule))
                                     events)}
    :metric {:metric/name (:trigger/metric-name rule)
             :metric/value (get metrics (:trigger/metric-name rule))}))

(defn- fired-at-now
  "Canonical ISO-8601 UTC string for the current instant."
  []
  (.format fired-timestamp-fmt (Instant/now)))

(defn- evaluate-rule
  "Decide one rule against the context. Returns the fired-rule map on a
  match, or nil when the rule does not fire."
  [rule events metrics]
  (let [fired? (case (:trigger/kind rule)
                 :event (let [c (event-rule-count (:trigger/event-type rule)
                                                  (:window (:trigger/rule rule))
                                                  events)
                              comparator (validate-fn (:comparator (:trigger/rule rule)))]
                          (comparator c (:threshold (:trigger/rule rule))))
                 :metric (let [name (:trigger/metric-name rule)
                               comparator (validate-fn (:comparator (:trigger/rule rule)))]
                           (when (contains? metrics name)
                             (let [value (get metrics name)]
                               (when-not (number? value)
                                 (throw (err/error :trigger/invalid
                                                   "metric context value must be a number"
                                                   {:metric/name name
                                                    :value value})))
                               (comparator (double value) (:threshold (:trigger/rule rule)))))))]
    (when fired?
      {:trigger/id (:trigger/id rule)
       :trigger/name (:trigger/name rule)
       :trigger/kind (:trigger/kind rule)
       :trigger/action (:trigger/action rule)
       :trigger/fired-at (fired-at-now)
       :trigger/context (fired-context rule events metrics)})))

(defn evaluate
  "Evaluate `rules` against `context` and return a vector of every
  fired-rule map (in `rules` order):

      {:trigger/id <uuid>
       :trigger/name <keyword>
       :trigger/kind <keyword>
       :trigger/action <keyword>
       :trigger/fired-at <ISO-8601 UTC string>
       :trigger/context <observed slice map>}

  `context` is {:events <sequential of Event maps> :metrics <name-keyword
  -> number map>}. Each rule whose match holds fires; rules that do not
  match are omitted. For :event rules the context slice is
  {:event/type ... :count <observed count>}; for :metric rules it is
  {:metric/name ... :metric/value <observed value>}.

  Throws :trigger/invalid for a malformed rule, a non-sequential
  :events, a non-map :metrics, or a non-numeric metric value in the
  context."
  [rules context]
  (when-not (sequential? (:events context))
    (throw (err/error :trigger/invalid
                      "context :events must be a sequential collection"
                      {:context context})))
  (when-not (or (nil? (:metrics context)) (map? (:metrics context)))
    (throw (err/error :trigger/invalid
                      "context :metrics must be a map"
                      {:context context})))
  (doseq [rule rules]
    (validate-rule! rule))
  (let [events (vec (:events context))
        metrics (or (:metrics context) {})]
    (into []
          (keep #(evaluate-rule % events metrics))
          rules)))

(defn make-registry
  "Return a fresh empty action registry: an atom mapping action-id
  keyword -> handler-fn. Handlers live only here (kernel-side state),
  never inside rule data (Global Constraint 22)."
  []
  (atom {}))

(defn register-action!
  "Register `handler` under `action-id` in `registry` and return the
  registry. `handler` is a function of one argument, the fired-rule
  map, returning an EDN-safe result. Validates: `registry` must be an
  atom, `action-id` a keyword and `handler` a function — else throws
  :trigger/invalid."
  [registry action-id handler]
  (when-not (instance? clojure.lang.Atom registry)
    (throw (err/error :trigger/invalid
                      "registry must be an atom"
                      {:registry registry})))
  (when-not (keyword? action-id)
    (throw (err/error :trigger/invalid
                      "action-id must be a keyword"
                      {:action/id action-id})))
  (when-not (fn? handler)
    (throw (err/error :trigger/invalid
                      "handler must be a function"
                      {:action/id action-id})))
  (swap! registry assoc action-id handler)
  registry)

(defn- run-actions-for-fired
  "Dispatch ONE fired rule to its handler, returning a single action
  result map. Per-fired-rule isolation: an unknown action id or a
  throwing handler produces an :error entry and does not stop anything
  else. A handler's ExceptionInfo ex-data :error/type is preserved when
  present, else :trigger/action-failed."
  [registry fired]
  (let [fired-id (:trigger/id fired)
        fired-name (:trigger/name fired)
        action-id (:trigger/action fired)
        handler (get @registry action-id)]
    (cond
      (nil? handler)
      {:trigger/id fired-id
       :trigger/name fired-name
       :action/id action-id
       :action/status :error
       :error/type :trigger/action-not-found
       :error/message "no handler registered for action"}

      :else
      (try
        {:trigger/id fired-id
         :trigger/name fired-name
         :action/id action-id
         :action/status :ok
         :action/result (handler fired)}
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)
                t (or (:error/type data) :trigger/action-failed)]
            {:trigger/id fired-id
             :trigger/name fired-name
             :action/id action-id
             :action/status :error
             :error/type t
             :error/message (.getMessage e)}))
        (catch Throwable t
          {:trigger/id fired-id
           :trigger/name fired-name
           :action/id action-id
           :action/status :error
           :error/type :trigger/action-failed
           :error/message (.getMessage t)})))))

(defn run-actions!
  "Dispatch every `fired` rule (as returned by `evaluate`) through
  `registry` and return a vector of per-fired-rule action results:

      {:trigger/id <uuid> :trigger/name <keyword> :action/id <keyword>
       :action/status :ok|:error
       :action/result ...  (on :ok)
       :error/type k :error/message s  (on :error)}

  Per-fired-rule isolation: an unknown :trigger/action (no registered
  handler) or a throwing handler produces an :error entry and does NOT
  stop the other actions. A handler exception whose ex-data carries an
  :error/type preserves that type; otherwise the type is
  :trigger/action-failed. An unknown action id yields an :error entry
  with :error/type :trigger/action-not-found — this is never thrown by
  run-actions!."
  [registry fired-rules]
  (when-not (instance? clojure.lang.Atom registry)
    (throw (err/error :trigger/invalid
                      "registry must be an atom"
                      {:registry registry})))
  (when-not (sequential? fired-rules)
    (throw (err/error :trigger/invalid
                      "fired-rules must be a sequential collection"
                      {:fired-rules fired-rules})))
  (mapv #(run-actions-for-fired registry %) fired-rules))

(defn check-events!
  "Convenience: evaluate `rules` over {:events events :metrics {}}
  then dispatch the fired rules through `registry`. Returns
  {:trigger/fired [...] :trigger/actions [...]}. Throws
  :trigger/invalid for a malformed rule, context or registry argument
  exactly as `evaluate` and `run-actions!` do."
  [registry rules events]
  (let [fired (evaluate rules {:events events :metrics {}})]
    {:trigger/fired fired
     :trigger/actions (run-actions! registry fired)}))
