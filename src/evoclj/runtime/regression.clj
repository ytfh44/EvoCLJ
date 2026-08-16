(ns evoclj.runtime.regression
  "Foundation F6 regression-detection trigger rule (Task A7, alert
  only).

  A data-driven :metric trigger rule that fires when a promoted
  child's paired utility drops below its parent by a threshold within
  a window. The rule is PURE DATA (Global Constraint 22) and
  satisfies the closed TriggerRuleSchema of evoclj.runtime.trigger:

      {:trigger/id <uuid>
       :trigger/name :monitor/regression
       :trigger/kind :metric
       :trigger/metric-name :utility/drop
       :trigger/rule {:threshold <number> :comparator :gt :window <int>}
       :trigger/action :monitor/alert-regression}

  The observed metric is the DROP — parent utility minus child
  utility — aggregated over a synthetic paired-utility series of
  samples:

      {:sample/utility-parent <number> :sample/utility-child <number>}

  `windowed-drop` returns the maximum drop over the last :window
  samples (nil or non-positive window = the whole series, mirroring
  the trigger :window semantics; a child that improves contributes 0,
  so the observation floors at 0.0). Detection delegates to
  evoclj.runtime.trigger: `check-series!` builds a metrics context
  {:utility/drop <max drop in window>}, hands the rule to
  trigger/evaluate, and dispatches any fired rule through the registry
  with trigger/run-actions! — so the rule fires exactly when some
  sample within the window dropped below the parent by more than
  :threshold (:gt).

  The alert action :monitor/alert-regression, wired with
  `register-alert-action!` (which uses trigger/register-action!),
  ONLY appends ONE audit event (:monitor/regression-alert) to the
  session's append-only log through evoclj.store.event/append-event!,
  anchored to the session's pinned generation/phenotype and its newest
  event (the promotion event-anchoring pattern). It performs NO other
  state mutation — no generation, session-state, or CURRENT-pointer
  writes (auto-rollback is Task C2a and lives outside this
  namespace). The handler returns the persisted event as its action
  result; a store failure (e.g. an unknown session) surfaces as an
  :error entry inside run-actions! results, per the trigger isolation
  contract."
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.trigger :as trigger]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util UUID)))

(def drop-metric-name
  "The metric name the regression rule observes: the drop (parent
  utility minus child utility) aggregated over the window."
  :utility/drop)

(def alert-action-id
  "The action id the regression rule targets — an alert ONLY (audit
  event append; no state mutation)."
  :monitor/alert-regression)

(def alert-event-type
  "The audit event type the alert action appends to the append-only
  event log."
  :monitor/regression-alert)

(defn- validate-regression-inputs!
  "Validate the pure-data rule inputs. A drop threshold must be a
  number; a window must be nil (whole series) or a positive int.
  Throws :trigger/invalid otherwise."
  [drop-threshold window]
  (when-not (number? drop-threshold)
    (throw (err/error :trigger/invalid
                      "regression drop-threshold must be a number"
                      {:threshold drop-threshold})))
  (when-not (or (nil? window) (and (int? window) (pos? window)))
    (throw (err/error :trigger/invalid
                      "regression window must be a positive int or nil (whole series)"
                      {:window window})))
  nil)

(defn regression-rule
  "The pure-data regression-detection rule for a `drop-threshold` (a
  number — the minimum parent-minus-child drop that counts as a
  regression) and `window` (a positive int of recent samples, or nil
  for the whole series). Returns a rule map satisfying
  TriggerRuleSchema: kind :metric, metric name :utility/drop,
  comparator :gt, action :monitor/alert-regression. The map contains
  NO functions and round-trips through pr-str / clojure.edn
  read-string. Optional `opts` may override :trigger/id and
  :trigger/name (for stable, externally-managed rule ids). Throws
  :trigger/invalid for a non-number threshold or a non-positive /
  non-int window."
  [drop-threshold window & [opts]]
  (validate-regression-inputs! drop-threshold window)
  (merge {:trigger/id (UUID/randomUUID)
          :trigger/name :monitor/regression
          :trigger/kind :metric
          :trigger/metric-name drop-metric-name
          :trigger/rule (cond-> {:threshold drop-threshold
                                 :comparator :gt}
                          window (assoc :window window))
          :trigger/action alert-action-id}
         opts))

(defn sample-drop
  "The drop of ONE paired-utility sample: parent utility minus child
  utility. Returns nil for a malformed sample (missing or non-numeric
  utility keys)."
  [sample]
  (let [p (:sample/utility-parent sample)
        c (:sample/utility-child sample)]
    (when (and (number? p) (number? c))
      (- p c))))

(defn windowed-drop
  "The maximum observed drop (parent utility minus child utility) over
  the last `window` samples of `samples`; a nil or non-positive
  :window means all samples, and a :window larger than the series
  means all samples (mirroring the trigger :window semantics). A child
  that improves contributes a negative drop, so the observation floors
  at 0.0 — an improving child is never a regression. An empty series
  has no observation and yields 0.0. Throws :trigger/invalid for a
  non-sequential `samples` or a malformed sample (missing or
  non-numeric utility keys)."
  [window samples]
  (when-not (sequential? samples)
    (throw (err/error :trigger/invalid
                      "regression samples must be a sequential collection"
                      {:samples samples})))
  (let [n (count samples)
        scoped (if (and window (pos-int? window) (<= window n))
                 (subvec (vec samples) (- n window))
                 samples)]
    (reduce (fn [mx s]
              (let [p (:sample/utility-parent s)
                    c (:sample/utility-child s)]
                (when-not (and (number? p) (number? c))
                  (throw (err/error :trigger/invalid
                                    "a regression sample must carry numeric :sample/utility-parent and :sample/utility-child"
                                    {:sample s})))
                (max mx (- p c))))
            0.0
            scoped)))

(defn check-series!
  "Evaluate `rule` against a synthetic paired-utility `samples` series
  and dispatch any fired rule through `registry`. Returns
  {:trigger/fired [...] :trigger/actions [...]} exactly as
  trigger/check-events! does, but for a :metric rule over the series:
  the observed drop (max parent-minus-child over the rule's :window,
  see windowed-drop) is fed to trigger/evaluate as the :utility/drop
  metric, so the rule fires exactly when a sample within the window
  dropped below the parent by more than :threshold (:gt). Throws
  :trigger/invalid for a malformed rule, registry argument, or sample
  series, exactly as `evaluate` and `run-actions!` do."
  [registry rule samples]
  (let [drop (windowed-drop (:window (:trigger/rule rule)) samples)
        fired (trigger/evaluate [rule] {:events [] :metrics {drop-metric-name drop}})]
    {:trigger/fired fired
     :trigger/actions (trigger/run-actions! registry fired)}))

(defn- read-anchor!
  "The audit-event anchor for `session-key`: the session's pinned
  :generation/id and :phenotype/id (append-event! enforces the match)
  and the id of its newest event as the causal :cause/event-id —
  mirroring the promotion event-anchoring pattern. Throws
  :store/session-not-found for an unknown session."
  [store session-key]
  (let [sess (first (sqlite/query store
                                  ["SELECT generation_id, phenotype_id FROM sessions WHERE id = ?"
                                   session-key]))
        newest (first (sqlite/query store
                                    ["SELECT MAX(id) AS id FROM events WHERE session_id = ?"
                                     session-key]))]
    (when-not sess
      (throw (err/error :store/session-not-found
                        "cannot anchor the regression audit event to an unknown session"
                        {:session/id session-key})))
    {:generation/id (:generation_id sess)
     :phenotype/id (:phenotype_id sess)
     :cause/event-id (:id newest)}))

(defn- alert-handler
  "The :monitor/alert-regression handler: append ONE audit event
  (:monitor/regression-alert) to `session-key`'s append-only log
  through evoclj.store.event/append-event! and return the persisted
  event as the action result. This is the ONLY effect: no generation,
  session-state, or CURRENT-pointer writes (auto-rollback is Task
  C2a). The event metadata carries the fired rule's id, name, the
  observed metric name and drop, and the fired-at timestamp."
  [store session-key]
  (fn [fired]
    (let [anchor (read-anchor! store session-key)
          ctx (:trigger/context fired)]
      (event/append-event! store
                           {:session/id (types/session-id session-key)
                            :generation/id (:generation/id anchor)
                            :phenotype/id (:phenotype/id anchor)
                            :event/type alert-event-type
                            :cause/event-id (:cause/event-id anchor)
                            :payload-ref nil
                            :metadata {:monitor/rule-id (str (:trigger/id fired))
                                       :monitor/rule-name (:trigger/name fired)
                                       :monitor/metric-name (:metric/name ctx)
                                       :monitor/drop (:metric/value ctx)
                                       :monitor/fired-at (:trigger/fired-at fired)}}))))

(defn register-alert-action!
  "Wire the :monitor/alert-regression action into `registry` (via
  trigger/register-action!) so that a fired regression rule appends
  ONE audit event to the append-only log of `session-id` through
  evoclj.store.event/append-event! and returns the persisted event as
  its action result — no other state mutation. `session-id` may be a
  #uuid or its canonical string. Returns the registry."
  [registry store session-id]
  (trigger/register-action! registry alert-action-id
                            (alert-handler store (str (types/session-id session-id)))))
