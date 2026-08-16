(ns evoclj.runtime.regression
  "Foundation F6 regression-detection trigger rules (Task A7 alert,
  Task C2a auto-rollback).

  A data-driven :metric trigger rule that fires when a promoted
  child's paired utility drops below its parent by a threshold within
  a window. The rule is PURE DATA (Global Constraint 22) and
  satisfies the closed TriggerRuleSchema of evoclj.runtime.trigger:

      {:trigger/id <uuid>
       :trigger/name :monitor/regression
       :trigger/kind :metric
       :trigger/metric-name :utility/drop
       :trigger/rule {:threshold <number> :comparator :gt :window <int>}
       :trigger/action <:monitor/alert-regression | :promotion/auto-rollback>}

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
  :threshold (:gt). Every fired rule's :trigger/context also carries
  :monitor/samples — the length of the observed series (the
  observation count) — so guarded actions can refuse to act below a
  minimum observation count.

  TWO actions are wired (both via trigger/register-action!, Task C1
  ACL descriptors):

  :monitor/alert-regression (Task A7, `register-alert-action!`) — the
  ALERT: appends exactly ONE audit event (:monitor/regression-alert)
  to the session's append-only log through
  evoclj.store.event/append-event!, anchored to the session's pinned
  generation/phenotype and its newest event (the promotion
  event-anchoring pattern). It performs NO other state mutation — no
  generation, session-state, or CURRENT-pointer writes.

  :promotion/auto-rollback (Task C2a, `register-rollback-action!`) —
  the ROLLBACK: when the fired rule carries an observation count at
  or above the :min-samples guard, it invokes the PUBLIC promotion
  rollback API (evoclj.promotion.rollback/rollback! — the ONLY code
  path that changes CURRENT, Global Constraint 15) to move the
  CURRENT pointer from the promoted child back to its parent
  (selection-only, Global Constraint 18). BELOW the guard — e.g. the
  first data point — the action REFUSES and returns {:rollback
  :skipped ...} with no state change (no knee-jerk rollback on a
  single observation), mirroring the monitor's :min-samples
  soft-guardrail semantics.

  Both handlers return their result as the action result; a handler
  failure (e.g. an unknown session) surfaces as an :error entry
  inside run-actions! results, per the trigger isolation contract."
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.rollback :as rollback]
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

(def auto-rollback-action-id
  "The action id the auto-rollback regression targets: a CAS-safe,
  selection-only rollback of the promoted child back to its parent
  through the PUBLIC promotion API (evoclj.promotion.rollback/rollback!
  — Global Constraint 15: the only code path that changes CURRENT)."
  :promotion/auto-rollback)

(def default-min-samples
  "The default minimum observation count (the guard) before
  :promotion/auto-rollback may act — a knee-jerk rollback on the
  first data point is refused, mirroring the monitor's :min-samples
  soft-guardrail semantics."
  3)

(def default-rollback-reason
  "The default machine-readable :reason carried into the rollback
  request and its :promotion/rollback event."
  :canary-regression)

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
  :trigger/name (for stable, externally-managed rule ids) and
  :trigger/action (e.g. :promotion/auto-rollback, so the SAME pure
  regression rule can drive the guarded rollback action instead of
  the alert). Throws
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
  dropped below the parent by more than :threshold (:gt). Every
  fired rule's :trigger/context also carries :monitor/samples — the
  length of the `samples` series (the observation count) — so
  guarded actions such as :promotion/auto-rollback can refuse to act
  below a minimum observation count. Throws :trigger/invalid for a
  malformed rule, registry argument, or sample series, exactly as
  `evaluate` and `run-actions!` do."
  [registry rule samples]
  (let [drop (windowed-drop (:window (:trigger/rule rule)) samples)
        n (count samples)
        fired (trigger/evaluate [rule] {:events [] :metrics {drop-metric-name drop}})
        ;; every fired rule carries the observation count so guarded
        ;; actions (:promotion/auto-rollback) can refuse below their
        ;; minimum observation count
        fired (mapv #(update % :trigger/context assoc :monitor/samples n) fired)]
    {:trigger/fired fired
     :trigger/actions (trigger/run-actions! registry fired)}))

(defn- read-anchor!
  "The audit-event anchor for `session-key`: the session's pinned
  :generation/id, :phenotype/id, and :resolution/id (the rollback
  promotion-system needs a valid resolution; the alert event ignores
  it) and the id of its newest event as the causal :cause/event-id —
  mirroring the promotion event-anchoring pattern. Throws
  :store/session-not-found for an unknown session."
  [store session-key]
  (let [sess (first (sqlite/query store
                                  ["SELECT generation_id, phenotype_id, resolution_id FROM sessions WHERE id = ?"
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
     :resolution/id (:resolution_id sess)
     :cause/event-id (:id newest)}))

(defn- alert-handler
  "The :monitor/alert-regression handler: append ONE audit event
  (:monitor/regression-alert) to `session-key`'s append-only log
  through evoclj.store.event/append-event! and return the persisted
  event as the action result. This is the ONLY effect: no generation,
  session-state, or CURRENT-pointer writes (the auto-rollback is a
  SEPARATE action, :promotion/auto-rollback). The event metadata
  carries the fired rule's id, name, the observed metric name and
  drop, and the fired-at timestamp."
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

;; --- Task C2a: the :promotion/auto-rollback action (guarded) -----------------

(defn- observation-count
  "The observation count carried by `fired`'s :trigger/context — the
  length of the observed series at check time (attached by
  check-series! as :monitor/samples) — or 0 when absent."
  [fired]
  (or (:monitor/samples (:trigger/context fired)) 0))

(defn- validate-rollback-opts!
  "Validate the auto-rollback registration options: :min-samples must
  be a positive int (the observation guard), :reason a keyword, and
  :rollback-fn a function (the public API by default; injectable for
  tests). Throws :trigger/invalid otherwise."
  [opts]
  (when-not (and (int? (:min-samples opts)) (pos? (:min-samples opts)))
    (throw (err/error :trigger/invalid
                      "auto-rollback :min-samples must be a positive int"
                      {:min-samples (:min-samples opts)})))
  (when-not (keyword? (:reason opts))
    (throw (err/error :trigger/invalid
                      "auto-rollback :reason must be a keyword"
                      {:reason (:reason opts)})))
  (when-not (fn? (:rollback-fn opts))
    (throw (err/error :trigger/invalid
                      "auto-rollback :rollback-fn must be a function"
                      {:rollback-fn (:rollback-fn opts)})))
  opts)

(defn- rollback-handler
  "The :promotion/auto-rollback handler: `fired` is a fired regression
  rule whose :trigger/context carries the observed drop and the
  observation count (:monitor/samples, attached by check-series!).
  When the count is at or above the :min-samples guard, the handler
  builds the promotion-system and the rollback request (the operator
  session from registration anchors the :promotion/rollback event; the
  promoted child is :from-generation, its parent :to-generation) and
  invokes the PUBLIC promotion rollback API (rollback/rollback! —
  Global Constraint 15, the only code path that changes CURRENT;
  injectable for tests), returning the API's result. BELOW the guard
  (e.g. the first data point) the action REFUSES and returns
  {:rollback :skipped :samples n :min-samples m
  :reason :observation-guard} with no state change. A store failure
  (e.g. an unknown session) surfaces as an :error entry inside
  run-actions! results, per the trigger isolation contract."
  [store cas-config session-key from-generation to-generation opts]
  (fn [fired]
    (let [n (observation-count fired)
          min-samples (:min-samples opts)]
      (if (< n min-samples)
        {:rollback :skipped
         :samples n
         :min-samples min-samples
         :reason :observation-guard}
        (let [anchor (read-anchor! store session-key)]
          ((:rollback-fn opts)
           {:store {:sqlite store :cas cas-config}
            :resolution/id (:resolution/id anchor)
            :event/session-id (types/session-id session-key)}
           {:from-generation from-generation
            :to-generation to-generation
            :reason (:reason opts)}))))))

(defn register-rollback-action!
  "Wire the :promotion/auto-rollback action into `registry` (via
  trigger/register-action!) so that a fired regression rule performs a
  CAS-safe, selection-only rollback of the promoted
  `from-generation` back to `to-generation` — but ONLY once the
  observation guard is met: `fired` must carry an observation count
  of at least `:min-samples` (default default-min-samples, 3; below
  the guard, e.g. the first data point, the action returns
  {:rollback :skipped ...} and changes nothing). The rollback goes
  through the PUBLIC promotion API
  (evoclj.promotion.rollback/rollback! — Global Constraint 15: the
  only code path that changes CURRENT; selection-only, Global
  Constraint 18), anchored to the operator `session-id` (which must
  exist and carry its :session/created root). `store` is the SQLite
  db and `cas` the CAS root/config. Optional `opts`:
  :min-samples (positive int), :reason (keyword, default
  default-rollback-reason :canary-regression), :rollback-fn (the
  public API by default; injectable for tests). Returns the
  registry."
  ([registry store cas session-id from-generation to-generation]
   (register-rollback-action! registry store cas session-id from-generation to-generation {}))
  ([registry store cas session-id from-generation to-generation opts]
   (when-not (and (string? from-generation) (string? to-generation))
     (throw (err/error :trigger/invalid
                       "auto-rollback generations must be strings"
                       {:from-generation from-generation
                        :to-generation to-generation})))
   (let [opts (merge {:min-samples default-min-samples
                      :reason default-rollback-reason
                      :rollback-fn rollback/rollback!}
                     opts)]
     (validate-rollback-opts! opts)
     (trigger/register-action! registry auto-rollback-action-id
                               (rollback-handler store cas
                                                 (str (types/session-id session-id))
                                                 from-generation to-generation opts)))))
