(ns evoclj.runtime.regression
  "Foundation F6 regression-detection trigger rules (Task A7 alert,
  Task C2a auto-rollback, Task C2b failure-driven case evolution).

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
  (:require [evoclj.eval.dataset :as dataset]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.rollback :as rollback]
            [evoclj.runtime.trigger :as trigger]
            [evoclj.store.enrichment :as enrich]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths)
           (java.time Instant)
           (java.util UUID)))

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

(defn- windowed-samples
  "The samples within the rule's window (nil or non-positive window =
  the whole series; a window larger than the series = all samples)."
  [window samples]
  (let [n (count samples)]
    (if (and window (pos-int? window) (<= window n))
      (subvec (vec samples) (- n window))
      samples)))

(defn- worst-sample
  "The sample with the maximum drop (parent utility minus child
  utility) within `window` — ties resolve to the earliest such sample
  — or nil for an empty series. Callers must have already validated
  the samples (windowed-drop throws on malformed ones)."
  [window samples]
  (let [scoped (windowed-samples window samples)]
    (when (seq scoped)
      (reduce (fn [best s]
                (if (> (sample-drop s) (sample-drop best)) s best))
              (first scoped)
              (rest scoped)))))

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
  below a minimum observation count. When the worst sample within the
  window carries a :sample/input, the fired context additionally
  carries that input as :monitor/failing-input — the input the
  case-evolution action (:monitor/evolve-case, Task C2b) turns into a
  new hidden-dataset case. Optional fourth argument `evidence-ref`
  (the triggering evidence-pack reference) is attached to every fired
  rule's context as :monitor/evidence-ref for the same action. Throws
  :trigger/invalid for a malformed rule, registry argument, or sample
  series, exactly as `evaluate` and `run-actions!` do."
  ([registry rule samples]
   (check-series! registry rule samples nil))
  ([registry rule samples evidence-ref]
   (let [drop (windowed-drop (:window (:trigger/rule rule)) samples)
         n (count samples)
         worst (worst-sample (:window (:trigger/rule rule)) samples)
         attach-input? (and worst (contains? worst :sample/input))
         failing-input (:sample/input worst)
         fired (trigger/evaluate [rule] {:events [] :metrics {drop-metric-name drop}})
         ;; every fired rule carries the observation count so guarded
         ;; actions (:promotion/auto-rollback) can refuse below their
         ;; minimum observation count; the worst sample's input and the
         ;; optional evidence ref feed the case-evolution action (C2b)
         fired (mapv (fn [f]
                       (cond-> (update f :trigger/context assoc :monitor/samples n)
                         attach-input? (update :trigger/context assoc :monitor/failing-input failing-input)
                         (some? evidence-ref) (update :trigger/context assoc :monitor/evidence-ref evidence-ref)))
                     fired)]
     {:trigger/fired fired
      :trigger/actions (trigger/run-actions! registry fired)})))

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

;; --- Task C2b: failure-driven evaluation case evolution ----------------------

(def case-evolution-action-id
  "The action id the case-evolution regression targets: appends ONE
  evaluation case derived from the failing input into the hidden
  (Selection) dataset — append-only, provenance-linked to the
  triggering evidence."
  :monitor/evolve-case)

(def case-origin-kind
  "The derived-metadata :kind attached to an evolved case: the
  enrichment class recording the case's regression origin and its
  evidence cause ref."
  :case/origin)

(def case-evolution-source
  "The :case/source marker carried by every evolved case."
  :regression)

(defn- validate-evolution-inputs!
  "Validate the case-evolution inputs: the failing input must be
  non-nil and the triggering evidence-pack ref a string. Throws
  :regression/invalid otherwise."
  [failing-input evidence-ref]
  (when (nil? failing-input)
    (throw (err/error :regression/invalid
                      "case evolution requires a non-nil failing input"
                      {:failing-input failing-input})))
  (when-not (string? evidence-ref)
    (throw (err/error :regression/invalid
                      "case evolution requires the triggering evidence pack ref as a string"
                      {:evidence-ref evidence-ref})))
  nil)

(defn- canonical-edn
  "Deterministic EDN form for hashing: maps sorted by their pr-str key
  form, sets by their pr-str element form, collections realized eagerly
  (the same convention as evoclj.eval.dataset and
  evoclj.evolution.evidence)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical-edn v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical-edn) x)
    (vector? x) (mapv canonical-edn x)
    (seq? x) (mapv canonical-edn x)
    :else x))

(defn- body-ref
  "The content address of a case :body — the deterministic hash of its
  canonical EDN form (the same canonical convention evoclj.eval.dataset
  uses, at body granularity). The dedup key: a duplicate regression is
  one whose failing input produces the same body address."
  [body]
  (hash/text-digest (pr-str (canonical-edn body))))

(defn- new-evolved-case
  "Build the append-only case map for a confirmed regression: a fresh
  uuid-derived :case/id, the failing input as the case :body (wrapped
  as {:input <failing-input>}), the deterministic :case/body-ref (the
  dedup key), the cause ref to the triggering evidence pack, and the
  :case/source marker."
  [failing-input evidence-ref]
  (let [id (keyword (str "case/regression-" (UUID/randomUUID)))
        body {:input failing-input}]
    {:case/id id
     :body body
     :case/body-ref (body-ref body)
     :case/cause evidence-ref
     :case/source case-evolution-source
     :case/created-at (str (Instant/now))}))

(defn- existing-case-with-body
  "The first case in `cases` whose :body has the same deterministic
  content address as `body` (the dedup key), or nil. The address is
  always recomputed from the stored body, so pre-existing cases without
  a :case/body-ref still dedup correctly."
  [cases body]
  (let [target (body-ref body)]
    (first (filter #(= target (body-ref (:body %))) cases))))

(defn- attach-case-provenance!
  "Attach the versioned enrichment record linking the evolved case to
  its triggering evidence via evoclj.store.enrichment/put-enrichment!:
  :entity/kind :case, :entity/id the case id string, :kind
  :case/origin, :cause the evidence-pack ref (the cause ref), and a
  small :payload recording the regression origin (source, body ref,
  cause). The derived-metadata layer never mutates the case body
  (Global Constraint 21 — the row stores refs, never the body)."
  [store case evidence-ref]
  (enrich/put-enrichment! store
                          {:entity/kind :case
                           :entity/id (str (:case/id case))
                           :kind case-origin-kind
                           :payload {:source case-evolution-source
                                     :body-ref (:case/body-ref case)
                                     :cause evidence-ref}
                           :cause evidence-ref}))

(defn- append-case-file!
  "Append ONE case file to `root`: a fresh .edn filename derived from
  the case id, written as pr-str EDN with LF endings. This is the ONLY
  dataset write and it NEVER touches an existing file (append-only)."
  [root case]
  (let [p (Paths/get (str root java.io.File/separator
                          (name (:case/id case)) ".edn")
                     (make-array String 0))]
    (Files/write p
                 (.getBytes (str (pr-str case) "\n") StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0)))
  case)

(defn evolve-case!
  "Append ONE evaluation case derived from a confirmed regression into
  the hidden (Selection) dataset — append-only and provenance-linked
  to the triggering evidence (Task C2b, Purpose T1b).

  `store` is the executor :stores map {:sqlite ... :cas ...} that
  evoclj.store.enrichment needs for the provenance record. `roots` is
  the dataset source -> root registry (default
  evoclj.eval.dataset/dataset-roots); the case is appended under the
  Selection root (:evals/selection — the informationally isolated,
  kernel-only selection set, Global Constraint 11). `failing-input` is
  the input the confirmed regression failed on (any non-nil EDN
  value); `evidence-ref` is the triggering evidence-pack reference (a
  string, e.g. \"sha256:<64 hex>\").

  APPEND-ONLY: the dedup scan is read-only — an existing case whose
  :body carries the same deterministic content address (:case/body-ref)
  short-circuits to {:case <existing> :duplicate true :appended false}
  with NO write (a duplicate regression never duplicates the case). On
  a miss the write path ONLY creates a NEW .edn file with a fresh
  uuid-derived :case/id; existing files are never modified or deleted.

  PROVENANCE: every evolved case carries :case/cause = evidence-ref
  (the cause ref to the triggering evidence pack) and a versioned
  :case/origin enrichment record (enrichment :cause = evidence-ref) is
  attached through the read-only evoclj.store.enrichment API BEFORE the
  file write — a provenance failure therefore leaves the dataset
  untouched.

  Returns {:case <case map> :duplicate <bool> :appended <bool>}.

  Typed errors: :regression/invalid (nil failing input, non-string
  evidence ref), :enrichment/* and :dataset/* passthrough from the
  store/dataset layers."
  ([store failing-input evidence-ref]
   (evolve-case! store dataset/dataset-roots failing-input evidence-ref))
  ([store roots failing-input evidence-ref]
   (validate-evolution-inputs! failing-input evidence-ref)
   (let [root (str (dataset/dataset-root :evals/selection roots))
         existing (existing-case-with-body (dataset/load-cases root)
                                           {:input failing-input})]
     (if existing
       {:case existing :duplicate true :appended false}
       (let [case (new-evolved-case failing-input evidence-ref)]
         (attach-case-provenance! store case evidence-ref)
         (append-case-file! root case)
         {:case case :duplicate false :appended true})))))

(defn- fired-failing-input
  "The failing input carried by `fired`'s :trigger/context — attached
  by check-series! from the worst sample's :sample/input."
  [fired]
  (get-in fired [:trigger/context :monitor/failing-input]))

(defn- fired-evidence-ref
  "The triggering evidence-pack ref carried by `fired`'s
  :trigger/context (:monitor/evidence-ref, attached by check-series!
  or the monitoring caller)."
  [fired]
  (get-in fired [:trigger/context :monitor/evidence-ref]))

(defn- case-evolution-handler
  "The :monitor/evolve-case handler: append ONE evaluation case derived
  from the confirmed regression into the hidden (Selection) dataset —
  append-only, provenance-linked to the triggering evidence — via
  evolve-case!, and return its {:case ... :duplicate ... :appended ...}
  result as the action result. The failing input and the evidence-pack
  ref are read from the fired rule's :trigger/context
  (:monitor/failing-input / :monitor/evidence-ref); when either is
  absent the action SKIPS with {:case-evolution :skipped :reason ...}
  and changes nothing — a case is never created without its evidence."
  [store roots]
  (fn [fired]
    (let [input (fired-failing-input fired)
          evidence (fired-evidence-ref fired)]
      (cond
        (nil? input) {:case-evolution :skipped
                      :reason :missing-failing-input}
        (nil? evidence) {:case-evolution :skipped
                         :reason :missing-evidence-ref}
        :else (evolve-case! store roots input evidence)))))

(defn register-case-evolution-action!
  "Wire the :monitor/evolve-case action into `registry` (via
  trigger/register-action!) so that a fired regression rule appends ONE
  evaluation case derived from the failing input into the hidden
  (Selection) dataset — append-only, provenance-linked to the
  triggering evidence — and returns the evolve-case! result
  ({:case ... :duplicate ... :appended ...}) as the action result.
  `store` is the executor :stores map and `roots` the dataset roots
  registry (both forwarded to evolve-case!; the case lands under
  :evals/selection). The failing input and evidence-pack ref come from
  the fired rule's :trigger/context (check-series! attaches
  :monitor/failing-input from the worst sample's :sample/input and
  :monitor/evidence-ref from its optional evidence-ref argument); a
  fired rule lacking either is SKIPPED with no state change. Returns
  the registry."
  [registry store roots]
  (trigger/register-action! registry case-evolution-action-id
                            (case-evolution-handler store roots)))
