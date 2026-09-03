(ns evoclj.promotion.monitor
  "component — evaluate online canary guardrails and automatic stop.

  Two pure/deciding halves and one persistence half:

  1. `decide` — the ONLINE GUARDRAIL EVALUATOR: takes the observed
     session metrics for the canary generation plus the configured
     thresholds and returns a decision map:

         {:stop? bool
          :reason (or nil
                     {:guardrail <kw> :kind :hard|:soft
                      :detail {...}})
          :evidence {:samples n
                     :aggregates {...}
                     :observations [...]}}

     Guardrail classification (normative, component):

         hard policy violation   → HARD (Step 1): ONE violation stops
                                   immediately. Hard violations are
                                   NEVER aggregated into a compensating
                                   score (Global Constraint 14), so a
                                   single violating observation stops
                                   the canary even far below
                                   :min-samples — and the hard reason
                                   always wins over any soft reason.
         provider denial surge   → SOFT (Step 2): a rate over the
                                   observation window
         session failure rate    → SOFT (Step 2)
         cost/task               → SOFT (Step 2)
         latency/task            → SOFT (Step 2)
         operator escalation     → SOFT (Step 2)

     SOFT guardrails act ONLY once `:min-samples` observations are in
     hand (a noisy small sample must not trigger a stop), and only
     when the observed aggregate STRICTLY exceeds the threshold. The
     soft checks run in the canonical order
     :failure-rate → :cost-per-task → :latency-per-task →
     :provider-denial-rate → :operator-escalation-rate; the first
     exceeded guardrail is the reason.

  2. `deactivate-canary` — the ROUTING EFFECT of a stop: the same
     deployment state with :active? false (component reads :active?
     only), so FUTURE sessions route to the current generation while
     already-created sessions stay pinned to the generation they were
     created under (the pin lives in the store row — component Step 2,
     Global Constraint 2). Stopping NEVER rewrites an existing
     session's generation.

  3. `stop-canary!` — the PERSISTENCE half: records the stop as
     promotion evidence. A stop record is appended to the operator
     session's append-only log as a :promotion/canary-stopped event
     whose :payload-ref points at a CAS metrics artifact holding the
     full evidence pack (reason + observed metrics + aggregates) —
     Global Constraint 21: rows reference bodies, they never duplicate
     them. The event's :metadata carries the small stop reason map,
     the profile, the stopped canary generation, the sample count, and
     the artifact reference. Already-running candidate sessions are
     then handled per profile:

         :cancel — each running candidate session's Work is driven to
             :cancelled via evoclj.store.work's compare-and-set (W2: Work
             is the sole durable lifecycle — the session row is never
             transitioned); a session with no active (queued/running/
             waiting) Work is recorded :skipped with its terminal Work
             state, so a single stop cannot be aborted by a racing worker.
         :finish — running candidate sessions are left running.

     The per-session outcome is returned so the caller can audit what
     happened to every already-running candidate session.

  ERROR CONTRACT (Global Constraint 22 — plain serializable data):
  :promotion/monitor-invalid — a malformed observation, threshold
  map, decision map, stop system, or profile (closed-map trust
  boundaries); :promotion/canary-stop-invalid — stop-canary! called
  with a decision whose :stop? is not true (a stop must be earned);
  :store/session-not-found / :promotion/event-anchor-missing — the
  operator anchor session is missing or carries no root event (the
  host's job, exactly as evoclj.promotion.promote documents)."
  (:require [clojure.edn :as edn]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
             [evoclj.promotion.canary :as canary]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session]
            [evoclj.store.work :as work-store]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)))

;; --- boundary validation ------------------------------------------------------

(def ^:private fraction?
  "A fraction in [0, 1]. (Raw function children inside :and do not
  compile in this malli version, so predicates are wrapped in explicit
  [:fn ...] — the evoclj.promotion.canary pattern.)"
  [:and number? [:fn (fn [x] (<= 0.0 (double x) 1.0))]])

(def ^:private nonneg-number?
  "A non-negative number."
  [:and number? [:fn (fn [x] (<= 0.0 (double x)))]])

(def ObservedMetricSchema
  "One observed session metric for the canary generation (closed).
  :hard-violations is a vector of violation maps — ANY non-empty
  vector is a hard stop (Step 1); :provider-denials and
  :operator-escalations are per-session counts feeding the soft
  rates; :cost/task and :latency/task are the per-task cost and
  latency of the session. All optional keys default to their neutral
  value in `normalize-observation`."
  [:map {:closed true}
   [:session/id [:fn types/session-id?]]
   [:outcome [:enum :ok :failed :cancelled]]
   [:hard-violations {:optional true} [:sequential :map]]
   [:provider-denials {:optional true} int?]
   [:operator-escalations {:optional true} int?]
   [:cost/task nonneg-number?]
   [:latency/task nonneg-number?]])

(def CanaryThresholdsSchema
  "The configured soft-threshold map (closed). :min-samples is the
  minimum observation count before ANY soft guardrail may act (Step
  2); the rest are the per-guardrail limits."
  [:map {:closed true}
   [:min-samples [:and int? [:fn (fn [x] (pos? x))]]]
   [:failure-rate fraction?]
   [:cost-per-task nonneg-number?]
   [:latency-per-task nonneg-number?]
   [:provider-denial-rate fraction?]
   [:operator-escalation-rate fraction?]])

(def StopDecisionSchema
  "The decision map `decide` returns and `stop-canary!` consumes
  (closed). :detail carries the guardrail-specific evidence."
  [:map {:closed true}
   [:stop? boolean?]
   [:reason [:maybe [:map {:closed true}
                     [:guardrail keyword?]
                     [:kind [:enum :hard :soft]]
                     [:detail :map]]]]
   [:evidence [:map {:closed true}
               [:samples int?]
               [:aggregates :map]
               [:observations [:sequential :map]]]]])

(def MonitorSystemSchema
  "The stop-canary! system contract (closed): the store (SQLite +
  CAS), the operator session anchoring the :promotion/canary-stopped
  event (its pinned generation becomes the event's :generation/id —
  mirroring evoclj.promotion.promote), the stopped canary generation,
  and the already-running candidate session ids."
  [:map {:closed true}
   [:store [:map {:closed true}
            [:sqlite any?]
            [:cas any?]]]
   [:event/session-id [:fn types/session-id?]]
   [:canary/generation string?]
   [:running/session-ids [:sequential [:fn types/session-id?]]]])

(defn- monitor-error!
  "Throw :promotion/monitor-invalid with a humanized Malli
  explanation."
  [kind expl]
  (throw (err/error :promotion/monitor-invalid
                    (str kind " does not satisfy the online monitor contract")
                    {:errors (me/humanize expl)})))

(defn- validate-thresholds!
  [thresholds]
  (when-let [expl (m/explain CanaryThresholdsSchema thresholds)]
    (monitor-error! "thresholds" expl))
  thresholds)

(defn- validate-observation!
  [observation]
  (when-let [expl (m/explain ObservedMetricSchema observation)]
    (monitor-error! "observed metric" expl))
  observation)

(defn- validate-decision!
  [decision]
  (when-let [expl (m/explain StopDecisionSchema decision)]
    (monitor-error! "decision" expl))
  decision)

(defn- validate-system!
  [system]
  (when-let [expl (m/explain MonitorSystemSchema system)]
    (throw (err/error :promotion/system-invalid
                      "stop system does not satisfy the online monitor contract"
                      {:errors (me/humanize expl)})))
  system)

(defn- normalize-observation
  "Fill the neutral defaults for the optional observation keys."
  [observation]
  (merge {:hard-violations []
          :provider-denials 0
          :operator-escalations 0}
         observation))

;; --- aggregation (pure) -------------------------------------------------------

(defn- aggregates
  "The observed aggregates over `obs`: counts, rates, and means. The
  means are over the (required) :cost/task and :latency/task of every
  observation; rates are over the sample count."
  [obs]
  (let [n (count obs)
        safe (max n 1)
        failures (count (filter #(= :failed (:outcome %)) obs))
        denials (reduce + 0 (map :provider-denials obs))
        escalations (reduce + 0 (map :operator-escalations obs))
        mean (fn [k] (double (/ (reduce + 0.0 (map k obs)) safe)))]
    {:samples n
     :failures failures
     :failure-rate (double (/ failures safe))
     :cost-per-task (mean :cost/task)
     :latency-per-task (mean :latency/task)
     :provider-denials denials
     :provider-denial-rate (double (/ denials safe))
     :operator-escalations escalations
     :operator-escalation-rate (double (/ escalations safe))}))

(def ^:private soft-order
  "The canonical order soft guardrails are evaluated in; the first
  exceeded guardrail names the stop reason."
  [:failure-rate :cost-per-task :latency-per-task
   :provider-denial-rate :operator-escalation-rate])

(defn- soft-violation
  "The first exceeded SOFT guardrail, or nil. Every soft guardrail
  requires samples >= :min-samples (Step 2 — a noisy small sample must
  not trigger) and a STRICTLY exceeded threshold."
  [obs thresholds]
  (let [n (count obs)
        aggs (aggregates obs)]
    (when (>= n (:min-samples thresholds))
      (first (keep (fn [guardrail]
                     (let [observed (double (get aggs guardrail))
                           limit (double (get thresholds guardrail))]
                       (when (> observed limit)
                         {:guardrail guardrail
                          :kind :soft
                          :detail {:observed observed
                                   :threshold limit
                                   :samples n}})))
                   soft-order)))))

;; --- Step 1: the hard gate (not aggregated) -----------------------------------

(defn- hard-violation
  "The FIRST hard violation across the observations, as a reason
  detail: {:session/id <sid> :violation <the violation map>}. Hard
  violations are not aggregated (Global Constraint 14): the first
  observation carrying a non-empty :hard-violations vector stops."
  [obs]
  (some (fn [o]
          (when-let [v (first (:hard-violations o))]
            {:session/id (:session/id o) :violation v}))
        obs))

;; --- the public decision ------------------------------------------------------

(defn decide
  "The online canary guardrail evaluation (Steps 1-2): observed
  session metrics for the canary generation + thresholds → a decision
  map {:stop? bool :reason (or nil {...}) :evidence {...}}.

  Step 1 first: ONE hard safety violation stops immediately — the
  first observation carrying a non-empty :hard-violations vector names
  the reason, regardless of sample count (hard violations are not
  aggregated — Global Constraint 14). Step 2: otherwise the SOFT
  guardrails (session failure rate, cost/task, latency/task, provider
  denial rate, operator escalation rate) act only when
  samples >= :min-samples and the observed aggregate strictly exceeds
  the threshold; the first exceeded guardrail in the canonical order
  names the reason. Below :min-samples, or with nothing exceeded, the
  decision is {:stop? false :reason nil}.

  The :evidence map always carries :samples, the computed
  :aggregates, and the normalized :observations, so a stop's rationale
  is fully auditable (Step 4 persistence consumes this verbatim).

  Throws :promotion/monitor-invalid for a malformed observation or
  threshold map (closed trust boundaries, Global Constraint 22)."
  [observed-metrics thresholds]
  (validate-thresholds! thresholds)
  (let [obs (mapv (comp normalize-observation
                        (fn [o] (validate-observation! o)))
                  observed-metrics)
        evidence {:samples (count obs)
                  :aggregates (aggregates obs)
                  :observations obs}]
    (if-let [detail (hard-violation obs)]
      {:stop? true
       :reason {:guardrail :hard-policy-violation
                :kind :hard
                :detail detail}
       :evidence evidence}
      (if-let [reason (soft-violation obs thresholds)]
        {:stop? true :reason reason :evidence evidence}
        {:stop? false :reason nil :evidence evidence}))))

;; --- Step 3: the routing effect (future sessions only) -------------------------

(defn deactivate-canary
  "The ROUTING EFFECT of a stop (Step 3): the same deployment state
  with the canary deactivated (:active? false) — component's routing
  reads :active? only, so every NEW session now routes to
  :current-generation while the canary generation is never selected.
  Already-created sessions are untouched: their pin lives in the store
  row (component Step 2, Global Constraint 2). nil deployment state
  stays nil (no canary information to deactivate). Pure."
  [deployment-state]
  (when deployment-state
    (assoc deployment-state :active? false)))

;; --- Step 4: persistence of the stop record ------------------------------------

(defn- read-event-anchor!
  "The operator anchor session's pinned generation and its newest
  event (its :session/created root — :promotion/canary-stopped is not
  a root event, so it must chain to an earlier event in the same
  session). Fails loudly when either is missing, so a stop can never
  commit without an appendable anchor (the promote! pattern)."
  [db session-key]
  (let [sess (first (sqlite/query db
                                  ["SELECT generation_id, phenotype_id FROM sessions WHERE id = ?"
                                   session-key]))]
    (when-not sess
      (throw (err/error :store/session-not-found
                        "cannot anchor the canary-stop event to an unknown operator session"
                        {:session/id session-key})))
    (let [newest (first (sqlite/query db
                                      ["SELECT MAX(id) AS id FROM events WHERE session_id = ?"
                                       session-key]))]
      (when (nil? (:id newest))
        (throw (err/error :promotion/event-anchor-missing
                          "the operator session must carry its :session/created root event first"
                          {:session/id session-key})))
      {:generation/id (:generation_id sess)
       :phenotype/id (:phenotype_id sess)
       :prev/event-id (:id newest)})))

(defn- cancel-session!
  "The :cancel profile action for ONE already-running candidate session:
  drive its active Work (queued/running/waiting) to :cancelled via
  evoclj.store.work/cancel-work! (W2 — Work is the sole durable lifecycle;
  the session row is never transitioned). A session whose Work already
  reached another terminal state is recorded :skipped with that Work state
  (never an aborted stop); a missing session is recorded :missing."
  [db session-id reason]
  (let [sid (types/session-id session-id)]
    (if-not (session/get-session db sid)
      {:session/id sid :action :missing}
      (let [works (work-store/list-works db sid)
            active (filter #(contains? #{:queued :running :waiting} (:work/state %)) works)]
        (if (seq active)
          (do (doseq [w active]
                (try (work-store/cancel-work! db (:work/id w))
                     (catch Exception _ nil)))
              {:session/id sid :action :cancelled})
          {:session/id sid :action :skipped
           :actual-state (some-> (last works) :work/state)})))))

(defn stop-canary!
  "Record the stop of the canary generation as promotion evidence and
  handle the already-running candidate sessions per `profile`.

  Evidence (Step 4): the full evidence pack
  {:stop/reason <reason> :stop/evidence <evidence>} is written to the
  CAS as a metrics artifact (:payload-ref, Global Constraint 21), and
  a :promotion/canary-stopped event is appended to the operator
  session's log (anchored exactly like promote! — the event's
  :generation/id is the operator session's pinned generation, its
  :cause is the session's newest event, its :payload-ref is the
  artifact, and its :metadata carries {:canary/generation <gen>
  :reason <reason> :profile <profile> :samples n :metrics-ref <id>}).

  Routing (Step 3): the returned :routing {:canary-active? false} is
  the routing effect for FUTURE sessions only; callers apply it via
  `deactivate-canary` on their deployment state. Existing sessions are
  never rewritten.

  Running sessions (Step 3): :cancel transitions each :running
  candidate session to :cancelled through the store (sessions no
  longer :running are recorded :skipped with their actual state);
  :finish leaves them running. Every session's outcome is returned in
  :running/actions.

  Returns {:stop/event <event> :metrics/artifact {:artifact/id ...}
  :running/actions [...] :routing {:canary-active? false}}.

  Throws :promotion/system-invalid (malformed system),
  :promotion/monitor-invalid (malformed decision or profile),
  :promotion/canary-stop-invalid (decision :stop? is not true — a
  stop must be earned by `decide`), :store/session-not-found or
  :promotion/event-anchor-missing (the operator anchor session is
  missing or has no root event)."
  [system decision profile]
  (validate-system! system)
  (validate-decision! decision)
  (when-not (contains? #{:finish :cancel} profile)
    (throw (err/error :promotion/monitor-invalid
                      "profile must be :finish or :cancel"
                      {:profile profile})))
  (when-not (true? (:stop? decision))
    (throw (err/error :promotion/canary-stop-invalid
                      "stop-canary! requires a decision whose :stop? is true"
                      {:stop? (:stop? decision)})))
  (let [db (get-in system [:store :sqlite])
        cas-config (get-in system [:store :cas])
        session-key (str (:event/session-id system))
        canary-gen (:canary/generation system)
        reason (:reason decision)
        evidence (:evidence decision)
        ;; Step 4: the metrics artifact (the full evidence pack; rows
        ;; reference bodies — Global Constraint 21)
        artifact (cas/put-bytes! cas-config
                                 (.getBytes (pr-str {:stop/reason reason
                                                     :stop/evidence evidence})
                                            StandardCharsets/UTF_8)
                                 {:media-type "application/edn"})
        artifact-id (:artifact/id artifact)
        ;; Step 4: the stop record — the :promotion/canary-stopped
        ;; event chained to the operator session's newest event
        anchor (read-event-anchor! db session-key)
        stop-event (event/append-event! db
                                        {:session/id (types/session-id session-key)
                                         :generation/id (:generation/id anchor)
                                         :phenotype/id (:phenotype/id anchor)
                                         :event/type :promotion/canary-stopped
                                         :prev/event-id (:prev/event-id anchor)
                                         :payload-ref artifact-id
                                         :metadata {:canary/generation canary-gen
                                                    :reason reason
                                                    :profile profile
                                                    :samples (:samples evidence)
                                                    :metrics-ref artifact-id}})
        ;; Step 3: handle the already-running candidate sessions
        running-actions (case profile
                          :finish (mapv (fn [sid] {:session/id sid :action :finish})
                                        (:running/session-ids system))
                          :cancel (mapv #(cancel-session! db % reason)
                                        (:running/session-ids system)))]
    {:stop/event stop-event
     :metrics/artifact artifact
     :running/actions running-actions
     :routing {:canary-active? false}}))

;; --- the advance path (healthy-window auto-rollout) ---------------------------

(def AdvanceThresholdsSchema
  "The advance path's threshold contract (closed): `:healthy-window` is
  the consecutive-observation count required before an auto-rollout may
  advance; the remaining keys are the per-guardrail soft limits checked
  (aggregates, exactly as `decide`) over the window."
  [:map {:closed true}
   [:healthy-window [:and int? [:fn (fn [x] (pos? x))]]]
   [:min-samples [:and int? [:fn (fn [x] (pos? x))]]]
   [:failure-rate fraction?]
   [:cost-per-task nonneg-number?]
   [:latency-per-task nonneg-number?]
   [:provider-denial-rate fraction?]
   [:operator-escalation-rate fraction?]])

(def AdvanceSystemSchema
  "The advance-canary! system contract (closed): the store (SQLite + CAS),
  the operator session anchoring the :promotion/canary-advanced event
  (its pinned generation becomes the event's :generation/id, mirroring
  stop-canary! and promote!), and the advanced canary generation."
  [:map {:closed true}
   [:store [:map {:closed true}
            [:sqlite any?]
            [:cas any?]]]
   [:event/session-id [:fn types/session-id?]]
   [:canary/generation string?]])

(defn- validate-advance-thresholds!
  [thresholds]
  (when-let [expl (m/explain AdvanceThresholdsSchema thresholds)]
    (monitor-error! "thresholds" expl))
  thresholds)

(defn- validate-advance-system!
  [system]
  (when-let [expl (m/explain AdvanceSystemSchema system)]
    (throw (err/error :promotion/system-invalid
                      "advance system does not satisfy the online monitor contract"
                      {:errors (me/humanize expl)})))
  system)

(defn- window-healthy?
  "True iff the window observations are continuously healthy: NO hard
  violations in any window observation, AND every soft aggregate
  (failure-rate, cost-per-task, latency-per-task, provider-denial-rate,
  operator-escalation-rate) is STRICTLY BELOW its threshold — exactly
  the `decide` soft semantics, evaluated over the window aggregates."
  [window thresholds]
  (and (nil? (hard-violation window))
       (every? (fn [guardrail]
                 (let [observed (double (get (aggregates window) guardrail))
                       limit (double (get thresholds guardrail))]
                   (< observed limit)))
               soft-order)))

(defn advance-canary!
  "When the canary has been healthy for `:healthy-window` consecutive
  observations, advance its allocation by one ladder rung (via
  `canary/advance-allocation`) and record a :promotion/canary-advanced
  event on the operator session.

  Returns {:deployment-state <updated-state> :event <event-map>}.

  Throws the same error types as stop-canary! for malformed inputs:
  :promotion/monitor-invalid, :promotion/system-invalid."
  [system deployment-state observations thresholds]
  (validate-advance-system! system)
  (validate-advance-thresholds! thresholds)
  (let [db (get-in system [:store :sqlite])
        cas-config (get-in system [:store :cas])
        session-key (str (:event/session-id system))
        canary-gen (:canary/generation system)
        n (:healthy-window thresholds)
        obs (mapv (comp normalize-observation
                      (fn [o] (validate-observation! o)))
                  observations)]
    (if (< (count obs) n)
      ;; Not enough observations in hand — clean no-op: deployment-state
      ;; is returned unchanged and no event is recorded.
      {:deployment-state deployment-state :event nil}
      (let [window (take-last n obs)
            window-aggs (aggregates window)]
        (if (window-healthy? window thresholds)
          (let [updated (canary/advance-allocation deployment-state)
                new-allocation (get-in updated [:canary :allocation])
                ;; the health evidence pack (rows reference bodies — Global
                ;; Constraint 21): the window observations plus aggregates
                artifact (cas/put-bytes! cas-config
                                         (.getBytes (pr-str {:health/observations window
                                                             :health/aggregates window-aggs
                                                             :healthy-window n})
                                                    StandardCharsets/UTF_8)
                                         {:media-type "application/edn"})
                artifact-id (:artifact/id artifact)
                anchor (read-event-anchor! db session-key)
                advance-event (event/append-event! db
                                                   {:session/id (types/session-id session-key)
                                                    :generation/id (:generation/id anchor)
                                                    :phenotype/id (:phenotype/id anchor)
                                                    :event/type :promotion/canary-advanced
                                                    :prev/event-id (:prev/event-id anchor)
                                                    :payload-ref artifact-id
                                                    :metadata {:canary/generation canary-gen
                                                               :allocation new-allocation
                                                               :healthy-window n
                                                               :metrics-ref artifact-id}})]
            {:deployment-state updated :event advance-event})
          ;; Window not healthy — clean no-op.
          {:deployment-state deployment-state :event nil})))))
