(ns evoclj.capability.policy
  "Pure authorization policy for the v0 capability broker (Task 4.4).

  The decision logic is deliberately separated from the broker entry
  point (evoclj.capability.broker): `decide` is a pure function of
  plain data — the lease collection, the requesting subject, the
  canonical resource, the requested action, the decision instant, and
  the per-lease call usage — and performs no I/O of any kind, so
  authorization can be tested without invoking a real provider (Task
  4.4 acceptance).

  The v0 decision model:

  - `intent-subject` derives the requesting subject from an intent:
    the one-key map {:phenotype/id ...} carrying the intent's own
    attribution (Global Constraint 20). Subject matching is EXACT, so
    a sibling phenotype from the same Genome is a different subject
    and must not match (Global Constraint 9).
  - `intent-action` derives the requested action from the intent
    type. A :intent/tool-call, :intent/model-call, :intent/memory-read,
    and :intent/memory-write each request the single action :invoke —
    the :required-action of every v0 provider descriptor (Task 4.3;
    memory intents landed in feature R1). Any other intent type
    requests nil, and a nil action is never a member of any :actions
    set, so those intents fail closed at the action check.
  - `decide` returns, for one lease collection, a DETERMINISTIC
    decision:

      {:decision :allow :lease-id <cap/id>}
      {:decision :deny :reason <reason>}

    Reason codes are stable and documented (they are part of the
    broker contract):

      :capability/missing           no lease grants this request
      :capability/subject-mismatch  the grant belongs to another phenotype
      :capability/expired           the grant window does not cover `now`
      :capability/action-denied     the requested action is not granted
      :capability/scope-denied      the canonical resource is outside the grant
      :capability/budget-exceeded   the lease's :max-calls is exhausted

    A single lease is checked in the FIXED order subject -> window ->
    action -> resource scope -> call budget, so the reported reason
    for a multiple-fault lease is deterministic. When several leases
    are present they are considered in a deterministic total order
    (sorted by :cap/id): the first lease that passes every check
    allows and its :cap/id is the decision's :lease-id; if no lease
    allows, the reported reason is the reason of the lease that got
    FURTHEST through the check sequence (ties broken by sorted
    order), or :capability/missing when there are no leases at all.
    Reordering the input collection therefore never changes a
    decision, and REMOVING leases can never turn a deny into an
    allow: allow decisions are monotone in the lease set (usage is an
    independent input, so a covering lease cannot be invalidated by
    adding others).

  :usage maps a lease's :cap/id to the number of calls ALREADY
  consumed under it. A :constraints {:max-calls N} lease allows the
  next call iff consumed < N — the decision counts the CURRENT call,
  so a :max-calls 2 lease allows the first two calls and denies the
  third — and an absent :max-calls means no call limit.

  Fail-closed and schema-gated: every decision input is validated
  before any judgment is made; malformed input throws
  :capability/schema-invalid rather than silently granting or
  denying. A capability is a bounded host-owned grant, so garbage
  never authorizes and never hides a caller bug (the same rule as
  evoclj.capability.lease)."
  (:require [evoclj.capability.lease :as lease]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [malli.core :as m]))

;; --- request derivation ----------------------------------------------------

(defn intent-subject
  "The requesting subject of `intent`: the one-key map
  {:phenotype/id ...} carrying exactly the intent's own attribution
  (Global Constraint 20). A lease grants ONE phenotype; authorization
  compares these ids exactly, so a sibling phenotype from the same
  Genome is a different subject and must not match (Global Constraint
  9)."
  [intent]
  {:phenotype/id (:phenotype/id intent)})

(def ^:private v0-actions
  "The v0 action requested by each intent type. :intent/tool-call
  requests the single action :invoke — the :required-action of every
  v0 provider descriptor (Task 4.3); :intent/model-call also requests
  :invoke (post-v0 extension 1 — model leases carry :actions
  #{:invoke}); :intent/memory-read and :intent/memory-write request
  :invoke too (feature R1 — a :memory-kv lease grants the exact
  {:kind :memory :id <key>} resource with :actions #{:invoke}). Every
  other v0 intent type requests nil, which no :actions set contains,
  so those intents fail closed at the action check."
  {:intent/tool-call :invoke
   :intent/model-call :invoke
   :intent/memory-read :invoke
   :intent/memory-write :invoke})

(defn intent-action
  "The action intent requests: :invoke for a v0 :intent/tool-call,
  :intent/model-call, :intent/memory-read, and :intent/memory-write;
  nil for every other v0 intent type. A nil action is never a member
  of any lease's :actions set, so an unknown intent type is never
  granted by a v0 lease (fail closed)."
  [intent]
  (get v0-actions (:intent/type intent)))

;; --- input gate ------------------------------------------------------------

(def ^:private NonNegIntSchema
  "A non-negative integer (usage counts, :max-calls)."
  [:and :int [:fn (fn [x] (not (neg? x)))]])

(def ^:private UsageSchema
  "Per-lease call usage: a map from :cap/id to the number of calls
  already consumed under that lease."
  [:map-of uuid? NonNegIntSchema])

(defn- validate-input!
  "Schema-gate every decision input; throw
  :capability/schema-invalid on any failure so no judgment is ever
  made on malformed data. Leases are validated individually with
  evoclj.capability.schema/validate-lease."
  [leases subject resource action now usage]
  (when-not (or (nil? leases) (coll? leases))
    (throw (err/error :capability/schema-invalid
                      "leases must be a collection of capability leases"
                      {:value (err/sanitize leases)})))
  (doseq [l leases]
    (schema/validate-lease l))
  (when-not (m/validate schema/SubjectSchema subject)
    (throw (err/error :capability/schema-invalid
                      "authorization subject must be {:phenotype/id ...}"
                      {:value (err/sanitize subject)})))
  (when-not (map? resource)
    (throw (err/error :capability/schema-invalid
                      "normalized resource must be a map"
                      {:value (err/sanitize resource)})))
  (when-not (keyword? action)
    (throw (err/error :capability/schema-invalid
                      "requested action must be a keyword"
                      {:value (err/sanitize action)})))
  (when-not (inst? now)
    (throw (err/error :capability/schema-invalid
                      "decision instant must be an #inst value"
                      {:value (err/sanitize now)})))
  (when-not (m/validate UsageSchema usage)
    (throw (err/error :capability/schema-invalid
                      "usage must map :cap/id to non-negative call counts"
                      {:value (err/sanitize usage)}))))

;; --- the decision ----------------------------------------------------------

(defn within-call-budget?
  "True when the lease's call budget admits one more call: an absent
  :max-calls is unlimited; otherwise the calls already consumed under
  the lease (usage[cap/id], defaulting to 0) must be strictly below
  :max-calls — the decision counts the CURRENT call, so a
  :max-calls 2 lease allows the first two calls and denies the
  third."
  [lease usage]
  (let [max-calls (get-in lease [:constraints :max-calls])]
    (or (nil? max-calls)
        (< (get usage (:cap/id lease) 0) max-calls))))

(defn- check-lease
  "Check ONE lease against the request; return [progress decision]
  where progress is how far the lease got through the fixed check
  sequence (1 = subject failed, ..., 6 = every check passed) and
  decision is the corresponding decision map. The check order is
  fixed and documented: subject, window, action, resource scope, call
  budget."
  [lease subject resource action now usage]
  (cond
    (not (lease/subject-matches? lease subject))
    [1 {:decision :deny :reason :capability/subject-mismatch}]

    (not (lease/valid-at? lease now))
    [2 {:decision :deny :reason :capability/expired}]

    (not (contains? (:actions lease) action))
    [3 {:decision :deny :reason :capability/action-denied}]

    (not (lease/resource-covers? lease resource action))
    [4 {:decision :deny :reason :capability/scope-denied}]

    (not (within-call-budget? lease usage))
    [5 {:decision :deny :reason :capability/budget-exceeded}]

    :else
    [6 {:decision :allow :lease-id (:cap/id lease)}]))

(defn decide
  "The pure authorization decision for one request.

  Inputs: `leases` (a collection of CapabilityLease values), the
  requesting `subject` ({:phenotype/id ...}), the canonical
  `resource` (the normalized resource descriptor from provider
  normalize-request, Task 4.3), the requested `action`, the decision
  `now` instant, and `usage` (map of :cap/id to calls consumed).

  Returns {:decision :allow :lease-id ...} when some lease grants the
  request, else {:decision :deny :reason ...} with a deterministic
  reason code (see the namespace docstring). Leases are considered in
  a deterministic total order (sorted by :cap/id), so reordering the
  input never changes the decision; removing leases can never turn a
  deny into an allow. Every input is schema-checked first; malformed
  input throws :capability/schema-invalid."
  [leases subject resource action now usage]
  (validate-input! leases subject resource action now usage)
  (let [ordered (sort-by :cap/id leases)]
    (loop [remaining ordered
           best nil]
      (if-let [l (first remaining)]
        (let [[progress decision] (check-lease l subject resource action now usage)]
          (if (= :allow (:decision decision))
            decision
            (recur (rest remaining)
                   (cond
                     (nil? best) [progress decision]
                     ;; ties keep the earlier (sorted) lease
                     (<= progress (first best)) best
                     :else [progress decision]))))
        (or (some-> best second)
            {:decision :deny :reason :capability/missing})))))
