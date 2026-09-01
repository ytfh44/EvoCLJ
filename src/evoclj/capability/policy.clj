(ns evoclj.capability.policy
  "Pure authorization policy for the v0 capability broker (component).

  The decision logic is deliberately separated from the broker entry
  point (evoclj.capability.broker): `decide` is a pure function of
  plain data — the lease collection, the requesting principal, the
  canonical resource, the requested action, the decision instant, and
  the per-lease call usage — and performs no I/O of any kind, so
  authorization can be tested without invoking a real provider (component acceptance).

  The v0 decision model:

  - `intent-principal` derives the requesting principal from an intent:
    SessionPrincipal(sid) carrying the intent's own attribution (Global
    Constraint 20). Principal matching is EXACT equality (I2), so a
    sibling session is a different principal and must not match.
    Session pin validation is separate (generation pin, not lease).
    When :session/id is missing (e.g. replay), returns OperatorPrincipal.
  - `intent-action` derives the requested action from the intent
    type. A :intent/tool-call, :intent/model-call, :intent/memory-read,
    and :intent/memory-write each request the single action :invoke —
    the :required-action of every v0 provider descriptor (component;
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
      :capability/principal-mismatch  the grant belongs to another principal
      :capability/principal-mismatch  alias of principal-mismatch
      :capability/expired           the grant window does not cover `now`
      :capability/action-denied     the requested action is not granted
      :capability/scope-denied      the canonical resource is outside the grant
      :capability/budget-exceeded   the lease's :max-calls is exhausted

    A single lease is checked in the FIXED order principal -> window ->
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
  (:require [clojure.set :as set]
            [evoclj.capability.constraint :as cstr]
            [evoclj.capability.grant :as grant]
            [evoclj.capability.lease :as lease]
            [evoclj.capability.resource-kind :as rk]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [malli.core :as m]))
;; --- request derivation ----------------------------------------------------

(defn intent-principal
  "The requesting principal of `intent`: SessionPrincipal(sid) carrying
  exactly the intent's own attribution (Global Constraint 20).
  When :session/id is missing (e.g. replay), returns OperatorPrincipal.
  A lease grants ONE principal; authorization compares principal exactly (I2)."
  [intent]
  (if-let [sid (:session/id intent)]
    {:principal/type :session :session/id sid}
    {:principal/type :operator}))

(defn intent-subject
  "Deprecated alias for intent-principal."
  [intent]
  (intent-principal intent))

(def ^:private v0-actions
  {:intent/tool-call :invoke
   :intent/model-call :invoke
   :intent/memory-read :invoke
   :intent/memory-write :invoke})
;; C1: derived from ResourceKindDescriptor registry (no hardcoded allowlist).
;; Kept as a var for backward compat; prefer (rk/allowed-actions-by-kind).
(def allowed-actions-by-kind
  (rk/allowed-actions-by-kind))

(def ^:private global-allowed-actions
  (apply set/union (vals (rk/allowed-actions-by-kind))))

(defn intent-action
  [intent]
  (get v0-actions (:intent/type intent)))

(defn resolve-action
  [target intent]
  (if-let [ra (:required-action target)]
    ra
    (intent-action intent)))

(defn resolve-target-action
  [target normalized-request intent]
  (let [kind (:kind (:resource normalized-request))]
    (case (:source target)
      :request (or (:action (:resource normalized-request))
                   (:action normalized-request)
                   (intent-action intent))
      :tool (:action normalized-request)
      :intent (intent-action intent))))

;; --- input gate ------------------------------------------------------------

(def ^:private NonNegIntSchema
  [:and :int [:fn (fn [x] (not (neg? x)))]])

(def ^:private UsageSchema
  [:map-of uuid? NonNegIntSchema])

(defn- validate-input!
  [leases principal resource action now usage]
  (when-not (or (nil? leases) (coll? leases))
    (throw (err/error :capability/schema-invalid
                      "leases must be a collection of capability leases"
                      {:value (err/sanitize leases)})))
  (doseq [l leases]
    (schema/validate-lease l))
  (when-not (m/validate schema/PrincipalSchema principal)
    (throw (err/error :capability/schema-invalid
                      "authorization principal must be a valid Principal"
                      {:value (err/sanitize principal)})))
  (when-not (map? resource)
    (throw (err/error :capability/schema-invalid
                      "normalized resource must be a map"
                      {:value (err/sanitize resource)})))
  (when-not (or (nil? action) (keyword? action))
    (throw (err/error :capability/schema-invalid
                      "action must be a keyword or nil"
                      {:value (err/sanitize action)})))
  (when-not (inst? now)
    (throw (err/error :capability/schema-invalid
                      "decision instant must be an #inst"
                      {:value (err/sanitize now)})))
  (when-not (m/validate UsageSchema (or usage {}))
    (throw (err/error :capability/schema-invalid
                      "usage must be a map of :cap/id to non-negative int"
                      {:value (err/sanitize usage)}))))

(defn within-call-budget?
  "C3: true when lease's quotas are not exceeded given usage map.
  Delegates to ConstraintDescriptor exceeded? for each quota dimension.
  Both :max-calls and :max-bytes are checked; nil = unlimited."
  [lease usage]
  (cstr/within-budget? (:constraints lease) usage (:cap/id lease)))
(defn- check-lease
  "Check a single lease against a request via Grant (C2) product order.
  ResourceScope × ActionSet: lease Grant must cover request Grant
  (resource covers? + actions superset).  Preserves the fixed reason order
  principal -> window -> action -> scope -> budget for deterministic denies."
  [lease principal resource action now usage]
  (cond
    (not (lease/principal-matches? lease principal))
    [1 {:decision :deny :reason :capability/principal-mismatch}]

    (not (lease/valid-at? lease now))
    [2 {:decision :deny :reason :capability/expired}]

    ;; Grant-level: check ActionSet first for distinct :action-denied reason,
    ;; then ResourceScope for :scope-denied. This is the decomposition of
    ;; Grant covers? = actions ⊇  ∧  resource covers? .
    (not (grant/action-set-covers? (:actions lease) #{action}))
    [3 {:decision :deny :reason :capability/action-denied}]

    (not (grant/resource-covers? (:resource lease) resource))
    [4 {:decision :deny :reason :capability/scope-denied}]

    ;; Fallback: full Grant covers? as single predicate (kept for completeness;
    ;; the two checks above already partition its failure modes).
    ;; If either dimension failed we already returned; this branch is unreachable
    ;; but guards against future ActionSet/ResourceScope changes.
    (not (grant/covers? {:resource (:resource lease) :actions (:actions lease)}
                         {:resource resource :actions #{action}}))
    [4 {:decision :deny :reason :capability/scope-denied}]

    (not (within-call-budget? lease usage))
    [5 {:decision :deny :reason :capability/budget-exceeded}]

    :else
    [6 {:decision :allow :lease-id (:cap/id lease)}]))

(defn decide
  [leases principal resource action now usage]
  (validate-input! leases principal resource action now usage)
  (if (empty? leases)
    {:decision :deny :reason :capability/missing}
    (let [sorted (sort-by :cap/id leases)
          results (mapv (fn [l]
                          (let [[progress decision] (check-lease l principal resource action now usage)]
                            {:progress progress :decision decision :lease l}))
                        sorted)
          allowed (some (fn [r] (when (= :allow (:decision (:decision r))) r)) results)]
      (if allowed
        (:decision allowed)
        (let [best (apply max-key :progress results)]
          (:decision best))))))
