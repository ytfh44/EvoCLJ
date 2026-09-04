(ns evoclj.capability.constraint
  "Constraint lattice for C3 Lease full algebra.

  Lease = Grant × Principal × TimeWindow × Quota.
  Quota is the product of ConstraintDescriptors — each dimension is a
  bounded numeric quota (max-calls, max-bytes, etc.) with:

    le?   — order: child ≤ parent (more restrictive)
    meet  — greatest lower bound (min for numeric, nil = top)
    consumption — budget check via usage map

  Derive = meet in every dimension (Grant attenuates? + constraint meet +
  TimeWindow meet).  Unknown constraint keys are rejected at the schema
  boundary (closed map) — widening via unknown-field passthrough is
  removed."
  (:require [clojure.set :as set]
            [evoclj.capability.registry :as reg]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol ConstraintDescriptor
  (ckey [this] "Keyword key for this constraint, e.g. :max-calls")
  (cschema [this] "Malli schema for the value")
  (le? [this parent-val child-val] "True when child ≤ parent. nil = unbounded top: any child ≤ nil, nil ≤ finite = false.")
  (meet [this parent-val child-val] "GLB of two values. For numeric quotas this is min; nil = top so meet(nil, v) = v.")
  (consumption-key [this] "Key in usage map, or nil when not usage-tracked")
  (exceeded? [this constraints usage lease-id] "True when usage exceeds the quota for lease-id"))

;; ---------------------------------------------------------------------------
;; Shared helpers
;; ---------------------------------------------------------------------------

(defn- numeric-le?
  "Numeric le? with nil = top (unbounded)."
  [parent child]
  (cond
    (nil? parent) true
    (nil? child) false
    :else (<= child parent)))

(defn- numeric-meet
  "Numeric meet with nil = top."
  [parent child]
  (cond
    (nil? parent) child
    (nil? child) parent
    :else (min parent child)))

;; ---------------------------------------------------------------------------
;; Built-in descriptors
;; ---------------------------------------------------------------------------

(defrecord MaxCallsDescriptor []
  ConstraintDescriptor
  (ckey [_] :max-calls)
  (cschema [_] [:maybe [:and :int [:fn (fn [x] (>= x 0))]]])
  (le? [_ p c] (numeric-le? p c))
  (meet [_ p c] (numeric-meet p c))
  (consumption-key [_] :max-calls)
  (exceeded? [_ constraints usage lease-id]
    (let [max-calls (get constraints :max-calls)
          consumed (get (or usage {}) lease-id 0)]
      (and (some? max-calls) (>= consumed max-calls)))))

(defrecord MaxBytesDescriptor []
  ConstraintDescriptor
  (ckey [_] :max-bytes)
  (cschema [_] [:maybe [:and :int [:fn (fn [x] (>= x 0))]]])
  (le? [_ p c] (numeric-le? p c))
  (meet [_ p c] (numeric-meet p c))
  (consumption-key [_] :max-bytes)
  (exceeded? [_ constraints usage lease-id]
    ;; usage for bytes would be keyed differently in practice; we reuse same usage map
    ;; but with lease-id -> bytes consumed. For now use same numeric check.
    (let [max-bytes (get constraints :max-bytes)
          consumed (get (or usage {}) lease-id 0)]
      (and (some? max-bytes) (>= consumed max-bytes)))))

;; Alias descriptor for camelCase :maxBytes -> same lattice as :max-bytes
;; We canonicalize :maxBytes to :max-bytes at validation time; descriptor still uses :max-bytes.
(defrecord MaxBytesAliasDescriptor []
  ConstraintDescriptor
  (ckey [_] :maxBytes)
  (cschema [_] [:maybe [:and :int [:fn (fn [x] (>= x 0))]]])
  (le? [_ p c] (numeric-le? p c))
  (meet [_ p c] (numeric-meet p c))
  (consumption-key [_] :maxBytes)
  (exceeded? [_ constraints usage lease-id]
    (let [max-bytes (get constraints :maxBytes)
          consumed (get (or usage {}) lease-id 0)]
      (and (some? max-bytes) (>= consumed max-bytes)))))

;; ---------------------------------------------------------------------------
;; Registry — sealed closed installation, modular definition (C3)
;; ---------------------------------------------------------------------------

(def ^:private builtin-descriptors
  "The built-in Constraint descriptors installed at boot (definition)."
  [(->MaxCallsDescriptor)
   (->MaxBytesDescriptor)])

(defn- descriptor-key
  "Registry key for a descriptor: its (ckey)."
  [d] (ckey d))

(defn- validate-descriptor
  "Throw :capability/invalid-descriptor unless d satisfies the protocol and
  keys to a keyword."
  [d]
  (when-not (satisfies? ConstraintDescriptor d)
    (throw (err/error :capability/invalid-descriptor
                      "descriptor must satisfy ConstraintDescriptor"
                      {:value (try (str (type d)) (catch Exception _ "unknown"))})))
  (let [k (ckey d)]
    (when-not (keyword? k)
      (throw (err/error :capability/invalid-descriptor
                        "descriptor key must be a keyword"
                        {:key k})))))

(defn build-registry
  "Build an UNSEALED registry seeded with `descriptors` (each validated and
  keyed by this namespace's rule). This is the support/test entry point for
  constructing a SEPARATE registry holding custom descriptors — to be threaded
  via `binding` *registry* — without mutating the sealed global. Grow it with
  evoclj.capability.registry/add!, freeze it with seal-registry!."
  [descriptors]
  (reg/build-registry descriptor-key validate-descriptor descriptors))

(defonce ^{:dynamic true
           :doc "The sealed global Constraint registry (boot -> build -> seal at load).

  Lattice and budget decisions (le?/meet/exceeded?) read this sealed registry:
  once sealed, no constraint descriptor can be added or removed, so a lease's
  quota semantics cannot change for the execution lifetime. Tests that need a
  custom descriptor build a SEPARATE registry via
  evoclj.capability.registry/build-registry and `binding` it here."}
  *registry*
  (reg/seal-registry!
   (reg/build-registry descriptor-key validate-descriptor builtin-descriptors)))

(defn get-descriptor
  "Get descriptor for constraint key, or nil (from the active registry)."
  [k] (reg/get-descriptor *registry* k))

(defn all-descriptors
  "Map of key -> descriptor (from the active registry)."
  [] (reg/all-descriptors *registry*))

(defn allowed-keys
  "Set of registered constraint keywords (from the active registry)."
  [] (reg/allowed-keys *registry*))

;; Canonicalization: :maxBytes -> :max-bytes
(defn canonicalize-constraints
  "Canonicalize constraint map keys: :maxBytes -> :max-bytes. Returns new map."
  [m]
  (when (map? m)
    (let [has-alias (contains? m :maxBytes)
          has-canonical (contains? m :max-bytes)]
      (cond
        (and has-alias has-canonical)
        ;; both present -> keep canonical, drop alias (canonical wins for meet)
        (dissoc m :maxBytes)
        has-alias
        (-> m (assoc :max-bytes (:maxBytes m)) (dissoc :maxBytes))
        :else m))))

(defn canonicalize-constraints-with-audit
  "Canonicalize but preserve audit keys :cap/attenuated-from and :attenuated-from."
  [m]
  (canonicalize-constraints m))

;; ---------------------------------------------------------------------------
;; Lattice operations over full constraint maps
;; ---------------------------------------------------------------------------

(def ^:private audit-keys
  #{:cap/attenuated-from :attenuated-from})

(defn- quota-keys
  "All quota keys excluding audit keys."
  [m]
  (set/difference (set (keys m)) audit-keys))

(defn known-keys
  "Set of known quota keys plus audit keys."
  []
  (set/union (allowed-keys) audit-keys))

(defn validate-constraints!
  "Throw :capability/schema-invalid if m contains unknown keys or invalid values.
  Audit keys are exempt from quota validation but must be uuids when present."
  [m]
  (when-not (map? m)
    (throw (err/error :capability/schema-invalid "constraints must be a map" {:value (err/sanitize m)})))
  (let [canon (canonicalize-constraints m)]
    ;; check unknown keys
    (doseq [k (keys canon)]
      (when-not (contains? (known-keys) k)
        (throw (err/error :capability/schema-invalid (str "unknown constraint key: " k) {:value (err/sanitize m) :key k}))))
    ;; validate audit keys are uuids when present
    (doseq [k audit-keys]
      (when (contains? canon k)
        (let [v (get canon k)]
          (when-not (uuid? v)
            (throw (err/error :capability/schema-invalid (str "audit key " k " must be uuid") {:value (err/sanitize v)}))))))
    ;; validate quota values are non-negative ints or nil (absent)
    (doseq [[k d] (all-descriptors)]
      (when (contains? canon k)
        (let [v (get canon k)]
          (when-not (or (nil? v) (and (int? v) (>= v 0)))
            (throw (err/error :capability/schema-invalid (str "constraint " k " must be non-negative int or nil") {:value (err/sanitize v)}))))))
    canon))

(defn le-constraints?
  "True when child constraints ≤ parent constraints in every quota dimension.
  Audit keys are ignored. Canonicalizes :maxBytes alias."
  [parent child]
  (let [p (canonicalize-constraints (or parent {}))
        c (canonicalize-constraints (or child {}))
        all-ks (set/union (quota-keys p) (quota-keys c) (allowed-keys))]
    (every? (fn [k]
              (let [d (get-descriptor k)]
                (if d
                  (le? d (get p k) (get c k))
                  ;; unknown quota key after canonicalization should not happen if validate called,
                  ;; but fail-closed: unknown child widening -> false
                  (if (contains? c k) false true))))
            all-ks)))

(defn meet-constraints
  "GLB of two constraint maps. Returns merged map with min per quota dimension.
  Audit keys are not merged — caller must assoc audit chain. Returns map with
  canonical keys."
  [a b]
  (let [ca (canonicalize-constraints (or a {}))
        cb (canonicalize-constraints (or b {}))
        quota-ks (set/union (quota-keys ca) (quota-keys cb) (allowed-keys))
        merged (reduce (fn [acc k]
                         (if-let [d (get-descriptor k)]
                           (let [pv (get ca k) cv (get cb k)
                                 mv (meet d pv cv)]
                             (if (some? mv)
                               (assoc acc k mv)
                               acc))
                           acc))
                       {}
                       quota-ks)]
    merged))

(defn within-budget?
  "True when lease's quotas are not exceeded given usage map.
  Checks every registered descriptor's exceeded? . Audit keys ignored."
  [constraints usage lease-id]
  (let [canon (canonicalize-constraints (or constraints {}))]
    (every? (fn [[_ d]]
              (not (exceeded? d canon usage lease-id)))
            (all-descriptors))))

(defn constraints->actions
  "Legacy helper — not used. Kept for symmetry."
  [m] m)

