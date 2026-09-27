(ns evoclj.capability.grant
  "Grant partial order — ResourceScope × ActionSet (C2).

  A Grant is a pair of a ResourceScope (a resource map with :kind) and an
  ActionSet (a set of keywords). The order is the product order:

    (r1,a1) ≤ (r2,a2)  iff  r1 covers r2  and  a1 ⊇ a2

  Resource covering is dispatched via ResourceKindDescriptor (C1):
    - filesystem /work covers /work/project-a  (path-inside? with slash boundary)
    - filesystem /work does NOT cover /workish (prefix without separator)
    - tool requires equality, etc.

  ActionSet is the subset lattice; meet is intersection.

  This namespace is pure (no I/O) and fail-closed: unknown kinds, mismatched
  kinds, or malformed inputs yield false/nil rather than throwing, so callers
  can map to :capability/scope-denied etc.  Construction (make-grant) is
  strict and throws :capability/schema-invalid on malformed grants."
  (:require [clojure.set :as set]
            [evoclj.capability.resource-kind :as rk]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Grant record — product type ResourceScope × ActionSet
;; ---------------------------------------------------------------------------

(defrecord Grant [resource actions])

(defn grant?
  "True when x is a Grant record."
  [x]
  (instance? Grant x))

(defn ->grant
  "Coerce a Grant, a plain map {:resource _ :actions _}, or a SEALED
  CapabilityLease to a Grant. Returns nil when the shape is missing.

  A sealed lease is neither a Grant nor a map, so without the third arm
  ->grant returns nil and covers?/attenuates? answer false — indistinguishable
  from a genuine scope denial. Accepting the lease keeps 'this is not a grant
  shape' distinguishable from 'this grant does not cover that'."
  [m]
  (cond
    (instance? Grant m) m
    (schema/lease? m) (let [{:keys [resource actions]} (schema/lease->map m)]
                        (->Grant resource actions))
    (and (map? m) (:resource m) (:actions m)) (->Grant (:resource m) (:actions m))
    :else nil))

(defn make-grant
  "Construct a validated Grant.  `resource` must be a map with :kind keyword,
  `actions` a non-empty set of keywords subset of the descriptor's allowed-actions
  when the kind is registered.  The resource is canonicalized. Throws
  :capability/schema-invalid on failure."
  [resource actions]
  (when-not (and (map? resource) (keyword? (:kind resource)))
    (throw (err/error :capability/schema-invalid
                      "grant resource must be a map with :kind keyword"
                      {:value (err/sanitize resource)})))
  (when-not (and (set? actions) (seq actions) (every? keyword? actions))
    (throw (err/error :capability/schema-invalid
                      "grant actions must be a non-empty set of keywords"
                      {:value (err/sanitize actions)})))
  (let [kind (:kind resource)
        d (rk/get-descriptor kind)]
    (when d
      (let [allowed (rk/allowed-actions d)]
        (when-not (set/subset? actions allowed)
          (throw (err/error :capability/schema-invalid
                            "grant actions must be subset of allowed-actions for kind"
                            {:kind kind :actions (err/sanitize actions) :allowed allowed})))))
    (let [canon (or (rk/canonicalize-resource resource) resource)]
      (->Grant canon actions))))

;; ---------------------------------------------------------------------------
;; ActionSet lattice
;; ---------------------------------------------------------------------------

(defn action-set-covers?
  "True when granted ActionSet superset covers requested ActionSet (⊇)."
  [granted requested]
  (and (set? granted) (set? requested)
       (set/subset? requested granted)))

(defn action-set-meet
  "Greatest lower bound of two ActionSets — intersection. Returns nil when
  intersection is empty (no common action)."
  [a b]
  (when (and (set? a) (set? b))
    (let [m (set/intersection a b)]
      (when (seq m) m))))

;; ---------------------------------------------------------------------------
;; ResourceScope helpers (via descriptor)
;; ---------------------------------------------------------------------------

(defn resource-attenuates?
  "True when parent resource attenuates child resource (via descriptor attenuates?)."
  [parent child]
  (and (map? parent) (map? child)
       (= (:kind parent) (:kind child))
       (boolean (when-let [d (rk/get-descriptor (:kind parent))]
                  (rk/attenuates? d parent child)))))

(defn resource-meet
  "GLB of two resources of the same kind via descriptor meet, or nil."
  [a b]
  (when (and (map? a) (map? b) (= (:kind a) (:kind b)))
    (when-let [d (rk/get-descriptor (:kind a))]
      (rk/meet d a b))))

;; ---------------------------------------------------------------------------
;; Grant order
;; ---------------------------------------------------------------------------

(defn covers?
  "True when granted Grant covers requested Grant.

  Product order: resource covers? AND actions superset (requested ⊆ granted).

  The resource half is rk/covers-resource? — the ONE cover decision. An
  earlier near-duplicate lived here (grant/resource-covers?) and the GC-08
  filesystem-escape tests exercised the resource-kind twin, so the tested
  function was not the one that authorized. Forwarding nil as the action
  preserves the previous behavior: every built-in descriptor ignores it."
  [granted requested]
  (let [g (->grant granted)
        r (->grant requested)]
    (boolean
     (when (and g r
                (map? (:resource g)) (map? (:resource r))
                (set? (:actions g)) (set? (:actions r)))
       (and (= (:kind (:resource g)) (:kind (:resource r)))
            (set/subset? (:actions r) (:actions g))
            (rk/covers-resource? (:resource g) (:resource r) nil))))))

(defn attenuates?
  "True when parent Grant attenuates child Grant (child ≤ parent).

  Product order with descriptor attenuates? for the resource dimension:
    parent attenuates child  iff  resource attenuates? AND actions subset.
  For filesystem, attenuates? == covers? (path-inside); for :tool it requires
  equality (both directions). Fail-closed → false."
  [parent child]
  (let [p (->grant parent)
        c (->grant child)]
    (boolean
     (when (and p c
                (map? (:resource p)) (map? (:resource c))
                (set? (:actions p)) (set? (:actions c)))
       (and (= (:kind (:resource p)) (:kind (:resource c)))
            (set/subset? (:actions c) (:actions p))
            (resource-attenuates? (:resource p) (:resource c)))))))

(defn meet
  "Greatest lower bound of two Grants, or nil when disjoint.

  Resource GLB via descriptor meet; ActionSet GLB via intersection.
  If either dimension is disjoint (resource meet nil or actions intersect empty)
  the whole meet is nil.  Returns a Grant record."
  [a b]
  (let [ga (->grant a)
        gb (->grant b)]
    (when (and ga gb
               (map? (:resource ga)) (map? (:resource gb))
               (= (:kind (:resource ga)) (:kind (:resource gb)))
               (set? (:actions ga)) (set? (:actions gb)))
      (when-let [rm (resource-meet (:resource ga) (:resource gb))]
        (when-let [am (action-set-meet (:actions ga) (:actions gb))]
          ;; The descriptor meet returns whichever PARENT it found more specific,
          ;; so the result is a non-canonical value exactly when the parent was.
          ;; Re-canonicalize: a meet is a value crossing the minting surface
          ;; (derive-lease! seals it), and a non-canonical scope would let two
          ;; mutually-covering unequal meets exist, so the order would be a
          ;; preorder again. The action-vocabulary half needs no separate fix —
          ;; make-lease now rejects a meet whose actions its kind cannot express.
          (->Grant (rk/canonicalize-resource rm) am))))))

