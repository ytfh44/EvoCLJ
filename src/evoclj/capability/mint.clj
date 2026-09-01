(ns evoclj.capability.mint
  "Unified lease issuance surface (P2).

  mint-lease! is the ONLY place that mints a CapabilityLease (INV-05
  single impl). It delegates validation and sealing to
  evoclj.capability.schema/make-lease (P1's sealed factory) and,
  when a LeaseRegistry atom is supplied, records the sealed lease
  as {:lease lease :revoked? false} so it is verifiable and
  revocable — the same shape as mount/filesystem register-lease!.

  Callers (evolution_tools, mount/filesystem issue-fs-lease,
  cli/session tool-lease/model-lease) must delegate here; grep for
  :cap/id in src/ should only hit this file (plus tests)."
  (:require [clojure.set :as set]
            [evoclj.capability.constraint :as cstr]
            [evoclj.capability.grant :as grant]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err])
  (:import (java.util Date UUID)))

(defn mint-lease!
  "Mint one sealed CapabilityLease and optionally record it in `registry`.

  registry — an atom mapping :cap/id -> {:lease <sealed> :revoked? <bool>}
             (as created by mount/filesystem create-lease-registry), or nil
             when no recording is needed. When supplied the sealed lease is
             stored with :revoked? false so verify/revoke paths work.

    :principal   Principal tagged union (I2) — required
    :resource    { :kind ... } — required (open, provider-defined)
    :actions     set of keywords — required, non-empty ⊆ allowlist
    :constraints map — optional, default {}
    :issued-at   #inst — optional, default now
    :expires-at  #inst — optional, default issued+1h; must be after issued
    :cap/id      uuid — optional, default fresh (alias :cap-id also accepted)

  Accepts legacy :subject as alias for :principal (break-compat transition:
  if both supplied, :principal wins; if neither, validates and throws).

  Validates via schema/validate-lease and asserts positive window
  (issued < expires) else throws :capability/schema-invalid via
  schema/make-lease. Returns the sealed CapabilityLease instance."
  [registry {:keys [principal subject resource actions constraints issued-at expires-at cap-id] :as opts}]
  (let [cap-id-val (or (:cap/id opts) cap-id (UUID/randomUUID))
        issued (or issued-at (Date.))
        expires (or expires-at (Date. (+ (.getTime ^Date issued) 3600000)))
        constraints-val (or constraints (:constraints opts) {})
        principal-val (or principal subject (:principal opts) (:subject opts))
        actions-val (cond
                      (nil? actions) (:actions opts)
                      :else actions)
        ;; Normalize actions to a set when caller passed a sequential coll
        actions-set (when actions-val
                      (if (set? actions-val) actions-val (set actions-val)))
        lease-map (cond-> {:cap/id cap-id-val
                           :principal principal-val
                           :resource resource
                           :actions actions-set
                           :constraints constraints-val
                           :issued-at issued
                           :expires-at expires}
                    true identity)]
    (when-not (.before ^Date ^Date issued ^Date expires)
      (throw (err/error :capability/schema-invalid
                        "capability lease must span positive window: :expires-at after :issued-at"
                        {:value (err/sanitize lease-map)})))
    (let [lease (schema/make-lease lease-map)]
      (when registry
        (swap! registry assoc (:cap/id lease) {:lease lease :revoked? false}))
      lease)))

(declare lease-revoked?)

(defn derive-lease!
  "Derive a narrowed child lease from a sealed parent lease (P4 attenuation).

  Attenuation rule (C3 full algebra — Lease = Grant × Principal × TimeWindow × Quota):
  the child must be *narrower* than the parent in every lattice dimension — never wider:

    - Grant       — child Grant ≤ parent Grant  (grant/attenuates? via ResourceKindDescriptor)
                  i.e. parent Grant covers child Grant (filesystem /work attenuates to /work/project-a;
                  tool requires equality; ActionSet subset)
    - Quota       — child constraints ≤ parent constraints per ConstraintDescriptor le?
                  (each quota dimension: max-calls, max-bytes, etc. nil = top/unbounded)
    - TimeWindow  — child issued ≥ parent issued  and  child expires ≤ parent expires
                  (meet of windows is intersection)

  Derive = meet in each dimension (product lattice GLB). Quota meet is per-dimension min;
  Grant meet is via ResourceKindDescriptor/meet + ActionSet intersection;
  TimeWindow meet is [max issued, min expires]. Unknown constraint keys are rejected
  fail-closed (closed map, no passthrough) — widening via :max-bytes 100 -> 1000 is denied.

  The child is sealed via schema/make-lease, carries
  :cap/attenuated-from (and :attenuated-from) in its :constraints for audit,
  and is recorded in `registry` when supplied.

  registry — LeaseRegistry atom or nil
  parent-lease — sealed CapabilityLease (schema/lease? true), not revoked
  opts — map with optional keys:
    :principal   override principal (for subagent delegation; must still be valid)
    :resource    override resource (must be attenuated by parent, C2/C3)
    :actions     set of actions (default: parent actions)
    :constraints map (default: inherit parent constraints; when supplied, must be ≤ parent per C3)
    :issued-at   #inst (default: parent issued)
    :expires-at  #inst (default: parent expires)
    :cap-id / :cap/id  child cap id (default: fresh UUID)

  Legacy :subject alias accepted for :principal.

  Throws :capability/attenuation-invalid when any narrowing rule is violated,
  and :capability/schema-invalid when the resulting lease is malformed."
  [registry parent-lease {:keys [principal subject resource actions constraints issued-at expires-at cap-id] :as opts}]
  (when-not (schema/lease? parent-lease)
    (throw (err/error :capability/attenuation-invalid
                      "derive-lease! requires a sealed CapabilityLease as parent"
                      {:value (err/sanitize parent-lease)})))
  (when (and registry (lease-revoked? registry (:cap/id parent-lease)))
    (throw (err/error :capability/attenuation-invalid
                      "cannot derive from a revoked parent lease"
                      {:parent-cap-id (:cap/id parent-lease)})))
  (let [parent-principal (or (:principal parent-lease) (:subject parent-lease))
        parent-resource (:resource parent-lease)
        parent-actions (:actions parent-lease)
        parent-constraints (:constraints parent-lease)
        parent-issued (:issued-at parent-lease)
        parent-expires (:expires-at parent-lease)
        parent-cap-id (:cap/id parent-lease)
        child-principal (or principal subject (:principal opts) (:subject opts) parent-principal)
        child-resource (if (contains? (or opts {}) :resource) (:resource opts) parent-resource)
        child-actions-raw (if (contains? (or opts {}) :actions) actions parent-actions)
        child-actions-set (when child-actions-raw
                            (if (set? child-actions-raw) child-actions-raw (set child-actions-raw)))
        ;; C3: constraints canonicalization + closed check via constraint registry.
        ;; When :constraints not supplied, inherit parent (no widening). When supplied,
        ;; even {} is explicit and checked for widening (strict atomic replacement).
        child-constraints-raw (if (contains? opts :constraints)
                                (:constraints opts)
                                parent-constraints)
        ;; Canonicalize alias :maxBytes -> :max-bytes before lattice ops
        canon-parent-c (cstr/canonicalize-constraints (or parent-constraints {}))
        canon-child-raw (cstr/canonicalize-constraints (or child-constraints-raw {}))
        child-issued (or issued-at (:issued-at opts) parent-issued)
        child-expires (or expires-at (:expires-at opts) parent-expires)
        cap-id-val (or (:cap/id opts) cap-id (UUID/randomUUID))]
    ;; Grant lattice: single attenuates? covers both ResourceScope and ActionSet
    (when-not (grant/attenuates? {:resource parent-resource :actions (or parent-actions #{})}
                                  {:resource child-resource :actions (or child-actions-set #{})})
      (throw (err/error :capability/attenuation-invalid
                        "derived lease Grant must be attenuated by parent (Grant attenuates? — ResourceScope × ActionSet)"
                        {:parent-resource (err/sanitize parent-resource)
                         :child-resource (err/sanitize child-resource)
                         :parent-actions (err/sanitize parent-actions)
                         :child-actions (err/sanitize child-actions-set)})))
    ;; Quota lattice: C3 ConstraintDescriptor le?
    (when-not (cstr/le-constraints? canon-parent-c canon-child-raw)
      (throw (err/error :capability/attenuation-invalid
                        "derived constraints must be ≤ parent constraints (C3 quota lattice: each dimension narrower)"
                        {:parent-constraints (err/sanitize canon-parent-c)
                         :child-constraints (err/sanitize canon-child-raw)})))
    ;; TimeWindow lattice
    (when (.before ^Date ^Date child-issued ^Date parent-issued)
      (throw (err/error :capability/attenuation-invalid
                        "derived issued-at must be >= parent issued-at"
                        {:parent-issued-at parent-issued
                         :child-issued-at child-issued})))
    (when (.after ^Date ^Date child-expires ^Date parent-expires)
      (throw (err/error :capability/attenuation-invalid
                        "derived expires-at must be <= parent expires-at"
                        {:parent-expires-at parent-expires
                         :child-expires-at child-expires})))
    (when-not (.before ^Date ^Date child-issued ^Date child-expires)
      (throw (err/error :capability/schema-invalid
                        "derived lease must span positive window: :expires-at after :issued-at"
                        {:value (err/sanitize {:cap/id cap-id-val
                                               :principal child-principal
                                               :resource child-resource
                                               :actions child-actions-set
                                               :constraints canon-child-raw
                                               :issued-at child-issued
                                               :expires-at child-expires})})))
    ;; Derive = meet in each dimension. For quota, meet is per-dimension min;
    ;; when child ≤ parent, meet = child, but we compute explicitly for algebra.
    (let [quota-meet (cstr/meet-constraints canon-parent-c canon-child-raw)
          ;; quota-meet is canon-child-raw when le holds, but use meet for explicit product GLB.
          ;; Preserve explicit child map's shape: if child was supplied, meet may fill missing parent keys;
          ;; however le already ensured child is ≤ parent, so meet = child for supplied keys and
          ;; for omitted keys (when child supplied) le would have failed, so we are not in that case.
          ;; Use quota-meet as canonical final; when child not supplied (inherit), quota-meet = parent.
          final-constraints-raw (if (contains? opts :constraints) quota-meet canon-parent-c)
          merged-constraints (assoc final-constraints-raw
                                    :cap/attenuated-from parent-cap-id
                                    :attenuated-from parent-cap-id)
          lease-map {:cap/id cap-id-val
                     :principal child-principal
                     :resource child-resource
                     :actions child-actions-set
                     :constraints merged-constraints
                     :issued-at child-issued
                     :expires-at child-expires}
          lease (schema/make-lease lease-map)]
      (when registry
        (swap! registry assoc (:cap/id lease) {:lease lease :revoked? false}))
      lease)))
;; ---------------------------------------------------------------------------
;; Generic LeaseRegistry helpers (P5) — unified shape for ANY kind
;; (tool/model/memory/filesystem). This is the single definition; mount/filesystem
;; delegates to it. The registry is an atom mapping :cap/id -> {:lease <sealed>
;; :revoked? <bool>}. Revoke is idempotent and fail-closed: a revoked lease
;; is rejected wherever it is verified (broker/policy + filesystem verify).
;; ---------------------------------------------------------------------------

(defn create-lease-registry
  "A verifiable lease ledger: an atom mapping :cap/id -> {:lease <sealed> :revoked? <bool>}."
  []
  (atom {}))

(defn get-lease
  "Look up a recorded lease by :cap/id, or nil when not recorded."
  [registry cap-id]
  (get-in @registry [cap-id :lease]))

(defn lease-revoked?
  "True when the lease with :cap/id is recorded as revoked."
  [registry cap-id]
  (boolean (get-in @registry [cap-id :revoked?])))

(defn revoked?
  "Alias of lease-revoked? — generic predicate for ANY kind."
  [registry cap-id]
  (lease-revoked? registry cap-id))

(defn revoke-lease!
  "Revoke the recorded lease with :cap/id (fail-closed: a revoked lease is
  rejected by broker/policy and verify paths). Idempotent. Returns nil.
  When the cap-id was never recorded, a tombstone {:lease nil :revoked? true}
  is stored so future verification is fail-closed."
  [registry cap-id]
  (swap! registry update cap-id (fn [rec]
                                  (cond-> (or rec {:lease nil :revoked? true})
                                    true (assoc :revoked? true))))
  nil)

(defn register-lease!
  "Validate `lease` as a proper CapabilityLease and record it in `registry`,
  making it verifiable. Returns the lease. Throws :capability/schema-invalid
  on a malformed lease."
  [registry lease]
  (schema/validate-lease lease)
  (swap! registry assoc (:cap/id lease) {:lease lease :revoked? false})
  lease)

(defn revoke-leases!
  "Revoke each lease in `leases` (a collection of CapabilityLease) in
  `registry` via `revoke-lease!`. Idempotent; nil `leases` or empty
  collection is a no-op. Returns nil. S4 cascade helper."
  [registry leases]
  (when (and registry (seq leases))
    (doseq [l leases]
      (when-let [cap-id (:cap/id l)]
        (revoke-lease! registry cap-id))))
  nil)

(defn leases-for-session
  "Return all leases in `registry` whose principal is SessionPrincipal(session-id).
  Str-coerced compare for UUID/string ids."
  [registry session-id]
  (let [sid (str session-id)
        target {:principal/type :session :session/id (try (java.util.UUID/fromString sid) (catch Exception _ sid))}]
    ;; str-coerced matching handles both UUID and string forms
    (->> @registry
         vals
         (keep :lease)
         (filterv (fn [l]
                    (let [p (or (:principal l) (:subject l))]
                      (and (= :session (:principal/type p))
                           (= sid (str (:session/id p))))))))))

(defn leases-for-principal
  "Return all leases in `registry` for exact `principal` (equality)."
  [registry principal]
  (->> @registry
       vals
       (keep :lease)
       (filterv (fn [l] (= principal (or (:principal l) (:subject l)))))))
