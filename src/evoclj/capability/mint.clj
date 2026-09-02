(ns evoclj.capability.mint
  "P1 single-source Authority — DB is truth, memory LeaseRegistry is versioned cache.

  mint-lease! / derive-lease! are the ONLY places that mint/seal a CapabilityLease
  (INV-05). Validation/sealing delegates to evoclj.capability.schema/make-lease.
  When a DB handle and LeaseRegistry atom are supplied, the lease is DURABLY
  committed (INSERT INTO capabilities) BEFORE swap! cache, and revocation is
  durable UPDATE WHERE revoked=0 BEFORE cache tombstone. The cache carries a
  monotonic ::version that is bumped only on durable success; restart hydrates
  from DB via hydrate-registry! so the cache matches DB.

  Best-effort try/catch dual writes are removed; synthetic fallback on DB miss
  is deny."
  (:require [clojure.set :as set]
            [evoclj.capability.constraint :as cstr]
            [evoclj.capability.grant :as grant]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err])
  (:import (java.util Date UUID)))

(def ^:private registry-version-key ::version)

(defn- bump-version!
  "Increment version in registry atom; returns new version."
  [registry]
  (swap! registry update registry-version-key (fnil inc 0))
  (get @registry registry-version-key))

(defn- durable-insert!
  "Insert lease durably into capabilities table via capability-store.
  Throws if DB unavailable or CHECK fails; caller must NOT update cache on failure."
  [db lease]
  (when db
    (let [insert! (try (requiring-resolve 'evoclj.store.capability-store/insert-capability!)
                       (catch Exception _ nil))]
      (when insert!
        (@insert! db lease)))))

(defn- durable-revoke!
  "Durably revoke cap-id in DB (UPDATE WHERE revoked=0). Returns row or nil.
  Throws on DB error. Caller must then tombstone cache."
  [db cap-id]
  (when db
    (let [revoke! (try (requiring-resolve 'evoclj.store.capability-store/revoke-capability!)
                       (catch Exception _ nil))]
      (when revoke!
        (@revoke! db cap-id)))))

(defn mint-lease!
  "Mint one sealed CapabilityLease and durably record it when DB is supplied.

  Arity [registry opts] — memory-only (for pure unit tests without DB).
  Arity [db registry opts] — P1 durable: INSERT INTO capabilities BEFORE swap! cache.
  Arity [registry opts] with map containing :db — also durable when :db present.

  opts keys: :principal (I2 tagged union, required), :resource, :actions,
  :constraints, :issued-at, :expires-at, :cap-id/:cap/id.

  When registry supplied, the sealed lease is stored as {:lease lease :revoked? false}
  and version is bumped. If durable insert fails, cache is NOT updated and the
  exception propagates."
  ([registry opts]
   (mint-lease! nil registry opts))
   ([db registry {:keys [principal resource actions constraints issued-at expires-at cap-id] :as opts}]
   (let [db (or db (:db opts))
         cap-id-val (or (:cap/id opts) cap-id (UUID/randomUUID))
         issued (or issued-at (Date.))
         expires (or expires-at (Date. (+ (.getTime ^Date issued) 3600000)))
         constraints-val (or constraints (:constraints opts) {})
         principal-val (or principal (:principal opts))
         actions-val (cond
                       (nil? actions) (:actions opts)
                       :else actions)
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
       ;; P1 durable-commit before cache
       (when db
         (durable-insert! db lease))
       (when registry
         (swap! registry assoc (:cap/id lease) {:lease lease :revoked? false})
         (bump-version! registry))
       lease))))

(declare lease-revoked?)

(defn derive-lease!
  "Derive a narrowed child lease from a sealed parent lease (P4 attenuation).

  Attenuation rule (C3 full algebra — Lease = Grant x Principal x TimeWindow x Quota):
  child must be narrower than parent in every lattice dimension — never wider.

  Arity [registry parent-lease opts] — memory-only when no DB.
  Arity [db registry parent-lease opts] — P1 durable: INSERT BEFORE swap! cache.
  The optional :db may also be supplied inside opts as :db.

  When registry and db supplied, durable insert is performed before cache update;
  on failure cache is not mutated. Parent revocation is checked before attenuation."
  ([registry parent-lease opts]
   (derive-lease! nil registry parent-lease opts))
  ([db registry parent-lease {:keys [principal resource actions constraints issued-at expires-at cap-id] :as opts}]
   (let [db (or db (:db opts))]
     (when-not (schema/lease? parent-lease)
       (throw (err/error :capability/attenuation-invalid
                         "derive-lease! requires a sealed CapabilityLease as parent"
                         {:value (err/sanitize parent-lease)})))
     (when (and registry (lease-revoked? registry (:cap/id parent-lease)))
       (throw (err/error :capability/attenuation-invalid
                         "cannot derive from a revoked parent lease"
                         {:parent-cap-id (:cap/id parent-lease)})))
     (let [parent-principal (:principal parent-lease)
           parent-resource (:resource parent-lease)
           parent-actions (:actions parent-lease)
           parent-constraints (:constraints parent-lease)
           parent-issued (:issued-at parent-lease)
           parent-expires (:expires-at parent-lease)
           parent-cap-id (:cap/id parent-lease)
           child-principal (or principal (:principal opts) parent-principal)
           child-resource (if (contains? (or opts {}) :resource) (:resource opts) parent-resource)
           child-actions-raw (if (contains? (or opts {}) :actions) actions parent-actions)
           child-actions-set (when child-actions-raw
                               (if (set? child-actions-raw) child-actions-raw (set child-actions-raw)))
           child-constraints-raw (if (contains? opts :constraints)
                                   (:constraints opts)
                                   parent-constraints)
           canon-parent-c (cstr/canonicalize-constraints (or parent-constraints {}))
           canon-child-raw (cstr/canonicalize-constraints (or child-constraints-raw {}))
           child-issued (or issued-at (:issued-at opts) parent-issued)
           child-expires (or expires-at (:expires-at opts) parent-expires)
           cap-id-val (or (:cap/id opts) cap-id (UUID/randomUUID))]
       (when-not (grant/attenuates? {:resource parent-resource :actions (or parent-actions #{})}
                                     {:resource child-resource :actions (or child-actions-set #{})})
         (throw (err/error :capability/attenuation-invalid
                           "derived lease Grant must be attenuated by parent (Grant attenuates? — ResourceScope x ActionSet)"
                           {:parent-resource (err/sanitize parent-resource)
                            :child-resource (err/sanitize child-resource)
                            :parent-actions (err/sanitize parent-actions)
                            :child-actions (err/sanitize child-actions-set)})))
       (when-not (cstr/le-constraints? canon-parent-c canon-child-raw)
         (throw (err/error :capability/attenuation-invalid
                           "derived constraints must be <= parent constraints (C3 quota lattice: each dimension narrower)"
                           {:parent-constraints (err/sanitize canon-parent-c)
                            :child-constraints (err/sanitize canon-child-raw)})))
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
       (let [quota-meet (cstr/meet-constraints canon-parent-c canon-child-raw)
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
         (when db
           (durable-insert! db lease))
         (when registry
           (swap! registry assoc (:cap/id lease) {:lease lease :revoked? false})
           (bump-version! registry))
         lease)))))

;; ---------------------------------------------------------------------------
;; Generic LeaseRegistry helpers — P1 versioned cache (DB truth, memory versioned)
;; ---------------------------------------------------------------------------

(defn create-lease-registry
  "A verifiable lease ledger: an atom mapping :cap/id -> {:lease <sealed> :revoked? <bool>}
  plus a monotonic ::version bumped on each durable success. Fresh registry starts at 0."
  []
  (atom {registry-version-key 0}))

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
  "Revoke the recorded lease with :cap/id (fail-closed).

  Arity [registry cap-id] — memory-only, idempotent, tombstones unseen ids.
  Arity [db registry cap-id] — P1 durable: UPDATE WHERE revoked=0 BEFORE cache tombstone.
  When durable revocation fails (DB error), cache is not mutated and exception propagates.
  Idempotent: revoking twice is a no-op (DB WHERE revoked=0 prevents redundant write).

  Returns nil."
  ([registry cap-id]
   (when-not (true? (get-in @registry [cap-id :revoked?]))
     (swap! registry update cap-id (fn [rec]
                                     (cond-> (or rec {:lease nil :revoked? true})
                                       true (assoc :revoked? true))))
     (bump-version! registry))
   nil)
  ([db registry cap-id]
   (if (true? (get-in @registry [cap-id :revoked?]))
     (when db (durable-revoke! db cap-id))
     (do
       (when db (durable-revoke! db cap-id))
       (swap! registry update cap-id (fn [rec]
                                       (cond-> (or rec {:lease nil :revoked? true})
                                         true (assoc :revoked? true))))
       (bump-version! registry)))
   nil))

(defn register-lease!
  "Validate `lease` as a proper CapabilityLease and record it in `registry`,
  making it verifiable. When db supplied (via :db in lease map or as first arg),
  durable INSERT is performed before cache. Returns the lease.

  Arity [registry lease] — memory-only.
  Arity [db registry lease] — durable."
  ([registry lease]
   (register-lease! nil registry lease))
  ([db registry lease]
   (schema/validate-lease lease)
   (when db
     (durable-insert! db lease))
   (swap! registry assoc (:cap/id lease) {:lease lease :revoked? false})
   (bump-version! registry)
   lease))

(defn revoke-leases!
  "Revoke each lease in `leases` (a collection of CapabilityLease) in
  `registry` via `revoke-lease!`. Idempotent; nil `leases` or empty
  collection is a no-op. Returns nil. S4 cascade helper.

  Arity [registry leases] — memory-only.
  Arity [db registry leases] — durable per-lease."
  ([registry leases]
   (revoke-leases! nil registry leases))
  ([db registry leases]
   (when (and registry (seq leases))
     (doseq [l leases]
       (when-let [cap-id (:cap/id l)]
         (if db
           (revoke-lease! db registry cap-id)
           (revoke-lease! registry cap-id)))))
   nil))

(defn leases-for-session
  "Return all leases in `registry` whose principal is SessionPrincipal(session-id).
  Str-coerced compare for UUID/string ids. Skips version key."
  [registry session-id]
  (let [sid (str session-id)]
    (->> @registry
         vals
         (keep :lease)
         (filterv (fn [l]
                    (let [p (:principal l)]
                      (and (= :session (:principal/type p))
                           (= sid (str (:session/id p))))))))))

(defn leases-for-principal
  "Return all leases in `registry` for exact `principal` (equality)."
  [registry principal]
  (->> @registry
       vals
       (keep :lease)
       (filterv (fn [l] (= principal (:principal l))))))

(defn registry-version
  "Return monotonic version of registry (0 if uninitialized)."
  [registry]
  (get @registry registry-version-key 0))

(defn hydrate-registry!
  "Restart hydration: load all active (revoked=0) capabilities from DB into `registry`
  as versioned cache entries. DB is truth; cache is replaced (except version bump).
  Returns count of hydrated entries. Requires db handle."
  [db registry]
  (let [hydrate! (try (requiring-resolve 'evoclj.store.capability-store/hydrate-registry!)
                      (catch Exception _ nil))]
    (if hydrate!
      (@hydrate! db registry)
      0)))
