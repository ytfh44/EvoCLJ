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
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err])
  (:import (java.util Date UUID)))

(defn mint-lease!
  "Mint one sealed CapabilityLease and optionally record it in `registry`.

  registry — an atom mapping :cap/id -> {:lease <sealed> :revoked? <bool>}
             (as created by mount/filesystem create-lease-registry), or nil
             when no recording is needed. When supplied the sealed lease is
             stored with :revoked? false so verify/revoke paths work.

    :subject     { :session/id <uuid> :phenotype/id \"sha256:...\" } — required, dual-anchor
    :resource    { :kind ... } — required (open, provider-defined)
    :actions     set of keywords — required, non-empty ⊆ allowlist
    :constraints map — optional, default {}
    :issued-at   #inst — optional, default now
    :expires-at  #inst — optional, default issued+1h; must be after issued
    :cap/id      uuid — optional, default fresh (alias :cap-id also accepted)

  Validates via schema/validate-lease and asserts positive window
  (issued < expires) else throws :capability/schema-invalid via
  schema/make-lease. Returns the sealed CapabilityLease instance."
  [registry {:keys [subject resource actions constraints issued-at expires-at cap-id] :as opts}]
  (let [cap-id-val (or (:cap/id opts) cap-id (UUID/randomUUID))
        issued (or issued-at (Date.))
        expires (or expires-at (Date. (+ (.getTime ^Date issued) 3600000)))
        constraints-val (or constraints (:constraints opts) {})
        actions-val (cond
                      (nil? actions) (:actions opts)
                      :else actions)
        ;; Normalize actions to a set when caller passed a sequential coll
        actions-set (when actions-val
                      (if (set? actions-val) actions-val (set actions-val)))
        lease-map (cond-> {:cap/id cap-id-val
                           :subject subject
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

  Attenuation rule (Wolfram [W-08..W-11]): the child must be *narrower* than
  the parent in every dimension — never wider:

    - actions  — child ⊆ parent
    - max-calls — child max-calls <= parent max-calls (nil means unlimited)
    - issued   — child issued >= parent issued
    - expires  — child expires <= parent expires
    - resource — child resource == parent resource (P4 keeps resource fixed)

  The child is sealed via schema/make-lease, carries
  :cap/attenuated-from (and :attenuated-from) in its :constraints for audit,
  and is recorded in `registry` when supplied.

  registry — LeaseRegistry atom or nil
  parent-lease — sealed CapabilityLease (schema/lease? true), not revoked
  opts — map with optional keys:
    :subject     override subject (for subagent delegation; must still be valid)
    :resource    override resource (must equal parent resource)
    :actions     set of actions (default: parent actions)
    :constraints map (default: {} merged with parent constraints, see below)
    :issued-at   #inst (default: now)
    :expires-at  #inst (default: parent expires)
    :cap-id / :cap/id  child cap id (default: fresh UUID)

  Throws :capability/attenuation-invalid when any narrowing rule is violated,
  and :capability/schema-invalid when the resulting lease is malformed."
  [registry parent-lease {:keys [subject resource actions constraints issued-at expires-at cap-id] :as opts}]
  (when-not (schema/lease? parent-lease)
    (throw (err/error :capability/attenuation-invalid
                      "derive-lease! requires a sealed CapabilityLease as parent"
                      {:value (err/sanitize parent-lease)})))
  (when (and registry (lease-revoked? registry (:cap/id parent-lease)))
    (throw (err/error :capability/attenuation-invalid
                      "cannot derive from a revoked parent lease"
                      {:parent-cap-id (:cap/id parent-lease)})))
  (let [parent-subject (:subject parent-lease)
        parent-resource (:resource parent-lease)
        parent-actions (:actions parent-lease)
        parent-constraints (:constraints parent-lease)
        parent-issued (:issued-at parent-lease)
        parent-expires (:expires-at parent-lease)
        parent-cap-id (:cap/id parent-lease)
        child-subject (or subject (:subject opts) parent-subject)
        child-resource (if (contains? (or opts {}) :resource) (:resource opts) parent-resource)
        child-actions-raw (if (contains? (or opts {}) :actions) actions parent-actions)
        child-actions-set (when child-actions-raw
                            (if (set? child-actions-raw) child-actions-raw (set child-actions-raw)))
        child-constraints-raw (or constraints (:constraints opts) parent-constraints)
        child-issued (or issued-at (:issued-at opts) parent-issued)
        child-expires (or expires-at (:expires-at opts) parent-expires)
        cap-id-val (or (:cap/id opts) cap-id (UUID/randomUUID))]
    (when-not (= child-resource parent-resource)
      (throw (err/error :capability/attenuation-invalid
                        "derived lease resource must equal parent resource"
                        {:parent-resource (err/sanitize parent-resource)
                         :child-resource (err/sanitize child-resource)})))
    (when-not (set/subset? (or child-actions-set #{}) (or parent-actions #{}))
      (throw (err/error :capability/attenuation-invalid
                        "derived actions must be subset of parent actions"
                        {:parent-actions (err/sanitize parent-actions)
                         :child-actions (err/sanitize child-actions-set)})))
    (let [parent-max (get parent-constraints :max-calls)
          child-max (get child-constraints-raw :max-calls)]
      (when (some? parent-max)
        (when (nil? child-max)
          (throw (err/error :capability/attenuation-invalid
                            "derived lease must not widen max-calls: parent has finite max-calls but child is unlimited"
                            {:parent-max-calls parent-max
                             :child-max-calls child-max})))
        (when (and (some? child-max) (> child-max parent-max))
          (throw (err/error :capability/attenuation-invalid
                            "derived max-calls must be <= parent max-calls"
                            {:parent-max-calls parent-max
                             :child-max-calls child-max}))))
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
                                                 :subject child-subject
                                                 :resource child-resource
                                                 :actions child-actions-set
                                                 :constraints child-constraints-raw
                                                 :issued-at child-issued
                                                 :expires-at child-expires})})))
      (let [merged-constraints (assoc child-constraints-raw
                                      :cap/attenuated-from parent-cap-id
                                      :attenuated-from parent-cap-id)
            lease-map {:cap/id cap-id-val
                       :subject child-subject
                       :resource child-resource
                       :actions child-actions-set
                       :constraints merged-constraints
                       :issued-at child-issued
                       :expires-at child-expires}
            lease (schema/make-lease lease-map)]
        (when registry
          (swap! registry assoc (:cap/id lease) {:lease lease :revoked? false}))
        lease))))

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
