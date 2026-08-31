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
  (:require [evoclj.capability.schema :as schema]
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
                    ;; allow callers that supplied :cap/id via opts map key
                    ;; already handled; ensure :cap/id wins even if opts had
                    ;; :cap-id alias — already resolved above.
                    true identity)]
    ;; Assert positive window with the mandated error type before sealing.
    ;; schema/make-lease also asserts this, but we keep the check here so
    ;; the error is :capability/schema-invalid even if schema were relaxed.
    (when-not (.before ^Date ^Date issued ^Date expires)
      (throw (err/error :capability/schema-invalid
                        "capability lease must span positive window: :expires-at after :issued-at"
                        {:value (err/sanitize lease-map)})))
    (let [lease (schema/make-lease lease-map)]
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
