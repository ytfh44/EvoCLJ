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

  opts — map with keys:
    :subject     { :phenotype/id \"sha256:...\" } — required
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
