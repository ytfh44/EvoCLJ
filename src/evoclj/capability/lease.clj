(ns evoclj.capability.lease
  "Lease semantics for the v0 CapabilityLease (component).

  A CapabilityLease is a bounded HOST-OWNED grant: the kernel issues a
  plain immutable map binding ONE principal, ONE resource grant, an
  :actions set, and an instant window; the model never sees a lease as
  a name — only the kernel's broker (Milestone 4) reads it. The three
  pure decision functions are the whole semantics:

    (valid-at? lease instant)          ; window check
    (principal-matches? lease principal) ; EXACT principal equality (I2)
    (resource-covers? lease normalized-resource action) ; resource + action

  Every decision input is schema-checked before any judgment is made;
  a malformed lease, principal, resource, or action throws
  :capability/schema-invalid rather than silently granting or denying
  (a capability is a bounded host-owned grant, so garbage never
  authorizes and never hides a caller bug).

  Principal matching is EXACT equality on the tagged union: a lease for
  P1 never authorizes P2. No wildcard, no nil, no placeholder, no
  dual-anchor fallback (I2).

  Resource coverage matches a CANONICAL resource plus an action:
  tool resources match by exact canonical id; filesystem resources
  match by containment of CANONICAL RESOLVED PATHS; memory resources
  match by exact canonical key id ({:kind :memory :id <key>}, feature
  R1 — the episodic-memory lease an :intent/memory-read/write carries
  is scoped to one exact key, mirroring :tool). Matching never happens
  on user-supplied strings: canonicalize-path resolves \".\"
  and \"..\" segments first, so a lease rooted at \"/work\" covers
  \"/work/a/../secret\" only because it resolves to \"/work/secret\"
  (inside the root), and a traversal escaping to \"/etc\" is never
  covered. The pure path canonicalization lives here so coverage is
  always decided on canonical forms; provider-side normalization of
  user-facing requests (kinds, ids, paths, Windows drive/backslash
  forms) is component (evoclj.provider). Unknown resource kinds fail
  closed: nothing is covered."
  (:require [clojure.string :as str]
            [evoclj.capability.resource-kind :as rk]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [malli.core :as m]))

;; --- pure path canonicalization (delegates to resource-kind, kept for compat) --------

(defn canonicalize-path
  "Deprecated alias — delegates to evoclj.capability.resource-kind/canonicalize-path.
  Kept for backward compat (lease tests call lease/canonicalize-path directly)."
  [s] (rk/canonicalize-path s))

;; Legacy private helpers retained for local use but now delegate; descriptor owns logic.
(defn- path-inside? [root path]
  (let [r (rk/canonicalize-path root) p (rk/canonicalize-path path)]
    (and r p (or (= r "/") (= r p) (str/starts-with? p (str r "/"))))))
(defn- canonicalize-mount-path [s] (when (string? s) (str/replace (or (rk/canonicalize-path (str "/" s)) "") #"^/" "")))
(defn- mount-path-inside? [grant-path req-path]
  (let [g (or (rk/canonicalize-path (str "/" (or grant-path ""))) "/")
        p (or (rk/canonicalize-path (str "/" (or req-path ""))) "/")]
    (or (= g "/") (= g p) (str/starts-with? p (str g "/")))))

;; --- shared input gate ------------------------------------------------------

(defn- validate-input!
  "Schema-check a lease and, when given, an additional decision input;
  throw :capability/schema-invalid on any failure. Every predicate
  gates its inputs here so no judgment is ever made on malformed data."
  [lease & [input-schema input]]
  (schema/validate-lease lease)
  (when (and input-schema (not (m/validate input-schema input)))
    (throw (err/error :capability/schema-invalid
                      "invalid capability lease decision input"
                      {:value (err/sanitize input)}))))

;; --- the three decision functions -------------------------------------------

(defn valid-at?
  "True when `instant` falls inside the lease's window:
  :issued-at INCLUSIVE, :expires-at EXCLUSIVE — a lease is valid AT
  :issued-at, dead AT :expires-at, and dead before :issued-at. The
  lease and the instant must be schema-valid or
  :capability/schema-invalid is thrown."
  [lease instant]
  (validate-input! lease)
  (when-not (inst? instant)
    (throw (err/error :capability/schema-invalid
                      "lease instant must be an #inst value"
                      {:value (err/sanitize instant)})))
  (and (not (.before ^java.util.Date instant ^java.util.Date (:issued-at lease)))
       (.before ^java.util.Date instant ^java.util.Date (:expires-at lease))))

(defn principal-matches?
  "True when the requesting `principal` equals the lease's principal (I2).
  No wildcard, no nil, no placeholder — exact equality on the tagged union.
  Legacy :subject maps are canonicalized to Principal for compat."
  [lease principal]
  (validate-input! lease schema/PrincipalSchema principal)
  (let [raw (or (:principal lease) (:subject lease))
        lp (if (and (map? raw) (:principal/type raw))
             raw
             (cond
               (and (map? raw) (:session/id raw)) {:principal/type :session :session/id (:session/id raw)}
               (map? raw) {:principal/type :operator}
               :else raw))]
    (= lp principal)))

(defn subject-matches?
  "Deprecated alias for principal-matches? — use principal-matches?."
  [lease principal]
  (principal-matches? lease principal))
(defn resource-covers?
  "True when the lease's :resource grant covers the canonical
  `normalized-resource` for `action`: dispatches via ResourceKindDescriptor
  (C1). The action must be in the lease's :actions set AND the descriptor's
  covers? must be true. Unknown kinds fail closed."
  [lease normalized-resource action]
  (validate-input! lease)
  (when-not (and (map? normalized-resource) (keyword? action))
    (throw (err/error :capability/schema-invalid
                      "resource must be a map and action a keyword"
                      {:value (err/sanitize normalized-resource)
                       :action (err/sanitize action)})))
  (let [granted (:resource lease)
        gk (:kind granted)
        rk-kind (:kind normalized-resource)]
    (and (contains? (:actions lease) action)
         (= gk rk-kind)
         (boolean
          (if-let [d (rk/get-descriptor gk)]
            (rk/covers? d granted normalized-resource action)
            false)))))

;; ---------------------------------------------------------------------------
;; Generic LeaseRegistry helpers (P5) — unified for ANY kind
;; Delegates to capability/mint (single definition). Kept here as well so
;; callers can require either mint or lease. Idempotent revoke, fail-closed.
;; ---------------------------------------------------------------------------

(defn create-lease-registry
  "Create a fresh LeaseRegistry atom (delegates to capability/mint)."
  []
  (atom {}))

(defn get-lease
  "Look up a recorded lease by :cap/id, or nil (delegates to mint)."
  [registry cap-id]
  (get-in @registry [cap-id :lease]))

(defn lease-revoked?
  "True when the lease with :cap/id is recorded as revoked."
  [registry cap-id]
  (boolean (get-in @registry [cap-id :revoked?])))

(defn revoked?
  "Alias of lease-revoked? for ANY kind."
  [registry cap-id]
  (lease-revoked? registry cap-id))

(defn revoke-lease!
  "Revoke the recorded lease with :cap/id, idempotent."
  [registry cap-id]
  (swap! registry update cap-id (fn [rec]
                                  (cond-> (or rec {:lease nil :revoked? true})
                                    true (assoc :revoked? true))))
  nil)

(defn revoke-leases!
  "Revoke each lease in `leases` via `revoke-lease!`. Idempotent. S4 helper
  mirroring capability/mint. Delegates to revoke-lease! for each cap-id."
  [registry leases]
  (when (and registry (seq leases))
    (doseq [l leases]
      (when-let [cap-id (:cap/id l)]
        (revoke-lease! registry cap-id))))
  nil)

(defn leases-for-session
  "Return leases in `registry` for SessionPrincipal `session-id` (str-coerced compare)."
  [registry session-id]
  (let [sid (str session-id)]
    (->> @registry
         vals
         (keep :lease)
         (filterv (fn [l]
                    (let [p (or (:principal l) (:subject l))]
                      (and (= :session (:principal/type p))
                           (= sid (str (:session/id p))))))))))

(defn leases-for-principal
  "Return leases in `registry` for exact `principal` (equality)."
  [registry principal]
  (->> @registry
       vals
       (keep :lease)
       (filterv (fn [l] (= principal (or (:principal l) (:subject l)))))))
