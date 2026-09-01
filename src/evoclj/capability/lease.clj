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
            [evoclj.capability.constraint :as cstr]
            [evoclj.capability.grant :as grant]
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
  Legacy :subject maps are canonicalized to Principal for compat (filesystem tests)."
  [lease principal]
  (let [canonical-principal (cond
                              (and (map? principal) (:principal/type principal)) principal
                              (and (map? principal) (:session/id principal) (not (contains? principal :principal/type))) {:principal/type :session :session/id (:session/id principal)}
                              (and (map? principal) (:job/id principal) (not (contains? principal :principal/type))) {:principal/type :job :job/id (:job/id principal)}
                              (and (map? principal) (:eval/id principal) (not (contains? principal :principal/type))) {:principal/type :eval :eval/id (:eval/id principal)}
                              :else principal)]
    (validate-input! lease schema/PrincipalSchema canonical-principal)
    (let [raw (or (:principal lease) (:subject lease))
          lp (if (and (map? raw) (:principal/type raw))
               raw
               (cond
                 (and (map? raw) (:session/id raw)) {:principal/type :session :session/id (:session/id raw)}
                 (map? raw) {:principal/type :operator}
                 :else raw))]
      (= lp canonical-principal))))

(defn subject-matches?
  "Deprecated alias for principal-matches? — use principal-matches?."
  [lease principal]
  (principal-matches? lease principal))
(defn resource-covers?
  "True when the lease's :resource grant covers the canonical
  `normalized-resource` for `action`: Grant covers? (C2) product order.
  Delegates to evoclj.capability.grant/covers? (ResourceScope × ActionSet).
  The action must be in the lease's :actions set AND the descriptor's
  covers? must be true. Unknown kinds fail closed."
  [lease normalized-resource action]
  (validate-input! lease)
  (when-not (and (map? normalized-resource) (keyword? action))
    (throw (err/error :capability/schema-invalid
                      "resource must be a map and action a keyword"
                      {:value (err/sanitize normalized-resource)
                       :action (err/sanitize action)})))
  (grant/covers? {:resource (:resource lease) :actions (:actions lease)}
                 {:resource normalized-resource :actions #{action}}))
;; C3 — Lease full algebra: constraints via ConstraintDescriptor
;; ---------------------------------------------------------------------------

(defn constraints-le?
  "True when child constraints ≤ parent constraints in every quota dimension.
  Delegates to evoclj.capability.constraint/le-constraints? (C3 lattice)."
  [parent-constraints child-constraints]
  (cstr/le-constraints? parent-constraints child-constraints))

(defn constraints-meet
  "GLB of two constraint maps (per-dimension min). Delegates to constraint/meet-constraints.
  Audit keys are not part of the quota lattice and are ignored."
  [a b]
  (cstr/meet-constraints a b))

(defn lease-attenuates?
  "True when child lease is attenuated by parent lease in the full product lattice:
  Grant attenuates? × constraints le? × TimeWindow le? (issued ≥, expires ≤).
  Principal is not part of the attenuates order (delegation may change principal)."
  [parent child]
  (let [grant-ok (grant/attenuates? {:resource (:resource parent) :actions (:actions parent)}
                                    {:resource (:resource child) :actions (:actions child)})
        quota-ok (constraints-le? (:constraints parent) (:constraints child))
        time-ok (and (not (.before ^java.util.Date (:issued-at child) ^java.util.Date (:issued-at parent)))
                     (not (.after ^java.util.Date (:expires-at child) ^java.util.Date (:expires-at parent))))]
    (and grant-ok quota-ok time-ok)))

(defn lease-meet
  "Greatest lower bound of two leases in the product lattice, or nil when disjoint.
  Grant meet via grant/meet, quota meet via constraints-meet, TimeWindow meet via
  [max issued, min expires]; principal is not merged (nil). Returns a plain map
  suitable for schema/make-lease (without :cap/id)."
  [a b]
  (when (and (map? a) (map? b))
    (when-let [g (grant/meet {:resource (:resource a) :actions (:actions a)}
                              {:resource (:resource b) :actions (:actions b)})]
      (let [quota (constraints-meet (:constraints a) (:constraints b))
            issued (if (.after ^java.util.Date (:issued-at a) ^java.util.Date (:issued-at b))
                     (:issued-at a) (:issued-at b))
            expires (if (.before ^java.util.Date (:expires-at a) ^java.util.Date (:expires-at b))
                      (:expires-at a) (:expires-at b))]
        (when (.before ^java.util.Date issued ^java.util.Date expires)
          {:resource (:resource g)
           :actions (:actions g)
           :constraints quota
           :issued-at issued
           :expires-at expires})))))

;; ---------------------------------------------------------------------------
;; Generic LeaseRegistry helpers (P5/P1) — versioned cache, delegates to mint
;; ---------------------------------------------------------------------------

(defn create-lease-registry
  "Create a fresh LeaseRegistry atom (delegates to capability/mint)."
  []
  (let [f (requiring-resolve 'evoclj.capability.mint/create-lease-registry)]
    (@f)))

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
  "Revoke the recorded lease with :cap/id, idempotent.
  Arity [registry cap-id] memory-only; [db registry cap-id] durable P1."
  ([registry cap-id]
   (let [f (requiring-resolve 'evoclj.capability.mint/revoke-lease!)]
     (@f registry cap-id)))
  ([db registry cap-id]
   (let [f (requiring-resolve 'evoclj.capability.mint/revoke-lease!)]
     (@f db registry cap-id))))

(defn revoke-leases!
  "Revoke each lease in `leases` via `revoke-lease!`. Idempotent."
  ([registry leases]
   (let [f (requiring-resolve 'evoclj.capability.mint/revoke-leases!)]
     (@f registry leases)))
  ([db registry leases]
   (let [f (requiring-resolve 'evoclj.capability.mint/revoke-leases!)]
     (@f db registry leases))))

(defn leases-for-session
  "Return leases in `registry` for SessionPrincipal `session-id` (str-coerced compare)."
  [registry session-id]
  (let [f (requiring-resolve 'evoclj.capability.mint/leases-for-session)]
    (@f registry session-id)))

(defn leases-for-principal
  "Return leases in `registry` for exact `principal` (equality)."
  [registry principal]
  (let [f (requiring-resolve 'evoclj.capability.mint/leases-for-principal)]
    (@f registry principal)))

(defn registry-version
  "Return monotonic version of registry (delegates to mint)."
  [registry]
  (let [f (requiring-resolve 'evoclj.capability.mint/registry-version)]
    (@f registry)))

(defn hydrate-registry!
  "Restart hydration: load active capabilities from DB into registry (delegates to mint)."
  [db registry]
  (let [f (requiring-resolve 'evoclj.capability.mint/hydrate-registry!)]
    (@f db registry)))
