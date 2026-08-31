(ns evoclj.mount.filesystem
  "Generic filesystem provider serving both Workspace (RW) and Skill (RO) mounts.

  Single provider instance takes a mount registry (atom map mount-id -> mount)
  and routes every operation by logical mount namespace.

  Canonical resource for filesystem uses logical mount namespace:
    {:kind :filesystem/path :mount/id [:skill \"debugging\" \"sha256:...\"] :path \"references/foo.md\"}
  Never a host absolute path like /home/x/.agents/skills/...

  Enforcement: EffectiveAccess = SurfaceAccessMax ∩ CapabilityLease
    - SurfaceAccessMax is mount's :access/max (e.g. Skill RO = #{:read :list :stat})
    - CapabilityLease is a proper v0 CapabilityLease (subject-bound, positive
      window, :actions set, :resource scope). Subject and expiry are FORCED
      (B4): a lease bound to a different subject, an expired lease, or a
      revoked lease is rejected with a precise typed error — never silently
      honored.
    Even if a lease incorrectly grants :write to a Skill mount, the backend
    still rejects because surface max does not contain :write (fail closed).

  Additional guarantees:
    - \"..\" never escapes mount (canonicalize-mount-path throws :filesystem/path-outside-mount)
    - mount A lease cannot access mount B (mount-id equality required)
    - CAS tree content is independent of upstream host changes after snapshot
      (manifest is loaded once from CAS and cached in backend)

  Operations:
    First batch (RO): stat, list, read
    Second batch (RW): write, create, delete — only when mount surface and lease both grant.

  This provider is the single filesystem I/O path. There is no separate
  read_skill_file / read-skill-file facade — all reads go through
  provider-read (no independent I/O stack)."
  (:require [clojure.string :as str]
            [evoclj.capability.lease :as lease]
            [evoclj.capability.mint :as cap-mint]
            [evoclj.capability.schema :as cap-schema]
            [evoclj.mount.backend :as backend]
            [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [malli.core :as m])
  (:import (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; Canonical resource
;; ---------------------------------------------------------------------------

(defn canonical-resource
  "Build the canonical filesystem resource for mount-id + path.

  Uses logical mount namespace, never host absolute path.
  Path is canonicalized (\"..\" escapes mount -> throws)."
  [mount-id path]
  (when-not (backend/mount-id? mount-id)
    (throw (err/error :filesystem/invalid-mount-id "invalid mount id" {:mount/id mount-id})))
  (let [canonical (backend/canonicalize-mount-path (or path ""))]
    {:kind :filesystem/path
     :mount/id mount-id
     :path canonical}))

;; ---------------------------------------------------------------------------
;; FS lease issuer (B4) — the wired issuer that mints, records, verifies,
;; and revokes filesystem CapabilityLeases. A lease is a bounded HOST-OWNED
;; grant bound to ONE exact subject and ONE canonical :filesystem/path
;; resource, spanning a positive window. The issuer is the ONLY source of a
;; valid grant (Global Constraint 9: a visible action never grants resource
;; authority); a lease minted here is recorded in a registry so it can be
;; re-verified and revoked (fail-closed, never silently honored).
;; ---------------------------------------------------------------------------

(defn create-lease-registry
  "A verifiable lease ledger: an atom mapping :cap/id ->
  {:lease <lease> :revoked? <boolean>}."
  []
  (atom {}))

(defn register-lease!
  "Validate `lease` as a proper filesystem CapabilityLease and record it in
  `lease-registry`, making it verifiable. Returns the lease. Throws typed
  :capability/schema-invalid on a malformed lease."
  [lease-registry lease]
  (cap-schema/validate-lease lease)
  (swap! lease-registry assoc (:cap/id lease) {:lease lease :revoked? false})
  lease)

(defn get-lease
  "Look up a recorded lease by :cap/id, or nil when not recorded."
  [lease-registry cap-id]
  (get-in @lease-registry [cap-id :lease]))

(defn lease-revoked?
  "True when the lease with :cap/id is recorded as revoked."
  [lease-registry cap-id]
  (boolean (get-in @lease-registry [cap-id :revoked?])))

(defn revoke-lease!
  "Revoke the recorded lease with :cap/id (fail-closed: a revoked lease is
  rejected by verify-fs-lease!/lease-grants?). Idempotent. Returns nil."
  [lease-registry cap-id]
  (swap! lease-registry update cap-id (fn [rec]
                                        (cond-> (or rec {:lease nil :revoked? true})
                                          true (assoc :revoked? true))))
  nil)

(defn- phenotype-id-valid?
  "True when subject is a { :phenotype/id <sha256> } map conforming to the
  capability SubjectSchema (exact phenotype id)."
  [subject]
  (boolean (m/validate cap-schema/SubjectSchema subject)))

(defn issue-fs-lease
  "The filesystem lease issuer (B4). Mint one v0 CapabilityLease granting
  ONE authorized subject `actions` over the canonical :filesystem/path
  resource {:mount/id mount-id :path path}, spanning a positive window.

  Inputs (all required unless noted):
    :subject     { :phenotype/id \"sha256:...\" } — the SINGLE phenotype the
                 grant belongs to (exact match; a sibling phenotype from the
                 same Genome is a different subject and never matches).
    :mount-id    canonical vector mount id ([:skill \"x\" \"sha256:...\"] or
                 [:workspace \"id\"]).
    :path        mount-relative path the grant covers (\"\" = whole mount).
    :actions     non-empty #{:read :list :stat :write :create :delete}.
    :issued-at   #inst (default now) — INCLUSIVE window start.
    :expires-at  #inst (default +1h) — EXCLUSIVE window end; must be after
                 :issued-at (a zero/negative window is a host bug, rejected).
    :constraints optional map (default {} — no call limit).
    :cap-id      optional #uuid (default fresh, alias cap/id also accepted).

  When `lease-registry` is supplied the lease is recorded (verifiable and
  revocable). Returns the sealed lease. Throws typed errors:
    :capability/schema-invalid  — malformed subject/mount/path/actions/window
    :filesystem/path-outside-mount — path escapes the mount.

  The issuer is the wired production path for granting filesystem access —
  simplified bare { :mount/id :path :actions } maps are NOT grants and are
  rejected by the access path (B4: subject/expiry are forced). Delegates
  to evoclj.capability.mint/mint-lease! (P2 single issuance surface)."
  [lease-registry
   {:keys [subject mount-id path actions issued-at expires-at constraints]
    :as opts}]
  (let [cap-id (or (get opts (keyword "cap/id")) (:cap-id opts))]
    (when-not (phenotype-id-valid? subject)
      (throw (err/error :capability/schema-invalid
                        "fs lease subject must be a single authorized phenotype ({:phenotype/id sha256})"
                        {:subject (err/sanitize subject)})))
    (when-not (backend/mount-id? mount-id)
      (throw (err/error :capability/schema-invalid
                        "fs lease requires a canonical vector :mount/id"
                        {:mount/id mount-id})))
    (let [canonical (backend/canonicalize-mount-path (or path ""))]
      (when-not (and (set? actions) (seq actions)
                     (every? backend/valid-capabilities actions))
        (throw (err/error :capability/schema-invalid
                          "fs lease requires a non-empty subset of valid filesystem actions"
                          {:actions (err/sanitize actions) :valid (vec backend/valid-capabilities)})))
      (let [issued (or issued-at (Date.))
            expires (or expires-at (Date. (+ (.getTime ^Date issued) 3600000)))]
        (when-not (inst? issued)
          (throw (err/error :capability/schema-invalid
                            "fs lease :issued-at must be an #inst" {:issued-at issued})))
        (when-not (inst? expires)
          (throw (err/error :capability/schema-invalid
                            "fs lease :expires-at must be an #inst" {:expires-at expires})))
        (cap-mint/mint-lease! lease-registry
                              {:cap-id (or cap-id (UUID/randomUUID))
                               :subject subject
                               :resource {:kind :filesystem/path :mount/id mount-id :path canonical}
                               :actions (set actions)
                               :constraints (or constraints {})
                               :issued-at issued
                               :expires-at expires})))))

(defn verify-fs-lease!
  "Re-verify a filesystem lease FAIL-CLOSED (B4 reload!/restore!
  re-verification and the access path). Throws the precise typed error:
    :capability/schema-invalid   — lease is not a valid CapabilityLease
    :capability/revoked          — lease is recorded as revoked in
                                   `:registry`, or (when a registry is
                                   supplied) is not recorded at all (the
                                   issuer is the only source of a grant)
    :capability/expired          — the window does not cover `:now`
    :capability/subject-mismatch — `:subject` (when supplied) differs from
                                   the lease's bound subject
  Returns the lease when valid. `:now` defaults to the access clock (expiry
  is FORCED — never optional). Uses evoclj.capability.lease for the expiry
  and subject judgements (single implementation, INV-05)."
  [lease {:keys [now subject registry]}]
  (cap-schema/validate-lease lease)
  (when (some? registry)
    (let [rec (get @registry (:cap/id lease))]
      (when (or (nil? rec) (:revoked? rec))
        (throw (err/error :capability/revoked
                          "lease is revoked or was never issued by the fs lease issuer"
                          {:cap/id (:cap/id lease)})))))
  (let [t (or now (Date.))]
    (when-not (lease/valid-at? lease t)
      (throw (err/error :capability/expired
                        "lease has expired"
                        {:cap/id (:cap/id lease)
                         :expires-at (:expires-at lease)
                         :now t}))))
  (when (and subject (not (lease/subject-matches? lease subject)))
    (throw (err/error :capability/subject-mismatch
                      "lease belongs to a different subject"
                      {:cap/id (:cap/id lease)
                       :lease-subject (:subject lease)
                       :request-subject (err/sanitize subject)})))
  lease)

;; ---------------------------------------------------------------------------
;; Lease enforcement — the access path FORCES subject and expiry (B4).
;; The lease MUST be a valid CapabilityLease (a bare { :mount/id :path :actions }
;; map is NOT a grant); a malformed lease throws, never silently grants or
;; denies. Coverage is delegated to evoclj.capability.lease (INV-05 — one
;; implementation, no re-implemented expiry/subject logic here).
;; ---------------------------------------------------------------------------

(defn- lease-grants?
  "True when a valid `lease` grants `action` for mount-id + req-path.
  B4: subject and expiry are FORCED — but only on the lease that actually
  covers this request (scope + action). A lease that does not cover the
  request simply does not grant (returns false, so another lease may); a
  lease that DOES cover it but is expired, revoked, or bound to a
  different subject throws the precise typed error (:capability/expired /
  :capability/revoked / :capability/subject-mismatch). The requesting
  subject comes from opts :subject (required for a covering lease); the
  instant from opts :now (defaults to the access clock); revocation /
  recording from opts :registry (when supplied)."
  [lease mount-id req-path action {:keys [now subject registry] :as opts}]
  (cap-schema/validate-lease lease)
  (if (lease/resource-covers? lease
                              {:kind :filesystem/path :mount/id mount-id :path req-path}
                              action)
    (do
      (when-not subject
        (throw (err/error :filesystem/lease-subject-required
                          "filesystem access requires a requesting :subject to enforce the lease subject binding"
                          {:mount/id mount-id :action action})))
      (verify-fs-lease! lease {:now now :subject subject :registry registry})
      true)
    false))

(defn- authorized?
  "True when any lease in collection grants action for resource.
  Surface check is separate (EffectiveAccess is intersection). A lease that
  is expired/revoked/subject-mismatched throws its precise typed error
  (fail-closed), never silently skipped."
  [leases mount-id req-path action opts]
  (some (fn [lease] (lease-grants? lease mount-id req-path action opts))
        (or leases [])))

(defn- check-effective-access!
  "Enforce EffectiveAccess = SurfaceAccessMax ∩ CapabilityLease.

  Throws:
    :mount/not-found if mount missing
    :filesystem/path-outside-mount if path escapes (from canonicalize)
    :mount/access-denied if surface does not contain action
    :capability/denied (or :filesystem/access-denied) if no lease grants

  Returns mount and canonical path on success."
  [registry mount-id raw-path action leases opts]
  (let [mount (backend/get-mount registry mount-id)]
    (when-not mount
      (throw (err/error :mount/not-found "mount not found" {:mount/id mount-id})))
    (let [canonical (backend/canonicalize-mount-path (or raw-path ""))
          surface (:access/max mount)]
      (when-not (contains? surface action)
        (throw (err/error :mount/read-only
                          (str "surface does not grant " action)
                          {:mount/id mount-id :action action :access/max surface})))
      (when-not (authorized? (or leases []) mount-id canonical action opts)
        (throw (err/error :capability/denied
                          (str "no lease grants " action " for mount")
                          {:mount/id mount-id :path canonical :action action})))
      {:mount mount :canonical canonical})))

;; ---------------------------------------------------------------------------
;; Provider
;; ---------------------------------------------------------------------------

(defrecord FilesystemProvider [mount-registry])

(defn make-provider
  "Create a generic filesystem provider bound to mount-registry atom.

  The same provider instance serves both Workspace and Skill mounts."
  [mount-registry]
  (when-not (instance? clojure.lang.Atom mount-registry)
    (throw (err/error :filesystem/invalid-registry "mount registry must be an atom" {:registry mount-registry})))
  (->FilesystemProvider mount-registry))

(defn provider-stat
  "Stat path inside mount. Requires :stat in surface and lease."
  [provider mount-id path & [{:keys [leases] :as opts}]]
  (let [{:keys [mount canonical]} (check-effective-access! (:mount-registry provider) mount-id path :stat (or leases (:leases opts)) opts)]
    (backend/backend-stat (:backend mount) canonical)))

(defn provider-list
  "List immediate children of directory path inside mount. Requires :list."
  [provider mount-id path & [{:keys [leases] :as opts}]]
  (let [{:keys [mount canonical]} (check-effective-access! (:mount-registry provider) mount-id path :list (or leases (:leases opts)) opts)]
    (backend/backend-list (:backend mount) canonical)))

(defn provider-read
  "Read file at path inside mount. Requires :read."
  [provider mount-id path & [{:keys [leases] :as opts}]]
  (let [{:keys [mount canonical]} (check-effective-access! (:mount-registry provider) mount-id path :read (or leases (:leases opts)) opts)]
    (backend/backend-read (:backend mount) canonical)))

(defn provider-write
  "Overwrite existing file. Requires :write on both surface and lease."
  [provider mount-id path bytes & [{:keys [leases] :as opts}]]
  (let [{:keys [mount canonical]} (check-effective-access! (:mount-registry provider) mount-id path :write (or leases (:leases opts)) opts)]
    (backend/backend-write (:backend mount) canonical bytes)))

(defn provider-create
  "Create new file. Requires :create on both surface and lease."
  [provider mount-id path bytes & [{:keys [leases] :as opts}]]
  (let [{:keys [mount canonical]} (check-effective-access! (:mount-registry provider) mount-id path :create (or leases (:leases opts)) opts)]
    (backend/backend-create (:backend mount) canonical bytes)))

(defn provider-delete
  "Delete file or empty directory. Requires :delete."
  [provider mount-id path & [{:keys [leases] :as opts}]]
  (let [{:keys [mount canonical]} (check-effective-access! (:mount-registry provider) mount-id path :delete (or leases (:leases opts)) opts)]
    (backend/backend-delete (:backend mount) canonical)))

;; Aliases for convenience (fs- prefix)
(def fs-stat provider-stat)
(def fs-list provider-list)
(def fs-read provider-read)
(def fs-write provider-write)
(def fs-create provider-create)
(def fs-delete provider-delete)

;; ---------------------------------------------------------------------------
;; Provider that also implements evoclj.provider.protocol for broker integration
;; (optional — allows filesystem intents to flow through dispatch)
;; ---------------------------------------------------------------------------

(extend-type FilesystemProvider
  proto/Provider
  (describe [_]
    {:tool/id :filesystem/generic
     :effect :pure
     :input-schema [:map [:mount/id vector?] [:path :string] [:operation keyword?]]
     :output-schema :any
     :required-action :invoke})
  (normalize-request [_ intent]
    (let [args (or (get-in intent [:payload :args])
                   (get-in intent [:payload])
                   {})]
      (when-not (and (contains? args :mount/id) (contains? args :path))
        (throw (err/error :provider/input-invalid "filesystem request requires :mount/id and :path" {:args args})))
      (let [op (or (:operation args) :read)
            resource (canonical-resource (:mount/id args) (:path args))]
        (assoc resource :action op :args args))))
  (execute-request! [this authorized-request]
    (let [{:keys [mount/id path action]} authorized-request
          op (or action :read)
          leases (:leases authorized-request)
          ;; The broker already authorized subject/expiry during decide; the
          ;; provider re-enforces them fail-closed. Derive the requesting
          ;; subject from the request when present, else from the lease it
          ;; will authorize against (a self-match is a no-op — the broker is
          ;; responsible), and never skip expiry (default to the access clock).
          opts {:leases leases
                :subject (or (:subject authorized-request)
                             (some-> (first (or leases [])) :subject))
                :now (or (:now authorized-request) (java.util.Date.))}]
      (case op
        :read (provider-read this id path opts)
        :list (provider-list this id path opts)
        :stat (provider-stat this id path opts)
        :write (provider-write this id path (:bytes authorized-request) opts)
        :create (provider-create this id path (:bytes authorized-request) opts)
        :delete (provider-delete this id path opts)
        (throw (err/error :provider/input-invalid "unknown filesystem operation" {:operation op}))))))
