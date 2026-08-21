(ns evoclj.mount.filesystem
  "Generic filesystem provider serving both Workspace (RW) and Skill (RO) mounts.

  Single provider instance takes a mount registry (atom map mount-id -> mount)
  and routes every operation by logical mount namespace.

  Canonical resource for filesystem uses logical mount namespace:
    {:kind :filesystem/path :mount/id [:skill \"debugging\" \"sha256:...\"] :path \"references/foo.md\"}
  Never a host absolute path like /home/x/.agents/skills/...

  Enforcement: EffectiveAccess = SurfaceAccessMax ∩ CapabilityLease
    - SurfaceAccessMax is mount's :access/max (e.g. Skill RO = #{:read :list :stat})
    - CapabilityLease is the lease's :actions set and :resource scope
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

  This provider is the single filesystem I/O path. If a legacy read_skill_file
  helper exists it must be a facade over this provider (no independent I/O stack)."
  (:require [clojure.string :as str]
            [evoclj.mount.backend :as backend]
            [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]))

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
;; Lease helpers — support both full CapabilityLease ({:resource {:mount/id :path} :actions})
;; and simplified test leases ({:mount/id :path :actions} or {:lease/id ...})
;; ---------------------------------------------------------------------------

(defn- lease-mount-id [lease]
  (or (get-in lease [:resource :mount/id])
      (:mount/id lease)
      (:mount-id lease)))

(defn- lease-path [lease]
  (or (get-in lease [:resource :path])
      (:path lease)
      ""))

(defn- lease-actions [lease]
  (cond
    (contains? lease :actions) (:actions lease)
    (contains? lease :action) #{(:action lease)}
    (contains? lease :capabilities) (:capabilities lease)
    :else #{}))

(defn- mount-path-inside?
  "True when request path is inside grant path (segment boundary).
  Empty grant path covers whole mount. Both are canonicalized."
  [grant-path req-path]
  (let [g (backend/canonicalize-mount-path (or grant-path ""))
        r (backend/canonicalize-mount-path (or req-path ""))]
    (or (= g "")
        (= g r)
        (str/starts-with? r (str g "/")))))

(defn- lease-grants?
  "True when lease grants action for mount-id + req-path.
  Checks mount-id equality, path prefix, and action membership.
  If lease carries :issued-at/:expires-at, expiry is enforced when :now supplied.
  If lease carries :subject, subject matching is enforced when :subject supplied."
  [lease mount-id req-path action {:keys [now subject]}]
  (let [lm (lease-mount-id lease)
        acts (lease-actions lease)]
    (and (some? lm)
         (= lm mount-id)
         (contains? acts action)
         (mount-path-inside? (lease-path lease) req-path)
         ;; optional expiry check if lease is full CapabilityLease
         (if (and (:issued-at lease) (:expires-at lease) now)
           (try
             (let [valid? (and (not (.before ^java.util.Date now ^java.util.Date (:issued-at lease)))
                               (.before ^java.util.Date now ^java.util.Date (:expires-at lease)))]
               valid?)
             (catch Exception _ false))
           true)
         ;; optional subject check
         (if (and (:subject lease) subject)
           (= (:phenotype/id (:subject lease)) (:phenotype/id subject))
           true))))

(defn- authorized?
  "True when any lease in collection grants action for resource.
  Surface check is separate (EffectiveAccess is intersection)."
  [leases mount-id req-path action opts]
  (some #(lease-grants? % mount-id req-path action opts) leases))

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
;; Facade for legacy read_skill_file (if any) — routes through generic provider
;; ---------------------------------------------------------------------------

(defn read-skill-file
  "Legacy entry point that must be a facade over the generic provider.
  Prefer provider-read directly."
  [provider mount-id path opts]
  (provider-read provider mount-id path opts))

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
          opts {:leases leases}]
      (case op
        :read (provider-read this id path opts)
        :list (provider-list this id path opts)
        :stat (provider-stat this id path opts)
        :write (provider-write this id path (:bytes authorized-request) opts)
        :create (provider-create this id path (:bytes authorized-request) opts)
        :delete (provider-delete this id path opts)
        (throw (err/error :provider/input-invalid "unknown filesystem operation" {:operation op}))))))
