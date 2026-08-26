(ns evoclj.environment.surface
  "Peer Surface abstraction for the dynamic environment.

  Three peer types exist at the same level, no hierarchy:

  - ContextSurface  {:surface/type :context   :surface/id :descriptor :materializer}
  - ToolSurface     {:surface/type :tools    :surface/id :entries {...}}
  - DirectorySurface{:surface/type :directory :surface/id :backend :access/max #{...}}

  Directory access uses a capability set instead of a binary RO/RW flag.
  Currently supported:

    #{:read :list :stat}                              ; read-only
    #{:read :list :stat :write :create :delete}       ; read-write

  Future capabilities not implemented now but reserved for extension:
  append-only, no-delete, write-existing-only, execute.
  They are mentioned here for forward compatibility but are not accepted
  by validation at present.

  All surfaces are plain maps. Validation is explicit and throws typed
  errors via evoclj.kernel.error."
  (:require [evoclj.kernel.error :as err]))

(def valid-capabilities
  "Currently accepted directory capabilities."
  #{:read :list :stat :write :create :delete})

(def future-capabilities
  "Reserved future extensions, not implemented now:
   append-only, no-delete, write-existing-only, execute."
  #{:append-only :no-delete :write-existing-only :execute})

(defn valid-access-max?
  [access]
  (and (set? access)
       (seq access)
       (every? valid-capabilities access)))

(defn- require-surface-id [m]
  (when-not (:surface/id m)
    (throw (err/error :surface/invalid-descriptor "missing :surface/id" {:surface m})))
  m)

(defn- validate-directory-access [access]
  (when-not (valid-access-max? access)
    (throw (err/error :surface/invalid-descriptor "invalid :access/max capability set"
                      {:access access :valid valid-capabilities})))
  ;; ensure future capabilities are not used yet
  (when (some future-capabilities access)
    (throw (err/error :surface/invalid-descriptor "future capability not yet supported"
                      {:access access :future future-capabilities}))))

(defn context-surface?
  [x]
  (and (map? x) (= :context (:surface/type x))))

(defn tool-surface?
  [x]
  (and (map? x) (= :tools (:surface/type x))))

(defn directory-surface?
  [x]
  (and (map? x) (= :directory (:surface/type x))))

(defn surface?
  [x]
  (or (context-surface? x) (tool-surface? x) (directory-surface? x)))

(defn validate-context-surface
  [m]
  (when-not (context-surface? m)
    (throw (err/error :surface/invalid-descriptor "not a ContextSurface" {:surface m})))
  (require-surface-id m)
  (when-not (contains? m :descriptor)
    (throw (err/error :surface/invalid-descriptor "ContextSurface missing :descriptor" {:surface m})))
  (when-not (contains? m :materializer)
    (throw (err/error :surface/invalid-descriptor "ContextSurface missing :materializer" {:surface m})))
  (when (nil? (:descriptor m))
    (throw (err/error :surface/invalid-descriptor "ContextSurface :descriptor must not be nil" {:surface m})))
  m)

(defn validate-tool-surface
  [m]
  (when-not (tool-surface? m)
    (throw (err/error :surface/invalid-descriptor "not a ToolSurface" {:surface m})))
  (require-surface-id m)
  (when-not (contains? m :entries)
    (throw (err/error :surface/invalid-descriptor "ToolSurface missing :entries" {:surface m})))
  (when-not (map? (:entries m))
    (throw (err/error :surface/invalid-descriptor "ToolSurface :entries must be a map" {:surface m})))
  m)

(defn validate-directory-surface
  [m]
  (when-not (directory-surface? m)
    (throw (err/error :surface/invalid-descriptor "not a DirectorySurface" {:surface m})))
  (require-surface-id m)
  (when-not (contains? m :backend)
    (throw (err/error :surface/invalid-descriptor "DirectorySurface missing :backend" {:surface m})))
  (when-not (contains? m :access/max)
    (throw (err/error :surface/invalid-descriptor "DirectorySurface missing :access/max" {:surface m})))
  (validate-directory-access (:access/max m))
  m)

(defn validate-surface
  "Dispatch validation by :surface/type."
  [m]
  (case (:surface/type m)
    :context (validate-context-surface m)
    :tools (validate-tool-surface m)
    :directory (validate-directory-surface m)
    (throw (err/error :surface/invalid-descriptor "unknown :surface/type" {:surface m}))))

(defn make-context-surface
  "Create a ContextSurface. Requires :id, :descriptor, :materializer."
  [{:keys [id descriptor materializer] :as opts}]
  (let [m {:surface/type :context
           :surface/id id
           :descriptor descriptor
           :materializer materializer}]
    (validate-context-surface m)
    (if-let [rev (:revision/id opts)]
      (assoc m :revision/id rev)
      m)))

(defn make-tool-surface
  "Create a ToolSurface. Requires :id and :entries map."
  [{:keys [id entries] :as opts}]
  (let [m {:surface/type :tools
           :surface/id id
           :entries (or entries {})}]
    (validate-tool-surface m)
    (if-let [rev (:revision/id opts)]
      (assoc m :revision/id rev)
      m)))

(defn make-directory-surface
  "Create a DirectorySurface. Requires :id, :backend, :access/max.
   :access/max must be a subset of #{:read :list :stat :write :create :delete}.
   Future capabilities append-only, no-delete, write-existing-only, execute
   are reserved but not implemented."
  [{:keys [id backend access-max] :as opts}]
  (let [m {:surface/type :directory
           :surface/id id
           :backend backend
           :access/max access-max}]
    (validate-directory-surface m)
    (if-let [rev (:revision/id opts)]
      (assoc m :revision/id rev)
      m)))
