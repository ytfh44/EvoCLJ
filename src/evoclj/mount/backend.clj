(ns evoclj.mount.backend
  "Mount and Backend foundation for unified filesystem.

  Mount map shape (required):
    {:mount/id <vector> :backend <Backend> :access/max #{capabilities}}

  Mount id is a logical namespace, e.g.:
    [:skill \"debugging\" \"sha256:...\"] or [:workspace \"project\"]

  Two backends:
    HostDirectoryBackend — live reads/writes against a host directory (Workspace)
    CASTreeBackend       — immutable snapshot tree stored in CAS (Skill)

  Access sets:
    Workspace: #{:read :list :stat :write :create :delete}
    Skill:     #{:read :list :stat} (read-only)

  The backend is opaque; callers route through the mount registry and
  filesystem provider, never by host absolute path."
  (:require [clojure.string :as str]
            [evoclj.fs.path :as fs-path]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [clojure.java.io :as io])
  (:import (java.nio.file Files LinkOption Path Paths)
           (java.nio.file.attribute BasicFileAttributes)
           (java.nio.charset StandardCharsets)))

(def workspace-access
  "Full RW access for Workspace mounts."
  #{:read :list :stat :write :create :delete})

(def skill-access
  "Read-only access for Skill snapshot mounts."
  #{:read :list :stat})

(def valid-capabilities
  #{:read :list :stat :write :create :delete})

(defn mount-id?
  "True when x is a vector with keyword head, e.g. [:skill \"name\" \"sha256:...\"] or [:workspace \"id\"]."
  [x]
  (and (vector? x)
       (seq x)
       (keyword? (first x))
       (every? string? (rest x))))

(defn valid-access?
  [access]
  (and (set? access)
       (seq access)
       (every? valid-capabilities access)))

(defn canonicalize-mount-path
  "Canonicalize a mount-relative path.

  - Backslashes -> slashes
  - Must be relative (no leading /, no drive letter)
  - Resolves \".\" and \"..\" ; \"..\" that would escape mount throws :filesystem/path-outside-mount
  - Empty string or \".\" denotes mount root -> \"\"
  - Uses fs/path for remaining validation (NUL, empty components etc)"
  [raw]
  (when-not (string? raw)
    (throw (err/error :filesystem/path-invalid "path must be a string" {:path raw})))
  (when (str/includes? raw "\u0000")
    (throw (err/error :filesystem/path-invalid "path must not contain NUL" {:path raw})))
  (let [s (str/replace raw "\\" "/")]
    (when (or (str/starts-with? s "/")
              (re-matches #"^[A-Za-z]:.*" s))
      (throw (err/error :filesystem/path-invalid "path must be relative within mount" {:path raw})))
    (if (or (= s "") (= s ".") (= s "./") (= s "/"))
      ""
      (let [parts (str/split s #"/" -1)]
        (when (some #(= "" %) parts)
          (throw (err/error :filesystem/path-invalid "path must not contain empty components" {:path raw})))
        (let [stack (reduce (fn [acc seg]
                              (cond
                                (= seg ".") acc
                                (= seg "..") (if (seq acc)
                                               (pop acc)
                                               (throw (err/error :filesystem/path-outside-mount
                                                                 "path escapes mount"
                                                                 {:path raw})))
                                :else (conj acc seg)))
                            [] parts)
              canonical (str/join "/" stack)]
          (if (= canonical "")
            ""
            (do
              ;; delegate remaining validation to fs/path (drive, NUL already checked, but ensure no . or .. left)
              (try
                (fs-path/normalize-relative-path canonical)
                (catch clojure.lang.ExceptionInfo e
                  (if (= :fs/path-invalid (:error/type (ex-data e)))
                    (throw (err/error :filesystem/path-invalid (ex-message e) (dissoc (ex-data e) :error/type)))
                    (throw e))))
              canonical)))))))

(defn- host-resolve
  "Resolve mount-relative canonical path against host root Path."
  [^Path root rel]
  (if (= rel "")
    root
    (let [parts (str/split rel #"/")]
      (reduce (fn [^Path p seg] (.resolve p ^String seg)) root parts))))

;; ---------------------------------------------------------------------------
;; Backend protocol
;; ---------------------------------------------------------------------------

(defprotocol Backend
  (backend-type [this] "Keyword :host-directory or :cas-tree")
  (backend-read [this rel-path] "Return bytes for file at rel-path, else throw :filesystem/not-found")
  (backend-stat [this rel-path] "Return {:path :type :size} for file or directory")
  (backend-list [this rel-path] "List immediate children of directory rel-path -> vector of {:path :name :type}")
  (backend-write [this rel-path bytes] "Overwrite existing file, fail if not exists or read-only")
  (backend-create [this rel-path bytes] "Create new file, fail if exists or read-only")
  (backend-delete [this rel-path] "Delete file or empty dir"))

;; ---------------------------------------------------------------------------
;; HostDirectoryBackend
;; ---------------------------------------------------------------------------

(defrecord HostDirectoryBackend [root]
  Backend
  (backend-type [_] :host-directory)
  (backend-read [_ rel-path]
    (let [p (canonicalize-mount-path rel-path)
          target (host-resolve root p)]
      (when-not (Files/exists target (make-array LinkOption 0))
        (throw (err/error :filesystem/not-found "file not found" {:path rel-path :canonical p})))
      (when (Files/isDirectory target (make-array LinkOption 0))
        (throw (err/error :filesystem/is-directory "path is a directory" {:path rel-path})))
      (when (Files/isSymbolicLink target)
        (throw (err/error :filesystem/symlink-rejected "symlink not allowed" {:path rel-path})))
      (Files/readAllBytes target)))
  (backend-stat [_ rel-path]
    (let [p (canonicalize-mount-path rel-path)
          target (host-resolve root p)]
      (when-not (Files/exists target (make-array LinkOption 0))
        (throw (err/error :filesystem/not-found "not found" {:path rel-path})))
      (let [is-dir (Files/isDirectory target (make-array LinkOption 0))
            is-file (Files/isRegularFile target (make-array LinkOption 0))
            size (if is-file (Files/size target) 0)]
        {:path p :type (cond is-dir :directory is-file :file :else :other) :size size})))
  (backend-list [_ rel-path]
    (let [p (canonicalize-mount-path rel-path)
          target (host-resolve root p)]
      (when-not (Files/exists target (make-array LinkOption 0))
        (throw (err/error :filesystem/not-found "directory not found" {:path rel-path})))
      (when-not (Files/isDirectory target (make-array LinkOption 0))
        (throw (err/error :filesystem/not-directory "not a directory" {:path rel-path})))
      (with-open [stream (Files/list target)]
        (->> (.iterator stream)
             iterator-seq
             (map (fn [^Path child]
                    (let [name (str (.getFileName child))
                          full (if (= p "") name (str p "/" name))
                          is-dir (Files/isDirectory child (make-array LinkOption 0))]
                      {:path full :name name :type (if is-dir :directory :file)})))
             (sort-by :name)
             vec))))
  (backend-write [_ rel-path bytes]
    (let [p (canonicalize-mount-path rel-path)
          target (host-resolve root p)]
      (when (= p "") (throw (err/error :filesystem/is-directory "cannot write to directory" {:path rel-path})))
      (when-not (Files/exists target (make-array LinkOption 0))
        (throw (err/error :filesystem/not-found "file does not exist for write" {:path rel-path})))
      (when (Files/isDirectory target (make-array LinkOption 0))
        (throw (err/error :filesystem/is-directory "path is a directory" {:path rel-path})))
      (let [ba (if (bytes? bytes) bytes (.getBytes (str bytes) StandardCharsets/UTF_8))
            parent (.getParent target)]
        (when parent (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0)))
        (Files/write target ba (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/WRITE java.nio.file.StandardOpenOption/TRUNCATE_EXISTING]))
        {:path p :size (alength ba)})))
  (backend-create [_ rel-path bytes]
    (let [p (canonicalize-mount-path rel-path)
          target (host-resolve root p)]
      (when (= p "") (throw (err/error :filesystem/is-directory "cannot create directory as file" {:path rel-path})))
      (when (Files/exists target (make-array LinkOption 0))
        (throw (err/error :filesystem/already-exists "file already exists" {:path rel-path})))
      (let [ba (if (bytes? bytes) bytes (.getBytes (str bytes) StandardCharsets/UTF_8))
            parent (.getParent target)]
        (when parent (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0)))
        (Files/write target ba (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE_NEW java.nio.file.StandardOpenOption/WRITE]))
        {:path p :size (alength ba)})))
  (backend-delete [_ rel-path]
    (let [p (canonicalize-mount-path rel-path)
          target (host-resolve root p)]
      (when (= p "") (throw (err/error :filesystem/is-directory "cannot delete mount root" {:path rel-path})))
      (when-not (Files/exists target (make-array LinkOption 0))
        (throw (err/error :filesystem/not-found "not found" {:path rel-path})))
      (Files/delete target)
      {:path p :deleted true})))

(defn host-directory-backend
  "Create HostDirectoryBackend from host directory root (string or Path)."
  [root]
  (let [^Path p (cond
                 (instance? Path root) root
                 (string? root) (Paths/get root (make-array String 0))
                 :else (throw (err/error :mount/invalid-root "root must be Path or string" {:root root})))]
    (when (Files/isSymbolicLink p)
      (throw (err/error :mount/invalid-root "root must not be symlink" {:path (str p)})))
    (when-not (Files/isDirectory p (make-array LinkOption 0))
      (throw (err/error :mount/invalid-root "root must be existing directory" {:path (str p)})))
    (->HostDirectoryBackend p)))

;; ---------------------------------------------------------------------------
;; CASTreeBackend (immutable snapshot)
;; ---------------------------------------------------------------------------

(defrecord CASTreeBackend [cas tree-id manifest]
  Backend
  (backend-type [_] :cas-tree)
  (backend-read [_ rel-path]
    (let [p (canonicalize-mount-path rel-path)]
      (when (= p "") (throw (err/error :filesystem/is-directory "path is a directory" {:path rel-path})))
      (if-let [{:keys [artifact/id]} (get-in manifest [:entries p])]
        (cas/get-bytes cas id)
        (throw (err/error :filesystem/not-found "file not found in tree" {:path rel-path :tree/id tree-id})))))
  (backend-stat [_ rel-path]
    (let [p (canonicalize-mount-path rel-path)]
      (cond
        (= p "")
        {:path "" :type :directory :size (count (:entries manifest))}
        (contains? (:entries manifest) p)
        (let [{:keys [size]} (get-in manifest [:entries p])]
          {:path p :type :file :size (or size 0)})
        ;; check if directory (any entry with prefix p/)
        (some (fn [[k _]] (or (= k p) (str/starts-with? k (str p "/")))) (:entries manifest))
        {:path p :type :directory :size 0}
        :else
        (throw (err/error :filesystem/not-found "not found in tree" {:path rel-path})))))
  (backend-list [_ rel-path]
    (let [p (canonicalize-mount-path rel-path)]
      ;; verify directory exists
      (when-not (or (= p "")
                    (contains? (:entries manifest) p)
                    (some (fn [[k _]] (str/starts-with? k (str p "/"))) (:entries manifest)))
        (throw (err/error :filesystem/not-found "directory not found" {:path rel-path})))
      (when (contains? (:entries manifest) p)
        (throw (err/error :filesystem/not-directory "path is a file, not directory" {:path rel-path})))
      (let [prefix (if (= p "") "" (str p "/"))
            children (reduce (fn [acc [k _]]
                               (if (or (= p "")
                                       (str/starts-with? k prefix))
                                 (let [rest (if (= p "") k (subs k (count prefix)))
                                       seg (first (str/split rest #"/"))]
                                   (if seg
                                     (let [full (if (= p "") seg (str p "/" seg))
                                           is-file (contains? (:entries manifest) full)
                                           deeper (some (fn [[kk _]] (and (not= kk full) (str/starts-with? kk (str full "/")))) (:entries manifest))
                                           typ (cond is-file (if deeper :directory :file) deeper :directory :else :file)]
                                       (assoc acc full {:path full :name seg :type typ}))
                                     acc))
                                 acc))
                             {} (:entries manifest))]
        (->> children vals (sort-by :name) vec))))
  (backend-write [_ _ _]
    (throw (err/error :mount/read-only "CAS tree is read-only" {})))
  (backend-create [_ _ _]
    (throw (err/error :mount/read-only "CAS tree is read-only" {})))
  (backend-delete [_ _]
    (throw (err/error :mount/read-only "CAS tree is read-only" {}))))

(defn cas-tree-backend
  "Create CASTreeBackend from CAS config and tree-id (\"sha256:...\" manifest id).

  Loads manifest from CAS; tree content is immutable thereafter.
  Mutating the upstream host directory after snapshot does not affect this backend."
  [cas tree-id]
  (when-not (string? tree-id)
    (throw (err/error :mount/invalid-tree-id "tree-id must be sha256 string" {:tree/id tree-id})))
  (let [manifest (snapshot/load-tree cas tree-id)]
    (when-not (map? manifest)
      (throw (err/error :mount/invalid-tree "manifest must be a map" {:tree/id tree-id})))
    (->CASTreeBackend cas tree-id manifest)))

;; ---------------------------------------------------------------------------
;; Mount
;; ---------------------------------------------------------------------------

(defn validate-mount
  "Validate mount map shape. Throws :mount/invalid on failure.

  WO-B3: :mount/id MUST be a canonical vector id (mount-id?) — never a
  bare scalar :surface/id. The :backend may satisfy the Backend protocol
  OR be a plain descriptor map (a not-yet-realized backend such as
  {:type :cas-tree :tree/id ...}); the filesystem provider enforces
  realizability at operation time, so a descriptor mount fails-closed at
  use rather than being rejected here (the degraded-descriptor path is
  retained so publication stays a single canonical register-mount!)."
  [m]
  (when-not (map? m)
    (throw (err/error :mount/invalid "mount must be a map" {:mount m})))
  (when-not (mount-id? (:mount/id m))
    (throw (err/error :mount/invalid "invalid :mount/id" {:mount m})))
  (when-not (:backend m)
    (throw (err/error :mount/invalid "missing :backend" {:mount m})))
  (when-not (or (satisfies? Backend (:backend m))
                (map? (:backend m)))
    (throw (err/error :mount/invalid "backend must satisfy Backend protocol or be a descriptor map" {:mount m})))
  (when-not (valid-access? (:access/max m))
    (throw (err/error :mount/invalid "invalid :access/max" {:mount m :valid valid-capabilities})))
  m)

(defn make-mount
  "Create a Mount map. Validates shape."
  [{:keys [mount-id backend access-max]}]
  (let [m {:mount/id mount-id :backend backend :access/max access-max}]
    (validate-mount m)
    m))

(defn make-host-mount
  "Create a Workspace-style host directory mount (RW)."
  [mount-id root-path]
  (make-mount {:mount-id mount-id
               :backend (host-directory-backend root-path)
               :access-max workspace-access}))

(defn make-skill-mount
  "Create a Skill-style CAS tree mount (RO)."
  [mount-id cas tree-id]
  (make-mount {:mount-id mount-id
               :backend (cas-tree-backend cas tree-id)
               :access-max skill-access}))

;; Convenience constructors matching requirement naming
(defn HostDirectoryBackend*
  "Alias constructor for compatibility."
  [root] (host-directory-backend root))

(defn CASTreeBackend*
  "Alias constructor for compatibility."
  [cas tree-id] (cas-tree-backend cas tree-id))

;; ---------------------------------------------------------------------------
;; Mount registry (simple atom map mount-id -> mount)
;; ---------------------------------------------------------------------------

(defn create-registry
  "Create an empty mount registry atom."
  []
  (atom {}))

(defn register-mount!
  "Register mount in registry. Throws :mount/collision if id already present."
  [registry mount]
  (validate-mount mount)
  (let [id (:mount/id mount)]
    (swap! registry (fn [m]
                      (when (contains? m id)
                        (throw (err/error :mount/collision "mount id already registered" {:mount/id id})))
                      (assoc m id mount)))
    mount))

(defn get-mount
  "Lookup mount by id in registry, or nil."
  [registry mount-id]
  (get @registry mount-id))

(defn list-mounts
  "List all mounts in registry."
  [registry]
  (vals @registry))

(defn unregister-mount!
  "Remove mount from registry."
  [registry mount-id]
  (swap! registry dissoc mount-id)
  nil)
