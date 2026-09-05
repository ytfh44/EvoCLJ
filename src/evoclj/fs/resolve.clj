(ns evoclj.fs.resolve
  "HostFilesystemObject authority — realpath-backed resolution shared by the
  capability descriptors and the filesystem mount provider.

  SPLIT (audit item 5 — filesystem authority is about OBJECTS, not strings):
    LogicalPath          — lexical authority over a pure-virtual namespace
                           (the CAS snapshot tree; no symlinks, junctions, or
                           reparse points can exist there). Lexical
                           canonicalization plus slash-boundary prefix
                           containment is SOUND there and stays fast and pure.
                           Owned by evoclj.capability.resource-kind
                           (`canonicalize-path`, `path-inside?`,
                           `canonicalize-mount-path`, `mount-path-inside?`).
    HostFilesystemObject — authority over real host filesystem OBJECTS.
                           Lexical containment is NOT sound here: a lease on
                           /work lexically covers /work/link/shadow even when
                           /work/link is a symlink to /etc. Every decision in
                           this namespace resolves the requested path with
                           realpath semantics and compares OBJECT identity
                           (real paths) — never a string prefix of the
                           unresolved request.

  Resolution algorithm (`resolve-under-root!`):
    1. Anchor on the real path of the scope root (`toRealPath`).
    2. Walk each requested segment from the anchor: a `.`/`..`/empty segment
       fails closed (callers pass canonical paths, so one can never appear);
       a component that IS a symlink (`Files/isSymbolicLink`, no-follow)
       fails closed — links are never followed, even ones whose target stays
       inside the scope (strict discipline, shared prior art with
       evoclj.fs.walk: no symlink anywhere on the resolved chain).
    3. Every EXISTING component is additionally re-anchored through
       `toRealPath` and required to stay under the anchor real path — this
       catches junctions, reparse points, and mount escapes that
       `isSymbolicLink` does not report.
    4. A nonexistent tail (create-path) is appended lexically to the deepest
       verified-real directory and prefix-checked; tail names cannot be links
       because they do not exist.

  TOCTOU residual (honest statement): the JVM exposes no `openat2` /
  `O_NOFOLLOW` directory-handle semantics, so resolution and effect are two
  steps. The provider guard runs them back-to-back inside the same privileged
  section (authorize -> resolve-and-compare -> act, no awaits and no user code
  between), which shrinks the race to a concurrent filesystem writer swapping
  a component in the microsecond window — an attacker that already has write
  access to the tree. Fully closing that needs OS support (Linux
  `openat2(RESOLVE_NO_SYMLINKS)`), a privileged file-server process holding
  directory handles, or an immutable snapshot: for threat-bearing trees
  prefer CAS mounts, whose manifest walk already rejects symlinks and whose
  content is immutable after snapshot."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err])
  (:import (java.nio.file Files LinkOption Path Paths)))

(def ^:private nofollow-links
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private follow-links
  (make-array LinkOption 0))

(defn- symlink-component?
  [^Path p]
  (Files/isSymbolicLink p))

(defn- exists-as-itself?
  [^Path p]
  (Files/exists p nofollow-links))

(defn anchor-real-path
  "Real path of an existing scope-root directory. Follows the root's own
  links (a mount root is validated symlink-free at construction) and throws
  :filesystem/root-invalid when the root is missing or not a directory."
  [root]
  (try
    (let [^Path p (cond
                    (instance? Path root) root
                    (string? root) (Paths/get ^String root (make-array String 0))
                    :else (throw (err/error :filesystem/root-invalid
                                            "scope root must be a Path or string"
                                            {})))
          ^Path real (.toRealPath p follow-links)]
      (when-not (Files/isDirectory real follow-links)
        (throw (err/error :filesystem/root-invalid
                          "scope root must be an existing directory"
                          {})))
      real)
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Exception e
      (throw (err/error :filesystem/root-invalid
                        "scope root is not resolvable"
                        {:cause (.getName (class e))})))))

(defn- canonical-segments!
  "Fail closed on any non-canonical segment. Callers pass canonical paths, so
  `.`/`..`/empty can never appear — seeing one means caller bug or attack."
  [segments]
  (doseq [s segments]
    (when (or (= s "..") (= s ".") (= s ""))
      (throw (err/error :filesystem/path-outside-mount
                        "path must be canonical (no . / .. / empty segments)"
                        {:segment (str s)}))))
  segments)

(defn resolve-under-root!
  "Strictly resolve `segments` under `anchor-real` (a real path from
  `anchor-real-path`). Returns {:real Path :existed? bool}.

  Fails closed with :filesystem/symlink-rejected when any component is a
  symlink, and :filesystem/path-outside-mount when any existing component's
  real path leaves the anchor (junction/reparse/mount escape) or the
  nonexistent tail would."
  [^Path anchor-real segments]
  (let [segs (vec (canonical-segments! segments))]
    (loop [^Path cur anchor-real
           remaining segs
           existed? true]
      (if (empty? remaining)
        {:real cur :existed? existed?}
        (let [seg (first remaining)
              ^Path child (.resolve cur ^String seg)]
          (cond
            (symlink-component? child)
            (throw (err/error :filesystem/symlink-rejected
                              "path component is a symlink; links are never followed"
                              {:segment (str seg)}))

            (exists-as-itself? child)
            (let [^Path real (.toRealPath child follow-links)]
              (when-not (.startsWith real anchor-real)
                (throw (err/error :filesystem/path-outside-mount
                                  "path component resolves outside the authorized scope"
                                  {:segment (str seg)})))
              (recur real (vec (rest remaining)) true))

            :else
            ;; Nonexistent tail: descendants of a missing path cannot exist, so
            ;; the whole tail is appended lexically to the deepest verified-real
            ;; directory and prefix-checked (belt and braces — canonical
            ;; segments contain no `..`, so the tail cannot climb).
            (let [^Path tail (reduce (fn [^Path p ^String s] (.resolve p s))
                                     cur remaining)
                  ^Path norm (.normalize tail)]
              (when-not (.startsWith norm anchor-real)
                (throw (err/error :filesystem/path-outside-mount
                                  "path resolves outside the authorized scope"
                                  {:segment (str seg)})))
              {:real norm :existed? false})))))))

(defn- split-mount-relative
  [rel]
  (if (= rel "")
    []
    (str/split rel #"/")))

(defn authorize-host-object!
  "Authorize mount-relative `request-rel` against the AUTHORIZING GRANT scope
  `grant-rel` (both canonical mount-namespace strings, \"\" = mount root)
  under host `root`. Resolves the grant scope strictly, then the request
  strictly, and requires the request OBJECT to stay inside the grant OBJECT.

  Returns {:anchor-real :grant-real :request-real :existed?}. Throws
  fail-closed: :filesystem/root-invalid, :filesystem/path-invalid,
  :filesystem/symlink-rejected, :filesystem/path-outside-mount."
  [root grant-rel request-rel]
  (when-not (and (string? grant-rel) (string? request-rel))
    (throw (err/error :filesystem/path-invalid
                      "grant and request paths must be strings"
                      {})))
  (let [^Path anchor (anchor-real-path root)
        grant-segs (split-mount-relative grant-rel)
        req-segs (split-mount-relative request-rel)]
    ;; Lexical scope gate first (necessary, not sufficient): the request must
    ;; name a path at-or-under the grant scope in the mount namespace.
    (when-not (or (= grant-rel "")
                  (= grant-rel request-rel)
                  (str/starts-with? request-rel (str grant-rel "/")))
      (throw (err/error :filesystem/path-outside-mount
                        "request is outside the granted scope"
                        {})))
    (let [^Path grant-real (:real (resolve-under-root! anchor grant-segs))
          res (resolve-under-root! anchor req-segs)]
      ;; Object-identity gate (sufficient): the resolved request object must
      ;; stay inside the resolved grant object.
      (when-not (or (= grant-rel "")
                    (.startsWith ^Path (:real res) grant-real))
        (throw (err/error :filesystem/path-outside-mount
                          "request object is outside the granted object"
                          {})))
      {:anchor-real anchor
       :grant-real grant-real
       :request-real (:real res)
       :existed? (:existed? res)})))

(defn- absolute-path
  "Normalized absolute Path for s, or nil (non-string, relative, garbage)."
  [s]
  (when (string? s)
    (try
      (let [^Path p (.normalize (Paths/get ^String s (make-array String 0)))]
        (when (.isAbsolute p) p))
      (catch Exception _ nil))))

(defn authorize-host-absolute!
  "Strict absolute-host variant of `authorize-host-object!`: `grant-abs` and
  `request-abs` are absolute host path strings. Returns the same resolution
  map; throws fail-closed."
  [grant-abs request-abs]
  (let [^Path gp (absolute-path grant-abs)
        ^Path rp (absolute-path request-abs)]
    (when (or (nil? gp) (nil? rp))
      (throw (err/error :filesystem/path-invalid
                        "host object paths must be absolute strings"
                        {})))
    (when-not (.startsWith rp gp)
      (throw (err/error :filesystem/path-outside-mount
                        "request is lexically outside the grant"
                        {})))
    (let [^Path anchor (anchor-real-path gp)
          tail (mapv str (iterator-seq (.iterator (.relativize gp rp))))
          res (resolve-under-root! anchor tail)]
      {:anchor-real anchor
       :grant-real anchor
       :request-real (:real res)
       :existed? (:existed? res)})))

(defn proven-host-escape?
  "True ONLY when realpath evidence PROVES `request-abs` escapes `grant-abs`
  (a symlink on the chain, or the resolved object outside the grant object).
  False when unverifiable (missing files, IO errors, non-absolute input) — the
  lexical LogicalPath decision then stands, and the effect layer re-checks
  before acting."
  [grant-abs request-abs]
  (try
    (authorize-host-absolute! grant-abs request-abs)
    false
    (catch clojure.lang.ExceptionInfo e
      (contains? #{:filesystem/symlink-rejected :filesystem/path-outside-mount}
                 (:error/type (ex-data e))))
    (catch Exception _ false)))
(defn covers-host-object?
  "Strict HostFilesystemObject cover decision for absolute host paths: true
  only when the request names an EXISTING object that provably resolves
  inside the grant object right now. Fail-closed (false) when unverifiable —
  including nonexistent paths (a name with no object has no object identity;
  the effect-time guard `authorize-host-absolute!` / `authorize-host-object!`
  handles create-tails explicitly via its `:existed?` flag)."
  [grant-abs request-abs]
  (try
    (boolean (let [res (authorize-host-absolute! grant-abs request-abs)]
               (:existed? res)))
    (catch Exception _ false)))
