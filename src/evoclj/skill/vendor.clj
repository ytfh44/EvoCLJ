(ns evoclj.skill.vendor
  "Vendor external Skill snapshot revision to Genome-owned skills/.

  Principle: Evolution may only mutate Genome-owned assets. Installed
  external Skills (~/.agents/skills, project/.agents/skills discovered
  via SkillSource) are NOT part of Genome, so mutation engine never
  receives that write target. If evolution of a Skill is desired, the
  explicit flow is:

    external Skill -> vendor/fork -> Genome skills/<name>/* -> candidate-owned asset -> Mutation IR

  Vendor MUST copy the immutable snapshot revision (CAS tree id), not a
  live host path. Upstream changes after vendor therefore do not affect
  the vendored copy. The copy lives under <genome-root>/skills/<name>/;
  all bundle files participate in Genome identity (load-genome tree-digest
  includes every file), and :skills is already a declared mutable class,
  so vendored files are immediately evolvable via existing Mutation IR
  without manifest edits.

  ATOMIC INSTALL (WO-S6 — tmp + verify + rename):
  vendor-skill! never writes directly into the final dest. It stages the
  whole tree into a unique temporary directory
  <genome-root>/skills/<vendor-staging-prefix><uuid>, VERIFIES that the
  staged copy is complete and correct (entry set, per-file size, per-file
  content hash against the CAS artifact id), then ATOMICALLY renames the
  staging directory onto the final dest-skill-dir. The result is that the
  dest is always either FULLY PRESENT (complete tree) or ABSENT — never a
  torn/mid-copy partial. A crash/failure before the rename leaves the OLD
  dest untouched (or absent on a first install) and at most an orphaned
  staging directory, which is cleaned later (below).

  ORPHAN CLEANUP (WO-S6 — fail-closed):
  A staging directory left behind by an interrupted/failed install is
  removed deterministically at the START of every install (and can be
  invoked directly via cleanup-orphan-staging!). The sweep is fail-closed:
  a staging dir it cannot delete, or a containment violation while
  sweeping, raises a typed error rather than silently leaving garbage.
  Staging directories that belong to a CONCURRENT in-process install are
  tracked in a register and never swept by another install.

  PATH SAFETY (fail-closed, typed — preserved from WO-S6P):
    - Segment-level '..' — every manifest entry path is validated on its
      segment list BEFORE any filesystem resolution; a '..' segment (or an
      absolute / backslash-crossing path) is rejected typed
      (:skill/vendor-path-escape / :skill/vendor-invalid-args), so a path
      can never be resolved out of the skill dir by lexical traversal.
    - realpath containment — the canonical (symlink-resolved) path of the
      destination's deepest existing ancestor must stay INSIDE the canonical
      path of <genome-root>. A symlinked <genome-root>/skills (or any
      intermediate component) that escapes the genome root is rejected typed
      (:skill/vendor-path-escape) BEFORE any destructive delete or write
      reaches the escaped target.
    - Delete-防-symlink-越界 — deleting a stale skill dir (or an orphan
      staging dir) walks the tree with NOFOLLOW_LINKS: a symbolic link is
      removed AS A LINK, never followed, so a link inside
      <genome-root>/skills/<name> cannot be used to delete files outside
      the root.

  CONCURRENCY (WO-S6):
  Two installs targeting the SAME skill-name serialize (per-skill-name
  lock), so the final dest is always one complete winner. Two installs into
  DIFFERENT skills may proceed concurrently; their staging directories have
  unique names (no collision) and the in-process staging register keeps one
  install's orphan sweep from deleting another's active staging."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types])
  (:import (java.nio.file Files Path Paths LinkOption StandardCopyOption)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)
           (java.util UUID)
           (java.util.concurrent.locks ReentrantLock)))

;; The documented staging-directory name convention. Tests and callers use
;; this same literal to fabricate a crash-orphan and to detect stray staging.
(def vendor-staging-prefix ".vendor-staging-")

(defn- coerce-path
  ^Path [x]
  (cond
    (instance? Path x) x
    (string? x) (Paths/get x (make-array String 0))
    :else (throw (err/error :skill/vendor-invalid-args "genome root must be Path or string" {:value (err/sanitize x)}))))

(defn- validate-skill-name!
  [n]
  (when-not (and (string? n) (not (str/blank? n)))
    (throw (err/error :skill/vendor-invalid-name "skill name must be non-empty string" {:skill/name n})))
  (when (or (str/includes? n "/") (str/includes? n "\\"))
    (throw (err/error :skill/vendor-invalid-name "skill name must be single path component (no slash)" {:skill/name n})))
  (when (contains? #{"." ".."} n)
    (throw (err/error :skill/vendor-invalid-name "skill name must not be '.' or '..'" {:skill/name n})))
  n)

(defn- validate-tree-id!
  [id]
  (when-not (types/artifact-id? id)
    (throw (err/error :skill/vendor-invalid-args "tree/id must be sha256:<64 hex>" {:tree/id id})))
  id)

(def ^:private nofollow-links
  "LinkOption array meaning NOFOLLOW_LINKS for the Files checks below, so a
  symbolic link is recognized AS A LINK (and, on delete, removed as a link)
  instead of being followed to an outside target."
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- validate-relative-path!
  "Validate a manifest entry's relative path at the SEGMENT level, before any
  filesystem resolution or write.

  Rejects (typed, fail-closed):
    - non-string / blank / backslash-containing paths (structural invalidity,
      :skill/vendor-invalid-args) — backslash is the Windows separator, so a
      backslash-crossing entry is an alternative traversal spelling;
    - absolute paths (window- and drive-qualified, caught via .isAbsolute:
      :skill/vendor-invalid-args);
    - any '..' segment (lexical escape of the skill dir, :skill/vendor-path-escape)
      — rejected on the segment list itself, not merely after .normalize.

  A well-formed forward-slash relative path (a literal name like 'a..b' is a
  single segment, NOT a '..', so it is allowed) is returned unchanged.

  Returns the validated path string."
  [rel]
  (when-not (string? rel)
    (throw (err/error :skill/vendor-invalid-args "manifest entry path must be string" {:path rel})))
  (when (str/blank? rel)
    (throw (err/error :skill/vendor-invalid-args "manifest entry path must be non-empty" {:path rel})))
  (when (str/includes? rel "\\")
    (throw (err/error :skill/vendor-invalid-args
                      "manifest entry path must not contain backslash" {:path rel})))
  (when (.isAbsolute (Paths/get rel (make-array String 0)))
    (throw (err/error :skill/vendor-invalid-args "vendored path must be relative" {:path rel})))
  (when (some #{".."} (str/split (str/replace rel "\\" "/") #"/"))
    (throw (err/error :skill/vendor-path-escape
                      "vendored path escapes skill dir (segment-level ..)"
                      {:path rel})))
  rel)

(defn- real-path
  "Canonical (symlink-resolved) path of `p`. Follows symlinks, i.e. returns
  where `p` actually points — the target a write/read would really touch."
  ^Path [^Path p]
  (.toRealPath p (make-array LinkOption 0)))

(defn- deepest-existing
  "The nearest existing ancestor of `p` (NOFOLLOW existence, so a symbolic
  link counts as existing). Walks up toward the filesystem root if needed."
  ^Path [^Path p]
  (loop [q p]
    (if (Files/exists q nofollow-links)
      q
      (let [r (.getParent q)]
        (if r (recur r) q)))))

(defn- ensure-contained-in-genome-root!
  "Fail-closed realpath containment check.

  The canonical (symlink-resolved) path of the deepest existing ancestor of
  `target` must live INSIDE the canonical path of `genome-root`. If `target`
  (or any component) traverses a symlink out of the genome root — e.g. a
  symlinked <genome-root>/skills pointing somewhere else — the resolved
  anchor escapes and this throws :skill/vendor-path-escape BEFORE any
  destructive delete or write reaches the escaped location.

  A path whose anchor cannot be canonicalized (e.g. a dangling symlink) also
  throws :skill/vendor-path-escape (fail-closed), never silently accepted."
  [^Path genome-root ^Path target]
  (let [root-real (try
                    (real-path genome-root)
                    (catch Exception _
                      (throw (err/error :skill/vendor-path-escape
                                        "cannot canonically resolve genome root"
                                        {:genome/root (str genome-root)}))))
        anchor (deepest-existing target)
        anchor-real (try
                      (real-path anchor)
                      (catch Exception _
                        (throw (err/error :skill/vendor-path-escape
                                          "cannot canonically resolve vendored path"
                                          {:path (str target)}))))]
    (when-not (.startsWith anchor-real root-real)
      (throw (err/error :skill/vendor-path-escape
                        "vendored path escapes genome root via symlink"
                        {:path (str target)
                         :genome-root (str root-real)
                         :resolved (str anchor-real)})))
    nil))

(defn- delete-recursively!
  "Delete a directory tree WITHOUT following symlinks.

  A symbolic link anywhere in the tree is removed AS A LINK (Files/deleteIfExists
  on the link, never recursing into its target), so deleting a stale skill dir
  can never delete files that live behind a link into an outside location.
  Directories are recursed; plain files are deleted. NOFOLLOW existence is used
  so a dangling link is still recognized and removed rather than left behind."
  [^Path p]
  (when (Files/exists p nofollow-links)
    (if (Files/isSymbolicLink p)
      (Files/deleteIfExists p)
      (do
        (when (Files/isDirectory p nofollow-links)
          (let [^java.nio.file.DirectoryStream s (Files/newDirectoryStream p)]
            (try
              (doseq [c s]
                (delete-recursively! c))
              (finally (.close s)))))
        (Files/deleteIfExists p))))
  nil)

;; --- in-process staging register (concurrency safety) -----------------------

(def ^:private active-staging
  "Absolute-path strings of staging directories currently owned by a running
  install in THIS process. The orphan sweep skips these, so one concurrent
  install never deletes another's active staging dir."
  (atom #{}))

(defn- staging-key ^String [^Path p] (str (.toAbsolutePath p)))

(defn- register-active! [^Path p] (swap! active-staging conj (staging-key p)))
(defn- unregister-active! [^Path p] (swap! active-staging disj (staging-key p)))
(defn- active? [^Path p] (contains? @active-staging (staging-key p)))

;; --- per-skill-name serialization (WO-S6) ------------------------------------

(def ^:private skill-locks (atom {}))

(defmacro ^:private with-skill-lock
  "Serialize the whole vendor operation for a single skill-name, so two
  concurrent installs of the SAME skill never race the shared dest. Different
  skill-names use different locks and may proceed concurrently."
  [skill-name & body]
  `(let [lock# (get (swap! skill-locks
                           (fn [m#] (if (contains? m# ~skill-name)
                                      m#
                                      (assoc m# ~skill-name (ReentrantLock.)))))
                    ~skill-name)]
     (.lock lock#)
     (try ~@body
          (finally (.unlock lock#)))))

;; --- orphan cleanup ----------------------------------------------------------

(defn cleanup-orphan-staging!
  "Remove left-over vendor staging directories under <genome-root>/skills/.

  Fail-closed: a containment violation (symlinked skills dir escaping the
  genome root) or a deletion failure raises a typed error rather than silently
  proceeding. Staging dirs owned by a concurrent in-process install are
  skipped. vendor-skill! invokes this at the start of every install; callers
  may also invoke it directly at startup."
  [genome-root]
  (let [^Path root (coerce-path genome-root)
        skills-dir (.resolve root "skills")]
    (ensure-contained-in-genome-root! root skills-dir)
    (when (Files/exists skills-dir nofollow-links)
      (let [^java.nio.file.DirectoryStream s (Files/newDirectoryStream skills-dir)]
        (try
          (doseq [child s]
            (let [nm (str (.getFileName child))]
              (when (and (str/starts-with? nm vendor-staging-prefix)
                         (not (active? child)))
                (try
                  (delete-recursively! child)
                  (catch clojure.lang.ExceptionInfo e (throw e))
                  (catch Throwable t
                    (throw (err/error :skill/vendor-orphan-cleanup-failed
                                      "failed to clean orphan vendor staging dir"
                                      {:path (str child) :cause (err/sanitize t)})))))))
          (finally (.close s)))))
    nil))

;; --- verify staged content ---------------------------------------------------

(defn- collect-files
  "Relative (forward-slash) paths of all regular files under `root`,
  enumerated NOFOLLOW. A symbolic link anywhere in the staged tree is a
  fail-closed rejection (:skill/vendor-verify-failed): a staging tree we
  wrote must contain only regular files."
  [^Path root]
  (letfn [(walk [^Path dir acc]
            (let [^java.nio.file.DirectoryStream s (Files/newDirectoryStream dir)]
              (try
                (reduce (fn [acc c]
                          (cond
                            (Files/isSymbolicLink c)
                            (throw (err/error :skill/vendor-verify-failed
                                              "symlink in staged vendor tree"
                                              {:path (str c)}))
                            (Files/isDirectory c nofollow-links) (walk c acc)
                            :else (conj acc c)))
                        acc s)
                (finally (.close s)))))]
    (->> (walk root [])
         (mapv (fn [^Path f]
                 (str/replace (str (.relativize root f)) "\\" "/"))))))

(defn- verify-staged!
  "Recheck the staged tree (content / limits / hash) BEFORE it is atomically
  renamed into place. Fail-closed typed :skill/vendor-verify-failed.

  Checks that:
    - the set of staged file paths EXACTLY equals the manifest entry set
      (no missing, no extra — a complete copy, i.e. the snapshot's limits on
      file count are satisfied);
    - each staged file's byte length matches the manifest :size;
    - each staged file's SHA-256 content hash matches the manifest entry's
      :artifact/id (so a corrupt/wrong CAS body is rejected before it can be
      renamed into the Genome directory).

  This is a behavior check, not a shape assertion: it refuses to rename a
  partial/corrupt staged tree into the Genome."
  [^Path staging-dir {:keys [entries]}]
  (let [staged-set (set (collect-files staging-dir))
        entry-set (set (keys entries))]
    (when-not (= staged-set entry-set)
      (throw (err/error :skill/vendor-verify-failed
                        "staged tree does not match manifest entry set"
                        {:missing (sort (seq (set/difference entry-set staged-set)))
                         :extra (sort (seq (set/difference staged-set entry-set)))})))
    (doseq [[rel {:keys [artifact/id size]}] entries]
      (let [^Path f (.resolve staging-dir (Paths/get rel (make-array String 0)))
            ba (Files/readAllBytes f)]
        (when (and size (not= size (alength ba)))
          (throw (err/error :skill/vendor-verify-failed
                            "staged file size mismatch"
                            {:path rel :expected size :actual (alength ba)})))
        (when (and id (not= id (hash/file-digest ba)))
          (throw (err/error :skill/vendor-verify-failed
                            "staged file content hash mismatch"
                            {:path rel :artifact/id id :actual (hash/file-digest ba)})))))
    nil))

;; --- atomic rename -----------------------------------------------------------

(defn- atomic-move!
  "Atomically move (rename) directory `src` onto `dst`. Prefers ATOMIC_MOVE
  (same-volume rename); falls back to a single move with REPLACE_EXISTING when
  the filesystem does not support atomic move. Either way the directory is
  moved as one unit — never copied file-by-file into a partial target."
  [^Path src ^Path dst]
  (try
    (Files/move src dst (into-array StandardCopyOption
                                    [StandardCopyOption/ATOMIC_MOVE
                                     StandardCopyOption/REPLACE_EXISTING]))
    (catch java.nio.file.AtomicMoveNotSupportedException _
      (Files/move src dst (into-array StandardCopyOption
                                      [StandardCopyOption/REPLACE_EXISTING])))))

(defn vendor-skill!
  "Vendor an external Skill snapshot revision into the Genome (atomic).

  Copies the immutable CAS tree identified by :tree/id (produced by
  fs/snapshot and CAS), NOT any live host path, into
  <genome-root>/skills/<skill-name>/.

  INSTALL IS ATOMIC (WO-S6): the tree is first written into a unique staging
  directory under <genome-root>/skills/, re-verified (entry set / per-file
  size / per-file content hash), and only then ATOMICALLY renamed onto the
  final dest-skill-dir. The dest is therefore always either a FULLY PRESENT
  complete tree or ABSENT — never a torn/mid-copy partial. A failure before
  the rename leaves the prior dest untouched (or absent on a first install)
  and any staged directory is cleaned up. Leftover staging dirs from an
  interrupted install are removed at the start of the next install (and can
  be swept directly via cleanup-orphan-staging!), fail-closed.

  CONCURRENCY (WO-S6): installs into the SAME skill-name serialize on a
  per-skill-name lock, so the dest always ends as one complete winner;
  different skills may proceed concurrently (unique staging names, in-process
  staging register).

  opts map:
    :genome/root  Path or string of genome bundle root (required)
    :cas          CAS handle (string path or {:root ...} map) (required)
    :skill/name   skill directory name under skills/ (required, single component)
    :tree/id      CAS tree id sha256:... of the skill snapshot (required)

  Also accepts :genome-root as alias for :genome/root for convenience.
  Accepts :skill-name as alias for :skill/name and :tree-id as alias.

  Returns {:skill/name ... :tree/id ... :genome/path ... :files [...] }.

  Throws :skill/vendor-* on invalid args, missing tree/artifact, verify
  failure, IO failure, or orphan-cleanup failure.
  The vendored files immediately participate in Genome identity (tree-digest)
  and are mutable via the existing :skills class — no manifest change needed.
  Upstream package changes do not affect the vendored copy because the copy
  is from snapshot revision.

  PATH SAFETY (fail-closed, typed — see the namespace docstring for the full
  statement): every entry path is rejected at the segment level before any
  resolution (:skill/vendor-path-escape on a '..' segment, :skill/vendor-invalid-args
  on an absolute/backslash/blank path); the destination is realpath-contained
  inside <genome-root> (a symlinked skills dir or intermediate that escapes the
  genome root throws :skill/vendor-path-escape BEFORE the destructive
  delete/write); and a stale skill dir (or orphan staging dir) is deleted with
  NOFOLLOW_LINKS so a symlink inside is removed as a link and never followed to
  an outside target."
  [{:keys [genome/root genome-root cas skill/name skill-name tree/id tree-id] :as opts}]
  (let [root-raw (or root genome-root (:genome/root opts) (:genome-root opts))
        cas-raw cas
        name-raw (or name skill-name (:skill/name opts) (:skill-name opts))
        tree-raw (or id tree-id (:tree/id opts) (:tree-id opts))]
    (when-not root-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :genome/root" {:opts (err/sanitize opts)})))
    (when-not cas-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :cas" {:opts (err/sanitize opts)})))
    (when-not name-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :skill/name" {:opts (err/sanitize opts)})))
    (when-not tree-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :tree/id" {:opts (err/sanitize opts)})))
    (let [^Path genome-root-path (coerce-path root-raw)
          skill-name (validate-skill-name! (str name-raw))
          tree-id (validate-tree-id! (str tree-raw))]
      (when-not (Files/isDirectory genome-root-path (make-array LinkOption 0))
        (throw (err/error :skill/vendor-invalid-args "genome root must be existing directory" {:genome/root (str genome-root-path)})))
      ;; Load snapshot manifest via CAS — this is the snapshot revision, not live path.
      (let [manifest (try
                       (snapshot/load-tree cas-raw tree-id)
                       (catch clojure.lang.ExceptionInfo e
                         (throw (err/error :skill/vendor-missing-tree "CAS tree not found for tree/id" {:tree/id tree-id :cause (ex-data e)})))
                       (catch Exception e
                         (throw (err/error :skill/vendor-missing-tree "CAS tree not found" {:tree/id tree-id :message (.getMessage e)}))))
            entries (:entries manifest)]
        (when-not (map? entries)
          (throw (err/error :skill/vendor-missing-tree "snapshot manifest missing :entries" {:tree/id tree-id :manifest manifest})))
        (with-skill-lock skill-name
          (let [skills-dir (.resolve genome-root-path (Paths/get "skills" (make-array String 0)))
                dest-skill-dir (.resolve skills-dir (Paths/get skill-name (make-array String 0)))]
            ;; realpath containment (fail-closed), BEFORE any destructive delete:
            ;; a symlinked skills dir that escapes the genome root is rejected
            ;; here, so the delete below never reaches the escaped target.
            (ensure-contained-in-genome-root! genome-root-path dest-skill-dir)
            ;; orphan cleanup at install START (fail-closed) — removes any
            ;; staging dir left by a crashed install; skips active ones.
            (cleanup-orphan-staging! genome-root-path)
            ;; unique staging dir inside the vendor root (same volume => atomic rename)
            (let [staging-dir (.resolve skills-dir (str vendor-staging-prefix (UUID/randomUUID)))]
              (register-active! staging-dir)
              (try
                (Files/createDirectories staging-dir (make-array FileAttribute 0))
                (ensure-contained-in-genome-root! genome-root-path staging-dir)
                (let [written (atom [])]
                  (doseq [[rel {:keys [artifact/id]}] entries]
                    (validate-relative-path! rel)
                    (let [^Path dest (.normalize (.resolve staging-dir
                                                           (Paths/get rel (make-array String 0))))]
                      ;; defense-in-depth: lexical containment of the resolved dest
                      (when-not (.startsWith dest staging-dir)
                        (throw (err/error :skill/vendor-path-escape "vendored path escapes skill dir"
                                          {:path rel :dest (str dest)})))
                      (let [parent (.getParent dest)]
                        (when parent (Files/createDirectories parent (make-array FileAttribute 0))))
                      ;; realpath containment of the actual file location, AFTER the
                      ;; parent dirs exist so symlinks in the chain resolve.
                      (ensure-contained-in-genome-root! genome-root-path dest)
                      (let [ba (try
                                 (cas/get-bytes cas-raw id)
                                 (catch Exception e
                                   (throw (err/error :skill/vendor-missing-artifact "CAS artifact missing for entry"
                                                     {:path rel :artifact/id id :message (.getMessage e)}))))]
                        (Files/write dest ^bytes ba (make-array java.nio.file.OpenOption 0))
                        (swap! written conj rel))))
                  ;; VERIFY staged content (content / limits / hash) before rename —
                  ;; a corrupt/partial staged tree is rejected typed, never renamed in.
                  (verify-staged! staging-dir manifest)
                  ;; atomic swap: remove stale dest (NOFOLLOW) then rename staging -> dest
                  (when (Files/exists dest-skill-dir nofollow-links)
                    (delete-recursively! dest-skill-dir))
                  (atomic-move! staging-dir dest-skill-dir)
                  {:skill/name skill-name
                   :tree/id tree-id
                   :genome/path (str dest-skill-dir)
                   :genome/root (str genome-root-path)
                   :files (sort @written)
                   :manifest manifest})
                (catch Throwable t
                  ;; On ANY failure, clean the current staging dir so no orphan
                  ;; is left; the prior dest is untouched (or absent). Then rethrow.
                  (unregister-active! staging-dir)
                  (when (Files/exists staging-dir nofollow-links)
                    (delete-recursively! staging-dir))
                  (throw t))
                (finally
                  (unregister-active! staging-dir))))))))))
