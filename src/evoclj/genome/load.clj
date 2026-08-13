(ns evoclj.genome.load
  "Load an immutable Genome bundle from disk (Task 1.4).

  `load-genome` walks the bundle directory depth-first WITHOUT following
  symbolic links, parses the manifest with clojure.edn/read-string and
  validates it BEFORE trusting any module path, reads every bundle file
  into an immutable in-memory value, and content-addresses the result.
  The returned value is a plain map:

    {:genome/id \"sha256:<64 hex>\"
     :genome/root <java.nio.file.Path>
     :manifest {...}
     :files {\"manifest.edn\" {:digest ... :bytes ... :kind :edn}
             ...}}

  Security and boundary properties (Global Constraints 1, 6, 22):

  - Only clojure.edn/read-string is ever used (never
    clojure.core/read-string), so no reader-eval and no host evaluation
    can run at load time; only the manifest and the four declared EDN
    modules are parsed at all.
  - The manifest is schema-validated before module paths are trusted,
    so traversal or absolute module paths fail with
    :genome/schema-invalid before any such path is resolved on disk.
  - Symbolic links anywhere in the bundle (file, directory, or escape)
    reject the whole bundle with :genome/symlink-rejected; the walk
    additionally verifies every directory resolves inside the real
    root, which catches Windows junctions that isSymbolicLink misses.
  - Duplicate normalized paths, a missing manifest, a missing required
    module, and unreadable entries reject with stable :error/type
    keywords (:genome/duplicate-path, :genome/manifest-missing,
    :genome/module-missing, :genome/unreadable).
  - File payloads carry immutable vectors of bytes (:bytes), never a
    mutable Java byte array; the digest is the canonical
    CRLF-normalized text digest for textual kinds (:edn :text :clj) and
    the raw byte digest for :binary. :genome/root may hold the
    java.nio.file.Path anchor per the task contract, but it never
    participates in hashing and never appears in :files.
  - Every file is read with Files/readAllBytes; no stream is opened or
    retained by this function, and no lazy sequence escapes."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.path :as path]
            [evoclj.genome.schema :as schema])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitResult Files LinkOption Path Paths SimpleFileVisitor)))

(def ^:private manifest-file-name "manifest.edn")

;; --- root validation -------------------------------------------------------

(defn- validate-root!
  "Coerce `x` (a java.nio.file.Path or string) to a Path and require it
  to be an existing, non-symlink directory."
  [x]
  (let [root (cond
               (instance? Path x) x
               (string? x) (try (Paths/get x (make-array String 0))
                                (catch Exception _
                                  (throw (err/error :genome/root-invalid
                                                    "genome root must be a valid path string"
                                                    {:path x :reason :bad-type}))))
               :else (throw (err/error :genome/root-invalid
                                       "genome root must be a java.nio.file.Path or a string"
                                       {:path x :reason :bad-type})))]
    (when (Files/isSymbolicLink root)
      (throw (err/error :genome/root-invalid
                        "genome root must not be a symbolic link"
                        {:path (str root) :reason :symlink})))
    (when-not (Files/isDirectory root (make-array LinkOption 0))
      (throw (err/error :genome/root-invalid
                        "genome root must be an existing directory"
                        {:path (str root)
                         :reason (if (Files/exists root (make-array LinkOption 0))
                                   :not-directory
                                   :not-found)})))
    root))

;; --- manifest gate ---------------------------------------------------------

(defn- check-manifest-entry!
  "Require manifest.edn to exist as a real, non-symlink regular file
  before anything else is read."
  [^Path manifest-file]
  (when-not (Files/exists manifest-file (make-array LinkOption 0))
    (throw (err/error :genome/manifest-missing
                      "genome bundle has no manifest.edn"
                      {:path manifest-file-name})))
  (when (Files/isSymbolicLink manifest-file)
    (throw (err/error :genome/symlink-rejected
                      "genome bundle must not contain symbolic links"
                      {:path manifest-file-name})))
  (when-not (Files/isRegularFile manifest-file (make-array LinkOption 0))
    (throw (err/error :genome/unreadable
                      "genome manifest is not a readable regular file"
                      {:path manifest-file-name}))))

;; --- directory walk without following links --------------------------------

(defn- outside-bundle?
  "True when `dir`, resolved to its real path (following links), is not
  inside `root-real`. Catches Windows junctions and any link-following
  edge that isSymbolicLink misses; fail-closed on error. The explicit
  empty LinkOption array is required: a zero-arg varargs call to
  toRealPath is not resolved statically on all hosts."
  [^Path root-real ^Path dir]
  (try
    (not (.startsWith (.toRealPath dir (make-array LinkOption 0)) root-real))
    (catch Exception _ true)))

(defn- make-walk-visitor
  "SimpleFileVisitor that records every regular file as [file relative
  path-string] and rejects symbolic links. Files/walkFileTree runs with
  default options, so symbolic links are never followed; the visitor
  still rejects them and verifies each directory resolves inside the
  real root. Any entry that cannot be read rejects the whole bundle."
  [^Path root acc]
  (let [root-real (.toRealPath root (make-array LinkOption 0))]
    (proxy [SimpleFileVisitor] []
      (preVisitDirectory [dir _attrs]
        (if (outside-bundle? root-real dir)
          (throw (err/error :genome/symlink-rejected
                            "genome bundle must not contain links outside the bundle"
                            {:path (str (.relativize root dir))}))
          FileVisitResult/CONTINUE))
      (visitFile [file _attrs]
        (if (Files/isSymbolicLink file)
          (throw (err/error :genome/symlink-rejected
                            "genome bundle must not contain symbolic links"
                            {:path (str (.relativize root file))}))
          (do (vswap! acc conj [file (str (.relativize root file))])
              FileVisitResult/CONTINUE)))
      (visitFileFailed [_file exc]
        (throw (err/error :genome/unreadable
                          "genome bundle contains an unreadable entry"
                          {:message (.getMessage exc)}))))))

(defn- walk-bundle!
  "Depth-first walk of `root` without following symbolic links. Returns
  a vector of [file Path, host-separator relative path string] pairs for
  every regular file in the bundle."
  [^Path root]
  (let [acc (volatile! [])]
    (Files/walkFileTree root (make-walk-visitor root acc))
    @acc))

;; --- normalization and duplicate detection ---------------------------------

(defn- normalize-files
  "Canonicalize each walked file's relative path and reject any pair of
  on-disk files that normalize to the same canonical path."
  [walked]
  (let [normalized (mapv (fn [[file rel]]
                           {:file file :path (path/normalize-relative-path rel)})
                         walked)
        dupes (->> normalized
                   (group-by :path)
                   (filter (fn [[_ entries]] (> (count entries) 1))))]
    (when (seq dupes)
      (throw (err/error :genome/duplicate-path
                        "duplicate normalized paths in genome bundle"
                        {:paths (mapv first dupes)})))
    normalized))

(defn- check-declared-modules!
  "Reject duplicate normalized declared module paths, then require every
  declared module to exist on disk as a real regular file. A declared
  path that exists but is not a file (e.g. a directory) is unreadable;
  one that does not exist is missing."
  [^Path root manifest walked-paths]
  (let [declared (mapv (fn [[module-k rel]]
                         [module-k (path/normalize-relative-path rel)])
                       (:modules manifest))
        dupes (->> declared
                   (group-by second)
                   (filter (fn [[_ entries]] (> (count entries) 1))))]
    (when (seq dupes)
      (throw (err/error :genome/duplicate-path
                        "duplicate normalized module paths in manifest"
                        {:paths (mapv first dupes)})))
    (doseq [[module-k p] declared]
      (cond
        (contains? walked-paths p) nil
        (Files/exists (.resolve root (Paths/get p (make-array String 0)))
                      (make-array LinkOption 0))
        (throw (err/error :genome/unreadable
                          "a required genome module is not a readable regular file"
                          {:module module-k :path p}))
        :else
        (throw (err/error :genome/module-missing
                          "a required genome module is missing from the bundle"
                          {:module module-k :path p}))))))

;; --- reading, parsing, kind inference --------------------------------------

(defn- read-regular-file!
  "Read one bundle file fully with Files/readAllBytes (no stream is
  retained). Any IOException becomes :genome/unreadable."
  [^Path file rel]
  (try
    (Files/readAllBytes file)
    (catch java.io.IOException e
      (throw (err/error :genome/unreadable
                        "genome file could not be read"
                        {:path rel :message (.getMessage e)})))))

(defn- parse-edn
  "Parse `rel`'s decoded text with clojure.edn/read-string. Never
  clojure.core/read-string: clojure.edn has no reader-eval and
  evaluates nothing, so a bundle cannot execute code at load time. Any
  read failure throws :genome/edn-invalid."
  [rel ^bytes ba]
  (try
    (edn/read-string (String. ba StandardCharsets/UTF_8))
    (catch Exception e
      (throw (err/error :genome/edn-invalid
                        "declared EDN file failed to parse"
                        {:path rel :message (.getMessage e)})))))

(defn- binary-bytes?
  "True when the byte array contains a NUL byte."
  [^bytes ba]
  (loop [i 0]
    (if (< i (alength ba))
      (if (zero? (aget ba i)) true (recur (inc i)))
      false)))

(defn- kind-of
  "Asset kind for a bundle file: :edn for .edn, :clj for .clj; otherwise
  :binary when the bytes contain NUL or do not decode cleanly as UTF-8,
  else :text."
  [^String rel ^bytes ba]
  (let [s (String. ba StandardCharsets/UTF_8)]
    (cond
      (str/ends-with? rel ".edn") :edn
      (str/ends-with? rel ".clj") :clj
      (binary-bytes? ba) :binary
      (str/includes? s "\uFFFD") :binary
      :else :text)))

(defn- file-value
  "Immutable {:digest :bytes :kind} payload for one bundle file. :bytes
  is an immutable vector of bytes (never a mutable Java byte array);
  the digest is the canonical CRLF-normalized text digest for textual
  kinds and the raw byte digest for :binary (normative hashing rules 1
  and 2)."
  [^String rel ^bytes ba]
  (let [kind (kind-of rel ba)
        digest (if (= kind :binary)
                 (hash/file-digest ba)
                 (hash/text-digest (String. ba StandardCharsets/UTF_8)))]
    {:digest digest :bytes (vec ba) :kind kind}))

(defn- declared-parse-paths
  "Canonical paths of the four declared EDN modules that must parse as
  EDN (the manifest itself is parsed before validation)."
  [manifest]
  (into #{} (map path/normalize-relative-path) (vals (:modules manifest))))

(defn- build-files
  "Read every walked file into its immutable payload, parsing each
  declared EDN module (a member of parse-paths) with
  clojure.edn/read-string as a validation pass."
  [walked parse-paths]
  (into {}
        (map (fn [{:keys [file path]}]
               (let [ba (read-regular-file! file path)]
                 (when (contains? parse-paths path)
                   (parse-edn path ba))
                 [path (file-value path ba)])))
        walked))

;; --- public entry point -----------------------------------------------------

(defn load-genome
  "Load the immutable Genome bundle rooted at `root-path` (a
  java.nio.file.Path or a string naming an existing directory).

  Returns a map with :genome/id (the canonical \"sha256:<64 hex>\"
  content address over every bundle file), :genome/root (the anchor
  Path, never hashed), :manifest (the parsed, schema-validated manifest,
  returned unchanged), and :files (map of canonical relative path to
  {:digest :bytes :kind}).

  Throws ExceptionInfo with a stable :error/type keyword:
  :genome/root-invalid, :genome/manifest-missing,
  :genome/schema-invalid, :genome/edn-invalid, :genome/symlink-rejected,
  :genome/unreadable, :genome/module-missing, :genome/duplicate-path, or
  :genome/path-invalid."
  [root-path]
  (let [root (validate-root! root-path)
        manifest-file (.resolve root manifest-file-name)
        _ (check-manifest-entry! manifest-file)
        manifest-bytes (read-regular-file! manifest-file manifest-file-name)
        manifest (schema/validate-manifest (parse-edn manifest-file-name manifest-bytes))
        walked (normalize-files (walk-bundle! root))
        walked-paths (into #{} (map :path) walked)
        _ (check-declared-modules! root manifest walked-paths)
        files (build-files walked (declared-parse-paths manifest))
        id (hash/tree-digest (mapv (fn [[p {:keys [digest]}]]
                                     {:path p :digest digest})
                                   files))]
    {:genome/id id
     :genome/root root
     :manifest manifest
     :files files}))
