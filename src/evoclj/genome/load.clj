(ns evoclj.genome.load
  "Load an immutable Genome bundle from disk (component).

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
    retained by this function, and no lazy sequence escapes.
  - Seed trust anchors (component, T2c): with anchors in force (an
    optional second argument — a map of seed genome id → expected
    \"sha256:\" digest, defaulting to nothing), the loaded bundle MUST
    be a pinned seed; a tampered seed (any byte changed), an unanchored
    bundle, or an anchor whose key and digest are out of sync refuses
    with :genome/trust-anchor-mismatch (load refused). Without anchors
    the load is unanchored and backward compatible. The shipped
    anchors live in resources/trust-anchors.edn (see `trust-anchors`)
    and are overridable via config."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [evoclj.fs.walk :as fs-walk]
            [evoclj.kernel.error :as err]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.path :as path]
            [evoclj.genome.schema :as schema]
            [evoclj.genome.types :as types])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitResult Files LinkOption Path Paths SimpleFileVisitor)))

(def ^:private manifest-file-name "manifest.edn")

;; --- seed trust anchors (component) ------------------------------------------

(def ^:private trust-anchor-resource
  "Classpath resource name of the shipped trust-anchor file."
  "trust-anchors.edn")

(defn- validate-anchors!
  "Validate a trust-anchors value at the trust boundary (Global
  Constraint 22). nil or an empty map mean 'no anchors in force' and
  pass through as nil. Anything else must be a map whose keys (seed
  genome ids) and values (expected \"sha256:\" digests) are all
  canonical genome ids; any violation throws
  :genome/trust-anchor-invalid. Fail-closed: a malformed anchor value
  never silently disables verification."
  [anchors]
  (cond
    (nil? anchors) nil
    (and (map? anchors) (empty? anchors)) nil
    (not (map? anchors))
    (throw (err/error :genome/trust-anchor-invalid
                      "trust anchors must be a map of seed genome id to expected sha256: digest"
                      {:anchors (err/sanitize anchors)}))
    :else
    (let [bad (into []
                    (comp (remove (fn [[k v]] (and (types/genome-id? k)
                                                   (types/genome-id? v))))
                          (map (fn [[k v]] {:seed/genome-id (err/sanitize k)
                                             :expected-digest (err/sanitize v)})))
                    anchors)]
      (when (seq bad)
        (throw (err/error :genome/trust-anchor-invalid
                          "trust anchors must map canonical genome ids to expected sha256: digests"
                          {:invalid-entries bad})))
      anchors)))

(defn- verify-trust-anchors!
  "Verify the loaded bundle's computed genome id against the trust
  anchors in force (component, T2c).

  With NO anchors in force (nil or an empty map) the load is
  unanchored and passes through unchanged (backward compatible). With
  anchors in force the bundle MUST be a pinned seed: its computed id
  must be a key of the anchors map and the pinned expected digest must
  equal it. Anything else — a tampered seed (any byte changed, so its
  id no longer matches any pinned seed), a bundle that was never a
  seed, or an anchor entry whose key and digest are out of sync — is a
  trust violation: typed :genome/trust-anchor-mismatch, load refused.
  Fail-closed: under anchors, a bundle that cannot prove it is a pinned
  seed is never loaded."
  [anchors id]
  (when (seq anchors)
    (if-let [expected (get anchors id)]
      (when (not= expected id)
        (throw (err/error :genome/trust-anchor-mismatch
                          "genome digest does not match its trust anchor"
                          {:genome/id id
                           :expected-digest expected
                           :reason :digest-mismatch})))
      (throw (err/error :genome/trust-anchor-mismatch
                        "genome is not a pinned seed; load refused by trust anchors"
                        {:genome/id id
                         :reason :not-anchored
                         :anchored-ids (vec (keys anchors))}))))
  id)

(defn trust-anchors
  "The seed trust anchors shipped in resources/trust-anchors.edn: a map
  of seed genome id → expected \"sha256:\" digest for every pinned
  seed (component). The file is parsed with clojure.edn/read-string and
  validated; an absent or unparseable file throws
  :genome/trust-anchor-invalid (fail-closed — a missing anchor file
  never silently means 'no anchors').

  Overridable via config: callers that source trust anchors from a
  configuration map pass that map directly to load-genome instead of
  this default."
  []
  (let [url (io/resource trust-anchor-resource)]
    (when-not url
      (throw (err/error :genome/trust-anchor-invalid
                        "trust-anchors.edn is missing from the classpath"
                        {:resource trust-anchor-resource})))
    (let [v (try
              (edn/read-string (slurp url))
              (catch Exception e
                (throw (err/error :genome/trust-anchor-invalid
                                  "trust-anchors.edn failed to parse"
                                  {:resource trust-anchor-resource
                                   :message (.getMessage e)}))))]
      (validate-anchors! v))))

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

;; --- directory walk (delegated to generic safe walker) ---------------------
;; Security rules (canonical path, no symlink/junction, no duplicate,
;; unreadable -> fail closed) live in evoclj.fs.walk so Genome and Skill
;; snapshots share one implementation.

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

  Optional second argument `trust-anchors` (component): the trust-anchor
  map of seed genome id → expected \"sha256:\" digest to enforce. With
  NO anchors (absent, nil, or empty) the load is unanchored and behaves
  byte-identically to the pre-C3 loader (backward compatible). With
  anchors in force the bundle MUST be a pinned seed — its computed id
  must be a key of the map and equal the pinned digest — otherwise the
  load refuses with :genome/trust-anchor-mismatch. See `trust-anchors`
  for the shipped anchors (overridable via config by passing a
  different map).

  Returns a map with :genome/id (the canonical \"sha256:<64 hex>\"
  content address over every bundle file), :genome/root (the anchor
  Path, never hashed), :manifest (the parsed, schema-validated manifest,
  returned unchanged), and :files (map of canonical relative path to
  {:digest :bytes :kind}).

  Throws ExceptionInfo with a stable :error/type keyword:
  :genome/root-invalid, :genome/manifest-missing,
  :genome/schema-invalid, :genome/edn-invalid, :genome/symlink-rejected,
  :genome/unreadable, :genome/module-missing, :genome/duplicate-path,
  :genome/path-invalid, :genome/trust-anchor-invalid, or
  :genome/trust-anchor-mismatch."
  ([root-path]
   (load-genome root-path nil))
  ([root-path trust-anchors]
   (let [root (validate-root! root-path)
         anchors (validate-anchors! trust-anchors)
         manifest-file (.resolve root manifest-file-name)
         _ (check-manifest-entry! manifest-file)
         manifest-bytes (read-regular-file! manifest-file manifest-file-name)
         manifest (schema/validate-manifest (parse-edn manifest-file-name manifest-bytes))
         walked-raw (fs-walk/walk-tree-genome-compat root)
         walked (mapv (fn [{:keys [path physical-path]}] {:file physical-path :path path}) walked-raw)
         walked-paths (into #{} (map :path) walked)
         _ (check-declared-modules! root manifest walked-paths)
         files (build-files walked (declared-parse-paths manifest))
         id (hash/tree-digest (mapv (fn [[p {:keys [digest]}]]
                                      {:path p :digest digest})
                                    files))]
     (verify-trust-anchors! anchors id)
     {:genome/id id
      :genome/root root
      :manifest manifest
      :files files})))
