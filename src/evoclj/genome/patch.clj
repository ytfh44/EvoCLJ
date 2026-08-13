(ns evoclj.genome.patch
  "Deterministic mutation application (Task 7.4).

  `apply-mutation` turns a validated Mutation IR plus a loaded immutable
  parent Genome into a newly loaded immutable candidate Genome with a
  new :genome/id:

    (apply-mutation parent-loaded-genome mutation output-dir)
    ;; => loaded candidate Genome map, :genome/root now the candidate dir

  Pipeline (Step 7: staging write -> validate/load/hash -> atomic
  candidate finalize):

  1. VALIDATE — evoclj.evolution.mutation/validate-mutation with the
     parent as context: op schemas, the :expect/hash requirement on
     every destructive op, canonical-relative-path gate (no traversal,
     drive letters, or symlink components), protected-path gate
     (kernel/eval/capability/evolution roots), and the declared
     mutable-class gate from the parent manifest. Anything rejected
     throws before a single byte is staged.
  2. STAGE — a fresh staging directory under output-dir receives a
     SAFE COPY of the bundle: bytes are taken from the already-loaded
     in-memory :files (never re-walked, never following symlinks), and
     every staged path is re-checked against the staging root
     (defense-in-depth: no symlink components, no escape).
  3. APPLY — ops run in order; each op's :expect/hash (when present)
     is verified against the CURRENT staged content, so a stale patch
     fails with :patch/preimage-mismatch BEFORE its op runs. The
     declarative op language is finite and closed: :set-edn
     :delete-edn (evoclj.genome.patch-edn), :insert-text :replace-text
     :delete-text (evoclj.genome.patch-text), :replace-form
     :insert-form :delete-form (evoclj.genome.patch-clj), and the
     topology graph ops (patch-edn, gated by the topology compiler).
  4. LOAD/HASH — the staged bundle is loaded with
     evoclj.genome.load/load-genome, which re-validates the manifest,
     parses every declared EDN module, rejects symlinks, and computes
     the canonical content address. Any failure (e.g. an op that
     corrupted a declared module) fails closed here.
  5. FINALIZE — the staging directory is moved to
     output-dir/<genome-id with ':' -> '-'> (an atomic rename on the
     same volume; idempotent when the candidate already exists). The
     returned Genome's :genome/root points at the final candidate dir.

  Determinism (Global Constraints 1 and 6): content and :genome/id
  depend only on the parent bytes plus the mutation value, so applying
  the same mutation twice into separate output dirs yields identical
  candidate IDs and identical file bytes.

  Safety: apply-mutation never writes a file whose path was not
  validated (mutation gate + staging-root re-check), never follows or
  creates a symbolic link, and on ANY failure deletes the staging
  directory — a stale patch leaves no candidate directory behind.

  Error contract (propagated stable :error/type keywords):
  :mutation/* (validation), :patch/preimage-mismatch,
  :patch/anchor-not-found, :patch/anchor-ambiguous,
  :patch/form-not-found, :patch/edn-invalid, :patch/edn-path-invalid,
  :patch/clj-invalid, :patch/edge-not-found, :patch/op-invalid,
  :patch/path-escape, :patch/staging-failed, :patch/finalize-failed,
  :topology/* (graph-op result gate), and :genome/* (final load)."
  (:require [clojure.string :as str]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as path]
            [evoclj.genome.patch-clj :as patch-clj]
            [evoclj.genome.patch-edn :as patch-edn]
            [evoclj.genome.patch-text :as patch-text]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file AtomicMoveNotSupportedException Files LinkOption
                         OpenOption Path Paths StandardCopyOption)
           (java.nio.file.attribute FileAttribute)))

;; --- op dispatch -----------------------------------------------------------

(defn- apply-one-op
  "Apply one validated op to `content` (the current text of the op's
  target file), dispatching to the owning patch namespace."
  [content op]
  (case (:op op)
    (:set-edn :delete-edn :add-node :remove-node :add-edge :remove-edge
     :update-node)
    (patch-edn/apply-op content op)

    (:insert-text :replace-text :delete-text)
    (patch-text/apply-op content op)

    (:replace-form :insert-form :delete-form)
    (patch-clj/apply-op content op)

    (throw (err/error :patch/op-invalid
                      "unknown mutation op"
                      {:op (:op op)}))))

;; --- filesystem helpers ----------------------------------------------------

(defn- coerce-path
  "Coerce a java.nio.file.Path or a string to a Path."
  [x]
  (cond
    (instance? Path x) x
    (string? x) (Paths/get x (make-array String 0))
    :else (throw (err/error :patch/op-invalid
                            "output-dir must be a java.nio.file.Path or a string"
                            {:output-dir x}))))

(defn- ensure-dir! ^Path [^Path dir]
  (Files/createDirectories dir (make-array FileAttribute 0))
  dir)

(defn- staging-dir! ^Path [^Path output-dir]
  (Files/createTempDirectory output-dir ".evoclj-staging-" (make-array FileAttribute 0)))

(defn- delete-recursively! [^Path dir]
  (when (Files/exists dir (make-array LinkOption 0))
    (let [f (.toFile dir)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)]
          (delete-recursively! (.toPath c))))
      (Files/delete dir))))

;; --- staging ---------------------------------------------------------------

(defn- textual-files
  "The parent's textual files (:edn :text :clj) decoded to strings, keyed
  by canonical relative path. Binary files are staged unchanged and can
  never be the target of an op."
  [parent]
  (into {}
        (keep (fn [[rel {:keys [kind bytes]}]]
                (when (contains? #{:edn :text :clj} kind)
                  [rel (String. (byte-array bytes) StandardCharsets/UTF_8)])))
        (:files parent)))

(defn- check-writable!
  "Defense-in-depth: require `rel` to be a canonical relative Genome
  path that resolves inside `staging` with no symlink component on the
  way (evoclj.genome.path/allowed-genome-path?). The mutation gate
  already ran; this second check re-anchors the write at the staging
  root so no op can ever reach outside the candidate."
  [^Path staging rel]
  (when-not (path/allowed-genome-path? staging rel)
    (throw (err/error :patch/path-escape
                      "staged write would escape the candidate staging root"
                      {:path rel}))))

(defn- write-staged-files!
  "Write every parent file into `staging`: textual files from `contents`
  (post-op), binary files verbatim from the parent's :bytes. Parent
  directories are created fresh, so nothing on disk can be followed."
  [^Path staging parent contents]
  (doseq [[rel {:keys [bytes]}] (:files parent)]
    (let [canonical (path/normalize-relative-path rel)
          _ (check-writable! staging canonical)
          target (.resolve staging (Paths/get canonical (make-array String 0)))]
      (Files/createDirectories (.getParent target) (make-array FileAttribute 0))
      (if (contains? contents canonical)
        (Files/write target (.getBytes ^String (get contents canonical)
                                       StandardCharsets/UTF_8)
                     (make-array OpenOption 0))
        (Files/write target (byte-array bytes) (make-array OpenOption 0))))))

;; --- op application --------------------------------------------------------

(defn- apply-ops!
  "Reduce the ops in order over the staged textual contents. Before each
  op, verify its :expect/hash (when present) against the CURRENT
  content of the op's target file — a stale preimage fails with
  :patch/preimage-mismatch and nothing further is staged."
  [contents mutation]
  (reduce
   (fn [acc op]
     (let [rel (:file op)
           content (get acc rel)]
       (when (nil? content)
         (throw (err/error :patch/op-invalid
                           "op targets a file that is not a mutable textual asset"
                           {:path rel :op (:op op)})))
       (when-let [expected (:expect/hash op)]
         (let [actual (hash/text-digest content)]
           (when-not (= expected actual)
             (throw (err/error :patch/preimage-mismatch
                               "stale preimage: target file content does not match :expect/hash"
                               {:path rel :op (:op op)
                                :expected expected :actual actual})))))
       (assoc acc rel (apply-one-op content op))))
   contents
   (:ops mutation)))

;; --- finalize --------------------------------------------------------------

(defn- candidate-dir-name
  "Directory name for a candidate: the genome id with ':' replaced so it
  is a legal directory name on every host (Windows forbids ':' in
  names). Deterministic: identical ids -> identical names."
  [genome-id]
  (str/replace genome-id ":" "-"))

(defn- finalize!
  "Atomically move the validated staging directory to
  output-dir/<candidate-dir-name>. If the candidate already exists
  (deterministic re-application of the same mutation), the staging copy
  is discarded and the existing candidate is returned. Any failure
  deletes staging and throws :patch/finalize-failed."
  [^Path output-dir ^Path staging genome-id]
  (let [target (.resolve output-dir (candidate-dir-name genome-id))]
    (cond
      (Files/exists target (make-array LinkOption 0))
      (do (delete-recursively! staging)
          target)

      :else
      (try
        (Files/move staging target (make-array StandardCopyOption 0))
        target
        (catch java.nio.file.FileAlreadyExistsException _
          (delete-recursively! staging)
          target)
        (catch Exception e
          (delete-recursively! staging)
          (throw (err/error :patch/finalize-failed
                            "could not finalize the candidate directory"
                            {:genome/id genome-id
                             :message (.getMessage e)})))))))

;; --- public entry point ----------------------------------------------------

(defn apply-mutation
  "Apply `mutation` to the loaded immutable `parent` Genome, staging a
  candidate bundle under `output-dir` and returning the newly loaded
  immutable candidate Genome (a map with a new :genome/id and
  :genome/root pointing at the finalized candidate directory).

  `parent` must be a loaded Genome map (evoclj.genome.load/load-genome),
  `mutation` a validated Mutation IR, and `output-dir` a
  java.nio.file.Path or string. On any failure the staging directory is
  removed, so no partial candidate is ever left behind."
  [parent mutation output-dir]
  (mutation/validate-mutation mutation parent)
  (let [output-dir (ensure-dir! (coerce-path output-dir))
        staging (staging-dir! output-dir)]
    (try
      (let [contents (textual-files parent)
            contents' (apply-ops! contents mutation)]
        (write-staged-files! staging parent contents'))
      (catch Throwable t
        (delete-recursively! staging)
        (throw t)))
    (let [candidate (load/load-genome staging)
          final-root (finalize! output-dir staging (:genome/id candidate))]
      (assoc candidate :genome/root final-root))))
