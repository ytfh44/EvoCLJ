(ns evoclj.fs.snapshot
  "Generic immutable tree snapshot for any directory.

  Unlike Genome hashing (CRLF-normalized text hashing), Skill snapshots
  preserve exact raw bytes so upstream package identity and our revision
  identity stay aligned. Every file is stored verbatim in CAS.

  Snapshot produces a canonical tree manifest:

    {:tree/version 1
     :entries {\"SKILL.md\" {:artifact/id \"sha256:...\" :size 123}
               \"references/x.md\" {:artifact/id \"sha256:...\" :size 456}}}

  The manifest itself is stored in CAS and its artifact id is the tree id.

  PREFLIGHT / STREAMING LIMITS (INV-03, WO-S4 — reject before read):
  snapshot-tree! runs a read-only PREFLIGHT before it reads or streams any
  file content:

    - path validation (symlink / junction escape / duplicate canonical
      path / traversal) is performed by evoclj.fs.walk/walk-tree, which
      touches only directory attributes, never file bytes;
    - per-file attribute metadata (:size, readability, regular-file) is
      gathered with Files/size + Files/isReadable + Files/isRegularFile,
      which inspect attributes WITHOUT loading bytes into memory;
    - the configured limits (:max-files :max-depth :max-file-bytes
      :max-total-bytes) are enforced against that metadata FIRST.

  Only after the preflight passes does the capture phase read each file
  and write it to CAS. Consequently an over-limit (or unreadable) tree is
  rejected fail-closed with a typed error BEFORE any CAS artifact is
  created — zero new artifacts, never a partial snapshot that is rejected
  mid-stream."
  (:require [clojure.edn :as edn]
            [evoclj.fs.walk :as walk]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path)))

(defn- check-limits!
  "Enforce the configured limits against a sequence of preflight entries
  that already carry :path and :size (gathered from filesystem attribute
  metadata — never from content). Throws :fs/snapshot-limit-exceeded on
  the first violated limit with its :limit and :actual. Pure; no I/O."
  [entries limits]
  (let [{:keys [max-depth max-files max-total-bytes max-file-bytes]} limits]
    (when (and max-files (> (count entries) max-files))
      (throw (err/error :fs/snapshot-limit-exceeded "max-files exceeded"
                        {:limit max-files :actual (count entries)})))
    (when max-depth
      (doseq [{:keys [path]} entries]
        (let [depth (count (clojure.string/split path #"/"))]
          (when (> depth max-depth)
            (throw (err/error :fs/snapshot-limit-exceeded "max-depth exceeded"
                              {:path path :depth depth :limit max-depth}))))))
    (when max-file-bytes
      (doseq [{:keys [size path]} entries]
        (when (> size max-file-bytes)
          (throw (err/error :fs/snapshot-limit-exceeded "max-file-bytes exceeded"
                            {:path path :size size :limit max-file-bytes})))))
    (when max-total-bytes
      (let [total (reduce + 0 (map :size entries))]
        (when (> total max-total-bytes)
          (throw (err/error :fs/snapshot-limit-exceeded "max-total-bytes exceeded"
                            {:total total :limit max-total-bytes})))))))

(defn- preflight-entries!
  "Gather path + attribute metadata (size, readability, regular-file) for
  every walked file WITHOUT reading any content (INV-03). Fail-closed:
  an entry that is not a regular file or is not readable throws
  :fs/unreadable before the capture phase touches a single byte."
  [entries-raw]
  (mapv (fn [{:keys [path physical-path]}]
          (let [^Path p physical-path
                _ (when-not (Files/isRegularFile p (make-array LinkOption 0))
                    (throw (err/error :fs/unreadable
                                      "entry is not a regular file"
                                      {:path path})))
                _ (when-not (Files/isReadable p)
                    (throw (err/error :fs/unreadable
                                      "entry is not readable"
                                      {:path path})))]
            {:path path :physical-path p :size (Files/size p)}))
        entries-raw))

(defn snapshot-tree!
  "Snapshot root directory to CAS under limits.

  root — Path or string
  cas  — CAS config (see evoclj.store.cas/->cas)
  limits — map with optional keys: :max-depth :max-files :max-total-bytes :max-file-bytes

  Runs a read-only PREFLIGHT (path/limit/readability validation) before
  reading or streaming any file content; an over-limit or unreadable tree
  is rejected fail-closed (typed :fs/snapshot-limit-exceeded /
  :fs/unreadable) BEFORE any CAS artifact is created (INV-03 — limits
  enforced before reads).

  Returns {:tree/id \"sha256:...\" :manifest {...} :entries {...}}.

  Each file is stored with exact raw bytes (no CRLF normalization).
  Manifest is stored as EDN bytes in CAS."
  [root cas limits]
  (let [;; 1) PREFLIGHT — walk (path validation) + attribute metadata, no content read
        entries-raw (walk/walk-tree root)
        preflight (preflight-entries! entries-raw)
        _ (check-limits! preflight limits)
        ;; 2) CAPTURE — only after preflight passes do we read + write to CAS
        entries (mapv (fn [{:keys [path physical-path]}]
                        (let [^Path p physical-path
                              ba (Files/readAllBytes p)
                              {:keys [artifact/id size]} (cas/put-bytes! cas ba {:media-type "application/octet-stream"})]
                          {:path path :artifact/id id :size size :physical-path p}))
                      preflight)
        ;; canonical ordering for deterministic manifest
        sorted (sort-by :path (fn [a b] (compare a b)) entries)
        manifest {:tree/version 1
                  :entries (into {} (map (fn [{:keys [path artifact/id size]}] [path {:artifact/id id :size size}]) sorted))}
        manifest-bytes (.getBytes (pr-str manifest) StandardCharsets/UTF_8)
        {:keys [artifact/id]} (cas/put-bytes! cas manifest-bytes {:media-type "application/edn"})]
    {:tree/id id
     :manifest manifest
     :entries sorted
     :size (count sorted)}))

(defn load-tree
  "Load a tree manifest from CAS by tree id."
  [cas tree-id]
  (let [ba (cas/get-bytes cas tree-id)
        s (String. ba StandardCharsets/UTF_8)]
    (edn/read-string s)))

(defn get-file-bytes
  "Get raw bytes of a file inside a tree via CAS."
  [cas tree-manifest path]
  (when-let [{:keys [artifact/id]} (get-in tree-manifest [:entries path])]
    (cas/get-bytes cas id)))
