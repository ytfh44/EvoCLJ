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
  mid-stream.

  TOCTOU HARDENING (WO-S5, e2e#10 — NOFOLLOW + identity re-check,
  platform-graded):
  The preflight validates metadata and the walker rejects symlinks, but a
  path can still be SWAPPED between that validation and the actual read
  (the readAllBytes/cas-write in the capture phase). S5 closes that
  window on the capture step (see capture-entry!) by:

    - NOFOLLOW — the file is opened/read with NOFOLLOW_LINKS, so a
      validated path switched to a symbolic link after preflight is never
      followed to an outside target; the open itself fails and the outside
      bytes are never read.
    - Identity re-check — the file identity is captured once at preflight
      (preflight-entries! -> identity-of) and re-verified immediately at
      capture (capture-entry!). A mismatch (path retargeted, file
      replaced, symlink introduced, path vanished) is a typed fail-closed
      reject (:fs/toctou-symlink / :fs/toctou-identity-mismatch) and the
      swapped file is NOT read, so zero content reaches CAS.
    - Platform-graded identity — identity-of reports the strongest
      available token per platform: :file-key (inode identity) from the
      basic file view when the host/populates it (POSIX and some NTFS
      hosts), plus :real-path, :size and :last-modified. Where :file-key
      is unavailable (e.g. a host returning null), the identity DEGRADES
      to real-path/size/last-modified rather than silently pretending an
      unverifiable token is strong; reading an unverifiable path still
      fails closed because real-path resolution and the atomic NOFOLLOW
      open also reject. The grading is deliberate: on every platform the
      re-check is either satisfied or the capture throw — never skipped."
  (:require [clojure.edn :as edn]
            [evoclj.fs.walk :as walk]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption OpenOption Path StandardOpenOption)
           (java.nio.file.attribute BasicFileAttributes)))

(declare identity-of capture-entry! read-open-bytes)

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
  "Gather path + attribute metadata (size, readability, regular-file)
  for every walked file WITHOUT reading any content (INV-03). Fail-closed:
  an entry that is not a regular file or is not readable throws
  :fs/unreadable before the capture phase touches a single byte.

  Each returned entry also carries the platform-graded file :identity for
  the TOCTOU re-check: the capture step re-verifies this identity and
  rejects a path swapped after preflight (see capture-entry! / identity-of)."
  [entries-raw]
  (mapv (fn [{:keys [path physical-path]}]
          (let [^Path p physical-path
                attrs (try
                        (Files/readAttributes p BasicFileAttributes
                                              (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                        (catch java.nio.file.NoSuchFileException _
                          (throw (err/error :fs/unreadable
                                            "entry is missing"
                                            {:path path}))))
                _ (when (.isSymbolicLink attrs)
                    (throw (err/error :fs/symlink-rejected
                                      "entry is a symbolic link"
                                      {:path path})))
                _ (when-not (.isRegularFile attrs)
                    (throw (err/error :fs/unreadable
                                      "entry is not a regular file"
                                      {:path path})))
                _ (when-not (Files/isReadable p)
                    (throw (err/error :fs/unreadable
                                      "entry is not readable"
                                      {:path path})))]
            {:path path :physical-path p :size (.size attrs)
             :identity (identity-of p attrs)}))
        entries-raw))

(defn- identity-of
  "Platform-graded file identity used for the TOCTOU re-verify.

  Captured at preflight (in preflight-entries!) and again at capture (in
  capture-entry!). A mismatch between the two means the on-disk object at
  the logical path changed asynchronously after preflight (it was
  retargeted, replaced, or a symbolic link was introduced) and the file
  must NOT be read.

  Grading (documented in the ns docstring): the strongest available token
  is used. `:file-key` (inode identity from the basic file view) is the
  primary identity when the host populates it (POSIX, some NTFS hosts);
  where it is null the identity degrades to real-path/size/last-modified
  so the check never silently drops to 'unverifiable => accept'. `:size`
  and `:last-modified` catch a same-path replacement even when the weak
  platform does not expose a stable :file-key."
  [^Path p ^BasicFileAttributes attrs]
  {:real-path (try
                (str (.toRealPath p (make-array LinkOption 0)))
                (catch java.io.IOException e
                  (throw (err/error :fs/unreadable
                                    "cannot resolve real path for file identity"
                                    {:path (str p) :cause (.getMessage e)}))))
   :file-key (.fileKey attrs)
   :size (.size attrs)
   :last-modified (.toMillis (.lastModifiedTime attrs))})

(defn- read-open-bytes
  "Read all bytes of `p` opening it WITHOUT following a symbolic link
  (NOFOLLOW_LINKS), so a path switched to a symlink after preflight is
  rejected at open rather than followed. Used only after the identity
  re-verify has passed."
  [^Path p]
  (with-open [in (Files/newInputStream p
                                       (into-array OpenOption
                                                   [StandardOpenOption/READ
                                                    LinkOption/NOFOLLOW_LINKS]))]
    (.readAllBytes in)))

(defn- capture-entry!
  "Capture a single preflighted entry, hardening the preflight->read window.

  `entry` is a preflight entry produced by preflight-entries! carrying
  {:path :physical-path :identity}. On return it stores the file's exact
  bytes in CAS and yields {:path :artifact/id :size :physical-path}.

  TOCTOU hardening (NOFOLLOW + identity re-check, platform-graded):
    1. Re-read the current file attributes (NOFOLLOW) at capture time.
       Symlink -> :fs/toctou-symlink; missing/unreadable/config -> the
       verification throws :fs/toctou-identity-mismatch (fail-closed).
    2. Re-verify the identity (:real-path :file-key :size :last-modified)
       against the preflight :identity; a mismatch (path retargeted, file
       replaced, symlink introduced) -> :fs/toctou-identity-mismatch.
    3. Only then read bytes via a NOFOLLOW open and write them to CAS.

  A mismatch is rejected BEFORE the read, so the swapped file's bytes are
  never read and no content reaches CAS."
  [cas {:keys [path physical-path identity]}]
  (let [^Path p physical-path
        attrs (try
                (Files/readAttributes p BasicFileAttributes
                                      (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                (catch java.nio.file.NoSuchFileException _
                  (throw (err/error :fs/toctou-identity-mismatch
                                    "snapshot target vanished between preflight and capture"
                                    {:path path})))
                (catch java.io.IOException e
                  (throw (err/error :fs/toctou-identity-mismatch
                                    "snapshot target is not verifiable at capture time"
                                    {:path path :cause (.getMessage e)}))))
        _ (when (.isSymbolicLink attrs)
            (throw (err/error :fs/toctou-symlink
                              "snapshot target became a symbolic link between preflight and capture"
                              {:path path})))
        _ (when-not (.isRegularFile attrs)
            (throw (err/error :fs/toctou-identity-mismatch
                              "snapshot target is no longer a regular file"
                              {:path path})))
        _ (when-not (= identity (identity-of p attrs))
            (throw (err/error :fs/toctou-identity-mismatch
                              "snapshot target changed identity between preflight and capture"
                              {:path path})))
        ba (try
             (read-open-bytes p)
             (catch java.io.IOException e
               ;; A swap in the window between identity-verify and the open is
               ;; still fail-closed + typed (NOFOLLOW rejects a symlink here).
               (throw (err/error (if (Files/isSymbolicLink p)
                                   :fs/toctou-symlink
                                   :fs/toctou-identity-mismatch)
                                 "snapshot target became unreadable at capture"
                                 {:path path :cause (.getMessage e)}))))
        {:keys [artifact/id size]} (cas/put-bytes! cas ba {:media-type "application/octet-stream"})]
    {:path path :artifact/id id :size size :physical-path p}))

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
        ;; 2) CAPTURE — only after preflight passes do we read + write to CAS.
        ;; Each entry is hardened: NOFOLLOW open + identity re-check so a file
        ;; swapped after preflight is rejected typed and never read (WO-S5).
        entries (mapv (fn [entry] (capture-entry! cas entry)) preflight)
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
