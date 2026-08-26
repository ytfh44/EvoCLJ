(ns evoclj.fs.snapshot-toctou-test
  "S5 — snapshot TOCTOU hardening (NOFOLLOW + identity re-check,
  platform-graded) (e2e#10).

  Self constraint is the WINDOW between the S4 PREFLIGHT (metadata
  validation, no content read) and the CAPTURE read (readAllBytes + CAS
  write). S5 hardens that window so a file swapped after validation —
  replaced, retargeted, or made into a symbolic link — is rejected with a
  typed error and its bytes are never read or stored.

  Guards exercised here:

    * NOFOLLOW — the capture reads WITHOUT following a symbolic link, so
      a validated path swapped to a symlink after preflight is never
      followed to an outside target and the outside bytes never reach CAS.
    * Identity re-check — the file identity (real-path / file-key / size /
      last-modified, platform-graded) is re-verified between preflight
      and capture; a mismatch is a typed fail-closed reject and the
      swapped file is NOT read.
    * Platform-graded fail-closed — where a strong identity token
      (file-key) is unavailable, the check still fails-closed on
      real-path/size/last-modified rather than silently honoring an
      unverifiable path. The vanished-file case is the fail-closed proof.

  Every test drives the production evoclj.fs.snapshot capture path over a
  real temp directory, a real CAS handle, and a genuinely swapped
  filesystem state — no injected fn, no cas-fn (INV-09)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.fs.snapshot :as snap]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path LinkOption OpenOption)
           (java.nio.file.attribute FileAttribute)))

;; --- helpers ---------------------------------------------------------------

(def ^:private generous-limits
  {:max-depth 32
   :max-files 2000
   :max-total-bytes (* 20 1024 1024)
   :max-file-bytes (* 5 1024 1024)})

(defn- temp-dir
  ^Path []
  (Files/createTempDirectory "evoclj-toctou-" (make-array FileAttribute 0)))

(defn- write-file!
  "Write `content` (UTF-8) under root/rel as EXACT bytes."
  [^Path root ^String rel ^String content]
  (let [p (.resolve root rel)]
    (when-let [parent (.getParent p)]
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array OpenOption 0))
    p))

(defn- count-cas-files
  "Count every regular file under a CAS root (body + meta.edn each)."
  [cas-root]
  (let [f (clojure.java.io/file (str cas-root))]
    (if-not (.isDirectory ^java.io.File f)
      0
      (count (filter #(.isFile ^java.io.File %) (file-seq f))))))

(defn- caught-type
  "Return the :error/type of the ExceptionInfo `f` throws, or ::no-error."
  [f]
  (try (f) ::no-error
       (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(defn- try-create-symlink!
  "Best-effort Files/createSymbolicLink. Returns false when the host
  refuses (no Developer Mode / symlink privilege)."
  [^Path target ^Path link]
  (try
    (Files/createSymbolicLink link target (make-array FileAttribute 0))
    true
    (catch Exception _ false)))

(defn- delete-recursively! [^Path dir]
  (when (Files/exists dir (make-array LinkOption 0))
    (let [f (.toFile dir)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)]
          (delete-recursively! (.toPath c))))
      (Files/deleteIfExists dir))))

(defn- preflight-one
  "Run the production preflight for a single real file and return its
  entry (carries the preflight :identity used for the TOCTOU re-check)."
  [^Path physical ^String rel]
  (first (#'snap/preflight-entries! [{:path rel :physical-path physical}])))

;; ---------------------------------------------------------------------------
;; HAPPY — an unchanged file is accepted (no false positive)
;; ---------------------------------------------------------------------------

(deftest unchanged-tree-snapshots-no-false-positive
  (testing "the full production snapshot-tree! accepts an unchanged tree"
    (let [dir (temp-dir)
          cas-dir (temp-dir)
          _ (write-file! dir "SKILL.md" "hello world")
          _ (write-file! dir "references/guide.md" "guide body")
          handle (cas/->cas (str cas-dir))
          r (snap/snapshot-tree! dir handle generous-limits)]
      (is (= 2 (:size r)))
      (is (= "hello world"
             (String. (snap/get-file-bytes handle (:manifest r) "SKILL.md")
                      StandardCharsets/UTF_8)))
      (is (= "guide body"
             (String. (snap/get-file-bytes handle (:manifest r) "references/guide.md")
                      StandardCharsets/UTF_8))))))

(deftest unchanged-entry-captures-identical-bytes
  (testing "capturing an unchanged preflighted entry stores its exact bytes"
    (let [dir (temp-dir)
          cas-dir (temp-dir)
          _ (write-file! dir "SKILL.md" "exact bytes here")
          physical (.resolve dir "SKILL.md")
          entry (preflight-one physical "SKILL.md")
          handle (cas/->cas (str cas-dir))
          result (#'snap/capture-entry! handle entry)]
      (is (= "SKILL.md" (:path result)))
      (is (= "exact bytes here"
             (String. (cas/get-bytes handle (:artifact/id result)) StandardCharsets/UTF_8))))))

;; ---------------------------------------------------------------------------
;; BRANCH / FAULT — a validated path swapped to a SYMLINK after preflight
;; → typed :fs/toctou-symlink, and the outside target is never read.
;; ---------------------------------------------------------------------------

(deftest symlink-swapped-in-is-rejected-no-outside-read
  (testing "a validated path switched to a symlink after preflight is rejected typed, no outside read"
    (let [dir (temp-dir)
          outside (temp-dir)
          cas-dir (temp-dir)
          _ (write-file! dir "SKILL.md" "ORIGINAL")
          secret (.resolve outside "secret.txt")
          _ (write-file! outside "secret.txt" "TOP-SECRET-OUTSIDE")
          physical (.resolve dir "SKILL.md")
          entry (preflight-one physical "SKILL.md")
          handle (cas/->cas (str cas-dir))
          before (count-cas-files cas-dir)]
      (Files/delete physical)
      (try
        (if (try-create-symlink! secret physical)
          (do
            (is (= :fs/toctou-symlink
                   (caught-type #(#'snap/capture-entry! handle entry)))
                "a symlink swap must fail-closed with the typed :fs/toctou-symlink")
            (is (= before (count-cas-files cas-dir))
                "the outside bytes MUST NOT be read or stored (no follow, no outside read)"))
          (testing "symlink creation unavailable on this host; skipped" (is true)))
        (finally
          (delete-recursively! dir)
          (delete-recursively! outside)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; BRANCH / FAULT — a validated path RE-TARGETED to a different file after
;; preflight → typed :fs/toctou-identity-mismatch; the swapped file is not
;; read (zero new CAS artifacts, content bytes never stored).
;; ---------------------------------------------------------------------------

(deftest retargeted-path-identity-mismatch-rejected
  (testing "a validated path retargeted to a different file after preflight is rejected typed"
    (let [dir (temp-dir)
          cas-dir (temp-dir)
          _ (write-file! dir "SKILL.md" "AAAA")
          physical (.resolve dir "SKILL.md")
          entry (preflight-one physical "SKILL.md")
          orig-mtime (:last-modified (:identity entry))
          handle (cas/->cas (str cas-dir))
          before (count-cas-files cas-dir)]
      ;; replace (delete + recreate) a DIFFERENT file at the same logical path,
      ;; forcing its mtime to MATCH the original so ONLY :size differs — this
      ;; deterministically exercises the :size dimension of the identity check.
      (Files/delete physical)
      (write-file! dir "SKILL.md" "MUCH-LONGER-CONTENT")
      (Files/setLastModifiedTime physical
                                 (java.nio.file.attribute.FileTime/fromMillis orig-mtime))
      (is (= :fs/toctou-identity-mismatch
             (caught-type #(#'snap/capture-entry! handle entry)))
          "a retargeted path must fail-closed with the typed :fs/toctou-identity-mismatch")
      (is (= before (count-cas-files cas-dir))
          "the swapped file's bytes MUST NOT be read into CAS")))
  (testing "same-size retarget still fails-closed (platform-graded fallback)"
    ;; A same-size replacement is caught by :file-key on hosts that expose it;
    ;; where :file-key is unavailable (this host), the graded fallback uses
    ;; :last-modified — forced to differ here so the check is deterministic.
    ;; Either way a replaced file must reject, never be silently accepted.
    (let [dir (temp-dir)
          cas-dir (temp-dir)
          _ (write-file! dir "SKILL.md" "AAAA")
          physical (.resolve dir "SKILL.md")
          entry (preflight-one physical "SKILL.md")
          orig-mtime (:last-modified (:identity entry))
          handle (cas/->cas (str cas-dir))
          before (count-cas-files cas-dir)]
      (Files/delete physical)
      (write-file! dir "SKILL.md" "BBBB")
      (Files/setLastModifiedTime physical
                                 (java.nio.file.attribute.FileTime/fromMillis
                                  (- orig-mtime 60000)))
      (is (= :fs/toctou-identity-mismatch
             (caught-type #(#'snap/capture-entry! handle entry))))
      (is (= before (count-cas-files cas-dir))))))

;; ---------------------------------------------------------------------------
;; BRANCH — PLATFORM-GRADED FAIL-CLOSED. The graded identity never drops the
;; re-check, and an unverifiable path (vanished file) fails closed.
;; ---------------------------------------------------------------------------

(deftest identity-is-platform-graded-and-fails-closed
  (testing "identity-of reports the graded tuple (real-path / file-key / size / last-modified)"
    (let [dir (temp-dir)
          _ (write-file! dir "SKILL.md" "hello world")
          physical (.resolve dir "SKILL.md")
          {:keys [identity]} (preflight-one physical "SKILL.md")]
      (is (= #{:real-path :file-key :size :last-modified}
             (set (keys identity))))
      ;; real-path resolves to the walked file; file-key is used when present
      ;; and (graded) nil when the host cannot provide it.
      (is (= (str (.toRealPath physical (make-array LinkOption 0)))
             (:real-path identity))))))

(deftest vanished-file-fail-closed-rejected
  (testing "a validated path that vanishes before capture is rejected typed (fail-closed, unverifiable)"
    (let [dir (temp-dir)
          cas-dir (temp-dir)
          _ (write-file! dir "SKILL.md" "AAAA")
          physical (.resolve dir "SKILL.md")
          entry (preflight-one physical "SKILL.md")
          handle (cas/->cas (str cas-dir))
          before (count-cas-files cas-dir)]
      (Files/delete physical)
      (is (= :fs/toctou-identity-mismatch
             (caught-type #(#'snap/capture-entry! handle entry)))
          "an unverifiable (vanished) path must fail closed, never silently honor it")
      (is (= before (count-cas-files cas-dir))))))

;; ---------------------------------------------------------------------------
;; CONCURRENCY — the re-check adds no shared state; concurrent captures into
;; one shared CAS stay independent (regression for the new per-entry verify).
;; ---------------------------------------------------------------------------

(deftest concurrent-captures-stay-independent
  (testing "concurrent captures into a shared CAS handle stay isolated"
    (let [dir-a (temp-dir)
          dir-b (temp-dir)
          cas-dir (temp-dir)
          _ (write-file! dir-a "SKILL.md" "AAA")
          _ (write-file! dir-b "SKILL.md" "BBB")
          handle (cas/->cas (str cas-dir))
          fa (future (snap/snapshot-tree! dir-a handle generous-limits))
          fb (future (snap/snapshot-tree! dir-b handle generous-limits))
          ra @fa
          rb @fb]
      (is (= 1 (:size ra)))
      (is (= 1 (:size rb)))
      (is (= "AAA" (String. (snap/get-file-bytes handle (:manifest ra) "SKILL.md") StandardCharsets/UTF_8)))
      (is (= "BBB" (String. (snap/get-file-bytes handle (:manifest rb) "SKILL.md") StandardCharsets/UTF_8))))))
