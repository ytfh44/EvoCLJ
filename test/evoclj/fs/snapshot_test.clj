(ns evoclj.fs.snapshot-test
  "S4 — snapshot preflight / streaming limits (reject before read) (e2e#9).

  Guards INV-03 (limits are enforced before reads): snapshot-tree! must
  validate path, readability, and the configured limits against METADATA
  gathered without reading any content, and reject a typed error BEFORE
  any file byte is read or written to CAS. An over-limit tree must
  therefore produce ZERO new CAS artifacts and never leave a partial
  snapshot.

  Every test drives the production evoclj.fs.snapshot/snapshot-tree! over
  a real temp directory and a real CAS handle (no injected fn, no cas-fn;
  INV-09)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.fs.snapshot :as snap]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir
  "Fresh temp directory (auto-rooted; the test JVM cleans it up)."
  []
  (Files/createTempDirectory "evoclj-snapshot-" (make-array FileAttribute 0)))

(defn- write-file!
  "Write `content` (String) under root/rel as EXACT UTF-8 bytes."
  [^Path root ^String rel content]
  (let [p (.resolve root rel)]
    (when-let [parent (.getParent p)]
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))
    p))

(defn- count-cas-files
  "Count every regular file under a CAS root (each stored artifact is
  body + meta.edn, so this grows monotonically with captures)."
  [cas-root]
  (let [f (clojure.java.io/file (str cas-root))]
    (if-not (.isDirectory ^java.io.File f)
      0
      (count (filter #(.isFile ^java.io.File %) (file-seq f))))))

(defn- caught-type
  "Run `f` returning the :error/type of the ExceptionInfo it throws, or
  ::no-error if it returns normally."
  [f]
  (try (f) ::no-error
       (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(defn- generous-limits
  "Comfortable defaults for fixtures well inside any boundary."
  []
  {:max-depth 32
   :max-files 2000
   :max-total-bytes (* 20 1024 1024)
   :max-file-bytes (* 5 1024 1024)})

;; ---------------------------------------------------------------------------
;; HAPPY + branch — deterministic, content-addressed capture within limits
;; ---------------------------------------------------------------------------

(deftest within-limits-snapshot-is-deterministic
  (let [dir (temp-dir)
        cas-a (temp-dir)
        cas-b (temp-dir)
        _ (write-file! dir "SKILL.md" "# Skill\nbody\n")
        _ (write-file! dir "references/guide.md" "guide")
        r1 (snap/snapshot-tree! dir (cas/->cas (str cas-a)) (generous-limits))
        r2 (snap/snapshot-tree! dir (cas/->cas (str cas-b)) (generous-limits))]
    (is (= (:tree/id r1) (:tree/id r2))
        "identical tree bytes -> identical content-addressed tree id across CAS roots")
    (is (= (:manifest r1) (:manifest r2))
        "manifest is a pure function of the tree bytes (no host/mtime/absolute path)")
    (is (= 2 (:size r1)))
    (is (= #{"SKILL.md" "references/guide.md"}
           (set (keys (:entries (:manifest r1))))))
    (is (= "sha256:"
           (subs (:tree/id r1) 0 7))
        "tree id uses the canonical sha256:<hex> artifact form")))

(deftest empty-limits-map-still-snapshots
  (let [dir (temp-dir)
        r (snap/snapshot-tree! dir (cas/->cas (str (temp-dir))) {})]
    (is (= 0 (:size r)) "empty tree with no limits yields an empty single-file manifest")) )

(deftest exactly-at-limit-is-allowed-not-advisory
  (testing "a tree exactly at each boundary is admitted (limits are real, not off-by-one)"
    (let [dir (temp-dir)
          _ (write-file! dir "a.md" "aa")          ; 2 bytes
          _ (write-file! dir "b.md" "bb")          ; 2 bytes, depth 1 each
          r (snap/snapshot-tree! dir (cas/->cas (str (temp-dir)))
                                 {:max-files 2
                                  :max-depth 1
                                  :max-file-bytes 2
                                  :max-total-bytes 4})]
      (is (= 2 (:size r))))))

;; ---------------------------------------------------------------------------
;; BRANCH — each limit is enforced by the PREFLIGHT: typed reject,
;; zero CAS artifacts (no content read / no partial snapshot)
;; ---------------------------------------------------------------------------

(deftest max-files-over-limit-rejected-before-read
  (let [dir (temp-dir)
        cas-dir (temp-dir)
        _ (write-file! dir "a.md" "a")
        _ (write-file! dir "b.md" "b")
        before (count-cas-files cas-dir)]
    (is (= :fs/snapshot-limit-exceeded
           (caught-type #(snap/snapshot-tree! dir (cas/->cas (str cas-dir)) {:max-files 1}))))
    (is (= before (count-cas-files cas-dir))
        "an over-limit tree must write zero CAS artifacts (reject BEFORE read)")))

(deftest max-depth-over-limit-rejected-before-read
  (let [dir (temp-dir)
        cas-dir (temp-dir)
        _ (write-file! dir "a/b/c.md" "deep")
        before (count-cas-files cas-dir)]
    (is (= :fs/snapshot-limit-exceeded
           (caught-type #(snap/snapshot-tree! dir (cas/->cas (str cas-dir)) {:max-depth 2}))))
    (is (= before (count-cas-files cas-dir))
        "depth-over-limit tree rejected with zero CAS artifacts before read")))

(deftest max-total-bytes-over-limit-rejected-before-read
  (let [dir (temp-dir)
        cas-dir (temp-dir)
        _ (write-file! dir "a.md" "1234567890")   ; 10 bytes
        before (count-cas-files cas-dir)]
    (is (= :fs/snapshot-limit-exceeded
           (caught-type #(snap/snapshot-tree! dir (cas/->cas (str cas-dir)) {:max-total-bytes 5}))))
    (is (= before (count-cas-files cas-dir))
        "total-bytes-over-limit tree rejected with zero CAS artifacts before read")))

(deftest max-file-bytes-over-limit-rejected-before-read
  (let [dir (temp-dir)
        cas-dir (temp-dir)
        _ (write-file! dir "huge.bin" (apply str (repeat 200 "x")))
        before (count-cas-files cas-dir)]
    (is (= :fs/snapshot-limit-exceeded
           (caught-type #(snap/snapshot-tree! dir (cas/->cas (str cas-dir)) {:max-file-bytes 100}))))
    (is (= before (count-cas-files cas-dir))
        "file-bytes-over-limit tree rejected with zero CAS artifacts before read")))

;; ---------------------------------------------------------------------------
;; FAULT — a huge tree rejected before read; the typed error carries the
;; violated limit and the actual value (fail-closed, not a vague failure)
;; ---------------------------------------------------------------------------

(deftest huge-tree-many-entries-rejected-before-read
  (let [dir (temp-dir)
        cas-dir (temp-dir)
        _ (dotimes [i 100] (write-file! dir (str "d" i ".md") (str "content " i)))
        before (count-cas-files cas-dir)]
    (is (= :fs/snapshot-limit-exceeded
           (caught-type #(snap/snapshot-tree! dir (cas/->cas (str cas-dir)) {:max-files 10}))))
    (is (= before (count-cas-files cas-dir))
        "a 100-file tree over :max-files is rejected with zero CAS artifacts")))

(deftest limit-error-carries-limit-and-actual
  (let [dir (temp-dir)
        _ (write-file! dir "a.md" "a")
        _ (write-file! dir "b.md" "b")
        e (try (snap/snapshot-tree! dir (cas/->cas (str (temp-dir))) {:max-files 1})
               (catch clojure.lang.ExceptionInfo ex ex))]
    (is (some? e))
    (is (= :fs/snapshot-limit-exceeded (:error/type (ex-data e))))
    (is (= 1 (:limit (ex-data e))))
    (is (= 2 (:actual (ex-data e)))
        "the reject is typed and carries the violated limit/actual values")))

(deftest combined-limit-reject-dominates
  (testing "the first violated limit is reported (fail-closed, deterministic)"
    (let [dir (temp-dir)
          _ (write-file! dir "a/b/c.md" (apply str (repeat 50 "x")))
          e (try (snap/snapshot-tree! dir (cas/->cas (str (temp-dir)))
                                      {:max-files 0
                                       :max-depth 2
                                       :max-file-bytes 10
                                       :max-total-bytes 5})
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e))
      (is (= :fs/snapshot-limit-exceeded (:error/type (ex-data e))))
      (is (= 0 (:limit (ex-data e)))
          "max-files is checked first among the limits"))))

;; ---------------------------------------------------------------------------
;; BRANCH — FAIL-CLOSED UNREADABLE (non-regular entry) — regression for the
;; :fs/unreadable preflight check that had NO test before this item (S4).
;; ---------------------------------------------------------------------------

(deftest non-regular-entry-rejected-fail-closed
  (testing "preflight rejects a non-regular entry as :fs/unreadable, before any read"
    ;; preflight-entries! is private, so we reach it directly (#') with the
    ;; exact {:path :physical-path} entry shape the production capture path
    ;; consumes. We cannot drive this through snapshot-tree! portably: walk-tree
    ;; never emits a directory (or any non-regular/special file) as an entry on
    ;; a cross-platform host — symlinks are already rejected at the walk with
    ;; :fs/symlink-rejected and a FIFO/pipe cannot be created portably (Windows).
    ;; The chosen trigger is a real subdirectory standing where a file is
    ;; expected: Files/isRegularFile is deterministically false on every
    ;; platform, so the fail-closed check fires exactly as it would in
    ;; production. The readability half of this branch is ACL-only and is not
    ;; exercised here (see note).
    (let [dir (temp-dir)
          sub (.resolve dir "subdir-expected-as-file")
          _ (Files/createDirectory sub (make-array FileAttribute 0))
          raw [{:path "subdir-expected-as-file" :physical-path sub}]
          e (try (#'snap/preflight-entries! raw)
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e) "a non-regular entry must throw (fail-closed), never silently pass")
      (is (= :fs/unreadable (:error/type (ex-data e))))
      (is (= "subdir-expected-as-file" (:path (ex-data e)))
          "the reject names the offending entry, so a fail-open mutation is detectable"))))

;; ---------------------------------------------------------------------------
;; CONCURRENCY — two independent captures into one shared CAS stay isolated
;; (CAS writes are content-addressed + atomic; the preflight adds no shared
;; mutable state)
;; ---------------------------------------------------------------------------

(deftest concurrent-captures-into-shared-cas-stay-independent
  (let [dir-a (temp-dir)
        dir-b (temp-dir)
        cas-dir (temp-dir)
        _ (write-file! dir-a "SKILL.md" "AAA")
        _ (write-file! dir-a "ref.md" "ref-A")
        _ (write-file! dir-b "SKILL.md" "BBB")
        handle (cas/->cas (str cas-dir))
        ;; launch both captures concurrently against the same CAS handle
        fa (future (snap/snapshot-tree! dir-a handle (generous-limits)))
        fb (future (snap/snapshot-tree! dir-b handle (generous-limits)))
        ra @fa
        rb @fb
        ;; rebuild both trees from CAS and confirm exact bytes survived
        ra2 (snap/snapshot-tree! dir-a handle (generous-limits))
        rb2 (snap/snapshot-tree! dir-b handle (generous-limits))]
    (is (= "AAA" (String. (snap/get-file-bytes handle (:manifest ra) "SKILL.md") StandardCharsets/UTF_8)))
    (is (= "ref-A" (String. (snap/get-file-bytes handle (:manifest ra) "ref.md") StandardCharsets/UTF_8)))
    (is (= "BBB" (String. (snap/get-file-bytes handle (:manifest rb) "SKILL.md") StandardCharsets/UTF_8)))
    (is (= (:tree/id ra) (:tree/id ra2))
        "re-capturing an identical tree into the same CAS is idempotent (same content id)")
    (is (= (:tree/id rb) (:tree/id rb2)))))
