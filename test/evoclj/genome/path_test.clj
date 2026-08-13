(ns evoclj.genome.path-test
  "Tests for canonical relative-path validation (Task 1.3).

  Genome file references are canonical slash-separated relative paths.
  `normalize-relative-path` canonicalizes backslashes to forward
  slashes so Windows-style input cannot smuggle traversal, and throws
  :genome/path-invalid for anything that cannot be represented
  canonically: absolute paths, drive letters, `.`/`..` components,
  empty components, and NUL bytes. `allowed-genome-path?` is the
  boolean gatekeeper and additionally rejects paths whose components
  traverse a symbolic link on disk (normative rule 4)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.genome.path :as path])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path Paths LinkOption)
           (java.nio.file.attribute FileAttribute)))

(defn- path-error
  "The ExceptionInfo thrown by normalize-relative-path, or nil when the
  path normalizes cleanly."
  [s]
  (try (path/normalize-relative-path s)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- is-path-invalid [s]
  (let [e (path-error s)]
    (is (instance? clojure.lang.ExceptionInfo e) (pr-str s))
    (is (= :genome/path-invalid (:error/type (ex-data e))) (pr-str s))))

(deftest normalize-accepts-canonical-relative-paths
  (testing "canonical slash-separated relative paths pass through unchanged"
    (is (= "manifest.edn" (path/normalize-relative-path "manifest.edn")))
    (is (= "skills/route.edn" (path/normalize-relative-path "skills/route.edn")))
    (is (= "a/b/c.edn" (path/normalize-relative-path "a/b/c.edn"))))
  (testing "backslashes are canonicalized to forward slashes"
    (is (= "a/b.edn" (path/normalize-relative-path "a\\b.edn")))
    (is (= "a/b/c.edn" (path/normalize-relative-path "a\\b/c.edn")))))

(deftest normalize-rejects-parent-traversal
  (is-path-invalid "../x")
  (is-path-invalid "a/../../b")
  (is-path-invalid "a/../b")
  (is-path-invalid "..")
  (is-path-invalid "a/.."))

(deftest normalize-rejects-absolute-paths
  (is-path-invalid "/tmp/x")
  (is-path-invalid "/x")
  (is-path-invalid "C:/x")
  (is-path-invalid "C:\\x")
  (testing "UNC shares become //server/share and are absolute"
    (is-path-invalid "\\\\server\\share\\x")))

(deftest normalize-rejects-windows-backslash-traversal
  (is-path-invalid "..\\..\\secret.edn")
  (is-path-invalid "a\\..\\b")
  (is-path-invalid "..\\x"))

(deftest normalize-rejects-dot-and-empty-components
  (is-path-invalid ".")
  (is-path-invalid "./x")
  (is-path-invalid "x/.")
  (is-path-invalid "x/")
  (is-path-invalid "x//y")
  (is-path-invalid ""))

(deftest normalize-rejects-nul-and-non-strings
  (is-path-invalid "a\u0000b")
  (is-path-invalid nil)
  (is-path-invalid 42))

(deftest allowed-genome-path-lexical
  (testing "canonical relative paths are allowed"
    (is (path/allowed-genome-path? "manifest.edn"))
    (is (path/allowed-genome-path? "skills/route.edn"))
    (testing "backslashes canonicalize to separators before the check"
      (is (path/allowed-genome-path? "a\\b.edn"))))
  (testing "unsafe paths are rejected"
    (is (not (path/allowed-genome-path? "../x")))
    (is (not (path/allowed-genome-path? "/tmp/x")))
    (is (not (path/allowed-genome-path? "a/../../b")))
    (is (not (path/allowed-genome-path? "..\\..\\secret.edn")))
    (is (not (path/allowed-genome-path? "C:\\tmp\\evil.edn")))
    (is (not (path/allowed-genome-path? "")))
    (is (not (path/allowed-genome-path? nil)))
    (is (not (path/allowed-genome-path? "a//b")))))

(deftest bytewise-lexical-order
  ;; U+1F600 😀 (UTF-8 F0 9F 98 80) vs U+E000 (UTF-8 EE 80 80):
  ;; bytewise the private-use char sorts first (0xEE < 0xF0), the
  ;; opposite of Clojure's UTF-16 code-unit compare (0xD83D < 0xE000).
  ;; Normative rule 5 demands bytewise lexical order.
  (let [emoji "\uD83D\uDE00"
        private-use "\uE000"]
    (is (neg? (path/bytewise-compare private-use emoji)))
    (is (pos? (path/bytewise-compare emoji private-use)))
    (is (pos? (compare private-use emoji)))   ; documents the divergence
    (is (zero? (path/bytewise-compare "a/b.txt" "a/b.txt")))
    (is (neg? (path/bytewise-compare "a.txt" "b.txt")))))

;; --- filesystem symlink fixture -------------------------------------------

(defn- temp-dir!
  ^Path []
  (Files/createTempDirectory "evoclj-path-test" (make-array FileAttribute 0)))

(defn- write-text-file!
  "Write `content` to `dir`/`rel` (a Path), creating parent directories."
  [^Path dir rel ^String content]
  (let [p (.resolve dir rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))
    p))

(defn- try-create-symlink!
  "Best-effort Files/createSymbolicLink. Returns false when the host
  refuses (Windows hosts without Developer Mode or symlink privileges)."
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

(deftest symlink-fixture-rejected
  (let [dir (temp-dir!)
        real (write-text-file! dir (Paths/get "real.edn" (make-array String 0))
                               "{:ok true}\n")
        link (.resolve dir "link.edn")]
    (try
      (if (try-create-symlink! real link)
        (testing "a path whose final component is a symlink is rejected"
          (is (not (path/allowed-genome-path? dir "link.edn")))
          (is (path/allowed-genome-path? dir "real.edn")))
        (testing "symlink creation unavailable on this host; skipped"
          (is true)))
      (finally
        (delete-recursively! dir)))))

(deftest symlink-intermediate-component-rejected
  (let [dir (temp-dir!)
        outside (temp-dir!)
        _ (write-text-file! outside (Paths/get "secret.edn" (make-array String 0))
                            "{:secret true}\n")
        sub (.resolve dir "sub")]
    (try
      (if (try-create-symlink! outside sub)
        (testing "a path traversing a symlinked directory is rejected"
          (is (not (path/allowed-genome-path? dir "sub/secret.edn")))
          (is (not (path/allowed-genome-path? dir "sub/other.edn"))))
        (testing "symlink creation unavailable on this host; skipped"
          (is true)))
      (finally
        (delete-recursively! dir)
        (delete-recursively! outside)))))
