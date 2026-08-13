(ns evoclj.genome.path
  "Canonical relative-path validation for Genome bundles (Task 1.3).

  Genome file references are canonical slash-separated relative paths.
  `normalize-relative-path` canonicalizes a candidate path — backslashes
  become forward slashes so Windows-style input cannot smuggle
  traversal — and throws a typed :genome/path-invalid error for
  anything that cannot be represented canonically: absolute paths,
  drive letters, `.`/`..` components, empty components, and NUL bytes.
  `allowed-genome-path?` is the boolean gatekeeper: it additionally
  rejects any path whose components traverse a symbolic link on disk
  (normative rule 4).

  Global Constraints 1 and 6 require Genome identity to depend only on
  logical content, so nothing host-specific (mtime, inode, owner,
  absolute path) ever becomes part of a Genome path."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path Paths)))

(defn- drive-letter? [s]
  (boolean (re-matches #"^[A-Za-z]:.*" s)))

(defn normalize-relative-path
  "Return the canonical slash-separated relative form of `s`.

  Backslashes are treated as separators and become `/`, so Windows
  style input is canonicalized before any further check. Throws
  :genome/path-invalid when `s` is not a non-empty string, contains a
  NUL byte, is absolute (leading `/`, drive letter, or UNC share), or
  contains `.`, `..`, or empty path components — the canonical form
  never contains those."
  [s]
  (when-not (and (string? s)
                 (not (str/includes? s "\u0000")))
    (throw (err/error :genome/path-invalid
                      "genome path must be a non-empty string without NUL bytes"
                      {:path s})))
  (when (zero? (count s))
    (throw (err/error :genome/path-invalid
                      "genome path must be non-empty"
                      {:path s})))
  (when (drive-letter? s)
    (throw (err/error :genome/path-invalid
                      "genome path must be relative, not absolute"
                      {:path s})))
  (let [slashed (str/replace s "\\" "/")]
    (when (str/starts-with? slashed "/")
      (throw (err/error :genome/path-invalid
                        "genome path must be relative, not absolute"
                        {:path s})))
    ;; Java String.split silently drops trailing empty components, so
    ;; trailing slashes and doubled separators are rejected explicitly.
    (when (str/ends-with? slashed "/")
      (throw (err/error :genome/path-invalid
                        "genome path must not contain empty components"
                        {:path s})))
    (when (str/includes? slashed "//")
      (throw (err/error :genome/path-invalid
                        "genome path must not contain empty components"
                        {:path s})))
    (let [components (str/split slashed #"/")]
      (when (some #(contains? #{"." ".."} %) components)
        (throw (err/error :genome/path-invalid
                          "genome path must not contain '.' or '..' components"
                          {:path s})))
      (str/join "/" components))))

(defn bytewise-compare
  "Compare two strings by their UTF-8 bytes in unsigned lexical order.

  This is the canonical Genome path ordering (normative rule 5):
  entries sort by the normalized path's UTF-8 bytes, not by Clojure's
  UTF-16 code-unit comparison, so e.g. U+1F600 sorts after U+E000 even
  though its UTF-16 code unit sorts before."
  [a b]
  (let [ba (.getBytes ^String a StandardCharsets/UTF_8)
        bb (.getBytes ^String b StandardCharsets/UTF_8)
        n (min (alength ba) (alength bb))]
    (loop [i 0]
      (if (< i n)
        (let [c (compare (bit-and 0xff (aget ba i))
                         (bit-and 0xff (aget bb i)))]
          (if (zero? c)
            (recur (inc i))
            c))
        (compare (alength ba) (alength bb))))))

(defn- base-path
  "Coerce a base (Path or String) to a java.nio.file.Path."
  [base]
  (cond
    (instance? Path base) base
    (string? base) (Paths/get base (make-array String 0))
    :else (throw (err/error :genome/path-invalid
                            "base must be a java.nio.file.Path or a string"
                            {:base base}))))

(defn- symlink-component?
  "True when any component of `rel` resolved against `base` is a
  symbolic link. Nonexistent components are never links, so this is
  safe for paths that do not exist yet."
  [^Path base ^Path rel]
  (loop [acc base
         remaining (iterator-seq (.iterator rel))]
    (if (seq remaining)
      (let [acc' (.resolve acc ^Path (first remaining))]
        (if (Files/isSymbolicLink acc')
          true
          (recur acc' (rest remaining))))
      false)))

(defn allowed-genome-path?
  "True when `path` is a safe canonical relative Genome path.

  A path is allowed when it normalizes to a canonical relative path
  (see `normalize-relative-path`) and no component of the path —
  resolved against `base` (a java.nio.file.Path or string; defaults to
  the JVM working directory) — is a symbolic link. Symlinks are
  rejected per normative rule 4 so a Genome bundle cannot reach
  outside itself. Any error, including an unresolvable base, is
  treated as disallowed (fail-closed)."
  ([path]
   (allowed-genome-path? (Paths/get "." (make-array String 0)) path))
  ([base path]
   (try
     (let [rel (Paths/get (normalize-relative-path path) (make-array String 0))]
       (not (symlink-component? (base-path base) rel)))
     (catch Exception _ false))))
