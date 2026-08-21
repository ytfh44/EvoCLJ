(ns evoclj.fs.path
  "Generic safe relative-path handling for filesystem trees.

  This module provides the same canonicalization and validation rules
  previously held in evoclj.genome.path, extracted so that Genome and
  future Skill snapshots share one safe walker.  Genome retains its own
  wrapper for backward compatibility, but the canonical validation
  lives here."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path Paths)))

(defn- drive-letter? [s]
  (boolean (re-matches #"^[A-Za-z]:.*" s)))

(defn normalize-relative-path
  "Return the canonical slash-separated relative form of s.

  Backslashes become forward slashes. Throws :fs/path-invalid when s
  is not a non-empty string without NUL, is absolute (leading /, drive
  letter, or UNC), or contains ., .., or empty components."
  [s]
  (when-not (and (string? s)
                 (not (str/includes? s "\u0000")))
    (throw (err/error :fs/path-invalid
                      "path must be a non-empty string without NUL bytes"
                      {:path s})))
  (when (zero? (count s))
    (throw (err/error :fs/path-invalid
                      "path must be non-empty"
                      {:path s})))
  (when (drive-letter? s)
    (throw (err/error :fs/path-invalid
                      "path must be relative, not absolute"
                      {:path s})))
  (let [slashed (str/replace s "\\" "/")]
    (when (str/starts-with? slashed "/")
      (throw (err/error :fs/path-invalid
                        "path must be relative, not absolute"
                        {:path s})))
    (when (str/ends-with? slashed "/")
      (throw (err/error :fs/path-invalid
                        "path must not contain empty components"
                        {:path s})))
    (when (str/includes? slashed "//")
      (throw (err/error :fs/path-invalid
                        "path must not contain empty components"
                        {:path s})))
    (let [components (str/split slashed #"/")]
      (when (some #(contains? #{"." ".."} %) components)
        (throw (err/error :fs/path-invalid
                          "path must not contain '.' or '..' components"
                          {:path s})))
      (str/join "/" components))))

(defn bytewise-compare
  "Compare two strings by UTF-8 bytes in unsigned lexical order."
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

(defn- base-path [base]
  (cond
    (instance? Path base) base
    (string? base) (Paths/get base (make-array String 0))
    :else (throw (err/error :fs/path-invalid
                            "base must be a java.nio.file.Path or a string"
                            {:base base}))))

(defn- symlink-component?
  [^Path base ^Path rel]
  (loop [acc base
         remaining (iterator-seq (.iterator rel))]
    (if (seq remaining)
      (let [acc' (.resolve acc ^Path (first remaining))]
        (if (Files/isSymbolicLink acc')
          true
          (recur acc' (rest remaining))))
      false)))

(defn allowed-path?
  "True when path is a safe canonical relative path and no component
  resolved against base is a symlink."
  ([path]
   (allowed-path? (Paths/get "." (make-array String 0)) path))
  ([base path]
   (try
     (let [rel (Paths/get (normalize-relative-path path) (make-array String 0))]
       (not (symlink-component? (base-path base) rel)))
     (catch Exception _ false))))
