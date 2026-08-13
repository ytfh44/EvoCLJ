(ns evoclj.genome.hash
  "Deterministic Genome hashing (Task 1.3).

  Implements the normative canonical hashing rules exactly:

    1. Text assets are hashed as UTF-8 bytes.
    2. CRLF/CR line endings are normalized to LF before hashing.
    3. mtime, inode, owner, and host absolute paths never participate.
    4. Symlinks, absolute paths, and `.`/`..` components are rejected.
    5. Tree entries are sorted by normalized path in bytewise lexical
       order (evoclj.genome.path/bytewise-compare).
    6. Each index line is: path + NUL + digest + LF.
    7. Genome ID = sha256:<hex> of the SHA-256 of the concatenated
       index bytes.

  Global Constraints 1 and 6 follow: identical logical content yields
  identical IDs and different content yields different IDs."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.genome.path :as path]
            [evoclj.genome.types :as types])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def ^:private hex-table
  (into [] (for [i (range 256)] (format "%02x" i))))

(defn- sha256-bytes
  ^bytes [^bytes ba]
  (.digest (MessageDigest/getInstance "SHA-256") ba))

(defn- hex
  ^String [^bytes ba]
  (apply str (map #(nth hex-table (bit-and 0xff %)) ba)))

(defn file-digest
  "Return \"sha256:<64 lowercase hex>\" for the given bytes.

  `bytes` may be a byte array or any seq of bytes. This digests the
  exact bytes supplied; text callers should use `text-digest` so
  CRLF/CR line endings are normalized to LF first (rule 2)."
  [bytes]
  (let [ba (if (bytes? bytes) bytes (byte-array bytes))]
    (str "sha256:" (hex (sha256-bytes ba)))))

(defn normalize-line-endings
  "Normalize CRLF and lone CR to LF (rule 2). Returns a new string."
  [s]
  (str/replace s #"\r\n|\r" "\n"))

(defn text-digest
  "Return \"sha256:<64 lowercase hex>\" of the UTF-8 bytes of `s` with
  CRLF/CR line endings normalized to LF (rules 1 and 2)."
  [s]
  (file-digest (.getBytes (normalize-line-endings s) StandardCharsets/UTF_8)))

(defn- entry->index-line
  "One canonical index line: path + NUL + digest + LF (rule 6)."
  [{:keys [path digest]}]
  (str path "\u0000" digest "\n"))

(defn tree-digest
  "Return the Genome ID \"sha256:<64 lowercase hex>\" for a tree of entries.

  `entries` is a sequence of {:path p :digest d} maps. Each path is
  canonicalized (throws :genome/path-invalid on traversal or absolute
  paths), each digest must be a canonical \"sha256:<64 hex>\" string,
  and duplicate normalized paths are rejected (both throw
  :genome/tree-invalid). Entries are sorted by normalized path in
  bytewise lexical order, the index lines (path + NUL + digest + LF)
  are concatenated, and the ID is the SHA-256 of those bytes
  (rules 5-7)."
  [entries]
  (let [normalized (mapv (fn [{:keys [path digest]}]
                           (let [p (path/normalize-relative-path path)]
                             (when-not (types/genome-id? digest)
                               (throw (err/error :genome/tree-invalid
                                                 "tree entry digest must be sha256:<64 hex>"
                                                 {:path p :digest digest})))
                             {:path p :digest digest}))
                         entries)
        dupes (->> normalized
                   (group-by :path)
                   (filter #(> (count (val %)) 1)))
        _ (when (seq dupes)
            (throw (err/error :genome/tree-invalid
                              "duplicate normalized paths in tree"
                              {:paths (mapv key dupes)})))
        sorted (sort-by :path path/bytewise-compare normalized)
        index (apply str (map entry->index-line sorted))]
    (types/genome-id
     (str "sha256:" (hex (sha256-bytes (.getBytes ^String index StandardCharsets/UTF_8)))))))
