(ns evoclj.genome.hash-test
  "Tests for deterministic Genome hashing (Task 1.3).

  Genome identity must depend only on canonical logical content
  (Global Constraints 1 and 6): text is hashed as UTF-8 bytes with
  CRLF/CR normalized to LF, tree entries are sorted by normalized path
  in bytewise lexical order, each index line is path + NUL + digest +
  LF, and the Genome ID is sha256:<hex> of the concatenated index
  bytes. mtime, inode, owner, and absolute paths never participate.

  The golden values below were computed independently per the
  normative rules (SHA-256 over the exact byte sequences); they are
  hard-coded so any accidental canonicalization change breaks a test."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types])
  (:import (java.nio.charset StandardCharsets)))

(def ^:private golden-text-digest
  ;; sha256 of the UTF-8 bytes "a\nb"
  "sha256:7e18f737311b2dc3b2f269dd78396b0351f14fb66efa879f768cb23181883c78")

(def ^:private golden-manifest-digest
  ;; sha256 of the UTF-8 bytes "{:genome/format 1}\n" (LF-normalized)
  "sha256:56bd2602d32e2bfb8e7b1ae01a15db877414651b25b78b5d19170b4cfc161116")

(def ^:private golden-route-digest
  ;; sha256 of the UTF-8 bytes "{:priority 3}\n"
  "sha256:2d71c8a5ae0d390b3773f1d9743707a5353f433cf55b07edf74c4703d043f650")

(def ^:private golden-genome-id
  ;; sha256 of the concatenated index bytes:
  ;;   "manifest.edn" \0 <manifest-digest> \n
  ;;   "skills/route.edn" \0 <route-digest> \n
  ;; where <manifest-digest> and <route-digest> are the FULL
  ;; "sha256:<hex>" digest strings produced by file-digest/text-digest
  ;; (rule 6: path + NUL + digest + LF).
  "sha256:498cb97a0ec462ac07794102157143ed18c9504f5ce14bda44a3674fadb037b6")

(def ^:private sha256-of-empty
  "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(deftest golden-file-digest
  (testing "file-digest hashes exact bytes; text-digest normalizes line endings first"
    (is (= golden-text-digest
           (hash/file-digest (.getBytes "a\nb" StandardCharsets/UTF_8))))
    (is (= golden-text-digest (hash/text-digest "a\nb")))
    (is (= golden-text-digest (hash/text-digest "a\r\nb")))
    (is (= golden-text-digest (hash/text-digest "a\rb")))))

(deftest golden-two-file-tree
  ;; A synthetic two-file tree whose expected Genome ID is hard-coded.
  ;; The manifest file is written with CRLF on purpose: if line-ending
  ;; normalization (rule 2) regressed, the per-file digest would differ
  ;; from golden-manifest-digest and the Genome ID would change.
  (let [manifest-content "{:genome/format 1}\r\n"
        route-content    "{:priority 3}\n"
        entries [{:path "manifest.edn" :digest (hash/text-digest manifest-content)}
                 {:path "skills/route.edn" :digest (hash/text-digest route-content)}]]
    (testing "per-file digests are canonical"
      (is (= golden-manifest-digest (hash/text-digest manifest-content)))
      (is (= golden-route-digest (hash/text-digest route-content))))
    (testing "tree-digest produces the hard-coded Genome ID"
      (is (= golden-genome-id (hash/tree-digest entries))))
    (testing "entry order must not matter"
      (is (= golden-genome-id (hash/tree-digest (reverse entries)))))))

(deftest file-digest-format
  (let [d (hash/file-digest (.getBytes "hello" StandardCharsets/UTF_8))]
    (is (types/genome-id? d))
    (is (= 71 (count d))))
  (testing "accepts byte sequences as well as byte arrays"
    (is (= (hash/file-digest (byte-array [(byte 97) (byte 98)]))
           (hash/text-digest "ab")))))

(deftest line-ending-normalization
  (testing "CRLF, lone CR, and LF-only spellings hash identically"
    (is (= (hash/text-digest "x\ny") (hash/text-digest "x\r\ny")))
    (is (= (hash/text-digest "x\ny") (hash/text-digest "x\ry")))
    (is (= (hash/text-digest "x\n\ny") (hash/text-digest "x\r\n\r\ny"))))
  (testing "normalization is exact"
    (is (= "x\ny" (hash/normalize-line-endings "x\r\ny")))
    (is (= "x\n\ny" (hash/normalize-line-endings "x\r\n\r\ny")))
    (is (= "x\ny" (hash/normalize-line-endings "x\ry")))))

(defn- seeded-shuffle
  "Deterministic permutation of coll driven by a java.util.Random.
  (clojure.core/shuffle has no Random-taking arity.)"
  [coll rng]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rng)
    (vec al)))

(deftest shuffled-entry-order-is-stable
  (let [paths ["manifest.edn" "skills/route.edn" "models.edn" "a/b/c.txt"
               "topology.edn" "memory.edn" "evolution.edn" "programs/route.clj"]
        entries (mapv (fn [p] {:path p :digest (hash/text-digest p)}) paths)
        base (hash/tree-digest entries)
        rng (java.util.Random. 20240131)]
    (testing "any permutation of the entries yields the same tree digest"
      (dotimes [i 100]
        (is (= base (hash/tree-digest (seeded-shuffle entries rng)))
            (str "permutation " i " changed the tree digest"))))))

(deftest one-changed-byte-changes-digest
  (testing "a single changed byte changes the file digest"
    (is (not= (hash/text-digest "alpha beta gamma")
              (hash/text-digest "alpha beta gamme"))))
  (testing "a changed entry digest changes the whole tree digest"
    (let [entries [{:path "a.txt" :digest (hash/text-digest "same content")}
                   {:path "b.txt" :digest (hash/text-digest "same content")}]
          id1 (hash/tree-digest entries)
          id2 (hash/tree-digest (assoc-in entries [1 :digest]
                                          (hash/text-digest "same contenf")))]
      (is (not= id1 id2))))
  (testing "a changed path changes the tree digest"
    (is (not= (hash/tree-digest [{:path "a.txt" :digest (hash/text-digest "x")}])
              (hash/tree-digest [{:path "a2.txt" :digest (hash/text-digest "x")}]))))
  (testing "changing only line endings does NOT change the digest"
    (is (= (hash/text-digest "x\r\ny") (hash/text-digest "x\ny")))))

(defn- tree-error
  "The ExceptionInfo thrown by tree-digest, or nil."
  [entries]
  (try (hash/tree-digest entries)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(deftest duplicate-normalized-path-rejected
  (testing "a\\b.txt and a/b.txt collapse to the same normalized path"
    (let [e (tree-error [{:path "a\\b.txt" :digest (hash/text-digest "1")}
                         {:path "a/b.txt" :digest (hash/text-digest "2")}])]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :genome/tree-invalid (:error/type (ex-data e))))
      (is (= ["a/b.txt"] (:paths (ex-data e)))))))

(deftest traversal-path-in-tree-rejected
  (let [e (tree-error [{:path "../evil.txt" :digest (hash/text-digest "x")}])]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :genome/path-invalid (:error/type (ex-data e))))))

(deftest invalid-digest-rejected
  (let [e (tree-error [{:path "a.txt" :digest "md5:abc"}])]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :genome/tree-invalid (:error/type (ex-data e))))))

(deftest tree-digest-returns-canonical-genome-id
  (let [id (hash/tree-digest [{:path "a.txt" :digest (hash/text-digest "x")}])]
    (is (types/genome-id? id))
    (is (= 71 (count id)))))

(deftest empty-tree-digest
  (testing "the empty tree hashes the empty index"
    (is (= sha256-of-empty (hash/tree-digest [])))))

(deftest non-ascii-paths-hash-as-utf8
  (let [entries [{:path "技能/提示词.edn" :digest (hash/text-digest "x")}
                 {:path "模型/嵌入.edn" :digest (hash/text-digest "y")}]
        id (hash/tree-digest entries)]
    (is (types/genome-id? id))
    (testing "order invariance holds for non-ASCII paths too"
      (is (= id (hash/tree-digest (reverse entries)))))))

(deftest digest-deterministic-across-calls
  (let [entries [{:path "manifest.edn" :digest (hash/text-digest "{:genome/format 1}\n")}
                 {:path "skills/route.edn" :digest (hash/text-digest "{:priority 3}\n")}]]
    (is (apply = (repeatedly 100 #(hash/tree-digest entries))))))
