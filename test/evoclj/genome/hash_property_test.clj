(ns evoclj.genome.hash-property-test
  "Property-based invariants of deterministic Genome hashing
  (Task E-prop).

  A test.check layer over the normative hashing rules (Task 1.3,
  Global Constraints 1 and 6):

    1. determinism — the same bytes always hash to the same canonical
       digest, in byte-array or seq form, across repeated calls;
    2. entry-order independence — every permutation of a tree's
       entries yields the same Genome ID (rules 5-7: entries are
       sorted by normalized path, so entry order never participates);
    3. single-byte-change sensitivity — flipping one byte of a payload
       or changing one character of text always changes the digest, so
       different logical content can never collide through
       canonicalization; and at the tree level, changing one entry's
       content changes the Genome ID.

  The example-based golden values live in evoclj.genome.hash-test;
  these suites are the statistical layer over the same invariants."
  (:require [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]))

;; --- generators ------------------------------------------------------------

(def ^:private byte-gen
  "A single random byte (signed, exactly as byte-array and get-bytes
  see it)."
  (gen/choose -128 127))

(def ^:private payload-gen
  "A random byte payload of 0-64 bytes."
  (gen/vector byte-gen 0 64))

(def ^:private non-empty-payload-gen
  "A random byte payload of 1-64 bytes."
  (gen/vector byte-gen 1 64))

(def ^:private text-char-gen
  "Printable characters that are NOT CR/LF: CRLF/CR normalization
  (rule 2) is the identity on these, so a single-character change can
  never be erased by line-ending canonicalization."
  (gen/one-of [gen/char-alphanumeric
               (gen/elements [\space \_ \- \. \: \/])]))

(def ^:private text-gen
  "A 1-40 character string over text-char-gen."
  (gen/fmap (partial apply str) (gen/vector text-char-gen 1 40)))

(defn- unique-paths-gen
  "Generator of a vector of 2-8 distinct, normalization-valid relative
  paths. Every generated path embeds its index (\"gen/<i>/<s>.txt\"),
  so no two paths in one vector can collapse to the same normalized
  form — duplicate normalized paths are rejected by tree-digest."
  []
  (gen/fmap
   (fn [segs]
     (mapv (fn [i s] (str "gen/" i "/" (if (seq s) s "leaf") ".txt"))
           (range) segs))
   (gen/vector gen/string-alphanumeric 2 8)))

(defn- tree-entries-gen
  "Generator of 2-8 entries {:path <distinct valid path> :digest
  <canonical sha256:<64 hex>>}."
  []
  (gen/fmap
   (fn [paths]
     (mapv (fn [p] {:path p :digest (hash/text-digest p)}) paths))
   (unique-paths-gen)))

;; --- determinism -----------------------------------------------------------

(defspec file-digest-deterministic 200
  (prop/for-all [payload payload-gen]
    (let [ba (byte-array payload)
          d (hash/file-digest ba)]
      (and (= d (hash/file-digest (vec payload))) ; seq form hashes identically
           (= d (hash/file-digest ba))            ; repeated call is stable
           (types/genome-id? d)                   ; canonical sha256:<64 hex>
           (= 71 (count d))))))

(defspec text-digest-deterministic 200
  (prop/for-all [s text-gen]
    (let [d (hash/text-digest s)]
      (and (= d (hash/text-digest s))
           (types/genome-id? d)))))

;; --- entry-order independence ---------------------------------------------

(defspec tree-digest-order-independent 200
  (prop/for-all [[entries perm]
                 (gen/let [entries (tree-entries-gen)]
                   (gen/tuple (gen/return entries)
                              (gen/shuffle (vec (range (count entries))))))]
    (= (hash/tree-digest entries)
       (hash/tree-digest (mapv entries perm)))))

;; --- single-byte-change sensitivity ----------------------------------------

(defspec single-byte-flip-changes-file-digest 200
  (prop/for-all [[ba idx]
                 (gen/let [payload non-empty-payload-gen]
                   (gen/tuple (gen/return (byte-array payload))
                              (gen/choose 0 (dec (count payload)))))]
    (let [flipped (byte-array (assoc (vec ba) idx
                                     (unchecked-byte (bit-xor (aget ba idx) 1))))]
      (not= (hash/file-digest ba)
            (hash/file-digest flipped)))))

(def ^:private change-alphabet
  "Characters used to replace a generated character: never a line
  ending and always distinct from the character it replaces."
  "abcdefghijklmnopqrstuvwxyz0123456789")

(defspec single-char-change-changes-text-digest 200
  (prop/for-all [[s idx]
                 (gen/let [s text-gen]
                   (gen/tuple (gen/return s)
                              (gen/choose 0 (dec (count s)))))]
    (let [orig (nth s idx)
          repl (first (remove #(= orig %) (cycle change-alphabet)))
          changed (str (subs s 0 idx) repl (subs s (inc idx)))]
      (not= (hash/text-digest s)
            (hash/text-digest changed)))))

(defspec tree-digest-sensitive-to-entry-content-change 100
  (prop/for-all [[entries idx]
                 (gen/let [entries (tree-entries-gen)]
                   (gen/tuple (gen/return entries)
                              (gen/choose 0 (dec (count entries)))))]
    (let [changed (assoc-in entries [idx :digest]
                            (hash/text-digest (str "changed-" (:path (nth entries idx)))))]
      (not= (hash/tree-digest entries)
            (hash/tree-digest changed)))))
