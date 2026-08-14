(ns verify6-content-addressing
  "Semantic verification #6 — content addressing determinism (GC 1, 6).
  Model: ID = sha256(sorted index of (path, digest) lines), digest =
  sha256(LF-normalized UTF-8 bytes). Properties: (a) same logical tree
  => same ID; (b) CRLF/CR normalization => same ID for line-ending
  variants; (c) any byte change => different ID; (d) entry order does
  not matter. Real code: evoclj.genome.load/load-genome,
  evoclj.genome.hash/tree-digest."
  (:require [evoclj.genome.load :as load]
            [evoclj.genome.hash :as hash]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(defn copy-tree! [src dst]
  (doseq [f (file-seq (.toFile (java.nio.file.Paths/get src (make-array String 0))))]
    (let [rel (.relativize (java.nio.file.Paths/get src (make-array String 0))
                           (.toPath f))
          target (.resolve (java.nio.file.Paths/get dst (make-array String 0)) rel)]
      (when (.isDirectory f)
        (java.nio.file.Files/createDirectories
         target (make-array java.nio.file.attribute.FileAttribute 0)))
      (when (.isFile f)
        (java.nio.file.Files/copy (.toPath f) target
                                  (make-array java.nio.file.CopyOption 0))))))

(defn- tmpdir [prefix]
  (str (java.nio.file.Files/createTempDirectory
        prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- deltree! [root]
  (when (java.nio.file.Files/exists root (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile root)))]
      (java.nio.file.Files/deleteIfExists (.toPath f)))))

(def seed (str (java.nio.file.Paths/get "genomes/seed" (make-array String 0))))

(let [g1 (load/load-genome seed)
      g2 (load/load-genome seed)
      t1 (tmpdir "verify-ca-crlf-")
      t2 (tmpdir "verify-ca-mut-")
      t3 (tmpdir "verify-ca-lf-")]
  (try
    (check! "(a) same tree loaded twice => same Genome ID"
            (= (:genome/id g1) (:genome/id g2))
            (:genome/id g1))
    ;; (b) CRLF variant: copy seed, convert all text bytes CRLF -> LF is
    ;; the canonical; here we go the other way: rewrite a text file with
    ;; CRLF endings and verify the ID does NOT change (normalization).
    (copy-tree! seed t1)
    (copy-tree! seed t3)
    (let [route (java.nio.file.Paths/get t3 (into-array String ["programs" "route.clj"]))
          bytes (java.nio.file.Files/readAllBytes route)
          crlf (byte-array
                (mapcat (fn [b] (if (= b (byte 10)) [(byte 13) (byte 10)] [b]))
                        bytes))]
      (java.nio.file.Files/write route crlf
                                 (make-array java.nio.file.OpenOption 0)))
    (check! "(b) CRLF line endings normalize to the same Genome ID"
            (= (:genome/id g1) (:genome/id (load/load-genome t3)))
            "line-ending variants are the same logical content")
    ;; (c) a content change that stays VALID EDN changes the ID
    (copy-tree! seed t2)
    (let [top (java.nio.file.Paths/get t2 (into-array String ["topology.edn"]))
          b (java.nio.file.Files/readAllBytes top)
          txt (String. b java.nio.charset.StandardCharsets/UTF_8)
          mutated (.getBytes (clojure.string/replace txt "max-steps 64" "max-steps 65")
                             java.nio.charset.StandardCharsets/UTF_8)]
      (java.nio.file.Files/write top mutated (make-array java.nio.file.OpenOption 0)))
    (check! "(c) any byte change => different Genome ID"
            (not= (:genome/id g1) (:genome/id (load/load-genome t2)))
            "single-byte mutation is a different content address")
    ;; (d) tree-digest order independence
    (let [entries (map (fn [[p {:keys [digest]}]] {:path p :digest digest})
                       (sort-by key (:files g1)))
          shuffled (shuffle entries)]
      (check! "(d) tree-digest ignores entry order"
              (= (hash/tree-digest entries) (hash/tree-digest shuffled))
              "sorted index is canonical"))
    (check! "(a') genome/id equals tree-digest of its own entries"
            (= (:genome/id g1)
               (hash/tree-digest (map (fn [[p {:keys [digest]}]] {:path p :digest digest})
                                      (sort-by key (:files g1)))))
            "ID is exactly the canonical tree digest")
    (finally
      (deltree! (java.nio.file.Paths/get t1 (make-array String 0)))
      (deltree! (java.nio.file.Paths/get t2 (make-array String 0)))
      (deltree! (java.nio.file.Paths/get t3 (make-array String 0))))))
(println "VERIFY6 DONE")
