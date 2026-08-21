(ns evoclj.store.cas-property-test
  "Property-based invariants of filesystem content-addressed storage
  (component).

  The model mirrored is scripts/verify-semantics/verify1_cas.clj
  section A: an atomic compare-and-set admits at most ONE winner —
  exactly one CURRENT, every other interleaved writer loses CLEANLY
  (verify1: :stale) — and the store converges to a single consistent
  state. For content-addressed storage the CAS is content itself: when
  N writers concurrently put the SAME bytes, the invariant is

    * at least one writer wins, and every winning writer observes the
      SAME :artifact/id — one winner;
    * exactly ONE body file exists — the store converges to a single
      logical artifact and the losing writers left nothing behind (no
      partial or duplicate artifact, no stray temp files);
    * a verification-enabled read serves exactly the canonical bytes.

  Host caveat (Windows/NTFS): put-bytes! writes atomically (temp file
  inside the artifact dir, fsync, Files/move with ATOMIC_MOVE +
  REPLACE_EXISTING). When two writers race the SAME target file,
  MoveFileEx can transiently fail with a sharing violation
  (AccessDeniedException) even though NTFS guarantees the atomicity of
  each single move — the check-then-act gap in put-bytes! (path-exists?
  then write) means two writers may both attempt the move. On POSIX
  rename always replaces, so all writers succeed; on Windows a losing
  writer raises. That is precisely verify1's model: one winner, the
  others fail — so the same-payload suite asserts the STORE invariant
  (converges to one artifact, losers leave no corruption) rather than
  all-succeed, and the losing writers' exceptions are expected, not
  masked corruption. Concurrent DISTINCT payloads have disjoint targets
  and must all succeed (no-cross-talk suite).

  Plus the deterministic-content-addressing invariant (same bytes →
  same id, one logical artifact, exact round-trip). Each suite starts
  all writers from a common latch to force real overlap."
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files Path)
           (java.util.concurrent Callable CountDownLatch Executors Future TimeUnit)))

;; --- temp roots ------------------------------------------------------------

(def ^:private roots (atom []))

(defn- temp-root
  "A throwaway CAS root in the system temp dir, registered for
  cleanup."
  []
  (let [p (Files/createTempDirectory "evoclj-cas-prop-"
                                     (make-array java.nio.file.attribute.FileAttribute 0))]
    (swap! roots conj p)
    p))

(defn- delete-tree!
  "Recursively delete a temp tree (children before parents)."
  [^Path root]
  (when (Files/exists root (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile root)))]
      (Files/deleteIfExists (.toPath f)))))

(use-fixtures :each
  (fn [f]
    (try (f)
         (finally
           (doseq [r @roots] (delete-tree! r))
           (reset! roots [])))))

;; --- helpers ---------------------------------------------------------------

(defn- concurrently
  "Run each thunk in its own thread, releasing all threads from a
  common start latch so writes truly overlap; return the results in
  submission order. Any thunk exception propagates (from the Future)."
  [thunks]
  (let [n (count thunks)
        start (CountDownLatch. 1)
        pool (Executors/newFixedThreadPool n)
        task (fn [f] (reify Callable
                       (call [_] (.await start) (f))))]
    (try
      (let [futs (mapv #(.submit pool (task %)) thunks)]
        (.countDown start)
        (mapv #(.get ^Future % 60 TimeUnit/SECONDS) futs))
      (finally (.shutdownNow pool)))))

(defn- concurrently-capturing
  "Like concurrently, but each thunk's outcome is captured as
  [true result] or [false throwable] — a losing concurrent writer may
  raise on this host (see the namespace docstring), and that is the
  expected failure mode of a loser, not corruption."
  [thunks]
  (let [n (count thunks)
        start (CountDownLatch. 1)
        pool (Executors/newFixedThreadPool n)
        task (fn [f] (reify Callable
                       (call [_] (.await start)
                             (try [true (f)]
                                  (catch Throwable t [false t])))))]
    (try
      (let [futs (mapv #(.submit pool (task %)) thunks)]
        (.countDown start)
        (mapv #(.get ^Future % 60 TimeUnit/SECONDS) futs))
      (finally (.shutdownNow pool)))))

(defn- body-file-count
  "Number of body files anywhere under the root."
  [^Path root]
  (count (filter #(= "body" (.getName ^java.io.File %))
                 (file-seq (.toFile root)))))

(defn- stray-temp-count
  "Number of leftover .evoclj-*.tmp files under the root (a partial
  write that was not cleaned up)."
  [^Path root]
  (count (filter (fn [f]
                   (let [n (.getName ^java.io.File f)]
                     (and (.startsWith n ".evoclj-") (.endsWith n ".tmp"))))
                 (file-seq (.toFile root)))))

;; --- generators ------------------------------------------------------------

(def ^:private byte-gen
  "A single random byte (signed, exactly as byte-array and get-bytes
  see it)."
  (gen/choose -128 127))

(def ^:private payload-gen
  "A random byte payload of 0-256 bytes."
  (gen/vector byte-gen 0 256))

(def ^:private non-empty-payload-gen
  "A random byte payload of 1-128 bytes."
  (gen/vector byte-gen 1 128))

(def ^:private writer-count-gen
  "Number of concurrent writers per interleaving."
  (gen/choose 2 4))

(def ^:private distinct-payloads-gen
  "2-4 pairwise-distinct payloads (distinctness is a precondition of
  the no-cross-talk property)."
  (gen/such-that (fn [ps] (= (count ps) (count (distinct ps))))
                 (gen/vector non-empty-payload-gen 2 4)))

;; --- properties ------------------------------------------------------------

(defspec content-addressing-deterministic 200
  (prop/for-all [payload payload-gen]
    (let [root (temp-root)
          store (cas/->cas root {:verify true})
          ba (byte-array payload)
          a (cas/put-bytes! store ba {:media-type "application/octet-stream"})
          b (cas/put-bytes! store ba {:media-type "application/octet-stream"})
          id (:artifact/id a)]
      (and (types/artifact-id? id)
           (= id (:artifact/id b))
           (= (count payload) (:size a) (:size b))
           (= 1 (body-file-count root))            ; one logical artifact
           (cas/exists? store id)
           (= (vec payload) (vec (cas/get-bytes store id))) ; verify=true round-trip
           (zero? (stray-temp-count root))))))

(defspec concurrent-same-payload-single-winner 200
  (prop/for-all [payload non-empty-payload-gen
                 n-writers writer-count-gen]
    (let [root (temp-root)
          ba (byte-array payload)
          outcomes (concurrently-capturing
                    (repeat n-writers
                            #(cas/put-bytes! root ba
                                              {:media-type "application/octet-stream"})))
          successes (keep (fn [[ok r]] (when ok r)) outcomes)
          ids (mapv :artifact/id successes)
          id (first ids)]
      (and (seq successes)                       ; at least one winner
           (apply = ids)                         ; every winner agrees on THE id
           (types/artifact-id? id)
           (= 1 (body-file-count root))          ; one logical artifact — losers
                                                ;   left nothing behind
           (zero? (stray-temp-count root))
           (= (vec payload)                      ; canonical bytes survive the
              (vec (cas/get-bytes (cas/->cas root {:verify true}) id)))))))

(defspec concurrent-distinct-payloads-no-cross-talk 100
  (prop/for-all [payloads distinct-payloads-gen]
    (let [root (temp-root)
          thunks (mapv (fn [payload]
                         (let [ba (byte-array payload)]
                           #(cas/put-bytes! root ba
                                             {:media-type "application/octet-stream"})))
                       payloads)
          results (concurrently thunks)
          ids (mapv :artifact/id results)
          expected-ids (mapv (fn [p] (hash/file-digest (byte-array p))) payloads)]
      (and (= ids expected-ids)                   ; ids are pure content hashes
           (= (count payloads) (count (distinct ids)))
           (= (count payloads) (body-file-count root))
           (zero? (stray-temp-count root))
           (every? true?
                   (map (fn [p id]
                          (= (vec p)
                             (vec (cas/get-bytes (cas/->cas root {:verify true}) id))))
                        payloads ids))))))
