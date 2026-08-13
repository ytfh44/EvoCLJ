(ns evoclj.store.cas-test
  "Task 5.2 tests for filesystem content-addressed storage.

  Step 1: identical bytes yield identical IDs and exactly one logical
  artifact (the second put is a no-op, never a duplicate). Step 2: puts
  are atomic — bytes are written to a temp file in the artifact
  directory, fsynced, and renamed into place, so a put never leaves a
  partial artifact behind and a stray temp file from an interrupted
  writer can never be read as the artifact. Step 3: with verification
  enabled, a corrupted body is detected by re-hashing on read and fails
  loudly with :store/cas-corrupt carrying expected/actual digests;
  without verification, reads serve stored bytes as-is. Step 4:
  meta.edn can never overwrite or redirect body identity — meta always
  carries the artifact's own id, a forged meta is rejected loudly, and
  putting different bytes never touches another artifact's meta.

  Temp roots live in the system temp directory (java.nio.file
  createTempDirectory) and are deleted after every test, even when a
  test fails."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.store.cas :as cas])
  (:import (java.io FileOutputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)))

;; --- shared fixtures -------------------------------------------------------

(def ^:private roots (atom []))

(defn- temp-root
  "A throwaway CAS root in the system temp dir, registered for cleanup."
  []
  (let [p (Files/createTempDirectory "evoclj-cas-"
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

;; --- helpers ----------------------------------------------------------------

(defn- txt [s]
  (.getBytes s StandardCharsets/UTF_8))

(defn- put!
  "put-bytes! sugar: UTF-8 bytes with an EDN media type by default."
  [root s & [opts]]
  (cas/put-bytes! root (txt s) (merge {:media-type "application/edn"} opts)))

(defn- artifact-dir
  "The artifact directory File for an id, per the normative layout
  <root>/sha256/<ab>/<64 hex>."
  [^Path root id]
  (io/file (.toFile root) "sha256" (subs id 7 9) (subs id 7)))

(defn- body-file
  "The body File for an id."
  [^Path root id]
  (io/file (artifact-dir root id) "body"))

(defn- body-file-count
  "Number of body files anywhere under the root."
  [^Path root]
  (count (filter #(= "body" (.getName %)) (file-seq (.toFile root)))))

(defn- write-bytes!
  "Overwrite a file with raw bytes (used to simulate corruption)."
  [p ba]
  (with-open [w (FileOutputStream. (if (instance? Path p) (.toFile ^Path p) p))]
    (.write w ba)))

(defn- cas-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

;; ============================================================================
;; Step 1 — identical bytes deduplicate to one logical artifact
;; ============================================================================

(deftest same-bytes-twice-same-id-one-artifact
  (let [root (temp-root)
        a (put! root "payload-1")
        b (put! root "payload-1")]
    (testing "identical bytes yield identical IDs and descriptors"
      (is (= (:artifact/id a) (:artifact/id b)))
      (is (= (count "payload-1") (:size a) (:size b)))
      (is (= "application/edn" (:media-type a) (:media-type b))))
    (testing "one logical artifact: a single body plus meta.edn"
      (is (= 1 (body-file-count root)))
      (is (= #{"body" "meta.edn"}
             (set (map #(.getName %)
                       (.listFiles (artifact-dir root (:artifact/id a))))))))))

(deftest put-returns-canonical-descriptor
  (let [root (temp-root)
        r (put! root "descriptor")]
    (is (types/artifact-id? (:artifact/id r)))
    (is (= (count "descriptor") (:size r)))
    (is (= "application/edn" (:media-type r)))))

(deftest different-bytes-different-id
  (let [root (temp-root)
        a (put! root "payload-1")
        b (put! root "payload-2")]
    (is (not= (:artifact/id a) (:artifact/id b)))
    (is (= 2 (body-file-count root)))))

;; ============================================================================
;; Step 2 — atomic write: temp file in the artifact dir, fsync, rename
;; ============================================================================

(deftest put-uses-normative-layout
  (let [root (temp-root)
        {:keys [artifact/id]} (put! root "layout-payload")
        dir (artifact-dir root id)]
    (testing "cas/sha256/<ab>/<64hex>/{body,meta.edn} exist"
      (is (Files/isRegularFile (.toPath (io/file dir "body"))
                              (make-array java.nio.file.LinkOption 0)))
      (is (Files/isRegularFile (.toPath (io/file dir "meta.edn"))
                              (make-array java.nio.file.LinkOption 0))))
    (testing "no temp or partial files remain after a put"
      (is (= #{"body" "meta.edn"}
             (set (map #(.getName %) (.listFiles dir))))))))

(deftest get-bytes-roundtrips-exact-bytes
  (let [root (temp-root)
        bytes (byte-array (map unchecked-byte (range 256)))
        {:keys [artifact/id size]} (cas/put-bytes! root bytes
                                                   {:media-type "application/octet-stream"})]
    (is (= 256 size))
    (is (= (vec bytes) (vec (cas/get-bytes root id))))
    (is (= 256 (alength (cas/get-bytes root id))))))

(deftest repeated-put-is-idempotent
  (let [root (temp-root)
        a (put! root "same")
        _ (put! root "same")
        b (put! root "same")
        id (:artifact/id a)]
    (is (= id (:artifact/id b)))
    (is (= 1 (body-file-count root)))
    (is (= "same" (slurp (body-file root id))))
    (is (= #{"body" "meta.edn"}
           (set (map #(.getName %) (.listFiles (artifact-dir root id))))))))

(deftest stray-temp-file-never-becomes-the-artifact
  (let [root (temp-root)
        bytes (txt "survivor")
        id (hash/file-digest bytes)
        dir (artifact-dir root id)]
    (.mkdirs dir)
    ;; an interrupted earlier writer left a partial temp file behind
    (spit (io/file dir ".evoclj-abandoned.tmp") "partial garbage")
    (let [r (cas/put-bytes! root bytes {:media-type "application/edn"})]
      (is (= id (:artifact/id r)))
      (is (true? (cas/exists? root id)))
      (is (= (vec bytes) (vec (cas/get-bytes root id))))
      (is (= 1 (body-file-count root))))))

;; ============================================================================
;; Step 3 — corrupted bodies are detected on read when verification is on
;; ============================================================================

(deftest verification-detects-corrupted-body
  (let [root (temp-root)
        store (cas/->cas root {:verify true})
        {:keys [artifact/id]} (cas/put-bytes! store (txt "intact-payload")
                                              {:media-type "application/edn"})
        tampered (txt "corrupted-bytes")]
    (write-bytes! (body-file root id) tampered)
    (let [e (cas-error #(cas/get-bytes store id))]
      (is (some? e))
      (is (= :store/cas-corrupt (:error/type (ex-data e))))
      (is (= id (:expected (ex-data e))))
      (is (= (hash/file-digest tampered) (:actual (ex-data e))))
      (is (not= id (:actual (ex-data e)))))))

(deftest verification-on-uncorrupted-reads-fine
  (let [root (temp-root)
        store (cas/->cas root {:verify true})
        {:keys [artifact/id]} (cas/put-bytes! store (txt "intact-payload")
                                              {:media-type "application/edn"})]
    (is (= (vec (txt "intact-payload")) (vec (cas/get-bytes store id))))
    (is (true? (cas/exists? store id)))))

(deftest verification-off-serves-stored-bytes
  (let [root (temp-root)
        {:keys [artifact/id]} (put! root "original")]
    (write-bytes! (body-file root id) (txt "tampered"))
    (testing "reads are cheap and unverified by default"
      (is (= (vec (txt "tampered")) (vec (cas/get-bytes root id)))))
    (testing "exists? never re-hashes"
      (is (true? (cas/exists? root id))))))

;; ============================================================================
;; Step 4 — metadata can never overwrite or redirect body identity
;; ============================================================================

(deftest meta-carries-only-its-own-artifact-id
  (let [root (temp-root)
        id1 (:artifact/id (put! root "payload-A"))
        id2 (:artifact/id (put! root "payload-B"))]
    (testing "each artifact's meta agrees with its own id"
      (is (= id1 (:artifact/id (cas/get-meta root id1))))
      (is (= id2 (:artifact/id (cas/get-meta root id2)))))
    (testing "putting different bytes never rewrites another artifact's meta"
      (is (= id1 (:artifact/id (cas/get-meta root id1))))
      (is (= 2 (body-file-count root))))))

(deftest forged-meta-cannot-redirect-the-body
  (let [root (temp-root)
        id1 (:artifact/id (put! root "payload-A"))
        id2 (:artifact/id (put! root "payload-B"))
        dir (artifact-dir root id1)]
    (spit (io/file dir "meta.edn")
          (pr-str {:artifact/id id2 :size 9 :media-type "application/edn"}))
    (testing "a forged meta is rejected loudly"
      (let [e (cas-error #(cas/get-meta root id1))]
        (is (some? e))
        (is (= :store/cas-meta-mismatch (:error/type (ex-data e))))))
    (testing "and it cannot redirect the body: get-bytes still serves id1's bytes"
      (is (= (vec (txt "payload-A")) (vec (cas/get-bytes root id1)))))
    (testing "a later put of the same bytes fails loudly rather than healing the forgery"
      (let [e (cas-error #(cas/put-bytes! root (txt "payload-A")
                                          {:media-type "application/edn"}))]
        (is (some? e))
        (is (= :store/cas-meta-mismatch (:error/type (ex-data e))))))))

(deftest media-type-update-keeps-id-and-body
  (let [root (temp-root)
        a (put! root "stable")
        b (put! root "stable" {:media-type "text/plain"})]
    (is (= (:artifact/id a) (:artifact/id b)))
    (is (= "text/plain" (:media-type b)))
    (testing "meta is rewritten, body untouched"
      (is (= (:artifact/id a) (:artifact/id (cas/get-meta root (:artifact/id a)))))
      (is (= "text/plain" (:media-type (cas/get-meta root (:artifact/id a)))))
      (is (= (vec (txt "stable")) (vec (cas/get-bytes root (:artifact/id a)))))
      (is (= 1 (body-file-count root))))))

;; ============================================================================
;; Edge cases — id validation and loud failures
;; ============================================================================

(deftest invalid-artifact-id-rejected
  (let [root (temp-root)]
    (doseq [bad ["md5:abc" "sha256:xyz" "sha256:123" "sha256:" "" nil]]
      (let [e (cas-error #(cas/get-bytes root bad))]
        (is (some? e) (pr-str bad))
        (is (= :store/cas-invalid-id (:error/type (ex-data e))) (pr-str bad))))
    (let [e (cas-error #(cas/exists? root "not-an-id"))]
      (is (some? e))
      (is (= :store/cas-invalid-id (:error/type (ex-data e)))))))

(deftest missing-artifact-fails-loudly
  (let [root (str (temp-root))          ; a bare string root, not a Path
        id (str "sha256:" (apply str (repeat 64 "a")))]
    (is (false? (cas/exists? root id)))
    (let [e (cas-error #(cas/get-bytes root id))]
      (is (some? e))
      (is (= :store/cas-missing (:error/type (ex-data e))))
      (is (= id (:artifact/id (ex-data e)))))))

(deftest default-media-type-is-octet-stream
  (let [root (temp-root)
        r (cas/put-bytes! root (txt "x") nil)]
    (is (= "application/octet-stream" (:media-type r)))
    (is (= "application/octet-stream"
           (:media-type (cas/get-meta root (:artifact/id r)))))))
