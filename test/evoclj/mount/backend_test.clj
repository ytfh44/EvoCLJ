(ns evoclj.mount.backend-test
  "WO-B3 — register-mount! is the single canonical registration path and
  mount-id MUST be a canonical vector id (never a bare scalar
  :surface/id). Pins the register-mount! contract end to end through the
  production mount backend."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.helpers :as h]
            [evoclj.mount.backend :as backend])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (Files/createTempDirectory "b3-mount" (make-array FileAttribute 0)))

(defn- host-mount
  [mount-id]
  (backend/make-host-mount mount-id (.toString (temp-dir))))

(defn- thrown-type
  "Run f; return the :error/type of a thrown ExceptionInfo, or ::none."
  [f]
  (try (f) ::none
       (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(deftest register-mount-accepts-canonical-vector-id
  (let [reg (backend/create-registry)
        m (host-mount [:workspace "ws"])]
    (testing "a canonical vector mount-id registers through register-mount!"
      (is (some? (backend/register-mount! reg m)))
      (is (some? (backend/get-mount reg [:workspace "ws"])))
      (is (= [:workspace "ws"] (:mount/id (first (backend/list-mounts reg)))))
      (is (= 1 (count (backend/list-mounts reg)))))))

(deftest register-mount-rejects-non-vector-mount-id
  (let [reg (backend/create-registry)
        m (host-mount [:workspace "ws"])]
    (testing "a scalar (keyword) mount-id is typed-rejected :mount/invalid"
      (is (= :mount/invalid (thrown-type #(backend/register-mount! reg (assoc m :mount/id :workspace/ws)))))
      (is (empty? (backend/list-mounts reg)) "nothing was registered"))
    (testing "a plain string mount-id is typed-rejected :mount/invalid"
      (is (= :mount/invalid (thrown-type #(backend/register-mount! reg (assoc m :mount/id "workspace-ws")))))
      (is (empty? (backend/list-mounts reg))))))

(deftest register-mount-rejects-duplicate-id
  (let [reg (backend/create-registry)
        m1 (host-mount [:workspace "dup"])
        m2 (host-mount [:workspace "dup"])]
    (testing "the first registration wins"
      (is (some? (backend/register-mount! reg m1))))
    (testing "a duplicate mount-id is typed-rejected :mount/collision"
      (is (= :mount/collision (thrown-type #(backend/register-mount! reg m2))))
      (is (= 1 (count (backend/list-mounts reg))) "registry unchanged by the failed duplicate"))))

(deftest register-mount-concurrent-duplicate-yields-single-winner
  (let [reg (backend/create-registry)
        m (host-mount [:workspace "race"])
        results (mapv deref
                      [(future (thrown-type #(backend/register-mount! reg m)))
                       (future (thrown-type #(backend/register-mount! reg m)))])]
    (testing "exactly one writer commits, the other gets :mount/collision"
      (is (= 1 (count (filter #{::none} results))))
      (is (= 1 (count (filter #{:mount/collision} results)))))
    (testing "the registry holds exactly one mount"
      (is (= 1 (count (backend/list-mounts reg)))))))

(deftest mount-id-predicate-rejects-scalars
  (testing "mount-id? is true only for canonical vector ids"
    (is (backend/mount-id? [:workspace "ws"]))
    (is (backend/mount-id? [:skill "debugging" "sha256:aa"]))
    (is (not (backend/mount-id? :workspace/ws)))
    (is (not (backend/mount-id? "workspace-ws")))
    (is (not (backend/mount-id? [:workspace :not-a-string])))
    (is (not (backend/mount-id? [])))))

;; ============================================================================
;; #37 — host-read TOCTOU hardening: NOFOLLOW open + identity re-verification
;; ============================================================================

(defn- write-file!
  [^java.nio.file.Path dir ^String name ^String content]
  (let [p (.resolve dir name)]
    (Files/write p (.getBytes content java.nio.charset.StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))
    p))

(deftest host-read-serves-regular-files
  (testing "#37: the hardened read path still serves ordinary files"
    (let [dir (temp-dir)
          m (backend/make-host-mount [:workspace "read"] (.toString dir))
          b (:backend m)
          body (apply str (repeat 4096 "x"))]
      (write-file! dir "hello.txt" "hello world")
      (write-file! dir "big.txt" body)
      (is (= "hello world"
             (String. (backend/backend-read b "hello.txt")
                      java.nio.charset.StandardCharsets/UTF_8))
          "small file round-trips")
      (is (= (count body)
             (alength ^bytes (backend/backend-read b "big.txt")))
          "multi-KB file is read completely through the channel path")
      (is (thrown? clojure.lang.ExceptionInfo
                   (backend/backend-read b "missing.txt"))
          "missing file still fails closed"))))

(deftest host-read-identity-guard-fails-closed-on-swap
  (testing "#37: a target whose identity changed after the pre-check is rejected, not served"
    (let [dir (temp-dir)
          m (backend/make-host-mount [:workspace "swap"] (.toString dir))
          b (:backend m)
          p (write-file! dir "target.txt" "original")
          attrs (Files/readAttributes
                 p java.nio.file.attribute.BasicFileAttributes
                 (make-array java.nio.file.LinkOption 0))
          before {:file-key (.fileKey attrs)
                  :size (.size attrs)
                  :last-modified (.toMillis (.lastModifiedTime attrs))}]
      ;; the unchanged file verifies clean
      (is (true? (backend/verify-unchanged! p before "target.txt")))
      ;; substitute a DIFFERENT object at the same path (delete + recreate
      ;; so the file-key / size identity cannot match)
      (Files/delete p)
      (write-file! dir "target.txt" "totally different contents")
      (let [t (try (backend/verify-unchanged! p before "target.txt")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? t) "the swapped target is rejected")
        (is (= :filesystem/toctou-identity-mismatch (:error/type (ex-data t)))
            "typed fail-closed error")
        (is (map? (:before (ex-data t))) "the guard reports the expected identity")
        (is (map? (:after (ex-data t))) "the guard reports the observed identity")))))

(deftest host-read-rejects-symlink-leaf
  (testing "#37: a symlink leaf is never followed by the read path"
    (let [dir (temp-dir)
          outside (temp-dir)
          m (backend/make-host-mount [:workspace "link"] (.toString dir))
          b (:backend m)]
      (write-file! outside "secret.txt" "top-secret")
      (if-not (h/try-create-symlink!
               (.resolve outside "secret.txt") (.resolve dir "link.txt"))
        (testing "symlink creation unavailable on this host; skipped" (is true))
        (let [t (try (backend/backend-read b "link.txt")
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
          (is (some? t) "reading a symlink leaf is denied")
          (is (= :filesystem/symlink-rejected (:error/type (ex-data t)))
              "typed symlink rejection"))))))
