(ns evoclj.mount.backend-test
  "WO-B3 — register-mount! is the single canonical registration path and
  mount-id MUST be a canonical vector id (never a bare scalar
  :surface/id). Pins the register-mount! contract end to end through the
  production mount backend."
  (:require [clojure.test :refer [deftest is testing]]
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
