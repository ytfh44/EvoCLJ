(ns evoclj.mount.filesystem-test
  "Unified mount — single provider serves workspace RW and skill RO."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [evoclj.fs.snapshot :as snap]
            [evoclj.mount.backend :as backend]
            [evoclj.mount.filesystem :as fs]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (Files/createTempDirectory "mount-test" (make-array FileAttribute 0)))

(defn- write-file [dir rel content]
  (let [p (.resolve dir rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes content) (make-array java.nio.file.OpenOption 0))
    p))

(deftest same-provider-serves-both
  (testing "single FilesystemProvider serves workspace and skill mount"
    (let [ws-dir (temp-dir)
          skill-dir (temp-dir)
          cas-dir (temp-dir)
          cas (cas/->cas (.toString cas-dir))
          _ (write-file ws-dir "a.txt" "workspace")
          _ (write-file skill-dir "SKILL.md" "skill")
          _ (write-file skill-dir "references/foo.md" "ref")
          snap-res (snap/snapshot-tree! (.toString skill-dir) cas {})
          ws-mount (backend/make-host-mount [:workspace "ws"] (.toString ws-dir))
          skill-mount (backend/make-skill-mount [:skill "demo" (:tree/id snap-res)] cas (:tree/id snap-res))
          reg (backend/create-registry)
          _ (backend/register-mount! reg ws-mount)
          _ (backend/register-mount! reg skill-mount)
          provider (fs/make-provider reg)
          ws-lease {:cap/id (random-uuid) :subject {:phenotype/id "p1"} :resource {:kind :filesystem/path :mount/id [:workspace "ws"] :path ""} :actions #{:read :list :stat :write :create :delete} :issued-at (java.util.Date.) :expires-at (java.util.Date. (+ (System/currentTimeMillis) 100000)) :constraints {}}
          skill-lease {:cap/id (random-uuid) :subject {:phenotype/id "p1"} :resource {:kind :filesystem/path :mount/id [:skill "demo" (:tree/id snap-res)] :path ""} :actions #{:read :list :stat} :issued-at (java.util.Date.) :expires-at (java.util.Date. (+ (System/currentTimeMillis) 100000)) :constraints {}}]
      ;; skill read via provider
      (let [content (fs/provider-read provider [:skill "demo" (:tree/id snap-res)] "SKILL.md" {:leases [skill-lease]})]
        (is (= "skill" (String. content))))
      ;; workspace read via same provider
      (let [content (fs/provider-read provider [:workspace "ws"] "a.txt" {:leases [ws-lease]})]
        (is (= "workspace" (String. content))))
      ;; skill write must fail even with RW lease
      (let [rw-leases (conj [skill-lease] {:cap/id (random-uuid) :subject {:phenotype/id "p1"} :resource {:kind :filesystem/path :mount/id [:skill "demo" (:tree/id snap-res)] :path ""} :actions #{:write :create} :issued-at (java.util.Date.) :expires-at (java.util.Date. (+ (System/currentTimeMillis) 100000)) :constraints {}})]
        (is (thrown? clojure.lang.ExceptionInfo (fs/provider-write provider [:skill "demo" (:tree/id snap-res)] "SKILL.md" (.getBytes "x") {:leases rw-leases}))))
      ;; workspace write requires both surface and lease
      (is (thrown? clojure.lang.ExceptionInfo (fs/provider-write provider [:workspace "ws"] "new.txt" (.getBytes "x") {:leases [skill-lease]})))
      (let [res (fs/provider-create provider [:workspace "ws"] "new.txt" (.getBytes "new") {:leases [ws-lease]})]
        (is (some? res)))
      ;; .. cannot escape
      (is (thrown? clojure.lang.ExceptionInfo (fs/provider-read provider [:skill "demo" (:tree/id snap-res)] "../etc/passwd" {:leases [skill-lease]})))
      ;; mount A lease cannot access mount B
      (is (thrown? clojure.lang.ExceptionInfo (fs/provider-read provider [:workspace "ws"] "a.txt" {:leases [skill-lease]})))
      ;; CAS snapshot independence
      (write-file skill-dir "SKILL.md" "changed")
      (let [content2 (fs/provider-read provider [:skill "demo" (:tree/id snap-res)] "SKILL.md" {:leases [skill-lease]})]
        (is (= "skill" (String. content2)) "snapshot independent of upstream")))))
