(ns evoclj.mount.filesystem-test
  "Unified mount — single provider serves workspace RW and skill RO."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [evoclj.fs.snapshot :as snap]
            [evoclj.mount.backend :as backend]
            [evoclj.mount.filesystem :as fs]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date)))

(defn- temp-dir []
  (Files/createTempDirectory "mount-test" (make-array FileAttribute 0)))

(defn- write-file [dir rel content]
  (let [p (.resolve dir rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes content) (make-array java.nio.file.OpenOption 0))
    p))

(def ^:private p1 (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private now (java.util.Date.))

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
          subject {:phenotype/id p1}
          ws-lease {:cap/id (random-uuid) :subject subject :resource {:kind :filesystem/path :mount/id [:workspace "ws"] :path ""} :actions #{:read :list :stat :write :create :delete} :issued-at now :expires-at (Date. (+ (.getTime now) 100000)) :constraints {}}
          skill-lease {:cap/id (random-uuid) :subject subject :resource {:kind :filesystem/path :mount/id [:skill "demo" (:tree/id snap-res)] :path ""} :actions #{:read :list :stat} :issued-at now :expires-at (Date. (+ (.getTime now) 100000)) :constraints {}}]
      ;; skill read via provider
      (let [content (fs/provider-read provider [:skill "demo" (:tree/id snap-res)] "SKILL.md" {:leases [skill-lease] :subject subject :now now})]
        (is (= "skill" (String. content))))
      ;; workspace read via same provider
      (let [content (fs/provider-read provider [:workspace "ws"] "a.txt" {:leases [ws-lease] :subject subject :now now})]
        (is (= "workspace" (String. content))))
      ;; skill write must fail even with RW lease
      (let [rw-leases (conj [skill-lease] {:cap/id (random-uuid) :subject subject :resource {:kind :filesystem/path :mount/id [:skill "demo" (:tree/id snap-res)] :path ""} :actions #{:write :create} :issued-at now :expires-at (Date. (+ (.getTime now) 100000)) :constraints {}})]
        (is (thrown? clojure.lang.ExceptionInfo (fs/provider-write provider [:skill "demo" (:tree/id snap-res)] "SKILL.md" (.getBytes "x") {:leases rw-leases :subject subject :now now}))))
      ;; workspace write requires both surface and lease
      (is (thrown? clojure.lang.ExceptionInfo (fs/provider-write provider [:workspace "ws"] "new.txt" (.getBytes "x") {:leases [skill-lease] :subject subject :now now})))
      (let [res (fs/provider-create provider [:workspace "ws"] "new.txt" (.getBytes "new") {:leases [ws-lease] :subject subject :now now})]
        (is (some? res)))
      ;; .. cannot escape
      (is (thrown? clojure.lang.ExceptionInfo (fs/provider-read provider [:skill "demo" (:tree/id snap-res)] "../etc/passwd" {:leases [skill-lease] :subject subject :now now})))
      ;; mount A lease cannot access mount B
      (is (thrown? clojure.lang.ExceptionInfo (fs/provider-read provider [:workspace "ws"] "a.txt" {:leases [skill-lease] :subject subject :now now})))
      ;; CAS snapshot independence
      (write-file skill-dir "SKILL.md" "changed")
      (let [content2 (fs/provider-read provider [:skill "demo" (:tree/id snap-res)] "SKILL.md" {:leases [skill-lease] :subject subject :now now})]
        (is (= "skill" (String. content2)) "snapshot independent of upstream")))))

(defn- err-type [thunk]
  "Run thunk, return the :error/type of the typed error it throws, else nil."
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(defn- skill-read-fixture
  "Build a FilesystemProvider serving a real CAS tree skill mount, plus a
  valid read lease and the mount-id. Returns
  {:provider p :mount-id id :lease lease :subject subject}."
  []
  (let [skill-dir (temp-dir)
        cas-dir (temp-dir)
        cas (cas/->cas (.toString cas-dir))
        _ (write-file skill-dir "SKILL.md" "skill")
        snap-res (snap/snapshot-tree! (.toString skill-dir) cas {})
        mount-id [:skill "demo" (:tree/id snap-res)]
        skill-mount (backend/make-skill-mount mount-id cas (:tree/id snap-res))
        reg (backend/create-registry)
        _ (backend/register-mount! reg skill-mount)
        provider (fs/make-provider reg)
        subject {:phenotype/id p1}
        lease {:cap/id (random-uuid) :subject subject :resource {:kind :filesystem/path :mount/id mount-id :path ""} :actions #{:read :list :stat} :issued-at now :expires-at (Date. (+ (.getTime now) 100000)) :constraints {}}]
    {:provider provider :mount-id mount-id :lease lease :subject subject}))

(deftest read-skill-file-facade-deleted-and-provider-read-intact
  (testing "X1: the legacy read-skill-file facade is dead (zero callers) and must be gone"
    (is (not (contains? (ns-publics 'evoclj.mount.filesystem) 'read-skill-file))
        "read-skill-file is a passthrough to provider-read with no callers; deleting it must not change behavior"))
  (testing "equivalence-to-deletion: the delegated production reader still serves a valid skill file"
    (let [{:keys [provider mount-id lease subject]} (skill-read-fixture)]
      (is (= "skill" (String. (fs/provider-read provider mount-id "SKILL.md" {:leases [lease] :subject subject :now now}))))))
  (testing "fault 1: a missing file is rejected with the typed :filesystem/not-found error"
    (let [{:keys [provider mount-id lease subject]} (skill-read-fixture)]
      (is (= :filesystem/not-found
             (err-type #(fs/provider-read provider mount-id "no-such.md" {:leases [lease] :subject subject :now now}))))))
  (testing "fault 2: a path escaping the skill mount is rejected with the typed :filesystem/path-outside-mount error"
    (let [{:keys [provider mount-id lease subject]} (skill-read-fixture)]
      (is (= :filesystem/path-outside-mount
             (err-type #(fs/provider-read provider mount-id "../etc/passwd" {:leases [lease] :subject subject :now now})))))))
