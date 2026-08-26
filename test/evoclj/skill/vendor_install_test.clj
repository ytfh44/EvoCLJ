(ns evoclj.skill.vendor-install-test
  "S6: atomic vendor install (tmp + verify + rename) and orphan cleanup.

  Drive the production vendor-skill! path end to end (INV-09) and assert the
  observable contract:
    - a successful install leaves a FULLY PRESENT dest and NO staging dir;
    - a failed/failed-verify install leaves dest ABSENT (or the prior fully
      present dest untouched) and NO staging dir — never a torn/mid-copy dest;
    - leftover orphan staging dirs from a crash are cleaned at install start
      (fail-closed);
    - staged content is re-verified (content/limits/hash) before rename, and a
      bad install is rejected typed with no dest.

  The staging-directory NAME convention (prefix \".vendor-staging-\") is a
  documented production convention (evoclj.skill.vendor); this suite uses the
  same literal so it can fabricate a realistic crash-orphan and detect stray
  staging dirs behaviorally."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [evoclj.skill.vendor :as vendor]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.store.cas :as cas]
            [evoclj.genome.load :as load])
  (:import (java.nio.file Files Path Paths LinkOption OpenOption)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)))

;; The production staging-dir prefix convention.
(def ^:private staging-prefix ".vendor-staging-")

(defn- temp-dir!
  ^Path []
  (Files/createTempDirectory "evoclj-vendor-install-test" (make-array FileAttribute 0)))

(defn- delete-recursively!
  [^Path dir]
  (when (Files/exists dir (make-array LinkOption 0))
    (let [f (.toFile dir)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)]
          (delete-recursively! (.toPath c))))
      (Files/deleteIfExists dir))
    nil))

(defn- write-text!
  [^Path dir rel ^String content]
  (let [p (.resolve dir rel)]
    (let [parent (.getParent p)]
      (when parent (Files/createDirectories parent (make-array FileAttribute 0))))
    (Files/write p (.getBytes content StandardCharsets/UTF_8) (make-array OpenOption 0))
    p))

(defn- nofollow []
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- exists?
  [^Path p]
  (Files/exists p (nofollow)))

(def ^:private manifest-source
  (pr-str {:genome/format 1
           :agent/id :main
           :agent/entry :graph/main
           :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
           :modules {:topology "topology.edn"
                     :models "models.edn"
                     :memory "memory.edn"
                     :evolution "evolution.edn"}
           :capabilities/requested #{:model/call}
           :evolution {:max-risk :behavioral
                       :mutable #{:parameters :prompts :skills :programs}}
           :metadata {:name "vendor-install-fixture"}}))

(defn- write-minimal-genome!
  [^Path dir]
  (write-text! dir "manifest.edn" manifest-source)
  (write-text! dir "topology.edn" "{:graph/id :graph/main}\n")
  (write-text! dir "models.edn" "{:models {}}\n")
  (write-text! dir "memory.edn" "{:memory {}}\n")
  (write-text! dir "evolution.edn" "{:evolution {}}\n")
  (write-text! dir "programs/route.clj" "(ns route)\n(defn run [x] x)\n")
  dir)

(defn- write-skill!
  [^Path skill-dir ^String body & {:keys [extra-files]}]
  (Files/createDirectories skill-dir (make-array FileAttribute 0))
  (write-text! skill-dir "SKILL.md" (str "---\nname: my-skill\ndescription: test skill\n---\n" body "\n"))
  (doseq [[rel content] extra-files]
    (write-text! skill-dir rel content))
  skill-dir)

(def ^:private snapshot-limits
  {:max-depth 20 :max-files 2000 :max-total-bytes (* 20 1024 1024) :max-file-bytes (* 5 1024 1024)})

(defn- put-tree!
  "Store an arbitrary tree-manifest map into CAS and return its tree id."
  [cas manifest]
  (let [mb (.getBytes (pr-str manifest) StandardCharsets/UTF_8)
        {:keys [artifact/id]} (cas/put-bytes! cas mb {:media-type "application/edn"})]
    id))

(defn- thrown-vendor
  "The ExceptionInfo thrown by vendor-skill!, or nil when it succeeds."
  [opts]
  (try (vendor/vendor-skill! opts) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- child-names
  "Immediate child names of a directory (NOFOLLOW listing)."
  [^Path dir]
  (when (exists? dir)
    (let [s (Files/newDirectoryStream dir)]
      (try
        (mapv str (iterator-seq (.iterator s)))
        (finally (.close s))))))

(defn- staging-leftover?
  "True when a documented staging dir (prefix in the vendor convention)
  is present directly under the given directory."
  [^Path dir]
  (boolean (some #(str/starts-with? % staging-prefix) (child-names dir))))

;; ---------------------------------------------------------------------------
;; 0. startup — public cleanup-orphan-staging! sweeps an orphan directly
;; ---------------------------------------------------------------------------

(deftest cleanup-orphan-staging-direct
  (testing "the public cleanup-orphan-staging! startup path sweeps an orphan directly"
    (let [genome-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [skills-dir (.resolve genome-dir "skills")
              orphan (.resolve skills-dir (str staging-prefix "dead-crash-direct"))
              _ (Files/createDirectories orphan (make-array FileAttribute 0))
              _ (write-text! orphan "SKILL.md" "partial")]
          (is (exists? orphan))
          (vendor/cleanup-orphan-staging! (str genome-dir))
          (is (not (exists? orphan)) "direct startup sweep removed the orphan")
          (is (not (staging-leftover? skills-dir))))
        (finally (delete-recursively! genome-dir))))))

;; ---------------------------------------------------------------------------
;; 1. happy — atomic install: dest fully present, staging gone
;; ---------------------------------------------------------------------------

(deftest install-is-atomic-no-leftover-staging
  (testing "a successful vendor leaves a fully-present dest and no staging dir"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext-skill (.resolve ext-root "my-skill")
              _ (write-skill! ext-skill "Body A" :extra-files {"references/guide.md" "guide A"})
              snap (snapshot/snapshot-tree! ext-skill cas snapshot-limits)
              tree-id (:tree/id snap)
              vres (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              skills-dir (.resolve genome-dir "skills")
              dest (.resolve skills-dir "my-skill")]
          (is (= "my-skill" (:skill/name vres)))
          (is (= tree-id (:tree/id vres)))
          (is (Files/isDirectory dest (make-array LinkOption 0)) "dest is a directory")
          (is (Files/exists (.resolve dest "SKILL.md") (make-array LinkOption 0)))
          (is (Files/exists (.resolve dest "references/guide.md") (make-array LinkOption 0)))
          (is (not (staging-leftover? skills-dir)) "staging dir renamed away, none left")
          ;; genome identity participates
          (let [reloaded (load/load-genome genome-dir)]
            (is (contains? (:files reloaded) "skills/my-skill/SKILL.md"))
            (is (contains? (:files reloaded) "skills/my-skill/references/guide.md"))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 2. branch — re-install replaces the whole dest atomically (never merges)
;; ---------------------------------------------------------------------------

(deftest reinstall-replaces-atomically-no-merge
  (testing "re-vendor replaces the whole dest atomically — old files gone, new present"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext-one (.resolve ext-root "one")
              ext-two (.resolve ext-root "two")
              _ (write-skill! ext-one "Body ONE" :extra-files {"old-marker.txt" "o"})
              treeOne (:tree/id (snapshot/snapshot-tree! ext-one cas snapshot-limits))
              _ (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeOne})
              skills-dir (.resolve genome-dir "skills")
              dest (.resolve skills-dir "my-skill")]
          (is (Files/exists (.resolve dest "old-marker.txt") (make-array LinkOption 0)))
          ;; a genuinely different second tree built in a CLEAN dir
          (write-skill! ext-two "Body TWO" :extra-files {"new.txt" "n"})
          (let [treeTwo (:tree/id (snapshot/snapshot-tree! ext-two cas snapshot-limits))]
            (is (not= treeOne treeTwo))
            (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeTwo})
            (is (not (Files/exists (.resolve dest "old-marker.txt") (make-array LinkOption 0))) "old file gone after replace")
            (is (Files/exists (.resolve dest "new.txt") (make-array LinkOption 0)))
            (is (Files/exists (.resolve dest "SKILL.md") (make-array LinkOption 0)))
            (is (not (staging-leftover? skills-dir)))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 3. branch+fault — orphan staging dir cleaned at install start
;; ---------------------------------------------------------------------------

(deftest orphan-staging-cleaned-on-install
  (testing "a leftover staging dir from a crashed install is cleaned at the start of the next install"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              skills-dir (.resolve genome-dir "skills")
              orphan (.resolve skills-dir (str staging-prefix "dead-crash-1"))
              _ (Files/createDirectories orphan (make-array FileAttribute 0))
              _ (write-text! orphan "SKILL.md" "partial")
              ext (.resolve ext-root "my-skill")
              _ (write-skill! ext "Body good")
              tree-id (:tree/id (snapshot/snapshot-tree! ext cas snapshot-limits))]
          (is (exists? orphan) "orphan created for the test")
          (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
          (is (not (exists? orphan)) "orphan staging removed")
          (is (not (staging-leftover? skills-dir))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

(deftest orphan-cleaned-even-when-install-fails
  (testing "orphan sweep runs at install START, so it cleans even when the install itself fails"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              skills-dir (.resolve genome-dir "skills")
              orphan (.resolve skills-dir (str staging-prefix "dead-crash-2"))
              _ (Files/createDirectories orphan (make-array FileAttribute 0))
              _ (write-text! orphan "SKILL.md" "partial")
              body "x\n"
              skill-md (str "---\nname: my-skill\ndescription: test skill\n---\n" body)
              md-id (:artifact/id (cas/put-bytes! cas (.getBytes skill-md StandardCharsets/UTF_8)
                                                   {:media-type "application/octet-stream"}))
              body-id (:artifact/id (cas/put-bytes! cas (.getBytes body StandardCharsets/UTF_8)
                                                   {:media-type "application/octet-stream"}))
              manifest {:tree/version 1
                        :entries {"SKILL.md" {:artifact/id md-id :size (count skill-md)}
                                  "a.md" {:artifact/id body-id :size (count body)}
                                  "missing.md" {:artifact/id (str "sha256:" (apply str (repeat 64 "b")))
                                                :size 1}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (contains? #{:skill/vendor-missing-artifact :store/cas-missing}
                         (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve skills-dir "my-skill") (make-array LinkOption 0))) "no torn dest")
          (is (not (exists? orphan)) "orphan cleaned even on failed install"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 4. fault — failure mid-stage leaves dest ABSENT (fresh) and staging cleaned
;; ---------------------------------------------------------------------------

(deftest failure-mid-stage-leaves-dest-absent-not-torn
  (testing "a missing artifact partway through staging leaves dest absent and staging cleaned"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              body "x\n"
              skill-md (str "---\nname: my-skill\ndescription: test skill\n---\n" body)
              md-id (:artifact/id (cas/put-bytes! cas (.getBytes skill-md StandardCharsets/UTF_8)
                                                   {:media-type "application/octet-stream"}))
              body-id (:artifact/id (cas/put-bytes! cas (.getBytes body StandardCharsets/UTF_8)
                                                   {:media-type "application/octet-stream"}))
              manifest {:tree/version 1
                        :entries {"SKILL.md" {:artifact/id md-id :size (count skill-md)}
                                  "sub/dir/a.md" {:artifact/id body-id :size (count body)}
                                  "missing.md" {:artifact/id (str "sha256:" (apply str (repeat 64 "e")))
                                                :size 1}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              skills-dir (.resolve genome-dir "skills")
              dest (.resolve skills-dir "my-skill")]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (contains? #{:skill/vendor-missing-artifact :store/cas-missing}
                         (:error/type (ex-data e))))
          ;; dest is ABSENT (never torn/partially written)
          (is (not (Files/exists dest (make-array LinkOption 0))) "dest absent, not a torn partial copy")
          (is (not (staging-leftover? skills-dir)) "staging cleaned"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 5. fault — a failed re-install leaves the prior fully-present dest untouched
;; ---------------------------------------------------------------------------

(deftest reinstall-failure-leaves-prior-dest-fully-present
  (testing "a failed re-install leaves the prior fully-present dest untouched (not torn)"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "my-skill")
              _ (write-skill! ext "Body PRIOR")
              tree-id (:tree/id (snapshot/snapshot-tree! ext cas snapshot-limits))
              _ (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              dest (.resolve (.resolve genome-dir "skills") "my-skill")
              prior-content (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8)
              body "x\n"
              skill-md (str "---\nname: my-skill\ndescription: test skill\n---\n" body)
              md-id (:artifact/id (cas/put-bytes! cas (.getBytes skill-md StandardCharsets/UTF_8)
                                                   {:media-type "application/octet-stream"}))
              body-id (:artifact/id (cas/put-bytes! cas (.getBytes body StandardCharsets/UTF_8)
                                                   {:media-type "application/octet-stream"}))
              manifest {:tree/version 1
                        :entries {"SKILL.md" {:artifact/id md-id :size (count skill-md)}
                                  "a.md" {:artifact/id body-id :size (count body)}
                                  "missing.md" {:artifact/id (str "sha256:" (apply str (repeat 64 "d")))
                                                :size 1}}}
              bad-tree (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id bad-tree})]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (= :skill/vendor-missing-artifact (:error/type (ex-data e))))
          ;; prior good dest is fully untouched
          (is (Files/exists (.resolve dest "SKILL.md") (make-array LinkOption 0)))
          (is (= prior-content (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8))
              "prior content intact")
          (is (not (Files/exists (.resolve dest "a.md") (make-array LinkOption 0))) "no partially staged file leaked into dest")
          (is (not (staging-leftover? (.resolve genome-dir "skills")))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 6. fault — verify rejects a bad install typed, with no dest
;; ---------------------------------------------------------------------------

(deftest verify-rejects-corrupt-artifact-typed-no-dest
  (testing "verify rechecks staged content; a corrupt CAS artifact is rejected typed, no dest"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              fake-id (str "sha256:" (apply str (repeat 64 "c")))
              ;; Content is a VALID skill (so the WO-S7 strict gate passes and
              ;; the failure is attributable to copy-integrity, not formatting).
              ;; Its digest still != fake-id, so verify-staged (WO-S6) must
              ;; reject the id mismatch before the rename.
              body "---\nname: my-skill\ndescription: test skill\n---\nCORRUPT-BODY\n"
              ;; Fabricate a mismatched CAS body: stash bytes whose digest
              ;; != fake-id under fake-id's body path, so get-bytes (verify
              ;; off) returns bytes that do NOT match their claimed id.
              body-path (cas/body-path cas fake-id)
              _ (Files/createDirectories (.getParent body-path) (make-array FileAttribute 0))
              _ (Files/write body-path (.getBytes body StandardCharsets/UTF_8) (make-array OpenOption 0))
              manifest {:tree/version 1
                        :entries {"SKILL.md" {:artifact/id fake-id :size (count body)}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              skills-dir (.resolve genome-dir "skills")]
          (is (instance? clojure.lang.ExceptionInfo e) "verify must reject the bad install")
          (is (= :skill/vendor-verify-failed (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve skills-dir "my-skill") (make-array LinkOption 0))) "no dest on verify failure")
          (is (not (staging-leftover? skills-dir)) "staging cleaned"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 7. concurrency — two concurrent installs into the same skill serialize:
;;    a single complete dest wins, no torn/mixed content, no leftover staging
;; ---------------------------------------------------------------------------

(deftest concurrent-installs-serialize-no-torn
  (testing "concurrent installs into the same skill yield ONE complete dest, no leftover staging"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "s")
              _ (write-skill! ext "Body A" :extra-files {"side.txt" "A"})
              treeA (:tree/id (snapshot/snapshot-tree! ext cas snapshot-limits))
              _ (write-skill! ext "Body B" :extra-files {"side.txt" "B"})
              treeB (:tree/id (snapshot/snapshot-tree! ext cas snapshot-limits))
              f1 (future (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeA}))
              f2 (future (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeB}))
              r1 @f1
              r2 @f2
              dest (.resolve (.resolve genome-dir "skills") "my-skill")
              content (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8)
              side (String. (Files/readAllBytes (.resolve dest "side.txt")) StandardCharsets/UTF_8)]
          (is (some? r1) "install A succeeded")
          (is (some? r2) "install B succeeded")
          (is (Files/exists (.resolve dest "SKILL.md") (make-array LinkOption 0)))
          (is (Files/exists (.resolve dest "side.txt") (make-array LinkOption 0)))
          (is (or (and (str/includes? content "Body A") (not (str/includes? content "Body B")))
                  (and (str/includes? content "Body B") (not (str/includes? content "Body A"))))
              "SKILL.md bytes are exactly one complete tree, not torn/mixed")
          (is (or (and (str/includes? content "Body A") (= side "A"))
                  (and (str/includes? content "Body B") (= side "B")))
              "every file comes from the SAME tree — no mixed content")
          (is (not (staging-leftover? (.resolve genome-dir "skills"))) "no staging leftover"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))
