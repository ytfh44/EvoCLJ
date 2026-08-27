(ns evoclj.skill.vendor-revend-test
  "S7: vendor strict gate (mandatory pre-install resolution) + re-vendor/diff
  entry + vendor audit events + overwrite-path correctness.

  Drive the production vendor-skill! / re-vendor! paths end to end (INV-09) and
  assert the strict contract:
    - a resolved/parseable manifest installs and RETURNS a typed audit event;
    - an unresolved/invalid manifest (no top-level SKILL.md, failed strict
      parse, or a skill-name/claim mismatch) is rejected typed BEFORE any
      install — no dest is created, no stale staging is left;
    - re-vendor! is idempotent (same source -> :status :unchanged, no write)
      and changed-only (different source -> :status :changed with a {:added
      :removed :changed} diff, dest replaced atomically);
    - overwriting an existing vendor path replaces the whole dest: old content
      gone, no stale files, no merge (WO-S6 atomic-replace semantics);
    - audit events are observable in the result and via an :audit/sink atom."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [evoclj.skill.vendor :as vendor]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.store.cas :as cas]
            [evoclj.genome.load :as load])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption OpenOption Path Paths)
           (java.nio.file.attribute FileAttribute)))

(def ^:private staging-prefix ".vendor-staging-")

(defn- temp-dir!
  ^Path []
  (Files/createTempDirectory "evoclj-vendor-revend-test" (make-array FileAttribute 0)))

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
           :metadata {:name "vendor-revend-fixture"}}))

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
  [opts]
  (try (vendor/vendor-skill! opts) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- thrown-revendor
  [opts]
  (try (vendor/re-vendor! opts) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- child-names
  [^Path dir]
  (when (exists? dir)
    (let [s (Files/newDirectoryStream dir)]
      (try
        (mapv str (iterator-seq (.iterator s)))
        (finally (.close s))))))

(defn- staging-leftover?
  [^Path dir]
  (boolean (some #(str/starts-with? % staging-prefix) (child-names dir))))

;; ---------------------------------------------------------------------------
;; 1. happy — a strictly-resolved manifest installs AND returns a typed audit
;; ---------------------------------------------------------------------------

(deftest resolved-manifest-installs-and-emits-audit
  (testing "a strictly-resolved manifest installs and returns a typed audit event"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "my-skill")
              _ (write-skill! ext "Body audit" :extra-files {"references/guide.md" "guide"})
              snap (snapshot/snapshot-tree! ext cas snapshot-limits)
              tree-id (:tree/id snap)
              vres (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              audit (:audit vres)]
          (is (= "my-skill" (:skill/name vres)))
          (is (= tree-id (:tree/id vres)))
          (is (map? audit) "audit event is returned and observable")
          (is (= :skill/vendored (:event/type audit)))
          (is (= "my-skill" (:skill/name audit)))
          (is (= tree-id (:tree/id audit)))
          (is (= tree-id (:version audit)))
          (is (= :evo/kernel (:actor audit)))
          (is (= :install (:action audit)))
          (is (true? (:installed? audit)))
          (is (false? (:changed? audit)))
          (is (contains? (set (:files audit)) "SKILL.md"))
          (is (Files/exists (.resolve (.resolve genome-dir "skills") "my-skill/SKILL.md")
                            (make-array LinkOption 0))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 2. fault — unresolved/invalid manifest rejected at the strict gate
;;    (missing top-level SKILL.md) BEFORE any install
;; ---------------------------------------------------------------------------

(deftest missing-skill-md-rejected-at-strict-gate
  (testing "a manifest with no top-level SKILL.md is rejected typed before any install"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              body "x\n"
              {:keys [artifact/id]} (cas/put-bytes! cas (.getBytes body StandardCharsets/UTF_8)
                                                    {:media-type "application/octet-stream"})
              manifest {:tree/version 1
                        :entries {"files/a.md" {:artifact/id id :size (count body)}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              skills-dir (.resolve genome-dir "skills")]
          (is (instance? clojure.lang.ExceptionInfo e) "must be rejected typed")
          (is (= :skill/vendor-missing-skill-md (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve skills-dir "my-skill") (make-array LinkOption 0)))
              "no dest was installed")
          (is (not (staging-leftover? skills-dir)) "no staging left"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 3. fault — SKILL.md that fails the strict parse is rejected typed
;; ---------------------------------------------------------------------------

(deftest invalid-skill-md-rejected-at-strict-gate
  (testing "a SKILL.md that fails strict validation is rejected typed before install"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "my-skill")
              _ (Files/createDirectories ext (make-array FileAttribute 0))
              _ (write-text! ext "SKILL.md" "no frontmatter here\n")
              snap (snapshot/snapshot-tree! ext cas snapshot-limits)
              tree-id (:tree/id snap)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              skills-dir (.resolve genome-dir "skills")]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (= :skill/vendor-invalid-manifest (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve skills-dir "my-skill") (make-array LinkOption 0)))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 4. fault — source-claim mismatch (declared name != target dir) rejected typed
;; ---------------------------------------------------------------------------

(deftest claim-mismatch-is-rejected-typed
  (testing "a SKILL.md claiming a different name is rejected typed (source claim mismatch)"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "other")
              _ (Files/createDirectories ext (make-array FileAttribute 0))
              _ (write-text! ext "SKILL.md" "---\nname: skill-other\ndescription: other skill\n---\nBody\n")
              snap (snapshot/snapshot-tree! ext cas snapshot-limits)
              tree-id (:tree/id snap)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              skills-dir (.resolve genome-dir "skills")]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (= :skill/vendor-claim-mismatch (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve skills-dir "my-skill") (make-array LinkOption 0)))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 5. branch — re-vendor is idempotent (same source -> :unchanged, no write)
;; ---------------------------------------------------------------------------

(deftest revendor-same-source-is-unchanged-noop
  (testing "re-vendoring the same tree is idempotent: :status :unchanged, no rewrite"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "my-skill")
              _ (write-skill! ext "Body same" :extra-files {"references/guide.md" "guide"})
              tree-id (:tree/id (snapshot/snapshot-tree! ext cas snapshot-limits))
              _ (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              dest (.resolve (.resolve genome-dir "skills") "my-skill")
              before-content (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8)
              r (vendor/re-vendor! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              after-content (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8)
              diff (:diff r)]
          (is (= :unchanged (:status r)))
          (is (empty? (:added diff)))
          (is (empty? (:removed diff)))
          (is (empty? (:changed diff)))
          (is (false? (:changed? (:audit r))))
          (is (false? (:installed? (:audit r))))
          (is (= :skill/re-vendored (:event/type (:audit r))))
          (is (contains? (set (:files (:audit r))) "SKILL.md"))
          (is (= before-content after-content) "content unchanged (no rewrite)")
          (is (not (staging-leftover? (.resolve genome-dir "skills")))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 6. branch+overwrite-path — re-vendor a changed source replaces the dest
;;    atomically with a diff report (old gone, new present, no merge)
;; ---------------------------------------------------------------------------

(deftest revendor-changed-replaces-atomically-with-diff
  (testing "re-vendoring a changed source replaces the dest atomically with a diff report"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-one (temp-dir!)
          ext-two (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              a (.resolve ext-one "my-skill")
              b (.resolve ext-two "my-skill")
              _ (write-skill! a "Body A" :extra-files {"old-marker.txt" "o"})
              treeA (:tree/id (snapshot/snapshot-tree! a cas snapshot-limits))
              _ (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeA})
              dest (.resolve (.resolve genome-dir "skills") "my-skill")
              _ (is (Files/exists (.resolve dest "old-marker.txt") (make-array LinkOption 0)))
              _ (write-skill! b "Body B" :extra-files {"new.txt" "n"})
              treeB (:tree/id (snapshot/snapshot-tree! b cas snapshot-limits))
              r (vendor/re-vendor! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeB})
              diff (:diff r)]
          (is (not= treeA treeB))
          (is (= :changed (:status r)))
          (is (true? (:changed? (:audit r))))
          (is (= :skill/re-vendored (:event/type (:audit r))))
          (is (= ["new.txt"] (:added diff)))
          (is (= ["old-marker.txt"] (:removed diff)))
          (is (contains? (set (:changed diff)) "SKILL.md") "SKILL.md changed")
          ;; overwrite correctness: old gone, new present, no merge, no stale, no leftover
          (is (not (Files/exists (.resolve dest "old-marker.txt") (make-array LinkOption 0))) "old file gone")
          (is (Files/exists (.resolve dest "new.txt") (make-array LinkOption 0)))
          (is (Files/exists (.resolve dest "SKILL.md") (make-array LinkOption 0)))
          (is (str/includes? (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8) "Body B"))
          (is (not (str/includes? (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8) "Body A")))
          (is (not (staging-leftover? (.resolve genome-dir "skills")))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-one)
          (delete-recursively! ext-two))))))

;; ---------------------------------------------------------------------------
;; 7. fault — re-vendor's strict gate rejects an invalid new source, leaving
;;    the currently installed dest untouched
;; ---------------------------------------------------------------------------

(deftest revendor-invalid-new-source-rejected-leaves-dest
  (testing "re-vendor's strict gate rejects an invalid new source, current dest untouched"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "my-skill")
              _ (write-skill! ext "Body good")
              tree-good (:tree/id (snapshot/snapshot-tree! ext cas snapshot-limits))
              _ (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-good})
              dest (.resolve (.resolve genome-dir "skills") "my-skill")
              prior (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8)
              bad-ext (.resolve ext-root "bad")
              _ (Files/createDirectories bad-ext (make-array FileAttribute 0))
              _ (write-text! bad-ext "SKILL.md" "no frontmatter\n")
              bad-tree (:tree/id (snapshot/snapshot-tree! bad-ext cas snapshot-limits))
              e (thrown-revendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id bad-tree})]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (= :skill/vendor-invalid-manifest (:error/type (ex-data e))))
          (is (= prior (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8))
              "current dest untouched"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 8. observable — vendor/re-vendor emits the audit event to an :audit/sink atom
;; ---------------------------------------------------------------------------

(deftest audit-event-emitted-to-sink
  (testing "vendor emits the audit event to an :audit/sink atom and in the result"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext (.resolve ext-root "my-skill")
              _ (write-skill! ext "Body sink")
              tree-id (:tree/id (snapshot/snapshot-tree! ext cas snapshot-limits))
              sink (atom [])
              vres (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill"
                                          :tree/id tree-id :audit/sink sink})]
          (is (= 1 (count @sink)) "one audit event emitted to the sink")
          (is (= :skill/vendored (:event/type (first @sink))))
          (is (= (:event/type (:audit vres)) :skill/vendored))
          (is (= (first @sink) (:audit vres)) "sink and result carry the same event"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 9. concurrency (shared dest) — concurrent re-vendors serialize: one complete
;;    tree wins, no torn/mixed content, no leftover staging
;; ---------------------------------------------------------------------------

(deftest concurrent-revendors-serialize-no-torn
  (testing "concurrent re-vendors into the same skill yield ONE complete dest, no leftover staging"
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
              f1 (future (vendor/re-vendor! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeA}))
              f2 (future (vendor/re-vendor! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeB}))
              r1 @f1
              r2 @f2
              dest (.resolve (.resolve genome-dir "skills") "my-skill")
              content (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8)
              side (String. (Files/readAllBytes (.resolve dest "side.txt")) StandardCharsets/UTF_8)]
          (is (some? r1) "re-vendor A succeeded")
          (is (some? r2) "re-vendor B succeeded")
          (is (Files/exists (.resolve dest "SKILL.md") (make-array LinkOption 0)))
          (is (Files/exists (.resolve dest "side.txt") (make-array LinkOption 0)))
          (is (or (and (str/includes? content "Body A") (not (str/includes? content "Body B")))
                  (and (str/includes? content "Body B") (not (str/includes? content "Body A"))))
              "SKILL.md is exactly one complete tree, not torn/mixed")
          (is (or (and (str/includes? content "Body A") (= side "A"))
                  (and (str/includes? content "Body B") (= side "B")))
              "every file comes from the SAME tree — no mixed content")
          (is (not (staging-leftover? (.resolve genome-dir "skills")))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))
