(ns evoclj.skill.vendor-test
  "Ownership split: external Skills vs vendored Genome skills."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [evoclj.helpers :as h]
            [evoclj.skill.vendor :as vendor]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.evolution.guard :as guard]
            [evoclj.genome.load :as load]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.patch :as patch]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files Path Paths LinkOption OpenOption)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

;; temp helpers collapsed to evoclj.helpers
(def ^:private temp-dir! h/temp-dir!)
(def ^:private delete-recursively! h/delete-recursively!)
(def ^:private write-text! h/write-text!)

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
           :metadata {:name "vendor-fixture"}}))

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

(def ^:private try-create-symlink! h/try-create-symlink!)

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

;; ---------------------------------------------------------------------------
;; 1. external Skill never mutation target (fails via allowlist)
;; ---------------------------------------------------------------------------


;; S4 helper — collapsed to evoclj.helpers
(def ^:private assert-validated h/assert-validated-simple)
(deftest external-skill-never-mutation-target
  (testing "attempt to mutate external skill path fails via allowlist"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [parent (load/load-genome genome-dir)
              cas (cas/->cas (str cas-dir))
              ;; external skill lives outside genome, e.g. .agents/skills
              external-file ".agents/skills/my-skill/SKILL.md"
              mut {:mutation/id (UUID/randomUUID)
                   :parent/genome-id (:genome/id parent)
                   :hypothesis/id (UUID/randomUUID)
                   :evidence/id (str "sha256:" (apply str (repeat 64 "a")))
                   :risk :behavioral
                   :ops [{:op :set-edn :file external-file :path [:a] :expect/hash (hash/text-digest "{:a 1}") :value 2}]
                   :expected-effect {:primary-metric :task/success :direction :increase}}
              e (try (mutation/validate-mutation mut parent) nil (catch clojure.lang.ExceptionInfo ex ex))]
          (is (instance? clojure.lang.ExceptionInfo e) "external path must be rejected")
          (is (contains? #{:mutation/undeclared-mutable-class :mutation/path-invalid :mutation/protected-path}
                         (:error/type (ex-data e)))
              (str "expected allowlist rejection, got " (:error/type (ex-data e))))
          ;; guard also rejects
          (let [ge (try (guard/validate-mutation-ownership! mut parent) nil (catch clojure.lang.ExceptionInfo ex ex))]
            (is (instance? clojure.lang.ExceptionInfo ge))
            (is (= (:error/type (ex-data e)) (:error/type (ex-data ge))))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 2. vendored Skill can be modified per manifest mutable rules
;; ---------------------------------------------------------------------------

(deftest vendored-skill-can-be-modified
  (testing "vendored skill under skills/<name>/* is mutable via :skills class"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext-skill-dir (.resolve ext-root "my-skill")
              _ (write-skill! ext-skill-dir "Body A original" :extra-files {"references/guide.md" "guide A"})
              snap (snapshot/snapshot-tree! ext-skill-dir cas snapshot-limits)
              tree-id (:tree/id snap)
              vres (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
          (is (= "my-skill" (:skill/name vres)))
          (is (= tree-id (:tree/id vres)))
          ;; reload genome: vendored files are now part of bundle
          (let [parent (load/load-genome genome-dir)]
            (is (contains? (:files parent) "skills/my-skill/SKILL.md") "vendored SKILL.md participates in genome identity")
            (is (contains? (:files parent) "skills/my-skill/references/guide.md"))
            ;; mutation targeting vendored file must PASS allowlist
            (let [target "skills/my-skill/SKILL.md"
                  file-val (get-in parent [:files target])
                  digest (:digest file-val)
                  content (String. (byte-array (:bytes file-val)) StandardCharsets/UTF_8)
                  _ (is (str/includes? content "Body A original"))
                  mut {:mutation/id (UUID/randomUUID)
                       :parent/genome-id (:genome/id parent)
                       :hypothesis/id (UUID/randomUUID)
                       :evidence/id (str "sha256:" (apply str (repeat 64 "b")))
                       :risk :behavioral
                       :ops [{:op :replace-text :file target :anchor "Body A original" :text "Body A updated" :expect/hash digest}]
                       :expected-effect {:primary-metric :task/success :direction :increase}}]
              ;; allowlist should accept
              (assert-validated mut (mutation/validate-mutation mut parent))
              (assert-validated mut (guard/validate-mutation-ownership! mut parent))
              ;; patch application should succeed with new candidate
              (let [out-dir (temp-dir!)]
                (try
                  (let [candidate (patch/apply-mutation parent (mutation/validate-mutation mut parent) (str out-dir))]
                    (is (contains? (:files candidate) target))
                    (let [cbytes (get-in candidate [:files target :bytes])
                          cstr (String. (byte-array cbytes) StandardCharsets/UTF_8)]
                      (is (str/includes? cstr "Body A updated"))
                      (is (not (str/includes? cstr "Body A original")))))
                  (finally (delete-recursively! out-dir)))))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 3. upstream package update does not affect vendored copy
;; ---------------------------------------------------------------------------

(deftest upstream-update-does-not-affect-vendored-copy
  (testing "vendor copies snapshot revision, not live host path"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext-skill-dir (.resolve ext-root "my-skill")
              _ (write-skill! ext-skill-dir "Body A vendored" :extra-files {"references/guide.md" "guide A"})
              snapA (snapshot/snapshot-tree! ext-skill-dir cas snapshot-limits)
              treeA (:tree/id snapA)
              _ (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id treeA})
              vendored-path (.resolve genome-dir "skills/my-skill/SKILL.md")
              vendored-path-guide (.resolve genome-dir "skills/my-skill/references/guide.md")
              contentA (String. (Files/readAllBytes vendored-path) StandardCharsets/UTF_8)
              guideA (String. (Files/readAllBytes vendored-path-guide) StandardCharsets/UTF_8)]
          (is (str/includes? contentA "Body A vendored"))
          (is (str/includes? guideA "guide A"))
          ;; upstream update: change live external dir to Body B
          (write-skill! ext-skill-dir "Body B upstream updated" :extra-files {"references/guide.md" "guide B updated" "references/new.md" "new file"})
          (let [snapB (snapshot/snapshot-tree! ext-skill-dir cas snapshot-limits)
                treeB (:tree/id snapB)]
            (is (not= treeA treeB) "new snapshot has different tree id")
            ;; vendored copy must remain A (not affected by upstream)
            (let [contentAfter (String. (Files/readAllBytes vendored-path) StandardCharsets/UTF_8)
                  guideAfter (String. (Files/readAllBytes vendored-path-guide) StandardCharsets/UTF_8)]
              (is (= contentA contentAfter) "vendored SKILL.md unchanged after upstream update")
              (is (= guideA guideAfter) "vendored guide unchanged"))
            ;; also verify via CAS: treeA still yields A, treeB yields B
            (let [manifestA (snapshot/load-tree cas treeA)
                  baA (snapshot/get-file-bytes cas manifestA "SKILL.md")
                  strA (String. ^bytes baA StandardCharsets/UTF_8)
                  manifestB (snapshot/load-tree cas treeB)
                  baB (snapshot/get-file-bytes cas manifestB "SKILL.md")
                  strB (String. ^bytes baB StandardCharsets/UTF_8)]
              (is (str/includes? strA "Body A vendored"))
              (is (str/includes? strB "Body B upstream"))
              (is (not= strA strB)))
            ;; genome reload still shows vendored A, not B, and does not contain new.md
            (let [reloaded (load/load-genome genome-dir)
                  vbytes (get-in reloaded [:files "skills/my-skill/SKILL.md" :bytes])
                  vstr (String. (byte-array vbytes) StandardCharsets/UTF_8)]
              (is (str/includes? vstr "Body A vendored"))
              (is (not (str/includes? vstr "Body B")))
              (is (not (contains? (:files reloaded) "skills/my-skill/references/new.md"))
                  "new upstream file not in vendored genome"))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root))))))

;; ---------------------------------------------------------------------------
;; 4. S6P happy — a legit nested relative path still vendors (realpath
;;    containment + segment-level '..' check do NOT reject a legal path).
;; ---------------------------------------------------------------------------

(deftest legit-nested-path-vendors
  (testing "a well-formed nested rel under skills/<name>/ vendors through the production path"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              nested-body "hello nested\n"
              ;; The strict gate (WO-S7) requires a resolvable top-level SKILL.md
              ;; whose declared name matches the target dir, so the fixture must
              ;; be a valid skill package; the nested files still vendor below.
              skill-md (str "---\nname: my-skill\ndescription: test skill\n---\n" nested-body)
              md-id (:artifact/id (cas/put-bytes! cas (.getBytes skill-md StandardCharsets/UTF_8)
                                                    {:media-type "application/octet-stream"}))
              nested-id (:artifact/id (cas/put-bytes! cas (.getBytes nested-body StandardCharsets/UTF_8)
                                                        {:media-type "application/octet-stream"}))
              manifest {:tree/version 1
                        :entries {"SKILL.md"         {:artifact/id md-id :size (count skill-md)}
                                  "sub/dir/SKILL.md" {:artifact/id nested-id :size (count nested-body)}
                                  "sub/top.md"       {:artifact/id nested-id :size (count nested-body)}}}
              tree-id (put-tree! cas manifest)
              vres (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
          (is (= "my-skill" (:skill/name vres)))
          (is (= tree-id (:tree/id vres)))
          (let [reloaded (load/load-genome genome-dir)]
            (is (contains? (:files reloaded) "skills/my-skill/sub/dir/SKILL.md"))
            (is (contains? (:files reloaded) "skills/my-skill/sub/top.md"))
            (is (contains? (:files reloaded) "skills/my-skill/SKILL.md"))
            (is (= nested-body (String. (byte-array (get-in reloaded [:files "skills/my-skill/sub/dir/SKILL.md" :bytes]))
                                        StandardCharsets/UTF_8)))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 5. S6P branch+fault #1 — a '..' segment escaping the skill dir is rejected
;;    at the segment/lexical level, typed :skill/vendor-path-escape, with no
;;    file written outside the root.
;; ---------------------------------------------------------------------------

(deftest dotdot-escape-is-rejected-typed
  (testing "a '../escape.txt' manifest entry is rejected before any write"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              manifest {:tree/version 1
                        :entries {"../escape.txt" {:artifact/id (str "sha256:" (apply str (repeat 64 "a")))
                                                   :size 5}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
          (is (instance? clojure.lang.ExceptionInfo e) "must be typed-rejected")
          (is (= :skill/vendor-path-escape (:error/type (ex-data e)))
              (str "expected segment-level escape type, got " (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve genome-dir "escape.txt") (make-array LinkOption 0)))
              "no file may escape the genome root"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

(deftest nested-dotdot-escape-is-rejected-typed
  (testing "a 'sub/../../escape.txt' manifest entry is rejected at the segment level"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              manifest {:tree/version 1
                        :entries {"sub/../../escape.txt" {:artifact/id (str "sha256:" (apply str (repeat 64 "a")))
                                                          :size 5}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (= :skill/vendor-path-escape (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve genome-dir "escape.txt") (make-array LinkOption 0)))
              "no file may escape the genome root"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 6. S6P branch — backslash / absolute structural invalidity is typed-rejected
;;    (defense in breadth; not the '..'-escape discriminator).
;; ---------------------------------------------------------------------------

(deftest non-escaping-dotdot-is-rejected
  (testing "a '..' segment is rejected at the segment level even when it does NOT escape"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              body "x\n"
              {:keys [artifact/id]} (cas/put-bytes! cas (.getBytes body StandardCharsets/UTF_8)
                                                    {:media-type "application/octet-stream"})
              manifest {:tree/version 1
                        :entries {"sub/../SKILL.md" {:artifact/id id :size (count body)}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (= :skill/vendor-path-escape (:error/type (ex-data e))) "rejected at segment level")
          (is (not (Files/exists (.resolve genome-dir "skills/my-skill/SKILL.md")
                                 (make-array LinkOption 0)))
              "no file may be written from a ..-containing rel"))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

(deftest backslash-escape-is-rejected
  (testing "a Windows backslash traversal entry is rejected typed"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              manifest {:tree/version 1
                        :entries {"..\\..\\escape.txt" {:artifact/id (str "sha256:" (apply str (repeat 64 "a")))
                                                        :size 5}}}
              tree-id (put-tree! cas manifest)
              e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (contains? #{:skill/vendor-invalid-args :skill/vendor-path-escape}
                         (:error/type (ex-data e))))
          (is (not (Files/exists (.resolve genome-dir "escape.txt") (make-array LinkOption 0)))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir))))))

;; ---------------------------------------------------------------------------
;; 7. S6P fault #2a — deleting a stale skill dir must NOT follow a symlink to
;;    an outside target; the link is removed as a link and the target is intact.
;; ---------------------------------------------------------------------------

(deftest delete-does-not-follow-symlink
  (testing "a symlinked child inside the skill dir is deleted as a link, never followed"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)
          outside-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext-skill (.resolve ext-root "my-skill")
              _ (write-skill! ext-skill "Body fixed" :extra-files {"references/guide.md" "guide fixed"})
              snap (snapshot/snapshot-tree! ext-skill cas snapshot-limits)
              tree-id (:tree/id snap)
              sentinel (write-text! outside-dir "secret.txt" "SECRET")
              dest (.resolve (.resolve genome-dir "skills") "my-skill")]
          (Files/createDirectories dest (make-array FileAttribute 0))
          (if (try-create-symlink! outside-dir (.resolve dest "evil"))
            (do
              (vendor/vendor-skill! {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})
              (is (Files/exists sentinel (make-array LinkOption 0))
                  "the symlink target was NOT followed for delete — outside content survives")
              (is (Files/exists (.resolve dest "SKILL.md") (make-array LinkOption 0))
                  "vendored content written into the refreshed skill dir")
              (is (not (Files/exists (.resolve dest "evil") (make-array LinkOption 0)))
                  "the symlink itself was removed AS A LINK")
              (is (str/includes? (String. (Files/readAllBytes (.resolve dest "SKILL.md")) StandardCharsets/UTF_8)
                                 "Body fixed")))
            (testing "symlink creation unavailable on this host; skipped" (is true))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root)
          (delete-recursively! outside-dir))))))

;; ---------------------------------------------------------------------------
;; 8. S6P fault #2b — realpath containment: a symlinked <genome-root>/skills
;;    pointing OUTSIDE the genome root is rejected before any write.
;; ---------------------------------------------------------------------------

(deftest symlinked-skills-escape-is-rejected-before-write
  (testing "a symlinked skills dir escaping the genome root is rejected typed before write"
    (let [genome-dir (temp-dir!)
          cas-dir (temp-dir!)
          ext-root (temp-dir!)
          outside-dir (temp-dir!)]
      (try
        (write-minimal-genome! genome-dir)
        (let [cas (cas/->cas (str cas-dir))
              ext-skill (.resolve ext-root "my-skill")
              _ (write-skill! ext-skill "Body bonus" :extra-files {"references/guide.md" "guide bonus"})
              snap (snapshot/snapshot-tree! ext-skill cas snapshot-limits)
              tree-id (:tree/id snap)
              skills-dir (.resolve genome-dir "skills")]
          (if (try-create-symlink! outside-dir skills-dir)
            (let [e (thrown-vendor {:genome/root genome-dir :cas cas :skill/name "my-skill" :tree/id tree-id})]
              (is (instance? clojure.lang.ExceptionInfo e))
              (is (= :skill/vendor-path-escape (:error/type (ex-data e))))
              (is (not (Files/exists (.resolve outside-dir "my-skill/SKILL.md")
                                     (make-array LinkOption 0)))
                  "nothing was written through the symlink into the outside target"))
            (testing "symlink creation unavailable on this host; skipped" (is true))))
        (finally
          (delete-recursively! genome-dir)
          (delete-recursively! cas-dir)
          (delete-recursively! ext-root)
          (delete-recursively! outside-dir))))))