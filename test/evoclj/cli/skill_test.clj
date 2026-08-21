(ns evoclj.cli.skill-test
  "CLI unification — skill UX commands delegating to Skill adapter."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [evoclj.cli.main :as main]
            [evoclj.skill.adapter :as adapter]
            [evoclj.skill.parser :as parser]
            [evoclj.environment.registry :as reg]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files Path Paths LinkOption)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)))

;; Helpers for temp CAS / skills / genome

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [^String p]
  (let [root (Paths/get p (make-array String 0))]
    (when (Files/exists root (make-array LinkOption 0))
      (with-open [stream (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
        (doseq [path (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists path))))))

(defn- write-skill! [^String root skill-name content]
  (let [dir (Paths/get root (into-array String [skill-name]))
        _ (Files/createDirectories dir (make-array FileAttribute 0))
        f (.resolve dir "SKILL.md")]
    (Files/write f (.getBytes ^String content StandardCharsets/UTF_8)
                 (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE
                                                       java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
                                                       java.nio.file.StandardOpenOption/WRITE]))
    (str f)))

(defn- make-cas [^String dir]
  (cas/->cas dir))

(defn- fresh-registry []
  (reg/create-registry))

(defn- setup-skill!
  "Create a skill 'debugging' under skills-root and return {:registry :cas :skills-root :cas-root :skill-name :tree-id}"
  []
  (let [cas-root (temp-dir "evoclj-skill-cas-")
        skills-root (temp-dir "evoclj-skills-")
        registry (fresh-registry)
        cas-handle (make-cas cas-root)
        _ (write-skill! skills-root "debugging"
                        "---\nname: debugging\ndescription: Debugging helper\n---\n# Body\nHello skill\n")
        source (adapter/make-skill-source {:source/id :skills/test :roots [skills-root] :cas cas-handle :registry registry})
        _ (adapter/refresh-skills! source)
        bundle (adapter/get-skill-bundle registry "debugging")]
    {:registry registry
     :cas cas-handle
     :cas-root cas-root
     :skills-root skills-root
     :skill-name "debugging"
     :tree-id (:revision/id bundle)
     :bundle bundle
     :source source}))

;; skill list

(deftest skill-list-empty-when-no-registry
  (testing "skill list with no registry returns empty"
    (let [{:keys [exit data]} (main/execute ["skill" "list"] {})]
      (is (= 0 exit))
      (is (= 0 (:count data)))
      (is (= [] (:skills data))))))

(deftest skill-list-delegates-to-adapter
  (testing "skill list lists offers via adapter"
    (let [{:keys [registry cas-root skills-root]} (setup-skill!)
          {:keys [exit data]} (main/execute ["skill" "list"] {:registry registry})]
      (is (= 0 exit))
      (is (= 1 (:count data)))
      (is (= 1 (count (:skills data))))
      (is (= "debugging" (:skill/name (first (:skills data)))))
      (delete-tree! cas-root)
      (delete-tree! skills-root))))

(deftest skill-list-via-registry-contains-revision
  (testing "skill list entries carry revision and bundle ids"
    (let [{:keys [registry cas-root skills-root tree-id]} (setup-skill!)
          {:keys [exit data]} (main/execute ["skill" "list"] {:registry registry})]
      (is (= 0 exit))
      (let [s (first (:skills data))]
        (is (= tree-id (:revision/id s)))
        (is (string? (:bundle/id s)))
        (is (str/starts-with? (:bundle/id s) "bundle:skill:")))
      (delete-tree! cas-root)
      (delete-tree! skills-root))))

;; skill inspect

(deftest skill-inspect-requires-name
  (testing "skill inspect without name is usage-invalid"
    (let [{:keys [exit data]} (main/execute ["skill" "inspect"] {})]
      (is (= 1 exit))
      (is (= :cli/usage-invalid (:error/type data))))))

(deftest skill-inspect-existing
  (testing "skill inspect <name> returns bundle and offer"
    (let [{:keys [registry cas-root skills-root]} (setup-skill!)
          {:keys [exit data]} (main/execute ["skill" "inspect" "debugging"] {:registry registry})]
      (is (= 0 exit))
      (is (= "debugging" (:skill/name data)))
      (is (string? (:revision/id data)))
      (is (map? (:bundle data)))
      (is (seq (:surfaces data)))
      (delete-tree! cas-root)
      (delete-tree! skills-root))))

(deftest skill-inspect-not-found
  (testing "skill inspect unknown name is skill-not-found"
    (let [registry (fresh-registry)
          {:keys [exit data]} (main/execute ["skill" "inspect" "missing"] {:registry registry})]
      (is (= 1 exit))
      (is (= :cli/skill-not-found (:error/type data))))))

(deftest skill-inspect-no-registry
  (testing "skill inspect with no registry is skill-not-found"
    (let [{:keys [exit data]} (main/execute ["skill" "inspect" "debugging"] {})]
      (is (= 1 exit))
      (is (= :cli/skill-not-found (:error/type data))))))

;; skill validate

(deftest skill-validate-valid
  (testing "skill validate <path> validates strict SKILL.md"
    (let [dir (temp-dir "evoclj-validate-")
          skill-dir (Paths/get dir (into-array String ["my-skill"]))
          _ (Files/createDirectories skill-dir (make-array FileAttribute 0))
          f (.resolve skill-dir "SKILL.md")
          _ (Files/write f (.getBytes "---\nname: my-skill\ndescription: a helper\n---\n# Body\nhello\n" StandardCharsets/UTF_8)
                         (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE java.nio.file.StandardOpenOption/WRITE]))
          {:keys [exit data]} (main/execute ["skill" "validate" (str f)] {})]
      (is (= 0 exit))
      (is (true? (:valid? data)))
      (is (= "my-skill" (:skill/name data)))
      (is (= "my-skill" (:name (:frontmatter data))))
      (delete-tree! dir))))

(deftest skill-validate-directory
  (testing "skill validate <dir> resolves SKILL.md inside"
    (let [dir (temp-dir "evoclj-validate-dir-")
          skill-dir (Paths/get dir (into-array String ["my-skill"]))
          _ (Files/createDirectories skill-dir (make-array FileAttribute 0))
          f (.resolve skill-dir "SKILL.md")
          _ (Files/write f (.getBytes "---\nname: my-skill\ndescription: a helper\n---\n# Body\nhello\n" StandardCharsets/UTF_8)
                         (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE java.nio.file.StandardOpenOption/WRITE]))
          {:keys [exit data]} (main/execute ["skill" "validate" (str skill-dir)] {})]
      (is (= 0 exit))
      (is (true? (:valid? data)))
      (delete-tree! dir))))

(deftest skill-validate-strict-failure
  (testing "skill validate strictly rejects missing name/description"
    (let [dir (temp-dir "evoclj-validate-bad-")
          f (Paths/get dir (into-array String ["SKILL.md"]))
          _ (Files/write f (.getBytes "---\ndescription: no name\n---\n# Body\n" StandardCharsets/UTF_8)
                         (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE java.nio.file.StandardOpenOption/WRITE]))
          {:keys [exit data]} (main/execute ["skill" "validate" (str f)] {})]
      (is (= 1 exit))
      ;; parser throws :skill/invalid-descriptor or :skill/yaml-invalid
      (is (contains? #{:skill/invalid-descriptor :skill/yaml-invalid} (:error/type data)))
      (delete-tree! dir))))

(deftest skill-validate-missing-path
  (testing "skill validate missing path is skill-not-found"
    (let [{:keys [exit data]} (main/execute ["skill" "validate" "/tmp/no-such-skill-SKILL.md"] {})]
      (is (= 1 exit))
      (is (= :cli/skill-not-found (:error/type data))))))

(deftest skill-validate-requires-path
  (testing "skill validate without path is usage-invalid"
    (let [{:keys [exit data]} (main/execute ["skill" "validate"] {})]
      (is (= 1 exit))
      (is (= :cli/usage-invalid (:error/type data))))))

;; skill vendor

(deftest skill-vendor-requires-name
  (testing "skill vendor without name is usage-invalid"
    (let [{:keys [exit data]} (main/execute ["skill" "vendor"] {})]
      (is (= 1 exit))
      (is (= :cli/usage-invalid (:error/type data))))))

(deftest skill-vendor-not-found
  (testing "skill vendor unknown name is skill-not-found"
    (let [registry (fresh-registry)
          cas-root (temp-dir "evoclj-cas-vendor-")
          cas-handle (make-cas cas-root)
          genome-root (temp-dir "evoclj-genome-vendor-")
          {:keys [exit data]} (main/execute ["skill" "vendor" "missing"]
                                              {:registry registry :cas cas-handle :genome/root genome-root})]
      (is (= 1 exit))
      (is (= :cli/skill-not-found (:error/type data)))
      (delete-tree! cas-root)
      (delete-tree! genome-root))))

(deftest skill-vendor-delegates-to-cas-snapshot
  (testing "skill vendor <name> copies CAS snapshot revision into genome"
    (let [{:keys [registry cas cas-root skills-root tree-id]} (setup-skill!)
          genome-root (temp-dir "evoclj-genome-vendor-")
          ;; vendor via CLI dispatch
          {:keys [exit data]} (main/execute ["skill" "vendor" "debugging"]
                                             {:registry registry :cas cas :genome/root genome-root})]
      (is (= 0 exit))
      (is (= "debugging" (:skill/name data)))
      (is (= tree-id (:tree/id data)))
      ;; verify vendored file exists and content matches snapshot, not live mutation
      (let [vendored (Paths/get genome-root (into-array String ["skills" "debugging" "SKILL.md"]))
            content (String. (Files/readAllBytes vendored) StandardCharsets/UTF_8)]
        (is (Files/exists vendored (make-array LinkOption 0)))
        (is (str/includes? content "Hello skill")))
      ;; upstream mutation should not affect vendored copy
      (let [live-skill (.resolve (Paths/get skills-root (into-array String ["debugging"])) "SKILL.md")]
        (Files/write live-skill (.getBytes "---\nname: debugging\ndescription: Debugging helper\n---\n# Body\nUpstream mutated\n" StandardCharsets/UTF_8)
                     (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/TRUNCATE_EXISTING java.nio.file.StandardOpenOption/WRITE])))
      (let [vendored (Paths/get genome-root (into-array String ["skills" "debugging" "SKILL.md"]))
            content (String. (Files/readAllBytes vendored) StandardCharsets/UTF_8)]
        (is (str/includes? content "Hello skill"))
        (is (not (str/includes? content "Upstream mutated")) "vendored copy isolated from live path"))
      (delete-tree! cas-root)
      (delete-tree! skills-root)
      (delete-tree! genome-root))))

(deftest skill-commands-are-registered
  (testing "skill list/inspect/validate/vendor are known commands"
    (let [{:keys [registry cas-root skills-root]} (setup-skill!)
          dir (temp-dir "evoclj-skill-cmd-")
          f (Paths/get dir (into-array String ["SKILL.md"]))
          _ (Files/write f (.getBytes "---\nname: x\ndescription: y\n---\n# Body\n" StandardCharsets/UTF_8)
                         (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE java.nio.file.StandardOpenOption/WRITE]))
          genome-root (temp-dir "evoclj-genome-cmd-")]
      (is (= 0 (:exit (main/execute ["skill" "list"] {:registry registry}))))
      (is (= 0 (:exit (main/execute ["skill" "inspect" "debugging"] {:registry registry}))))
      (is (= 0 (:exit (main/execute ["skill" "validate" (str f)] {}))))
      (is (= 0 (:exit (main/execute ["skill" "vendor" "debugging"] {:registry registry :cas (make-cas cas-root) :genome/root genome-root}))))
      (delete-tree! cas-root)
      (delete-tree! skills-root)
      (delete-tree! dir)
      (delete-tree! genome-root))))
