(ns evoclj.context.s1-test
  "S1 — P0 materializer descriptor :cas-tree-file; generic materializer
  detects trees; assembler placeholder segments fail closed (INV-04 /
  INV-09). All tests drive the production materializer/assembler through
  REAL CAS access (fixtures/make-skill-tree! produces a real CAS tree via
  evoclj.fs.snapshot) — no cas-fn, no injected resolver."
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [evoclj.context.binding :as binding]
            [evoclj.context.materializer :as mat]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.store.cas :as cas]
            [evoclj.support.cas-tree-fixtures :as fixtures])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir
  "Create a fresh temp directory (auto-cleanable via cleanup-tree!)."
  []
  (Files/createTempDirectory "evoclj-s1-" (make-array FileAttribute 0)))

(defn- temp-cas
  "Create a real CAS handle rooted at a fresh temp dir."
  []
  (cas/->cas (str (temp-dir))))

(defn- sha
  "A deterministic canonical sha256 artifact id for fault tests."
  [hex]
  (str "sha256:" (apply str (repeat 64 hex))))

(defn- cleanup-tree!
  [^java.nio.file.Path root]
  (when (Files/exists root (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile root)))]
      (try (Files/deleteIfExists (.toPath f)) (catch Exception _ nil)))))

(defn- skill-tree!
  "Snapshot a skill tree with the given SKILL.md content; returns tree/id."
  [cas-handle skmd & [extra]]
  (let [dir (temp-dir)
        snap (fixtures/make-skill-tree!
              {:root dir
               :files (merge {"SKILL.md" skmd} (or extra {}))
               :cas cas-handle})]
    {:dir dir :tree/id (:tree/id snap)}))

;; ---------------------------------------------------------------------------
;; 1. HAPPY — :cas-tree-file descriptor hydrates a tree from its file
;; ---------------------------------------------------------------------------

(t/deftest cas-tree-file-descriptor-hydrates-tree
  (let [skmd "# Skill A\nBody A line\n"
        guide "# Guide A\nGuide body\n"
        cas-handle (temp-cas)
        {:keys [dir tree/id]} (skill-tree! cas-handle skmd {"references/guide.md" guide})
        b (binding/make-binding {:logical-id [:skill "a"]
                                 :revision-id id
                                 :bundle-id "bundle:a"
                                 :descriptor {:type :cas-tree-file :path "references/guide.md"}})]
    (try
      (let [res (mat/materialize {:history ""
                                  :bindings [b]
                                  :catalog nil
                                  :policy nil
                                  :cas cas-handle})
            seg (first (:effective/segments res))]
        (t/is (= [:skill "a"] (:segment/logical-id seg)))
        (t/is (= id (:segment/revision-id seg)))
        (t/is (= guide (:segment/content seg))
              "tree-file descriptor must read the :path file from the CAS tree")
        (t/is (not= skmd (:segment/content seg))
              "tree-file descriptor must NOT fall back to SKILL.md when the path names another file"))
      (finally (cleanup-tree! dir)))))

;; ---------------------------------------------------------------------------
;; 2. BRANCH — generic materializer detects tree vs leaf (real CAS, no cas-fn)
;; ---------------------------------------------------------------------------

(t/deftest generic-materializer-distinguishes-tree-vs-leaf
  (let [skmd "# Skill T\nTree body\n"
        leaf-content "just a plain leaf artifact"
        cas-handle (temp-cas)
        {:keys [dir tree/id]} (skill-tree! cas-handle skmd)
        leaf-id (:artifact/id (cas/put-bytes! cas-handle
                                              (.getBytes leaf-content StandardCharsets/UTF_8)
                                              {:media-type "text/markdown"}))
        ;; both bindings carry NO descriptor -> generic detection decides
        btree (binding/make-binding {:logical-id [:skill "tree"]
                                     :revision-id id
                                     :bundle-id "bundle:t"})
        bleaf (binding/make-binding {:logical-id [:skill "leaf"]
                                     :revision-id leaf-id
                                     :bundle-id "bundle:l"})]
    (try
      (let [res (mat/materialize {:history ""
                                  :bindings [btree bleaf]
                                  :catalog nil
                                  :policy nil
                                  :cas cas-handle})
            segs (:effective/segments res)
            seg-for (fn [lid] (first (filter #(= lid (:segment/logical-id %)) segs)))]
        (t/is (= 2 (count segs)))
        (t/is (= skmd (:segment/content (seg-for [:skill "tree"])))
              "generic materializer must hydrate a tree blob by reading SKILL.md")
        (t/is (= leaf-content (:segment/content (seg-for [:skill "leaf"])))
              "generic materializer must hydrate a leaf blob verbatim")
        (t/is (not= skmd (:segment/content (seg-for [:skill "leaf"])))
              "leaf must not be misread as a tree"))
      (finally (cleanup-tree! dir)))))

;; ---------------------------------------------------------------------------
;; 3. FAULT — :cas-tree-file descriptor referencing a missing path fails closed
;; ---------------------------------------------------------------------------

(t/deftest cas-tree-file-missing-path-fails-closed
  (let [cas-handle (temp-cas)
        {:keys [dir tree/id]} (skill-tree! cas-handle "body")
        b (binding/make-binding {:logical-id [:skill "a"]
                                 :revision-id id
                                 :bundle-id "bundle:a"
                                 :descriptor {:type :cas-tree-file :path "nope.md"}})]
    (try
      (t/is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"(?i)absent|missing|not found"
                              (mat/materialize {:history ""
                                                :bindings [b]
                                                :catalog nil
                                                :policy nil
                                                :cas cas-handle}))
            "a tree-file path with no entry must throw, never emit a degraded placeholder")
      (finally (cleanup-tree! dir)))))

;; ---------------------------------------------------------------------------
;; 4. FAULT — a tree-backed binding whose tree is absent from CAS fails closed
;; ---------------------------------------------------------------------------

(t/deftest missing-tree-fails-closed
  (let [b (binding/make-binding {:logical-id [:skill "a"]
                                 :revision-id (sha "a")
                                 :bundle-id "bundle:a"})]
    (t/is (thrown? clojure.lang.ExceptionInfo
                   (mat/materialize {:history ""
                                     :bindings [b]
                                     :catalog nil
                                     :policy nil
                                     :cas (temp-cas)}))
          "a missing CAS tree must throw a typed error, not emit partial content")))

;; ---------------------------------------------------------------------------
;; 5. FAULT — assembler unresolved placeholder fails closed (no partial emit)
;; ---------------------------------------------------------------------------

(t/deftest assembler-placeholder-fails-closed
  (let [b (binding/make-binding {:logical-id [:skill "a"]
                                 :revision-id (sha "b")
                                 :bundle-id "bundle:a"})]
    (t/is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"(?i)placeholder|cas|resolve"
                            (assembler/assemble
                             {:base/messages [] :requested-tools []}
                             {:session-bindings [b] :cas nil}))
          "assembler must fail closed with a typed error instead of emitting a degraded placeholder segment")))

;; ---------------------------------------------------------------------------
;; 7. FAULT — an unknown binding descriptor kind fails closed at the boundary
;; ---------------------------------------------------------------------------

(t/deftest unknown-descriptor-kind-fails-closed
  (t/is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"(?i)unknown|descriptor"
                          (binding/make-binding {:logical-id [:skill "a"]
                                                 :revision-id (sha "c")
                                                 :bundle-id "bundle:a"
                                                 :descriptor {:type :cas-mystery}}))
        "an unknown materializer descriptor kind must throw at the binding boundary, never silently degrade"))

;; ---------------------------------------------------------------------------
;; 6. HAPPY — assembler WITH cas materializes a tree binding (proves the
;;    fail-closed path is not the only path)
;; ---------------------------------------------------------------------------

(t/deftest assembler-with-cas-materializes-tree
  (let [skmd "# Skill A\nAssembled body\n"
        cas-handle (temp-cas)
        {:keys [dir tree/id]} (skill-tree! cas-handle skmd)
        b (binding/make-binding {:logical-id [:skill "a"]
                                 :revision-id id
                                 :bundle-id "bundle:a"})]
    (try
      (let [prepared (assembler/assemble {:base/messages [] :requested-tools []}
                                         {:session-bindings [b] :cas cas-handle})
            sys-messages (filterv #(= "system" (:role %)) (:messages prepared))
            contents (mapv :content sys-messages)]
        (t/is (some #(str/includes? % skmd) contents)
              "assembler with cas must inject the tree's SKILL.md as a system message"))
      (finally (cleanup-tree! dir)))))
