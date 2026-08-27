(ns evoclj.skill.surface-test
  "S2 — skill/surface.clj 收口: single-arity materializer, fail-closed
  (INV-04), uniform content shape, no body-cache fallback, dead-code gone.

  All tests drive the PRODUCTION skill surface builder
  (evoclj.skill.surface/make-context-surface / skill->bundle) and invoke the
  materializer closure it produces against REAL CAS trees (via
  evoclj.fs.snapshot through test/evoclj/support/cas_tree_fixtures) — no
  cas-fn, no injected resolver (INV-09)."
  (:require [clojure.test :as t]
            [evoclj.skill.surface :as surf]
            [evoclj.environment.surface :as envsurf]
            [evoclj.support.cas-tree-fixtures :as fixtures]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir
  "Create a fresh temp directory."
  []
  (Files/createTempDirectory "evoclj-s2-" (make-array FileAttribute 0)))

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

(defn- skill-ctx
  "Build a production skill ContextSurface (via skill/surface.clj)."
  [{:keys [skill-name body cas tree-id]}]
  (surf/make-context-surface {:skill-name (or skill-name "a")
                              :frontmatter {}
                              :body body
                              :cas cas
                              :tree-id tree-id}))

(defn- skill-tree!
  "Snapshot a real skill tree into `cas-handle`; returns {:dir :tree/id}."
  [cas-handle skmd & [extra]]
  (let [dir (temp-dir)
        snap (fixtures/make-skill-tree!
              {:root dir
               :files (merge {"SKILL.md" skmd} (or extra {}))
               :cas cas-handle})]
    {:dir dir :tree/id (:tree/id snap)}))

;; ---------------------------------------------------------------------------
;; 1. HAPPY — surface produced (uniform shape) + consumed; exact CAS content
;; ---------------------------------------------------------------------------

(t/deftest skill-surface-materializer-returns-exact-cas-content
  (let [skmd "# Skill A\nBody A line\n"
        cas-handle (temp-cas)
        {:keys [dir tree/id]} (skill-tree! cas-handle skmd)]
    (try
      (let [s (skill-ctx {:skill-name "a" :body "STALE-CACHED" :cas cas-handle :tree-id id})
            mat (:materializer s)
            content (mat cas-handle id)]
        (t/is (= skmd content)
              "materializer returns the EXACT pinned CAS content, never the cached body")
        (t/is (string? content)
              "uniform content shape: materializer returns a content string")
        (t/is (envsurf/context-surface? s) "the skill surface is a valid ContextSurface")
        (t/is (= :context (:surface/type s)) "uniform shape: :surface/type :context")
        (t/is (= #{:surface/type :surface/id :descriptor :materializer :revision/id}
                 (set (keys s)))
              "uniform content shape: one canonical ContextSurface key set"))
      (finally (cleanup-tree! dir)))))

(t/deftest skill->bundle-produces-co-versioned-uniform-bundle
  (let [skmd "# Skill B\nBody B\n"
        cas-handle (temp-cas)
        {:keys [dir tree/id]} (skill-tree! cas-handle skmd)]
    (try
      (let [b (surf/skill->bundle {:skill/name "a" :tree/id id :frontmatter {} :body "cached" :cas cas-handle})
            surfaces (:surfaces b)
            ctx (first (filter #(= :context (:surface/type %)) surfaces))
            dir (first (filter #(= :directory (:surface/type %)) surfaces))]
        (t/is (= 2 (count surfaces)))
        (t/is (envsurf/context-surface? ctx) "context surface is a peer ContextSurface")
        (t/is (envsurf/directory-surface? dir) "directory surface is a peer DirectorySurface")
        ;; co-versioned: both surfaces carry the same revision/id
        (t/is (= id (:revision/id ctx)) "context surface carries the tree revision")
        (t/is (= id (:revision/id dir)) "directory surface carries the same revision")
        ;; consume: context materializer reads exact content from CAS
        (t/is (= skmd ((:materializer ctx) cas-handle id))
              "consuming the produced context surface yields the pinned SKILL.md"))
      (finally (cleanup-tree! dir)))))

;; ---------------------------------------------------------------------------
;; 2. BRANCH — single arity (INV-05: no ambiguous multi-arity overloads)
;; ---------------------------------------------------------------------------

(t/deftest skill-surface-materializer-single-arity
  (let [cas-handle (temp-cas)
        s (skill-ctx {:skill-name "a" :body "x" :cas cas-handle :tree-id (sha "a")})
        mat (:materializer s)]
    (t/is (thrown? clojure.lang.ArityException (mat))
          "arity 0 removed: no body-cache hint overload")
    (t/is (thrown? clojure.lang.ArityException (mat cas-handle))
          "arity 1 removed: no redispatch overload")
    (t/is (thrown? clojure.lang.ArityException (mat cas-handle (sha "a") {}))
          "arity 3 removed: no redispatch overload")
    (t/is (fn? mat) "surface materializer is a single function")))

;; ---------------------------------------------------------------------------
;; 3. BRANCH — fail-closed surface read (INV-04: no silent body-cache fallback)
;; ---------------------------------------------------------------------------

(t/deftest skill-surface-materializer-fails-closed-no-body-fallback
  (let [cas-handle (temp-cas)
        s (skill-ctx {:skill-name "a" :body "CACHED-BODY" :cas cas-handle :tree-id (sha "a")})
        mat (:materializer s)]
    ;; missing tree in CAS -> typed error, NOT the cached body
    (t/is (thrown? clojure.lang.ExceptionInfo (mat cas-handle (sha "a")))
          "a missing CAS tree must throw a typed error, never return the cached body")
    ;; nil CAS resolver -> typed error
    (t/is (thrown? clojure.lang.ExceptionInfo (mat nil (sha "a")))
          "nil CAS resolver must throw, never degrade to the cached body")
    ;; tree exists but SKILL.md absent -> typed error, NOT the cached body
    (let [dir2 (temp-dir)
          snap2 (fixtures/make-skill-tree! {:root dir2 :files {"guide.md" "g"} :cas cas-handle})
          id2 (:tree/id snap2)
          s2 (skill-ctx {:skill-name "b" :body "CACHED" :cas cas-handle :tree-id id2})]
      (try
        (t/is (thrown? clojure.lang.ExceptionInfo ((:materializer s2) cas-handle id2))
              "tree without SKILL.md must throw, never return the cached body")
        (finally (cleanup-tree! dir2))))))

(t/deftest skill-surface-materializer-error-is-typed
  (let [cas-handle (temp-cas)
        s (skill-ctx {:skill-name "a" :body "x" :cas cas-handle :tree-id (sha "a")})]
    (t/is (contains? #{:skill/materializer-missing-cas
                       :skill/missing-skill-md
                       :store/cas-missing}
                     (:error/type (ex-data (try
                                             ((:materializer s) cas-handle (sha "a"))
                                             (catch clojure.lang.ExceptionInfo e e)))))
          "fail-closed error carries a stable typed :error/type")))

;; ---------------------------------------------------------------------------
;; 4. FAULT — malformed/empty surface shape is rejected typed at the boundary
;; ---------------------------------------------------------------------------

(t/deftest skill-surface-builder-rejects-malformed-input-typed
  ;; make-context-surface (skill) delegates to env surface validation and the
  ;; skill-name validator: a malformed/empty surface must be rejected with a
  ;; typed error, never constructed into a silent degraded surface.
  (t/is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"(?i)non-empty string|invalid"
                          (surf/make-context-surface {:skill-name "  " :frontmatter {} :body "x" :cas nil :tree-id nil}))
        "a blank/invalid skill name fails closed with a typed surface error")
  (t/is (thrown? clojure.lang.ExceptionInfo
                 (envsurf/make-context-surface {:id nil :descriptor nil :materializer nil}))
        "a nil/empty surface is rejected typed"))
