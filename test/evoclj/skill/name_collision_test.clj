(ns evoclj.skill.name-collision-test
  "S12 — deterministic same-name skill resolution across scopes.

  When the SAME skill name is discoverable from more than one scope
  (:project, :user, :extra), it resolves by the FIXED precedence
  project > user > extra — never by registration order and never by an
  arbitrary tie-break. An OPTIONAL fail-closed mode `:on-collision :error`
  turns a cross-scope same-name collision into a TYPED error instead of
  silently picking a winner.

  Every end-to-end assertion drives the production path
  `evoclj.skill.adapter/refresh-skills!` -> `SkillSource/snapshot!` (INV-09):
  no injected test hook, no shape-only assertion standing in for behavior,
  and no replication of the source's resolution logic in the test. The
  pure resolution functions in `evoclj.skill.collect` are exercised through
  the same production code that the snapshot path calls."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.skill.adapter :as adapter]
            [evoclj.skill.collect :as collect]
            [evoclj.environment.registry :as reg]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path Paths)
           (java.nio.file.attribute FileAttribute)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(def ^:private cas-roots (atom []))
(def ^:private tmp-roots (atom []))

(defn- track-cas! [p] (swap! cas-roots conj p) p)
(defn- track-tmp! [p] (swap! tmp-roots conj p) p)

(defn- temp-cas-root []
  (let [p (Files/createTempDirectory "evoclj-s12-cas-" (make-array FileAttribute 0))]
    (track-cas! p) p))

(defn- temp-skills-root []
  (let [p (Files/createTempDirectory "evoclj-s12-skills-" (make-array FileAttribute 0))]
    (track-tmp! p) p))

(defn- delete-tree! [^Path r]
  (when (Files/exists r (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile r)))]
      (try (Files/deleteIfExists (.toPath f)) (catch Exception _ nil)))))

(defn- cleanup! []
  (doseq [^Path r @cas-roots] (delete-tree! r))
  (reset! cas-roots [])
  (doseq [^Path r @tmp-roots] (delete-tree! r))
  (reset! tmp-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- write-skill!
  "Create <root>/<skill-name>/SKILL.md carrying a distinct description so the
   winning scope is observable. `desc` distinguishes each scope's version."
  [^Path root skill-name desc]
  (let [skill-dir (.resolve root skill-name)
        _ (Files/createDirectories skill-dir (make-array FileAttribute 0))
        skill-md (.resolve skill-dir "SKILL.md")
        md (str "---\nname: " skill-name "\ndescription: " desc "\n---\n# " skill-name "\n" desc "-body\n")
        _ (Files/write skill-md (.getBytes ^String md StandardCharsets/UTF_8)
                       (into-array java.nio.file.OpenOption
                                   [java.nio.file.StandardOpenOption/CREATE
                                    java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
                                    java.nio.file.StandardOpenOption/WRITE]))]
    skill-dir))

(defn- make-cas [^Path root]
  (cas/->cas (str root)))

(defn- make-source
  "Build a SkillSource over explicit project/user/extra roots, exposing the
   S12 scope + collision options. `root-scopes` is a map scope -> [paths]."
  [cas-root {:keys [root-scopes on-collision] :or {on-collision :precedence}}]
  (adapter/make-skill-source
   (cond-> {:source/id :skills/s12-test
            :cas (make-cas cas-root)
            :on-collision on-collision}
     (seq root-scopes) (assoc :root-scopes root-scopes))))

(defn- snapshot-payload
  "Run the full production refresh (refresh-skills! -> snapshot!) and return
   the payload map."
  [source]
  (let [{:keys [snapshot]} (adapter/refresh-skills! source)]
    (:payload snapshot)))

(defn- snapshot-description
  "Run production refresh and return the winner's frontmatter description for
   `skill-name`, or nil when the skill is absent."
  [source skill-name]
  (get-in (snapshot-payload source) [:skills skill-name :frontmatter :description]))

(defn- snapshot-skill-names
  "Run production refresh and return the set of published skill names."
  [source]
  (set (keys (:skills (snapshot-payload source)))))

(defn- snapshot-error-type
  "Run production refresh, returning :error/type of the thrown ExceptionInfo,
   or nil when no throw occurred."
  [source]
  (try
    (adapter/refresh-skills! source)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

(defn- snapshot-error-data
  "Run production refresh, returning the ex-data of the thrown ExceptionInfo."
  [source]
  (try
    (adapter/refresh-skills! source)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- build-error-type
  "Return :error/type when constructing a SkillSource throws, else nil."
  [opts]
  (try
    (adapter/make-skill-source opts)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

;; ---------------------------------------------------------------------------
;; 1. Happy path: distinct names in every scope are ALL preserved
;; ---------------------------------------------------------------------------

(deftest distinct-names-in-every-scope-are-preserved
  (testing "distinct names from project/user/extra all publish; none is dropped"
    (let [p (temp-skills-root) u (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "alpha" "PROJECT-ALPHA")
          _ (write-skill! u "beta"  "USER-BETA")
          _ (write-skill! e "gamma" "EXTRA-GAMMA")
          source (make-source cas-root {:root-scopes {:project [p] :user [u] :extra [e]}})]
      (is (= #{"alpha" "beta" "gamma"} (snapshot-skill-names source))))))

;; ---------------------------------------------------------------------------
;; 2. Branch: project beats user beats extra for the SAME name
;; ---------------------------------------------------------------------------

(deftest project-beats-user-beats-extra
  (testing "same-name skill present in project, user AND extra resolves to project"
    (let [p (temp-skills-root) u (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "dupe" "PROJECT-DUPE")
          _ (write-skill! u "dupe" "USER-DUPE")
          _ (write-skill! e "dupe" "EXTRA-DUPE")
          source (make-source cas-root {:root-scopes {:project [p] :user [u] :extra [e]}})]
      (is (= "PROJECT-DUPE" (snapshot-description source "dupe"))))))

(deftest user-beats-extra
  (testing "same-name in user AND extra only resolves to user (extra is lowest)"
    (let [u (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! u "dupe" "USER-DUPE")
          _ (write-skill! e "dupe" "EXTRA-DUPE")
          source (make-source cas-root {:root-scopes {:user [u] :extra [e]}})]
      (is (= "USER-DUPE" (snapshot-description source "dupe"))))))

(deftest legacy-extra-roots-classify-as-extra-lowest
  (testing "legacy :roots + :extra-roots derives :user > :extra without an explicit :root-scopes map"
    (let [u (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! u "dupe" "USER-DUPE")
          _ (write-skill! e "dupe" "EXTRA-DUPE")
          source (adapter/make-skill-source {:source/id :skills/s12-test
                                             :cas (make-cas cas-root)
                                             :roots [u]
                                             :extra-roots [e]})]
      (is (= "USER-DUPE" (snapshot-description source "dupe"))))))

(deftest project-beats-extra
  (testing "same-name in project AND extra only resolves to project"
    (let [p (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "dupe" "PROJECT-DUPE")
          _ (write-skill! e "dupe" "EXTRA-DUPE")
          source (make-source cas-root {:root-scopes {:project [p] :extra [e]}})]
      (is (= "PROJECT-DUPE" (snapshot-description source "dupe"))))))

;; ---------------------------------------------------------------------------
;; 3. Branch: RESULT is independent of registration/iteration ORDER
;; ---------------------------------------------------------------------------

(deftest precedence-is-order-independent
  (testing "reordering the scope map (project first vs last) and the paths within a scope never changes the winner"
    (let [p (temp-skills-root) u (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "dupe" "PROJECT-DUPE")
          _ (write-skill! u "dupe" "USER-DUPE")
          _ (write-skill! e "dupe" "EXTRA-DUPE")
          ;; project FIRST in one ordering (last-wins would yield extra), project
          ;; LAST in the other (last-wins would yield project). Precedence must
          ;; yield project in BOTH — i.e. independent of processing order.
          s1 (make-source cas-root {:root-scopes (array-map :project [p] :user [u] :extra [e])})
          s2 (make-source cas-root {:root-scopes (array-map :extra [e] :user [u] :project [p])})]
      (is (= "PROJECT-DUPE" (snapshot-description s1 "dupe")))
      (is (= "PROJECT-DUPE" (snapshot-description s2 "dupe"))))))

;; ---------------------------------------------------------------------------
;; 4. Branch: :on-collision :error makes a cross-scope collision a typed error
;; ---------------------------------------------------------------------------

(deftest on-collision-error-throws-typed
  (testing ":on-collision :error turns a cross-scope same-name collision into a typed error (fail-closed)"
    (let [p (temp-skills-root) u (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "dupe" "PROJECT-DUPE")
          _ (write-skill! u "dupe" "USER-DUPE")
          source (make-source cas-root {:root-scopes {:project [p] :user [u]} :on-collision :error})]
      (is (= :skill/name-collision (snapshot-error-type source))))))

(deftest on-collision-error-exposes-collision-details
  (testing "the typed error carries the colliding names and the scopes it spans"
    (let [p (temp-skills-root) u (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "dupe" "PROJECT-DUPE")
          _ (write-skill! u "dupe" "USER-DUPE")
          _ (write-skill! e "dupe" "EXTRA-DUPE")
          source (make-source cas-root {:root-scopes {:project [p] :user [u] :extra [e]} :on-collision :error})
          data (snapshot-error-data source)]
      (is (= :skill/name-collision (:error/type data)))
      (is (= "dupe" (:skill/name data)))
      (is (vector? (:collisions data)))
      (is (some #(= "dupe" %) (:collisions data)))
      (is (= [:extra :project :user] (get-in data [:scopes "dupe"]))))))

;; ---------------------------------------------------------------------------
;; 5. 'Different names unaffected' under :on-collision :error
;; ---------------------------------------------------------------------------

(deftest on-collision-error-distinct-names-do-not-throw
  (testing ":on-collision :error does NOT fire when names are distinct across scopes"
    (let [p (temp-skills-root) u (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "alpha" "PROJECT-ALPHA")
          _ (write-skill! u "beta"  "USER-BETA")
          source (make-source cas-root {:root-scopes {:project [p] :user [u]} :on-collision :error})]
      (is (nil? (snapshot-error-type source)))
      (is (= #{"alpha" "beta"} (snapshot-skill-names source))))))

;; ---------------------------------------------------------------------------
;; 6. Fault 1: an unknown scope keyword is rejected typed (fail-closed)
;; ---------------------------------------------------------------------------

(deftest unknown-scope-is-rejected-typed
  (testing "a :root-scopes map carrying an unrecognized scope keyword fails closed"
    (let [p (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "alpha" "A")]
      (is (= :skill/invalid-root-scope
             (build-error-type {:source/id :skills/s12-test
                                :cas (make-cas cas-root)
                                :root-scopes {:system [p]}}))))))

;; ---------------------------------------------------------------------------
;; 7. Fault 2: an invalid :on-collision value is rejected typed (fail-closed)
;; ---------------------------------------------------------------------------

(deftest invalid-collision-mode-is-rejected-typed
  (testing "an :on-collision value other than :precedence/:error fails closed"
    (let [p (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "alpha" "A")]
      (is (= :skill/invalid-collision-mode
             (build-error-type {:source/id :skills/s12-test
                                :cas (make-cas cas-root)
                                :root-scopes {:project [p]}
                                :on-collision :random}))))))

;; ---------------------------------------------------------------------------
;; 8. Same-scope duplicate resolves deterministically (never a typed error)
;; ---------------------------------------------------------------------------

(deftest same-scope-duplicate-resolves-deterministically
  (testing "two roots of the SAME scope sharing a name do NOT trip :error; they resolve deterministically by content"
    (let [a (temp-skills-root) b (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! a "dupe" "SAME-A")
          _ (write-skill! b "dupe" "SAME-B")
          source (make-source cas-root {:root-scopes {:user [a b]} :on-collision :error})
          d1 (snapshot-description source "dupe")
          source2 (make-source cas-root {:root-scopes {:user [b a]} :on-collision :error})
          d2 (snapshot-description source2 "dupe")]
      (is (nil? (snapshot-error-type source)) "no typed error for same-scope duplicate")
      (is (#{"SAME-A" "SAME-B"} d1) "winner is one of the same-scope entries")
      (is (= d1 d2) "winner is stable under root reorder"))))

;; ---------------------------------------------------------------------------
;; 9. Concurrency: independent sources resolve their own collisions in parallel
;; ---------------------------------------------------------------------------

(deftest parallel-sources-resolve-their-own-collisions
  (testing "concurrent snapshots over independent scoped sources each pick their own precedence winner"
    (let [configs (mapv (fn [i]
                          (let [p (temp-skills-root) u (temp-skills-root)
                                cas-root (temp-cas-root)]
                            (write-skill! p "dupe" (str "P" i))
                            (write-skill! u "dupe" (str "U" i))
                            {:p p :u u :cas-root cas-root :i i}))
                        (range 8))
          results (doall (pmap (fn [{:keys [p u cas-root i]}]
                                 (let [source (make-source cas-root {:root-scopes {:project [p] :user [u]}})]
                                   {:i i :desc (snapshot-description source "dupe")}))
                               configs))]
      (doseq [r results]
        (is (= (str "P" (:i r)) (:desc r)) (str "source " (:i r) " resolves project"))))))

;; ---------------------------------------------------------------------------
;; 4b. The resolved winner reaches the PUBLISHED catalog (registry refresh!)
;; ---------------------------------------------------------------------------

(deftest resolved-winner-reaches-published-catalog
  (testing "the precedence winner is what the registry's project->publish path exposes to the catalog"
    (let [p (temp-skills-root) u (temp-skills-root) e (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! p "dupe" "PROJECT-DUPE")
          _ (write-skill! u "dupe" "USER-DUPE")
          _ (write-skill! e "dupe" "EXTRA-DUPE")
          registry (reg/create-registry)
          source (make-source cas-root {:root-scopes {:project [p] :user [u] :extra [e]}})
          _ (reg/register-source! registry source)
          res (reg/refresh! registry)]
      (is (= :published (:status res)) "source published through the registry")
      (let [offers (adapter/list-offers registry)]
        (is (= 1 (count offers)) "one collided name resolves to ONE catalog entry")
        (let [o (first offers)]
          (is (= [:skill "dupe"] (:offer/logical-id o)))
          (is (= "dupe" (:offer/name o)))
          (is (str/includes? (or (:offer/description o) "") "PROJECT-DUPE")
              "the project-scope (highest precedence) skill is what the catalog exposes"))))))

;; ---------------------------------------------------------------------------
;; 10. Pure resolution functions (the production-side code, called directly)
;; ---------------------------------------------------------------------------

(deftest pure-resolve-skills-picks-highest-precedence
  (testing "collect/resolve-skills picks the highest-precedence scope entry"
    (let [entries [{:skill/name "dupe" :scope :extra :tree/id "sha256:0000000000000000000000000000000000000000000000000000000000000001" :frontmatter {:description "E"} :body ""}
                   {:skill/name "dupe" :scope :user :tree/id "sha256:0000000000000000000000000000000000000000000000000000000000000002" :frontmatter {:description "U"} :body ""}
                   {:skill/name "dupe" :scope :project :tree/id "sha256:0000000000000000000000000000000000000000000000000000000000000003" :frontmatter {:description "P"} :body ""}]
          resolved (collect/resolve-skills entries :precedence)]
      (is (= :project (:scope (get resolved "dupe"))))
      (is (= "P" (get-in resolved ["dupe" :frontmatter :description]))))))

(deftest pure-colliding-names-reports-cross-scope
  (testing "collect/colliding-names returns names spanning 2+ distinct scopes, sorted"
    (is (= ["dupe"]
           (collect/colliding-names [{:skill/name "dupe" :scope :project} {:skill/name "dupe" :scope :user} {:skill/name "ok" :scope :user}])))))
