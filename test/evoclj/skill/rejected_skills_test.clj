(ns evoclj.skill.rejected-skills-test
  "S11 — lenient discovery reports REJECTED skills with reasons.

  In :lenient discovery mode a skill whose per-skill capture/parse throws
  (invalid YAML manifest, unknown frontmatter key, disallowed YAML tag, etc.)
  is skipped for publication, but it MUST be REPORTED, never silently dropped.
  The SkillSource snapshot payload therefore carries a `:rejected-skills` list
  (skill dir name -> typed reason) plus a `:skill/rejected-count`.

  Every assertion here drives the production path
  `evoclj.skill.adapter/refresh-skills!` -> `SkillSource/snapshot!` (INV-09):
  no injected test hook, no shape-only assertion standing in for behavior, and
  no replication of the source's rejection logic in the test — the distinction
  between an accepted skill and a rejected one is judged entirely by the
  production capture + parse path and surfaced through the real snapshot.

  Fail-closed contract: a rejected skill ALWAYS appears in `:rejected-skills`
  with a `:reason` (a typed, serializable error-data map) — it can never be
  dropped silently."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [evoclj.skill.adapter :as adapter]
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
  (let [p (Files/createTempDirectory "evoclj-s11-cas-" (make-array FileAttribute 0))]
    (track-cas! p) p))

(defn- temp-skills-root []
  (let [p (Files/createTempDirectory "evoclj-s11-skills-" (make-array FileAttribute 0))]
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
  "Create <root>/<skill-name>/SKILL.md with `md`."
  [^Path root skill-name skill-md-content]
  (let [skill-dir (.resolve root skill-name)
        _ (Files/createDirectories skill-dir (make-array FileAttribute 0))
        skill-md (.resolve skill-dir "SKILL.md")
        _ (Files/write skill-md (.getBytes ^String skill-md-content StandardCharsets/UTF_8)
                       (into-array java.nio.file.OpenOption
                                   [java.nio.file.StandardOpenOption/CREATE
                                    java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
                                    java.nio.file.StandardOpenOption/WRITE]))]
    skill-dir))

(defn- make-cas [^Path root]
  (cas/->cas (str root)))

(defn- make-source
  ([skills-root cas-root] (make-source skills-root cas-root {}))
  ([skills-root cas-root {:keys [strict?]}]
   (adapter/make-skill-source {:source/id :skills/s11-test
                               :roots [skills-root]
                               :cas (make-cas cas-root)
                               :strict? (boolean strict?)})))

(defn- snapshot-payload
  "Run the full production refresh (snapshot!) and return the payload map."
  [source]
  (let [{:keys [snapshot]} (adapter/refresh-skills! source)]
    (:payload snapshot)))

(defn- rejected-names [payload]
  (set (map :skill/name (:rejected-skills payload))))

(defn- reason-type-for [payload name]
  (some->> (:rejected-skills payload)
           (filter #(= name (:skill/name %)))
           first
           :reason
           :error/type))

;; ---------------------------------------------------------------------------
;; Fixture SKILL.md sources (production classes)
;; ---------------------------------------------------------------------------

(def ^:private valid-md
  "A well-formed skill that the lenient source accepts."
  "---\nname: good\ndescription: a helper\nallowed-tools: [Read, Bash]\n---\n# Body\n")

(def ^:private unknown-key-md
  "Unknown frontmatter key -> :skill/invalid-descriptor (both modes)."
  "---\nname: badkey\ndescription: d\nunknown-key: foo\n---\n# Body\n")

(def ^:private bad-yaml-md
  "Explicit disallowed YAML tag -> :skill/yaml-invalid (both modes)."
  "---\nname: badyaml\ndescription: d\npayload: !!java/object \"x\"\n---\n# Body\n")

(def ^:private bad-token-md
  "Bad allowed-tools token -> WARN in lenient (S9), NOT a rejection."
  "---\nname: badtoken\ndescription: d\nallowed-tools: [Read, \"Bad Token\"]\n---\n# Body\n")

;; ---------------------------------------------------------------------------
;; 1. Happy path: valid skills -> empty rejected report
;; ---------------------------------------------------------------------------

(deftest valid-skills-only-produce-empty-rejected-list
  (testing "a source whose skills all parse cleanly yields an EMPTY :rejected-skills and zero rejected-count"
    (let [skills-root (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! skills-root "good" valid-md)
          source (make-source skills-root cas-root)
          payload (snapshot-payload source)]
      (is (vector? (:rejected-skills payload)))
      (is (empty? (:rejected-skills payload)) "no skill rejected")
      (is (= 0 (:skill/rejected-count payload)))
      (is (contains? (set (keys (:skills payload))) "good"))
      (is (= 1 (:skill/count payload))))))

;; ---------------------------------------------------------------------------
;; 2. Branch: a single rejected skill is reported with a typed reason
;; ---------------------------------------------------------------------------

(deftest rejected-skill-is-reported-with-reason
  (testing "an invalid (unknown frontmatter key) skill is reported with a typed reason; the valid skill is not"
    (let [skills-root (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! skills-root "good" valid-md)
          _ (write-skill! skills-root "badkey" unknown-key-md)
          source (make-source skills-root cas-root)
          payload (snapshot-payload source)]
      ;; valid accepted, invalid NOT accepted
      (is (contains? (set (keys (:skills payload))) "good"))
      (is (not (contains? (set (keys (:skills payload))) "badkey")))
      ;; rejection reported
      (is (= #{"badkey"} (rejected-names payload)))
      (is (= :skill/invalid-descriptor (reason-type-for payload "badkey")))
      (is (= 1 (:skill/rejected-count payload))))))

;; ---------------------------------------------------------------------------
;; 3. Branch: multiple rejected skills ALL listed with reasons; valid absent
;; ---------------------------------------------------------------------------

(deftest multiple-rejected-skills-all-listed-with-reasons
  (testing "every rejected skill is listed with its own reason; valid skills are absent"
    (let [skills-root (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! skills-root "good" valid-md)
          _ (write-skill! skills-root "badkey" unknown-key-md)
          _ (write-skill! skills-root "badyaml" bad-yaml-md)
          source (make-source skills-root cas-root)
          payload (snapshot-payload source)
          reasons (into {} (map (fn [e] [(:skill/name e) (:error/type (:reason e))])
                                (:rejected-skills payload)))]
      (is (= #{"badkey" "badyaml"} (rejected-names payload)) "both rejected skills listed")
      (is (= :skill/invalid-descriptor (get reasons "badkey")))
      (is (= :skill/yaml-invalid (get reasons "badyaml")))
      (is (= 2 (:skill/rejected-count payload)))
      (is (contains? (set (keys (:skills payload))) "good"))
      (is (not (contains? (set (keys (:skills payload))) "badkey")))
      (is (not (contains? (set (keys (:skills payload))) "badyaml"))))))

;; ---------------------------------------------------------------------------
;; 4. Fault: each rejection class surfaces a reason; report never omits
;; ---------------------------------------------------------------------------

(deftest each-rejection-class-surfaces-a-reason-and-none-are-omitted
  (testing "two distinct rejection classes (invalid-descriptor / yaml-invalid) each yield a typed reason, and the report is complete"
    (let [skills-root (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! skills-root "good" valid-md)
          _ (write-skill! skills-root "key-a" unknown-key-md)
          _ (write-skill! skills-root "key-b" (str/replace unknown-key-md "badkey" "key-b"))
          _ (write-skill! skills-root "yaml-a" bad-yaml-md)
          _ (write-skill! skills-root "yaml-b" (str/replace bad-yaml-md "badyaml" "yaml-b"))
          source (make-source skills-root cas-root)
          payload (snapshot-payload source)
          rejected (:rejected-skills payload)
          by-name (into {} (map (fn [e] [(:skill/name e) e]) rejected))]
      ;; all four rejected, all present — fail-closed contract (none omitted)
      (is (= 4 (:skill/rejected-count payload)))
      (is (= 4 (count rejected)))
      (is (= #{"key-a" "key-b" "yaml-a" "yaml-b"} (set (keys by-name))))
      ;; every entry carries a typed, serializable reason map
      (doseq [[_name e] by-name]
        (is (map? (:reason e)))
        (is (keyword? (:error/type (:reason e))))
        (is (string? (:error/message (:reason e)))))
      ;; class correctness
      (is (= :skill/invalid-descriptor (:error/type (:reason (get by-name "key-a")))))
      (is (= :skill/invalid-descriptor (:error/type (:reason (get by-name "key-b")))))
      (is (= :skill/yaml-invalid (:error/type (:reason (get by-name "yaml-a")))))
      (is (= :skill/yaml-invalid (:error/type (:reason (get by-name "yaml-b"))))))))

;; ---------------------------------------------------------------------------
;; 5. Report shape is stable and observable
;; ---------------------------------------------------------------------------

(deftest report-shape-is-stable-and-observable
  (testing ":rejected-skills is a vector of {skill/name -> reason} maps with a typed :reason"
    (let [skills-root (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! skills-root "badkey" unknown-key-md)
          source (make-source skills-root cas-root)
          payload (snapshot-payload source)
          rejected (:rejected-skills payload)]
      (is (vector? rejected))
      (is (= 1 (count rejected)))
      (let [entry (first rejected)]
        (is (= "badkey" (:skill/name entry)))
        (is (string? (:skill/name entry)))
        (is (= :skill/invalid-descriptor (:error/type (:reason entry))))
        (is (contains? (:reason entry) :error/message))
        (is (map? (:reason entry)))))))

;; ---------------------------------------------------------------------------
;; 6. Boundary: a bad allowed-tools token is a WARN (S9), not a rejection
;; ---------------------------------------------------------------------------

(deftest bad-tool-token-is-accepted-not-rejected-in-lenient
  (testing "a bad allowed-tools token is surfaced by S9 as a WARN, so the skill is still published and absent from :rejected-skills"
    (let [skills-root (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! skills-root "badtoken" bad-token-md)
          source (make-source skills-root cas-root)
          payload (snapshot-payload source)]
      (is (contains? (set (keys (:skills payload))) "badtoken") "bad-token skill is still published")
      (is (empty? (:rejected-skills payload)) "bad-token skill is NOT a rejection in lenient")
      (is (= 0 (:skill/rejected-count payload))))))

;; ---------------------------------------------------------------------------
;; 7. Boundary: :strict mode still fails closed (lenient report never weakens it)
;; ---------------------------------------------------------------------------

(deftest strict-mode-still-fails-closed-on-rejected-skill
  (testing ":strict mode still aborts (throws) on the first rejected skill; the lenient report never weakens strict fail-closed"
    (let [skills-root (temp-skills-root)
          cas-root (temp-cas-root)
          _ (write-skill! skills-root "good" valid-md)
          _ (write-skill! skills-root "badkey" unknown-key-md)
          source (make-source skills-root cas-root {:strict? true})]
      (is (thrown? clojure.lang.ExceptionInfo (adapter/refresh-skills! source))
          "strict source throws instead of reporting/continuing"))))

;; ---------------------------------------------------------------------------
;; 7. Concurrency: independent sources do not cross-contaminate the report
;; ---------------------------------------------------------------------------

(deftest concurrent-snapshots-report-only-their-own-rejected-skills
  (testing "parallel snapshots over independent sources each report only their own rejected skills"
    (let [configs (mapv (fn [i]
                          (let [skills-root (temp-skills-root)
                                cas-root (temp-cas-root)]
                            (write-skill! skills-root "good" valid-md)
                            (if (even? i)
                              (write-skill! skills-root "badkey" unknown-key-md)
                              (write-skill! skills-root "badyaml" bad-yaml-md))
                            {:skills-root skills-root :cas-root cas-root :i i}))
                        (range 8))
          results (doall (pmap (fn [{:keys [skills-root cas-root i]}]
                                 (let [source (make-source skills-root cas-root)
                                       payload (snapshot-payload source)]
                                   {:i i
                                    :names (rejected-names payload)
                                    :count (:skill/rejected-count payload)}))
                               configs))]
      (doseq [r results]
        (let [expected (if (even? (:i r)) #{"badkey"} #{"badyaml"})]
          (is (= expected (:names r)) (str "source " (:i r) " reports only its own rejected skill"))
          (is (= 1 (:count r)) (str "source " (:i r) " has exactly one rejection")))))))
