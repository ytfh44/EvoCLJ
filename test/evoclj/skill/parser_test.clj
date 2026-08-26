(ns evoclj.skill.parser-test
  "S8: parser allowlist / YAML tag-event interception / timestamp-as-string /
  snakeyaml direct-dependency lock.

  All assertions traverse the production path evoclj.skill.parser/parse-skill-content
  (INV-09). No test-only injection hooks, no shape-only assertions, no replicated
  parser logic."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [evoclj.skill.parser :as parser])
  (:import (java.util Date)
           (java.time.temporal Temporal)))

;; ---------------------------------------------------------------------------
;; Happy path: a valid strict manifest parses to expected data.
;; ---------------------------------------------------------------------------

(deftest happy-valid-strict-manifest-parses
  (testing "valid strict SKILL.md parses name/description/allowed-tools and body"
    (let [content "---\nname: my-skill\ndescription: a helper\nallowed-tools: \"Read, Bash\"\n---\n# Body\nhello\n"
          parsed (parser/parse-skill-content content :strict)
          fm (:frontmatter parsed)]
      (is (= "my-skill" (:name fm)))
      (is (= "a helper" (:description fm)))
      (is (= "Read, Bash" (:allowed-tools fm)))
      (is (= "# Body\nhello" (:body parsed)))
      (is (= :strict (:mode parsed))))))

;; ---------------------------------------------------------------------------
;; Branch 1: allowlist — unknown YAML key/src rejects fail-closed.
;; ---------------------------------------------------------------------------

(deftest allowlist-rejects-unknown-key
  (testing "an unknown top-level frontmatter key is typed-rejected in strict AND lenient"
    (let [content "---\nname: x\ndescription: y\nbogus: 1\n---\n# body\n"]
      (testing "strict"
        (try
          (parser/parse-skill-content content :strict)
          (is false "strict accepted unknown key")
          (catch clojure.lang.ExceptionInfo e
            (is (= :skill/invalid-descriptor (:error/type (ex-data e))))
            (is (contains? (set (:keys (ex-data e))) :bogus)))))
      (testing "lenient"
        (try
          (parser/parse-skill-content content :lenient)
          (is false "lenient accepted unknown key")
          (catch clojure.lang.ExceptionInfo e
            (is (= :skill/invalid-descriptor (:error/type (ex-data e))))))))))

(deftest allowlist-accepts-known-keys
  (testing "the canonical keys and allowed-tools aliases are accepted"
    (doseq [content ["---\nname: x\ndescription: y\nallowed-tools: [Read]\n---\n# b\n"
                     "---\nname: x\ndescription: y\nallowed_tools: \"Read\"\n---\n# b\n"
                     "---\nname: x\ndescription: y\ntools: Read\n---\n# b\n"]]
      (is (map? (:frontmatter (parser/parse-skill-content content :strict)))))))

;; ---------------------------------------------------------------------------
;; Branch 2: tag event interception — unsafe/unrecognized tags reject typed.
;; ---------------------------------------------------------------------------

(deftest tag-interception-rejects-unsafe-tags
  (testing "a custom/unknown single-bang tag is rejected and the tag is carried in error data"
    (let [content "---\nname: x\ndescription: y\nd: !foo bar\n---\n# b\n"]
      (try
        (parser/parse-skill-content content :strict)
        (is false "custom tag accepted")
        (catch clojure.lang.ExceptionInfo e
          (is (= :skill/yaml-invalid (:error/type (ex-data e))))
          (is (contains? (ex-data e) :tag) "error data must carry the rejected tag"))))))

(deftest tag-interception-rejects-explicit-object-tags
  (testing "explicit double-bang object/constructor tags are rejected in both modes"
    (let [content "---\nname: x\ndescription: y\npayload: !!java/object \"java.lang.Runtime\"\n---\n# b\n"]
      (is (thrown? clojure.lang.ExceptionInfo (parser/parse-skill-content content :strict)))
      (is (thrown? clojure.lang.ExceptionInfo (parser/parse-skill-content content :lenient))))))

;; ---------------------------------------------------------------------------
;; Branch 3: timestamp-as-string — date/timestamps are STRINGS, never objects.
;; ---------------------------------------------------------------------------

(deftest timestamp-scalars-read-as-strings
  (testing "implicit date scalar is read as a STRING, never a Date/Temporal"
    (let [content "---\nname: x\ndescription: 2024-01-01\n---\n# body\n"
          parsed (parser/parse-skill-content content :strict)
          v (get-in parsed [:frontmatter :description])]
      (is (string? v) "value is a string")
      (is (= "2024-01-01" v))
      (is (not (instance? Date v)))
      (is (not (instance? Temporal v))))))

(deftest timestamp-datetime-read-as-strings
  (testing "implicit datetime scalar is read as a STRING, never a Date/Temporal"
    (let [content "---\nname: x\ndescription: 2024-05-01T10:00:00Z\n---\n# body\n"
          parsed (parser/parse-skill-content content :strict)
          v (get-in parsed [:frontmatter :description])]
      (is (string? v))
      (is (= "2024-05-01T10:00:00Z" v))
      (is (not (instance? Date v)))
      (is (not (instance? Temporal v))))))

;; ---------------------------------------------------------------------------
;; Concurrency: no shared mutable state exists (each parse builds its own
;; SafeConstructor/Yaml), so parallel parses must be independent.
;; ---------------------------------------------------------------------------

(deftest concurrent-parses-are-independent
  (testing "parallel parses of distinct contents do not cross-contaminate"
    (let [contents (mapv (fn [i]
                           (str "---\nname: skill-" i "\ndescription: desc-" i
                                (when (odd? i) "\nallowed-tools: [Read]") "\n---\n# Body\n" i))
                         (range 20))
          results (doall (pmap #(parser/parse-skill-content % :strict) contents))]
      (doseq [[i r] (map-indexed vector results)]
        (is (= (str "skill-" i) (get-in r [:frontmatter :name])))
        (is (= (str "desc-" i) (get-in r [:frontmatter :description])))
        (is (= (str "# Body\n" i) (:body r)))))))

;; ---------------------------------------------------------------------------
;; Regression: existing strict/lenient semantics preserved.
;; ---------------------------------------------------------------------------

(deftest regression-strict-requires-name-description
  (testing "strict still requires non-blank name and description"
    (let [missing-name "---\ndescription: no name\n---\n# Body\n"]
      (is (thrown? clojure.lang.ExceptionInfo (parser/parse-skill-content missing-name :strict)))))
  (let [missing-desc "---\nname: n\n---\n# Body\n"]
    (is (thrown? clojure.lang.ExceptionInfo (parser/parse-skill-content missing-desc :strict)))
    ;; lenient still allows missing description
    (is (map? (:frontmatter (parser/parse-skill-content missing-desc :lenient))))))

(deftest regression-allowed-tools-parsing
  (testing "allowed-tools raw + parsed tokens preserved (string and list forms)"
    (let [p1 (parser/parse-skill-content "---\nname: s1\ndescription: d1\nallowed-tools: [Read, Write]\n---\n# B\n" :lenient)
          p2 (parser/parse-skill-content "---\nname: s2\ndescription: d2\nallowed-tools: \"Read, Bash\"\n---\n# B\n" :lenient)]
      (is (= ["Read" "Write"] (get-in p1 [:allowed-tools :parsed])))
      (is (= ["Read" "Bash"] (get-in p2 [:allowed-tools :parsed])))
      (is (not (contains? (:frontmatter p1) :capabilities)) "never mints capability/lease"))))

;; ---------------------------------------------------------------------------
;; SnakeYAML direct-dependency lock (BT2 pinned 2.3).
;; ---------------------------------------------------------------------------

(defn- read-project-deps []
  (let [f (io/file (System/getProperty "user.dir") "deps.edn")]
    (when-not (.exists f)
      (throw (ex-info "deps.edn not found at repo root" {:file (str f)})))
    (edn/read-string (slurp f))))

(deftest snakeyaml-is-direct-pinned-dep
  (testing "snakeyaml is a DIRECT dependency pinned to 2.3 (BT2)"
    (let [deps (:deps (read-project-deps))
          entry (get deps 'org.yaml/snakeyaml)]
      (is (some? entry) "snakeyaml must be a direct dep")
      (is (= {:mvn/version "2.3"} entry) "snakeyaml must be pinned to 2.3"))))
