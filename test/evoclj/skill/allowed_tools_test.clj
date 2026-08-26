(ns evoclj.skill.allowed-tools-test
  "S9: allowed-tools token grammar validation — lenient WARN / strict REJECT.

  All assertions traverse the production path evoclj.skill.parser/parse-skill-content
  (INV-09). There is no test-only injection hook, no shape-assertion impersonating
  behavior, and no replicated parser logic: the invalid/valid distinction is judged
  by the production token grammar and surfaced through the real parse result and the
  real typed throw.

  Grammar (documented in evoclj.skill.parser): a valid allowed-tools token is a
  non-empty, non-blank string, with no internal whitespace/comma/separator, whose
  characters form a well-formed tool identifier (letters, digits, and the
  namespacing separators . _ / : + -). Empty, whitespace-only, and malformed
  (e.g. containing a space, comma, quote, semicolon, or other illegal character)
  tokens are INVALID.

  - strict mode: any invalid token => typed :skill/invalid-tool-token (fail-closed).
  - lenient mode: invalid tokens => parse continues, but a WARN diagnostic is
    surfaced (never silent, never fatal)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.skill.parser :as parser]))

(defn- content
  "Build a minimal SKILL.md body with a single frontmatter line."
  [at-line]
  (str "---\nname: s\ndescription: d\n" at-line "\n---\n# Body\n"))

;; ---------------------------------------------------------------------------
;; Happy path: well-formed tokens accepted in both modes.
;; ---------------------------------------------------------------------------

(deftest valid-tokens-accepted-in-both-modes
  (testing "well-formed allowed-tools tokens are accepted in strict AND lenient"
    (doseq [mode [:strict :lenient]
            at ["allowed-tools: [Read, Bash]"
                "allowed-tools: \"Read, Bash\""
                "allowed-tools: [Read, Write, Grep, Glob]"
                "allowed-tools: [AskUserQuestion, TodoWrite, ExitPlanMode]"
                "allowed-tools: [fixture/echo, filesystem/generic, server-a/read_file]"]]
      (let [parsed (parser/parse-skill-content (content at) mode)
            at-map (:allowed-tools parsed)]
        (is (map? parsed) (str "parses in " mode))
        (is (true? (:valid? at-map)) (str "all tokens valid in " mode ": " at))
        (is (= [] (:invalid at-map)) (str "no invalid tokens in " mode ": " at))
        (is (= [] (:warnings parsed)) (str "no WARN diagnostic in " mode ": " at))))))

;; ---------------------------------------------------------------------------
;; Branch: strict rejects malformed tokens typed (fail-closed).
;; ---------------------------------------------------------------------------

(deftest strict-rejects-invalid-token-typed
  (testing "strict fails closed (typed) on ANY invalid allowed-tools token"
    (doseq [at ["allowed-tools: [Read, \"\"]"
                "allowed-tools: [Read, \"  \"]"
                "allowed-tools: [Read, \"Bad Token\"]"
                "allowed-tools: [Read, \"Read,Bash\"]"
                "allowed-tools: [Read;Write]"]]
      (try
        (parser/parse-skill-content (content at) :strict)
        (is false (str "strict must REJECT: " at))
        (catch clojure.lang.ExceptionInfo e
          (is (= :skill/invalid-tool-token (:error/type (ex-data e))) (str "typed error for: " at))
          (is (seq (:invalid (ex-data e))) (str "carries the invalid token list for: " at)))))))

;; ---------------------------------------------------------------------------
;; Branch: lenient warns + continues (diagnostic surfaced, not silent, not fatal).
;; ---------------------------------------------------------------------------

(deftest lenient-warns-and-continues
  (testing "lenient does not throw but surfaces an invalid-token diagnostic"
    (let [parsed (parser/parse-skill-content (content "allowed-tools: [Read, \"Bad Token\"]") :lenient)
          at-map (:allowed-tools parsed)]
      (is (map? parsed) "lenient continues (not fatal)")
      (is (false? (:valid? at-map)) "invalid token flagged")
      (is (contains? (set (:invalid at-map)) "Bad Token") "invalid token present in :invalid")
      (is (not (empty? (:warnings parsed))) "lenient surfaces a WARN diagnostic")
      (is (= :skill/invalid-tool-token (:type (first (:warnings parsed)))) "diagnostic is typed")
      (is (contains? (set (:allowed-tools-invalid parsed)) "Bad Token") "diagnostic surfaced at top level")
      (is (= ["Read" "Bad Token"] (:parsed at-map)) "invalid token is surfaced, not silently dropped")
      (testing "the very same content is fail-closed in strict"
        (is (thrown? clojure.lang.ExceptionInfo
                     (parser/parse-skill-content (content "allowed-tools: [Read, \"Bad Token\"]") :strict)))))))

;; ---------------------------------------------------------------------------
;; Fault: empty / whitespace-only tokens are rejected (strict) / notified (lenient).
;; ---------------------------------------------------------------------------

(deftest empty-and-whitespace-tokens-are-notified
  (testing "empty and whitespace-only allowed-tools tokens are rejected in strict / notified in lenient"
    (doseq [at ["allowed-tools: [Read, \"\"]"
                "allowed-tools: [Read, \"  \"]"]]
      (testing (str "token: " at)
        (is (thrown? clojure.lang.ExceptionInfo
                     (parser/parse-skill-content (content at) :strict))
          "strict rejects empty/whitespace token")
        (let [p (parser/parse-skill-content (content at) :lenient)]
          (is (false? (get-in p [:allowed-tools :valid?])))
          (is (seq (get-in p [:allowed-tools :invalid])) "invalid token surfaced"))
        (is (seq (:allowed-tools-invalid (parser/parse-skill-content (content at) :lenient))))))))

;; ---------------------------------------------------------------------------
;; Fault: malformed character sets are rejected / notified.
;; ---------------------------------------------------------------------------

(deftest malformed-charset-tokens-rejected-or-notified
  (testing "tokens with illegal/sentinel characters are rejected in strict / notified in lenient"
    (doseq [at ["allowed-tools: [Read, \"#bad\"]"
                "allowed-tools: [Read, \"a!b\"]"
                "allowed-tools: [Read, \"quote'q\"]"]]
      (testing (str "charset: " at)
        (is (thrown? clojure.lang.ExceptionInfo (parser/parse-skill-content (content at) :strict)))
        (let [p (parser/parse-skill-content (content at) :lenient)]
          (is (false? (get-in p [:allowed-tools :valid?])))
          (is (seq (get-in p [:allowed-tools :invalid]))))))))

;; ---------------------------------------------------------------------------
;; Concurrency: no shared mutable state in the grammar path.
;; ---------------------------------------------------------------------------

(deftest concurrent-parses-are-independent
  (testing "parallel parse of varied allowed-tools does not cross-contaminate"
    (let [inputs (mapv (fn [i]
                         (content (if (even? i)
                                    "allowed-tools: [Read, Bash]"
                                    "allowed-tools: [Read, \"Bad Token\"]")))
                       (range 24))
          results (doall (pmap #(parser/parse-skill-content % :lenient) inputs))]
      (doseq [[i r] (map-indexed vector results)]
        (let [at-map (:allowed-tools r)]
          (if (even? i)
            (do (is (true? (:valid? at-map)) (str "input " i " all valid"))
                (is (= [] (:invalid at-map))))
            (do (is (false? (:valid? at-map)) (str "input " i " has invalid token"))
                (is (seq (:invalid at-map))))))))))
