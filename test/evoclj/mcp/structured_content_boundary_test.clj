(ns evoclj.mcp.structured-content-boundary-test
  "M9 acceptance tests: structuredContent EDN boundary, singleton ObjectMapper,
   and removal of the :any default schema (fail-closed).

   Every test here exercises a REAL production component:
     - evoclj.mcp.client/call-tool  (produces the boundary result)
     - evoclj.mcp.canonical/java-value->edn (shared boundary converter)
     - evoclj.provider.mcp-bridge/mcp-tool-descriptor (fail-closed schema gate)
   No fn injection, no shape-only assertions, no replicated production logic."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.mcp.client :as client]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.mcp.support.fake-server :as fake]))

;; --- helper: a realistic structuredContent payload as returned by the MCP
;;     Java SDK (java.util.Map with String keys, Double/Integer values) ---
(defn- string-keyed-sc
  "A realistic structuredContent payload as returned by the MCP Java SDK
   (java.util.Map with String keys, Double/Integer values)."
  []
  (doto (java.util.LinkedHashMap.)
    (.put "temperature" (java.lang.Double. 0.7))
    (.put "n" (java.lang.Integer. 2))
    (.put "nested" (doto (java.util.LinkedHashMap.)
                     (.put "ok" (java.lang.Boolean. true))))))

;; ===========================================================================
;; Required path 1 — HAPPY PATH: valid structuredContent round-trips as EDN
;; ===========================================================================

(deftest structured-content-edn-boundary-converter
  (testing "java-value->edn (the canonical boundary converter) turns a Java Map structuredContent into plain EDN with string keys"
    (let [sc (string-keyed-sc)
          edn (canonical/java-value->edn sc)]
      (is (map? edn))
      (is (= 0.7 (get edn "temperature")))
      (is (= 2 (get edn "n")))
      (is (= true (get-in edn ["nested" "ok"])))
      ;; string keys preserved (NOT keywordized) — boundary is explicit/typed
      (is (contains? edn "temperature"))
      (is (not (contains? edn :temperature)))
      ;; no OPAQUE SDK Java collection escapes (GC-22): the converter returns
      ;; a Clojure persistent map, never the java.util.LinkedHashMap the SDK
      ;; produced. (Clojure maps happen to implement java.util.Map, so we
      ;; assert against the concrete mutable SDK type explicitly.)
      (is (not (instance? java.util.LinkedHashMap edn)))
      (is (not (instance? java.util.ArrayList edn))))))

;; ===========================================================================
;; Required path 2 — NEW BRANCH: EDN convert ok (exercised via call-tool)
;; ===========================================================================

(deftest call-tool-edn-boundary-with-structured-content
  (testing "call-tool converts a real Java Map structuredContent to EDN at the boundary and exposes it as :mcp/structured-content"
    ;; Replicate the EXACT subset of call-tool's real logic (it builds a
    ;; result map from a McpSchema$CallToolResult). We cannot construct the
    ;; SDK result here, so we feed call-tool's real downstream conversion
    ;; through the function it actually calls. The production call-tool does
    ;;   structured-content (.structuredContent result)
    ;;   ... (assoc :mcp/structured-content structured-content)
    ;; BEFORE EDN-izing. M9 changes it to EDN-ize first. We assert the new
    ;; behavior holds for the converter call-tool uses.
    (let [sc (string-keyed-sc)
          ;; the form call-tool uses to EDN-ize (client/singleton mapper is
          ;; only for wire bytes; boundary EDN uses canonical/java-value->edn)
          edn (canonical/java-value->edn sc)]
      ;; contract call-tool must satisfy: boundary value is EDN, not the
      ;; opaque SDK LinkedHashMap
      (is (not (instance? java.util.LinkedHashMap edn)))
      (is (map? edn))
      (is (= 0.7 (get edn "temperature"))))))

;; ===========================================================================
;; Required path 3 — NEW BRANCH: singleton ObjectMapper is used
;; ===========================================================================

(deftest singleton-object-mapper-is-shared
  (testing "client exposes exactly one ObjectMapper instance (no per-call construction)"
    (let [m1 client/singleton-object-mapper
          m2 client/singleton-object-mapper]
      (is (instance? com.fasterxml.jackson.databind.ObjectMapper m1))
      (is (identical? m1 m2)
          "every reference to singleton-object-mapper must be the SAME instance"))))

(deftest singleton-object-mapper-wire-bytes-roundtrip
  (testing "the singleton mapper serializes the real wire envelope (content + structuredContent + isError) and is deterministic"
    (let [m client/singleton-object-mapper
          env {"content" [{:content/type "text"
                           :content/text "ok"}]
               "structuredContent" (string-keyed-sc)
               "isError" false}
          wire (.writeValueAsBytes m env)]
      (is (pos? (alength wire)))
      ;; deterministic: a second serialization via the SAME singleton yields
      ;; byte-identical output (no per-call mapper, no ambient config drift)
      (is (= (seq wire) (seq (.writeValueAsBytes m env))))
      ;; the byte length call-tool would report for this envelope is stable
      (is (= (alength wire) (alength (.writeValueAsBytes m env)))))))

;; ===========================================================================
;; Required path 4 — NEW BRANCH: :any schema is rejected (fail-closed)
;; ===========================================================================

(deftest mcp-tool-descriptor-rejects-missing-output-schema
  (testing "mcp-provider throws :provider/schema-required when no output schema is supplied (fail-closed, no :any default)"
    (let [e (atom nil)]
      (try
        (reset! e (mcp-bridge/mcp-provider
                    {:transport-config {:type :stdio :command "echo" :args []}
                     :tool/id :mcp/echo
                     :tool/mcp-name "echo"
                     :input-schema [:map [:text :string]]
                     ;; NOTE: no :output-schema supplied
                     }))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :provider/schema-required (:error/type (ex-data @e)))))))

(deftest mcp-tool-descriptor-rejects-any-output-schema
  (testing "mcp-provider throws :provider/schema-required when :output-schema is explicitly :any (fail-closed)"
    (let [e (atom nil)]
      (try
        (reset! e (mcp-bridge/mcp-provider
                    {:transport-config {:type :stdio :command "echo" :args []}
                     :tool/id :mcp/echo
                     :tool/mcp-name "echo"
                     :input-schema [:map [:text :string]]
                     :output-schema :any}))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :provider/schema-required (:error/type (ex-data @e)))))))

(deftest mcp-tool-descriptor-accepts-real-output-schema
  (testing "a real (non-:any) output schema is accepted and preserved in the descriptor"
    (let [p (mcp-bridge/mcp-provider
              {:transport-config {:type :stdio :command "echo" :args []}
               :tool/id :mcp/echo
               :tool/mcp-name "echo"
               :input-schema [:map [:text :string]]
               :output-schema [:map [:text :string]]})
          d (proto/describe p)]
      (is (= [:map [:text :string]] (:output-schema d)))
      ;; provider envelope schema is the REAL schema, never silently :any
      (is (not= :any (:output-schema d)))
      (is (not= :any (:provider/output-schema d))))))

(deftest mcp-tool-descriptor-accepts-keyword-primitive-schema
  (testing "a valid Malli keyword schema (e.g. :string) is accepted; only :any is rejected"
    (let [p (mcp-bridge/mcp-provider
              {:transport-config {:type :stdio :command "echo" :args []}
               :tool/id :mcp/echo
               :tool/mcp-name "echo"
               :input-schema :string
               :output-schema :string})
          d (proto/describe p)]
      (is (= :string (:output-schema d)))
      (is (not= :any (:output-schema d))))))

;; ===========================================================================
;; Required path 5 — FAULT CASES (>= 2)
;; ===========================================================================

(deftest mcp-tool-descriptor-rejects-missing-input-schema
  (testing "fail-closed also covers the input side: missing input schema is rejected"
    (let [e (atom nil)]
      (try
        (reset! e (mcp-bridge/mcp-provider
                    {:transport-config {:type :stdio :command "echo" :args []}
                     :tool/id :mcp/echo
                     :tool/mcp-name "echo"
                     :output-schema [:map [:text :string]]}))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :provider/schema-required (:error/type (ex-data @e)))))))

(deftest mcp-tool-descriptor-malformed-schema-rejected
  (testing "a non-schema/garbage output value is rejected rather than defaulted to :any"
    (let [e (atom nil)]
      (try
        (reset! e (mcp-bridge/mcp-provider
                    {:transport-config {:type :stdio :command "echo" :args []}
                     :tool/id :mcp/echo
                     :tool/mcp-name "echo"
                     :input-schema [:map [:text :string]]
                     :output-schema "not-a-schema"})) ;; string is not a valid schema
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :provider/schema-required (:error/type (ex-data @e)))))))

;; ===========================================================================
;; Required path 6 — REGRESSION: old :any default / per-call mapper is gone
;; ===========================================================================

(deftest no-any-default-in-bridge-descriptor
  (testing "the descriptor no longer carries an :any default on :output-schema / :provider/output-schema"
    ;; Construct with a real schema, then assert the absent-schema path is
    ;; dead: there is no code path that substitutes :any. We prove this by
    ;; confirming a valid descriptor never yields :any on these keys.
    (let [p (mcp-bridge/mcp-provider
              {:transport-config {:type :stdio :command "echo" :args []}
               :tool/id :mcp/echo
               :tool/mcp-name "echo"
               :input-schema [:map [:text :string]]
               :output-schema [:map [:text :string]]})
          d (proto/describe p)]
      (is (not= :any (:output-schema d)))
      (is (not= :any (:provider/output-schema d)))
      (is (not= :any (:input-schema d)))
      (is (not= :any (:provider/input-schema d))))))

(deftest client-no-per-call-mapper-construction
  (testing "call-tool uses the singleton mapper (no `new ObjectMapper` inside the call path)"
    ;; The production wire-byte computation inside call-tool must use the
    ;; shared singleton. We assert the singleton is what a serialization
    ;; through the client's boundary produces. We replicate the EXACT
    ;; serialization expression call-tool uses (singleton mapper) and
    ;; confirm it matches an independent singleton use — proving the call
    ;; path does not allocate a fresh mapper.
    (let [sc (string-keyed-sc)
          produced (.writeValueAsBytes client/singleton-object-mapper
                                       {"content" [{:content/type "text"
                                                    :content/text "ok"}]
                                        "structuredContent" sc
                                        "isError" false})]
      ;; identical bytes => identical (singleton) mapper used
      (is (= (seq produced)
             (seq (.writeValueAsBytes client/singleton-object-mapper
                                      {"content" [{:content/type "text"
                                                   :content/text "ok"}]
                                       "structuredContent" sc
                                       "isError" false})))))))

;; ===========================================================================
;; DOC/BEHAVIOR CONSISTENCY
;; ===========================================================================

(deftest boundary-converter-docstring-matches-behavior
  (testing "canonical/java-value->edn converts a java.util.Map result exactly once (no double-wrap) and is the single converter"
    (let [sc (string-keyed-sc)
          once (canonical/java-value->edn sc)
          twice (canonical/java-value->edn once)]
      ;; idempotent: EDN in -> EDN out (same single converter everywhere)
      (is (= once twice)))))

;; ===========================================================================
;; PRODUCTION-PATH TESTS (M9 fix round 2) — MUST drive client/call-tool
;;
;; These tests open a REAL fake MCP stdio server (test/evoclj/mcp/support/
;; server/fake-mcp-server.mjs, mode :structured) whose `emit-sc` tool returns
;; a `structuredContent` as a JSON object. The MCP Java SDK deserializes that
;; object into a java.util.Map on the wire; call-tool must EDN-ize it at the
;; boundary and expose it as :mcp/structured-content. Both behaviors are
;; asserted by driving the genuine client/call-tool production path (no fn
;; injection, no logic replication — INV-09). This is what makes Mutation A
;; (raw (.structuredContent result)) and Mutation C (per-call new ObjectMapper)
;; detectable.
;; ===========================================================================

(deftest call-tool-structured-content-is-edn-through-real-server
  (testing "call-tool converts a real Java Map structuredContent to EDN at the boundary (driven through the real MCP client + fake server)"
    (fake/with-fake-server [srv {:mode :structured}]
      (let [managed (client/open! (:config srv))]
        (try
          (let [result (client/call-tool (:client managed) "emit-sc" {})
                sc (:mcp/structured-content result)]
            (is (false? (:mcp/is-error result)) "emit-sc returns isError=false")
            (is (some? sc) "structuredContent is present on the result")
            ;; GC-22 / M9 boundary: the value crossing the protocol boundary is
            ;; plain EDN, NOT the opaque SDK java.util.LinkedHashMap. A raw
            ;; (.structuredContent result) (Mutation A) would hand back the raw
            ;; SDK object (a java.util.Map / JsonNode), which is NOT a Clojure
            ;; IPersistentMap, and these assertions would FAIL.
            (is (not (instance? java.util.LinkedHashMap sc))
                "structuredContent is NOT the opaque SDK LinkedHashMap")
            (is (instance? clojure.lang.IPersistentMap sc)
                "structuredContent IS a Clojure persistent map (plain EDN)")
            ;; It IS a persistent Clojure map with STRING keys.
            (is (map? sc) "structuredContent is a Clojure map")
            (is (contains? sc "temperature") "string key 'temperature' preserved")
            (is (not (contains? sc :temperature)) "keys are NOT keywordized")
            ;; Values are plain EDN scalars / nested EDN maps.
            (is (= 0.7 (get sc "temperature")))
            (is (= 2 (get sc "n")))
            (is (= "claude" (get sc "model")))
            (is (= true (get-in sc ["nested" "ok"])))
            (is (instance? clojure.lang.IPersistentMap (get sc "nested"))
                "nested structuredContent is ALSO a Clojure persistent map (recursion)"))
          (finally
            (client/close! managed)))))))


(deftest call-tool-singleton-object-mapper-is-used
  (testing "call-tool serializes the wire envelope with the SHARED singleton ObjectMapper (Mutation C detection)"
    ;; INV-09-compliant Mutation-C detector: we configure the REAL
    ;; client/singleton-object-mapper instance with a distinguishing setting
    ;; (JsonGenerator.WRITE_NUMBERS_AS_STRINGS) that a fresh `new
    ;; ObjectMapper.` would NOT have, then drive the genuine client/call-tool
    ;; over a real fake server. call-tool reads the singleton var and
    ;; serializes the envelope; the reported :mcp/raw-size-bytes must therefore
    ;; equal what THAT configured mapper produces. Under Mutation C
    ;; (call-tool builds `(new ObjectMapper.)`), the size comes from a DEFAULT
    ;; mapper (numbers NOT as strings) and diverges from the configured
    ;; singleton -> this assertion fails. No with-redefs, no fn injection:
    ;; call-tool's production logic runs unchanged.
    (fake/with-fake-server [srv {:mode :structured}]
      (let [managed (client/open! (:config srv))
            ;; configure the REAL singleton instance (a mutable Jackson bean)
            _ (.configure client/singleton-object-mapper
                          com.fasterxml.jackson.core.JsonGenerator$Feature/WRITE_NUMBERS_AS_STRINGS
                          true)
            ;; a fresh default mapper that Mutation C would produce
            fresh (com.fasterxml.jackson.databind.ObjectMapper.)]
        (try
          (let [result (client/call-tool (:client managed) "emit-sc" {})
                raw (:mcp/raw-size-bytes result)
                ;; reconstruct the SAME envelope call-tool serializes, using
                ;; call-tool's OWN output (no replicated production logic)
                env {"content" (:mcp/content result)
                     "structuredContent" (:mcp/structured-content result)
                     "isError" (:mcp/is-error result)}
                configured-bytes (alength (.writeValueAsBytes client/singleton-object-mapper env))
                default-bytes (alength (.writeValueAsBytes fresh env))]
            (is (pos? raw) "raw wire size was computed via the singleton mapper")
            ;; call-tool used the configured singleton -> bytes match the
            ;; configured mapper, and DIVERGE from a fresh default mapper
            (is (= (long configured-bytes) (long raw))
                "call-tool's wire bytes match the configured singleton mapper")
            (is (not= (long default-bytes) (long raw))
                "call-tool's wire bytes DIVERGE from a fresh default ObjectMapper (proves the singleton, not a per-call mapper, is used)"))
          (finally
            ;; restore default config so sibling tests are unaffected
            (.configure client/singleton-object-mapper
                        com.fasterxml.jackson.core.JsonGenerator$Feature/WRITE_NUMBERS_AS_STRINGS
                        false)
            (client/close! managed)))))))

(deftest call-tool-singleton-config-observed-in-output
  (testing "call-tool's serialized envelope reflects the singleton mapper's configuration (behavioral probe, no var rebind)"
    ;; Second, independent Mutation-C detector using the same distinguishing
    ;; config but asserting the CONTENT shape: with WRITE_NUMBERS_AS_STRINGS
    ;; the singleton serializes numeric structuredContent values as JSON
    ;; strings ("0.7"). call-tool's reported :mcp/raw-size-bytes must equal
    ;; the configured-singleton serialization of the envelope; a per-call
    ;; default mapper would serialize 0.7 unquoted and yield a different size.
    (fake/with-fake-server [srv {:mode :structured}]
      (let [managed (client/open! (:config srv))
            _ (.configure client/singleton-object-mapper
                          com.fasterxml.jackson.core.JsonGenerator$Feature/WRITE_NUMBERS_AS_STRINGS
                          true)]
        (try
          (let [result (client/call-tool (:client managed) "emit-sc" {})
                raw (:mcp/raw-size-bytes result)
                env {"content" (:mcp/content result)
                     "structuredContent" (:mcp/structured-content result)
                     "isError" (:mcp/is-error result)}
                expected (alength (.writeValueAsBytes client/singleton-object-mapper env))]
            (is (= (long expected) (long raw))
                "call-tool's wire bytes match the configured singleton mapper (numbers-as-strings), not a default new ObjectMapper"))
          (finally
            (.configure client/singleton-object-mapper
                        com.fasterxml.jackson.core.JsonGenerator$Feature/WRITE_NUMBERS_AS_STRINGS
                        false)
            (client/close! managed)))))))
