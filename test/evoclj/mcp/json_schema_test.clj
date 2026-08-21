(ns evoclj.mcp.json-schema-test
  "Compatibility battery for evoclj.mcp.json-schema on com.networknt
   json-schema-validator 3.0.0 (the version required by MCP Java SDK 2.0.0,
   whose mcp-json-jackson3 module declares it as a compile dependency).

   Locks the pre-upgrade public contract: conforming values -> true,
   non-conforming -> false, and the DoS budgets (regex count, node/depth
   caps, time ceiling, external $ref denial) still fire as exceptions after
   the SchemaRegistry API migration.
   Schemas use string keys throughout, matching real MCP tool descriptors
   (parsed JSON maps) — the budget/ref guards key off the cheshire-serialized
   JSON text handed to the validator, not pr-str."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.json-schema :as json-schema]))

(deftest accepts-conforming-value
  (testing "sequential-thinking-shaped input schema validates a good payload"
    (is (true? (json-schema/validate
                {"type" "object"
                 "properties" {"thought" {"type" "string"}
                               "thoughtNumber" {"type" "integer"}}
                 "required" ["thought" "thoughtNumber"]}
                {"thought" "a step" "thoughtNumber" 1})))))

(deftest rejects-nonconforming-value
  (testing "wrong type for a required property fails validation"
    (is (false? (json-schema/validate
                 {"type" "object"
                  "properties" {"thoughtNumber" {"type" "integer"}}
                  "required" ["thoughtNumber"]}
                 {"thoughtNumber" "one"})))))

(deftest rejects-missing-required-field
  (testing "missing required property fails validation"
    (is (false? (json-schema/validate
                 {"type" "object"
                  "properties" {"thoughtNumber" {"type" "integer"}}
                  "required" ["thoughtNumber"]}
                 {})))))

(deftest regex-budget-still-enforced
  (testing ">10 \"pattern\" occurrences throw before any validator runs"
    (let [schema {"type" "object"
                  "properties" (into {}
                                     (map (fn [i]
                                            [(str "k" i) {"type" "string" "pattern" "x"}])
                                          (range 11)))}]
      (is (thrown-with-msg? Exception #"regex budget exceeded"
                            (json-schema/validate schema {"k0" "x"})))))
  (testing "keyword :pattern keys count too — budget reads serialized JSON, not pr-str"
    ;; Regression lock: under the old pr-str basis, {:pattern ...} printed
    ;; unquoted and counted 0, letting 11 keyword-key patterns slip through.
    (let [schema {"type" "object"
                  "properties" (into {}
                                     (map (fn [i]
                                            [(str "k" i) {:type "string" :pattern "x"}])
                                          (range 11)))}]
      (is (thrown-with-msg? Exception #"regex budget exceeded"
                            (json-schema/validate schema {"k0" "x"}))))))

(deftest external-ref-still-denied
  (testing "a non-local $ref is refused by our typed guard, extracted from JSON text"
    ;; deny-external-ref! now extracts refs from the serialized JSON text
    ;; (the exact bytes handed to the validator), so an EDN-shaped schema is
    ;; caught by OUR ex-info instead of surfacing as a library SchemaException
    ;; after remote resolution was already attempted.
    (let [ex (try
               (json-schema/validate
                {"$ref" "https://evil.example/schema.json"}
                {})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "expected our typed external-$ref denial")
      (when (some? ex)
        (is (= "external $ref denied" (.getMessage ex)))
        (is (= "https://evil.example/schema.json" (:ref (ex-data ex))))))))

(deftest internal-ref-allowed-and-resolves
  (testing "local $ref (#...) passes the guard and resolves under 2020-12"
    (is (true? (json-schema/validate
                {"type" "object"
                 "$defs" {"pos" {"type" "integer" "minimum" 1}}
                 "properties" {"n" {"$ref" "#/$defs/pos"}}
                 "required" ["n"]}
                {"n" 3})))))

;; ---- budget boundary regressions (reviewer-verified shapes) ---------------
;; depth: a {"type" "integer"} leaf scores 2 and each {"type" "array"
;; "items" inner} wrapper adds exactly 1, so `wraps` -> depth 2+wraps.
;; nodes: a wide object with n leaf properties scores 3+2n.

(defn- nested-array-schema [wraps]
  (reduce (fn [inner _] {"type" "array" "items" inner})
          {"type" "integer"}
          (range wraps)))

(defn- wide-object-schema [prop-count]
  {"type" "object"
   "properties" (into {}
                      (map (fn [i] [(str "p" i) {"type" "string"}])
                           (range prop-count)))})

(deftest depth-budget-fires-past-20
  (testing "nesting depth 21 throws the depth budget; depth 20 validates"
    (is (thrown-with-msg? Exception #"schema depth budget exceeded"
                          (json-schema/validate (nested-array-schema 19) [])))
    (is (true? (json-schema/validate (nested-array-schema 18) [])))))

(deftest node-count-budget-fires-past-1000
  (testing "2201 schema nodes throw the node-count budget; 801 nodes validate"
    (is (thrown-with-msg? Exception #"schema node-count budget exceeded"
                          (json-schema/validate (wide-object-schema 1099) {})))
    (is (true? (json-schema/validate (wide-object-schema 399) {})))))

(deftest time-budget-fires-on-expensive-validation
  (testing "an oversized instance whose full validation exceeds 200ms throws the time budget"
    ;; 500k elements: ~3x margin over the 200ms threshold across machine
    ;; speeds (150k measured 259ms on one dev box but <200ms on another).
    (is (thrown-with-msg? Exception #"validation time budget exceeded"
                          (json-schema/validate
                           {"type" "array" "items" {"type" "integer"}}
                           (vec (repeat 500000 "not-an-integer")))))))
