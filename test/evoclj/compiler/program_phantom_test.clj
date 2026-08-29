(ns evoclj.compiler.program-phantom-test
  "PLT3 phantom schema: compile must fail on unregistered keyword :schema/unicorn
  and compiled descriptor must carry resolved Malli schemas (Definition > validation)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.program :as program]
            [evoclj.genome.hash :as hash]
            [evoclj.store.schema :as schema]
            [malli.core :as m])
  (:import (java.nio.charset StandardCharsets)))

(defn- genome-with
  [source]
  {:files {"programs/inline.clj"
           {:digest (hash/text-digest source)
            :bytes (vec (.getBytes source StandardCharsets/UTF_8))
            :kind :clj}}})

(defn- inline-descriptor
  [input-schema output-schema]
  {:program/id :program/inline
   :file "programs/inline.clj"
   :entry 'agent.route/run
   :input-schema input-schema
   :output-schema output-schema})

(deftest phantom-schema-unicorn-fails-compile
  (testing "Definition > validation: phantom keyword :schema/unicorn is unrepresentable"
    (let [g (genome-with "(ns agent.route)
(defn run [x] x)")
          bad (inline-descriptor :schema/unicorn :schema/unicorn)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (program/compile-program-descriptor bad g))
          "compile must fail on phantom :schema/unicorn")
      (try
        (program/compile-program-descriptor bad g)
        (catch clojure.lang.ExceptionInfo e
          (is (= :program/invalid (:error/type (ex-data e))))
          (is (= :unknown-schema (:reason (ex-data e))))
          (is (= :schema/unicorn (:schema (ex-data e))))))))
  (testing "phantom in only one of input/output also fails"
    (let [g (genome-with "(ns agent.route)
(defn run [x] x)")]
      (is (thrown? clojure.lang.ExceptionInfo
                   (program/compile-program-descriptor
                    (inline-descriptor :schema/unicorn :schema/route-input) g)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (program/compile-program-descriptor
                    (inline-descriptor :schema/route-input :schema/unicorn) g)))))
  (testing "registered schemas compile"
    (let [g (genome-with "(ns agent.route)
(defn run [x] x)")
          ok (inline-descriptor :schema/route-input :schema/intent-or-route)
          compiled (program/compile-program-descriptor ok g)]
      (is (map? compiled))
      (is (= :schema/route-input (:input-schema compiled)))
      (is (= :schema/intent-or-route (:output-schema compiled))))))

(deftest compiled-descriptor-carries-resolved-malli-schema
  (testing "compiled descriptor carries resolved Malli schemas, not just keywords"
    (let [g (genome-with "(ns agent.route)
(defn run [x] x)")
          d (program/compile-program-descriptor
             (inline-descriptor :schema/route-input :schema/intent-or-route) g)]
      ;; original keywords still present (backward compat)
      (is (= :schema/route-input (:input-schema d)))
      (is (= :schema/intent-or-route (:output-schema d)))
      ;; resolved schemas present under :schema/input etc
      (is (some? (:schema/input d)) "must carry resolved input Malli schema")
      (is (some? (:schema/output d)) "must carry resolved output Malli schema")
      (is (some? (:input-schema/schema d)) "aliased resolved input")
      (is (some? (:output-schema/schema d)) "aliased resolved output")
      ;; resolved values are valid Malli schemas
      (is (m/schema (:schema/input d)))
      (is (m/schema (:schema/output d)))
      (is (= (:schema/input d) (:input-schema/schema d)))
      (is (= (:schema/output d) (:output-schema/schema d)))
      ;; resolved schemas equal registry values
      (is (= (schema/resolve-schema :schema/route-input) (:schema/input d)))
      (is (= (schema/resolve-schema :schema/intent-or-route) (:schema/output d)))
      ;; resolved schemas actually validate
      (is (m/validate (:schema/input d) {:op :echo :text "hi"}))
      (is (m/validate (:schema/output d) {:action {:intent/type :intent/finish :payload {:value 1}}}))
      ;; phantom would not be valid Malli schema — ensure compiled form is not just keyword
      (is (vector? (:schema/input d)) "resolved schema is Malli vector, not keyword")
      (is (vector? (:schema/output d))))))

(deftest schema-registry-closed
  (testing "registry is closed — only known schemas registered"
    (is (schema/registered? :schema/route-input))
    (is (schema/registered? :schema/intent-or-route))
    (is (not (schema/registered? :schema/unicorn)))
    (is (nil? (schema/resolve-schema :schema/unicorn)))
    (is (thrown? clojure.lang.ExceptionInfo (schema/resolve-schema! :schema/unicorn)))))
