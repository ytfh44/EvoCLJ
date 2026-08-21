(ns evoclj.genome.schema-test
  "Tests for the v1 Genome manifest schemas (component).

  The manifest is a pure EDN contract evaluated at the trust boundary:
  the schema is a closed map, so unknown top-level keys are rejected
  unless they live inside :metadata; validation never coerces values —
  a valid manifest is returned unchanged; and any failure throws
  :genome/schema-invalid carrying a fully serializable Malli
  explanation (safe for pr-str / clojure.edn read-string)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.kernel.error :as err]
            [evoclj.genome.schema :as schema]))

(def ^:private valid-manifest
  {:genome/format 1
   :agent/id :main
   :agent/entry :graph/main
   :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
   :modules {:topology "topology.edn"
             :models "models.edn"
             :memory "memory.edn"
             :evolution "evolution.edn"}
   :capabilities/requested #{:model/call}
   :evolution {:max-risk :behavioral
               :mutable #{:parameters :prompts :skills :programs}}
   :metadata {:name "seed-agent"
              :description "minimal fixture"}})

(defn- manifest-error
  "Return the ExceptionInfo thrown by validate-manifest, or nil when the
  manifest validates."
  [x]
  (try (schema/validate-manifest x)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- is-schema-invalid [x]
  (let [e (manifest-error x)]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :genome/schema-invalid (:error/type (ex-data e))))))

(deftest valid-manifest-accepted-unchanged
  (testing "the required v1 shape validates and is returned unchanged (no coercion)"
    (is (identical? valid-manifest (schema/validate-manifest valid-manifest)))))

(deftest minimal-valid-fixture-validates
  (testing "the on-disk minimal fixture matches the required v1 shape"
    (let [m (edn/read-string
             (slurp (io/resource "fixtures/genomes/minimal-valid/manifest.edn")))]
      (is (= m (schema/validate-manifest m)))
      (is (= valid-manifest m)))))

(deftest rejects-missing-genome-format
  (is-schema-invalid (dissoc valid-manifest :genome/format)))

(deftest rejects-wrong-abi-value-type
  (testing "an ABI slot must be a positive integer, not a string"
    (is-schema-invalid (assoc-in valid-manifest [:abi :kernel] "one")))
  (testing "the :abi value itself must be a map"
    (is-schema-invalid (assoc valid-manifest :abi :nope)))
  (testing "ABI slots are closed: an undeclared slot is rejected"
    (is-schema-invalid (assoc-in valid-manifest [:abi :filesystem] 1))))

(deftest rejects-absolute-or-traversal-module-path
  (testing "unix absolute path"
    (is-schema-invalid (assoc-in valid-manifest [:modules :topology] "/tmp/evil.edn")))
  (testing "windows absolute path"
    (is-schema-invalid (assoc-in valid-manifest [:modules :models] "C:\\tmp\\evil.edn")))
  (testing "windows backslash traversal"
    (is-schema-invalid (assoc-in valid-manifest [:modules :memory] "..\\..\\secret.edn")))
  (testing "relative traversal"
    (is-schema-invalid (assoc-in valid-manifest [:modules :evolution] "a/../../b.edn")))
  (testing "module slots are closed: an undeclared module is rejected"
    (is-schema-invalid (assoc-in valid-manifest [:modules :skills] "skills.edn"))))

(deftest rejects-unknown-risk-keyword
  (is-schema-invalid (assoc-in valid-manifest [:evolution :max-risk] :catastrophic)))

(deftest rejects-unknown-top-level-keys
  (testing "the manifest is a closed map at the trust boundary"
    (is-schema-invalid (assoc valid-manifest :rogue/key 42))))

(deftest allows-arbitrary-keys-inside-metadata
  (testing "unknown keys are permitted inside :metadata only"
    (let [m (assoc-in valid-manifest [:metadata :custom] {:anything #{1 2} :nested [3 4]})]
      (is (identical? m (schema/validate-manifest m))))
    (testing ":metadata must still be a map"
      (is-schema-invalid (assoc valid-manifest :metadata "nope")))))

(deftest invalid-manifest-fixture-is-rejected
  (let [m (edn/read-string
           (slurp (io/resource "fixtures/genomes/invalid-manifest/manifest.edn")))]
    (is-schema-invalid m)))

(deftest explanation-round-trips-through-edn
  (testing "the Malli explanation in the error data is fully serializable"
    (let [e (manifest-error (assoc-in valid-manifest [:abi :kernel] "one"))
          d (err/error-data e)]
      (is (= :genome/schema-invalid (:error/type d)))
      (is (contains? (:error/data d) :explanation))
      (is (seq (:errors (:explanation (:error/data d)))))
      (is (= d (edn/read-string (pr-str d)))))))
