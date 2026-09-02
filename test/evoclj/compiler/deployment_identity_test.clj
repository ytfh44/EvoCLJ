(ns evoclj.compiler.deployment-identity-test
  "I1 tests for CodeImage / Deployment / Execution identity split."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.core :as compiler-core]
            [evoclj.eval.snapshot :as snapshot]
            [evoclj.genome.load :as load]
            [evoclj.genome.types :as types]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import (java.nio.charset StandardCharsets)))

(defn- fixture-root []
  (.toPath (io/file (io/resource "fixtures/genomes/minimal-valid"))))

(defn- fixture-catalog []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- seed-loaded-genome []
  (assoc (load/load-genome (fixture-root))
         :programs [(route-descriptor)]))

(defn- lease [phenotype-id tool-id]
  {:cap/id (random-uuid)
   :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
   :resource {:kind :tool :id tool-id}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at (java.util.Date.)
   :expires-at (java.util.Date. (+ (.getTime (java.util.Date.)) 60000))})

(deftest code-id-vs-deployment-id-split
  (let [seed (seed-loaded-genome)
        compiled (compiler-core/compile-genome seed (fixture-catalog))
        code-id (:code/id compiled)
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider))
        sources (into {}
                      (for [[pid descriptor] (:programs compiled)]
                        [pid
                         (String. (byte-array (get-in seed [:files (:file descriptor) :bytes]))
                                  StandardCharsets/UTF_8)]))
        deps-a {:stores {:sqlite :poison :cas {:root :poison}}
                :providers {:registry reg}
                :capabilities {:leases [(lease code-id :fixture/echo)] :usage (atom {})}
                :bindings [[:skill "debugging" "sha256:1111111111111111111111111111111111111111111111111111111111111111"]]
                :program-sources sources}
        deps-b {:stores {:sqlite :poison :cas {:root :poison}}
                :providers {:registry reg}
                :capabilities {:leases [(lease code-id :fixture/path-resolve)] :usage (atom {})}
                :bindings [[:skill "search" "sha256:2222222222222222222222222222222222222222222222222222222222222222"]]
                :program-sources sources}
        ph-a (phenotype/instantiate compiled deps-a)
        ph-b (phenotype/instantiate compiled deps-b)
        ph-a2 (phenotype/instantiate compiled deps-a)]
    (testing "CompiledGenome exposes :code/id :deployment/id :execution/id (I1)"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" code-id))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:deployment/id compiled)))
      (is (types/execution-id? (:execution/id compiled)))
      (is (types/code-id? (:code/id compiled)) "code/id canonical")
      (is (types/deployment-id? (:deployment/id compiled))))
    (testing "same compiled code instantiated across deployments shares CodeId"
      (is (= (:code/id ph-a) (:code/id ph-b)))
      (is (= code-id (:code/id ph-a))))
    (testing "different runtime leases and bindings yield distinct DeploymentIds"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:deployment/id ph-a)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:deployment/id ph-b)))
      (is (not= (:deployment/id ph-a) (:deployment/id ph-b))))
    (testing "Two Execution with same CodeImage have same code/id but different execution/id (I1 acceptance)"
      (is (= (:code/id ph-a) (:code/id ph-a2)) "same CodeImage => same code/id")
      (is (not= (:execution/id ph-a) (:execution/id ph-a2)) "distinct Execution per activation")
      (is (types/execution-id? (:execution/id ph-a)))
      (is (types/execution-id? (:execution/id ph-a2))))
    (testing "eval snapshot exposes matching code-id and deployment-id helpers"
      (is (= code-id
             (snapshot/code-id (:abi compiled) (:code/genome-id compiled) (:code/resolution-id compiled))))
      (is (= (:deployment/id ph-a)
             (compiler-core/deployment-id code-id
                                         (:bindings deps-a)
                                         (get-in deps-a [:capabilities :leases])))))
    (testing "compile output verifiable: code/id is H(abi, genome, resolution)"
      (let [expected (snapshot/code-id (:abi compiled) (:code/genome-id compiled) (:code/resolution-id compiled))]
        (is (= expected (:code/id compiled)) "code/id verifiable via snapshot helper")))))
