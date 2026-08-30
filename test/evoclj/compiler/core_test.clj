(ns evoclj.compiler.core-test
  "Tests for compile-genome orchestration and the Phenotype identity
  (component).

  compile-genome is orchestration only: it composes the focused modules
  (load-genome / validate-manifest / resolve-models / compile-topology /
  compile-program-descriptor) into a pure CompiledGenome and derives
  the Phenotype ID. The Phenotype ID formula is normative:

    phenotype-id = SHA256(kernel-abi || genome-id || resolution-id)

  where kernel-abi is the manifest's :abi map serialized canonically
  (sorted keys), genome-id and resolution-id are the canonical
  \"sha256:<64 hex>\" strings, and the digest uses the same UTF-8 /
  CRLF-normalized hashing as evoclj.genome.hash with the same
  \"sha256:<hex>\" format. Changing only the Resolution (a different
  provider catalog) therefore changes the Phenotype ID but never the
  Genome ID (Global Constraints 1, 2, 6).

  The program registry follows component choice (a): an in-memory
  descriptor list that rides on the loaded-genome value under
  :programs (never a manifest change). The compiled value is pure,
  fully serializable EDN data (Global Constraint 22): program
  descriptors carry :source/digest references and never the source
  bytes, and no :files payload or byte array appears in the compiled
  value.

  The Milestone 2 exit test compiles the seed fixture 100 times in one
  process and asserts identical semantic output and IDs."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.core :as core]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.types :as types])
  (:import (java.nio.charset StandardCharsets)))

;; --- fixture and helper functions -----------------------------------------

(def ^:private contract-keys
  "The normative CompiledGenome key set (Detailed Public Data
  Contracts)."
  #{:compiled/genome-id :compiled/resolution-id :compiled/phenotype-id
    :abi :manifest :topology :effects :programs :requested-capabilities
    :resolution})

(defn- fixture-root
  "The bundle directory for a named fixture under test/fixtures/genomes."
  [name]
  (.toPath (io/file (io/resource (str "fixtures/genomes/" name)))))

(defn- fixture-catalog
  "The on-disk provider catalog fixture."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor
  "The seed route program descriptor (component)."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- seed-loaded-genome
  "The real minimal-valid bundle loaded from disk with the in-memory
  program registry attached (component choice (a))."
  []
  (assoc (load/load-genome (fixture-root "minimal-valid"))
         :programs [(route-descriptor)]))

(defn- compiled
  "Compile the seed fixture once."
  []
  (core/compile-genome (seed-loaded-genome) (fixture-catalog)))

(defn- compile-error
  "The ExceptionInfo thrown by compile-genome, or nil."
  [loaded-genome provider-catalog]
  (try (core/compile-genome loaded-genome provider-catalog)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- no-byte-payloads?
  "True when v contains no Java byte array and no :bytes key anywhere
  in its tree."
  [v]
  (cond
    (bytes? v) false
    (map? v) (and (not (contains? v :bytes))
                  (every? (fn [[k val]]
                            (and (no-byte-payloads? k) (no-byte-payloads? val)))
                          v))
    (vector? v) (every? no-byte-payloads? v)
    (seq? v) (every? no-byte-payloads? v)
    (set? v) (every? no-byte-payloads? v)
    :else true))

(defn- inline-topology-edn
  "An :emit-only topology that references no programs."
  []
  "{:graph/id :graph/main :entry :node/finish
    :nodes {:node/finish {:node/type :emit}}}")

(defn- inline-loaded-genome
  "A hand-built loaded-genome whose topology references no programs, so
  a two-argument compile-genome needs no program registry."
  []
  (let [topology-src (inline-topology-edn)
        models-src "{:models {}}"]
    {:genome/id (hash/tree-digest [])
     :manifest {:genome/format 1
                :agent/id :main
                :agent/entry :graph/main
                :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                :modules {:topology "topology.edn"
                          :models "models.edn"
                          :memory "memory.edn"
                          :evolution "evolution.edn"}
                :capabilities/requested #{}
                :evolution {:max-risk :behavioral
                            :mutable #{:parameters :prompts :skills :programs}}
                :metadata {}}
     :files {"topology.edn"
             {:digest (hash/text-digest topology-src)
              :bytes (vec (.getBytes topology-src StandardCharsets/UTF_8))
              :kind :edn}
             "models.edn"
             {:digest (hash/text-digest models-src)
              :bytes (vec (.getBytes models-src StandardCharsets/UTF_8))
              :kind :edn}}}))

;; --- full fixture compile -------------------------------------------------

(deftest full-seed-fixture-compiles
  (let [g (seed-loaded-genome)
        c (core/compile-genome g (fixture-catalog))]
    (testing "the compiled key set is exactly the normative contract"
      (is (= contract-keys (set (keys c)))))
    (testing "all three ids are canonical content-addressed ids"
      (is (types/genome-id? (:compiled/genome-id c)))
      (is (types/resolution-id? (:compiled/resolution-id c)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:compiled/phenotype-id c))))
    (testing "the genome id is the loaded genome's content address"
      (is (= (:genome/id g) (:compiled/genome-id c))))
    (testing ":abi passes through from the manifest"
      (is (= {:kernel 1 :genome 1 :intent 1 :tool 1} (:abi c)))
      (is (= (:abi (:manifest g)) (:abi c))))
    (testing ":manifest passes through unchanged"
      (is (= (:manifest g) (:manifest c))))
    (testing "the topology is the compiled adjacency IR"
      (is (= :node/planner (:entry (:topology c))))
      (is (= {:node/planner [:node/router]
              :node/router []
              :node/finish []}
             (:adjacency (:topology c)))))
    (testing "requested capabilities pass through"
      (is (= #{:model/call} (:requested-capabilities c))))
    (testing "the route program compiles to a digest-only descriptor"
      (let [d (get-in c [:programs :program/route])]
        (is (= :program/route (:program/id d)))
        (is (= "programs/route.clj" (:file d)))
        (is (= 'agent.route/run (:entry d)))
        (is (= (get-in g [:files "programs/route.clj" :digest]) (:source/digest d)))
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:source/digest d)))))
    (testing "the resolution resolved the seed model"
      (is (= {:alias :reasoning/high
              :provider :fixture
              :provider-model "fixture-model-v1"
              :adapter-version "1"}
             (get-in c [:resolution :models :planner]))))
    (testing "the compiled value is pure serializable EDN"
      (is (= c (edn/read-string (pr-str c)))))))

;; --- resolution change moves only the Phenotype ID ------------------------

(deftest changing-only-the-resolution-changes-phenotype-not-genome
  (let [catalog-v1 (fixture-catalog)
        catalog-v2 (assoc-in (fixture-catalog)
                             [:reasoning/high :provider-model]
                             "fixture-model-v2")
        c1 (core/compile-genome (seed-loaded-genome) catalog-v1)
        c2 (core/compile-genome (seed-loaded-genome) catalog-v2)]
    (testing "the genome id is untouched by resolution changes"
      (is (= (:compiled/genome-id c1) (:compiled/genome-id c2))))
    (testing "the resolution id changes"
      (is (not= (:compiled/resolution-id c1) (:compiled/resolution-id c2))))
    (testing "the phenotype id changes with the resolution"
      (is (not= (:compiled/phenotype-id c1) (:compiled/phenotype-id c2))))
    (testing "the same catalog compiles to identical output and ids"
      (let [c3 (core/compile-genome (seed-loaded-genome) catalog-v1)]
        (is (= c1 c3))
        (is (= (:compiled/phenotype-id c1) (:compiled/phenotype-id c3)))))))

;; --- EDN round-trip without source bytes ----------------------------------

(deftest compiled-genome-round-trips-through-edn-without-source-bytes
  (let [c (compiled)
        rt (edn/read-string (pr-str c))]
    (testing "pr-str / clojure.edn round-trip is the identity (Global Constraint 22)"
      (is (= c rt)))
    (testing "program descriptors carry digest references, never source bytes"
      (doseq [[pid d] (:programs c)]
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:source/digest d)) (str pid))
        (is (not (contains? d :bytes)) (str pid))))
    (testing "no :files payload or byte array appears in the compiled value"
      (is (not (contains? c :files)))
      (is (no-byte-payloads? c)))))

;; --- Milestone 2 exit test: 100 identical compilations --------------------

(deftest milestone-2-exit-test-100-compilations-are-identical
  (let [catalog (fixture-catalog)
        first-compiled (core/compile-genome (seed-loaded-genome) catalog)
        rest-compiled (vec (repeatedly 99
                                       #(core/compile-genome (seed-loaded-genome) catalog)))]
    (testing "all 100 compilations are semantically identical"
      (is (every? #(= first-compiled %) rest-compiled)))
    (testing "all three ids are identical across iterations"
      (is (every? #(= (:compiled/genome-id first-compiled)
                      (:compiled/genome-id %)) rest-compiled))
      (is (every? #(= (:compiled/resolution-id first-compiled)
                      (:compiled/resolution-id %)) rest-compiled))
      (is (every? #(= (:compiled/phenotype-id first-compiled)
                      (:compiled/phenotype-id %)) rest-compiled)))))

;; --- orchestration boundaries ---------------------------------------------

(deftest two-argument-compile-works-without-program-registry
  (let [c (core/compile-genome (inline-loaded-genome) (fixture-catalog))]
    (is (= {} (:programs c)))
    (is (= :node/finish (:entry (:topology c))))
    (is (types/genome-id? (:compiled/genome-id c)))
    (is (types/resolution-id? (:compiled/resolution-id c)))
    (is (= c (edn/read-string (pr-str c))))))

(deftest topology-referenced-program-without-registry-fails-closed
  (let [g (load/load-genome (fixture-root "minimal-valid"))
        e (compile-error g (fixture-catalog))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :compiler/program-unresolved (:error/type (ex-data e))))
    (is (= :program/route (:program-id (ex-data e))))))

(deftest duplicate-program-ids-in-registry-rejected
  (let [g (assoc (load/load-genome (fixture-root "minimal-valid"))
                 :programs [(route-descriptor) (route-descriptor)])
        e (compile-error g (fixture-catalog))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :compiler/invalid (:error/type (ex-data e))))
    (is (= :duplicate-program-id (:reason (ex-data e))))))

(deftest malformed-program-registry-rejected
  (let [g (assoc (load/load-genome (fixture-root "minimal-valid"))
                 :programs :not-a-list)
        e (compile-error g (fixture-catalog))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :compiler/invalid (:error/type (ex-data e))))
    (is (= :invalid-program-registry (:reason (ex-data e))))))

(deftest invalid-loaded-genome-rejected
  (doseq [bad [nil
               "not-a-genome"
               {:genome/id "sha256:nope" :manifest {} :files {}}
               {:genome/id (hash/tree-digest []) :manifest {} :files "nope"}]]
    (let [e (compile-error bad (fixture-catalog))]
      (is (instance? clojure.lang.ExceptionInfo e) (pr-str bad))
      (is (= :compiler/invalid (:error/type (ex-data e))) (pr-str bad)))))
