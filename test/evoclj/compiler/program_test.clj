(ns evoclj.compiler.program-test
  "Tests for static discovery and validation of evolvable SCI programs
  (Task 2.3).

  Genome programs are DECLARED via descriptors, never inferred from
  arbitrary source files; this task keeps `compile-program-descriptor`
  a pure function over a descriptor map plus a loaded Genome (choice
  (a) in the task brief): the descriptor list itself lives in
  memory/tests and the seed Genome's program registry wiring arrives in
  Task 3.4/6.x, so the Task 1.2 closed-map manifest schema stays
  untouched. compile-program-descriptor validates file existence, path,
  entry symbol, and source readability, then performs compile-policy
  inspection (best-effort; the SCI sandbox remains the final
  enforcement layer): load-file, eval, require/use of undeclared host
  namespaces, Java class literals (e.g. (System/getenv ...)), host
  interop special forms, ns :import clauses, and #= reader-eval forms
  are all rejected where detectable.

  Source is parsed with rewrite-clj (structure only — nothing is
  evaluated) and the returned ProgramDescriptor is pure serializable
  EDN data (Global Constraint 22): :source/digest reuses the loaded
  Genome's canonical CRLF-normalized text digest of the program file
  (evoclj.genome.hash), :entry is the declared entry symbol, and the
  declared schemas pass through."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.program :as program]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load])
  (:import (java.nio.charset StandardCharsets)))

;; --- fixture and helper functions -----------------------------------------

(defn- fixture-root
  "The bundle directory for a named fixture under test/fixtures/genomes."
  [name]
  (.toPath (io/file (io/resource (str "fixtures/genomes/" name)))))

(defn- minimal-valid-genome
  "The real loaded minimal-valid bundle, which now contains
  programs/route.clj."
  []
  (load/load-genome (fixture-root "minimal-valid")))

(defn- route-descriptor
  "The seed route program descriptor from the Task 2.3 example."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- genome-with
  "A minimal loaded-Genome value whose :files holds one inline .clj
  program, shaped like evoclj.genome.load's payloads."
  [source]
  {:files {"programs/inline.clj"
           {:digest (hash/text-digest source)
            :bytes (vec (.getBytes source StandardCharsets/UTF_8))
            :kind :clj}}})

(defn- inline-descriptor
  "A descriptor pointing at the inline program in `genome-with`."
  []
  {:program/id :program/inline
   :file "programs/inline.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- compile-error
  "The ExceptionInfo thrown by compile-program-descriptor, or nil."
  [descriptor genome]
  (try (program/compile-program-descriptor descriptor genome)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- policy-error
  "Compile the inline descriptor against an inline source and return
  the ExceptionInfo (expected :program/policy-violation), or nil."
  [source]
  (compile-error (inline-descriptor) (genome-with source)))

;; --- the route.clj fixture is a real bundle program -----------------------

(deftest route-fixture-is-a-real-bundle-program
  (let [g (minimal-valid-genome)]
    (testing "the route program is part of the minimal-valid bundle"
      (is (contains? (:files g) "programs/route.clj")))
    (testing "it is classified as Clojure source, never parsed as EDN"
      (is (= :clj (get-in g [:files "programs/route.clj" :kind]))))))

;; --- valid descriptor -----------------------------------------------------

(deftest valid-route-descriptor-compiles
  (let [g (minimal-valid-genome)
        d (program/compile-program-descriptor (route-descriptor) g)]
    (testing "identity fields pass through"
      (is (= :program/route (:program/id d)))
      (is (= "programs/route.clj" (:file d)))
      (is (= 'agent.route/run (:entry d))))
    (testing "declared schemas pass through"
      (is (= :schema/route-input (:input-schema d)))
      (is (= :schema/intent-or-route (:output-schema d))))
    (testing "the source digest reuses the Genome's canonical file digest"
      (is (= (get-in g [:files "programs/route.clj" :digest]) (:source/digest d)))
      (is (= (hash/text-digest
              (slurp (io/resource "fixtures/genomes/minimal-valid/programs/route.clj")))
             (:source/digest d)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:source/digest d))))
    (testing "the program's declared namespace is recorded"
      (is (= ['agent.route] (:source/ns d))))
    (testing "the descriptor is pure serializable EDN (Global Constraint 22)"
      (is (= d (edn/read-string (pr-str d)))))))

(deftest compilation-is-deterministic
  (let [g (minimal-valid-genome)]
    (is (= (program/compile-program-descriptor (route-descriptor) g)
           (program/compile-program-descriptor (route-descriptor) g)))))

(deftest in-memory-program-registry-compiles
  ;; Choice (a): the program registry is an in-memory descriptor list
  ;; validated against the loaded Genome; no manifest programs key.
  (let [g (minimal-valid-genome)
        compiled (mapv #(program/compile-program-descriptor % g)
                       [(route-descriptor)])]
    (is (= [:program/route] (mapv :program/id compiled)))
    (is (= #{"programs/route.clj"} (set (map :file compiled))))))

;; --- clean source passes policy -------------------------------------------

(deftest route-fixture-passes-static-policy
  (is (nil? (program/policy-violation
             (slurp (io/resource "fixtures/genomes/minimal-valid/programs/route.clj"))))))

(deftest inline-clean-source-passes-policy
  (is (nil? (program/policy-violation
             "(ns agent.route)\n(defn run [input] {:action {:intent/type :intent/tool-call}})"))))

;; --- invalid descriptor variants ------------------------------------------

(deftest malformed-descriptors-rejected
  (doseq [[label bad] [["not a map" :not-a-map]
                       ["missing :program/id" (dissoc (route-descriptor) :program/id)]
                       ["missing :file" (dissoc (route-descriptor) :file)]
                       ["missing :entry" (dissoc (route-descriptor) :entry)]
                       ["missing :input-schema" (dissoc (route-descriptor) :input-schema)]
                       ["missing :output-schema" (dissoc (route-descriptor) :output-schema)]
                       ["unknown descriptor key" (assoc (route-descriptor) :extra :x)]]]
    (let [e (compile-error bad (minimal-valid-genome))]
      (is (instance? clojure.lang.ExceptionInfo e) label)
      (is (= :program/invalid (:error/type (ex-data e))) label))))

(deftest invalid-entry-symbol-rejected
  (doseq [bad [:program/route "agent.route/run" 'run]]
    (let [e (compile-error (assoc (route-descriptor) :entry bad)
                           (minimal-valid-genome))]
      (is (instance? clojure.lang.ExceptionInfo e) (pr-str bad))
      (is (= :program/invalid (:error/type (ex-data e))) (pr-str bad))
      (is (= :invalid-entry (:reason (ex-data e))) (pr-str bad)))))

(deftest invalid-schema-keywords-rejected
  (doseq [[k bad] [[:input-schema "not-a-keyword"]
                   [:output-schema 42]]]
    (let [e (compile-error (assoc (route-descriptor) k bad)
                           (minimal-valid-genome))]
      (is (instance? clojure.lang.ExceptionInfo e) (pr-str k))
      (is (= :program/invalid (:error/type (ex-data e))) (pr-str k)))))

(deftest invalid-genome-value-rejected
  (let [e (compile-error (route-descriptor) {:genome/id "sha256:not-a-genome"})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :program/invalid (:error/type (ex-data e))))
    (is (= :invalid-genome (:reason (ex-data e))))))

;; --- file existence and path ----------------------------------------------

(deftest missing-file-rejected
  (let [e (compile-error (assoc (route-descriptor) :file "programs/nope.clj")
                         (minimal-valid-genome))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :program/file-missing (:error/type (ex-data e))))
    (is (= "programs/nope.clj" (:file (ex-data e))))))

(deftest traversal-file-path-rejected
  (doseq [bad ["../escape.clj" "/abs/escape.clj" "a/../../b.clj" "C:\\evil.clj"]]
    (let [e (compile-error (assoc (route-descriptor) :file bad)
                           (minimal-valid-genome))]
      (is (instance? clojure.lang.ExceptionInfo e) bad)
      (is (= :program/path-invalid (:error/type (ex-data e))) bad))))

(deftest non-clojure-source-file-rejected
  (doseq [bad ["programs/route.edn" "programs/route.txt"]]
    (let [e (compile-error (assoc (route-descriptor) :file bad)
                           (minimal-valid-genome))]
      (is (instance? clojure.lang.ExceptionInfo e) bad)
      (is (= :program/invalid (:error/type (ex-data e))) bad)
      (is (= :not-clojure-source (:reason (ex-data e))) bad))))

;; --- source readability ---------------------------------------------------

(deftest unparseable-source-rejected
  (let [e (policy-error "(defn run [x] x")]  ; unbalanced parens
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :program/parse-error (:error/type (ex-data e))))))

;; --- entry symbol validation ----------------------------------------------

(deftest entry-not-defined-in-source-rejected
  (let [e (policy-error "(ns agent.route)\n(defn rnn [x] x)")]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :program/invalid (:error/type (ex-data e))))
    (is (= :entry-missing (:reason (ex-data e))))))

(deftest entry-namespace-mismatch-rejected
  (let [e (compile-error (inline-descriptor)
                         (genome-with "(ns other.ns)\n(defn run [x] x)"))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :program/invalid (:error/type (ex-data e))))
    (is (= :entry-namespace-mismatch (:reason (ex-data e))))))

(deftest entry-namespace-implicitly-declared-without-ns-form
  (let [d (program/compile-program-descriptor
           (inline-descriptor)
           (genome-with "(defn run [x] x)"))]
    (is (= 'agent.route/run (:entry d)))
    (is (= [] (:source/ns d)))))

;; --- static policy: load-file / eval --------------------------------------

(deftest load-file-rejected
  (let [e (policy-error "(ns agent.route)\n(defn run [x] (load-file \"evil.clj\"))")]
    (is (= :program/policy-violation (:error/type (ex-data e))))
    (is (= :load-file (:reason (ex-data e))))
    (is (= "load-file" (:symbol (ex-data e))))))

(deftest eval-rejected
  (let [e (policy-error "(ns agent.route)\n(defn run [x] (eval x))")]
    (is (= :program/policy-violation (:error/type (ex-data e))))
    (is (= :eval (:reason (ex-data e))))
    (is (= "eval" (:symbol (ex-data e))))))

(deftest fully-qualified-eval-rejected
  (let [e (policy-error "(ns agent.route)\n(defn run [x] (clojure.core/eval x))")]
    (is (= :program/policy-violation (:error/type (ex-data e))))
    (is (= :eval (:reason (ex-data e))))))

;; --- static policy: Java class literals / host namespaces -----------------

(deftest host-namespace-rejected
  (doseq [src ["(ns agent.route)\n(defn run [x] (System/getenv \"PATH\"))"
               "(ns agent.route)\n(defn run [x] (String/valueOf x))"]]
    (let [e (policy-error src)]
      (is (= :program/policy-violation (:error/type (ex-data e))) src)
      (is (= :host-namespace (:reason (ex-data e))) src))))

(deftest class-literal-rejected
  (doseq [src ["(ns agent.route)\n(def klass String)"
               "(ns agent.route)\n(defn run [x] (java.io.File. \"/tmp/x\"))"
               "(ns agent.route)\n(defn run [x] (java.io.File \"/tmp/x\"))"
               "(ns agent.route)\n(defn run [x] (instance? java.io.File x))"]]
    (let [e (policy-error src)]
      (is (= :program/policy-violation (:error/type (ex-data e))) src)
      (is (= :class-literal (:reason (ex-data e))) src))))

(deftest interop-special-form-rejected
  (doseq [src ["(ns agent.route)\n(defn run [x] (. System (getenv \"PATH\")))"
               "(ns agent.route)\n(defn run [x] (.. System (getProperties)))"]]
    (let [e (policy-error src)]
      (is (= :program/policy-violation (:error/type (ex-data e))) src)
      (is (= :interop (:reason (ex-data e))) src))))

;; --- static policy: require/use of undeclared host namespaces -------------

(deftest undeclared-require-rejected
  (doseq [src ["(ns agent.route)\n(require 'clojure.string)"
               "(ns agent.route)\n(require '[clojure.string :as str])"
               "(ns agent.route (:require [clojure.string :as str]))\n(defn run [x] x)"
               "(ns agent.route (:require [clojure.set]))\n(defn run [x] x)"]]
    (let [e (policy-error src)]
      (is (= :program/policy-violation (:error/type (ex-data e))) src)
      (is (= :undeclared-require (:reason (ex-data e))) src))))

(deftest self-require-and-core-require-allowed
  (doseq [src ["(ns agent.route)\n(require 'agent.route)"
               "(ns agent.route)\n(require 'clojure.core)"]]
    (is (nil? (program/policy-violation src)) src)))

(deftest ns-import-rejected
  (let [e (policy-error "(ns agent.route (:import [java.io File]))\n(defn run [x] x)")]
    (is (= :program/policy-violation (:error/type (ex-data e))))
    (is (= :host-import (:reason (ex-data e))))))

;; --- static policy: #= reader-eval ----------------------------------------

(deftest reader-eval-rejected
  (let [e (policy-error "(ns agent.route)\n(defn run [x] #=(System/exit 0))")]
    (is (= :program/policy-violation (:error/type (ex-data e))))
    (is (= :reader-eval (:reason (ex-data e))))))

;; --- quoted interop data is inert -----------------------------------------

(deftest quoted-interop-data-is-inert
  (is (nil? (program/policy-violation
             "(ns agent.route)\n(def x '(System/getenv \"a\"))"))))
