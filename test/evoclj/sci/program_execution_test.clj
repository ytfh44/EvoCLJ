(ns evoclj.sci.program-execution-test
  "Tests for loading a compiled Genome program into an isolated SCI
  context and invoking its declared entry (Task 3.4).

  load-program! evaluates a compiled ProgramDescriptor's source ONCE
  into the SCI context owned by a runtime (a Phenotype-style
  {:context ctx :programs {...}} map) and registers the program under
  its :program/id; invoke! looks up the registered entry symbol inside
  that same context and calls it with EDN input under the deterministic
  limits of evoclj.sci.execute, returning validated, fully materialized
  EDN output. Loading mutates only the isolated SCI context — never
  host Vars, never the source Genome (Global Constraints 3, 7, 22, 23).

  The three required scenarios are covered directly:

  - Step 1: the seed programs/route.clj runs end to end from its
    compiled descriptor (evoclj.compiler.program) to EDN output.
  - Step 2: two runtimes loaded from the same Genome are independent —
    redefining a SCI var in one context never affects the other.
  - Step 3: execution never mutates the Genome: the compiled
    descriptor's :source/digest, the source bytes held by the bundle,
    and a fresh reload (:genome/id and :files) are all unchanged after
    running.

  The Milestone 3 exit scenario is asserted here as well: the seed
  routing program computes a decision from EDN input, and loading a
  program that attempts a host side effect (System/getenv) is denied
  by the closed SCI sandbox. The full denial surface (filesystem,
  environment, process, eval, require, interop, mutation) is asserted
  by the existing evoclj.sci.context-test adversarial suite, which runs
  unchanged in the full test suite."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.program :as program]
            [evoclj.genome.load :as load]
            [evoclj.sci.context :as context]
            [evoclj.sci.execute :as execute])
  (:import (java.nio.charset StandardCharsets)))

;; --- fixture and helper functions ------------------------------------------

(defn- fixture-root
  "The bundle directory for a named fixture under test/fixtures/genomes."
  [name]
  (.toPath (io/file (io/resource (str "fixtures/genomes/" name)))))

(defn- minimal-valid-genome
  "The real loaded minimal-valid bundle, which contains
  programs/route.clj (the seed Genome routing program)."
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

(defn- route-source
  "The seed Genome routing program source text (Task 2.3 fixture)."
  []
  (slurp (io/resource "fixtures/genomes/minimal-valid/programs/route.clj")))

(defn- make-runtime
  "A Phenotype-style sci-runtime: a fresh closed SCI context plus an
  empty program registry keyed by :program/id."
  []
  {:context (context/make-context {})
   :programs {}})

(defn- load-route-runtime
  "Compile the seed route program from the real minimal-valid bundle,
  load it into a fresh runtime, and return the runtime."
  []
  (execute/load-program! (make-runtime)
                         (program/compile-program-descriptor
                          (route-descriptor) (minimal-valid-genome))
                         (route-source)))

;; ============================================================================
;; Step 1 — the seed route.clj program end to end, descriptor to EDN output
;; ============================================================================

(deftest seed-route-program-runs-end-to-end
  (let [runtime (load-route-runtime)]
    (testing "echo input routes to a typed tool-call intent"
      (let [result (execute/invoke! runtime :program/route
                                    {:op :echo :text "hi"})]
        (is (= :ok (:status result)))
        (is (= {:action {:intent/type :intent/tool-call
                         :payload {:tool/id :fixture/echo
                                   :args {:text "hi"}}}}
               (:value result)))
        (is (pos? (:steps (:usage result))))
        (is (nat-int? (:wall-ms (:usage result))))))
    (testing "finish input routes to a typed finish intent"
      (is (= {:action {:intent/type :intent/finish :payload {:value 7}}}
             (:value (execute/invoke! runtime :program/route
                                      {:op :finish :value 7})))))
    (testing "an unknown op falls through to finish carrying the input"
      (is (= {:action {:intent/type :intent/finish
                       :payload {:value {:op :weird}}}}
             (:value (execute/invoke! runtime :program/route {:op :weird})))))
    (testing "a missing op falls through to finish carrying the input"
      (is (= {:action {:intent/type :intent/finish
                       :payload {:value {:text "x"}}}}
             (:value (execute/invoke! runtime :program/route {:text "x"})))))
    (testing "the output is fully realized plain EDN (Global Constraint 22)"
      (let [value (:value (execute/invoke! runtime :program/route
                                           {:op :echo :text "hi"}))]
        (is (= value (edn/read-string (pr-str value))))))))

;; ============================================================================
;; Step 2 — two independent contexts loaded from the same Genome
;; ============================================================================

(deftest redefining-a-var-in-one-context-does-not-affect-the-other
  (let [genome (minimal-valid-genome)
        compiled (program/compile-program-descriptor (route-descriptor) genome)
        source (route-source)
        first-runtime (execute/load-program! (make-runtime) compiled source)
        second-runtime (execute/load-program! (make-runtime) compiled source)
        ;; a successor Genome redefining the entry var in agent.route
        hijacked-source (str "(ns agent.route)\n"
                             "(defn run [input]\n"
                             "  {:action {:intent/type :intent/finish\n"
                             "            :payload {:value :hijacked}}})\n")
        first-runtime (execute/load-program! first-runtime compiled
                                             hijacked-source)]
    (testing "the redefined context reflects the new definition"
      (is (= {:action {:intent/type :intent/finish
                       :payload {:value :hijacked}}}
             (:value (execute/invoke! first-runtime :program/route
                                      {:op :echo :text "hi"})))))
    (testing "the sibling context keeps the original program behavior"
      (is (= {:action {:intent/type :intent/tool-call
                       :payload {:tool/id :fixture/echo :args {:text "hi"}}}}
             (:value (execute/invoke! second-runtime :program/route
                                      {:op :echo :text "hi"})))))))

;; ============================================================================
;; Step 3 — the Genome is never mutated by loading or execution
;; ============================================================================

(deftest genome-digest-and-source-bytes-unchanged-by-execution
  (let [genome (minimal-valid-genome)
        bundle-file (get-in genome [:files "programs/route.clj"])
        compiled (program/compile-program-descriptor (route-descriptor) genome)
        source (route-source)
        runtime (execute/load-program! (make-runtime) compiled source)]
    (dotimes [_ 5]
      (execute/invoke! runtime :program/route {:op :echo :text "x"}))
    (testing "the compiled descriptor's :source/digest still equals the Genome file digest"
      (is (= (:digest bundle-file) (:source/digest compiled))))
    (testing "the source bytes held by the Genome are unchanged"
      (is (= (vec (.getBytes source StandardCharsets/UTF_8))
             (:bytes bundle-file))))
    (testing "a fresh load of the same bundle is the identical immutable Genome"
      (let [reloaded (minimal-valid-genome)]
        (is (= (:genome/id genome) (:genome/id reloaded)))
        (is (= (:files genome) (:files reloaded)))))))

;; ============================================================================
;; boundary behavior of invoke!
;; ============================================================================

(deftest non-edn-safe-input-is-rejected-with-a-typed-error
  (let [runtime (load-route-runtime)
        result (execute/invoke! runtime :program/route (fn [x] x))]
    (is (= :error (:status result)))
    (is (= :program/input-invalid (:error/type (:error result))))
    (is (= (:error result) (edn/read-string (pr-str (:error result)))))))

(deftest unknown-program-id-is-a-typed-error
  (let [runtime (load-route-runtime)
        result (execute/invoke! runtime :program/nope {:op :echo :text "x"})]
    (is (= :error (:status result)))
    (is (= :program/invalid (:error/type (:error result))))
    (is (= :program-not-found (:reason (:error/data (:error result)))))))

(deftest loaded-program-loops-are-interrupted-not-hung
  (testing "limits still apply to an already-loaded program: the delegating
            interrupt installed by load-program! makes a per-invocation
            step budget interrupt an infinite loop in the loaded source,
            so invoke! never hangs the host"
    (let [compiled {:program/id :fixture/loop
                    :entry 'fixture.loop/run}
          runtime (execute/load-program!
                   (make-runtime) compiled
                   "(ns fixture.loop)\n(defn run [x] (loop [] (recur)))")
          result (execute/invoke! runtime :fixture/loop nil
                                  {:wall-ms 10000 :max-steps 1000})]
      (is (= :error (:status result)))
      (is (= :sci/limit-exceeded (:error/type (:error result))))
      (is (contains? #{:max-steps :wall-ms}
                     (:limit (:error/data (:error result)))))
      (is (= (:error result) (edn/read-string (pr-str (:error result))))))))

(deftest top-level-loop-in-source-is-interrupted-at-load
  (testing "a Genome whose source loops at top level is interrupted during
            load-program! (the default limits bound the load-time
            evaluation), so a broken Genome cannot hang the host"
    (is (thrown? clojure.lang.ExceptionInfo
                 (execute/load-program!
                  (make-runtime)
                  {:program/id :fixture/top :entry 'fixture.top/run}
                  "(ns fixture.top)\n(loop [] (recur))")))))

;; ============================================================================
;; Milestone 3 exit — decisions computed, host side effects denied
;; ============================================================================

(deftest hostile-program-source-is-denied-at-load
  (testing "loading a program that attempts a host side effect is denied by
            the closed SCI sandbox (the compiler's static policy is bypassed
            here on purpose: the sandbox is the final enforcement layer)"
    (let [compiled (program/compile-program-descriptor
                    (route-descriptor) (minimal-valid-genome))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (execute/load-program!
                    (make-runtime) compiled
                    "(ns agent.route)\n(defn run [x] (System/getenv \"PATH\"))")))
      (is (thrown? clojure.lang.ExceptionInfo
                   (execute/load-program!
                    (make-runtime) compiled
                    "(ns agent.route)\n(defn run [x] (slurp \"/etc/passwd\"))"))))))
