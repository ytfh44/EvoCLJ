(ns evoclj.runtime.phenotype-test
  "Task 6.1 tests for Phenotype construction and lifecycle.

  instantiate turns a CompiledGenome (evoclj.compiler.core) plus
  runtime-deps — the map of stores/providers/capabilities/program
  sources the HOST injects — into a live Phenotype:

    {:phenotype/id ...
     :compiled <CompiledGenome>   ; the SAME immutable value, shared
     :sci-runtime ...             ; a FRESH isolated SCI runtime
     :providers ...               ; host's registry, by reference
     :capabilities ...            ; host's leases + usage, by reference
     :stores ...}                 ; declared stores, passed through

  The five normative scenarios:

  - Step 1: two Phenotypes from ONE Genome have isolated SCI mutable
    contexts while sharing the immutable compiled data — redefining a
    program var in one context never affects the sibling (Global
    Constraints 3, 22, 23).
  - Step 2: halt! is idempotent and never touches host-owned
    resources (the stores/providers live in runtime-deps and belong
    to the host, so halt! releases only what the phenotype owns).
  - Step 3: construction opens NO resources beyond what runtime-deps
    declares — a deps map whose stores could never be opened still
    instantiates, the host's registry/usage atoms are referenced by
    identity (never replaced), and nothing is registered or consumed.
  - Step 4: Integrant (evoclj.runtime.system) wires ONLY the stable
    host components (:store/sqlite :store/cas :provider/registry
    :capability/broker) with thin init-key/halt-key methods; Genome
    graph nodes (:node/planner, :program/route, :graph/main) are
    never Integrant components, and the component map builds directly
    without the Integrant runtime.
  - Fail-closed construction: a compiled program with no declared
    source, a malformed CompiledGenome, a non-atom registry, a
    malformed lease, or a missing usage atom all throw typed errors."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.core :as core]
            [evoclj.genome.load :as load]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.system :as system]
            [evoclj.sci.execute :as execute]
            [integrant.core :as ig])
  (:import (java.nio.charset StandardCharsets)))

;; --- fixture and helper functions ------------------------------------------

(defn- fixture-root
  "The bundle directory for a named fixture under test/fixtures/genomes."
  [name]
  (.toPath (io/file (io/resource (str "fixtures/genomes/" name)))))

(defn- fixture-catalog
  "The on-disk provider catalog fixture."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor
  "The seed route program descriptor (Task 2.3)."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- seed-loaded-genome
  "The real minimal-valid bundle with the in-memory program registry
  attached (Task 2.3 choice (a))."
  []
  (assoc (load/load-genome (fixture-root "minimal-valid"))
         :programs [(route-descriptor)]))

(defn- compiled
  "Compile the seed fixture once."
  []
  (core/compile-genome (seed-loaded-genome) (fixture-catalog)))

(defn- genome-program-sources
  "Decode the source text of every compiled program out of the loaded
  Genome's immutable :files payloads (the host's job — it owns the
  Genome bundle; the CompiledGenome itself carries only :source/digest
  references, Global Constraint 22)."
  [loaded-genome]
  (into {}
        (for [descriptor (:programs loaded-genome)]
          [(:program/id descriptor)
           (String. (byte-array (get-in loaded-genome
                                        [:files (:file descriptor) :bytes]))
                    StandardCharsets/UTF_8)])))

(defn- echo-lease
  "A valid CapabilityLease granting this phenotype's exact id the
  :fixture/echo :invoke action for the next minute."
  [phenotype-id]
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :subject {:phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- runtime-deps
  "A realistic runtime-deps map for `compiled`: a registry carrying
  the :fixture/echo provider, a usage atom, one lease for the
  phenotype, the decoded program sources, and DECLARED-ONLY stores
  whose values (:poison) could never be opened — construction must
  never touch them."
  [compiled-genome]
  (let [registry (registry/create-registry)]
    (registry/register! registry (fixture/echo-provider))
    {:stores {:sqlite :poison
              :cas {:root :poison}}
     :providers {:registry registry}
     :capabilities {:leases [(echo-lease (:compiled/phenotype-id compiled-genome))]
                    :usage (atom {})}
     :program-sources (genome-program-sources (seed-loaded-genome))}))

(defn- instantiate-error
  "The ExceptionInfo thrown by instantiate, or nil."
  [compiled-genome deps]
  (try (phenotype/instantiate compiled-genome deps)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

;; ============================================================================
;; Step 1 — two Phenotypes from one Genome: isolated SCI, shared compiled data
;; ============================================================================

(deftest two-phenotypes-from-one-genome-have-isolated-sci-contexts
  (let [compiled (compiled)
        deps (runtime-deps compiled)
        p1 (phenotype/instantiate compiled deps)
        p2 (phenotype/instantiate compiled deps)]
    (testing "both phenotypes share the SAME immutable CompiledGenome value"
      (is (identical? compiled (:compiled p1)))
      (is (identical? (:compiled p1) (:compiled p2))))
    (testing "both carry the same canonical phenotype id, from the compiled value"
      (is (= (:compiled/phenotype-id compiled) (:phenotype/id p1)))
      (is (= (:phenotype/id p1) (:phenotype/id p2)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:phenotype/id p1))))
    (testing "each owns a DISTINCT isolated SCI runtime and context"
      (is (not (identical? (:sci-runtime p1) (:sci-runtime p2))))
      (is (not (identical? (get-in p1 [:sci-runtime :context])
                           (get-in p2 [:sci-runtime :context])))))
    (testing "every compiled program is loaded into each runtime at construction"
      (is (= (set (keys (:programs compiled)))
             (set (keys (get-in p1 [:sci-runtime :programs])))))
      (is (= (set (keys (:programs compiled)))
             (set (keys (get-in p2 [:sci-runtime :programs]))))))
    (testing "redefining a program var in one phenotype leaves the sibling untouched"
      (let [hijacked (str "(ns agent.route)\n"
                          "(defn run [input]\n"
                          "  {:action {:intent/type :intent/finish\n"
                          "            :payload {:value :hijacked}}})\n")
            p1 (update p1 :sci-runtime
                       execute/load-program!
                       (get-in compiled [:programs :program/route])
                       hijacked)]
        (testing "the redefined phenotype reflects the new definition"
          (is (= {:action {:intent/type :intent/finish
                           :payload {:value :hijacked}}}
                 (:value (execute/invoke! (:sci-runtime p1) :program/route
                                          {:op :echo :text "hi"})))))
        (testing "the sibling phenotype keeps the original program behavior"
          (is (= {:action {:intent/type :intent/tool-call
                           :payload {:tool/id :fixture/echo :args {:text "hi"}}}}
                 (:value (execute/invoke! (:sci-runtime p2) :program/route
                                          {:op :echo :text "hi"})))))))
    (testing "neither phenotype modified the shared immutable genome"
      (is (= (:compiled/genome-id compiled)
             (:compiled/genome-id (:compiled p1))))
      (is (= (:compiled/genome-id compiled)
             (:compiled/genome-id (:compiled p2)))))))

;; ============================================================================
;; Step 2 — halt! is idempotent and leaves host resources alone
;; ============================================================================

(deftest halt-is-idempotent-and-never-touches-host-resources
  (let [compiled (compiled)
        deps (runtime-deps compiled)
        registry (get-in deps [:providers :registry])
        usage (get-in deps [:capabilities :usage])
        p (phenotype/instantiate compiled deps)]
    (testing "a fresh phenotype is not halted"
      (is (not (phenotype/halted? p))))
    (testing "halt! marks the phenotype and returns it"
      (let [halted (phenotype/halt! p)]
        (is (phenotype/halted? halted))
        (is (= p (dissoc halted :halted?)))))
    (testing "calling halt! twice is a no-op, not an error"
      (let [once (phenotype/halt! p)
            twice (phenotype/halt! once)]
        (is (= once twice))
        (is (phenotype/halted? twice))
        (is (map? (phenotype/halt! twice)))))
    (testing "halt! never closes or replaces host-owned resources"
      (is (identical? registry (get-in p [:providers :registry])))
      (is (identical? usage (get-in p [:capabilities :usage])))
      (is (= #{:fixture/echo} (set (keys @registry))))
      (is (= {} @usage))
      (testing "the phenotype's owned sci-runtime is still intact for inspection"
        (is (= #{:program/route}
               (set (keys (get-in p [:sci-runtime :programs])))))))))

;; ============================================================================
;; Step 3 — construction opens no resources beyond runtime-deps
;; ============================================================================

(deftest construction-opens-no-resources-beyond-runtime-deps
  (let [compiled (compiled)
        registry (registry/create-registry)
        _ (registry/register! registry (fixture/echo-provider))
        usage (atom {})
        deps {:stores {:sqlite :poison       ; could never be opened
                       :cas {:root :poison}} ; could never be opened
              :providers {:registry registry}
              :capabilities {:leases []
                             :usage usage}
              :program-sources (genome-program-sources (seed-loaded-genome))}
        p (phenotype/instantiate compiled deps)]
    (testing "a deps map whose stores could never open still instantiates"
      (is (map? p))
      (is (= (:compiled/phenotype-id compiled) (:phenotype/id p))))
    (testing "the host's registry and usage atoms are referenced by identity, never replaced"
      (is (identical? registry (get-in p [:providers :registry])))
      (is (identical? usage (get-in p [:capabilities :usage])))
      (is (= [] (get-in p [:capabilities :leases]))))
    (testing "construction registers nothing and consumes no calls"
      (is (= #{:fixture/echo} (set (keys @registry))))
      (is (= {} @usage)))
    (testing "the declared stores pass through untouched"
      (is (= (:stores deps) (:stores p))))
    (testing "two instantiations never share the phenotype's owned SCI runtime"
      (let [p2 (phenotype/instantiate compiled deps)]
        (is (not (identical? (:sci-runtime p) (:sci-runtime p2))))))))

;; ============================================================================
;; Step 4 — Integrant wires stable host components only
;; ============================================================================

(deftest integrant-wires-only-stable-host-components
  (testing "the four stable host component keys are declared and wired"
    (doseq [k [:store/sqlite :store/cas :provider/registry :capability/broker]]
      (is (contains? (methods ig/init-key) k) (str k " init-key"))
      (is (contains? (methods ig/halt-key!) k) (str k " halt-key!"))))
  (testing "the key constants are exported by evoclj.runtime.system"
    (is (= :store/sqlite system/store-sqlite-key))
    (is (= :store/cas system/store-cas-key))
    (is (= :provider/registry system/provider-registry-key))
    (is (= :capability/broker system/capability-broker-key)))
  (testing "components construct directly without the Integrant runtime"
    (testing ":store/sqlite coerces a path into a java.jdbc spec"
      (is (= {:classname "org.sqlite.JDBC"
              :subprotocol "sqlite"
              :subname ":memory:"}
             (ig/init-key :store/sqlite ":memory:"))))
    (testing ":store/cas builds a config map from a root or {:root ...} map"
      (let [cas (ig/init-key :store/cas {:root "evoclj-cas-test" :verify true})]
        (is (instance? java.nio.file.Path (:root cas)))
        (is (true? (:verify cas))))
      (let [cas (ig/init-key :store/cas "evoclj-cas-test")]
        (is (instance? java.nio.file.Path (:root cas)))
        (is (false? (:verify cas)))))
    (testing ":provider/registry creates a fresh registry atom"
      (let [reg (ig/init-key :provider/registry {})]
        (is (instance? clojure.lang.Atom reg))
        (is (= {} @reg))))
    (testing ":capability/broker builds a broker context from injected deps"
      (let [reg (registry/create-registry)
            broker (ig/init-key :capability/broker {:registry reg :leases []})]
        (is (identical? reg (:registry broker)))
        (is (= [] (:leases broker)))
        (is (instance? clojure.lang.Atom (:usage broker))))))
  (testing "Genome graph nodes are never Integrant components"
    (doseq [k [:node/planner :node/router :node/finish :program/route :graph/main]]
      (is (not (contains? (methods ig/init-key) k)) (str k))
      (is (not (contains? (methods ig/halt-key!) k)) (str k)))))

;; ============================================================================
;; fail-closed construction
;; ============================================================================

(deftest malformed-inputs-fail-closed
  (let [compiled (compiled)
        deps (runtime-deps compiled)]
    (testing "a compiled program with no declared source is rejected"
      (let [e (instantiate-error compiled (assoc deps :program-sources {}))]
        (is (= :runtime/source-missing (:error/type (ex-data e))))
        (is (= :program/route (:program/id (ex-data e))))))
    (testing "a non-string source value is rejected"
      (let [e (instantiate-error compiled
                                 (assoc-in deps [:program-sources :program/route]
                                           :not-a-string))]
        (is (= :runtime/source-missing (:error/type (ex-data e))))))
    (testing "a CompiledGenome without a canonical phenotype id is rejected"
      (let [e (instantiate-error (dissoc compiled :compiled/phenotype-id) deps)]
        (is (= :runtime/invalid-compiled (:error/type (ex-data e)))))
      (let [e (instantiate-error (assoc compiled :compiled/phenotype-id "G42") deps)]
        (is (= :runtime/invalid-compiled (:error/type (ex-data e))))))
    (testing "a non-atom provider registry is rejected"
      (let [e (instantiate-error compiled
                                 (assoc-in deps [:providers :registry] :nope))]
        (is (= :runtime/deps-invalid (:error/type (ex-data e))))
        (is (= :registry-not-atom (:reason (ex-data e))))))
    (testing "a missing usage atom is rejected"
      (let [e (instantiate-error compiled
                                 (assoc-in deps [:capabilities :usage] nil))]
        (is (= :runtime/deps-invalid (:error/type (ex-data e))))
        (is (= :usage-missing (:reason (ex-data e))))))
    (testing "a malformed lease is rejected at construction"
      (let [bad-lease (dissoc (echo-lease (:compiled/phenotype-id compiled))
                              :actions)
            e (instantiate-error compiled
                                 (assoc-in deps [:capabilities :leases] [bad-lease]))]
        (is (= :runtime/deps-invalid (:error/type (ex-data e))))
        (is (= :invalid-lease (:reason (ex-data e))))))
    (testing "a missing :program-sources map is rejected"
      (let [e (instantiate-error compiled (dissoc deps :program-sources))]
        (is (= :runtime/deps-invalid (:error/type (ex-data e))))
        (is (= :program-sources-missing (:reason (ex-data e))))))
    (testing "a non-map :stores value is rejected"
      (let [e (instantiate-error compiled (assoc deps :stores :poison))]
        (is (= :runtime/deps-invalid (:error/type (ex-data e))))
        (is (= :stores-invalid (:reason (ex-data e))))))))
