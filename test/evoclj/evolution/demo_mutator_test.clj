(ns evoclj.evolution.demo-mutator-test
  "component tests — the built-in heuristic mutator and the :demo profile.

  The demo mutator (evoclj.evolution.demo-mutator) is the NON-LLM
  Mutator adapter the :demo config profile injects through the CLI's
  existing host injection path (:overrides -> build-config). It
  performs deterministic template/function-swap mutations over the
  seed genome's OWN mutable program file (programs/route.clj), so a
  candidate produced from it must always pass the compiler topology
  validation (compile-genome) and be byte-deterministic for identical
  inputs (Global Constraint 6).

  Coverage:
  - the mutator proposes schema-valid mutations that APPLY and COMPILE
    (compiler topology passes) from the real genomes/seed parent;
  - identical contexts yield identical proposals (determinism);
  - the :demo profile is a BUILT-IN, resolvable config profile;
  - selecting the :demo profile makes the CLI host inject the demo
    mutator plus the demo's hidden selection cases/fixtures through
    the :overrides seam (the existing host injection path);
  - the acceptance scenario: a FRESH state dir + the :demo profile ->
    one `cycle` invocation runs evolve -> eval -> promote headless to
    a promoted candidate (CURRENT moves atomically)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.main :as main]
            [evoclj.cli.session :as session]
            [evoclj.compiler.core :as compiler]
            [evoclj.config :as config]
            [evoclj.evolution.budget :as budget]
            [evoclj.evolution.core :as core]
            [evoclj.evolution.demo-mutator :as demo]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.genome.patch :as patch]
            [evoclj.promotion.current :as current]
            [evoclj.helpers :as h]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- the real seed Genome (G1) ------------------------------------------------

(def ^:private generation-id "generation-1")

(defn- seed-root
  "The real seed Genome bundle directory (genomes/seed)."
  []
  (let [p (.toPath (io/file "genomes" "seed"))]
    (when-not (Files/isDirectory p (make-array LinkOption 0))
      (throw (ex-info "genomes/seed bundle not found (run from the repo root)"
                      {:path (str p)})))
    p))

(defn- route-descriptor
  "The seed route program descriptor (component choice (a))."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- seed-loaded-genome
  "The REAL genomes/seed bundle loaded from disk with its program
  registry attached (G1)."
  []
  (assoc (load/load-genome (seed-root))
         :programs [(route-descriptor)]))

(defn- fixture-catalog
  "The on-disk provider catalog fixture (component Resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- demo-context
  "The full, closed Mutator context the orchestrator hands the demo
  mutator: the real seed parent and a schema-valid diagnosis that
  carries NO hypothesis (a fresh state dir has no evidence yet, so the
  pattern Diagnostician proposes nothing — the demo mutator must still
  propose, supplying its own :hypothesis/id)."
  [& [parent]]
  (let [parent (or parent (seed-loaded-genome))]
    {:generation/id generation-id
     :parent/genome-id (:genome/id parent)
     :parent-genome parent
     :diagnosis {:diagnosis/id (str "sha256:" (apply str (repeat 64 "d")))
                 :evidence/id (str "sha256:" (apply str (repeat 64 "e")))
                 :hypotheses []}
     :history []
     :budget-profile budget/v0-profile}))

(defn- completed-mutation
  "Complete an adapter-returned mutation with the lineage fields the
  orchestrator (evolution.core/complete-mutation!) adds before
  validation/patching: :mutation/id, :parent/genome-id, and
  :evidence/id (the demo mutator already owns :risk, :ops,
  :hypothesis/id, and :expected-effect). A FIXED :mutation/id keeps
  the applied candidate a pure function of parent bytes + ops (Global
  Constraint 6)."
  [parent m]
  (merge {:mutation/id (java.util.UUID/nameUUIDFromBytes
                        (.getBytes "evoclj/demo-mutation"
                                   StandardCharsets/UTF_8))
          :parent/genome-id (:genome/id parent)
          :evidence/id (str "sha256:" (apply str (repeat 64 "e")))}
         m))

;; --- temp dirs / stores (test temp dirs only) ----------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-dir
  [prefix]
  (let [d (str (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! temp-paths conj d)
    d))

(defn- delete-tree!
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

;; ============================================================================
;; 1. the demo mutator produces genomes that pass compiler topology validation
;; ============================================================================


;; S4 helper — collapsed to evoclj.helpers
(def ^:private assert-validated h/assert-validated-simple)
(deftest demo-mutator-produces-topology-valid-genomes
  (let [parent (seed-loaded-genome)
        proposed (core/propose-mutations (demo/demo-mutator)
                                         (demo-context parent))
        m (first proposed)]
    (testing "the adapter proposes the deterministic template-swap library"
      (is (= 3 (count proposed)))
      (is (= :program (:risk m)))
      (is (= "programs/route.clj" (get-in m [:ops 0 :file])))
      (is (= :replace-form (get-in m [:ops 0 :op])))
      (testing "the kernel-computed :expect/hash is the parent file's digest"
        (is (= (get-in parent [:files "programs/route.clj" :digest])
               (get-in m [:ops 0 :expect/hash]))))
      (testing "every proposal carries its own hypothesis (an empty
                diagnosis never blocks the heuristic mutator)"
        (is (uuid? (:hypothesis/id m)))
        (is (every? #(uuid? (:hypothesis/id %)) proposed))))
    (testing "each proposed mutation is schema-valid against the parent
              once the orchestrator completes its lineage fields"
      (assert-validated (completed-mutation parent m) (mutation/validate-mutation (completed-mutation parent m) parent)))
    (testing "applying + compiling the mutation yields a topology-valid
              candidate G2 (compiler topology passes)"
      (let [candidate (patch/apply-mutation parent
                                            (mutation/validate-mutation (completed-mutation parent m) parent)
                                            (temp-dir "evoclj-demo-candidates-"))
            compiled (compiler/compile-genome
                      (assoc candidate :programs [(route-descriptor)])
                      (fixture-catalog))]
        (is (some? compiled))
        (is (some? (:compiled/resolution-id compiled)))
        (is (not= (:genome/id parent) (:compiled/genome-id compiled))
            "G2 is a new content address, not G1")
        (testing "the candidate bundle reloads to its content address"
          (is (= (:compiled/genome-id compiled)
                 (:genome/id (load/load-genome (:genome/root candidate))))))))))

;; ============================================================================
;; 2. determinism — identical inputs, identical proposals
;; ============================================================================

(deftest demo-mutator-is-deterministic-for-identical-inputs
  (let [ctx (demo-context)
        m (demo/demo-mutator)]
    (testing "two proposals over the identical context are byte-identical"
      (is (= (core/propose-mutations m ctx)
             (core/propose-mutations m ctx))))
    (testing "a fresh adapter instance agrees with the first"
      (is (= (core/propose-mutations (demo/demo-mutator) ctx)
             (core/propose-mutations m ctx))))
    (testing "the same parent bytes + mutation value yield the same
              applied candidate hash (Global Constraint 6)"
      (let [parent (seed-loaded-genome)
            propose (fn []
                      (completed-mutation
                       parent
                       (first (core/propose-mutations
                               (demo/demo-mutator)
                               (demo-context parent)))))
            apply1 (patch/apply-mutation parent (mutation/validate-mutation (propose) parent)
                                         (temp-dir "evoclj-demo-det-cand-1-"))
            apply2 (patch/apply-mutation parent (mutation/validate-mutation (propose) parent)
                                         (temp-dir "evoclj-demo-det-cand-2-"))]
        (is (= (:genome/id apply1) (:genome/id apply2)))))))

;; ============================================================================
;; 3. the :demo profile is a built-in, resolvable config profile
;; ============================================================================

(deftest demo-profile-is-builtin-and-resolvable
  (let [c (config/load-config (config/demo-profile))
        r (config/resolve-profile c :demo)]
    (testing "config/demo-profile loads into a valid envelope"
      (is (some? c))
      (is (contains? (:config/profiles c) :demo)))
    (testing "resolve-profile :demo applies the demo's section overrides
              (the demo cap merges over the base budget keys)"
      (is (= {:max-candidates 3 :max-cost 0.0 :max-tokens 0}
             (:config/budget r))))
    (testing "an unknown profile still fails closed"
      (is (= :config/profile-not-found
             (:error/type (ex-data (try (config/resolve-profile c :nope)
                                        nil
                                        (catch clojure.lang.ExceptionInfo e e)))))))))

;; ============================================================================
;; 4. the demo profile wires the mutator through the host injection path
;; ============================================================================

(deftest demo-profile-wires-demo-mutator-through-host-injection
  (let [system (session/build-system
                {:state-dir (temp-dir "evoclj-demo-profile-")
                 :config/profile :demo})
        evo (:evolution/system system)
        es (:eval/system system)]
    (testing "the evolution system carries a real Mutator, not :none"
      (is (satisfies? core/Mutator (:mutator evo))))
    (testing "the eval system carries the demo's hidden selection surface"
      (is (= #{:sel/demo-echo :sel/demo-echo-b}
             (set (keys (:selection/cases es)))))
      (is (= #{:fixture/echo :fixture/echo-b}
             (set (keys (:selection/fixtures es))))))))

;; ============================================================================
;; 5. acceptance: fresh state dir + demo profile -> cycle promotes headless
;; ============================================================================

(defn- dash
  [id]
  (str/replace id ":" "-"))

(defn- genome-index-bytes
  "The canonical CAS body of a loaded Genome (Database Invariant 7)."
  [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by (fn [[p _]] p) gpath/bytewise-compare (:files loaded)))))

(defn- copy-tree!
  [src dest]
  (let [from (Paths/get (str src) (make-array String 0))
        to (Paths/get (str dest) (make-array String 0))]
    (with-open [stream (Files/walk from (make-array FileVisitOption 0))]
      (doseq [p (iterator-seq (.iterator stream))]
        (let [rel (.relativize from p)
              target (.resolve to rel)]
          (when (Files/isDirectory p (make-array LinkOption 0))
            (Files/createDirectories target (make-array FileAttribute 0)))
          (when (Files/isRegularFile p (make-array LinkOption 0))
            (Files/createDirectories (.getParent target)
                                     (make-array FileAttribute 0))
            (Files/copy p target (make-array java.nio.file.CopyOption 0))))))))

(defn- provision-demo-state!
  "A FRESH state dir provisioned like a real host deployment: migrated
  db, the generation-1 row (current = 1) pinned to the REAL seed
  genome's content address, G1's canonical body in the CAS, and the G1
  bundle at <state-dir>/genomes/<id-as-dash>. NO sessions, NO episodes
  — the demo runs headless from an empty evolution set."
  []
  (let [dir (temp-dir "evoclj-demo-state-")
        _ (Files/createDirectories (Paths/get (str dir "/db")
                                              (make-array String 0))
                                   (make-array FileAttribute 0))
        db-path (str dir "/db/evoclj.db")
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        loaded (seed-loaded-genome)
        compiled (compiler/compile-genome loaded (fixture-catalog))
        genome-id (:compiled/genome-id compiled)
        resolution-id (:compiled/resolution-id compiled)
        cas-root (str dir "/cas")
        cas-store (cas/->cas cas-root)]
    (artifact/ensure-artifact! db genome-id "application/octet-stream" 0)
    (artifact/ensure-artifact! db resolution-id "application/edn" 0)
    (artifact/ensure-genome! db genome-id)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    (cas/put-bytes! cas-store
                    (.getBytes (genome-index-bytes loaded) StandardCharsets/UTF_8)
                    {})
    (copy-tree! (seed-root) (str dir "/genomes/" (dash genome-id)))
    {:state-dir dir :db db :genome-id genome-id}))

(deftest demo-cycle-propose-eval-promote-runs-headless-to-a-promoted-candidate
  (let [{:keys [state-dir db genome-id]} (provision-demo-state!)]
    (testing "one `cycle` invocation under the :demo profile walks the
              whole loop with no model and no injected overrides"
      (let [{:keys [exit data]} (main/execute ["cycle"]
                                              {:state-dir state-dir
                                               :config/profile :demo})]
        (is (= 0 exit) (str "cycle exits 0: " (pr-str data)))
        (is (= generation-id (:generation/id data)))
        (let [phases (:phases data)]
          (testing "EVOLVE materialized the deterministic candidates"
            (let [evolve (:evolve phases)]
              (is (true? (:run? evolve)))
              (is (= 3 (count (:candidates evolve))))
              (let [g2 (first (:candidates evolve))]
                (is (= :evaluation-pending (:state g2)))
                (is (= genome-id (:parent/genome-id g2)))
                (is (not= genome-id (:candidate/genome-id g2)))
                (is (= :program (:risk g2))))))
          (testing "EVAL finalized every candidate; exactly one is eligible"
            (let [evals (:eval phases)]
              (is (= 3 (count evals)))
              (is (every? #(contains? % :evaluation/id) evals))
              (is (= 1 (count (filter #(true? (get-in % [:eligibility :eligible?]))
                                      evals))))))
          (testing "PROMOTE moved CURRENT atomically to the eligible candidate"
            (let [promotes (:promote phases)]
              (is (= 1 (count promotes)))
              (is (= :promoted (get-in promotes [0 :status])))
              (let [to (get-in promotes [0 :outcome :to])]
                (is (not= generation-id to))
                (is (= to (:id (current/current-generation db)))
                    "CURRENT now names the promoted generation")))))))))

;; --- the demo mutator is a plain Mutator adapter --------------------------------

(deftest demo-mutator-satisfies-the-mutator-protocol
  (testing "the adapter satisfies the Mutator protocol (adapter pattern)"
    (is (satisfies? core/Mutator (demo/demo-mutator))))
  (testing "a non-map context fails closed with :mutation/context-invalid"
    (let [e (try (core/propose-mutations (demo/demo-mutator) :nope) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :mutation/context-invalid (:error/type (ex-data e)))))))