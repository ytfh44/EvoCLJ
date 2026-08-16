(ns evoclj.perf.full-cycle-harness
  "Task O4 — the full-cycle timing harness + performance baseline.

  The harness runs evolve -> eval -> promote on a seed Genome through
  the SAME public subsystem APIs the `cycle` CLI command walks
  (evolution.core/propose-candidates!, eval.core/evaluate-candidate!,
  promotion.promote/promote!) and returns a structured EDN report with
  PER-PHASE wall-clock timings and F2 metric records:

      {:harness/name :full-cycle
       :provider {:mode :fixture
                  :model/endpoint? <bool>       ; a configured model is
                  :model/configured [<ids>]     ;  reachable (API key)
                  :catalog/status <kw>          ; models.dev refresh result
                  :model/note <str>}            ; honest no-endpoint note
       :seed {:source <bundle root> :generation/id :genome/id}
       :phases {:evolve {:run? bool :candidates n :candidate-ids [...]
                         :wall-ms n}
                :eval   {:evaluated n :eligible n :failed n :wall-ms n
                         :evaluations [{:candidate/id :evaluation/id
                                        :eligibility {...} :wall-ms n} ...]}
                :promote {:attempted n :promoted n :stale n :failed n
                          :wall-ms n
                          :outcomes [{:candidate/id :status :outcome} ...]}}
       :cycle/wall-ms n
       :f2/metrics [<metric records>]}

  The F2 metric records (evoclj.metrics.core, closed MetricSchema)
  include the harness-level :cycle/* records AND the Task A2 eval
  envelope (:eval/<phase>-ms, :eval/<phase>-outcome, :eval/total-ms)
  that evaluate-candidate! records into the same collector.

  MODEL POLICY (honest fallback): the host system is built with the
  real models.dev catalog (short 3 s fetch timeout — the catalog is
  cached under the state dir, and an unreachable source degrades to
  :catalog/unavailable without failing startup). A model endpoint is
  reported reachable ONLY when the kernel-owned model registry has a
  model with a live provider (an API key). On this host no API keys
  are configured, so :provider/mode is :fixture, :model/endpoint? is
  false, and the baseline numbers are FIXTURE-mode — never
  fabricated as real-model timings (the explicit no-endpoint note is
  written into the report and into docs/performance-baseline.md).

  THE SEED GENOME (documented deviation, Repo Convention 5): the
  harness evolves test/fixtures/evolution-e2e/route-a as G1 — the
  SAME deterministic full-cycle fixture the cli cycle tests use. The
  genomes/seed bundle's route never fails (every input finishes
  successfully), so it can never produce the failure evidence the
  deterministic pattern Diagnostician needs to fire a :task/success
  hypothesis; route-a fails :echo-b deterministically, producing the
  Evolution set that fires the mutation and a real promotion.

  RUN (from the repo root):
      clojure -M scripts/full-cycle.clj [--state-dir <dir>] [--out <file>]
  When --out is given the report is written to the file as EDN;
  otherwise it is printed to stdout. The test suite drives the same
  code in-process via load-file (the top-level runner is disabled when
  the system property evoclj.harness.loaded-by-test is set)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.cli.session :as cli-session]
            [evoclj.compiler.core :as compiler]
            [evoclj.eval.core :as eval-core]
            [evoclj.eval.replay :as replay]
            [evoclj.evolution.core :as evolution]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.metrics.core :as f2]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.model-registry :as model-registry]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.promotion.promote :as promote]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session-store]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant)
           (java.util Date UUID)))

;; ============================================================================
;; harness constants and fixture plumbing
;; ============================================================================

(def ^:private seed-source
  "The G1 bundle the harness evolves (see the namespace docstring for
  why route-a, not genomes/seed, is the harness seed)."
  "test/fixtures/evolution-e2e/route-a")

(def ^:private selection-source
  "The hidden selection case directory (Global Constraint 11: the
  harness reads the case BODIES only in harness code, never inside an
  agent workspace)."
  "test/fixtures/evolution-e2e/selection")

(def ^:private catalog-source
  "The Task 2.1 resolution provider-catalog fixture (the :fixture
  adapters; no real provider credentials ever appear — Global
  Constraint 22)."
  "test/fixtures/resolution/provider-catalog.edn")

(def ^:private repo-root
  "The repo root this script lives in: the grandparent of
  scripts/full-cycle.clj when *file* is usable, else the JVM working
  directory (the documented run location)."
  (or (some-> *file* io/file .getParentFile .getParentFile str)
      (System/getProperty "user.dir")
      "."))

(defn- fixture-path
  "A repo-rooted path under `parts` (e.g. (fixture-path \"test\"
  \"fixtures\" \"evolution-e2e\" \"route-a\"))."
  [& parts]
  (str repo-root "/" (str/join "/" parts)))

(defn- route-descriptor []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- fixture-catalog []
  (edn/read-string (slurp (io/file (fixture-path "test" "fixtures"
                                                 "resolution"
                                                 "provider-catalog.edn")))))

(defn- dash [id] (str/replace id ":" "-"))

;; --- filesystem helpers ------------------------------------------------------

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [path]
  (when (Files/exists (Paths/get path (make-array String 0))
                      (make-array LinkOption 0))
    (with-open [stream (Files/walk (Paths/get path (make-array String 0))
                                   (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- copy-tree! [src dest]
  (let [from (Paths/get src (make-array String 0))
        to (Paths/get dest (make-array String 0))]
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

(defn- genome-index-bytes
  "The canonical CAS body of a loaded Genome — the exact serialization
  of evoclj.genome.hash/tree-digest (path + NUL + digest + LF per
  entry, sorted bytewise) whose SHA-256 is the genome's content
  address."
  [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by (fn [[p _]] p) gpath/bytewise-compare (:files loaded)))))

(defn- program-sources [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded [:files (:file descriptor) :bytes]))
                        StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- compile-bundle [bundle-root]
  (let [loaded (assoc (load/load-genome bundle-root)
                      :programs [(route-descriptor)])]
    {:loaded loaded
     :compiled (compiler/compile-genome loaded (fixture-catalog))}))

;; ============================================================================
;; provision! — a fresh state dir provisioned like a real host deployment
;; ============================================================================

(defn- provision!
  "Provision `state-dir`: migrated db, the generation-1 row (current =
  1), the G1 canonical body in the CAS, and the G1 bundle at
  <state-dir>/genomes/<id-as-dash>. Returns the context map the
  harness phases read."
  [state-dir]
  (let [db-path (str state-dir "/db/evoclj.db")
        _ (Files/createDirectories (Paths/get (str state-dir "/db")
                                              (make-array String 0))
                                   (make-array FileAttribute 0))
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        {:keys [loaded compiled]} (compile-bundle (fixture-path "test" "fixtures"
                                                                "evolution-e2e"
                                                                "route-a"))
        genome-id (:compiled/genome-id compiled)
        resolution-id (:compiled/resolution-id compiled)
        cas-root (str state-dir "/cas")
        cas-store (cas/->cas cas-root)]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "generation-1"
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    (cas/put-bytes! cas-store
                    (.getBytes (genome-index-bytes loaded) StandardCharsets/UTF_8)
                    {})
    (copy-tree! (fixture-path "test" "fixtures" "evolution-e2e" "route-a")
                (str state-dir "/genomes/" (dash genome-id)))
    {:state-dir state-dir :db db :db-path db-path :cas-root cas-root
     :cas-store cas-store :loaded loaded :compiled compiled
     :genome-id genome-id :resolution-id resolution-id}))

;; ============================================================================
;; recording the Evolution set + the replay case (fixture path)
;; ============================================================================

(defn- echo-lease [phenotype-id]
  (let [now (Date.)]
    {:cap/id (random-uuid)
     :subject {:phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 100}
     :issued-at now
     :expires-at (Date. (+ (.getTime now) 60000))}))

(defn- run-fixture-session!
  "Run ONE fixture session pinned to the seed generation through the
  real pipeline (load -> compile -> instantiate -> pinned session ->
  run-session! -> materialize Episode). Returns {:session/id :result}."
  [ctx task]
  (let [{:keys [db cas-store compiled loaded]} ctx
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider {}))
        _ (registry/register! reg (fixture/non-idempotent-provider {}))
        usage (atom {})
        lease (echo-lease (:compiled/phenotype-id compiled))
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases [lease] :usage usage}
             :program-sources (program-sources loaded compiled)})
        executor {:phenotype ph
                  :stores {:sqlite db :cas cas-store}
                  :dispatch (dispatch/make-broker-context
                             {:registry reg :leases [lease] :usage usage})}
        sid (:session/id
             (session-store/create-session!
              db
              {:genome/id (:compiled/genome-id compiled)
               :resolution/id (:compiled/resolution-id compiled)
               :phenotype/id (:compiled/phenotype-id compiled)
               :generation/id "generation-1"}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id "generation-1"
                          :phenotype/id (:compiled/phenotype-id compiled)
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    (let [result (scheduler/run-session! executor sid task)]
      (episode/materialize-episode! {:sqlite db :cas cas-store} sid)
      {:session/id sid :result result :executor executor})))

(defn- read-artifact [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id) StandardCharsets/UTF_8)))

(defn- replay-case-from-session [ctx sid task expected-output]
  (let [store {:sqlite (:db ctx) :cas (:cas-store ctx)}
        events (event/events-for-session (:db ctx) sid)
        completed (first (filter #(= :provider/call-completed (:event/type %))
                                 events))
        response (read-artifact store (:payload-ref completed))
        decision (first expected-output)
        payload (get-in decision [:action :payload])]
    (replay/build-replay-case
     {:episode/id (random-uuid)
      :outcome {:status :completed}}
     [{:intent/type :intent/tool-call
       :effect :read
       :payload payload
       :response response}]
     {:case/id :replay/a
      :task-input task
      :expected-output expected-output
      :mode :fixture})))

(defn- record-evolution-set!
  "Run the Evolution set the deterministic Diagnostician reads: one
  class-A success + two class-B failures (route-a fails :echo-b by
  construction). Returns the context with the recorded :task,
  :expected output, and the built replay case."
  [ctx]
  (let [task {:op :echo-a :text "hi"}
        expected [{:action {:intent/type :intent/tool-call
                            :payload {:tool/id :fixture/echo
                                      :args {:text "hi"}}}}
                  {:text "hi"}]
        run (run-fixture-session! ctx task)
        sid (:session/id run)]
    (run-fixture-session! ctx {:op :echo-b :text "bo"})
    (run-fixture-session! ctx {:op :echo-b :text "go"})
    (assoc ctx :task task :expected expected
           :replay-case (replay-case-from-session ctx sid task expected))))

;; ============================================================================
;; the deterministic mutator (the recording-mutator pattern)
;; ============================================================================

(defn- deterministic-uuid [s]
  (UUID/nameUUIDFromBytes (.getBytes s StandardCharsets/UTF_8)))

(defn- echo-b-provider []
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/echo-b
       :effect :pure
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke
       :retry {:safe? true}})
    (normalize-request [_ intent]
      (let [args (get-in intent [:payload :args])]
        (when-not (map? args)
          (throw (ex-info "tool-call payload must carry an :args map"
                          {:error/type :provider/input-invalid})))
        {:tool/id :fixture/echo-b
         :resource {:kind :tool :id :fixture/echo-b}
         :args args}))
    (execute-request! [_ authorized-request]
      {:text (get-in authorized-request [:args :text])})))

(defn- g2-case-form []
  (list (quote case) (quote op)
        :echo-a {:action (list (quote tool-call-intent) :fixture/echo
                               {:text (list (quote get) (quote input) :text)})}
        :echo-b {:action (list (quote tool-call-intent) :fixture/echo-b
                               {:text (list (quote get) (quote input) :text)})}
        {:action (list (quote finish-intent) (quote input))}))

(defn- route-replacement-op [parent-genome form]
  {:op :replace-form
   :file "programs/route.clj"
   :selector [(quote case)]
   :expect/hash (get-in parent-genome [:files "programs/route.clj" :digest])
   :form form})

(defn- delta-mutation [parent diagnosis hypothesis form suffix]
  (let [content {:parent/genome-id (:genome/id parent)
                 :hypothesis/id (:hypothesis/id hypothesis)
                 :evidence/id (:evidence/id diagnosis)
                 :risk :program
                 :ops [(route-replacement-op parent form)]
                 :expected-effect {:primary-metric :task/success
                                   :direction :increase}}]
    (assoc content
           :mutation/id (deterministic-uuid (pr-str [content suffix])))))

(defn- propose-deltas
  "The deterministic Mutator: when the diagnosis fires the
  :task/success hypothesis, propose the ONE bounded mutation that
  replaces the route case with the reference decision table (route-b
  behavior)."
  [ctx]
  (when-let [hypothesis (some #(when (= :task/success (:pattern %)) %)
                              (:hypotheses (:diagnosis ctx)))]
    (let [parent (:parent-genome ctx)
          diagnosis (:diagnosis ctx)]
      [(delta-mutation parent diagnosis hypothesis (g2-case-form) "g2")])))

(defn- selection-cases []
  (into {}
        (map (fn [f]
               (let [c (edn/read-string (slurp f))]
                 [(:case/id c) c])))
        [(io/file (fixture-path "test" "fixtures" "evolution-e2e"
                                "selection" "sel-a.edn"))
         (io/file (fixture-path "test" "fixtures" "evolution-e2e"
                                "selection" "sel-b.edn"))]))

(defn- cycle-overrides
  "The single :overrides seam for the harness: the deterministic
  Mutator (evolution) AND the evaluator hidden cases/fixtures (eval)."
  [ctx]
  {:evolution/system
   {:mutator (fn [mutation-ctx] (propose-deltas mutation-ctx))}
   :eval/system
   {:selection/cases (selection-cases)
    :selection/fixtures
    {:fixture/echo (fn [_seed] (fixture/echo-provider {}))
     :fixture/echo-b (fn [_seed] (echo-b-provider))}
    :replay/cases {:replay/a (:replay-case ctx)}
    :replay/fixtures
    {:fixture/echo (fn [] (fixture/echo-provider {}))
     :fixture/echo-b (fn [] (echo-b-provider))}}})

;; ============================================================================
;; timing + the candidate record mapping
;; ============================================================================

(defn- timed
  "Run f and return {:wall-ms <long> :result <value|nil> :error
  <typed data|nil>}. Wall time is measured with System/nanoTime
  around f (elapsed is read AFTER f returns) and is still reported
  honestly when f throws."
  [f]
  (let [t0 (System/nanoTime)]
    (try
      (let [result (f)]
        {:wall-ms (quot (- (System/nanoTime) t0) 1000000)
         :result result
         :error nil})
      (catch Throwable t
        {:wall-ms (quot (- (System/nanoTime) t0) 1000000)
         :result nil
         :error {:error/type (or (:error/type (ex-data t)) :error/unknown)
                 :message (.getMessage t)}}))))

(defn- error-data [t]
  (let [ed (ex-data t)]
    {:error/type (or (:error/type ed) :error/unknown)
     :message (.getMessage t)}))

(def ^:private db-state->state
  "The candidates.state vocabulary -> the machine states (the same
  mapping evoclj.evolution.candidate documents; replicated here for
  the harness's read-only candidate SELECT — the harness NEVER writes
  SQL, exactly like the cli layer)."
  {"materialized" :materialized
   "evaluating" :evaluation-pending
   "eligible" :evaluated
   "promoted" :promoted
   "rejected" :rejected
   "stale" :stale})

(defn- row->candidate [row]
  {:candidate/id (UUID/fromString (:id row))
   :parent/generation-id (:parent_generation_id row)
   :parent/genome-id (:parent_genome_id row)
   :candidate/genome-id (:genome_id row)
   :mutation/id (UUID/fromString (:mutation_id row))
   :evidence/id (:evidence_id row)
   :risk (keyword (:risk row))
   :state (get db-state->state (:state row))
   :created-at (Date/from (Instant/parse (:created_at row)))})

(defn- candidate-shape [c]
  (select-keys c [:candidate/id :parent/generation-id :parent/genome-id
                  :candidate/genome-id :mutation/id :evidence/id
                  :risk :state]))

(defn- candidates-for-generation [system generation-id]
  (->> (sqlite/query (cli-session/db-of system)
                     ["SELECT * FROM candidates
                       WHERE parent_generation_id = ?
                       ORDER BY created_at ASC, id ASC"
                      generation-id])
       (mapv row->candidate)))

(defn- evaluation-pending-for-generation [system generation-id]
  (vec (filter #(= :evaluation-pending (:state %))
               (candidates-for-generation system generation-id))))

;; ============================================================================
;; the three timed phases (the same public APIs the `cycle` command uses)
;; ============================================================================

(defn- evolve-phase
  "PHASE EVOLVE: evolution.core/propose-candidates! over the recorded
  Evolution set with the harness-injected deterministic Mutator."
  [system opts generation-id]
  (let [es (:evolution/system system)
        evolution-system (assoc es
                                :genome-loader
                                (fn []
                                  (let [genome-id (cli-session/generation-genome-id
                                                   system generation-id)]
                                    (cli-session/load-genome-for-execution
                                     (cli-session/resolve-bundle-root
                                      opts genome-id)))))
        request {:generation/id generation-id
                 :evidence-selector {:recent 3
                                     :include-successes 1
                                     :include-failures 2
                                     :include-high-cost 1}
                 :max-candidates 3}
        candidates (evolution/propose-candidates! evolution-system request)]
    {:candidates (vec candidates)
     :candidate-ids (mapv :candidate/id candidates)}))

(defn- evaluate-one
  "EVAL one :evaluation-pending candidate under one profile, passing
  the F2 collector into evaluate-candidate! (the Task A2 envelope)."
  [system opts generation-id c collector profile-id]
  (let [cid (:candidate/id c)
        parent-gen-id (:parent/generation-id c)
        parent-genome-id (cli-session/generation-genome-id system parent-gen-id)
        parent-root (cli-session/resolve-bundle-root opts parent-genome-id)
        candidate-root (cli-session/candidate-bundle-root
                        opts (:candidate/genome-id c))
        evaluator (cli-session/build-evaluator system parent-gen-id parent-root
                                               c candidate-root)
        evaluation (eval-core/evaluate-candidate! evaluator cid profile-id
                                                  collector)]
    {:candidate/id cid
     :evaluation/id (:evaluation/id evaluation)
     :eligibility (:eligibility evaluation)}))

(defn- compiled-resolution-id
  "The compiled ResolutionId of a candidate Genome bundle (compilation
  is the host's job — promote! never compiles)."
  [bundle-root]
  (:compiled/resolution-id
   (compiler/compile-genome (cli-session/load-genome-for-execution bundle-root)
                            cli-session/provider-catalog)))

(defn- store-candidate-genome-body!
  "Host bookkeeping the promotion phase requires: persist the
  candidate Genome's canonical body into the CAS under its content
  address so promote!'s integrity re-hash (Database Invariant 7)
  passes."
  [opts system genome-id]
  (cas/put-bytes! (cli-session/cas-of system)
                  (.getBytes (genome-index-bytes
                              (cli-session/load-genome-for-execution
                               (cli-session/resolve-bundle-root opts genome-id)))
                             StandardCharsets/UTF_8)
                  {}))

(defn- promote-one
  "PROMOTE one eligible candidate through promotion.promote/promote!
  (the atomic CURRENT compare-and-set — Global Constraint 15)."
  [system opts store cand-by-id ev]
  (let [cid (:candidate/id ev)
        c (get cand-by-id cid)
        parent-gen-id (:parent/generation-id c)
        op-session (cli-session/operator-session! opts system parent-gen-id)
        candidate-root (cli-session/candidate-bundle-root
                        opts (:candidate/genome-id c))
        _ (store-candidate-genome-body! opts system (:candidate/genome-id c))
        promotion-system {:store store
                          :resolution/id (compiled-resolution-id candidate-root)
                          :event/session-id op-session}
        result (promote/promote! promotion-system
                                 {:candidate-id cid
                                  :evaluation-id (:evaluation/id ev)
                                  :expected-parent-generation parent-gen-id})]
    {:candidate/id cid
     :status (:status result)
     :outcome result}))

;; ============================================================================
;; the honest provider/mode report
;; ============================================================================

(defn- provider-state
  "The honest provider section: :mode :fixture with the model
  endpoint/configured-models facts and the explicit note. A model is
  'configured' only when the kernel-owned registry has a live provider
  for it (an API key was found); on this host none are configured, so
  the harness reports no endpoint and runs the deterministic fixture
  cases."
  [system]
  (let [reg (:model/registry system)
        configured (if (instance? clojure.lang.Atom reg)
                     (model-registry/configured-models reg)
                     [])
        catalog-status (get-in system [:modelsdev/catalog :catalog/status])]
    {:mode :fixture
     :model/endpoint? (boolean (seq configured))
     :model/configured (vec configured)
     :catalog/status catalog-status
     :model/note (if (seq configured)
                   (str "configured model(s): " (str/join ", " configured)
                        " — the harness still runs its deterministic fixture "
                        "cases headlessly")
                   (str "NO MODEL ENDPOINT: the kernel-owned model registry "
                        "has no live provider (catalog "
                        (name (or catalog-status :unknown))
                        ", no API keys configured), so the harness fell back "
                        "to the fixture providers. The measured numbers below "
                        "are FIXTURE-mode timings, not real-model cycle "
                        "timings."))}))

;; ============================================================================
;; run-harness — the entry point the test drives
;; ============================================================================

(defn run-harness
  "Run the full evolve -> eval -> promote cycle headlessly on the
  fixture path and return the structured EDN report (see the namespace
  docstring for the contract). `opts`:

    :state-dir  optional; defaults to a fresh temp dir (deleted when
                :cleanup? is true — the default)
    :profile    the evaluation profile id (default :default-v1)
    :cleanup?   delete the temp state dir afterwards (default true)

  Every phase is timed with System/nanoTime and every F2 metric record
  (harness :cycle/* plus the Task A2 eval envelope) is collected into
  one collector, so the report's :f2/metrics round-trips as EDN."
  [opts]
  (let [state-dir (or (:state-dir opts) (temp-dir "evoclj-fullcycle-"))
        ;; thread the RESOLVED state-dir back into opts so every
        ;; cli-session helper (resolve-bundle-root, candidate-bundle-root,
        ;; operator-session!) resolves against the harness's state dir,
        ;; never the ./evoclj-state default
        opts (assoc opts :state-dir state-dir)
        cleanup? (get opts :cleanup? true)
        profile-id (or (:profile opts) :default-v1)
        generation-id "generation-1"
        ctx (-> (provision! state-dir)
                (record-evolution-set!))
        system (cli-session/build-system
                {:state-dir state-dir
                 :overrides (merge {:modelsdev/catalog {:timeout-ms 3000}}
                                   (cycle-overrides ctx))})
        store (cli-session/store-of system)
        collector (atom [])
        record-cycle! (fn [name scope scope-id value unit]
                        (f2/collect-metric!
                         collector
                         (f2/record-metric name scope scope-id value unit)))
        provider (provider-state system)
        t0 (System/nanoTime)]
    (try
      (let [evolve (timed (fn [] (evolve-phase system opts generation-id)))
            evolve-run (if (:error evolve)
                         {:run? true :error (:error evolve)
                          :wall-ms (:wall-ms evolve)}
                         (let [res (:result evolve)]
                           {:run? true
                            :candidates (count (:candidates res))
                            :candidate-ids (:candidate-ids res)
                            :wall-ms (:wall-ms evolve)}))
            pending (if (:error evolve)
                      []
                      (evaluation-pending-for-generation system generation-id))
            cand-by-id (into {} (map (fn [c] [(:candidate/id c) c])) pending)
            eval-phase (timed
                        (fn []
                          (mapv (fn [c]
                                  (let [r (timed (fn []
                                                   (evaluate-one system opts
                                                                 generation-id c
                                                                 collector
                                                                 profile-id)))]
                                    (cond-> {:candidate/id (:candidate/id c)
                                             :wall-ms (:wall-ms r)}
                                      (:result r)
                                      (assoc :evaluation/id
                                             (get-in r [:result :evaluation/id])
                                             :eligibility
                                             (get-in r [:result :eligibility]))
                                      (:error r) (assoc :error (:error r)))))
                                pending)))
            evals (or (:result eval-phase) [])
            eval-run (cond-> {:evaluated (count evals)
                              :eligible (count (filter #(true? (get-in %
                                                                  [:eligibility
                                                                   :eligible?]))
                                                       evals))
                              :failed (count (filter #(contains? % :error) evals))
                              :wall-ms (:wall-ms eval-phase)
                              :evaluations evals}
                       (:error eval-phase) (assoc :error (:error eval-phase)))
            passing (filterv #(and (contains? % :eligibility)
                                   (true? (get-in % [:eligibility :eligible?])))
                             evals)
            promote-phase (timed
                           (fn []
                             (mapv (fn [ev]
                                     (let [r (timed (fn []
                                                      (promote-one system opts
                                                                   store cand-by-id
                                                                   ev)))]
                                       (cond-> {:candidate/id (:candidate/id ev)
                                                :wall-ms (:wall-ms r)}
                                         (:result r)
                                         (assoc :status (get-in r [:result :status])
                                                :outcome (:result r))
                                         (:error r) (assoc :error (:error r)))))
                                   passing)))
            outcomes (or (:result promote-phase) [])
            promote-run (cond-> {:attempted (count outcomes)
                                 :promoted (count (filter #(= :promoted (:status %))
                                                          outcomes))
                                 :stale (count (filter #(= :stale (:status %))
                                                       outcomes))
                                 :failed (count (filter #(contains? % :error)
                                                        outcomes))
                                 :wall-ms (:wall-ms promote-phase)
                                 :outcomes outcomes}
                          (:error promote-phase) (assoc :error (:error promote-phase)))
            total-ms (quot (- (System/nanoTime) t0) 1000000)]
        (record-cycle! :cycle/evolve-ms :evolution generation-id
                       (:wall-ms evolve) :ms)
        (record-cycle! :cycle/eval-ms :eval generation-id
                       (:wall-ms eval-phase) :ms)
        (record-cycle! :cycle/promote-ms :runtime generation-id
                       (:wall-ms promote-phase) :ms)
        (record-cycle! :cycle/total-ms :runtime generation-id total-ms :ms)
        (record-cycle! :cycle/promoted? :runtime generation-id
                       (if (pos? (:promoted promote-run)) 1.0 0.0) :boolean)
        {:harness/name :full-cycle
         :harness/version 1
         :provider provider
         :state-dir state-dir
         :seed {:source seed-source
                :generation/id generation-id
                :genome/id (:genome-id ctx)}
         :phases {:evolve evolve-run
                  :eval eval-run
                  :promote promote-run}
         :cycle/wall-ms total-ms
         :f2/metrics (vec @collector)})
      (finally
        (when cleanup? (delete-tree! state-dir))))))

;; ============================================================================
;; EDN output + the script entry point
;; ============================================================================

(defn write-report!
  "Write `report` to `path` as structured EDN (pr-str; read back with
  clojure.edn/read-string it is identical). Returns `path`."
  [report path]
  (let [f (io/file path)]
    (when-let [p (.getParentFile f)]
      (.mkdirs p))
    (spit path (pr-str report))
    path))

(defn- parse-cli-args
  "Minimal option parsing for --state-dir <dir> and --out <file>;
  unknown options are ignored with a notice."
  [args]
  (loop [args args opts {}]
    (if-let [a (first args)]
      (case a
        "--state-dir" (recur (nnext args) (assoc opts :state-dir (second args)))
        "--out" (recur (nnext args) (assoc opts :out (second args)))
        (do (println (str "ignoring unknown option " a))
            (recur (rest args) opts)))
      opts)))

(defn -main
  "Run the harness from the command line:

      clojure -M scripts/full-cycle.clj [--state-dir <dir>] [--out <file>]

  Without --out the report is printed to stdout as EDN; with --out it
  is written to the file. Exits 0 on a complete run, 1 on an
  unexpected harness failure."
  [& args]
  (let [opts (parse-cli-args args)
        report (run-harness opts)]
    (if-let [out (:out opts)]
      (do (write-report! report out)
          (println (str "full-cycle report written to " out)))
      (prn report))
    (shutdown-agents)
    (System/exit 0)))

;; --- the script entry point ---------------------------------------------------
;;
;; `clojure -M scripts/full-cycle.clj` loads this file as a script and
;; runs the top-level form below. The test suite load-files the SAME
;; file in-process; to keep that load a pure definition (no harness
;; run, no System/exit), the test sets the system property
;; evoclj.harness.loaded-by-test before load-file and the runner is
;; gated on its absence.

(when (and (bound? #'*command-line-args*)
           (not (System/getProperty "evoclj.harness.loaded-by-test")))
  (let [code (try
               (let [opts (parse-cli-args (or (seq *command-line-args*) []))
                     report (run-harness opts)]
                 (if-let [out (:out opts)]
                   (do (write-report! report out)
                       (println (str "full-cycle report written to " out)))
                   (prn report))
                 0)
               (catch Throwable t
                 (prn {:error/type :harness/script-failure
                       :message (or (.getMessage t) (str t))})
                 1))]
    (shutdown-agents)
    (System/exit code)))
