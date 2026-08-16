(ns evoclj.eval.runner
  "The G5 per-side scheduler harness (Task 8.4).

  run-side! evaluates ONE side of ONE pair through the FULL scheduler
  stack — evoclj.runtime.scheduler/run-session! with FRESH temp stores
  (a migrated sqlite database and a CAS, created and disposed per
  side), a candidate Genome loaded and compiled FROM SCRATCH (Global
  Constraints 4, 6 — never a cached Mutator claim), a FRESH Phenotype
  (a fresh isolated SCI runtime carrying fresh session namespaces,
  Global Constraint 23), and a fresh pinned session. The pair's
  derived persisted seed (Step 1 — derived in evoclj.eval.paired) is
  handed to every fixture provider that accepts one, so parent and
  candidate of one pair observe the SAME deterministic fixture
  version wherever the provider supports determinism.

  ISOLATION (Global Constraints 11, 12, 23): this harness mounts NO
  dataset into any workspace and touches NO production state. The
  selection case body arrives already resolved by the evaluator
  (evoclj.eval.paired); the case's :task-input is the only part ever
  fed to the session (that is the prompt the candidate must solve);
  the case's :expected-output never enters this namespace's result.
  Every run happens against throwaway temp stores that are deleted in
  a finally block, so a candidate can never observe, modify, or
  persist anything outside its own side's scratch state.

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/paired-fixture-missing (a case tool has no fixture provider),
  :eval/paired-genome-unresolved (the evaluator cannot resolve a
  side's genome root). Scheduler/compiler/phenotype/store errors
  propagate as their own typed errors.

  Task A3 (Foundation F4): candidate-batch-tasks builds the run-batch!
  task maps for a batch of candidate ids — the eval layer's task
  contract for the worker pool (evoclj.eval.workers), consumed by
  evoclj.eval.core/evaluate-batch!. The batch reuses this harness's
  run-side! isolation (fresh throwaway stores per side, Global
  Constraints 11/12/23) for every candidate of every task; the pool
  adds only bounded concurrency, per-task timeout, and per-task error
  isolation on top.

  REAL MODEL EXECUTION (post-v0 extension 1): the G5 evaluator is
  OPTIONALLY augmented to run Genomes whose topology contains :llm
  nodes through real model providers, with both new keys reserved by
  the Task 8.7 contract:

    :model/registry  — the kernel-owned model registry atom (the
      result of evoclj.provider.model-registry/build-model-registry);
      when PRESENT the broker context is built with :model-registry
      injected AND a model lease for this side's exact phenotype id,
      so the llm node's :intent/model-call intents (attributed to the
      side phenotype) dispatch through dispatch-model-call! to real
      providers. When ABSENT no model lease and no :model-registry
      are injected — an :llm topology fails closed with the existing
      :provider/not-found :reason :no-model-registry (never a silent
      fallback); fixture-only Genomes are unaffected.
    :model/resource  — the model resource template the model lease
      grants, e.g. {:kind :model :id \"lmstudio/*\"} (the prefix the
      model leases grant). Optional even when :model/registry is
      present; defaults to {:kind :model :id \"*/*\"}."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.compiler.core :as compiler]
            [evoclj.genome.load :as load]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.usage :as usage]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util Date)))

;; --- temp stores (fresh scratch state per side, Global Constraint 23) ------

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifacts)."
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn temp-stores!
  "Fresh evaluation stores for ONE side run: a migrated sqlite database
  in a temp file, seeded with a generation row for `generation-id` (the
  pair's parent generation — both sides of a pair are evaluated in the
  SAME generation context), plus a fresh temp CAS root. Returns
  {:sqlite <db spec> :cas <cas> :paths [<temp paths>]}."
  [generation-id]
  (let [db-path (str (Files/createTempFile "evoclj-paired-" ".db"
                                           (make-array FileAttribute 0)))
        cas-path (str (Files/createTempDirectory "evoclj-paired-cas-"
                                                 (make-array FileAttribute 0)))
        db (sqlite/spec db-path)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id (str "sha256:" (apply str (repeat 64 "0")))
                     :resolution_id (str "sha256:" (apply str (repeat 64 "1")))
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at "2025-01-01T00:00:00Z"}))
    {:sqlite db :cas (cas/->cas cas-path) :paths [db-path cas-path]}))

(defn dispose-stores!
  "Delete the temp paths created by temp-stores! (idempotent)."
  [{:keys [paths]}]
  (doseq [p paths]
    (delete-tree! (Paths/get p (make-array String 0)))))

;; --- the harness ------------------------------------------------------------

(defn- program-registry
  "The side's program descriptor registry: the evaluator's :programs
  value, called with the loaded genome when it is a fn, returned as-is
  when it is a vector, and empty by default."
  [evaluator loaded]
  (let [p (:programs evaluator)]
    (cond
      (fn? p) (p loaded)
      (nil? p) []
      :else p)))

(defn- program-sources
  "Decode every compiled program's source text from the loaded bundle
  files (Global Constraint 22: the CompiledGenome carries only
  :source/digest references; the source text lives in the bundle)."
  [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (:bytes (get-in loaded [:files (:file descriptor)])))
                         StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- fixture-for
  "Resolve the fixture provider for one tool of the case: a
  :selection/fixtures value that is a fn of one arg is CALLED with the
  pair's derived seed (Step 1 — a deterministic fixture version shared
  by both sides of the pair); a provider value is used as-is (its
  determinism is its own). A case tool with no fixture fails closed —
  selection evaluation cannot stand in for it."
  [evaluator tool-id seed]
  (let [f (get-in evaluator [:selection/fixtures tool-id])]
    (when-not f
      (throw (err/error :eval/paired-fixture-missing
                        "no fixture provider registered for this selection tool"
                        {:tool/id tool-id})))
    (if (fn? f) (f seed) f)))

(defn- leases-for
  "One CapabilityLease per tool id in the case, granting this side's
  exact phenotype id the tool's :invoke action (the :required-action of
  every v0 descriptor) — the broker authorizes tool calls against these
  leases exactly as in a live run."
  [tool-ids phenotype-id]
  (let [now (Date.)
        expires (Date. (+ (.getTime now) 60000))]
    (mapv (fn [tool-id]
            {:cap/id (random-uuid)
             :subject {:phenotype/id phenotype-id}
             :resource {:kind :tool :id tool-id}
             :actions #{:invoke}
             :constraints {:max-calls 10000}
             :issued-at now
             :expires-at expires})
          tool-ids)))

(def ^:private default-model-resource
  "The model lease resource used when the evaluator supplies NO
  :model/resource: {:kind :model :id \"*/*\"}. Because the capability
  matcher treats a model grant ending in \"/*\" as the literal string
  prefix before it, the \"*/*\" grant becomes the prefix \"*\" — which
  no concrete model id (e.g. \"lmstudio/fake\") ever starts with — so
  this default matches NOTHING. It is a deliberately fail-closed
  default: enabling :model/registry without a :model/resource never
  accidentally over-grants, it simply lets the side fail closed with a
  model denial until the host wires a real provider prefix (e.g.
  {:kind :model :id \"lmstudio/*\"})."
  {:kind :model :id "*/*"})

(defn- model-lease
  "One CapabilityLease granting this side's exact phenotype id the
  :model resource's :invoke action. The llm node attributes its
  :intent/model-call intents to the side phenotype
  (:phenotype/id), so the lease subject MUST match it exactly —
  mirroring how leases-for grants tool leases with the same subject
  (Global Constraint 9: a sibling phenotype never resource-authorizes).
  :resource is the evaluator's :model/resource or
  default-model-resource; :max-calls is generous so legitimate
  evaluation loop traffic is never budget-starved mid-run."
  [phenotype-id resource]
  (let [now (Date.)
        expires (Date. (+ (.getTime now) 60000))]
    {:cap/id (random-uuid)
     :subject {:phenotype/id phenotype-id}
     :resource (or resource default-model-resource)
     :actions #{:invoke}
     :constraints {:max-calls 10000}
     :issued-at now
     :expires-at expires}))

(defn- create-pinned-session!
  "create-session! pinned to the compiled genome's identity, then
  append the :session/created root event (the host's job — the
  scheduler anchors its causal chain on it). Returns the session id."
  [stores compiled generation-id]
  (let [db (:sqlite stores)
        sid (:session/id
             (session/create-session!
              db
              {:genome/id (:compiled/genome-id compiled)
               :resolution/id (:compiled/resolution-id compiled)
               :phenotype/id (:compiled/phenotype-id compiled)
               :generation/id generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id generation-id
                          :phenotype/id (:compiled/phenotype-id compiled)
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

;; --- side usage (Task 12.1 counters, Feature C) ------------------------------

(defn- usage-from-output
  "Convert ONE side output value that carries model usage into a
  runtime.usage-shaped sample, or nil when it carries none. Each such
  output value is the provider result value of a model dispatch (e.g.
  {:model/output {...} :usage {:model-input-tokens 10
  :model-output-tokens 6} :model-cost-units 0.16}) — the Task 12.1
  model counters appear either nested under :usage (:model-input-tokens
  / :model-output-tokens) or at the value's own top level
  (:model-cost-units / :provider-reported-cost, both runtime.usage
  counter keys). Returns a counter-only sample for runtime.usage/add."
  [out]
  (when (map? out)
    (let [u (:usage out)
          has-usage? (and (map? u) (contains? u :model-input-tokens)
                          (contains? u :model-output-tokens))
          cost-units (or (get u :model-cost-units)
                         (:model-cost-units out)
                         (:provider-reported-cost out))
          reported (or (get u :provider-reported-cost)
                       (:provider-reported-cost out)
                       (:model-cost-units out))]
      (when (or has-usage?
                (contains? out :model-cost-units)
                (contains? out :provider-reported-cost))
        (cond-> {}
          has-usage? (assoc :model-input-tokens (:model-input-tokens u 0)
                            :model-output-tokens (:model-output-tokens u 0))
          (or has-usage?
              (contains? out :model-cost-units)
              (contains? out :provider-reported-cost))
          (assoc :model-cost-units (or cost-units 0.0)
                 :provider-reported-cost (or reported 0.0)))))))

(defn- side-usage
  "The runtime.usage-style sample for ONE side, attributed to the
  side's fresh session (:session/id — Global Constraint 20).
  Aggregated from (a) the model usage carried by every side output
  value (each output from a model dispatch carries :usage and/or
  :model-cost-units/:provider-reported-cost — scanned with
  usage-from-output and combined with runtime.usage/add), plus (b)
  :provider-calls (the total broker call count from the usage atom,
  which maps :cap/id -> count) and (c) :steps / :wall-ms when the
  scheduler result reports them (v0's scheduler result does not, so
  they are absent unless a future scheduler emits them).

  The map is ALWAYS present on the side result (consumers never
  special-case nil): when a run produced no model usage it aggregates
  to runtime.usage/empty-usage and carries only the :provider-calls
  counter and the :session/id attribution."
  [outputs usage-atom run sid]
  (let [model (reduce (fn [acc out]
                        (if-let [s (usage-from-output out)]
                          (usage/add acc s)
                          acc))
                      usage/empty-usage
                      (or outputs []))
        calls (reduce + 0 (vals @usage-atom))
        sample (cond-> model
                 (contains? run :steps) (assoc :steps (:steps run))
                 (contains? run :wall-ms) (assoc :wall-ms (:wall-ms run)))
        sample (assoc sample :provider-calls calls)]
    (usage/attributed sample {:session/id sid})))

(defn run-side!
  "Run ONE side of ONE pair through the full scheduler with fresh temp
  stores.

  `evaluator` is the G5 evaluator context (see evoclj.eval.paired for
  its contract — plus the optional G5 evaluator keys :model/registry
  (the kernel-owned model registry atom) and :model/resource (the model
  lease resource template), which switch on real model execution for
  :llm topologies (see the ns docstring). `opts` keys:

      :genome/root    <bundle directory path>   ; the side's Genome
      :side/kind      :parent | :candidate
      :side/id        <string>                  ; parent-generation |
                                                ;   candidate-id
      :generation/id  <string>                  ; the parent generation

  `case-map` is the resolved selection case (carrying :case/id,
  :task-input, :expected-output, :tools); `seed` is the pair's derived
  persisted seed (Step 1).

  Returns the side result map:

      {:side/kind ... :side/id ...
       :side/instance-id <uuid>      ; the FRESH Phenotype INSTANCE marker
       :side/phenotype-id <sha256>
       :side/session-id <uuid>       ; the fresh pinned session
       :side/status :completed | :failed | :budget-exhausted
       :side/output-ref <sha256 | nil>
       :side/outputs <vector | nil>  ; read back from the temp CAS
       :side/usage <runtime.usage sample>  ; ALWAYS present: the Task 12.1
       ;   model counters aggregated from the side outputs plus
       ;   :provider-calls (the usage atom's :cap/id -> count total),
       ;   attributed to the fresh session (see side-usage). A run with
       ;   no model usage carries empty counters.
       :side/error <artifact ref | nil>}  ; :failed only

  The side result carries NO case prompts and NO expected outputs."
  [evaluator opts case-map seed]
  (let [genome-root (:genome/root opts)
        side-kind (:side/kind opts)
        side-id (:side/id opts)
        generation-id (:generation/id opts)]
    (when-not genome-root
      (throw (err/error :eval/paired-genome-unresolved
                        "no genome bundle root for this side"
                        {:side/kind side-kind :side/id side-id})))
    (let [stores (temp-stores! generation-id)]
    (try
      (let [loaded (load/load-genome genome-root)
            compiled (compiler/compile-genome
                      (assoc loaded :programs (program-registry evaluator loaded))
                      (:provider/catalog evaluator))
            registry (registry/create-registry)
            tool-ids (sort (:tools case-map))
            _ (doseq [tool-id tool-ids]
                (registry/register! registry (fixture-for evaluator tool-id seed)))
            usage (atom {})
            leases (leases-for tool-ids (:compiled/phenotype-id compiled))
            model-registry (when (contains? evaluator :model/registry)
                             (:model/registry evaluator))
            leases (cond-> leases
                     model-registry
                     (conj (model-lease (:compiled/phenotype-id compiled)
                                        (:model/resource evaluator))))
            broker (dispatch/make-broker-context
                    (cond-> {:registry registry :leases leases :usage usage}
                      model-registry (assoc :model-registry model-registry)))
            ph (phenotype/instantiate
                compiled
                {:stores {:sqlite :poison :cas {:root :poison}}
                 :providers {:registry registry}
                 :capabilities {:leases leases :usage usage}
                 :program-sources (program-sources loaded compiled)})
            sid (create-pinned-session! stores compiled generation-id)
            run (scheduler/run-session!
                 {:phenotype ph
                  :stores {:sqlite (:sqlite stores) :cas (:cas stores)}
                  :dispatch broker}
                 sid (:task-input case-map))
            outputs (when (:output-ref run)
                      (edn/read-string
                       (String. (cas/get-bytes (:cas stores) (:output-ref run))
                                StandardCharsets/UTF_8)))]
        {:side/kind side-kind
         :side/id side-id
         :side/instance-id (random-uuid)
         :side/phenotype-id (:compiled/phenotype-id compiled)
         :side/session-id sid
         :side/status (:status run)
         :side/output-ref (:output-ref run)
         :side/outputs outputs
         :side/usage (side-usage outputs usage run sid)
         :side/error (:error/artifact-ref run)})
      (finally
        (dispose-stores! stores))))))

;; --- Task A3 — the batch task contract (Foundation F4) --------------------------

(defn candidate-batch-tasks
  "Build the run-batch! task maps for a batch of candidate ids (Task
  A3 — Foundation F4): one {:task/id <candidate-id> :candidate/id
  <candidate-id>} per id. :task/id is the stable per-candidate
  identity every batch entry carries (:task/index is force-set by
  run-batch! to the original position); :candidate/id is the payload
  key the batch task-runner evaluates. `candidate-ids` must be a
  sequential collection of distinct EDN-safe ids — the caller
  (evoclj.eval.core/evaluate-batch!) has already resolved them
  against the candidate store, so this stays pure."
  [candidate-ids]
  (mapv (fn [cid] {:task/id cid :candidate/id cid}) candidate-ids))
