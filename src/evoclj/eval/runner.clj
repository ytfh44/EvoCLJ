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
  propagate as their own typed errors."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.compiler.core :as compiler]
            [evoclj.genome.load :as load]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
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

(defn run-side!
  "Run ONE side of ONE pair through the full scheduler with fresh temp
  stores.

  `evaluator` is the G5 evaluator context (see evoclj.eval.paired for
  its contract). `opts` keys:

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
            broker (dispatch/make-broker-context
                    {:registry registry :leases leases :usage usage})
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
         :side/error (:error/artifact-ref run)})
      (finally
        (dispose-stores! stores))))))
