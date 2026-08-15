(ns evoclj.cli.evolution
  "The evolution-facing CLI commands (Task 10.2): `evolve`,
  `candidate list`, `candidate inspect`, and `eval`.

  `evolve` and `eval` are the MUTATING commands of the evolution
  lifecycle; both go through the public subsystem entry points
  (evolution.core/propose-candidates! and eval.core/
  evaluate-candidate!) and never touch the CURRENT pointer (Global
  Constraint 15 — evolution has no activation rights). `candidate
  list` is the ONE read in the cli layer that has no public listing
  API; it uses the read-only SELECT helper of evoclj.cli.session over
  the candidates table ONLY (never generations, never a write) and
  maps the rows back to the public Candidate contract shape."
  (:require [clojure.edn :as edn]
            [evoclj.cli.session :as session]
            [evoclj.compiler.core :as compiler]
            [evoclj.eval.core :as eval-core]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.core :as evolution]
            [evoclj.genome.path :as gpath]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.promote :as promote]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util Date UUID)))

;; --- shared helpers ----------------------------------------------------------

(defn- positional
  [opts n]
  (let [pos (:positionals opts)]
    (or (nth pos n nil)
        (throw (err/error :cli/usage-invalid
                          "missing positional argument"
                          {:usage (str "expected " (inc n) " positional argument(s)")})))))

(defn- required-opt
  [opts k usage]
  (or (get-in opts [:options k])
      (throw (err/error :cli/usage-invalid
                        (str "missing required option --" (name k))
                        {:usage usage}))))

(defn- uuid-arg [s]
  (try (UUID/fromString (str s))
       (catch Exception _
         (throw (err/error :cli/usage-invalid
                           "expected a uuid"
                           {:value s})))))

;; --- candidate record mapping (Task 5.1 vocabulary → public states) ----------

(def ^:private db-state->state
  "The candidates.state vocabulary → the machine states (the same
  mapping evoclj.evolution.candidate documents; replicated here so
  `candidate list` can present the public Candidate contract)."
  {"materialized" :materialized
   "evaluating" :evaluation-pending
   "eligible" :evaluated
   "promoted" :promoted
   "rejected" :rejected
   "stale" :stale})

(defn- row->candidate
  "A candidates row as the public Candidate contract map."
  [row]
  {:candidate/id (UUID/fromString (:id row))
   :parent/generation-id (:parent_generation_id row)
   :parent/genome-id (:parent_genome_id row)
   :candidate/genome-id (:genome_id row)
   :mutation/id (UUID/fromString (:mutation_id row))
   :evidence/id (:evidence_id row)
   :risk (keyword (:risk row))
   :state (get db-state->state (:state row))
   :created-at (Date/from (Instant/parse (:created_at row)))})

(defn- candidate-shape
  "The concise Candidate record returned by the CLI commands (no
  timestamps in list output)."
  [c]
  (select-keys c [:candidate/id :parent/generation-id :parent/genome-id
                  :candidate/genome-id :mutation/id :evidence/id
                  :risk :state]))

;; --- commands ----------------------------------------------------------------

(defn evolve!
  "evoclj evolve --generation <id|current>

  Run one evolution proposal cycle (evolution.core/propose-candidates!
  — the public Evolution API) over the generation's store evidence.
  The CLI ships no Mutator adapter (:none — YAGNI, Global Constraint
  24), so v0 proposes nothing unless a host injected one through the
  config :overrides seam; the CURRENT pointer is never touched."
  [opts]
  (let [generation (required-opt opts :generation
                                 "evoclj evolve --generation <id|current>")
        system (session/build-system opts)
        es (:evolution/system system)
        ;; 'current' resolves to the concrete CURRENT generation id (the
        ;; orchestrator receives a concrete id); other ids pass through
        ;; so propose-candidates!'s own validation owns the error
        ;; contract (:evolution/generation-not-found)
        generation-id (if (= generation "current")
                        (if-let [cg (session/current-generation-info system)]
                          (:generation/id cg)
                          (throw (err/error :cli/generation-not-found
                                            "no CURRENT generation to evolve"
                                            {})))
                        generation)
        ;; the parent bundle is resolved lazily INSIDE the cycle (the
        ;; loader only runs after propose-candidates! verified the
        ;; generation row), so an unknown generation fails with the
        ;; orchestrator's own :evolution/generation-not-found
        evolution-system (assoc es
                                :genome-loader
                                (fn []
                                  (let [genome-id (session/generation-genome-id
                                                  system generation-id)]
                                    (session/load-genome-for-execution
                                     (session/resolve-bundle-root opts genome-id)))))
        request {:generation/id generation-id
                 :evidence-selector {:recent 3
                                     :include-successes 1
                                     :include-failures 2
                                     :include-high-cost 1}
                 :max-candidates 3}
        candidates (evolution/propose-candidates! evolution-system request)]
    {:generation/id generation-id
     :candidates (mapv candidate-shape candidates)}))

(defn candidate-list!
  "evoclj candidate list

  Every persisted Candidate record in creation order. This is the cli
  layer's only candidates-table read (see the namespace docstring)."
  [opts]
  (let [system (session/build-system opts)
        rows (sqlite/query (session/db-of system)
                           ["SELECT * FROM candidates
                             ORDER BY created_at ASC, id ASC"])]
    {:candidates (mapv (comp candidate-shape row->candidate) rows)}))

(defn candidate-inspect!
  "evoclj candidate inspect <id>

  One Candidate record, through the public read API
  (evolution.candidate/find-candidate)."
  [opts]
  (let [cid (uuid-arg (positional opts 0))
        system (session/build-system opts)
        c (candidate/find-candidate (session/store-of system) cid)]
    (when-not c
      (throw (err/error :cli/candidate-not-found
                        "no candidate with this id"
                        {:candidate/id cid})))
    (candidate-shape c)))

(defn eval!
  "evoclj eval <candidate-id> --profile <profile-id>

  Run the full Task 8.7 evaluation pipeline for one candidate under
  one registered profile (eval.core/evaluate-candidate! — the public
  Evaluation API) and return the finalized Evaluation record. The
  evaluator's hidden selection/replay cases and fixture providers are
  the host's injection (config :overrides); the CLI ships none in v0,
  so an un-injected eval fails closed with the evaluator's typed
  error."
  [opts]
  (let [cid (uuid-arg (positional opts 0))
        profile-id (or (some-> (get-in opts [:options :profile]) keyword)
                       :default-v1)
        system (session/build-system opts)
        store (session/store-of system)
        c (candidate/find-candidate store cid)]
    (when-not c
      (throw (err/error :cli/candidate-not-found
                        "no candidate with this id"
                        {:candidate/id cid})))
    (let [parent-gen-id (:parent/generation-id c)
          parent-genome-id (session/generation-genome-id system parent-gen-id)
          parent-root (session/resolve-bundle-root opts parent-genome-id)
          candidate-root (session/candidate-bundle-root opts
                                                        (:candidate/genome-id c))
          evaluator (session/build-evaluator system parent-gen-id parent-root
                                             c candidate-root)
          evaluation (eval-core/evaluate-candidate! evaluator cid profile-id)]
      {:evaluation evaluation})))

;; --- cycle: one operator command walking the full loop ----------------------

(defn- candidates-for-generation
  "Every persisted Candidate record for `generation-id`, in creation
  order (the cli layer's read-only candidates SELECT by parent
  generation). Read-only."
  [system generation-id]
  (->> (sqlite/query (session/db-of system)
                     ["SELECT * FROM candidates
                       WHERE parent_generation_id = ?
                       ORDER BY created_at ASC, id ASC"
                      generation-id])
       (mapv row->candidate)))

(defn- evaluation-pending-for-generation
  "The Candidate records of `generation-id` still in
  :evaluation-pending (the :candidates rows whose state maps to
  :evaluation-pending), in creation order."
  [system generation-id]
  (vec (filter #(= :evaluation-pending (:state %))
               (candidates-for-generation system generation-id))))

(defn- max-candidates-arg
  "Parse --max-candidates into a positive int (nil when absent)."
  [s]
  (when s
    (try
      (let [n (Long/parseLong (str s))]
        (when (pos? n) n))
      (catch Exception _
        (throw (err/error :cli/usage-invalid
                          "expected a positive integer for --max-candidates"
                          {:value s}))))))

(defn- error-data
  "The sanitizable {\":error/type\" \":message\"} of a thrown value (plain
  EDN-safe data — Global Constraint 22)."
  [t]
  (let [ed (ex-data t)]
    {:error/type (or (:error/type ed) :error/unknown)
     :message (.getMessage t)}))

(defn- genome-index-body
  "The canonical CAS body of a loaded Genome — the exact serialization
  of evoclj.genome.hash/tree-digest (path + NUL + digest + LF per
  entry, sorted bytewise) whose SHA-256 is the genome's content
  address. This is the body promote!'s integrity re-hash reads back
  (Database Invariant 7)."
  [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by (fn [[p _]] p) gpath/bytewise-compare (:files loaded)))))

(defn- store-candidate-genome-body!
  "Host bookkeeping the promotion phase requires: persist the candidate
  Genome's canonical body into the CAS under its content address so
  promote!'s integrity re-hash (Database Invariant 7) passes. Standalone
  `promote` assumes an operator already provisioned this; cycle, as the
  self-contained orchestrator, ensures it before promoting."
  [opts system genome-id]
  (cas/put-bytes! (session/cas-of system)
                  (.getBytes (genome-index-body
                              (session/load-genome-for-execution
                               (session/resolve-bundle-root opts genome-id)))
                             StandardCharsets/UTF_8)
                  {}))

(defn- compiled-resolution-id
  "The compiled ResolutionId of a candidate Genome bundle (compilation
  is the host's job — promote! never compiles; mirrors cli/promotion.clj)."
  [bundle-root]
  (:compiled/resolution-id
   (compiler/compile-genome (session/load-genome-for-execution bundle-root)
                            session/provider-catalog)))

(defn cycle!
  "evoclj cycle [--generation <id|current>] [--profile <profile-id>]
        [--max-candidates <n>] [--evolve] [--no-promote]

  ONE operator command that walks the full loop — evolve → eval →
  promote — and returns a structured EDN-safe report. It is a CLI
  orchestration, not a daemon: evolution.core/propose-candidates!,
  eval.core/evaluate-candidate!, and promotion.promote/promote! stay
  the authoritative subsystems, and the only pointer that moves is the
  atomic CURRENT compare-and-set inside promote! (Global Constraint
  15).

  Phases:
    - EVOLVE runs exactly what `evolve` runs (propose-candidates! with
      the same evidence-selector), but ONLY when --evolve is given OR no
      :evaluation-pending candidate exists for the generation (so a
      rerun evaluates pre-existing candidates without proposing a
      duplicate cycle).
    - EVAL evaluates every :evaluation-pending candidate of the
      generation under one profile (default :default-v1) and collects the
      Evaluation record.
    - PROMOTE runs cli/promotion.clj's promote! pipeline for every
      evaluation whose eligibility :eligible? is exactly true (reading
      the :eligibility {:eligible? bool :reasons [...]} verdict of the
      Evaluation record), SKIPPING the pointer move when --no-promote is
      given (the report documents the would-be promotion).

  A failed eval or failed promote for ONE candidate never aborts the
  cycle: the failure is collected into the report as per-candidate
  evidence. A broken system map or a genuinely missing candidate id
  remains a hard typed :cli/* error.
  Report: {:generation/id ... :phases {:evolve {:run? bool :candidates [...]}
                                       :eval [{:candidate/id :evaluation/id
                                               :eligibility {...} | :error {...}} ...]
                                       :promote [{:candidate/id :status :outcome
                                                  :error} ...]}}"
  [opts]
  (let [generation (or (get-in opts [:options :generation]) "current")
        profile-id (or (some-> (get-in opts [:options :profile]) keyword)
                       :default-v1)
        max-candidates (max-candidates-arg (get-in opts [:options :max-candidates]))
        evolve? (boolean (get-in opts [:options :evolve]))
        no-promote? (boolean (get-in opts [:options :no-promote]))
        system (session/build-system opts)
        es (:evolution/system system)
        generation-id (if (= generation "current")
                        (if-let [cg (session/current-generation-info system)]
                          (:generation/id cg)
                          (throw (err/error :cli/generation-not-found
                                            "no CURRENT generation to cycle"
                                            {})))
                        generation)
        store (session/store-of system)]
    (let [evolve-run? (or evolve? (empty? (evaluation-pending-for-generation
                                           system generation-id)))
          evolve-report
          (if evolve-run?
            (let [evolution-system (assoc es
                                           :genome-loader
                                           (fn []
                                             (let [genome-id (session/generation-genome-id
                                                             system generation-id)]
                                               (session/load-genome-for-execution
                                                (session/resolve-bundle-root opts genome-id)))))
                  request {:generation/id generation-id
                           :evidence-selector {:recent 3
                                               :include-successes 1
                                               :include-failures 2
                                               :include-high-cost 1}
                           :max-candidates (or max-candidates 3)}
                  candidates (evolution/propose-candidates!
                              evolution-system request)]
              {:run? true :candidates (mapv candidate-shape candidates)})
            {:run? false :candidates []})
          pending (evaluation-pending-for-generation system generation-id)
          cand-by-id (into {} (map (fn [c] [(:candidate/id c) c])) pending)]
      (let [evals (mapv (fn [c]
                          (let [cid (:candidate/id c)]
                            (try
                              (let [parent-gen-id (:parent/generation-id c)
                                    parent-genome-id (session/generation-genome-id
                                                      system parent-gen-id)
                                    parent-root (session/resolve-bundle-root
                                                 opts parent-genome-id)
                                    candidate-root (session/candidate-bundle-root
                                                    opts (:candidate/genome-id c))
                                    evaluator (session/build-evaluator
                                               system parent-gen-id parent-root
                                               c candidate-root)
                                    evaluation (eval-core/evaluate-candidate!
                                                evaluator cid profile-id)]
                                {:candidate/id cid
                                 :evaluation/id (:evaluation/id evaluation)
                                 :eligibility (:eligibility evaluation)})
                              (catch Exception t
                                {:candidate/id cid :error (error-data t)}))))
                        pending)
            passing (filterv #(and (contains? % :eligibility)
                                   (true? (get-in % [:eligibility :eligible?])))
                             evals)
            promotes (mapv (fn [e]
                             (let [cid (:candidate/id e)]
                               (when-not (contains? cand-by-id cid)
                                 (throw (err/error :cli/candidate-not-found
                                                   "no candidate with this id for promotion"
                                                   {:candidate/id cid})))
                               (if no-promote?
                                 {:candidate/id cid
                                  :status :skipped
                                  :reason :no-promote
                                  :eligible? true}
                                 (try
                                   (let [c (get cand-by-id cid)
                                         parent-gen-id (:parent/generation-id c)
                                         op-session (session/operator-session!
                                                     opts system parent-gen-id)
                                         candidate-root (session/candidate-bundle-root
                                                         opts (:candidate/genome-id c))
                                         _ (store-candidate-genome-body! opts system
                                                                        (:candidate/genome-id c))
                                         promotion-system {:store store
                                                           :resolution/id (compiled-resolution-id
                                                                           candidate-root)
                                                           :event/session-id op-session}
                                         result (promote/promote!
                                                 promotion-system
                                                 {:candidate-id cid
                                                  :evaluation-id (:evaluation/id e)
                                                  :expected-parent-generation parent-gen-id})]
                                     {:candidate/id cid
                                      :status (:status result)
                                      :outcome result})
                                   (catch Exception t
                                     {:candidate/id cid :status :error
                                      :error (error-data t)})))))
                           passing)]
        {:generation/id generation-id
         :phases {:evolve evolve-report
                  :eval evals
                  :promote promotes}}))))

