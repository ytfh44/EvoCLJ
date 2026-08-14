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
  (:require [evoclj.cli.session :as session]
            [evoclj.eval.core :as eval-core]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.core :as evolution]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
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
