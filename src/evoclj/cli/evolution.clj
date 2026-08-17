(ns evoclj.cli.evolution
  "The evolution-facing CLI commands (Task 10.2): `evolve`,
  `candidate list`, `candidate inspect` (with the Task E3 `--diff`
  report), and `eval`.

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
            [clojure.string :as str]
            [evoclj.cli.session :as session]
            [evoclj.compiler.core :as compiler]
            [evoclj.eval.core :as eval-core]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.core :as evolution]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.kernel.error :as err]
            [evoclj.config :as config]
             [evoclj.evolution.scheduler :as scheduler]
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

;; --- candidate diff report (Task E3, roadmap E3) -----------------------------
;;
;; `candidate inspect <id> --diff` shows the per-file LINE diff of the
;; candidate Genome vs its parent Genome. The comparison is the canonical
;; one (evoclj.genome.load :files keyed by canonical relative path);
;; identical files are absent from the report, so a diff of a genome
;; against itself is empty. Line hunks are computed with a deterministic
;; LCS line diff — no dependency beyond the existing clojure.string.

(defn- split-lines
  "`content` split into its lines (1-indexed by construction). An empty
  string is zero lines; a trailing newline yields no empty final line."
  [content]
  (if (empty? content)
    []
    (str/split-lines content)))

(defn- decode-text
  "The UTF-8 text of one loaded Genome file value (the :bytes are an
  immutable vector of bytes, as evoclj.genome.load documents)."
  [file-value]
  (String. (byte-array (:bytes file-value)) StandardCharsets/UTF_8))

(defn- line-diff
  "The deterministic LCS line diff of `left` and `right` (vectors of
  lines): a sequence of steps, each {:op :keep|:del|:add, :text <line>}
  plus the 1-based line number on the step's side (:left for :keep and
  :del, :right for :keep and :add). Ties in the LCS backtrack break
  toward the deletion (up) first, so identical inputs produce the same
  steps on every run."
  [left right]
  (let [m (count left)
        n (count right)
        table (make-array Integer/TYPE (inc m) (inc n))]
    (doseq [i (range (inc m))
            j (range (inc n))]
      (aset-int table i j
                (cond
                  (zero? i) 0
                  (zero? j) 0
                  (= (nth left (dec i)) (nth right (dec j)))
                  (inc (aget table (dec i) (dec j)))
                  :else
                  (max (aget table (dec i) j)
                       (aget table i (dec j))))))
    (loop [i m j n acc []]
      (cond
        (and (zero? i) (zero? j)) (vec (reverse acc))
        (zero? j)
        (recur (dec i) j
               (conj acc {:op :del :left i :text (nth left (dec i))}))
        (zero? i)
        (recur i (dec j)
               (conj acc {:op :add :right j :text (nth right (dec j))}))
        (= (nth left (dec i)) (nth right (dec j)))
        (recur (dec i) (dec j)
               (conj acc {:op :keep :left i :right j
                          :text (nth left (dec i))}))
        (>= (aget table (dec i) j) (aget table i (dec j)))
        (recur (dec i) j
               (conj acc {:op :del :left i :text (nth left (dec i))}))
        :else
        (recur i (dec j)
               (conj acc {:op :add :right j :text (nth right (dec j))}))))))

(defn- numbered-lines
  "`lines` as the report's line maps {:number <1-based> :text <line>}."
  [lines]
  (mapv (fn [i l] {:number (inc i) :text l})
        (range) lines))

(defn- hunks-of
  "Group the consecutive non-:keep steps of a line diff into hunks.
  Each hunk carries the first 1-based line on each side (:left/start,
  :right/start — nil when that side has no lines in the hunk) plus the
  per-side line maps."
  [steps]
  (->> steps
       (partition-by #(= :keep (:op %)))
       (remove (fn [g] (= :keep (:op (first g)))))
       (mapv (fn [g]
               (let [lefts (->> g
                                (filter #(= :del (:op %)))
                                (mapv (fn [s] {:number (:left s) :text (:text s)})))
                     rights (->> g
                                 (filter #(= :add (:op %)))
                                 (mapv (fn [s] {:number (:right s) :text (:text s)})))]
                 {:left/start (when (seq lefts) (:number (first lefts)))
                  :right/start (when (seq rights) (:number (first rights)))
                  :left/lines lefts
                  :right/lines rights})))))

(defn- file-diff
  "The per-file diff of one path present in at least one of the two
  genomes: :changed (both sides, line hunks), :added (right only), or
  :removed (left only)."
  [path left-text right-text]
  (cond
    (nil? left-text)
    {:file path
     :status :added
     :hunks [{:left/start nil :right/start 1
              :left/lines [] :right/lines (numbered-lines
                                            (split-lines right-text))}]}

    (nil? right-text)
    {:file path
     :status :removed
     :hunks [{:left/start 1 :right/start nil
              :left/lines (numbered-lines (split-lines left-text))
              :right/lines []}]}

    :else
    {:file path
     :status :changed
     :hunks (hunks-of (line-diff (split-lines left-text)
                                 (split-lines right-text)))}
    ))

(defn diff-genomes
  "The per-file LINE diff of two loaded immutable Genomes (Task E3):

    {:diff/identical? <bool>
     :diff/files [{:file <canonical relative path>
                   :status :added|:removed|:changed
                   :hunks [{:left/start <n|nil> :right/start <n|nil>
                            :left/lines [{:number <n> :text <s>} ...]
                            :right/lines [{:number <n> :text <s>} ...]} ...]} ...]}

  Files whose decoded text is identical in both Genomes are ABSENT from
  :diff/files — the report lists exactly the added, removed, and
  changed paths. :diff/identical? is true exactly when no file differs
  (which, for content-addressed Genomes, is the same as the two
  :genome/id values being equal). Read-only and deterministic."
  [left right]
  (let [lf (:files left)
        rf (:files right)
        paths (into (sorted-set) (concat (keys lf) (keys rf)))
        files (->> paths
                   (keep (fn [p]
                           (let [l (get lf p)
                                 r (get rf p)]
                             (cond
                               (and l r)
                               (let [lt (decode-text l)
                                     rt (decode-text r)]
                                 (when (not= lt rt)
                                   (file-diff p lt rt)))

                               l (file-diff p (decode-text l) nil)
                               r (file-diff p nil (decode-text r))
                               :else nil))))
                   vec)]
    {:diff/identical? (empty? files)
     :diff/files files}))

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
  "evoclj candidate inspect <id> [--diff]

  One Candidate record, through the public read API
  (evolution.candidate/find-candidate). With --diff, the report also
  carries the per-file line diff of the parent Genome vs the candidate
  Genome (Task E3): both bundles are loaded from the CLI genome store
  and compared with diff-genomes, so the diff is read-only and
  deterministic.

  NOTE on argument order: --diff is a read-only flag and the CLI parser
  consumes a following non-option token as its value, so it must trail
  the candidate id: `candidate inspect <id> --diff`."
  [opts]
  (let [cid (uuid-arg (positional opts 0))
        system (session/build-system opts)
        c (candidate/find-candidate (session/store-of system) cid)]
    (when-not c
      (throw (err/error :cli/candidate-not-found
                        "no candidate with this id"
                        {:candidate/id cid})))
    (if (:diff (:options opts))
      (let [parent (load/load-genome
                    (session/resolve-bundle-root opts (:parent/genome-id c)))
            candidate (load/load-genome
                       (session/resolve-bundle-root opts (:candidate/genome-id c)))]
        (merge (candidate-shape c) (diff-genomes parent candidate)))
      (candidate-shape c))))

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

;; --- loop: the long-horizon operator command (Task A2) ----------------------

(defn- load-loop-config
  "The F5 `:config/evolution-loop` section for `opts` — the loop-config
  `run-cycles!` consults. Read from the SAME config envelope the host
  builds (foundation F5: defaults deep-merged with the config source —
  the `:config` map / EDN string or the EVOCLJ_CONFIG file — then the
  selected profile resolved). The CLI's env scalar overrides and the
  :demo profile only touch `:config/budget`, so `:config/evolution-loop`
  is unaffected by them; this mirrors session/config-envelope's
  resolution of the envelope without duplicating its private helpers."
  [opts]
  (let [env (or (:env opts) (System/getenv))
        profile-key (some-> (or (:config/profile opts)
                                (get env "EVOCLJ_PROFILE"))
                            keyword)
        source (or (:config opts)
                   (when-let [f (get env "EVOCLJ_CONFIG")] (slurp f))
                   {})
        loaded (config/load-config source)
        profiled (if profile-key
                   (config/resolve-profile loaded profile-key)
                   loaded)]
    (:config/evolution-loop (config/validate-config! profiled))))

(defn- build-loop-evaluator
  "The evaluator value the scheduler runner reuses for EVERY candidate of
  the CURRENT generation. Mirrors session/build-evaluator (which `cycle`
  calls PER candidate) but folds all the generation's candidates into one
  `:genome/roots` map — run-pipeline's resolve-root! needs the parent
  generation root AND every candidate's root — so a single evaluator
  value serves the whole generation."
  [opts system generation-id]
  (let [es (:eval/system system)
        parent-genome-id (session/generation-genome-id system generation-id)
        parent-root (session/resolve-bundle-root opts parent-genome-id)
        cands (candidates-for-generation system generation-id)
        roots (into {generation-id parent-root}
                    (map (fn [c]
                           [(str (:candidate/id c))
                            (session/candidate-bundle-root opts (:candidate/genome-id c))])
                         cands))]
    {:store (:store es)
     :provider/catalog (:provider/catalog es)
     :kernel/abi (:kernel/abi es)
     :profiles (:profiles es)
     :genome/roots roots
     :selection/cases (:selection/cases es)
     :selection/fixtures (:selection/fixtures es)
     :replay/cases (:replay/cases es)
     :replay/fixtures (:replay/fixtures es)
     :programs (fn [_loaded] [session/route-descriptor])}))

(defn- build-loop-promotion-system
  "The promotion-system value the scheduler runner reuses (mirrors the
  per-candidate promotion-system `cycle` builds). Anchored to the
  CURRENT generation's operator session; the resolution id is compiled
  from the first candidate's bundle (a real loop promotes only when
  candidates exist, so the first candidate is representative). With no
  candidates yet a `:derive` placeholder is used — `promote!` is only
  reached for passing candidates, which implies candidates exist."
  [opts system generation-id]
  (let [store (session/store-of system)
        cands (candidates-for-generation system generation-id)
        first-cand (first cands)]
    (if first-cand
      (let [op-session (session/operator-session! opts system generation-id)
            candidate-root (session/candidate-bundle-root
                            opts (:candidate/genome-id first-cand))]
        {:store store
         :resolution/id (compiled-resolution-id candidate-root)
         :event/session-id op-session})
      {:store store
       :resolution/id nil
       :event/session-id :derive})))

(defn loop!
  "evoclj loop [--max-cycles <n>] [--no-promote] [--profile <profile-id>]

  The long-horizon operator command (Task A2): repeatedly walk the full
  loop — evolve → eval → promote — through the EXISTING single-generation
  public APIs, exactly as `cycle` does, but across MANY generations until
  the loop policy (evoclj.evolution.loop-policy) stops it.

  It reuses `cycle`'s host wiring — session/build-system, the F5 config
  envelope, the provider catalog, and the session helpers — to construct
  the production one-generation runner (scheduler/make-generation-runner)
  and drives it with scheduler/run-cycles!. `cycle` is NOT modified: this
  is additive wiring only.

  Options:
    --max-cycles <n>  hard safety cap on generations executed (default
                      1000, the same default run-cycles! applies).
    --no-promote      run evolve/eval but never move the CURRENT pointer;
                      the scheduler runner records would-be promotions.
    --profile <id>    evaluation profile id (default :default-v1).

  loop-config is the F5 `:config/evolution-loop` section
  (:max-generations :plateau-window :min-improvement :stop-on-regression?),
  read from the config envelope.

  Returns the scheduler's advisor map, printed as EDN by the CLI:
    {:generations [...] :final-decision <kw> :stop-reason <str>
     :cycles <int>}"
  [opts]
  (let [max-cycles (max-candidates-arg (get-in opts [:options :max-cycles]))
        no-promote? (boolean (get-in opts [:options :no-promote]))
        profile-id (or (some-> (get-in opts [:options :profile]) keyword)
                       :default-v1)
        system (session/build-system opts)
        es (:evolution/system system)
        ;; the CURRENT pointer read; nil when no generation is current
        ;; (the runner then throws :scheduler/no-current)
        current-id (fn [] (:generation/id (session/current-generation-info system)))
        gen-id (current-id)
        ;; the evolution-system's genome-loader resolves the CURRENT
        ;; generation's parent genome at CALL time (the pointer moves
        ;; between generations, so it reads live each step)
        evolution-system (assoc es
                                :genome-loader
                                (fn []
                                  (let [g (current-id)]
                                    (when-not g
                                      (throw (ex-info "no CURRENT generation to evolve"
                                                      {:error/type :scheduler/no-current})))
                                    (session/load-genome-for-execution
                                     (session/resolve-bundle-root
                                      opts (session/generation-genome-id system g))))))
        ;; the per-generation subsystem handles (built once from the
        ;; CURRENT generation present at command start; the live pointer
        ;; is re-read by current-id / candidates-for-generation each step)
        evaluator (when gen-id
                    (build-loop-evaluator opts system gen-id))
        promotion-system (when gen-id
                           (build-loop-promotion-system opts system gen-id))
        candidates-for-generation (fn [gid] (candidates-for-generation system gid))
        ctx {:evolution-system evolution-system
             :evaluator evaluator
             :promotion-system promotion-system
             :profile-id profile-id
             :current-generation-id current-id
             :candidates-for-generation candidates-for-generation
             :evidence-selector {:recent 3 :include-successes 1
                                 :include-failures 2 :include-high-cost 1}
             :max-candidates 3
             :no-promote? no-promote?}
        loop-config (load-loop-config opts)
        run-generation (scheduler/make-generation-runner ctx)]
    (scheduler/run-cycles! run-generation loop-config
                           :max-cycles (or max-cycles 1000))))

