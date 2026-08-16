(ns evoclj.cli.promotion
  "The promotion-facing CLI commands (Task 10.2): `promote`,
  `rollback`, and `lineage`.

  `promote` and `rollback` are the ONLY cli commands that move the
  CURRENT generation pointer, and both go exclusively through the
  public Promotion APIs (promotion.promote/promote! and
  promotion.rollback/rollback! — the atomic CURRENT compare-and-set,
  Global Constraint 15). This namespace contains no SQL of any kind
  and no dependency on the CURRENT machinery (no promotion.current
  alias): the current generation is read through the public recovery
  scan (evoclj.cli.session/current-generation-info) and the pointer
  moves only inside promote!/rollback!'s transactions. `lineage` is a
  read-only reconstruction through promotion.lineage/lineage, rendered
  as the Task D2 per-generation report (genome ids, the file-level
  diff summary vs the parent, the promotion reason, and the evidence
  provenance refs).

  The diff summary reuses evoclj.cli.evolution/diff-genomes — the
  same deterministic LCS machinery `candidate inspect --diff` (Task
  E3) uses — so the report never duplicates the diff algorithm; the
  full per-file line hunks belong to that command, and lineage shows
  only the stats + changed file paths (roadmap O5). cli.evolution
  does not require this namespace, so there is no require cycle."
  (:require [clojure.string :as str]
            [evoclj.cli.evolution :as evolution]
            [evoclj.cli.session :as session]
            [evoclj.compiler.core :as compiler]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.lineage :as lineage]
            [evoclj.promotion.promote :as promote]
            [evoclj.promotion.rollback :as rollback])
  (:import (java.util UUID)))

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

(defn- parse-keyword
  "Parse a keyword option value (':canary-regression' or
  'canary-regression')."
  [s]
  (let [t (str s)]
    (if (str/starts-with? t ":")
      (keyword (subs t 1))
      (keyword t))))

(defn- compiled-resolution-id
  "The compiled ResolutionId of a candidate Genome bundle (compilation
  is the host's job — promote! never compiles)."
  [bundle-root]
  (:compiled/resolution-id
   (compiler/compile-genome (session/load-genome-for-execution bundle-root)
                            session/provider-catalog)))

;; --- commands ----------------------------------------------------------------

(defn promote!
  "evoclj promote <candidate-id> --evaluation <id>

  Promote an :evaluated candidate through promotion.promote/promote!
  (the atomic CURRENT compare-and-set — Global Constraint 15). The
  CLI builds the promotion-system with the candidate Genome's compiled
  Resolution id and a fresh operator session pinned to the candidate's
  parent generation (the event anchor promote! requires). Returns the
  outcome ({:status :promoted :from :to} or {:status :stale ...})."
  [opts]
  (let [cand-id (uuid-arg (positional opts 0))
        eval-id (uuid-arg (required-opt opts :evaluation
                                        "evoclj promote <candidate-id> --evaluation <id>"))
        system (session/build-system opts)
        store (session/store-of system)
        c (candidate/find-candidate store cand-id)]
    (when-not c
      (throw (err/error :cli/candidate-not-found
                        "no candidate with this id"
                        {:candidate/id cand-id})))
    (let [parent-gen-id (:parent/generation-id c)
          op-session (session/operator-session! opts system parent-gen-id)
          candidate-root (session/candidate-bundle-root opts
                                                        (:candidate/genome-id c))
          promotion-system {:store store
                            :resolution/id (compiled-resolution-id candidate-root)
                            :event/session-id op-session}
          result (promote/promote! promotion-system
                                   {:candidate-id cand-id
                                    :evaluation-id eval-id
                                    :expected-parent-generation parent-gen-id})]
      result)))

(defn rollback!
  "evoclj rollback --to <generation-id> --reason <keyword>

  Move the CURRENT pointer back to a superseded generation through
  promotion.rollback/rollback! (selection-only — Global Constraint
  18; nothing is deleted and no external effect is compensated). The
  operator session is pinned to the CURRENT (from-) generation."
  [opts]
  (let [to (required-opt opts :to
                         "evoclj rollback --to <generation-id> --reason <keyword>")
        reason (required-opt opts :reason
                             "evoclj rollback --to <generation-id> --reason <keyword>")
        system (session/build-system opts)
        current (session/current-generation-info system)]
    (when-not current
      (throw (err/error :promotion/cas-invalid
                        "no CURRENT generation to roll back from"
                        {})))
    (let [from (:generation/id current)
          identity (session/generation-identity opts system from)
          op-session (session/operator-session! opts system from)
          promotion-system {:store (session/store-of system)
                            :resolution/id (:resolution/id identity)
                            :event/session-id op-session}
          result (rollback/rollback! promotion-system
                                     {:from-generation from
                                      :to-generation to
                                      :reason (parse-keyword reason)})]
      result)))

;; --- lineage report (Task D2, roadmap O5) ------------------------------------
;;
;; `lineage` renders the reconstruction (promotion.lineage/lineage) as
;; a per-generation REPORT: every node carries the genome ids of the
;; generation and its parent, the file-level diff SUMMARY vs the
;; parent (stats + changed files), the promotion reason, and the
;; evidence provenance refs (the evidence pack + the CAS artifact
;; refs backing the node). The raw lineage node fields (:generation,
;; :parent, :mutation, :evidence, :evaluation, :promotion, :children)
;; are preserved — the report only adds keys.

(defn- diff-stats
  "The file-level diff STATS of a diff-genomes report: the counts of
  differing files by status plus the total inserted/deleted lines
  (every hunk's right/left line counts — a removed file contributes
  deletions only, an added file insertions only)."
  [files]
  (let [status-count (fn [s] (count (filter #(= s (:status %)) files)))]
    {:files (count files)
     :added (status-count :added)
     :removed (status-count :removed)
     :changed (status-count :changed)
     :insertions (reduce + 0 (for [f files
                                   h (:hunks f)]
                               (count (:right/lines h))))
     :deletions (reduce + 0 (for [f files
                                  h (:hunks f)]
                              (count (:left/lines h))))}))

(defn- diff-summary
  "The file-level diff SUMMARY of the child Genome vs the parent
  Genome (both loaded bundles): the parent/child genome ids,
  :diff/identical?, the stats, and the changed file paths in canonical
  order. A summary, not the full hunks — the per-file line diff
  belongs to `candidate inspect --diff` (Task E3)."
  [parent child]
  (let [d (evolution/diff-genomes parent child)]
    {:parent/genome-id (:genome/id parent)
     :genome/id (:genome/id child)
     :diff/identical? (:diff/identical? d)
     :stats (diff-stats (:diff/files d))
     :files (mapv :file (:diff/files d))}))

(defn- diff-vs-parent
  "The file-level diff summary of a lineage node's generation vs its
  parent generation, both Genome bundles loaded from the CLI genome
  store (the same bundles `candidate inspect` resolves). Nil when the
  node has no generation (a rejected candidate branch — its candidate
  Genome is not exposed by the lineage record) or no parent (the
  seed). A bundle missing from the CLI store fails closed with
  :cli/genome-not-found — the diff cannot be shown (the E3
  candidate-inspect precedent)."
  [opts node]
  (let [genome-id (get-in node [:generation :genome/id])
        parent-id (get-in node [:parent :genome/id])]
    (when (and genome-id parent-id)
      (diff-summary
       (session/load-genome-for-execution
        (session/resolve-bundle-root opts parent-id))
       (session/load-genome-for-execution
        (session/resolve-bundle-root opts genome-id))))))

(defn- provenance
  "The evidence provenance refs of one lineage node: the frozen
  evidence pack content address (nil for the seed) plus every CAS
  artifact ref that backs the node's claims — the generation Genome,
  the parent Genome, the evidence pack, and the evaluation's
  paired-results ref when present. All are content addresses (Global
  Constraint 21) and resolve through the store CAS."
  [node]
  (let [refs (filterv some?
                      [(get-in node [:generation :genome/id])
                       (get-in node [:parent :genome/id])
                       (get-in node [:evidence :evidence/id])
                       (get-in node [:evaluation :paired-results-ref])])]
    {:evidence/id (get-in node [:evidence :evidence/id])
     :cas/refs (vec (distinct refs))}))

(defn- lineage-entry
  "One lineage node as the D2 per-generation report entry: the raw
  node fields plus the surfaced generation/genome ids (:generation/id,
  :genome/id, :parent/genome-id, :parent/generation-id), the promotion
  reason (:promotion/reason), the evidence provenance refs
  (:provenance), and — when both the generation and its parent exist —
  the file-level diff summary vs the parent (:diff)."
  [opts node]
  (let [genome-id (get-in node [:generation :genome/id])
        parent-id (get-in node [:parent :genome/id])]
    (cond-> node
      true (assoc :generation/id (get-in node [:generation :generation/id])
                  :genome/id genome-id
                  :parent/genome-id parent-id
                  :parent/generation-id (get-in node [:parent :generation/id])
                  :promotion/reason (get-in node [:promotion :reason])
                  :provenance (provenance node))
      (and genome-id parent-id) (assoc :diff (diff-vs-parent opts node)))))

(defn- lineage-report
  "The D2 lineage report: the full lineage tree with every node
  rendered as a per-generation entry, children recursively."
  [opts node]
  (-> (lineage-entry opts node)
      (update :children (fn [children]
                          (mapv #(lineage-report opts %) children)))))

(defn lineage!
  "evoclj lineage <generation-id>

  Reconstruct the complete evolutionary history of a generation
  (promotion.lineage/lineage — strict integrity verification over
  every referenced artifact) as the Task D2 per-generation report
  (roadmap O5). Every node carries:

    :generation/id, :genome/id, :parent/genome-id,
        :parent/generation-id — the ids of the generation and its
        parent (the seed has no parent; a rejected candidate branch
        has no generation).
    :diff — the file-level diff SUMMARY vs the parent: :stats (files/
        added/removed/changed/insertions/deletions) and :files (the
        changed file paths), computed from the bundles in the CLI
        genome store with the same machinery as `candidate inspect
        --diff` (Task E3). Absent for the seed and for rejected
        candidate branches (their candidate Genome is not exposed by
        the lineage record). A bundle missing from the CLI store fails
        closed with :cli/genome-not-found — the diff cannot be shown.
    :promotion/reason — the promotion decision's recorded reason (nil
        for the seed; the rejection reason on a rejected branch).
    :provenance — {:evidence/id <evidence pack ref> :cas/refs [...]}
        the evidence provenance refs: the frozen evidence pack and
        every CAS artifact ref backing the node (generation Genome,
        parent Genome, evidence pack, paired-results ref). All are
        content addresses (Global Constraint 21) and resolve through
        the store CAS.

  The raw lineage node fields (:generation, :parent, :mutation,
  :evidence, :evaluation, :promotion, :children) are preserved. EDN
  by default; --pretty renders the same data through the CLI's human
  renderer. Read-only — the reconstruction never writes."
  [opts]
  (let [generation (positional opts 0)
        system (session/build-system opts)
        store (session/store-of system)
        tree (lineage/lineage store generation)]
    (lineage-report opts tree)))
