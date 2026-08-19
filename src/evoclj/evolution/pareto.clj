(ns evoclj.evolution.pareto
  "Multi-objective Pareto archive (S3-1).

  A ParetoArchive tracks the non-dominated candidates across
  generations. Dominance is defined as:

      A dominates B  iff  (A >= B in ALL objectives) AND (A > B in AT
      LEAST ONE objective).

  For the v0 objectives:
    - :task/success — higher is better.
    - :cost        — lower is better (negated for comparison).
    - :complexity  — lower is better (negated for comparison).

  The archive is an in-memory vector of candidate-evaluation pairs.
  Every fn returns a new archive vector."

  (:require [evoclj.genome.hash :as hash]))

;; --- the archive --------------------------------------------------------------

(defn create-pareto-archive
  "Return an empty Pareto archive (an empty vector)."
  []
  [])

;; --- dominance ---------------------------------------------------------------

(defn- objective-score
  "Return the comparable score for `objective` in `scores-map`. For
  minimization objectives we negate so that 'greater is better' holds
  uniformly."
  [scores-map objective]
  (let [v (get scores-map objective)]
    (if (nil? v)
      ##-Inf
      (case objective
        :cost (- v)
        :complexity (- v)
        v))))

(defn dominates?
  "True when `a` dominates `b` under `objectives` (a seq of keywords).
  A dominates B when A is >= B in ALL objectives and > B in AT LEAST
  ONE objective."
  [a b objectives]
  (let [a-scores (mapv #(objective-score a %) objectives)
        b-scores (mapv #(objective-score b %) objectives)
        pairs (map vector a-scores b-scores)
        all-ge (every? (fn [[x y]] (<= y x)) pairs)
        any-gt (some (fn [[x y]] (< y x)) pairs)]
    (and all-ge any-gt)))

;; --- mutation ----------------------------------------------------------------

(defn add-candidate!
  "Add a candidate with its `scores` map to `archive`, returning the
  updated archive. When `archive` is an atom, swap! it and return the
  atom (side-effecting).

  - Remove any existing archive entries dominated by the new candidate.
  - If the new candidate is dominated by any existing entry, discard it
    and return the archive unchanged.
  - Otherwise, append the new candidate and return the filtered archive.

  `scores` is expected to carry at least the keys `:task/success`,
  `:cost`, and `:complexity` (all doubles). Missing keys are treated as
  ##-Inf, which makes domination unlikely for minimization objectives
  but possible for :task/success — callers should supply all three."
  [archive scores]
  (let [objectives [:task/success :cost :complexity]
        update (fn [arc]
                 (if (some #(dominates? % scores objectives) arc)
                   arc
                   (conj (vec (remove #(dominates? scores % objectives) arc)) scores)))]
    (if (instance? clojure.lang.IAtom archive)
      (do (swap! archive update) archive)
      (update archive))))

;; --- query --------------------------------------------------------------------

(defn frontier
  "Return the current non-dominated candidates (the Pareto frontier)
  as a vector of scores maps."
  [archive]
  (vec archive))
