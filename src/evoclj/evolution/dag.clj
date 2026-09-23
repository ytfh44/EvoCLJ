(ns evoclj.evolution.dag
  "Pure contracts for speculative evolution branches.

  This namespace deliberately contains no persistence or CURRENT-pointer
  logic. It provides bounded deterministic frontier selection for callers
  such as the scheduler: a stable candidate order (score descending, then
  digest/id bytes) and a bounded selection that retains rejected and
  unselected entries as evidence.

  v0 scope: merge plans and beam search are excluded by the v0 Non-Goals
  (docs/implementation-plan.md — genetic crossover/population algorithms
  beyond at most three sibling candidates) and are not part of this
  namespace.")

(defn- bytes-key [x]
  (let [a (.getBytes (str x) java.nio.charset.StandardCharsets/UTF_8)]
    (vec (map #(bit-and 0xff %) a))))

(defn deterministic-order
  "Stable candidate order: score descending, then digest, genome/id, and id bytewise."
  [candidates score-fn]
  (sort-by (fn [candidate]
             [(- (double (or (score-fn candidate) 0.0)))
              (bytes-key (or (:genome/digest candidate)
                             (:candidate/digest candidate)
                             (:digest candidate)
                             (:genome/id candidate)
                             (:candidate/genome-id candidate)
                             (:candidate/id candidate)
                             (:id candidate)
                             ""))
              (bytes-key (or (:candidate/id candidate) (:id candidate) ""))]) candidates))

(defn bounded-frontier
  "Select at most K deterministic candidates from a frontier under an eval budget.
  The result retains rejected/unselected entries as evidence and never mutates
  CURRENT or any store."
  [candidates {:keys [k frontier eval-budget score-fn seed]
               :or {k 1 eval-budget Long/MAX_VALUE score-fn (constantly 0.0)}}]
  (let [limit (long (max 0 (min (or k 1) (or frontier k 1))))
        ordered (vec (deterministic-order (take (long (max 0 eval-budget)) candidates) score-fn))]
    {:selected (vec (take limit ordered))
     :unselected (vec (drop limit ordered))
     :evaluated (count ordered)
     :eval-budget eval-budget
     :k limit
     :frontier (or frontier k 1)
     :seed seed}))
