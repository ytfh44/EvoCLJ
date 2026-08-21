(ns evoclj.eval.cost-guard
  "Pure cost hard-stop guard for the evolution loop (component).

   The guard answers a single question: has the evolution loop spent
   past its budget? It is a PURE function — no IO, no state, no side
   effects — so the same inputs always yield the same output and it can
   be tested and reasoned about in isolation. E3 will call it from
   `evoclj.evolution.scheduler/run-cycles!` to decide whether to halt
   cycling."
  (:require [evoclj.kernel.error :as err]))

(defn should-stop?
  "Pure cost hard-stop check (component).

   Input:
     - `cumulative-cost` — the accumulated cost so far (number).
     - `threshold` — the `:max-cost` value from config (number).

   Returns:
     - `:stop` when `cumulative-cost` strictly exceeds `threshold`.
     - `:continue` otherwise.

   This is a PURE function: no IO, no state, no side effects. The same
   inputs always yield the same output.

   The comparison is strict: only a cumulative cost STRICTLY GREATER
   than the threshold triggers `:stop`. Equal or below yields `:continue`.

   Throws `:eval/cost-guard-invalid` if either argument is not a number."
  [cumulative-cost threshold]
  (when-not (number? cumulative-cost)
    (throw (err/error
             :eval/cost-guard-invalid
             "cumulative-cost must be a number"
             {:cumulative-cost cumulative-cost})))
  (when-not (number? threshold)
    (throw (err/error
             :eval/cost-guard-invalid
             "threshold must be a number"
             {:threshold threshold})))
  (if (> cumulative-cost threshold)
    :stop
    :continue))
