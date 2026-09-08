(ns verify7-work-states
  "Semantic verification #7 — Work state-machine closure and legality.
  Work is the sole durable lifecycle: the seven states and transition table
  come from evoclj.runtime.work, which is also the store validation source."
  (:require [evoclj.runtime.work :as work]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def all-states work/work-states)
(def transitions work/work-transitions)
(def terminals work/terminal-states)

(check! "closure: every source and target is a known Work state"
        (work/edges-legal?)
        (pr-str (sort all-states)))

(check! "acyclic: Work transitions have no cycle"
        (work/acyclic?)
        (pr-str transitions))

(check! "terminals: every terminal Work state is a sink"
        (work/terminals-sink?)
        (pr-str terminals))

(check! "queued reaches succeeded"
        (work/queued->succeeded-path?)
        "a successful Work has a reachable terminal path")

(check! "queued reaches timed-out"
        (work/queued->timed-out-path?)
        "deadline expiry is reachable through running")

(doseq [from (sort all-states) to (sort all-states)]
  (check! (str "transition legality " from " -> " to)
          (= (work/valid-transition? from to)
             (contains? (get transitions from #{}) to))
          (str "expected " (contains? (get transitions from #{}) to))))

(check! "verification aggregate passes"
        (:pass? (work/verify-work-sm))
        (pr-str (work/verify-work-sm)))

(println "VERIFY7 DONE")
