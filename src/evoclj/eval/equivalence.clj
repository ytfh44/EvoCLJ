(ns evoclj.eval.equivalence
  "The kernel-side output-equivalence registry and its ONE resolution
  rule, shared by the G4 replay and G5 paired evaluators.

  A case declares :output/equiv? as

    - nil      => byte-identical output (the default oracle),
    - a fn     => used as-is (in-memory cases),
    - a keyword => looked up in the evaluator's :equivalence/by-keyword
      registry merged OVER default-equivalences (EDN-safe persisted
      cases, Global Constraint 22).

  An unknown keyword — or any other :output/equiv? shape — fails closed
  with the CALLING evaluator's error type: the two evaluators keep their
  own error contract (:eval/replay-equiv-unknown /
  :eval/paired-equiv-unknown), so the error keyword is a parameter of
  resolve-equiv, never a second copy of the rule."
  (:require [evoclj.kernel.error :as err]))

(def default-equivalences
  "The kernel-side equivalence registry. :equivalence/byte-identical is
  the default oracle; evaluator contexts may extend the registry via
  :equivalence/by-keyword."
  {:equivalence/byte-identical =})

(defn resolve-equiv
  "Resolve the case's :output/equiv? to a predicate fn: a declared fn
  is used as-is, a keyword is looked up in the evaluator's
  :equivalence/by-keyword merged over default-equivalences, and nil
  means byte-identical output. `error-type` is the calling evaluator's
  error keyword, carried verbatim on both failure branches."
  [error-type evaluator case-map]
  (let [e (:output/equiv? case-map)]
    (cond
      (nil? e) =
      (fn? e) e
      (keyword? e) (or (get (merge default-equivalences
                                   (:equivalence/by-keyword evaluator))
                            e)
                       (throw (err/error error-type
                                         "no equivalence predicate registered under this keyword"
                                         {:equivalence/keyword e})))
      :else (throw (err/error error-type
                              ":output/equiv? must be a fn, a keyword, or nil"
                              {:value (err/sanitize e)})))))
