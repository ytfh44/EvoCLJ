(ns evoclj.eval.harness-registry
  "Pure-function registry for hidden selection sets (隐藏评估集).

  Evolutionary evaluation in EvoCLJ relies on hidden selection sets that are
  kept invisible to executors, diagnosticians, and mutators. This namespace
  provides a pure data structure and a small set of pure functions for
  registering and querying those sets. There is no global state and no IO:
  every function takes an explicit `registry` (a vector of set records) and
  returns data, so callers compose it freely.

  A selection set record is a plain map:
    {:set/id               <kw>     ;; unique-ish identifier
     :set/source           <kw>     ;; e.g. :fixture (disk fixture) or :generator (programmatic)
     :set/version          <string> ;; version label
     :set/path             <string> ;; location of the set data
     :set/generation-scoped? <bool> ;; true = per-generation (ephemeral); false = cross-generation stable (long-term exam hall)}")

(defn register-set
  "Append `set-record` to `registry` (a vector) and return the new vector.
  Duplicate `:set/id` values are allowed: the record is simply appended, so
  callers may register a newer version alongside an older one."
  [registry set-record]
  (conj (vec registry) set-record))

(defn list-sets
  "Return the registry vector itself (the full ordered list of set records)."
  [registry]
  (vec registry))

(defn get-set
  "Return the record in `registry` whose `:set/id` equals `set-id`, or nil if none.
  When multiple records share the same id, the last (most recently registered)
  one wins."
  [registry set-id]
  (reduce (fn [_found rec]
            (if (= (:set/id rec) set-id)
              (reduced rec)
              nil))
          nil
          (rseq (vec registry))))

(defn active-sets
  "Return all cross-generation-stable sets: records whose
  `:set/generation-scoped?` is false. These do not change across generations
  and can serve as long-term, stable exam halls."
  [registry]
  (vec (filter (fn [rec] (not (:set/generation-scoped? rec))) registry)))
