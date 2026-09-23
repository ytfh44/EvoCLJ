(ns evoclj.store.eval-read
  "The eval-table READ API (component): the single decoder for persisted
  eval_runs rows and the join that resolves eval_results rows to their
  hidden case refs.

  Why this namespace exists: the eval tables are read by both
  evoclj.eval.core (the Evaluation record contract) and the CLI's
  eval-inspect command. Decoding the five EDN columns
  (:gates :summary :eligibility and the per-result :metric/:detail) in
  two places is how a display path drifts from the record contract, so
  the decode lives here once and both callers consume it.

  Boundary — eval_results/eval_cases have NO production writer today:
  evoclj.eval.core writes eval_runs only (the v0 pipeline records
  per-case verdicts inside the :gates and :summary artifacts, not as
  rows). `case-results` therefore returns an empty vector for every
  production run; it exists so the inspection path has a store-owned
  reader when a writer lands. Adding such a writer is a new feature, not
  part of this read API.

  Error contract: none of its own — the readers return plain data (nil /
  empty vector when nothing matches) and let store/sqlite errors
  propagate as-is (Global Constraint 22: plain serializable data)."
  (:require [clojure.edn :as edn]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.util Date UUID)))

(defn row->evaluation
  "Convert an eval_runs row into the public Evaluation record
  (evoclj.eval.core/EvaluationSchema). This is the ONLY decoder for the
  row's EDN columns."
  [row]
  {:evaluation/id (UUID/fromString (:id row))
   :candidate/id (UUID/fromString (:candidate_id row))
   :parent/generation-id (:parent_generation_id row)
   :profile/id (keyword (subs (:profile_id row) 1))
   :gates (edn/read-string (:gates row))
   :paired-results-ref (:paired_results_ref row)
   :summary (edn/read-string (:summary row))
   :eligibility (edn/read-string (:eligibility row))
   :created-at (Date/from (Instant/parse (:created_at row)))})

(defn find-evaluation-row
  "The RAW eval_runs row for `evaluation-id`, or nil. Callers that need
  only the Evaluation record use `find-evaluation`; this exists for the
  callers that also report row-level columns (e.g. the eval-inspect
  command's :status)."
  [db evaluation-id]
  (first (sqlite/query (sqlite/db-spec db)
                       ["SELECT * FROM eval_runs WHERE id = ?"
                        (str evaluation-id)])))

(defn find-evaluation
  "The public Evaluation record for `evaluation-id`, or nil when no
  evaluation has that id. Read-only."
  [db evaluation-id]
  (some-> (find-evaluation-row db evaluation-id)
          row->evaluation))

(defn find-evaluations-by-candidate
  "Every finalized Evaluation record for a candidate, in creation order.
  Read-only."
  [db candidate-id]
  (->> (sqlite/query (sqlite/db-spec db)
                     ["SELECT * FROM eval_runs WHERE candidate_id = ?
                       ORDER BY created_at ASC, id ASC"
                      (str candidate-id)])
       (mapv row->evaluation)))

(defn case-results
  "Every eval_results row of one evaluation, joined to its hidden case
  ref and decoded, in insertion order:

      [{:case/ref <string> :gate <keyword> :passed <boolean>
        :metric <edn|nil> :detail <edn|nil>} ...]

  Empty for every production run — no production code writes
  eval_results (see the namespace docstring)."
  [db eval-run-id]
  (->> (sqlite/query (sqlite/db-spec db)
                     ["SELECT ec.case_ref AS case_ref, er.gate AS gate,
                       er.passed AS passed, er.metric AS metric,
                       er.detail AS detail
                       FROM eval_results er
                       JOIN eval_cases ec ON ec.id = er.case_id
                       WHERE er.eval_run_id = ?
                       ORDER BY er.id ASC"
                      (str eval-run-id)])
       (mapv (fn [r]
               {:case/ref (:case_ref r)
                :gate (keyword (:gate r))
                :passed (= 1 (:passed r))
                :metric (some-> (:metric r) edn/read-string)
                :detail (some-> (:detail r) edn/read-string)}))))
