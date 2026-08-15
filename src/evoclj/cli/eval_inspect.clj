(ns evoclj.cli.eval-inspect
  "The evaluation-inspection CLI command (feature V3): `evoclj
  eval-inspect <evaluation-id>`.

  Reads the complete persisted record of ONE evaluation: the eval_runs
  row (candidate, profile, gates, summary, eligibility, status) plus
  every eval_results row (per-case gate verdicts joined to the hidden
  case refs). READ-ONLY — the CLI layer never writes the eval tables."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.cli.session :as session]
            [evoclj.kernel.error :as err])
  (:import (java.util UUID)))

(defn- positional
  [opts n]
  (let [pos (:positionals opts)]
    (or (nth pos n nil)
        (throw (err/error :cli/usage-invalid
                          "missing positional argument"
                          {:usage (str "expected " (inc n) " positional argument(s)")})))))

(defn- uuid-arg [s]
  (try (UUID/fromString (str s))
       (catch Exception _
         (throw (err/error :cli/usage-invalid
                           "expected a uuid"
                           {:value s})))))

(defn eval-inspect!
  "evoclj eval-inspect <evaluation-id>

  The complete persisted record of ONE evaluation (feature V3).
  Returns plain EDN-safe data; an unknown id returns
  {:evaluation/id <id> :found false}."
  [opts]
  (let [eid (uuid-arg (positional opts 0))
        system (session/build-system opts)
        db (session/db-of system)
        run (first (jdbc/query db
                               ["SELECT * FROM eval_runs WHERE id = ?"
                                (str eid)]))]
    (if-not run
      {:evaluation/id eid :found false}
      (let [rows (jdbc/query db
                            ["SELECT ec.case_ref AS case_ref, er.gate AS gate,
                              er.passed AS passed, er.metric AS metric,
                              er.detail AS detail
                              FROM eval_results er
                              JOIN eval_cases ec ON ec.id = er.case_id
                              WHERE er.eval_run_id = ?
                              ORDER BY er.id ASC"
                             (str eid)])
            results (mapv (fn [r]
                            {:case/ref (:case_ref r)
                             :gate (keyword (:gate r))
                             :passed (= 1 (:passed r))
                             :metric (some-> (:metric r) edn/read-string)
                             :detail (some-> (:detail r) edn/read-string)})
                           rows)]
        {:evaluation/id eid
         :found true
         :candidate/id (uuid-arg (:candidate_id run))
         :profile-id (:profile_id run)
         :status (keyword (:status run))
         :gates (edn/read-string (:gates run))
         :summary (edn/read-string (:summary run))
         :eligibility (edn/read-string (:eligibility run))
         :case-results results}))))
