(ns evoclj.cli.eval-inspect
  "The evaluation-inspection CLI command (feature V3): `evoclj
  eval-inspect <evaluation-id>`.

  Reads the complete persisted record of ONE evaluation: the eval_runs
  row (candidate, profile, gates, summary, eligibility, status) plus
  every eval_results row (per-case gate verdicts joined to the hidden
  case refs). READ-ONLY — the CLI layer never writes the eval tables,
  and it decodes no eval column itself: the row decode belongs to
  evoclj.store.eval-read."
  (:require [evoclj.cli.session :as session]
            [evoclj.kernel.error :as err]
            [evoclj.store.eval-read :as eval-read])
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
        row (eval-read/find-evaluation-row db eid)]
    (if-not row
      {:evaluation/id eid :found false}
      (let [evaluation (eval-read/row->evaluation row)]
        {:evaluation/id eid
         :found true
         :candidate/id (:candidate/id evaluation)
         :profile-id (str (:profile/id evaluation))
         :status (keyword (:status row))
         :gates (:gates evaluation)
         :summary (:summary evaluation)
         :eligibility (:eligibility evaluation)
         :case-results (eval-read/case-results db eid)}))))
