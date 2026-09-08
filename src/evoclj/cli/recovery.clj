(ns evoclj.cli.recovery
  "The recovery-scan CLI command (feature O3): `evoclj recovery`.

  Runs the normative READ-ONLY store scan (store.recovery/
  scan-recovery-state), adds Work orphans from the sole durable
  lifecycle, and returns the combined report. The command writes
  nothing (both scans are read-only)."
  (:require [evoclj.cli.session :as session]
            [evoclj.store.recovery :as recovery]))

(defn recovery-scan!
  "evoclj recovery

  The READ-ONLY store integrity/recovery report (feature O3):
  {:orphaned-works [...] :missing-artifacts [...] :invalid-event-chains [...]
   :stale-candidates [...]} - the scan plus the Work orphan classification.
  Writes nothing."
  [opts]
  (let [system (session/build-system opts)
        store (session/store-of system)
        db (:sqlite store)
        report (recovery/scan-recovery-state db (:cas store))]
    (assoc report :orphaned-works (recovery/find-orphaned-works db))))
