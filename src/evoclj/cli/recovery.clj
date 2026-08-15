(ns evoclj.cli.recovery
  "The recovery-scan CLI command (feature O3): `evoclj recovery`.

  Runs the normative READ-ONLY store scan (store.recovery/
  scan-recovery-state) and returns its report: orphaned sessions,
  missing artifacts, invalid event chains, and stale candidates.
  The command writes nothing (the scan never writes)."
  (:require [evoclj.cli.session :as session]
            [evoclj.store.recovery :as recovery]))

(defn recovery-scan!
  "evoclj recovery

  The READ-ONLY store integrity/recovery report (feature O3):
  {:orphaned-sessions [...] :missing-artifacts [...]
   :invalid-event-chains [...] :stale-candidates [...]} - the exact
  scan-recovery-state contract. Writes nothing."
  [opts]
  (let [system (session/build-system opts)
        store (session/store-of system)]
    (recovery/scan-recovery-state (:sqlite store) (:cas store))))
