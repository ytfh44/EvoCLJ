(ns evoclj.store.invariant-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.static :as static]
            [evoclj.evolution.invariant :as evolution]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.existence :as existence]
            [evoclj.store.invariant :as invariant-store]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path Paths)
           (java.nio.file.attribute FileAttribute)))

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))
(defn- temp-db [] (let [p (str (Files/createTempFile "inv-" ".db" (make-array FileAttribute 0)))] (swap! db-paths conj p) p))
(defn- temp-cas [] (let [p (Files/createTempDirectory "inv-cas-" (make-array FileAttribute 0))] (swap! cas-roots conj p) p))
(defn- delete-tree! [^Path root] (when (Files/exists root (make-array LinkOption 0)) (doseq [f (reverse (file-seq (.toFile root)))] (Files/deleteIfExists (.toPath f)))))
(defn- cleanup! [] (doseq [p @db-paths] (Files/deleteIfExists (Paths/get p (make-array String 0)))) (reset! db-paths []) (doseq [p @cas-roots] (delete-tree! p)) (reset! cas-roots []))
(use-fixtures :each (fn [f] (static/clear-suites!) (try (f) (finally (static/clear-suites!) (cleanup!)))))
(defn- fresh-store [] (let [db (sqlite/spec (temp-db)) c (cas/->cas (str (temp-cas)))] (migrate/migrate! db) {:sqlite db :cas c}))
(defn- count-rows [s table] (:n (first (sqlite/query (:sqlite s) [(str "SELECT count(*) AS n FROM " table)]))))
(defn- proof! [s x]
  (let [b (.getBytes (pr-str x) StandardCharsets/UTF_8)
        o (cas/put-bytes! (:cas s) b {:media-type "application/edn"})
        aid (:artifact/id o)]
    (artifact/ensure-artifact! (:sqlite s) aid "application/edn" (:size o))
    (existence/verified-digest (:cas s) aid)))
(defn- base-predicate [] {:predicate/type :dsl :dsl {:op :equals :path [:state] :value :safe}})
(defn- proposal [e r a]
  {:proposal/id "p1" :proposer "owner" :scope :runtime :risk :low :version 1
   :registry/revision (static/registry-revision) :predicate (base-predicate)
   :evidence/refs [e] :replay/refs [r] :adversarial/refs [a]})
(defn- error-type
  "The :error/type carried by the ExceptionInfo thrown by thunk, or nil."
  [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))
(defn- approved-store-and-refs []
  (let [s (fresh-store)
        e (proof! s {:evidence :input})
        d (proof! s {:details :g3})
        t (proof! s {:target :candidate})
        pd (:predicate/digest (evolution/validate-predicate! (base-predicate)))
        revision (static/registry-revision)
        envelope (fn [kind] {:proposal/id "p1" :predicate/digest pd :registry/revision revision
                             :run/kind kind :target/digest (existence/digest-of t) :evaluation/id nil :candidate/id nil
                             :gate/id :G3-deterministic-suites :status :pass :details-ref (existence/digest-of d) :details/ref nil
                             :model/policy :recorded-only :deterministic? true :fresh-model? false :passed? true})
        r (proof! s (envelope :replay))
        a (proof! s (envelope :adversarial))
        p (invariant-store/propose! s (proposal e r a))]
    (invariant-store/append-run! s (:proposal/id p) {:run/id "r" :kind :replay :result/ref r})
    (invariant-store/append-run! s (:proposal/id p) {:run/id "a" :kind :adversarial :result/ref a})
    (invariant-store/approve! s "p1" "reviewer" {:replay/ref r :adversarial/ref a})
    {:store s :replay r :adversarial a}))
(defn- setup-approved [] (:store (approved-store-and-refs)))

(deftest raw-and-forged-references-cannot-persist
  (let [s (fresh-store)]
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/propose! s (proposal "raw" "raw" "raw"))))
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/append-run! s "p1" {:kind :replay :result/ref "sha256:forged"})))
    (is (= 0 (count-rows s "invariant_proposals")))))
(deftest approval-binds-exact-selected-runs-and-g3
  (let [s (setup-approved)]
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/approve! s "p1" "reviewer2" {:replay/ref (proof! s {:other true}) :adversarial/ref (proof! s {:other true})})))
    (is (= 1 (count-rows s "invariant_decisions")))))
(deftest forged-approved-decision-selected-ref-rejected-by-scan-and-activation
  (let [s (setup-approved)
        row (first (sqlite/query (:sqlite s) ["SELECT * FROM invariant_decisions WHERE proposal_id = 'p1'"]))
        original (edn/read-string (String. (cas/get-bytes (:cas s) (:decision_digest row)) StandardCharsets/UTF_8))
        forged (assoc original :replay/ref (:adversarial/ref original))
        forged-digest (existence/digest-of (proof! s forged))]
    ;; The append-only trigger is bypassed only inside this throwaway forged-row test.
    (sqlite/exec! (:sqlite s) ["DROP TRIGGER invariant_decisions_no_update"])
    (sqlite/exec! (:sqlite s) ["UPDATE invariant_decisions SET decision_digest = ? WHERE id = ?"
                                forged-digest (:id row)])
    (sqlite/exec! (:sqlite s) ["CREATE TRIGGER invariant_decisions_no_update
                                BEFORE UPDATE ON invariant_decisions
                                BEGIN SELECT RAISE(ABORT, 'invariant decisions are immutable'); END"])
    (is (thrown? clojure.lang.ExceptionInfo
                 (recovery/startup-integrity-scan (:sqlite s) (:cas s))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (invariant-store/activate! s "p1" "reviewer")))))
(deftest decisions-are-terminal-and-idempotent
  (let [s (fresh-store) e (proof! s {}) r (proof! s {}) a (proof! s {})]
    (invariant-store/propose! s (proposal e r a))
    (is (= :proposed (:status (invariant-store/get-proposal s "p1"))))
    (is (invariant-store/reject! s "p1" "reviewer" "no"))
    (is (= :rejected (:status (invariant-store/get-proposal s "p1"))))
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/approve! s "p1" "reviewer" {})))
    (is (invariant-store/reject! s "p1" "reviewer" "no"))
    (is (= 1 (count-rows s "invariant_decisions")))))
(deftest repeated-identical-decisions-return-the-existing-row
  (testing "a repeated identical approval is idempotent and returns the stored row"
    (let [{:keys [store replay adversarial]} (approved-store-and-refs)
          row (first (sqlite/query (:sqlite store) ["SELECT * FROM invariant_decisions WHERE proposal_id = 'p1'"]))
          again (invariant-store/approve! store "p1" "reviewer" {:replay/ref replay :adversarial/ref adversarial})]
      (is (= 1 (count-rows store "invariant_decisions")))
      (is (= (:id row) (:id again)))
      (is (= (:decision_digest row) (:decision_digest again)))))
  (testing "a repeated identical rejection is idempotent and returns the existing decision"
    (let [s (fresh-store) e (proof! s {}) r (proof! s {}) a (proof! s {})]
      (invariant-store/propose! s (proposal e r a))
      (invariant-store/reject! s "p1" "reviewer" "no")
      (let [row (first (sqlite/query (:sqlite s) ["SELECT * FROM invariant_decisions WHERE proposal_id = 'p1'"]))
            again (invariant-store/reject! s "p1" "reviewer" "no")]
        (is (= 1 (count-rows s "invariant_decisions")))
        (is (= (:id row) (:decision/id again)))
        (is (= (:decision_digest row) (:decision/digest again)))))))
(deftest a-different-decision-on-a-decided-proposal-fails-closed
  (testing "a second approval with different inputs is a typed conflict"
    (let [{:keys [store replay adversarial]} (approved-store-and-refs)]
      (is (= :invariant/decision-conflict
             (error-type #(invariant-store/approve! store "p1" "reviewer2" {:replay/ref replay :adversarial/ref adversarial}))))
      (is (= 1 (count-rows store "invariant_decisions")))))
  (testing "rejecting an already-approved proposal is a typed conflict"
    (let [store (setup-approved)]
      (is (= :invariant/decision-conflict
             (error-type #(invariant-store/reject! store "p1" "reviewer" "changed my mind"))))
      (is (= 1 (count-rows store "invariant_decisions"))))))
(deftest publication-requires-opaque-durable-proof
  (let [d {:invariant/id "x" :version 1 :predicate {:predicate/type :kernel-rule :rule/id :x} :registry/revision (static/registry-revision) :activation/committed? true}]
    (is (thrown? clojure.lang.ExceptionInfo (static/publish-active-invariant! d)))))
(deftest activation-disable-and-reactivation
  (let [s (setup-approved)]
    (is (= :approved (:status (invariant-store/get-proposal s "p1"))))
    (invariant-store/activate! s "p1" "reviewer")
    (is (= :active (:status (invariant-store/get-proposal s "p1"))))
    (is (invariant-store/disable! s "p1" "reviewer" "retired"))
    (is (= :disabled (:status (invariant-store/get-proposal s "p1"))))
    (is (invariant-store/disable! s "p1" "reviewer" "retired"))
    (is (= 1 (count-rows s "invariant_disables")))
    (is (= 2 (count-rows s "invariant_events")))
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/activate! s "p1" "reviewer")))))
(deftest disabled-terminal-activation-passes-recovery-scan
  (let [s (setup-approved)]
    (invariant-store/activate! s "p1" "reviewer")
    (invariant-store/disable! s "p1" "reviewer" "retired")
    (is (= :disabled (:status (invariant-store/get-proposal s "p1"))))
    (let [disable (first (sqlite/query (:sqlite s) ["SELECT * FROM invariant_disables WHERE proposal_id = 'p1'"]))
          event (first (sqlite/query (:sqlite s) ["SELECT * FROM invariant_events WHERE activation_id = (SELECT id FROM invariant_activations WHERE proposal_id = 'p1') AND event_type = 'disabled'"]))
          report (recovery/startup-integrity-scan (:sqlite s) (:cas s))]
      (is (= (:disable_digest disable) (:payload_ref event)))
      (is (true? (:ok? report)))
      (is (empty? (get-in report [:invariant-state :terminal-evidence-missing]))))))
(deftest durable-row-and-cas-authority-fails-closed
  (let [s (setup-approved)
        row (first (sqlite/query (:sqlite s) ["SELECT * FROM invariant_proposals WHERE id = 'p1'"]))
        unrelated (proof! s {:unrelated true})
        verify (deref (var invariant-store/verify-proposal-row!))]
    (is (thrown? clojure.lang.ExceptionInfo (verify s (assoc row :proposer "tampered"))))
    (is (thrown? clojure.lang.ExceptionInfo (verify s (assoc row :proposal_digest unrelated))))))

(deftest disable-only-current-active-and-exactly-idempotent
  (let [s (setup-approved)]
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/disable! s "p1" "reviewer" "before activation")))
    (invariant-store/activate! s "p1" "reviewer")
    (invariant-store/disable! s "p1" "reviewer" "retired")
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/disable! s "p1" "reviewer" "different")))))

(deftest disable-rejects-rejected-and-quarantined-terminal-states
  (let [s (fresh-store)
        e (proof! s {}) r (proof! s {}) a (proof! s {})]
    (invariant-store/propose! s (proposal e r a))
    (invariant-store/reject! s "p1" "reviewer" "no")
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/disable! s "p1" "reviewer" "late"))))
  (let [s (setup-approved)]
    (invariant-store/activate! s "p1" "reviewer")
    (let [row (first (sqlite/query (:sqlite s) ["SELECT * FROM invariant_activations WHERE proposal_id = 'p1'" ]))
          quarantine (deref (var invariant-store/quarantine-activation!))]
      (quarantine s row)
      (is (= :quarantined (:status (invariant-store/get-proposal s "p1"))))
      (is (thrown? clojure.lang.ExceptionInfo (invariant-store/disable! s "p1" "reviewer" "late"))))))
(deftest unavailable-scan-is-hard-in-strict-startup
  (let [s (fresh-store)]
    (with-redefs-fn {#'recovery/invariant-integrity (fn [_ _] (throw (java.sql.SQLException. "offline")))}
      #(is (thrown? clojure.lang.ExceptionInfo (recovery/startup-integrity-scan (:sqlite s) (:cas s)))))))
(deftest registry-revision-includes-immutable-suite-version
  (static/register-suite! {:suite/id :x :suite/type :unit :suite/version 1 :check (fn [_] nil)})
  (let [a (static/registry-revision)]
    (static/clear-suites!)
    (static/register-suite! {:suite/id :x :suite/type :unit :suite/version 2 :check (fn [_] nil)})
    (is (not= a (static/registry-revision)))))

(deftest partial-activation-recovery-quarantines
  (let [s (setup-approved)]
    (invariant-store/activate! s "p1" "reviewer")
    (sqlite/exec! (:sqlite s) ["DELETE FROM invariant_outbox WHERE event_type = 'activated'"])
    (static/clear-suites!)
    (let [result (invariant-store/recover-activations! s)]
      (is (= :quarantined (:status (first result))))
      (is (= :quarantined (:status (invariant-store/get-proposal s "p1"))))
      (is (empty? (static/active-invariants))))))
(deftest stray-disabled-event-outbox-cannot-start-or-recover
  (let [s (setup-approved)]
    (invariant-store/activate! s "p1" "reviewer")
    (let [activation (first (sqlite/query (:sqlite s)
                                          ["SELECT * FROM invariant_activations WHERE proposal_id = 'p1'"]))
          stray (existence/digest-of (proof! s {:status :disabled :stray true}))]
      (sqlite/exec! (:sqlite s)
                    ["INSERT INTO invariant_events (activation_id,event_type,payload_ref,created_at)
                      VALUES (?,?,?,?)"
                     (:id activation) "disabled" stray "2025-01-01T00:00:00Z"])
      (let [event (first (sqlite/query (:sqlite s)
                                       ["SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'disabled'"
                                        (:id activation)]))]
        (sqlite/exec! (:sqlite s)
                      ["INSERT INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at)
                        VALUES (?,?,?,?,0,?)"
                       (str (random-uuid)) (:id activation) (:id event) "disabled" "2025-01-01T00:00:00Z"])
        (is (thrown? clojure.lang.ExceptionInfo
                     (recovery/startup-integrity-scan (:sqlite s) (:cas s)))
            "startup scan rejects disabled evidence without a disable decision")
        (static/clear-active-invariants!)
        (is (thrown? clojure.lang.ExceptionInfo
                     (invariant-store/recover-activations! s))
            "recovery fails closed instead of publishing the stray-disabled activation")
        (is (empty? (static/active-invariants)))
        (is (empty? (sqlite/query (:sqlite s)
                                  ["SELECT id FROM invariant_disables WHERE proposal_id = 'p1'"]))
            "the fixture remains a stray event/outbox with no fabricated decision")
        (is (empty? (sqlite/query (:sqlite s)
                                  ["SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined'"
                                   (:id activation)]))
            "disabled and quarantined terminal evidence never coexist")))))

(deftest activation-envelope-id-must-match-durable-row
  (let [s (setup-approved)
        proposal (first (sqlite/query (:sqlite s)
                                      ["SELECT * FROM invariant_proposals WHERE id = 'p1'"]))
        decision (first (sqlite/query (:sqlite s)
                                      ["SELECT * FROM invariant_decisions WHERE proposal_id = 'p1'"]))
        aid "activation-id-row"
        activation-digest (existence/digest-of (proof! s {:activation/id "activation-id-envelope-mismatch"
                                     :proposal/id "p1"
                                     :decision/id (:id decision)
                                     :version 1
                                     :predicate (edn/read-string (:predicate_json proposal))
                                     :registry/revision (:registry_revision proposal)
                                     :reviewer "reviewer"}))]
    (sqlite/exec! (:sqlite s)
                  ["INSERT INTO invariant_activations
                    (id,proposal_id,decision_id,version,predicate_digest,activation_digest,registry_revision,status,committed_at)
                    VALUES (?,?,?,?,?,?,?,?,?)"
                   aid "p1" (:id decision) 1 (:predicate_digest proposal) activation-digest
                   (:registry_revision proposal) "active" "2025-01-01T00:00:00Z"])
    (sqlite/exec! (:sqlite s)
                  ["INSERT INTO invariant_events (activation_id,event_type,payload_ref,created_at)
                    VALUES (?,?,?,?)"
                   aid "activated" activation-digest "2025-01-01T00:00:00Z"])
    (let [event (first (sqlite/query (:sqlite s)
                                     ["SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'activated'"
                                      aid]))]
      (sqlite/exec! (:sqlite s)
                    ["INSERT INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at)
                      VALUES (?,?,?,?,0,?)"
                     (str (random-uuid)) aid (:id event) "activated" "2025-01-01T00:00:00Z"])
      (is (thrown? clojure.lang.ExceptionInfo
                   (recovery/startup-integrity-scan (:sqlite s) (:cas s)))
          "startup scan rejects an activation envelope bound to another id")
      (static/clear-active-invariants!)
      (let [result (invariant-store/recover-activations! s)]
        (is (= :quarantined (:status (first result))))
        (is (= aid (:activation/id (first result))))
        (is (empty? (static/active-invariants)))
        (is (= 1 (count (sqlite/query (:sqlite s)
                                      ["SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined'"
                                       aid])))
            "activation-id mismatch is durably quarantined")))))

(deftest run-digest-result-ref-must-match-sql-row
  (let [s (setup-approved)
        row (first (sqlite/query (:sqlite s)
                                 ["SELECT * FROM invariant_runs WHERE proposal_id = 'p1' AND kind = 'replay'"]))
        decoded (edn/read-string (String. (cas/get-bytes (:cas s) (:run_digest row)) StandardCharsets/UTF_8))
        rebound (first (sqlite/query (:sqlite s)
                                     ["SELECT * FROM invariant_runs WHERE proposal_id = 'p1' AND kind = 'adversarial'"]))
        forged (assoc decoded :result-ref (:result_ref rebound))
        forged-digest (existence/digest-of (proof! s forged))]
    (sqlite/exec! (:sqlite s) ["DROP TRIGGER invariant_runs_no_update"])
    (sqlite/exec! (:sqlite s) ["UPDATE invariant_runs SET run_digest = ? WHERE id = ?"
                                forged-digest (:id row)])
    (sqlite/exec! (:sqlite s) ["CREATE TRIGGER invariant_runs_no_update
                                BEFORE UPDATE ON invariant_runs BEGIN SELECT RAISE(ABORT, 'invariant runs are immutable'); END"])
    (is (thrown? clojure.lang.ExceptionInfo
                 (recovery/startup-integrity-scan (:sqlite s) (:cas s)))
        "startup scan rejects a run digest rebound to another SQL result")))

(deftest terminal-status-without-evidence-is-hard-finding
  (let [s (setup-approved)]
    (invariant-store/activate! s "p1" "reviewer")
    (let [activation (first (sqlite/query (:sqlite s)
                                          ["SELECT * FROM invariant_activations WHERE proposal_id = 'p1'"]))
          aid (:id activation)]
      (sqlite/exec! (:sqlite s) ["DROP TRIGGER invariant_activations_no_update"])
      (sqlite/exec! (:sqlite s) ["UPDATE invariant_activations SET status = 'disabled' WHERE id = ?" aid])
      (sqlite/exec! (:sqlite s) ["CREATE TRIGGER invariant_activations_no_update
                                  BEFORE UPDATE ON invariant_activations BEGIN SELECT RAISE(ABORT, 'invariant activations are immutable'); END"])
      (is (thrown? clojure.lang.ExceptionInfo
                   (invariant-store/verify-activation! s (assoc activation :status "disabled")))
          "non-active rows cannot pass active activation verification")
      (let [report (recovery/startup-integrity-scan (:sqlite s) (:cas s) {:strict? false})
            findings (get-in report [:invariant-state :terminal-evidence-missing])]
        (is (= [aid] (mapv :activation/id findings))
            "terminal rows without evidence remain visible to integrity scans")
        (is (thrown? clojure.lang.ExceptionInfo
                     (recovery/startup-integrity-scan (:sqlite s) (:cas s)))
            "malformed terminal status is hard in strict startup mode")))))