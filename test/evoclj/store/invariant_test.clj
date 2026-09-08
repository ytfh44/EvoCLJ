(ns evoclj.store.invariant-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
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
(defn- setup-approved []
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
    s))

(deftest raw-and-forged-references-cannot-persist
  (let [s (fresh-store)]
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/propose! s (proposal "raw" "raw" "raw"))))
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/append-run! s "p1" {:kind :replay :result/ref "sha256:forged"})))
    (is (= 0 (count-rows s "invariant_proposals")))))
(deftest approval-binds-exact-selected-runs-and-g3
  (let [s (setup-approved)]
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/approve! s "p1" "reviewer2" {:replay/ref (proof! s {:other true}) :adversarial/ref (proof! s {:other true})})))
    (is (= 1 (count-rows s "invariant_decisions")))))
(deftest decisions-are-terminal-and-idempotent
  (let [s (fresh-store) e (proof! s {}) r (proof! s {}) a (proof! s {})]
    (invariant-store/propose! s (proposal e r a))
    (is (= :proposed (:status (invariant-store/get-proposal s "p1"))))
    (is (invariant-store/reject! s "p1" "reviewer" "no"))
    (is (= :rejected (:status (invariant-store/get-proposal s "p1"))))
    (is (thrown? clojure.lang.ExceptionInfo (invariant-store/approve! s "p1" "reviewer" {})))
    (is (invariant-store/reject! s "p1" "reviewer" "no"))
    (is (= 1 (count-rows s "invariant_decisions")))))
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