(ns evoclj.promotion.monitor-test
  "Task 9.4 tests: online canary guardrails and automatic stop.

  The guardrails (Task 9.4, normative):

      hard policy violation          → HARD (Step 1): ONE violation
                                       immediately stops new sessions
                                       from entering the candidate.
                                       Hard violations are never
                                       aggregated into a weighted score
                                       (Global Constraint 14).
      provider denial surge          → SOFT (Step 2): a rate over the
                                       observation window
      session failure rate           → SOFT (Step 2)
      cost/task                      → SOFT (Step 2)
      latency/task                   → SOFT (Step 2)
      operator escalation rate       → SOFT (Step 2)

  Soft guardrails act ONLY once :min-samples observations are in hand —
  a noisy small sample must not trigger a stop.

  The numbered steps:

  - Step 1: one hard safety violation stops immediately — even with a
    single observation and far below :min-samples.
  - Step 2: soft thresholds need :min-samples before acting; at or
    above :min-samples an exceeded rate/average stops with the
    offending guardrail named.
  - Step 3: stopping changes routing for FUTURE sessions only
    (deactivate-canary routes every new key to the current generation
    while a pre-existing candidate session stays pinned); what happens
    to already-running candidate sessions follows the profile
    (:cancel marks them :cancelled via the store transition, :finish
    leaves them running) and is recorded per session.
  - Step 4: the stop reason and observed metrics are persisted as
    promotion evidence: a :promotion/canary-stopped event whose
    metadata carries the reason and whose :payload-ref points at a CAS
    metrics artifact holding the full evidence pack (Global Constraint
    21 — the row references, it never duplicates).

  Fresh temp databases are migrated from the classpath migrations and
  deleted after every test; the CAS root is a throwaway temp dir."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.promotion.canary :as canary]
            [evoclj.promotion.monitor :as monitor]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixtures -------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private g42 "G42")
(def ^:private g43 "G43")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (Files/createTempFile "evoclj-monitor-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-cas-root
  "A throwaway CAS root directory in the system temp dir."
  []
  (let [p (str (Files/createTempDirectory "evoclj-monitor-cas-"
                                          (make-array FileAttribute 0)))]
    (swap! cas-roots conj p)
    p))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifact
  bodies/meta files)."
  [p]
  (let [path (Paths/get p (make-array String 0))]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (with-open [stream (Files/walk path (make-array java.nio.file.FileVisitOption 0))]
        (doseq [q (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists q))))))

(defn- cleanup!
  "Delete every temp db file and CAS root created during this run."
  []
  (doseq [p @db-paths]
    (Files/deleteIfExists (Paths/get p (make-array String 0))))
  (doseq [p @cas-roots]
    (delete-tree! p))
  (reset! db-paths [])
  (reset! cas-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- fresh-cas
  "A fresh CAS backed by a throwaway temp root."
  []
  (cas/->cas (temp-cas-root) {}))

(defn- seed-generations!
  "Insert the CURRENT G42 row (current = 1) and the canary G43 row
  (current = 0) sessions can be pinned to."
  [db]
  (sqlite/with-db [conn db]
    (doseq [[g current] [[g42 1] [g43 0]]]
      (jdbc/insert! conn :generations
                    {:id g
                     :genome_id genome
                     :resolution_id resolution
                     :parent_id nil
                     :state "active"
                     :current current
                     :created_at now}))))

(defn- operator-session!
  "Create an operator session pinned to the CURRENT generation and
  append its :session/created root event (the host's job — stop-canary!
  anchors the :promotion/canary-stopped event to this session).
  Returns the session id."
  [db]
  (let [sid (:session/id
             (session/create-session!
              db
              {:genome/id genome
               :resolution/id resolution
               :phenotype/id phenotype
               :generation/id g42}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id g42
                          :phenotype/id phenotype
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- running-session!
  "Create a candidate session pinned to the CANARY generation G43 and
  transition it :created → :resolving → :running (an already-running
  candidate session). Returns the session id."
  [db]
  (let [sid (:session/id
             (session/create-session!
              db
              {:genome/id genome
               :resolution/id resolution
               :phenotype/id phenotype
               :generation/id g43}))]
    (session/transition-session! db sid :created :resolving nil)
    (session/transition-session! db sid :resolving :running nil)
    sid))

(defn- deployment-state
  "The Task 9.3 deployment-state shape with an ACTIVE G43 canary;
  callers merge overrides."
  [& [overrides]]
  (merge {:current-generation g42
          :canary {:generation g43
                   :allocation 0.10
                   :ladder [0.10 0.25 0.50 1.0]
                   :version "v1"}
          :active? true}
         overrides))

(defn- thresholds
  "The Task 9.4 thresholds; callers merge overrides."
  [& [overrides]]
  (merge {:min-samples 20
          :failure-rate 0.25
          :cost-per-task 0.10
          :latency-per-task 1000.0
          :provider-denial-rate 0.10
          :operator-escalation-rate 0.05}
         overrides))

(defn- observation
  "One session observation map; callers merge overrides."
  [session-id & [overrides]]
  (merge {:session/id session-id
          :outcome :ok
          :hard-violations []
          :provider-denials 0
          :operator-escalations 0
          :cost/task 0.01
          :latency/task 100.0}
         overrides))

(defn- n-observations
  "n observations (distinct session ids), each with `overrides`
  merged in."
  [n & [overrides]]
  (mapv (fn [_] (observation (java.util.UUID/randomUUID) overrides))
        (range n)))

(defn- stop-system
  "The stop-canary! system contract for the operator session and the
  running candidate sessions."
  [db cas-root operator-sid running-sids]
  {:store {:sqlite db :cas cas-root}
   :event/session-id operator-sid
   :canary/generation g43
   :running/session-ids running-sids})

(defn- error-type
  "The :error/type of the ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

;; ============================================================================
;; Step 1 — ONE hard safety violation immediately stops new sessions from
;;          entering the candidate (not aggregated — Global Constraint 14)
;; ============================================================================

(deftest one-hard-violation-stops-immediately
  (let [sid (java.util.UUID/randomUUID)
        decision (monitor/decide
                  [(observation sid {:hard-violations [{:rule :policy/pii-leak
                                                        :detail "candidate leaked"}]})]
                  (thresholds))]
    (testing "a single hard violation stops, even with ONE observation"
      (is (true? (:stop? decision))))
    (testing "hard violations are not aggregated and ignore :min-samples"
      (is (< (:samples (:evidence decision)) (:min-samples (thresholds)))))
    (testing "the reason names the hard guardrail and the violating session"
      (is (= {:guardrail :hard-policy-violation
              :kind :hard
              :detail {:session/id sid
                       :violation {:rule :policy/pii-leak
                                   :detail "candidate leaked"}}}
             (:reason decision))))
    (testing "the violating observation is part of the evidence"
      (is (= 1 (:samples (:evidence decision)))))))

(deftest hard-violation-wins-over-soft-thresholds
  (let [sid (java.util.UUID/randomUUID)
        obs (n-observations 25 {:outcome :failed
                                :cost/task 5.0
                                :latency/task 9000.0})]
    (testing "with a soft threshold ALSO exceeded, the hard violation is the reason"
      (let [decision (monitor/decide
                      (conj obs (observation sid {:hard-violations [{:rule :policy/x}]}))
                      (thresholds))]
        (is (true? (:stop? decision)))
        (is (= :hard-policy-violation (get-in decision [:reason :guardrail])))))))

;; ============================================================================
;; Step 2 — soft thresholds require a MINIMUM SAMPLE COUNT before acting
;; ============================================================================

(deftest soft-thresholds-require-minimum-samples
  (let [th (thresholds)
        failed (fn [n] (n-observations n {:outcome :failed}))]
    (testing "19 failed sessions (below :min-samples 20) do NOT stop"
      (let [decision (monitor/decide (failed 19) th)]
        (is (false? (:stop? decision)))
        (is (nil? (:reason decision)))))
    (testing "a single failed session (maximally noisy small sample) does NOT stop"
      (is (false? (:stop? (monitor/decide (failed 1) th)))))
    (testing "20 failed sessions (at :min-samples) stop with :failure-rate"
      (let [decision (monitor/decide (failed 20) th)]
        (is (true? (:stop? decision)))
        (is (= {:guardrail :failure-rate :kind :soft
                :detail {:observed 1.0 :threshold 0.25 :samples 20}}
               (:reason decision)))))))

(deftest cost-and-latency-thresholds-need-samples-and-name-the-guardrail
  (testing "cost/task above threshold at :min-samples stops with :cost-per-task"
    (let [decision (monitor/decide
                    (n-observations 20 {:cost/task 0.5 :latency/task 50.0})
                    (thresholds))]
      (is (true? (:stop? decision)))
      (is (= :cost-per-task (get-in decision [:reason :guardrail])))))
  (testing "latency/task above threshold at :min-samples stops with :latency-per-task"
    (let [decision (monitor/decide
                    (n-observations 20 {:cost/task 0.01 :latency/task 5000.0})
                    (thresholds))]
      (is (true? (:stop? decision)))
      (is (= :latency-per-task (get-in decision [:reason :guardrail])))))
  (testing "provider-denial and operator-escalation rates are soft too"
    (let [denials (monitor/decide (n-observations 20 {:provider-denials 5})
                                  (thresholds))
          escalations (monitor/decide (n-observations 20 {:operator-escalations 3})
                                      (thresholds))]
      (is (= :provider-denial-rate (get-in denials [:reason :guardrail])))
      (is (= :operator-escalation-rate (get-in escalations [:reason :guardrail])))))
  (testing "at :min-samples with every soft metric below threshold, no stop"
    (let [decision (monitor/decide (n-observations 20) (thresholds))]
      (is (false? (:stop? decision)))
      (is (nil? (:reason decision)))))
  (testing "below :min-samples, an extreme cost does NOT stop"
    (let [decision (monitor/decide (n-observations 19 {:cost/task 50.0})
                                   (thresholds))]
      (is (false? (:stop? decision))))))

;; ============================================================================
;; Step 3 — stopping changes routing for FUTURE sessions only; what happens
;;          to already-running candidate sessions follows the profile
;; ============================================================================

(deftest deactivate-canary-changes-routing-for-future-sessions-only
  (let [db (fresh-db)
        _ (seed-generations! db)
        running-sid (running-session! db)
        ds (deployment-state)
        inactive (monitor/deactivate-canary ds)]
    (testing "deactivation is the :active? false routing state (Task 9.3)"
      (is (false? (:active? inactive)))
      (is (= (:current-generation ds) (:current-generation inactive))))
    (testing "every new key now routes to the current generation, never the canary"
      (let [keys (mapv #(format "session-key-%05d" %) (range 10000))]
        (is (every? #(= g42 (canary/select-generation-for-new-session inactive %))
                    keys))))
    (testing "the canary generation is never selected for a new session"
      (is (not-any? #(= g43 (canary/select-generation-for-new-session inactive %))
                    (mapv #(format "session-key-%05d" %) (range 10000))))))
  (testing "a session already pinned to the canary stays pinned (store-level pin)"
    (let [db (fresh-db)
          _ (seed-generations! db)
          running-sid (running-session! db)]
      (is (= g43 (:generation/id (session/get-session db running-sid)))))))

(defn- hard-stop-decision
  "A decision whose reason is a hard policy violation."
  []
  (monitor/decide [(observation (java.util.UUID/randomUUID)
                               {:hard-violations [{:rule :policy/x}]})]
                  (thresholds)))

(deftest cancel-profile-cancels-running-sessions-and-records-it
  (let [db (fresh-db)
        cas-root (temp-cas-root)
        _ (seed-generations! db)
        operator-sid (operator-session! db)
        running-sids [(running-session! db) (running-session! db)]
        decision (hard-stop-decision)
        result (monitor/stop-canary!
                (stop-system db cas-root operator-sid running-sids)
                decision :cancel)]
    (testing "every already-running candidate session was marked :cancelled"
      (doseq [sid running-sids]
        (is (= :cancelled (:state (session/get-session db sid))))))
    (testing "the per-session action is recorded"
      (is (= (set (map #(hash-map :session/id % :action :cancelled) running-sids))
             (set (:running/actions result)))))
    (testing "the routing result records the canary is off"
      (is (= {:canary-active? false} (:routing result))))))

(deftest finish-profile-leaves-running-sessions-running
  (let [db (fresh-db)
        cas-root (temp-cas-root)
        _ (seed-generations! db)
        operator-sid (operator-session! db)
        running-sids [(running-session! db)]
        result (monitor/stop-canary!
                (stop-system db cas-root operator-sid running-sids)
                (hard-stop-decision) :finish)]
    (testing "running candidate sessions are left running under :finish"
      (is (= :running (:state (session/get-session db (first running-sids))))))
    (testing "the recorded action says :finish"
      (is (= [{:session/id (first running-sids) :action :finish}]
             (:running/actions result))))))

(deftest cancel-skips-sessions-that-are-no-longer-running
  (let [db (fresh-db)
        cas-root (temp-cas-root)
        _ (seed-generations! db)
        operator-sid (operator-session! db)
        finished-sid (running-session! db)
        _ (session/transition-session! db finished-sid :running :waiting nil)
        _ (session/transition-session! db finished-sid :waiting :completed nil)
        result (monitor/stop-canary!
                (stop-system db cas-root operator-sid [finished-sid])
                (hard-stop-decision) :cancel)]
    (testing "an already-completed session is recorded :skipped, not cancelled"
      (is (= [{:session/id finished-sid :action :skipped
               :actual-state :completed}]
             (:running/actions result))))
    (testing "the completed session stays completed"
      (is (= :completed (:state (session/get-session db finished-sid)))))))

;; ============================================================================
;; Step 4 — persist the stop reason and observed metrics as promotion evidence
;; ============================================================================

(deftest stop-persists-reason-and-metrics-as-promotion-evidence
  (let [db (fresh-db)
        cas-root (temp-cas-root)
        _ (seed-generations! db)
        operator-sid (operator-session! db)
        decision (hard-stop-decision)
        result (monitor/stop-canary!
                (stop-system db cas-root operator-sid [])
                decision :cancel)]
    (testing "a :promotion/canary-stopped event exists on the operator session"
      (let [stopped (first (event/events-by-type db operator-sid
                                                 :promotion/canary-stopped))]
        (is (some? stopped))
        (testing "the event carries the stop reason, profile, and metrics ref"
          (is (= (:reason decision) (:reason (:metadata stopped))))
          (is (= :cancel (:profile (:metadata stopped))))
          (is (= g43 (:canary/generation (:metadata stopped))))
          (is (= 1 (:samples (:metadata stopped)))))
        (testing "the event references the metrics artifact (Global Constraint 21)"
          (is (= (:metrics-ref (:metadata stopped)) (:payload-ref stopped)))
          (is (str/starts-with? (:payload-ref stopped) "sha256:")))
        (testing "the metrics artifact body holds the full evidence pack"
          (let [body (cas/get-bytes cas-root (:payload-ref stopped))
                pack (edn/read-string (String. body StandardCharsets/UTF_8))]
            (is (= (:reason decision) (:stop/reason pack)))
            (is (= (:evidence decision) (:stop/evidence pack)))
            (is (= 1 (get-in pack [:stop/evidence :samples])))))))))

;; ============================================================================
;; Boundary validation (fail closed, Global Constraint 22)
;; ============================================================================

(deftest malformed-inputs-fail-closed
  (testing "unknown threshold keys are rejected"
    (is (= :promotion/monitor-invalid
           (error-type #(monitor/decide [] (assoc (thresholds) :bogus 1))))))
  (testing "a missing :min-samples is rejected"
    (is (= :promotion/monitor-invalid
           (error-type #(monitor/decide [] (dissoc (thresholds) :min-samples))))))
  (testing "an unknown observation key is rejected"
    (is (= :promotion/monitor-invalid
           (error-type #(monitor/decide
                         [(assoc (observation (java.util.UUID/randomUUID)) :bogus 1)]
                         (thresholds))))))
  (testing "a non-keyword outcome is rejected"
    (is (= :promotion/monitor-invalid
           (error-type #(monitor/decide
                         [(observation (java.util.UUID/randomUUID) {:outcome "failed"})]
                         (thresholds))))))
  (testing "a stop with a non-true :stop? decision is rejected"
    (let [db (fresh-db)
          cas-root (temp-cas-root)
          _ (seed-generations! db)
          operator-sid (operator-session! db)
          decision (assoc (hard-stop-decision) :stop? false)]
      (is (= :promotion/canary-stop-invalid
             (error-type #(monitor/stop-canary!
                           (stop-system db cas-root operator-sid [])
                           decision :cancel))))))
  (testing "an unknown profile is rejected"
    (let [db (fresh-db)
          cas-root (temp-cas-root)
          _ (seed-generations! db)
          operator-sid (operator-session! db)]
      (is (= :promotion/monitor-invalid
             (error-type #(monitor/stop-canary!
                           (stop-system db cas-root operator-sid [])
                           (hard-stop-decision) :nuke)))))))
