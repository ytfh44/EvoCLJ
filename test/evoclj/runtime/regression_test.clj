(ns evoclj.runtime.regression-test
  "Task A7 tests: the regression-detection trigger rule (Foundation
  F6, alert only).

  Covers: the pure-data regression rule (no functions anywhere, EDN
  round-trip, construction validation); the windowed drop observation
  over a synthetic paired-utility series (window truncation, window
  larger than the series, empty series, malformed samples);
  `check-series!` firing exactly when the drop crosses the threshold
  within the window (and NOT when the crossing lies outside the
  window, and NOT at the boundary); and the :monitor/alert-regression
  action — wired via trigger/register-action!, appending exactly ONE
  audit event through evoclj.store.event/append-event! with NO other
  state mutation (sessions/generations rows unchanged, hash chain
  still verifies), and appending nothing when the rule stays quiet."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.runtime.regression :as reg]
            [evoclj.runtime.trigger :as trigger]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private resolution "resolution-1")

(def ^:private db-paths (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-regression-" ".db"
                (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup!
  "Delete every temp db file created during this run."
  []
  (doseq [p @db-paths]
    (java.nio.file.Files/deleteIfExists
     (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- seed-session!
  "Insert a generation row (once) and a session row; returns the
  session id (a #uuid)."
  [db]
  (let [sid (random-uuid)]
    (sqlite/with-db [conn db]
      (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen]))
        (jdbc/insert! conn :generations
                      {:id gen
                       :genome_id genome
                       :resolution_id resolution
                       :parent_id nil
                       :state "active"
                       :current 0
                       :created_at now}))
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id gen
                     :genome_id genome
                     :resolution_id resolution
                     :phenotype_id phenotype
                     :state "created"
                     :created_at now}))
    sid))

(defn- seed-root-event!
  "Append the :session/created root event that opens `sid`'s causal
  chain; returns the persisted event (the audit event's cause anchor)."
  [db sid]
  (event/append-event! db
                       {:session/id sid
                        :generation/id gen
                        :phenotype/id phenotype
                        :event/type :session/created
                        :cause/event-id nil
                        :payload-ref nil
                        :metadata {}}))

;; --- helpers ---------------------------------------------------------------

(defn- error-type
  "The :error/type of the ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(defn- has-fn?
  "True when x contains a function value at any depth (rule data must
  be fn-free per Global Constraint 22)."
  [x]
  (cond
    (fn? x) true
    (map? x) (boolean (some has-fn? (concat (keys x) (vals x))))
    (coll? x) (boolean (some has-fn? x))
    :else false))

(defn- sample
  "One synthetic paired-utility sample: the promoted child's utility
  vs its parent's."
  [parent child]
  {:sample/utility-parent parent
   :sample/utility-child child})

(defn- series
  "A synthetic paired-utility series from [parent child] pairs."
  [pairs]
  (mapv (fn [[p c]] (sample p c)) pairs))

(defn- fired-contexts
  "The :trigger/context maps of every fired rule."
  [result]
  (mapv :trigger/context (:trigger/fired result)))

;; --- rule data (pure, fn-free) ---------------------------------------------

(deftest regression-rule-is-pure-data
  (let [rule (reg/regression-rule 5.0 3)]
    (testing "the rule is data only: no functions anywhere, EDN round-trip"
      (is (false? (has-fn? rule)))
      (is (= rule (edn/read-string (pr-str rule)))))
    (testing "the rule satisfies the trigger :metric contract"
      (is (= :metric (:trigger/kind rule)))
      (is (= :utility/drop (:trigger/metric-name rule)))
      (is (= :gt (:comparator (:trigger/rule rule))))
      (is (= 5.0 (:threshold (:trigger/rule rule))))
      (is (= 3 (:window (:trigger/rule rule))))
      (is (= :monitor/alert-regression (:trigger/action rule)))))
  (testing "a nil window omits the :window key and stays closed-schema valid"
    (let [rule (reg/regression-rule 5.0 nil)]
      (is (false? (contains? (:trigger/rule rule) :window)))
      (is (false? (has-fn? rule))))))

(deftest regression-rule-validates-construction
  (testing "a non-number threshold is rejected"
    (is (= :trigger/invalid (error-type #(reg/regression-rule "5" 3)))))
  (testing "a non-integer window is rejected"
    (is (= :trigger/invalid (error-type #(reg/regression-rule 5.0 3.0)))))
  (testing "a non-positive window is rejected"
    (is (= :trigger/invalid (error-type #(reg/regression-rule 5.0 0))))))

;; --- windowed drop observation ---------------------------------------------

(deftest windowed-drop-window-semantics
  (testing "max drop over the whole series with a nil window"
    (is (= 9.0 (reg/windowed-drop nil (series [[10.0 1.0] [10.0 9.0] [10.0 8.0]])))))
  (testing "a window larger than the series means all samples"
    (is (= 9.0 (reg/windowed-drop 100 (series [[10.0 1.0] [10.0 9.0]])))))
  (testing "only the last N samples count"
    (is (= 2.0 (reg/windowed-drop 2 (series [[10.0 1.0] [10.0 9.0] [10.0 8.0]])))))
  (testing "an improving child contributes no drop (the observation floors at 0)"
    (is (= 0.0 (reg/windowed-drop nil (series [[10.0 12.0] [10.0 11.0]])))))
  (testing "an empty series observes 0.0"
    (is (= 0.0 (reg/windowed-drop 3 [])))))

(deftest windowed-drop-rejects-malformed-samples
  (testing "a missing utility key is rejected"
    (is (= :trigger/invalid (error-type #(reg/windowed-drop nil [{:sample/utility-parent 10.0}])))))
  (testing "a non-numeric utility is rejected"
    (is (= :trigger/invalid (error-type #(reg/windowed-drop
                                           nil [{:sample/utility-parent 10.0
                                                 :sample/utility-child "low"}])))))
  (testing "a non-sequential series is rejected"
    (is (= :trigger/invalid (error-type #(reg/windowed-drop nil :nope))))))

;; --- check-series!: fires exactly within the window ------------------------

(deftest check-series-fires-exactly-within-window
  (let [reg (trigger/make-registry)
        rule (reg/regression-rule 5.0 3)]
    (testing "a 7.0 drop inside the last 3 samples fires"
      (let [result (reg/check-series! reg rule
                                      (series [[10.0 9.0] [10.0 8.5] [10.0 3.0] [10.0 9.5]]))]
        (is (= 1 (count (:trigger/fired result))))
        (let [f (first (:trigger/fired result))]
          (is (= :metric (:trigger/kind f)))
          (is (= :monitor/alert-regression (:trigger/action f)))
          (is (= {:metric/name :utility/drop :metric/value 7.0}
                 (:trigger/context f))))))
    (testing "a drop below the threshold inside the window does not fire"
      (let [result (reg/check-series! reg rule
                                      (series [[10.0 9.0] [10.0 8.5] [10.0 9.0] [10.0 9.5]]))]
        (is (= 0 (count (:trigger/fired result))))))
    (testing "a nil window observes the whole series"
      (let [result (reg/check-series! reg (reg/regression-rule 5.0 nil)
                                      (series [[10.0 1.0] [10.0 9.0]]))]
        (is (= 1 (count (:trigger/fired result))))))))

(deftest check-series-does-not-fire-outside-window-or-at-boundary
  (let [reg (trigger/make-registry)]
    (testing "a drop outside the window does not fire"
      ;; the 9.0 drop is the FIRST sample — outside the last-3 window
      (let [samples (series [[10.0 1.0] [10.0 9.0] [10.0 9.0] [10.0 9.0]])]
        (is (= 0 (count (:trigger/fired (reg/check-series! reg (reg/regression-rule 5.0 3) samples)))))
        ;; the same series with a window of 4 sees the drop and fires
        (is (= 1 (count (:trigger/fired (reg/check-series! reg (reg/regression-rule 5.0 4) samples)))))))
    (testing "a drop exactly at the threshold does not fire (:gt)"
      (let [result (reg/check-series! reg (reg/regression-rule 5.0 1) (series [[10.0 5.0]]))]
        (is (= 0 (count (:trigger/fired result))))))
    (testing "an empty series observes no drop and does not fire"
      (is (= 0 (count (:trigger/fired (reg/check-series! reg (reg/regression-rule 5.0 3) []))))))))

;; --- the :monitor/alert-regression action (audit only) ---------------------

(deftest register-alert-action-wires-the-action
  (let [db (fresh-db)
        sid (seed-session! db)
        reg (trigger/make-registry)
        returned (reg/register-alert-action! reg db sid)]
    (testing "registration returns the registry and wires the action id"
      (is (identical? reg returned))
      (is (= #{:monitor/alert-regression} (set (keys @reg)))))))

(deftest alert-action-appends-audit-event-only
  (let [db (fresh-db)
        sid (seed-session! db)
        root (seed-root-event! db sid)
        reg (trigger/make-registry)
        _ (reg/register-alert-action! reg db sid)
        rule (reg/regression-rule 5.0 3)
        samples (series [[10.0 9.0] [10.0 8.5] [10.0 3.0] [10.0 9.5]]) ; max drop in window 7.0
        before-count (count (event/events-for-session db sid))
        before-session (first (jdbc/query db ["SELECT * FROM sessions WHERE id = ?" (str sid)]))
        before-gens (jdbc/query db ["SELECT * FROM generations"])
        result (reg/check-series! reg rule samples)]
    (testing "the rule fired and the action ran :ok, returning the persisted event"
      (is (= 1 (count (:trigger/fired result))))
      (is (= 1 (count (:trigger/actions result))))
      (let [a (first (:trigger/actions result))]
        (is (= :ok (:action/status a)))
        (is (= :monitor/alert-regression (:action/id a)))
        (is (= (:trigger/id (first (:trigger/fired result))) (:trigger/id a)))
        (is (pos-int? (:event/id (:action/result a))))))
    (testing "exactly ONE audit event was appended, carrying the observed drop"
      (let [events (event/events-for-session db sid)]
        (is (= (inc before-count) (count events)))
        (let [audit (last events)]
          (is (= :monitor/regression-alert (:event/type audit)))
          (is (= (:event/id root) (:cause/event-id audit)))
          (is (= :utility/drop (get-in audit [:metadata :monitor/metric-name])))
          (is (= 7.0 (get-in audit [:metadata :monitor/drop])))
          (is (= :monitor/regression (get-in audit [:metadata :monitor/rule-name])))
          (is (string? (get-in audit [:metadata :monitor/fired-at]))))))
    (testing "no state mutation: session and generation rows unchanged, chain verifies"
      (is (= before-session (first (jdbc/query db ["SELECT * FROM sessions WHERE id = ?" (str sid)]))))
      (is (= before-gens (jdbc/query db ["SELECT * FROM generations"])))
      (is (= {:valid? true :events (inc before-count)}
             (event/verify-event-chain db sid))))))

(deftest alert-action-appends-nothing-when-rule-quiet
  (let [db (fresh-db)
        sid (seed-session! db)
        _ (seed-root-event! db sid)
        reg (trigger/make-registry)
        _ (reg/register-alert-action! reg db sid)
        before-count (count (event/events-for-session db sid))
        result (reg/check-series! reg (reg/regression-rule 5.0 3)
                                  (series [[10.0 9.0] [10.0 8.5] [10.0 9.0]]))]
    (testing "no fired rule means no action and no appended event"
      (is (= 0 (count (:trigger/fired result))))
      (is (= 0 (count (:trigger/actions result))))
      (is (= before-count (count (event/events-for-session db sid)))))))

(deftest alert-action-surfaces-unknown-session-as-error-entry
  (let [db (fresh-db)
        _ (seed-session! db)
        reg (trigger/make-registry)
        _ (reg/register-alert-action! reg db (random-uuid)) ; not the seeded session
        result (reg/check-series! reg (reg/regression-rule 5.0 1)
                                  (series [[10.0 1.0]]))]
    (testing "a handler failure becomes an :error entry, not a throw"
      (is (= 1 (count (:trigger/fired result))))
      (let [a (first (:trigger/actions result))]
        (is (= :error (:action/status a)))
        (is (= :store/session-not-found (:error/type a)))))))
