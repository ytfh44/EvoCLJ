(ns evoclj.runtime.trigger-test
  "Foundation F6 tests: data-driven triggers over the append-only event
  stream.

  Covers: match-event-rule counting over a synthetic event vector with
  every comparator and :window semantics; match-metric-rule over a
  metrics value; `evaluate` fired shapes (id/name/kind/context with the
  observed count) and invalid-context guards; the action registry
  (register-action! validation, run-actions! :ok entries, unknown-action
  and throwing-handler :error entries, per-fired-rule isolation); and
  check-events! end to end.

  component adds the action-registry authority audit + ACL: descriptor
  validation (a descriptor missing :action/id / :action/allowlist /
  :action/subject-scope throws :trigger/invalid); unauthorized subjects
  denied with :trigger/action-unauthorized and NEVER executed; and, with
  an audit context {:store ... :session/id ...}, every executed action
  and every denial appends an audit event (:action/executed /
  :action/denied) to the session's append-only store.

  All rule data is pure data (Global Constraint 22): handlers live only
  in the registry atom, never inside rules."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.trigger :as trigger]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util UUID)))

;; --- helpers ---------------------------------------------------------------

(defn- error-type
  "The :error/type of the ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(defn- event-type
  "A minimal synthetic event carrying at least :event/type (public Event
  maps satisfy the shape the matcher needs)."
  [type]
  {:event/type type})

(defn- event-vector
  "10 events: 5 :intent/tool-call and 5 :provider/turn, in a fixed
  interleaved order (tool-call on even indices 0,2,4,6,8)."
  []
  (mapv (fn [i] (event-type (if (even? i) :intent/tool-call :provider/turn)))
        (range 10)))

(defn- id
  "A fresh trigger id."
  []
  (UUID/randomUUID))

(defn- rule-section
  "Split caller overrides into the :trigger/rule section keys and the
  top-level keys."
  [overrides]
  (let [rule-keys #{:threshold :comparator :window}
        rule-part (select-keys overrides rule-keys)
        top-part (apply dissoc overrides rule-keys)]
    {:rule rule-part :top top-part}))

(defn- event-rule
  "A minimal valid :event rule; callers pass overrides whose
  :threshold / :comparator / :window keys land in :trigger/rule."
  [n & [overrides]]
  (let [{:keys [rule top]} (rule-section (or overrides {}))]
    (merge {:trigger/id (id)
            :trigger/name (keyword "t" (str "fire-" n))
            :trigger/kind :event
            :trigger/event-type :intent/tool-call
            :trigger/rule {:threshold 3 :comparator :gt}
            :trigger/action :t/alarm}
           top
           (when (seq rule) {:trigger/rule (merge {:threshold 3 :comparator :gt} rule)}))))

(defn- metric-rule
  "A minimal valid :metric rule; callers pass overrides whose
  :threshold / :comparator / :window keys land in :trigger/rule."
  [n & [overrides]]
  (let [{:keys [rule top]} (rule-section (or overrides {}))]
    (merge {:trigger/id (id)
            :trigger/name (keyword "m" (str "met-" n))
            :trigger/kind :metric
            :trigger/metric-name :cost/task
            :trigger/rule {:threshold 1.0 :comparator :gt}
            :trigger/action :t/budget-stop}
           top
           (when (seq rule) {:trigger/rule (merge {:threshold 1.0 :comparator :gt} rule)}))))

(defn- fired-map
  "A bare fired-rule map (as `evaluate` would produce) for driving
  run-actions!. Callers override the action id."
  [n & [overrides]]
  (merge {:trigger/id (id)
          :trigger/name (keyword "f" (str "fired-" n))
          :trigger/kind :event
          :trigger/action :alarm}
         overrides))

;; --- store fixtures (audit tests) ------------------------------------------

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
                "evoclj-trigger-" ".db"
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
    (doseq [[artifact-id media-type]
            [[genome "application/octet-stream"]
             [resolution "application/edn"]
             [phenotype "application/edn"]]]
      (artifact/ensure-artifact! db artifact-id media-type 0))
    (artifact/ensure-genome! db genome)
    db))

(defn- seed-session!
  "Insert a generation row (once) and a session row; returns the
  session id (a #uuid)."
  [db]
  (let [sid (random-uuid)]
    (doseq [[artifact-id media-type]
            [[genome "application/octet-stream"]
             [resolution "application/edn"]
             [phenotype "application/edn"]]]
      (artifact/ensure-artifact! db artifact-id media-type 0))
    (artifact/ensure-genome! db genome)
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

;; --- match-event-rule -------------------------------------------------------

(deftest match-event-rule-counts-and-compares
  (let [events (event-vector)] ; 5 tool-calls among 10 events
    (testing "comparators against the observed count (5 tool-calls)"
      (is (true? (trigger/match-event-rule (event-rule 1 {:threshold 4 :comparator :gt}) events)))
      (is (false? (trigger/match-event-rule (event-rule 2 {:threshold 6 :comparator :gt}) events)))
      (is (true? (trigger/match-event-rule (event-rule 3 {:threshold 5 :comparator :gte}) events)))
      (is (false? (trigger/match-event-rule (event-rule 4 {:threshold 6 :comparator :gte}) events)))
      (is (true? (trigger/match-event-rule (event-rule 5 {:threshold 6 :comparator :lt}) events)))
      (is (false? (trigger/match-event-rule (event-rule 6 {:threshold 4 :comparator :lt}) events)))
      (is (true? (trigger/match-event-rule (event-rule 7 {:threshold 5 :comparator :lte}) events)))
      (is (true? (trigger/match-event-rule (event-rule 8 {:threshold 5 :comparator :eq}) events)))
      (is (false? (trigger/match-event-rule (event-rule 9 {:threshold 4 :comparator :eq}) events))))))

(deftest match-event-rule-window-semantics
  (let [events (event-vector)] ; tool-calls on even indices 0,2,4,6,8
    (testing "a :window counts only the last N events"
      ;; Last 4 events (indices 6,7,8,9) contain tool-calls 6 and 8 = 2
      (let [base (event-rule 1 {:window 4 :threshold 1 :comparator :gt})]
        (is (true? (trigger/match-event-rule base events)))
        (let [w2 (assoc-in base [:trigger/rule :threshold] 2)
              w3 (assoc-in base [:trigger/rule :threshold] 3)]
          ;; 2 tool-calls vs a strict threshold of 2 does NOT fire
          (is (false? (trigger/match-event-rule w2 events)))
          (is (false? (trigger/match-event-rule w3 events)))))
      ;; Window of 3 (indices 7,8,9) contains 1 tool-call (index 8)
      (let [w3 (event-rule 2 {:window 3 :threshold 1 :comparator :gte})]
        (is (true? (trigger/match-event-rule w3 events)))
        (is (false? (trigger/match-event-rule
                     (assoc-in w3 [:trigger/rule :threshold] 2) events))))
      ;; Window of 1 (index 9) contains 0 tool-calls
      (let [w1 (event-rule 3 {:window 1 :threshold 0 :comparator :eq})]
        (is (true? (trigger/match-event-rule w1 events)))))
    (testing "a :window larger than the sequence counts everything"
      (let [rule (event-rule 4 {:window 100 :threshold 5 :comparator :gte})]
        (is (true? (trigger/match-event-rule rule events)))))
    (testing ":window <= 0 is treated as all events"
      (doseq [w [0 -1]]
        (let [rule (event-rule 5 {:window w :threshold 5 :comparator :gte})]
          (is (true? (trigger/match-event-rule rule events))
              (str "window " w " treated as all")))))))

(deftest match-event-rule-zero-count-when-type-absent
  (let [events (event-vector)] ; no :eviction/candidate events
    (testing "an absent type counts 0 and satisfies only an :eq 0 or :lte"
      (is (false? (trigger/match-event-rule
                   (event-rule 1 {:trigger/event-type :eviction/candidate
                                  :threshold 0 :comparator :gt})
                   events)))
      (is (true? (trigger/match-event-rule
                  (event-rule 2 {:trigger/event-type :eviction/candidate
                                 :threshold 0 :comparator :eq})
                  events)))
      (is (true? (trigger/match-event-rule
                  (event-rule 3 {:trigger/event-type :eviction/candidate
                                 :threshold 0 :comparator :lte})
                  events))))))

(deftest match-event-rule-malformed-rule-throws
  (let [events (event-vector)]
    (testing "a missing required key is rejected"
      (is (= :trigger/invalid (error-type #(trigger/match-event-rule
                                            (dissoc (event-rule 1) :trigger/name)
                                            events)))))
    (testing "an unknown kind is rejected"
      (is (= :trigger/invalid (error-type #(trigger/match-event-rule
                                            (assoc (event-rule 2) :trigger/kind :bogus)
                                            events)))))
    (testing "a non-enum comparator is rejected"
      (is (= :trigger/invalid (error-type #(trigger/match-event-rule
                                            (assoc-in (event-rule 3)
                                                      [:trigger/rule :comparator] :between)
                                            events)))))
    (testing "a non-number threshold is rejected"
      (is (= :trigger/invalid (error-type #(trigger/match-event-rule
                                            (assoc-in (event-rule 4)
                                                      [:trigger/rule :threshold] "high")
                                            events)))))
    (testing "an unknown top-level key is rejected (closed schema)"
      (is (= :trigger/invalid (error-type #(trigger/match-event-rule
                                            (assoc (event-rule 5) :trigger/bogus 1)
                                            events)))))))

;; --- match-metric-rule ------------------------------------------------------

(deftest match-metric-rule-comparators
  (let [value 5.0]
    (testing "each comparator branch against the value"
      (is (true? (trigger/match-metric-rule (metric-rule 1 {:threshold 4.0 :comparator :gt}) value)))
      (is (false? (trigger/match-metric-rule (metric-rule 2 {:threshold 5.0 :comparator :gt}) value)))
      (is (true? (trigger/match-metric-rule (metric-rule 3 {:threshold 5.0 :comparator :gte}) value)))
      (is (true? (trigger/match-metric-rule (metric-rule 4 {:threshold 6.0 :comparator :lt}) value)))
      (is (false? (trigger/match-metric-rule (metric-rule 5 {:threshold 5.0 :comparator :lt}) value)))
      (is (true? (trigger/match-metric-rule (metric-rule 6 {:threshold 5.0 :comparator :lte}) value)))
      (is (true? (trigger/match-metric-rule (metric-rule 7 {:threshold 5.0 :comparator :eq}) value)))
      (is (false? (trigger/match-metric-rule (metric-rule 8 {:threshold 6.0 :comparator :eq}) value)))
      (is (true? (trigger/match-metric-rule (metric-rule 9 {:threshold 4 :comparator :gte}) 5))))))

(deftest match-metric-rule-non-numeric-throws
  (testing "a non-numeric string value is rejected"
    (is (= :trigger/invalid (error-type #(trigger/match-metric-rule (metric-rule 1) "high")))))
  (testing "a nil value is rejected"
    (is (= :trigger/invalid (error-type #(trigger/match-metric-rule (metric-rule 2) nil))))))

;; --- evaluate ---------------------------------------------------------------

(deftest evaluate-fires-matching-event-rules-with-context
  (let [events (event-vector) ; 5 tool-calls
        id1 (id)
        id2 (id)
        rules [(assoc (event-rule 1 {:threshold 4 :comparator :gt}) :trigger/id id1)
               (assoc (event-rule 2 {:threshold 2 :comparator :gte}) :trigger/id id2)
               (event-rule 3 {:trigger/event-type :absent/type :threshold 0 :comparator :gt})]
        fired (trigger/evaluate rules {:events events :metrics {}})]
    (testing "matching rules fire; the absent-type rule does not"
      (is (= 2 (count fired)))
      (is (= #{id1 id2} (set (map :trigger/id fired)))))
    (testing "a fired event rule carries id, name, kind, fired-at"
      (let [f (first (filter #(= id1 (:trigger/id %)) fired))]
        (is (= {:trigger/id id1 :trigger/name :t/fire-1 :trigger/kind :event}
               (select-keys f [:trigger/id :trigger/name :trigger/kind])))
        (is (string? (:trigger/fired-at f)))
        (is (re-find #"^\d{4}-\d{2}-\d{2}T" (:trigger/fired-at f)))))
    (testing "the context carries the observed count"
      (let [f (first (filter #(= id1 (:trigger/id %)) fired))]
        (is (= 5 (:count (:trigger/context f))))
        (is (= :intent/tool-call (:event/type (:trigger/context f))))))))

(deftest evaluate-fires-metric-rules-with-value-context
  (let [id1 (id)
        rules [(assoc (metric-rule 1 {:trigger/metric-name :cost/task
                                      :threshold 1.0 :comparator :gt})
                      :trigger/id id1)
               (metric-rule 2 {:trigger/metric-name :cost/task :threshold 99.0 :comparator :gt})
               (metric-rule 3 {:trigger/metric-name :latency/task :threshold 1.0 :comparator :gt})]
        fired (trigger/evaluate rules {:events [] :metrics {:cost/task 5.0
                                                            :latency/task 0.5}})]
    (testing "only the cost rule above threshold fires"
      (is (= 1 (count fired)))
      (is (= id1 (:trigger/id (first fired)))))
    (testing "the context carries the metric name and value"
      (let [f (first fired)]
        (is (= {:metric/name :cost/task :metric/value 5.0} (:trigger/context f)))
        (is (= :metric (:trigger/kind f)))))))

(deftest evaluate-does-not-fire-non-matching-rules
  (let [events (event-vector) ; 5 tool-calls
        rules [(event-rule 1 {:threshold 6 :comparator :gt}) ; 5 > 6 false
               (metric-rule 2 {:trigger/metric-name :never-seen :threshold 1 :comparator :gt})] ; no value
        fired (trigger/evaluate rules {:events events :metrics {}})]
    (is (= 0 (count fired)))))

(deftest evaluate-invalid-context-throws
  (let [events (event-vector)
        rules [(event-rule 1)]]
    (testing "non-sequential :events is rejected"
      (is (= :trigger/invalid (error-type #(trigger/evaluate rules {:events :nope :metrics {}})))))
    (testing "non-map :metrics is rejected"
      (is (= :trigger/invalid (error-type #(trigger/evaluate rules {:events events :metrics [:x]})))))
    (testing "a malformed rule is rejected"
      (is (= :trigger/invalid (error-type #(trigger/evaluate
                                            [(dissoc (event-rule 2) :trigger/name)]
                                            {:events events :metrics {}})))))
    (testing "a non-numeric metric value in context is rejected"
      (is (= :trigger/invalid (error-type #(trigger/evaluate
                                            [(metric-rule 3 {:trigger/metric-name :cost/task})]
                                            {:events [] :metrics {:cost/task "high"}})))))))

;; --- run-actions! -----------------------------------------------------------

(deftest run-actions-ok-entries-with-results
  (let [reg-id (id)
        fired [(assoc (fired-map 1) :trigger/id reg-id)]
        reg (trigger/make-registry)
        _ (trigger/register-action! reg :alarm (fn [f] {:handled true :for (:trigger/id f)}))
        results (trigger/run-actions! reg fired)]
    (testing "a successful handler yields an :ok entry carrying its result"
      (is (= 1 (count results)))
      (let [r (first results)]
        (is (= :ok (:action/status r)))
        (is (= :alarm (:action/id r)))
        (is (= reg-id (:trigger/id r)))
        (is (= :f/fired-1 (:trigger/name r)))
        (is (= {:handled true :for reg-id} (:action/result r)))
        (is (nil? (:error/type r)))))))

(deftest run-actions-unknown-action-is-an-error-entry
  (let [reg (trigger/make-registry)
        fired [(assoc (fired-map 1) :trigger/action :unknown-action)]
        results (trigger/run-actions! reg fired)]
    (testing "an unknown action id yields an :error entry, not a throw"
      (is (= 1 (count results)))
      (let [r (first results)]
        (is (= :error (:action/status r)))
        (is (= :trigger/action-not-found (:error/type r)))
        (is (string? (:error/message r)))
        (is (= :unknown-action (:action/id r)))))))

(deftest run-actions-throwing-handler-preserves-error-type
  (let [reg (trigger/make-registry)
        _ (trigger/register-action! reg :boom!
                                    (fn [_] (throw (err/error :t/budget-exceeded
                                                              "cost overrun"
                                                              {}))))
        fired [(assoc (fired-map 1) :trigger/action :boom!)]
        results (trigger/run-actions! reg fired)]
    (testing "a throwing handler yields an :error entry preserving :error/type"
      (let [r (first results)]
        (is (= :error (:action/status r)))
        (is (= :t/budget-exceeded (:error/type r)))
        (is (= "cost overrun" (:error/message r)))))))

(deftest run-actions-throwing-handler-defaults-error-type
  (let [reg (trigger/make-registry)
        _ (trigger/register-action! reg :boom! (fn [_] (throw (ex-info "kaboom" {}))))
        results (trigger/run-actions! reg [(assoc (fired-map 1) :trigger/action :boom!)])]
    (testing "an exception without :error/type defaults to :trigger/action-failed"
      (is (= :trigger/action-failed (:error/type (first results)))))))

(deftest run-actions-isolation-bad-action-does-not-stop-others
  (let [reg (trigger/make-registry)
        _ (trigger/register-action! reg :good (fn [f] {:ok (:trigger/name f)}))
        _ (trigger/register-action! reg :bad! (fn [_] (throw (err/error :t/failure "bad" {}))))
        good-id (id)
        bad-id (id)
        missing-id (id)
        fired [(assoc (fired-map 1) :trigger/id good-id :trigger/action :good)
               (assoc (fired-map 2) :trigger/id bad-id :trigger/action :bad!)
               (assoc (fired-map 3) :trigger/id missing-id :trigger/action :no-such)]
        results (trigger/run-actions! reg fired)]
    (testing "all three actions produced results; the good one is :ok, the others :error"
      (is (= 3 (count results)))
      (let [ok (first results)
            bad (second results)
            missing (nth results 2)]
        (is (= :ok (:action/status ok)))
        (is (= {:ok :f/fired-1} (:action/result ok)))
        (is (= :error (:action/status bad)))
        (is (= :t/failure (:error/type bad)))
        (is (= :error (:action/status missing)))
        (is (= :trigger/action-not-found (:error/type missing)))))))

;; --- register-action! -------------------------------------------------------

(deftest register-action-validates
  (let [reg (trigger/make-registry)]
    (testing "a non-atom registry is rejected"
      (is (= :trigger/invalid (error-type #(trigger/register-action! {} :a (fn [_] 1))))))
    (testing "a non-keyword action id is rejected"
      (is (= :trigger/invalid (error-type #(trigger/register-action! reg "a" (fn [_] 1))))))
    (testing "a non-function handler is rejected"
      (is (= :trigger/invalid (error-type #(trigger/register-action! reg :a :not-a-fn)))))
    (testing "a valid registration returns the registry and dispatches"
      (is (identical? reg (trigger/register-action! reg :ok (fn [_] :done))))
      (is (= [:ok] (keys @reg)))
      (is (= :ok (:action/status (first (trigger/run-actions!
                                         reg [{:trigger/id (id) :trigger/name :t/x
                                               :trigger/action :ok}]))))))))

;; --- check-events! ----------------------------------------------------------

(deftest check-events-end-to-end
  (let [events (event-vector) ; 5 tool-calls; 5 5
        fires-id (id)
        quiet-id (id)
        rules [(assoc (event-rule 1 {:threshold 4 :comparator :gt}) :trigger/id fires-id)
               (assoc (event-rule 2 {:threshold 99 :comparator :gt}) :trigger/id quiet-id)]
        reg (trigger/make-registry)
        _ (trigger/register-action! reg :t/alarm (fn [f] {:alerted (:trigger/name f)}))
        result (trigger/check-events! reg rules events)]
    (testing "the fired slice holds only the matching rule"
      (is (= 1 (count (:trigger/fired result))))
      (is (= fires-id (:trigger/id (first (:trigger/fired result))))))
    (testing "the action ran for the fired rule and returned an EDN-safe result"
      (is (= 1 (count (:trigger/actions result))))
      (let [r (first (:trigger/actions result))]
        (is (= :ok (:action/status r)))
        (is (= :t/alarm (:action/id r)))
        (is (= {:alerted :t/fire-1} (:action/result r)))))))

;; --- component: action descriptor validation ---------------------------------

(deftest register-action-descriptor-validates
  (let [reg (trigger/make-registry)]
    (testing "a descriptor map missing a required key is rejected"
      (is (= :trigger/invalid (error-type #(trigger/register-action!
                                            reg {:action/allowlist [:ops/operator]
                                                 :action/subject-scope :scope/alert}
                                            (fn [_] 1))))) ; missing :action/id
      (is (= :trigger/invalid (error-type #(trigger/register-action!
                                            reg {:action/id :a
                                                 :action/subject-scope :scope/alert}
                                            (fn [_] 1))))) ; missing :action/allowlist
      (is (= :trigger/invalid (error-type #(trigger/register-action!
                                            reg {:action/id :a
                                                 :action/allowlist [:ops/operator]}
                                            (fn [_] 1))))) ; missing :action/subject-scope
      (is (= :trigger/invalid (error-type #(trigger/register-action! reg {} (fn [_] 1))))))
    (testing "a malformed descriptor is rejected"
      (is (= :trigger/invalid (error-type #(trigger/register-action!
                                            reg {:action/id :a
                                                 :action/allowlist ["ops"]
                                                 :action/subject-scope :scope/alert}
                                            (fn [_] 1)))))
      (is (= :trigger/invalid (error-type #(trigger/register-action!
                                            reg {:action/id :a
                                                 :action/allowlist [:ops/operator]
                                                 :action/subject-scope 7}
                                            (fn [_] 1))))))
    (testing "a valid descriptor registers the action under :action/id and preserves it"
      (let [desc {:action/id :a
                  :action/allowlist [:ops/operator]
                  :action/subject-scope :scope/alert}]
        (is (identical? reg (trigger/register-action! reg desc (fn [_] :done))))
        (is (= #{:a} (set (keys @reg))))
        (is (= desc (get-in @reg [:a :descriptor])))))))

;; --- component: ACL enforcement ----------------------------------------------

(deftest run-actions-unauthorized-action-never-executes
  (let [calls (atom 0)
        reg (trigger/make-registry)
        _ (trigger/register-action! reg
                                    {:action/id :alarm
                                     :action/allowlist [:ops/operator]
                                     :action/subject-scope :scope/alert}
                                    (fn [_] (swap! calls inc) {:alerted true}))]
    (testing "a subject outside the allowlist is denied and NEVER executed"
      (let [r (first (trigger/run-actions!
                      reg [(assoc (fired-map 1) :action/subject :ops/agent)]))]
        (is (= :error (:action/status r)))
        (is (= :trigger/action-unauthorized (:error/type r)))
        (is (string? (:error/message r)))
        (is (= 0 @calls))))
    (testing "a fired rule with no subject is denied for an access-restricted action"
      (let [r (first (trigger/run-actions! reg [(fired-map 2)]))]
        (is (= :error (:action/status r)))
        (is (= :trigger/action-unauthorized (:error/type r)))
        (is (= 0 @calls))))
    (testing "an authorized subject runs the action"
      (let [r (first (trigger/run-actions!
                      reg [(assoc (fired-map 3) :action/subject :ops/operator)]))]
        (is (= :ok (:action/status r)))
        (is (= {:alerted true} (:action/result r)))
        (is (= 1 @calls))))))

;; --- component: audit trail ---------------------------------------------------

(deftest run-actions-audits-authorized-execution
  (let [db (fresh-db)
        sid (seed-session! db)
        root (seed-root-event! db sid)
        before (count (event/events-for-session db sid))
        reg (trigger/make-registry)
        _ (trigger/register-action! reg
                                    {:action/id :alarm
                                     :action/allowlist [:ops/operator]
                                     :action/subject-scope :scope/alert}
                                    (fn [f] {:alerted (:trigger/name f)}))
        fired [(assoc (fired-map 1) :action/subject :ops/operator)]
        results (trigger/run-actions! reg fired {:store db :session/id sid})]
    (testing "the authorized action ran :ok and was audited"
      (let [r (first results)]
        (is (= :ok (:action/status r)))
        (is (= {:alerted :f/fired-1} (:action/result r)))
        (is (pos-int? (:audit/event-id r)))))
    (testing "exactly one :action/executed audit event was appended with attribution"
      (is (= (inc before) (count (event/events-for-session db sid))))
      (let [audit (last (event/events-for-session db sid))]
        (is (= :action/executed (:event/type audit)))
        (is (= (:event/id root) (:cause/event-id audit)))
        (is (= :alarm (get-in audit [:metadata :action/id])))
        (is (= :ops/operator (get-in audit [:metadata :action/subject])))
        (is (= :scope/alert (get-in audit [:metadata :action/subject-scope])))
        (is (= :ok (get-in audit [:metadata :action/status])))
        (is (= :f/fired-1 (get-in audit [:metadata :trigger/name])))))))

(deftest run-actions-audits-unauthorized-denial
  (let [db (fresh-db)
        sid (seed-session! db)
        _ (seed-root-event! db sid)
        before (count (event/events-for-session db sid))
        calls (atom 0)
        reg (trigger/make-registry)
        _ (trigger/register-action! reg
                                    {:action/id :alarm
                                     :action/allowlist [:ops/operator]
                                     :action/subject-scope :scope/alert}
                                    (fn [_] (swap! calls inc) {:alerted true}))
        fired [(assoc (fired-map 1) :action/subject :ops/agent)]
        results (trigger/run-actions! reg fired {:store db :session/id sid})]
    (testing "the unauthorized action was NOT executed"
      (is (= 0 @calls)))
    (testing "the denial is an :error entry, never a throw, and was audited"
      (let [r (first results)]
        (is (= :error (:action/status r)))
        (is (= :trigger/action-unauthorized (:error/type r)))
        (is (pos-int? (:audit/event-id r)))))
    (testing "one :action/denied audit event was appended with the attempted subject"
      (is (= (inc before) (count (event/events-for-session db sid))))
      (let [audit (last (event/events-for-session db sid))]
        (is (= :action/denied (:event/type audit)))
        (is (= :ops/agent (get-in audit [:metadata :action/subject])))
        (is (= :denied (get-in audit [:metadata :action/status])))
        (is (= :trigger/action-unauthorized (get-in audit [:metadata :error/type])))))))

(deftest run-actions-audits-unknown-action-denial
  (let [db (fresh-db)
        sid (seed-session! db)
        _ (seed-root-event! db sid)
        before (count (event/events-for-session db sid))
        reg (trigger/make-registry)
        fired [(assoc (fired-map 1) :trigger/action :no-such-action)]
        results (trigger/run-actions! reg fired {:store db :session/id sid})]
    (testing "an unknown action is an :error entry and audited as a denial"
      (let [r (first results)]
        (is (= :error (:action/status r)))
        (is (= :trigger/action-not-found (:error/type r)))
        (is (pos-int? (:audit/event-id r))))
      (is (= (inc before) (count (event/events-for-session db sid))))
      (let [audit (last (event/events-for-session db sid))]
        (is (= :action/denied (:event/type audit)))
        (is (= :no-such-action (get-in audit [:metadata :action/id])))))))

(deftest run-actions-audits-executed-handler-failure
  (let [db (fresh-db)
        sid (seed-session! db)
        _ (seed-root-event! db sid)
        before (count (event/events-for-session db sid))
        reg (trigger/make-registry)
        _ (trigger/register-action! reg
                                    {:action/id :boom!
                                     :action/allowlist [:ops/operator]
                                     :action/subject-scope :scope/x}
                                    (fn [_] (throw (err/error :t/failure "bad" {}))))
        results (trigger/run-actions! reg [(assoc (fired-map 1)
                                                  :trigger/action :boom!
                                                  :action/subject :ops/operator)]
                                       {:store db :session/id sid})]
    (testing "a throwing handler is still an executed (audited) action"
      (let [r (first results)]
        (is (= :error (:action/status r)))
        (is (= :t/failure (:error/type r)))
        (is (pos-int? (:audit/event-id r))))
      (is (= (inc before) (count (event/events-for-session db sid))))
      (let [audit (last (event/events-for-session db sid))]
        (is (= :action/executed (:event/type audit)))
        (is (= :error (get-in audit [:metadata :action/status])))
        (is (= :t/failure (get-in audit [:metadata :error/type])))))))

(deftest run-actions-without-audit-context-appends-nothing
  (let [db (fresh-db)
        sid (seed-session! db)
        _ (seed-root-event! db sid)
        before (count (event/events-for-session db sid))
        reg (trigger/make-registry)
        _ (trigger/register-action! reg
                                    {:action/id :alarm
                                     :action/allowlist [:ops/operator]
                                     :action/subject-scope :scope/alert}
                                    (fn [_] :done))
        results (trigger/run-actions! reg [(assoc (fired-map 1) :action/subject :ops/operator)])]
    (testing "the 2-arity path performs the ACL but appends no audit events"
      (let [r (first results)]
        (is (= :ok (:action/status r)))
        (is (nil? (:audit/event-id r)))
        (is (= before (count (event/events-for-session db sid))))))))

(deftest run-actions-audit-failure-is-isolated
  (let [db (fresh-db) ; migrated, but no session seeded
        reg (trigger/make-registry)
        _ (trigger/register-action! reg
                                    {:action/id :alarm
                                     :action/allowlist [:ops/operator]
                                     :action/subject-scope :scope/alert}
                                    (fn [_] :done))
        fired [(assoc (fired-map 1) :action/subject :ops/operator)]
        results (trigger/run-actions! reg fired {:store db :session/id (random-uuid)})]
    (testing "an audit append failure becomes :audit/error; the action still ran; no throw"
      (let [r (first results)]
        (is (= :ok (:action/status r)))
        (is (= :done (:action/result r)))
        (is (string? (:audit/error r)))))))

(deftest check-events-end-to-end-with-audit
  (let [db (fresh-db)
        sid (seed-session! db)
        _ (seed-root-event! db sid)
        events (event-vector) ; 5 tool-calls
        reg (trigger/make-registry)
        _ (trigger/register-action! reg
                                    {:action/id :t/alarm
                                     :action/allowlist nil
                                     :action/subject-scope :scope/alert}
                                    (fn [f] {:alerted (:trigger/name f)}))
        result (trigger/check-events! reg [(event-rule 1 {:threshold 4 :comparator :gt})]
                                      events {:store db :session/id sid})]
    (testing "check-events! with an audit context executes and audits"
      (is (= 1 (count (:trigger/fired result))))
      (let [r (first (:trigger/actions result))]
        (is (= :ok (:action/status r)))
        (is (= :t/alarm (:action/id r)))
        (is (= {:alerted :t/fire-1} (:action/result r)))
        (is (pos-int? (:audit/event-id r))))
      (is (= :action/executed (:event/type (last (event/events-for-session db sid))))))))
