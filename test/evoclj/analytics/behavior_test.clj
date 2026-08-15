(ns evoclj.analytics.behavior-test
  "Tests for the behavior-profile layer (Foundation F1).

  Covers the input contract (:analytics/events-invalid on non-
  sequential and malformed input), the zeroed empty profile, the full
  synthetic-session fold (intent counts, tool-seq order, failure
  classification, status, resource sums, wall time), fingerprint
  determinism and sensitivity, failure summarization, per-tool usage
  stats, and the derived/explicit status rules."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [evoclj.analytics.behavior :as behavior]))

(def ^:private session-id
  (java.util.UUID/randomUUID))

(def ^:private full-events
  "A synthetic full-session event vector. Seq order matters: intent
  category comes from :metadata :intent/type, tool ids from
  :metadata :tool/id / :resource/id, model calls from the
  :intent/model-call intent type, and failures from the failure
  signals (real scheduler metadata keys are reflected here)."
  [{:event/seq 1  :event/type :session/created   :session/id session-id
    :metadata {:wall-ms 150}}
   {:event/seq 2  :event/type :session/started
    :metadata {:wall-ms 50}}
   {:event/seq 3  :event/type :node/completed
    :metadata {:intent/type :intent/tool-call :tool/id :fs-read
               :model-input-tokens 10 :provider-calls 1}}
   {:event/seq 4  :event/type :node/completed
    :metadata {:intent/type :intent/tool-call :tool/id "db-write"
               :model-output-tokens 20 :provider-calls 1}}
   {:event/seq 5  :event/type :node/completed
    :metadata {:intent/type :intent/memory-read :model-input-tokens 5}}
   {:event/seq 6  :event/type :intent/model-call
    :metadata {:duration-ms 40}}
   {:event/seq 7  :event/type :node/failed
    :metadata {:intent/type :intent/model-call
               :error/type :provider/model-unavailable
               :error/message "upstream down"}}
   {:event/seq 8  :event/type :node/failed
    :metadata {:intent/type :intent/tool-call :tool/id :fs-write
               :error/type :intent/schema-invalid}}
   {:event/seq 9  :event/type :intent/finish
    :metadata {}}
   {:event/seq 10 :event/type :intent/fail
    :metadata {:error/type :intent/task-unsolvable :reason :no-solution}}
   {:event/seq 11 :event/type :session/completed
    :metadata {:status :completed :wall-ms 1000}}
   {:event/seq 12 :event/type :node/failed
    :metadata {:intent/type :intent/model-call
               :error/type :provider/model-unavailable}}])

(defn- profile-error
  "The ExceptionInfo thrown by a profile fn, or nil."
  [thunk]
  (try (thunk)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(deftest empty-events-zeroed-profile
  (testing "the empty log folds to a zeroed, schema-valid profile"
    (let [p (behavior/profile-events [])]
      (is (nil? (m/explain behavior/BehaviorProfileSchema p)))
      (is (nil? (:behavior/session-id p)))
      (is (zero? (:behavior/n-events p)))
      (is (= {:tool-call {:count 0 :by-tool {}}
              :model-call {:count 0}
              :memory-read {:count 0}
              :memory-write {:count 0}
              :finish {:count 0}
              :fail {:count 0}}
             (:behavior/intents p)))
      (is (empty? (:behavior/failures p)))
      (is (empty? (:behavior/tool-seq p)))
      (is (= :completed (:behavior/status p)))
      (is (nil? (:behavior/wall-ms p)))
      (is (= {:provider-calls 0 :model-input-tokens 0 :model-output-tokens 0}
             (:behavior/resource p))))))

(deftest full-session-profile
  (let [p (behavior/profile-events full-events)]
    (testing "schema-valid and closed"
      (is (nil? (m/explain behavior/BehaviorProfileSchema p))))
    (testing "session id and event count"
      (is (= session-id (:behavior/session-id p)))
      (is (= 12 (:behavior/n-events p))))
    (testing "intent counts and per-tool map"
      (is (= {:tool-call {:count 3 :by-tool {"fs-read" 1 "db-write" 1 "fs-write" 1}}
              :model-call {:count 3}
              :memory-read {:count 1}
              :memory-write {:count 0}
              :finish {:count 1}
              :fail {:count 1}}
             (:behavior/intents p))))
    (testing "tool-seq keeps every invocation in event order"
      (is (= ["fs-read" "db-write" "fs-write"]
             (:behavior/tool-seq p))))
    (testing "failures are classified by the taxonomy and carry event/seq + detail"
      (is (= [{:event/seq 7  :failure/type :failure/model
               :detail {:error/type :provider/model-unavailable
                        :error/message "upstream down"
                        :intent/type :intent/model-call}}
              {:event/seq 8  :failure/type :failure/schema
               :detail {:error/type :intent/schema-invalid
                        :intent/type :intent/tool-call}}
              {:event/seq 10 :failure/type :failure/unknown
               :detail {:error/type :intent/task-unsolvable
                        :reason :no-solution}}
              {:event/seq 12 :failure/type :failure/model
               :detail {:error/type :provider/model-unavailable
                        :intent/type :intent/model-call}}]
             (:behavior/failures p))))
    (testing "explicit status wins"
      (is (= :completed (:behavior/status p))))
    (testing "wall time sums first-present wall-ms / duration-ms"
      (is (= 1240 (:behavior/wall-ms p))))
    (testing "resource counters sum across events"
      (is (= {:provider-calls 2
              :model-input-tokens 15
              :model-output-tokens 20}
             (:behavior/resource p))))))

(deftest status-derivation
  (testing "budget-exhausted maps from :session/state as a string"
    (let [p (behavior/profile-events
             [{:event/seq 1 :event/type :session/budget-exhausted
               :metadata {:session/state "budget-exhausted"}}])]
      (is (= :budget-exhausted (:behavior/status p)))))
  (testing "failed maps from the :status keyword"
    (let [p (behavior/profile-events
             [{:event/seq 1 :event/type :session/failed
               :metadata {:status :failed}}])]
      (is (= :failed (:behavior/status p)))))
  (testing "derived status: any failure -> :failed when no explicit status"
    (let [p (behavior/profile-events
             [{:event/seq 1 :event/type :node/failed
               :metadata {:error/type :boom}}])]
      (is (= :failed (:behavior/status p)))))
  (testing "an unrecognized explicit status value -> :unknown"
    (let [p (behavior/profile-events
             [{:event/seq 1 :event/type :session/completed
               :metadata {:status :mystery}}])]
      (is (= :unknown (:behavior/status p))))))

(deftest fingerprint-deterministic-and-sensitive
  (testing "deterministic: same profile hashes the same way"
    (let [p (behavior/profile-events full-events)]
      (is (= (behavior/fingerprint p) (behavior/fingerprint p)))))
  (testing "stable across a redundant rebuild of an equal profile"
    (let [p1 (behavior/profile-events full-events)
          p2 (behavior/profile-events (vec full-events))]
      (is (= (behavior/fingerprint p1) (behavior/fingerprint p2)))))
  (testing "sensitive: changing one event changes the hash"
    (let [p1 (behavior/profile-events full-events)
          changed-events (update-in full-events [2 :metadata :tool/id]
                                    (constantly :fs-read-copy))
          p2 (behavior/profile-events changed-events)]
      (is (not= (behavior/fingerprint p1) (behavior/fingerprint p2)))))
  (testing "canonical sha256:<64 hex> form"
    (let [f (behavior/fingerprint (behavior/profile-events full-events))]
      (is (re-matches #"sha256:[0-9a-f]{64}" f)))))

(deftest failure-summary
  (testing "grouped by type, sorted by count desc then type, with seqs"
    (is (= [{:failure/type :failure/model :count 2 :event/seqs [7 12]}
            {:failure/type :failure/schema :count 1 :event/seqs [8]}
            {:failure/type :failure/unknown :count 1 :event/seqs [10]}]
           (behavior/summarize-failures full-events))))
  (testing "empty log -> empty summary"
    (is (= [] (behavior/summarize-failures [])))))

(deftest tool-usage-stats
  (testing "per-tool calls, first/last seq, sorted by :tool/id"
    (is (= [{:tool/id "db-write" :calls 1 :first-seq 4 :last-seq 4}
            {:tool/id "fs-read"   :calls 1 :first-seq 3 :last-seq 3}
            {:tool/id "fs-write"  :calls 1 :first-seq 8 :last-seq 8}]
           (behavior/tool-usage-stats full-events))))
  (testing "repeated use of one tool keeps first/last seq"
    (let [events [{:event/seq 1 :event/type :intent/tool-call
                   :metadata {:tool/id :poke}}
                  {:event/seq 2 :event/type :intent/tool-call
                   :metadata {:tool/id :poke}}]]
      (is (= [{:tool/id "poke" :calls 2 :first-seq 1 :last-seq 2}]
             (behavior/tool-usage-stats events)))))
  (testing "empty log -> empty stats"
    (is (= [] (behavior/tool-usage-stats [])))))

(deftest invalid-input-rejected
  (testing "non-sequential input throws :analytics/events-invalid"
    (doseq [bad [42 {:event/seq 1} "events" nil]]
      (let [e (profile-error #(behavior/profile-events bad))]
        (is (instance? clojure.lang.ExceptionInfo e))
        (is (= :analytics/events-invalid (:error/type (ex-data e))))
        (is (= :not-sequential (:reason (ex-data e)))))))
  (testing "a malformed element throws :analytics/events-invalid"
    (let [e (profile-error #(behavior/profile-events
                             [{:event/seq 1 :event/type :node/completed
                               :metadata {}} {:foo 1}]))]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :analytics/events-invalid (:error/type (ex-data e))))
      (is (= :malformed-element (:reason (ex-data e))))))
  (testing "summarize-failures and tool-usage-stats validate their input too"
    (let [e1 (profile-error #(behavior/summarize-failures {:not :a-seq}))
          e2 (profile-error #(behavior/tool-usage-stats 5))]
      (is (= :analytics/events-invalid (:error/type (ex-data e1))))
      (is (= :analytics/events-invalid (:error/type (ex-data e2)))))))
