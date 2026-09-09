(ns evoclj.evolution.observation-test
  (:require [clojure.test :refer [deftest is]]
            [evoclj.evolution.diagnostic :as diagnostic]
            [evoclj.evolution.observation :as observation]))

(def ^:private id-a
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private id-b
  "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(def ^:private evidence
  {:evidence/id id-a
   :generation/id "generation-1"
   :cutoff-event-id 1
   :episodes []
   :summary {}})

(def ^:private diagnosis
  {:diagnosis/id id-a
   :evidence/id id-a
   :hypotheses []})

(def ^:private subject
  {:artifact/revision id-a
   :workspace/id "workspace-1"})

(def ^:private details
  {:producer {:kind :clj-kondo :version "2026.09"}
   :subject subject
   :findings []
   :exit-status 0
   :status :complete
   :captured-at (java.util.Date. 0)})

(defn- bundle []
  (diagnostic/build-bundle evidence diagnosis details))

(defn- thrown-error-type
  [f]
  (:error/type
   (ex-data
    (try
      (f)
      nil
      (catch clojure.lang.ExceptionInfo e e)))))

(deftest same-subject-is-fresh-and-admitted
  (let [b (bundle)
        token (observation/capture-token b)]
    (is (= :fresh (:status (observation/freshness b subject))))
    (is (= {:decision :accept
            :reason :fresh
            :diagnostic/id (:diagnostic/id b)
            :subject subject}
           (observation/guard-async-result token b subject)))))

(deftest changed-revision-rejects-late-result
  (let [b (bundle)
        token (observation/capture-token b)
        current (assoc subject :artifact/revision id-b)]
    (is (= :stale (:status (observation/freshness b current))))
    (is (= :reject (:decision (observation/guard-async-result token b current))))
    (is (= :stale (:reason (observation/guard-async-result token b current))))))

(deftest changed-snapshot-rejects-late-result
  (let [b (diagnostic/build-bundle evidence diagnosis
                                   (assoc details :subject
                                          (assoc subject :snapshot/id id-a)))
        token (observation/capture-token b)
        current (assoc subject :snapshot/id id-b)]
    (is (= :stale (:status (observation/freshness b current))))
    (is (= :reject (:decision (observation/guard-async-result token b current))))))

(deftest different-workspace-is-not-admitted-as-stale
  (let [b (bundle)
        current (assoc subject :workspace/id "workspace-2")
        result (observation/guard-async-result
                (observation/capture-token b) b current)]
    (is (= :scope-mismatch (:status (observation/freshness b current))))
    (is (= {:decision :reject
            :reason :scope-mismatch
            :diagnostic/id (:diagnostic/id b)
            :captured subject
            :current current}
           result))))

(deftest mismatched-token-fails-closed
  (let [b (bundle)
        token (assoc (observation/capture-token b) :observation/id id-b)]
    (is (= :observation/token-mismatch
           (thrown-error-type
            #(observation/guard-async-result token b subject))))))
