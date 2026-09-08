(ns evoclj.store.invariant-test
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.static :as static]
            [evoclj.store.cas :as cas]
            [evoclj.store.invariant :as invariant-store]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files LinkOption Path Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent CountDownLatch TimeUnit)))

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db []
  (let [path (str (Files/createTempFile "evoclj-invariants-" ".db"
                                       (make-array FileAttribute 0)))]
    (swap! db-paths conj path)
    path))

(defn- temp-cas []
  (let [path (Files/createTempDirectory "evoclj-invariants-cas-"
                                       (make-array FileAttribute 0))]
    (swap! cas-roots conj path)
    path))

(defn- delete-tree! [^Path root]
  (when (Files/exists root (make-array LinkOption 0))
    (doseq [file (reverse (file-seq (.toFile root)))]
      (Files/deleteIfExists (.toPath file)))))

(defn- cleanup! []
  (doseq [path @db-paths]
    (Files/deleteIfExists (Paths/get path (make-array String 0))))
  (reset! db-paths [])
  (doseq [root @cas-roots] (delete-tree! root))
  (reset! cas-roots []))

(use-fixtures :each
  (fn [f]
    (static/clear-suites!)
    (try (f)
         (finally
           (static/clear-suites!)
           (cleanup!)))))

(defn- fresh-store []
  (let [db (sqlite/spec (temp-db))
        c (cas/->cas (str (temp-cas)))]
    (migrate/migrate! db)
    {:sqlite db :cas c}))

(defn- row-count [store table]
  (:n (first (sqlite/query (:sqlite store)
                           [(str "SELECT count(*) AS n FROM " table)]))))

(defn- base-proposal [store]
  {:proposal/id "proposal-1"
   :proposer "proposer-1"
   :scope :runtime
   :risk :low
   :version 1
   :registry/revision (static/registry-revision)
   :predicate {:predicate/type :dsl
               :dsl {:op :equals :path [:state] :value :safe}}
   :evidence/refs ["evidence-1"]
   :replay/refs ["replay-1"]
   :adversarial/refs ["adversarial-1"]})

(defn- approved-store []
  (let [store (fresh-store)
        p (invariant-store/propose! store (base-proposal store))]
    (invariant-store/append-run! store (:proposal/id p)
                                  {:run/id "run-replay"
                                   :kind :replay
                                   :result {:state :safe}
                                   :model/policy :recorded
                                   :deterministic? true
                                   :fresh-model? false
                                   :passed? true})
    (invariant-store/append-run! store (:proposal/id p)
                                  {:run/id "run-adversarial"
                                   :kind :adversarial
                                   :result {:state :safe}
                                   :gate :g3
                                   :model/policy :recorded
                                   :deterministic? true
                                   :fresh-model? false
                                   :passed? true})
    (invariant-store/approve! store (:proposal/id p) "reviewer-1" {})
    store))

(deftest cas-first-rejects-missing-evidence-before-dependent-row
  (let [store (fresh-store)
        missing (str "sha256:" (apply str (repeat 64 "f")))
        p (assoc (base-proposal store) :evidence/refs [missing])]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"absent from CAS"
                          (invariant-store/propose! store p)))
    (is (= 0 (row-count store "invariant_proposals")))
    (is (= 0 (row-count store "invariant_runs")))))

(deftest unknown-kernel-rule-and-protected-gates-are-not-publishable
  (let [revision (static/registry-revision)
        descriptor (fn [rule]
                     {:invariant/id "generated-1"
                      :version 1
                      :predicate {:predicate/type :kernel-rule :rule/id rule}
                      :registry/revision revision
                      :activation/committed? true})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not registered"
                          (static/publish-active-invariant! (descriptor :rule/unknown))))
    (doseq [gate [:G0-parse :G1-schema-abi :G2-static-policy :G3-deterministic-suites]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"G0-G3"
                            (static/publish-active-invariant! (descriptor gate)))))))

(deftest activation-failpoint-rolls-back-all-durable-rows
  (let [store (approved-store)
        fault (fn [] (throw (ex-info "activation failpoint" {:test true})))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"activation failpoint"
                          (invariant-store/activate! store "proposal-1" "reviewer-1"
                                                     {:failpoints {:before-event-append fault}})))
    (is (= 0 (row-count store "invariant_activations")))
    (is (= 0 (row-count store "invariant_events")))
    (is (= 0 (row-count store "invariant_outbox")))
    (is (empty? (static/active-invariants)))))

(deftest activation-is-idempotent-concurrent-and-publishes-after-commit
  (let [store (approved-store)
        start (CountDownLatch. 1)
        results (doall
                 (for [_ (range 2)]
                   (future
                     (.await start 10 TimeUnit/SECONDS)
                     (try
                       (invariant-store/activate! store "proposal-1" "reviewer-1")
                       (catch Throwable t t)))))]
    (.countDown start)
    (let [out (mapv #(deref % 30000 ::timeout) results)]
      (is (every? map? out))
      (is (= 1 (row-count store "invariant_activations")))
      (is (= 1 (row-count store "invariant_events")))
      (is (= 1 (row-count store "invariant_outbox")))
      (is (= 1 (count (static/active-invariants)))))))

(deftest pending-proposal-never-auto-activates
  (let [store (fresh-store)]
    (invariant-store/propose! store (base-proposal store))
    (is (empty? (invariant-store/recover-activations! store)))
    (is (empty? (static/active-invariants)))
    (is (= 0 (row-count store "invariant_activations")))))

(deftest recovery-republishes-only-committed-activation
  (let [store (approved-store)]
    (invariant-store/activate! store "proposal-1" "reviewer-1")
    (static/clear-active-invariants!)
    (is (= [{:activation/id (-> (sqlite/query (:sqlite store)
                                                ["SELECT id FROM invariant_activations"])
                               first :id)
             :status :published}]
           (invariant-store/recover-activations! store)))
    (is (= 1 (count (static/active-invariants))))
    (is (empty? (invariant-store/recover-activations! store)))))

(deftest disable-removes-active-publication-but-preserves-history
  (let [store (approved-store)]
    (invariant-store/activate! store "proposal-1" "reviewer-1")
    (invariant-store/disable! store "proposal-1" "reviewer-1" "unsafe in production")
    (is (empty? (static/active-invariants)))
    (is (= 1 (row-count store "invariant_proposals")))
    (is (= 1 (row-count store "invariant_decisions")))
    (is (= 1 (row-count store "invariant_activations")))
    (is (= 1 (row-count store "invariant_disables")))
    (is (= 1 (row-count store "invariant_events")))
    (is (= 1 (row-count store "invariant_outbox")))))

(deftest proposal-and-publication-do-not-mutate-static-suites
  (static/register-suite! {:suite/id :rule/known
                           :suite/type :unit
                           :check (fn [_] nil)})
  (let [before (static/registered-suites)
        store (fresh-store)
        proposal (assoc (base-proposal store)
                        :predicate {:predicate/type :kernel-rule :rule/id :rule/known})]
    (invariant-store/propose! store proposal)
    (is (= before (static/registered-suites)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"requires passing"
                          (invariant-store/approve! store "proposal-1" "reviewer-1" {})))
    (is (= before (static/registered-suites)))))

(deftest active-rule-violation-is-a-hard-failure
  (static/register-suite! {:suite/id :rule/unsafe
                           :suite/type :unit
                           :check (fn [_] {:violation :unsafe-state})})
  (static/publish-active-invariant!
   {:invariant/id "generated-unsafe"
    :version 1
    :predicate {:predicate/type :kernel-rule :rule/id :rule/unsafe}
    :registry/revision (static/registry-revision)
    :activation/committed? true})
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"invariant"
                        (static/run-active-invariants {:state :unsafe}))))
