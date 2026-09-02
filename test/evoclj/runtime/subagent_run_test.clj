(ns evoclj.runtime.subagent-run-test
  "S3 child runtime execution — run-subagent! with isolated SCI and independent event chains."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as jdbc]
            [evoclj.capability.mint :as mint]
            [evoclj.runtime.subagent :as subagent]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util Date UUID)))

;; --- fixtures --------------------------------------------------------------

(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private gen "generation-1")
(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 4102444800000))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (java.nio.file.Files/createTempFile "evoclj-s3-" ".db"
                                                   (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try (java.nio.file.Files/deleteIfExists (java.nio.file.Paths/get p (into-array String [])))
         (catch Exception _)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db []
  (let [path (temp-db-path)
        _ (migrate/migrate! path)
        db path]
    (sqlite/with-db [conn db]
      (let [now "2025-01-01T00:00:00Z"]
        (doseq [h [genome resolution phenotype]]
          (try (jdbc/insert! conn :artifacts {:hash h :media_type "application/octet-stream" :size 0 :created_at now})
               (catch Exception _)))
        (try (jdbc/insert! conn :genomes {:id genome :created_at now})
             (catch Exception _))
        (try (jdbc/insert! conn :generations {:id gen :genome_id genome :resolution_id resolution :parent_id nil :state "active" :current 1 :created_at now})
             (catch Exception _))))
    db))

(defn- parent-lease [session-id phenotype-id actions]
  (mint/mint-lease! nil {:principal {:principal/type :session :session/id session-id}
                         :resource {:kind :tool :id :fixture/echo}
                         :actions actions
                         :constraints {}
                         :issued-at issued-at
                         :expires-at expires-at}))

(defn- create-parent-session! [db]
  (let [sess (session/create-session! db {:genome/id genome :resolution/id resolution :phenotype/id phenotype :generation/id gen})
        sid (:session/id sess)]
    (event/append-event! db {:session/id sid
                             :generation/id gen
                             :phenotype/id phenotype
                             :event/type :session/created
                             :prev/event-id nil
                             :payload-ref nil
                             :metadata {}})
    sess))

;; ===========================================================================
;; 1 — run-subagent! executes a simple task and returns succeeded status
;; ===========================================================================

(deftest run-subagent-executes-echo-and-returns-succeeded
  (testing "run-subagent! runs the child session's echo task and returns :completed"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "hello"} [pl])
          result (subagent/run-subagent! db parent-id session-id {:text "hello-echo"})]
      (is (= :completed (:status result)) "child run should complete")
      (is (= session-id (:session/id result)) "result session id matches child")
      (is (some? (:output-ref result)) "output ref present on success")
      (let [child-sess (session/get-session db session-id)]
        (is (= :completed (:state child-sess)) "child session transitioned to :completed"))
      (is (:valid? (event/verify-event-chain db session-id)) "child hash chain valid after run")
      (is (:valid? (event/verify-event-chain db parent-id)) "parent chain still valid"))))

(deftest run-subagent-throws-not-found-for-missing-child
  (testing "run-subagent! throws :subagent/not-found when child session does not exist"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          fake-child (UUID/randomUUID)]
      (let [ex (try (subagent/run-subagent! db parent-id fake-child {:text "hi"})
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex) "should throw")
        (is (= :subagent/not-found (:error/type (ex-data ex))) "error type is :subagent/not-found")
        (is (= fake-child (:session/id (ex-data ex))) "error carries child id")))))

;; ===========================================================================
;; 2 — child event chain has correct seq and cause links
;; ===========================================================================

(deftest child-event-chain-has-correct-seq-and-cause-links
  (testing "child chain seq is 1..M, each cause links to previous event, and parent spawn links to child"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "child-task"} [pl])
          child-id session-id
          _ (subagent/run-subagent! db parent-id child-id {:text "seq-check"})
          child-events (event/events-for-session db child-id)
          parent-events (event/events-for-session db parent-id)]
      ;; seq 1..M
      (let [seqs (mapv :event/seq child-events)]
        (is (= (vec (range 1 (inc (count child-events)))) seqs) "child seq is 1..M"))
      ;; cause chain: each event's cause is previous event's id, except root
      (is (nil? (:prev/event-id (first child-events))) "child root has no cause")
      (doseq [[prev cur] (partition 2 1 child-events)]
        (is (= (:event/id prev) (:prev/event-id cur))
            (str "child cause link broken at seq " (:event/seq cur))))
      ;; verify hash chain
      (is (:valid? (event/verify-event-chain db child-id)) "child hash chain valid")
      ;; parent's :subagent/spawned links to child
      (let [spawned (some #(when (= :subagent/spawned (:event/type %)) %) parent-events)]
        (is (some? spawned) "parent has :subagent/spawned")
        (is (= child-id (get-in spawned [:metadata :child/session-id])) "spawned metadata carries child id")
        ;; parent seq also 1..M
        (let [pseqs (mapv :event/seq parent-events)]
          (is (= (vec (range 1 (inc (count parent-events)))) pseqs) "parent seq is 1..M"))
        (is (:valid? (event/verify-event-chain db parent-id)) "parent hash chain valid")))))

;; ===========================================================================
;; 3 — parallel parent+child sessions have no cross-leakage
;; ===========================================================================

(deftest parallel-parent-and-child-have-no-cross-leakage
  (testing "child and parent event chains are independent; no child events in parent and vice versa"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          ;; spawn two children
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "child1"} [pl])
          child1-id session-id
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "child2"} [pl])
          child2-id session-id
          ;; snapshot parent events before child runs
          parent-before (event/events-for-session db parent-id)
          parent-before-count (count parent-before)
          ;; run children (sequentially, but chains must remain independent like parallel)
          res1 (subagent/run-subagent! db parent-id child1-id {:text "hello-child1"})
          res2 (subagent/run-subagent! db parent-id child2-id {:text "hello-child2"})
          parent-after (event/events-for-session db parent-id)
          child1-events (event/events-for-session db child1-id)
          child2-events (event/events-for-session db child2-id)]
      (is (= :completed (:status res1)) "child1 completed")
      (is (= :completed (:status res2)) "child2 completed")
      ;; parent chain unchanged by child execution (only spawn events, no child runtime events)
      (is (= parent-before-count (count parent-after)) "parent event count unchanged after child runs")
      ;; all child1 events have session/id == child1-id, none leak to parent
      (is (every? #(= child1-id (:session/id %)) child1-events) "child1 chain only contains child1 events")
      (is (every? #(= child2-id (:session/id %)) child2-events) "child2 chain only contains child2 events")
      (is (every? #(= parent-id (:session/id %)) parent-after) "parent chain only contains parent events")
      ;; no overlap: child ids not present in parent chain and vice versa
      (let [parent-ids (set (map :event/id parent-after))
            child1-ids (set (map :event/id child1-events))
            child2-ids (set (map :event/id child2-events))]
        (is (empty? (clojure.set/intersection parent-ids child1-ids)) "no event id overlap parent/child1")
        (is (empty? (clojure.set/intersection parent-ids child2-ids)) "no event id overlap parent/child2")
        (is (empty? (clojure.set/intersection child1-ids child2-ids)) "no event id overlap child1/child2"))
      ;; both chains hash-valid
      (is (:valid? (event/verify-event-chain db child1-id)) "child1 hash valid")
      (is (:valid? (event/verify-event-chain db child2-id)) "child2 hash valid")
      (is (:valid? (event/verify-event-chain db parent-id)) "parent hash valid")
      ;; child SCI isolation: running one child does not affect the other's outputs (both succeeded independently)
      (is (not= (:output-ref res1) (:output-ref res2)) "different output refs, no shared output state"))))
