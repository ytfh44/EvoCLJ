(ns evoclj.runtime.subagent-cancel-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as jdbc]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.mint :as mint]
            [evoclj.broker.registry :as reg]
            [evoclj.runtime.subagent :as subagent]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.work :as work-store]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.intent.core :as icore])
  (:import (java.util Date UUID)))
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private gen "generation-1")
(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 4102444800000))
(def ^:private in-window (Date. 1700001800000))
(def ^:private db-paths (atom []))
(defn- temp-db-path []
  (let [f (java.io.File/createTempFile "evoclj-cancel-test-" ".db")
        p (.getAbsolutePath f)]
    (.delete f)
    (swap! db-paths conj p)
    p))
(defn- cleanup! []
  (doseq [p @db-paths]
    (try (.delete (java.io.File. p)) (catch Exception _)))
  (reset! db-paths [])
  (try (subagent/clear-subagent-lease-state!) (catch Exception _)))
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
        (try (jdbc/insert! conn :genomes {:id genome :created_at now}) (catch Exception _))
        (try (jdbc/insert! conn :generations {:id gen :genome_id genome :resolution_id resolution :parent_id nil :state "active" :current 1 :created_at now}) (catch Exception _))))
    db))

(defn- session-work-state [db sid]
  (some-> (last (work-store/list-works db sid)) :work/state))
(defn- parent-lease [session-id phenotype-id actions]
  (mint/mint-lease! nil {:principal {:principal/type :session :session/id session-id}
                         :resource {:kind :tool :id :fixture/echo}
                         :actions actions
                         :constraints {:max-calls 10}
                         :issued-at issued-at
                         :expires-at expires-at}))
(defn- create-parent-session! [db]
  (let [sess (session/create-session! db {:genome/id genome :resolution/id resolution :phenotype/id phenotype :generation/id gen})
        sid (:session/id sess)]
    (event/append-event! db {:session/id sid :generation/id gen :phenotype/id phenotype :event/type :session/created :prev/event-id nil :payload-ref nil :metadata {}})
    sess))
(defn- tool-intent [session-id phenotype-id]
  (icore/tool-call session-id phenotype-id :node/test 1 {:tool/id :fixture/echo :args {:text "hi"}} {:wall-ms 1000}))
(defn- authorize-with-lease [lease]
  (let [pr (:principal lease)
        sid (:session/id pr)
        pid (or (:phenotype/id pr) phenotype)
        it (tool-intent sid pid)
        normalized {:resource {:kind :tool :id :fixture/echo} :action :invoke}
        registry (reg/default-registry)]
    (broker/authorize {:intent it :normalized-request normalized :leases [lease] :usage {} :now in-window :registry registry :lease-registry subagent/subagent-lease-registry})))
(deftest cancel-single-child-revokes-leases
  (testing "cancel-subagent! revokes child's derived leases; next authorize is :capability/revoked"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:child/keys [session-id capabilities]} (subagent/spawn-subagent! db parent-id {} [pl])
          child-id session-id
          child-lease (first capabilities)]
      (is (some? child-lease) "spawn produced a derived lease")
      (let [d1 (authorize-with-lease child-lease)]
        (is (= :allow (:decision d1)) "child lease allows before cancel"))
      (let [res (subagent/cancel-subagent! db parent-id child-id :user-request)]
        (is (some? res) "cancel returns result")
        (is (contains? (set (:cancelled res)) child-id) "cancelled includes child"))
      (is (= :cancelled (session-work-state db child-id)) "child Work is :cancelled (Work owns the lifecycle)")
      (let [evts (event/events-for-session db parent-id)
            types (set (map :event/type evts))]
        (is (contains? types :subagent/cancelled) "parent has :subagent/cancelled"))
      (let [evts (event/events-for-session db child-id)
            types (set (map :event/type evts))]
        (is (contains? types :session/cancelled) "child has :session/cancelled"))
      (let [d2 (authorize-with-lease child-lease)]
        (is (= :deny (:decision d2)) "after cancel lease is denied")
        (is (= :capability/revoked (:reason d2)) "reason is :capability/revoked")))))
(deftest cancel-parent-cascades-to-grandchildren
  (testing "3-level tree: cancel of mid node cascades to grandchild (and tree helper cascades from root)"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          r1 (subagent/spawn-subagent! db parent-id {} [pl])
          l1-id (:child/session-id r1)
          l1-leases (:child/capabilities r1)
          l1-lease (first l1-leases)
          r2 (subagent/spawn-subagent! db l1-id {} l1-leases)
          l2-id (:child/session-id r2)
          l2-leases (:child/capabilities r2)
          l2-lease (first l2-leases)
          r3 (subagent/spawn-subagent! db l2-id {} l2-leases)
          l3-id (:child/session-id r3)
          l3-lease (first (:child/capabilities r3))]
      (is (= :allow (:decision (authorize-with-lease l1-lease))) "L1 allows before")
      (is (= :allow (:decision (authorize-with-lease l2-lease))) "L2 allows before")
      (is (= :allow (:decision (authorize-with-lease l3-lease))) "L3 allows before")
      (let [desc (session/list-descendants db parent-id)]
        (is (= 3 (count desc)) "parent has 3 descendants")
        (is (= #{l1-id l2-id l3-id} (set desc)) "descendants are L1,L2,L3"))
      (subagent/cancel-subagent! db parent-id l1-id :parent-cancel)
      (is (= :capability/revoked (:reason (authorize-with-lease l1-lease))) "L1 revoked after cascade")
      (is (= :capability/revoked (:reason (authorize-with-lease l2-lease))) "L2 revoked after cascade")
      (is (= :capability/revoked (:reason (authorize-with-lease l3-lease))) "L3 revoked after cascade")
      (is (= :cancelled (session-work-state db l1-id)) "L1 Work cancelled")
      (is (= :cancelled (session-work-state db l2-id)) "L2 Work cancelled")
      (is (= :cancelled (session-work-state db l3-id)) "L3 Work cancelled")
      (let [db2 (fresh-db)
            parent2 (create-parent-session! db2)
            pid2 (:session/id parent2)
            pl2 (parent-lease pid2 phenotype #{:invoke})
            a (subagent/spawn-subagent! db2 pid2 {} [pl2])
            a-id (:child/session-id a)
            a-lease (first (:child/capabilities a))
            b (subagent/spawn-subagent! db2 a-id {} (:child/capabilities a))
            b-id (:child/session-id b)
            b-lease (first (:child/capabilities b))]
        (is (= :allow (:decision (authorize-with-lease a-lease))) "tree: A allows before")
        (let [res (subagent/cancel-subagent-tree! db2 pid2 :user-request)]
          (is (some? res) "tree cancel returns")
          (is (contains? (set (:cancelled res)) a-id) "tree includes A")
          (is (contains? (set (:cancelled res)) b-id) "tree includes B"))
        (is (= :capability/revoked (:reason (authorize-with-lease a-lease))) "A revoked after tree")
        (is (= :capability/revoked (:reason (authorize-with-lease b-lease))) "B revoked after tree")))))
(deftest cancel-already-cancelled-is-idempotent
  (testing "double cancel is idempotent: no throw, still :cancelled, still :capability/revoked"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:child/keys [session-id capabilities]} (subagent/spawn-subagent! db parent-id {} [pl])
          child-id session-id
          child-lease (first capabilities)]
      (let [r1 (subagent/cancel-subagent! db parent-id child-id :user-request)]
        (is (false? (:already-cancelled? r1)) "first cancel not already-cancelled")
        (is (= :cancelled (session-work-state db child-id)) "after first, Work cancelled"))
      (let [before-evts (count (event/events-for-session db child-id))
            r2 (subagent/cancel-subagent! db parent-id child-id :user-request)]
        (is (true? (:already-cancelled? r2)) "second cancel reports already-cancelled")
        (is (= :cancelled (session-work-state db child-id)) "still cancelled")
        (let [after-evts (count (event/events-for-session db child-id))]
          (is (= before-evts after-evts) "no duplicate :session/cancelled event on idempotent retry"))
        (is (= :capability/revoked (:reason (authorize-with-lease child-lease))) "still revoked after second cancel")
        (let [r3 (subagent/cancel-subagent-tree! db child-id :user-request)]
          (is (true? (:already-cancelled? r3)) "tree cancel on already-cancelled is idempotent"))))))
