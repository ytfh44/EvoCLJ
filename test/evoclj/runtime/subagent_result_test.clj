(ns evoclj.runtime.subagent-result-test
  "S5 result delivery — deliver-result! appends :subagent/result to parent,
  validation on non-completed child, and orphan recovery via subagent_links."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as jdbc]
            [evoclj.capability.mint :as mint]
            [evoclj.runtime.subagent :as subagent]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util Date UUID)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private gen "generation-1")
(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 4102444800000))
(def ^:private cas-ref-good (str "sha256:" (apply str (repeat 64 "f"))))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-s5-" ".db" (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try (Files/deleteIfExists (.toPath (java.io.File. p))) (catch Exception _)))
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
        (try (jdbc/insert! conn :genomes {:id genome :created_at now})
             (catch Exception _))
        (try (jdbc/insert! conn :generations {:id gen :genome_id genome :resolution_id resolution :parent_id nil :state "active" :current 1 :created_at now})
             (catch Exception _))))
    db))

(defn- parent-lease [session-id phenotype-id actions]
  (mint/mint-lease! nil {:subject {:session/id session-id :phenotype/id phenotype-id}
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
                             :cause/event-id nil
                             :payload-ref nil
                             :metadata {}})
    sess))

;; ---------------------------------------------------------------------------
;; 1 — deliver-result! appends :subagent/result to parent with correct ids
;; ---------------------------------------------------------------------------

(deftest deliver-result-appends-subagent-result-to-parent
  (testing "deliver-result! appends :subagent/result event to parent with correct child id and cas-ref"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "child-task"} [pl])
          _ (subagent/run-subagent! db parent-id session-id {:text "hello-echo"})
          ;; child is now :completed
          child-sess (session/get-session db session-id)
          _ (is (= :completed (:state child-sess)) "child should be :completed after run")
          result-event (subagent/deliver-result! db parent-id session-id cas-ref-good)
          parent-events (event/events-for-session db parent-id)
          result-events (filter #(= :subagent/result (:event/type %)) parent-events)
          latest (last result-events)]
      (is (some? result-event) "deliver-result! returned event")
      (is (= :subagent/result (:event/type latest)) "parent has :subagent/result")
      (is (= session-id (:child/session-id (:metadata latest))) "metadata has correct child id")
      (is (= cas-ref-good (:result/cas-ref (:metadata latest))) "metadata has correct cas-ref")
      (is (= :succeeded (:result/status (:metadata latest))) "metadata status is :succeeded")
      (is (:valid? (event/verify-event-chain db parent-id)) "parent chain hash valid")
      (is (:valid? (event/verify-event-chain db session-id)) "child chain hash valid"))))

;; ---------------------------------------------------------------------------
;; 2 — deliver on non-completed child fails :subagent/not-completed
;; ---------------------------------------------------------------------------

(deftest deliver-on-running-child-fails-not-completed
  (testing "deliver-result! on non-completed (running/created) child throws :subagent/not-completed"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "child2"} [pl])]
      ;; child is still :created, not completed
      (let [ex (try (subagent/deliver-result! db parent-id session-id cas-ref-good)
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex) "should throw")
        (is (= :subagent/not-completed (:error/type (ex-data ex))) "error type is :subagent/not-completed")
        (is (= session-id (:session/id (ex-data ex))) "error carries child id"))
      ;; also ensure no :subagent/result was appended to parent
      (let [parent-events (event/events-for-session db parent-id)
            result-events (filter #(= :subagent/result (:event/type %)) parent-events)]
        (is (empty? result-events) "no result event appended on failure")))))

;; ---------------------------------------------------------------------------
;; 3 — orphaned child (parent completed, child running) is found by recovery
;; ---------------------------------------------------------------------------

(deftest orphaned-child-found-by-recovery-helper
  (testing "orphaned child (parent completed, child running) is found by find-orphaned-subagents"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "orphan-task"} [pl])
          child-id session-id]
      ;; move child to :running via valid transitions :created->:resolving->:running
      (session/transition-session! db child-id :created :resolving nil)
      (session/transition-session! db child-id :resolving :running nil)
      ;; move parent to :completed via :created->:resolving->:running->:waiting->:completed
      (session/transition-session! db parent-id :created :resolving nil)
      (session/transition-session! db parent-id :resolving :running nil)
      (session/transition-session! db parent-id :running :waiting nil)
      (session/transition-session! db parent-id :waiting :completed nil)
      (let [parent-sess (session/get-session db parent-id)
            child-sess (session/get-session db child-id)]
        (is (= :completed (:state parent-sess)) "parent is :completed")
        (is (= :running (:state child-sess)) "child is :running"))
      (let [orphans (recovery/find-orphaned-subagents db)]
        (is (= 1 (count orphans)) "exactly one orphan")
        (let [o (first orphans)]
          (is (= parent-id (:parent/session-id o)) "orphan parent id correct")
          (is (= child-id (:child/session-id o)) "orphan child id correct")
          (is (= :completed (:parent/state o)) "parent state :completed")
          (is (= :running (:child/state o)) "child state :running")))
      ;; also ensure non-orphan case (parent running, child running) is not reported
      (let [db2 (fresh-db)
            parent2 (create-parent-session! db2)
            pid2 (:session/id parent2)
            pl2 (parent-lease pid2 phenotype #{:invoke})
            {:keys [child/session-id]} (subagent/spawn-subagent! db2 pid2 {} [pl2])
            cid2 session-id]
        (session/transition-session! db2 cid2 :created :resolving nil)
        (session/transition-session! db2 cid2 :resolving :running nil)
        (session/transition-session! db2 pid2 :created :resolving nil)
        (session/transition-session! db2 pid2 :resolving :running nil)
        (let [orphans2 (recovery/find-orphaned-subagents db2)]
          (is (empty? orphans2) "no orphan when parent still :running"))))))
