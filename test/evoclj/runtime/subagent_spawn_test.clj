(ns evoclj.runtime.subagent-spawn-test
  "S2 subagent spawn — session creation + derived leases + parent event (GC-20, P3/S1)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.capability.mint :as mint]
            [evoclj.capability.schema :as schema]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.registry :as registry]
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
  (let [p (str (java.nio.file.Files/createTempFile "evoclj-s2-" ".db"
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

;; ===========================================================================
;; 1 — spawn creates child session row with parent link
;; ===========================================================================

(deftest spawn-creates-child-session-with-parent-link
  (testing "spawn creates child row pinned to parent genome/resolution/phenotype and records parent link"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke :read})
          res (subagent/spawn-subagent! db parent-id {:task "hello"} [pl])
          child-id (:child/session-id res)
          child-sess (:child/session res)]
      (is (uuid? child-id) "child id is uuid")
      (is (some? child-sess) "child session map returned")
      (is (= :created (:state child-sess)) "child status :created")
      (is (= genome (:genome/id child-sess)) "same genome as parent")
      (is (= resolution (:resolution/id child-sess)) "same resolution as parent")
      (is (= phenotype (:phenotype/id child-sess)) "same phenotype as parent")
      (is (= gen (:generation/id child-sess)) "same generation as parent")
      (is (= parent-id (subagent/get-parent-session-id db child-id)) "parent link stored")
      (is (= [child-id] (subagent/child-session-ids db parent-id)) "child appears in parent's children")
      (let [child-events (event/events-for-session db child-id)]
        (is (= :session/created (:event/type (first child-events))) "child chain opens with :session/created")))))

;; ===========================================================================
;; 2 — parent event chain has :subagent/spawned with child id
;; ===========================================================================

(deftest spawn-appends-subagent-spawned-event-to-parent
  (testing "parent chain gets :subagent/spawned event causally after its latest"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          before (event/events-for-session db parent-id)
          latest-before (:event/id (last before))
          res (subagent/spawn-subagent! db parent-id {:task "child-task"} [])
          child-id (:child/session-id res)
          after (event/events-for-session db parent-id)
          spawned (last after)]
      (is (= (inc (count before)) (count after)) "one event appended")
      (is (= :subagent/spawned (:event/type spawned)) "type is :subagent/spawned")
      (is (= latest-before (:cause/event-id spawned)) "cause is parent's previous latest")
      (is (= child-id (get-in spawned [:metadata :child/session-id])) "metadata carries child id")
      (is (:valid? (event/verify-event-chain db parent-id)) "parent chain hash still valid")
      (is (:valid? (event/verify-event-chain db child-id)) "child chain hash valid"))))

;; ===========================================================================
;; 3 — child capabilities are attenuated (actions ⊆ parent)
;; ===========================================================================

(deftest child-capabilities-are-attenuated
  (testing "derived child leases have actions ⊆ parent and subject is child"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl1 (parent-lease parent-id phenotype #{:invoke :read :list})
          pl2 (parent-lease parent-id phenotype #{:invoke})
          res (subagent/spawn-subagent! db parent-id {} [pl1 pl2])
          child-id (:child/session-id res)
          caps (:child/capabilities res)]
      (is (= 2 (count caps)) "one derived per parent lease")
      (doseq [cl caps]
        (is (schema/lease? cl) "child lease is sealed")
        (is (= {:session/id child-id :phenotype/id phenotype} (:subject cl)) "subject is child"))
      (let [parent-actions (set (mapcat :actions [pl1 pl2]))
            child-actions (set (mapcat :actions caps))]
        (is (set/subset? child-actions parent-actions) "child actions subset of parent union"))
      (is (set/subset? (:actions (first caps)) (:actions pl1)) "first child ⊆ first parent")
      (is (set/subset? (:actions (second caps)) (:actions pl2)) "second child ⊆ second parent")
      (doseq [[pl cl] (map vector [pl1 pl2] caps)]
        (is (not (.before ^Date (:issued-at cl) ^Date (:issued-at pl))) "child issued >= parent issued")
        (is (not (.after ^Date (:expires-at cl) ^Date (:expires-at pl))) "child expires <= parent expires")))))

;; ===========================================================================
;; 4 — dispatch :intent/subagent-spawn via broker (and existing dispatch still green)
;; ===========================================================================

(deftest dispatch-subagent-spawn-returns-child-id
  (testing "intent/dispatch handles :intent/subagent-spawn and returns child id"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          parent-events (event/events-for-session db parent-id)
          cause-id (:event/id (last parent-events))
          pl (parent-lease parent-id phenotype #{:invoke})
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [pl] :db db})
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-spawn
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :child/spec {:task "via-dispatch"}
                            :child/capabilities []}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :ok (:result/status res)) "dispatch returns :ok")
      (is (uuid? (get-in res [:value :child/session-id])) "value carries child id")
      (let [child-id (get-in res [:value :child/session-id])
            child (session/get-session db child-id)]
        (is (some? child) "child session exists in store")
        (is (= parent-id (subagent/get-parent-session-id db child-id)) "link via dispatch")))))

  (deftest existing-tool-call-dispatch-still-green
    (testing "existing :intent/tool-call still dispatches through pipeline (regression)"
      (let [reg (registry/create-registry)
            _ (registry/register! reg
                                  (reify evoclj.provider.protocol/Provider
                                    (describe [_] {:tool/id :fixture/echo :effect :pure :input-schema [:map [:text string?]] :output-schema [:map [:text string?]] :required-action :invoke :retry {:safe? true}})
                                    (normalize-request [_ intent] {:tool/id :fixture/echo :resource {:kind :tool :id :fixture/echo} :args (get-in intent [:payload :args])})
                                    (execute-request! [_ req] {:text (get-in req [:args :text])})))
            sid (random-uuid)
            pl (mint/mint-lease! nil {:subject {:session/id sid :phenotype/id phenotype}
                                      :resource {:kind :tool :id :fixture/echo}
                                      :actions #{:invoke}
                                      :constraints {}
                                      :issued-at issued-at
                                      :expires-at expires-at})
            ctx (dispatch/make-broker-context {:registry reg :leases [pl]})
            intent {:intent/id (random-uuid)
                    :intent/type :intent/tool-call
                    :session/id sid
                    :phenotype/id phenotype
                    :node/id :node/tool
                    :cause/event-id 1
                    :payload {:tool/id :fixture/echo :args {:text "hello"}}
                    :budget {:wall-ms 1000}
                    :metadata {:idempotency/key (str (random-uuid))}}
            res (dispatch/dispatch! ctx intent)]
        (is (= :ok (:result/status res)) "tool echo still ok")
        (is (= {:text "hello"} (:value res)) "value correct"))))
