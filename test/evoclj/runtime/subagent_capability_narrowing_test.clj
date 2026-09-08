(ns evoclj.runtime.subagent-capability-narrowing-test
  "Capability slice: spawn derivation is Grant meet, not identity.
  The model's :capabilities request narrows (fewer requested caps =
  fewer child leases); the spawn right is denied unless explicitly granted."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
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

(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private gen "generation-1")
(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 4102444800000))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (java.nio.file.Files/createTempFile "evoclj-capnarr- " ".db"
                                                   (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try (java.nio.file.Files/deleteIfExists (java.nio.file.Paths/get p (into-array String [])))
         (catch Exception _)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!) (subagent/clear-subagent-lease-state!)))

(defn- fresh-db []
  (let [path (temp-db-path)
        _ (migrate/migrate! path)
        db path]
    (sqlite/with-db [conn db]
      (let [now "2025-01-01T00:00:00Z"]
        (doseq [h [genome resolution phenotype]]
          (try (clojure.java.jdbc/insert! conn :artifacts {:hash h :media_type "application/octet-stream" :size 0 :created_at now})
               (catch Exception _)))
        (try (clojure.java.jdbc/insert! conn :genomes {:id genome :created_at now})
             (catch Exception _))
        (try (clojure.java.jdbc/insert! conn :generations {:id gen :genome_id genome :resolution_id resolution :parent_id nil :state "active" :current 1 :created_at now})
             (catch Exception _))))
    db))

(defn- tool-lease [session-id tool-id actions]
  (mint/mint-lease! nil {:principal {:principal/type :session :session/id session-id}
                         :resource {:kind :tool :id tool-id}
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
    (evoclj.store.work/create-work! db {:work/id (UUID/randomUUID)
                                        :work/type :session/run
                                        :work/state :queued
                                        :work/session-id sid
                                        :work/created-at (Date. 1700000000000)})
    sess))

(defn- child-tool-ids [caps]
  (set (map #(get-in % [:resource :id]) caps)))

(deftest narrowing-request-yields-fewer-leases
  (testing "requesting one of two parent caps yields one child lease"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl1 (tool-lease parent-id :fixture/echo #{:invoke})
          pl2 (tool-lease parent-id :agent/status #{:invoke})
          res (subagent/spawn-subagent! db parent-id
                                        {:task "narrowed work"
                                         :capabilities ["tool:fixture/echo"]}
                                        [pl1 pl2])
          caps (:child/capabilities res)]
      (is (= 1 (count caps)) "fewer requested caps = fewer child leases")
      (is (= #{:fixture/echo} (child-tool-ids caps)) "the requested cap survives")
      (is (every? schema/lease? caps) "child leases are sealed")
      (is (= {:principal/type :session :session/id (:child/session-id res)}
             (:principal (first caps)))
          "child lease subject is the child session"))))

(deftest spawn-right-denied-by-default
  (testing "implicit spawn-right inheritance is denied without an explicit request"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl1 (tool-lease parent-id :fixture/echo #{:invoke})
          pl2 (tool-lease parent-id :agent/spawn #{:invoke})
          res (subagent/spawn-subagent! db parent-id {:task "no spawn for child"} [pl1 pl2])
          caps (:child/capabilities res)]
      (is (= 1 (count caps)) "spawn lease dropped by default")
      (is (= #{:fixture/echo} (child-tool-ids caps)) "non-spawn leases still derived"))))

(deftest spawn-right-explicit-request-inherits
  (testing "explicitly requesting the spawn right inherits it"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl1 (tool-lease parent-id :fixture/echo #{:invoke})
          pl2 (tool-lease parent-id :agent/spawn #{:invoke})
          res (subagent/spawn-subagent! db parent-id
                                        {:task "delegating work"
                                         :capabilities ["tool:agent/spawn" "tool:fixture/echo"]}
                                        [pl1 pl2])
          caps (:child/capabilities res)]
      (is (= 2 (count caps)) "explicit spawn request is honored")
      (is (= #{:fixture/echo :agent/spawn} (child-tool-ids caps))))))

(deftest spawn-provider-wires-capabilities-request
  (testing ":agent/spawn :capabilities arg narrows the derived child leases end to end"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          reg (registry/create-registry)
          _ (registry/register! reg (subagent/agent-spawn-provider db))
          _ (registry/register! reg (subagent/agent-status-provider db))
          ctx (dispatch/make-broker-context {:registry reg
                                             :leases [(tool-lease parent-id :agent/spawn #{:invoke})
                                                      (tool-lease parent-id :fixture/echo #{:invoke})
                                                      (tool-lease parent-id :agent/status #{:invoke})]
                                             :db db})
          parent-events (event/events-for-session db parent-id)
          cause-id (:event/id (last parent-events))
          intent {:intent/id (UUID/randomUUID)
                  :intent/type :intent/tool-call
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:tool/id :agent/spawn
                            :args {:task "narrowed via tool"
                                   :capabilities ["tool:fixture/echo"]}}
                  :budget {:wall-ms 1000}
                  ;; :agent/spawn is :effect :write — the tool-call path
                  ;; demands an idempotency key in :metadata.
                  :metadata {:idempotency/key (str (UUID/randomUUID))}}
          res (dispatch/dispatch! ctx intent)
          caps (get-in res [:value :child/capabilities])]
      (is (= :ok (:result/status res)) (str "dispatch ok: " (pr-str res)))
      (is (= 1 (count caps)) "tool-level request narrows to one child lease")
      (is (= #{:fixture/echo} (child-tool-ids caps))))))
