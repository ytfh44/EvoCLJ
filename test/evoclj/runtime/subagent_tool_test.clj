(ns evoclj.runtime.subagent-tool-test
  "S6 agent/spawn tool surface with depth/budget caps."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.capability.mint :as mint]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.subagent :as subagent]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.tool.specs :as specs]
            [malli.core :as m])
  (:import (java.util Date UUID)))

(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private gen "generation-1")
(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 4102444800000))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (java.nio.file.Files/createTempFile "evoclj-s6-" ".db"
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
;; 1 — :agent/spawn tool descriptor is valid and has correct schema
;; ===========================================================================

(deftest agent-spawn-tool-descriptor-valid
  (testing ":agent/spawn descriptor validates and has correct shape"
    (let [desc subagent/agent-spawn-tool-descriptor]
      ;; validates via single source (tool.specs)
      (is (= desc (specs/validate-descriptor desc)) "validates via tool.specs")
      (is (= :agent/spawn (:tool/id desc)) "tool id")
      (is (= "Spawn a subagent session" (:tool/description desc)) "description")
      (is (map? (:tool/parameters desc)) "parameters is map")
      (is (= "object" (get-in desc [:tool/parameters :type])) "parameters type object")
      (is (= "string" (get-in desc [:tool/parameters :properties :task :type])) "task is string")
      (is (= ["task"] (get-in desc [:tool/parameters :required])) "task required")
      (is (= {:max-calls 10} (:tool/budget desc)) "budget max-calls 10")
      (is (= :pure (:effect desc)) "effect pure")
      (is (= :invoke (:required-action desc)) "required-action invoke")
      ;; input-schema must require :task string and validate correctly
      (is (m/validate (:input-schema desc) {:task "hello"}) "task validates")
      (is (not (m/validate (:input-schema desc) {})) "missing task fails")
      (is (m/validate (:input-schema desc) {:task "hello" :capabilities ["a" "b"]}) "capabilities optional"))
    ;; also canonical spec alias validates
    (let [desc specs/agent-spawn-tool]
      (is (= desc (specs/validate-descriptor desc)) "canonical spec validates")
      (is (= :agent/spawn (:tool/id desc)))
      (is (= {:max-calls 10} (:tool/budget desc))))
    ;; :agent/status also valid
    (let [desc subagent/agent-status-tool-descriptor]
      (is (= desc (specs/validate-descriptor desc)) "status validates")
      (is (= :agent/status (:tool/id desc)))
      (is (= "Query subagent status" (:tool/description desc))))))

;; ===========================================================================
;; 2 — spawn via tool call creates child session (integration with dispatch)
;; ===========================================================================

(deftest spawn-via-tool-call-creates-child
  (testing "spawn via :intent/tool-call :agent/spawn creates child session"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          reg (registry/create-registry)
          ;; also register the providers for completeness, though dispatch now handles agent tools directly via db fallback
          _ (registry/register! reg (subagent/agent-spawn-provider db))
          _ (registry/register! reg (subagent/agent-status-provider db))
          ctx (dispatch/make-broker-context {:registry reg :leases [pl] :db db})
          parent-events (event/events-for-session db parent-id)
          cause-id (:event/id (last parent-events))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/tool-call
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:tool/id :agent/spawn
                            :args {:task "child via tool"}}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)
          child-id (get-in res [:value :child/session-id])]
      (is (= :ok (:result/status res)) (str "dispatch ok: " (pr-str res)))
      (is (uuid? child-id) "child id is uuid")
      (let [child (session/get-session db child-id)]
        (is (some? child) "child session exists")
        (is (= parent-id (subagent/get-parent-session-id db child-id)) "parent link stored")
        (is (= :created (:state child)) "child state created"))
      ;; verify :agent/status tool also works via dispatch
      (let [cause2 (:event/id (last (event/events-for-session db parent-id)))
            status-intent {:intent/id (random-uuid)
                           :intent/type :intent/tool-call
                           :session/id parent-id
                           :phenotype/id phenotype
                           :node/id :node/tool
                           :cause/event-id cause2
                           :payload {:tool/id :agent/status
                                     :args {:session-id (str child-id)}}
                           :budget {:wall-ms 1000}
                           :metadata {}}
            status-res (dispatch/dispatch! ctx status-intent)]
        (is (= :ok (:result/status status-res)) "status dispatch ok")
        (is (= child-id (get-in status-res [:value :session/id])) "status returns child id")
        (is (= :created (get-in status-res [:value :state])) "status is :created"))))
  (testing "spawn via :intent/subagent-spawn also creates child (S2 path)"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [pl] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-spawn
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :child/spec {:task "via subagent intent"}
                            :child/capabilities []}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :ok (:result/status res)))
      (is (uuid? (get-in res [:value :child/session-id]))))))

;; ===========================================================================
;; 3 — depth cap enforced (depth 5 -> next spawn fails)
;; ===========================================================================

(deftest depth-cap-enforced
  (testing "depth 5 is max; spawn at depth 5 -> next fails with :subagent/depth-exceeded"
    (let [db (fresh-db)
          root (create-parent-session! db)
          root-id (:session/id root)]
      ;; build chain depth 5: root(0) -> a1(1) -> a2(2) -> a3(3) -> a4(4) -> a5(5)
      (loop [parent-id root-id depth 0 chain [root-id]]
        (if (= depth 5)
          (let [parent-depth (subagent/subagent-depth db parent-id)
                _ (is (= 5 parent-depth) "parent at depth 5")]
            ;; next spawn should exceed cap
            (try
              (subagent/spawn-subagent! db parent-id {:task "too-deep"} [])
              (is false "expected depth-exceeded")
              (catch clojure.lang.ExceptionInfo e
                (is (= :subagent/depth-exceeded (:error/type (ex-data e))) (str "got " (pr-str (ex-data e))))))
            ;; also via dispatch tool-call path should report error
            (let [reg (registry/create-registry)
                  ctx (dispatch/make-broker-context {:registry reg :db db})
                  cause-id (:event/id (last (event/events-for-session db parent-id)))
                  intent {:intent/id (random-uuid)
                          :intent/type :intent/tool-call
                          :session/id parent-id
                          :phenotype/id phenotype
                          :node/id :node/tool
                          :cause/event-id cause-id
                          :payload {:tool/id :agent/spawn :args {:task "too-deep via tool"}}
                          :budget {:wall-ms 1000}
                          :metadata {}}
                  res (dispatch/dispatch! ctx intent)]
              (is (= :error (:result/status res)) "dispatch returns error for depth exceeded")
              (is (= :subagent/depth-exceeded (:error/type res)) (str "error type: " (pr-str res)))))
          (let [res (subagent/spawn-subagent! db parent-id {:task (str "level-" (inc depth))} [])
                child-id (:child/session-id res)]
            (is (uuid? child-id) (str "spawn at depth " depth " ok"))
            (recur child-id (inc depth) (conj chain child-id))))))))

