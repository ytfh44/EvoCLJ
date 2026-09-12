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
            [evoclj.store.work :as work-store]
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
  (mint/mint-lease! nil {:principal {:principal/type :session :session/id session-id}
                         :resource {:kind :tool :id :fixture/echo}
                         :actions actions
                         :constraints {}
                         :issued-at issued-at
                         :expires-at expires-at}))

 (defn- agent-lease
  "Exact lease for one agent tool (:agent/spawn or :agent/status) bound
  to the requesting session (I2 principal equality, exact tool id)."
  [session-id tool-id actions]
  (mint/mint-lease! nil {:principal {:principal/type :session :session/id session-id}
                         :resource {:kind :tool :id tool-id}
                         :actions actions
                         :constraints {}
                         :issued-at issued-at
                         :expires-at expires-at}))

 (defn- create-parent-work!
  "Create a queued parent Work for `session-id` and return its id (W2:
  the spawn intent's :parent/work-id attribution)."
  [db session-id]
  (let [wid (UUID/randomUUID)]
    (work-store/create-work! db {:work/id wid
                                 :work/type :session/run
                                 :work/state :queued
                                 :work/session-id session-id})
    wid))


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
    (work-store/create-work! db {:work/id (UUID/randomUUID)
                                 :work/type :session/run
                                 :work/state :queued
                                 :work/session-id sid
                                 :work/created-at (Date. 1700000000000)})
    sess))

;; ===========================================================================
;; 1 — :agent/spawn tool descriptor is valid and has correct schema
;; ===========================================================================

(deftest agent-spawn-tool-descriptor-valid
  (testing ":agent/spawn descriptor validates and has correct shape"
    (let [desc subagent/agent-spawn-tool-descriptor]
      ;; validates via single source (tool.specs)
      (is (= desc (specs/validate-descriptor desc)) "validates via tool.specs")
      (is (identical? desc specs/agent-spawn-tool) "no duplicate descriptor: alias IS the canonical map")
      (is (= :agent/spawn (:tool/id desc)) "tool id")
      (is (= "Spawn a subagent session" (:tool/description desc)) "description")
      (is (map? (:tool/parameters desc)) "parameters is map")
      (is (= "object" (get-in desc [:tool/parameters :type])) "parameters type object")
      (is (= "string" (get-in desc [:tool/parameters :properties :task :type])) "task is string")
      (is (= ["task"] (get-in desc [:tool/parameters :required])) "task required")
      (is (= {:max-calls 10} (:tool/budget desc)) "budget max-calls 10")
      (is (= :write (:effect desc)) "effect write: spawn persists session row + event + link + child Work")
      (is (= :invoke (:required-action desc)) "required-action invoke")
      ;; input-schema must require :task string and validate correctly
      (is (m/validate (:input-schema desc) {:task "hello"}) "task validates")
      (is (not (m/validate (:input-schema desc) {})) "missing task fails")
      (is (m/validate (:input-schema desc) {:task "hello" :capabilities ["a" "b"]}) "capabilities optional")
      ;; output carries the child Work handle (the durable execution identity)
      (is (m/validate (:output-schema desc) {:child/session-id (random-uuid) :child/work-id (random-uuid)})
          "output validates with session + work handles"))
    ;; also canonical spec alias validates
    (let [desc specs/agent-spawn-tool]
      (is (= desc (specs/validate-descriptor desc)) "canonical spec validates")
      (is (= :agent/spawn (:tool/id desc)))
      (is (= {:max-calls 10} (:tool/budget desc))))
    ;; :agent/status also valid
    (let [desc subagent/agent-status-tool-descriptor]
      (is (= desc (specs/validate-descriptor desc)) "status validates")
      (is (identical? desc specs/agent-status-tool) "status alias IS the canonical map")
      (is (= :agent/status (:tool/id desc)))
      (is (= "Query subagent status" (:tool/description desc))))
    ;; :agent/cancel also valid
    (let [desc subagent/agent-cancel-tool-descriptor]
      (is (= desc (specs/validate-descriptor desc)) "cancel validates")
      (is (identical? desc specs/agent-cancel-tool) "cancel alias IS the canonical map")
      (is (= :agent/cancel (:tool/id desc))))))

(deftest subagent-tool-catalog-is-single-sourced
  (testing "subagent-tool-catalog carries the tool.specs wire entries, no duplicates"
    (is (= 3 (count subagent/subagent-tool-catalog)) "three S6 wire tools")
    (is (= [specs/agent-spawn-wire-tool specs/agent-status-wire-tool specs/agent-cancel-wire-tool]
           subagent/subagent-tool-catalog)
        "catalog IS the canonical wire entries")
    (is (= #{:agent/spawn :agent/status :agent/cancel}
           (set (map :tool subagent/subagent-tool-catalog)))
        "catalog pins exactly the three agent tool ids")))

;; ===========================================================================
;; 2 — spawn via tool call creates child session (integration with dispatch)
;; ===========================================================================

(deftest spawn-via-tool-call-creates-child
  (testing "spawn via :intent/tool-call :agent/spawn creates child session"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          reg (registry/create-registry)
          _ (registry/register! reg (subagent/agent-spawn-provider db))
          _ (registry/register! reg (subagent/agent-status-provider db))
          ctx (dispatch/make-broker-context {:registry reg
                                             :leases [(agent-lease parent-id :agent/spawn #{:invoke})
                                                      (agent-lease parent-id :agent/status #{:invoke})]
                                             :db db})
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
                  ;; :agent/spawn is :effect :write — the tool-call path
                  ;; demands an idempotency key in :metadata.
                  :metadata {:idempotency/key (str (random-uuid))}}
          res (dispatch/dispatch! ctx intent)
          child-id (get-in res [:value :child/session-id])
          child-work-id (get-in res [:value :child/work-id])]
      (is (= :ok (:result/status res)) (str "dispatch ok: " (pr-str res)))
      (is (uuid? child-id) "child id is uuid")
      (is (seq (get-in res [:value :child/capabilities]))
          "child inherits the broker leases (attenuated, never empty)")
      (let [child (session/get-session db child-id)]
        (is (some? child) "child session exists")
        (is (= parent-id (work-store/get-parent-session-id db child-id)) "parent link stored")
        (is (not (contains? child :state)) "session row has no lifecycle state"))
      ;; W2: the lifecycle truth is the child Work CAS — exactly one queued
      ;; :subagent/run Work, and the returned handle names it.
      (let [child-works (work-store/list-works db child-id)]
        (is (= 1 (count child-works)) "exactly one child Work per spawn")
        (is (= :queued (:work/state (first child-works))) "child Work starts :queued")
        (is (= :subagent/run (:work/type (first child-works))) "child Work type is :subagent/run")
        (is (= (:work/id (first child-works)) child-work-id) "returned handle names the Work row"))
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
        ;; W2: Work is the sole lifecycle — a spawned child has a queued
        ;; :subagent/run Work, so status reports :queued (not the session
        ;; row's :created).
        (is (= :queued (get-in status-res [:value :state])) "status is :queued")))
  (testing "spawn via :intent/subagent-spawn also creates child (S2 path)"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          parent-work-id (create-parent-work! db parent-id)
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-spawn
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :parent/work-id parent-work-id
                            :child/spec {:task "via subagent intent"}
                            :child/capabilities []}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :ok (:result/status res)))
      (is (uuid? (get-in res [:value :child/session-id])))))))

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
                  _ (registry/register! reg (subagent/agent-spawn-provider db))
                  ctx (dispatch/make-broker-context {:registry reg
                                                     :leases [(agent-lease parent-id :agent/spawn #{:invoke})]
                                                     :db db})
                  cause-id (:event/id (last (event/events-for-session db parent-id)))
                  intent {:intent/id (random-uuid)
                          :intent/type :intent/tool-call
                          :session/id parent-id
                          :phenotype/id phenotype
                          :node/id :node/tool
                          :cause/event-id cause-id
                          :payload {:tool/id :agent/spawn :args {:task "too-deep via tool"}}
                          :budget {:wall-ms 1000}
                          :metadata {:idempotency/key (str (random-uuid))}}
                  res (dispatch/dispatch! ctx intent)]
              (is (= :error (:result/status res)) "dispatch returns error for depth exceeded")
              ;; Through the provider pipeline every provider throw is a
              ;; :provider/execution-failed whose cause preserves the typed
              ;; domain error.
              (is (= :provider/execution-failed (:error/type res)) (str "error type: " (pr-str res)))
              (is (= :subagent/depth-exceeded (get-in res [:error/data :cause :error/type])))))
          (let [res (subagent/spawn-subagent! db parent-id {:task (str "level-" (inc depth))} [])
                child-id (:child/session-id res)]
            (is (uuid? child-id) (str "spawn at depth " depth " ok"))
            (recur child-id (inc depth) (conj chain child-id))))))))

;; ===========================================================================
;; 4 — agent tools authorize against exact leases (no special-case bypass)
;; ===========================================================================

(deftest agent-tools-require-exact-leases
  (testing "spawn without any lease denies (exact lease auth, no bypass)"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          reg (registry/create-registry)
          _ (registry/register! reg (subagent/agent-spawn-provider db))
          _ (registry/register! reg (subagent/agent-status-provider db))
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/tool-call
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:tool/id :agent/spawn :args {:task "no lease"}}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :error (:result/status res)) "no lease means denied")
      (is (= :capability/denied (:error/type res)) (str "got " (pr-str res)))))
  (testing "a lease for another tool does not authorize spawn"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          reg (registry/create-registry)
          _ (registry/register! reg (subagent/agent-spawn-provider db))
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(parent-lease parent-id phenotype #{:invoke})]
                :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/tool-call
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:tool/id :agent/spawn :args {:task "wrong lease"}}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :capability/denied (:error/type res)) (str "got " (pr-str res)))))
  (testing "unregistered agent tool fails closed as not-found"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(agent-lease parent-id :agent/spawn #{:invoke})]
                :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/tool-call
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:tool/id :agent/spawn :args {:task "no provider"}}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :provider/not-found (:error/type res)) (str "got " (pr-str res))))))

;; ===========================================================================
;; 5 — spawn intent binds parent to the principal + validates parent work
;; ===========================================================================

(deftest spawn-intent-validates-principal-and-work
  (testing "payload parent differing from intent session is principal-mismatch"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-spawn
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id (random-uuid)
                            :parent/work-id (random-uuid)
                            :child/spec {:task "mismatch"}
                            :child/capabilities []}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :error (:result/status res)))
      (is (= :capability/principal-mismatch (:error/type res)) (str "got " (pr-str res)))))
  (testing "missing parent work is work-not-found"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-spawn
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :parent/work-id (random-uuid)
                            :child/spec {:task "no work"}
                            :child/capabilities []}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :store/work-not-found (:error/type res)) (str "got " (pr-str res)))))
  (testing "parent work owned by another session is work-invalid"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          other (create-parent-session! db)
          other-work-id (create-parent-work! db (:session/id other))
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-spawn
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :parent/work-id other-work-id
                            :child/spec {:task "foreign work"}
                            :child/capabilities []}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :store/work-invalid (:error/type res)) (str "got " (pr-str res)))))
  (testing "explicit parent work becomes the child work's parent"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          parent-work-id (create-parent-work! db parent-id)
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-spawn
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :parent/work-id parent-work-id
                            :child/spec {:task "attributed"}
                            :child/capabilities []}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)
          child-id (get-in res [:value :child/session-id])
          child-works (work-store/list-works db child-id)]
      (is (= :ok (:result/status res)) (str "got " (pr-str res)))
      (is (= 1 (count child-works)) "one child work")
      (is (= parent-work-id (:work/parent-work-id (first child-works)))
          "child work attributed to the explicit parent work"))))

;; ===========================================================================
;; 6 — result and cancel intents execute end to end
;; ===========================================================================

(def ^:private cas-ref-good
  (str "sha256:" (apply str (repeat 64 "b"))))

(deftest result-and-cancel-intents-execute
  (testing "subagent-result delivers after the child completes (idempotent with run-time auto-delivery)"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id child/work-id]} (subagent/spawn-subagent! db parent-id {:task "child-task"} [pl])
          _ (subagent/run-subagent! db parent-id session-id {:text "hello-echo"})
          ;; the supplied CAS must equal the child Work's payload_ref
          cas-ref (:work/payload-ref (work-store/fetch-work db work-id))
          _ (is (string? cas-ref) "child work carries the output CAS ref")
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          parent-events-before (event/events-for-session db parent-id)
          cause-id (:event/id (last parent-events-before))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-result
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :child/session-id session-id
                            :child/work-id work-id
                            :terminal/event-id (:event/id (last (event/events-for-session db session-id)))
                            :result/cas-ref cas-ref}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)
          parent-events-after (event/events-for-session db parent-id)]
      (is (= :ok (:result/status res)) (str "got " (pr-str res)))
      (is (= session-id (get-in res [:value :child/session-id])))
      (is (= work-id (get-in res [:value :child/work-id])) "work handle echoed")
      (is (= (count parent-events-before) (count parent-events-after))
          "re-delivery is a no-op (run-time auto-delivery already recorded the terminal)")
      (is (= :subagent/result (:event/type (last parent-events-after))))))
  (testing "subagent-result for another parent's child is scope-denied"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          other (create-parent-session! db)
          other-id (:session/id other)
          {:keys [child/session-id]} (subagent/spawn-subagent! db other-id {:task "not yours"} [])
          other-child-id session-id
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-result
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:parent/session-id parent-id
                            :child/session-id other-child-id
                            :result/cas-ref cas-ref-good}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :capability/scope-denied (:error/type res)) (str "got " (pr-str res)))))
  (testing "subagent-cancel cancels the subtree"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "doomed"} [])
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db parent-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-cancel
                  :session/id parent-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:target/session-id session-id
                            :reason :user-request}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :ok (:result/status res)) (str "got " (pr-str res)))
      (is (false? (get-in res [:value :already-cancelled?])))
      (is (= [session-id] (get-in res [:value :cancelled])))))
  (testing "subagent-cancel by a non-ancestor is scope-denied"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          outsider (create-parent-session! db)
          outsider-id (:session/id outsider)
          {:keys [child/session-id]} (subagent/spawn-subagent! db parent-id {:task "safe"} [])
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cause-id (:event/id (last (event/events-for-session db outsider-id)))
          intent {:intent/id (random-uuid)
                  :intent/type :intent/subagent-cancel
                  :session/id outsider-id
                  :phenotype/id phenotype
                  :node/id :node/tool
                  :cause/event-id cause-id
                  :payload {:target/session-id session-id
                            :reason :user-request}
                  :budget {:wall-ms 1000}
                  :metadata {}}
          res (dispatch/dispatch! ctx intent)]
      (is (= :capability/scope-denied (:error/type res)) (str "got " (pr-str res))))))

;; ===========================================================================
;; 7 — agent/status is descendant-scoped
;; ===========================================================================

(deftest status-is-descendant-scoped
  (testing "parent sees child and grandchild; sibling and self deny"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          c1 (:child/session-id (subagent/spawn-subagent! db parent-id {:task "c1"} []))
          c2 (:child/session-id (subagent/spawn-subagent! db parent-id {:task "c2"} []))
          gc (:child/session-id (subagent/spawn-subagent! db c1 {:task "gc"} []))
          reg (registry/create-registry)
          _ (registry/register! reg (subagent/agent-status-provider db))
          status-lease (agent-lease parent-id :agent/status #{:invoke})
          c1-status-lease (agent-lease c1 :agent/status #{:invoke})
          ctx (dispatch/make-broker-context {:registry reg :leases [status-lease c1-status-lease] :db db})
          query (fn [requester target]
                  (let [cause-id (:event/id (last (event/events-for-session db requester)))]
                    (dispatch/dispatch!
                     ctx
                     {:intent/id (random-uuid)
                      :intent/type :intent/tool-call
                      :session/id requester
                      :phenotype/id phenotype
                      :node/id :node/tool
                      :cause/event-id cause-id
                      :payload {:tool/id :agent/status
                                :args {:session-id (str target)}}
                      :budget {:wall-ms 1000}
                      :metadata {}})))]
      (let [r1 (query parent-id c1)]
        (is (= :ok (:result/status r1)) (str "parent sees child: " (pr-str r1)))
        (is (= c1 (get-in r1 [:value :session/id]))))
      (let [rg (query parent-id gc)]
        (is (= :ok (:result/status rg)) (str "parent sees grandchild: " (pr-str rg))))
      (let [rsib (query c1 c2)]
        (is (= :error (:result/status rsib)) "sibling query fails")
        (is (= :provider/execution-failed (:error/type rsib)) (str "got " (pr-str rsib)))
        (is (= :capability/scope-denied (get-in rsib [:error/data :cause :error/type]))
            "cause preserves scope-denied"))
      (let [rself (query parent-id parent-id)]
        (is (= :provider/execution-failed (:error/type rself)) "self query fails")
        (is (= :capability/scope-denied (get-in rself [:error/data :cause :error/type])))))))

(deftest cancel-intent-accepts-work-id
  (testing "subagent-cancel with :target/work-id cancels; a foreign work-id is :store/work-invalid"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          spawned (subagent/spawn-subagent! db parent-id {:task "doomed"} [])
          child-id (:child/session-id spawned)
          child-work (:child/work-id spawned)
          reg (registry/create-registry)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :db db})
          cancel (fn [payload]
                   (let [cause-id (:event/id (last (event/events-for-session db parent-id)))]
                     (dispatch/dispatch! ctx {:intent/id (random-uuid)
                                              :intent/type :intent/subagent-cancel
                                              :session/id parent-id
                                              :phenotype/id phenotype
                                              :node/id :node/tool
                                              :cause/event-id cause-id
                                              :payload payload
                                              :budget {:wall-ms 1000}
                                              :metadata {}})))]
      (let [res (cancel {:target/session-id child-id :target/work-id child-work :reason :user-request})]
        (is (= :ok (:result/status res)) (str "got " (pr-str res)))
        (is (= [child-id] (get-in res [:value :cancelled])) "cancelled the child")
        (is (= :cancelled (:work/state (work-store/fetch-work db child-work))) "child work cancelled"))
      ;; a work-id owned by another session is rejected, not acted on
      (let [other (create-parent-session! db)
            other-work (let [w (UUID/randomUUID)]
                         (work-store/create-work! db {:work/id w :work/type :session/run
                                                      :work/state :queued :work/session-id (:session/id other)})
                         w)
            res (cancel {:target/session-id child-id :target/work-id other-work :reason :user-request})]
        (is (= :store/work-invalid (:error/type res)) (str "got " (pr-str res)))))))

(deftest agent-cancel-provider-is-descendant-scoped-and-work-addressable
  (testing ":agent/cancel cancels a child by session or work id; outsiders are scope-denied"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          spawned (subagent/spawn-subagent! db parent-id {:task "c1"} [])
          c1 (:child/session-id spawned)
          c1-work (:child/work-id spawned)
          outsider (create-parent-session! db)
          outsider-id (:session/id outsider)
          reg (registry/create-registry)
          _ (registry/register! reg (subagent/agent-cancel-provider db))
          cancel-lease (agent-lease parent-id :agent/cancel #{:invoke})
          outsider-lease (agent-lease outsider-id :agent/cancel #{:invoke})
          ctx (dispatch/make-broker-context {:registry reg :leases [cancel-lease outsider-lease] :db db})
          call (fn [requester args]
                 (let [cause-id (:event/id (last (event/events-for-session db requester)))]
                   (dispatch/dispatch! ctx {:intent/id (random-uuid)
                                            :intent/type :intent/tool-call
                                            :session/id requester
                                            :phenotype/id phenotype
                                            :node/id :node/tool
                                            :cause/event-id cause-id
                                            :payload {:tool/id :agent/cancel :args args}
                                            :budget {:wall-ms 1000}
                                            :metadata {}})))]
      (let [r (call outsider-id {:session-id (str c1)})]
        (is (= :provider/execution-failed (:error/type r)) (str "got " (pr-str r)))
        (is (= :capability/scope-denied (get-in r [:error/data :cause :error/type])) "outsider denied"))
      (let [r (call parent-id {:work-id (str c1-work)})]
        (is (= :ok (:result/status r)) (str "got " (pr-str r)))
        (is (= [c1] (get-in r [:value :cancelled])) "cancelled by work id")
        (is (= c1-work (get-in r [:value :work/id])) "work id echoed")))))

