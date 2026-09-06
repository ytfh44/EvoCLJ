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
            [evoclj.store.work :as work-store]
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

;; ---------------------------------------------------------------------------
;; 1 — deliver-result! appends :subagent/result to parent with correct ids
;; ---------------------------------------------------------------------------

(deftest deliver-result-appends-subagent-result-to-parent
  (testing "run auto-delivers the child terminal; manual deliver is an idempotent no-op returning the recorded event"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id child/work-id]} (subagent/spawn-subagent! db parent-id {:task "child-task"} [pl])
          run-res (subagent/run-subagent! db parent-id session-id {:text "hello-echo"})
          ;; W1/W2: Work owns the lifecycle — the session row stays :created
          ;; (immutable identity); completion truth is the child Work :succeeded.
          child-work-state (some-> (work-store/fetch-work db work-id) :work/state)
          _ (is (= :succeeded child-work-state) "child Work should be :succeeded after run")
          cas-ref (:work/payload-ref (work-store/fetch-work db work-id))
          _ (is (string? cas-ref) "child Work carries the scheduler's output CAS ref as payload_ref")
          _ (is (true? (get-in run-res [:delivery :delivered])) "auto-delivery closed the loop on run")
          auto-id (get-in run-res [:delivery :event/id])
          _ (is (some? auto-id) "auto-delivery recorded an event id")
          result-event (subagent/deliver-result! db parent-id session-id cas-ref)
          parent-events (event/events-for-session db parent-id)
          result-events (filter #(= :subagent/result (:event/type %)) parent-events)
          latest (last result-events)]
      (is (= 1 (count result-events)) "exactly one :subagent/result (manual re-deliver is a no-op)")
      (is (= auto-id (:event/id result-event)) "manual deliver returns the recorded event")
      (is (some? result-event) "deliver-result! returned event")
      (is (= :subagent/result (:event/type latest)) "parent has :subagent/result")
      (is (= session-id (:child/session-id (:metadata latest))) "metadata has correct child id")
      (is (= work-id (:child/work-id (:metadata latest))) "metadata carries the child work id")
      (is (= (:event/id (last (event/events-for-session db session-id)))
             (:terminal/event-id (:metadata latest)))
          "metadata names the child terminal event")
      (is (= cas-ref (:result/cas-ref (:metadata latest))) "metadata has correct cas-ref")
      (is (= :succeeded (:result/status (:metadata latest))) "metadata status is :succeeded")
      (is (= #{{:from (:terminal/event-id (:metadata latest)) :type :subagent/result}}
             (:causal-links latest))
          "causal link points at the child terminal event")
      (is (:valid? (event/verify-event-chain db parent-id)) "parent chain hash valid")
      (is (:valid? (event/verify-event-chain db session-id)) "child chain hash valid"))))

(deftest deliver-result-cas-mismatch-fails
  (testing "deliver-result! with a cas-ref that differs from the child Work payload_ref throws :store/cas-mismatch and appends nothing"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id child/work-id]} (subagent/spawn-subagent! db parent-id {:task "mismatch"} [pl])
          ;; undo the auto-delivery's recorded event count baseline: run first,
          ;; then attempt a conflicting manual delivery
          _ (subagent/run-subagent! db parent-id session-id {:text "hello"})
          bound (:work/payload-ref (work-store/fetch-work db work-id))
          bad-ref (str "sha256:" (apply str (repeat 64 "0")))
          _ (is (not= bound bad-ref) "test ref really differs")
          before (count (filter #(= :subagent/result (:event/type %))
                                (event/events-for-session db parent-id)))
          ex (try (subagent/deliver-result-for-works! db parent-id nil work-id
                                                      (:event/id (last (event/events-for-session db session-id)))
                                                      bad-ref)
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "should throw")
      (is (= :store/cas-mismatch (:error/type (ex-data ex))) "error type is :store/cas-mismatch")
      (is (= bound (:work/payload-ref (ex-data ex))) "error carries the canonical bound")
      (let [after (count (filter #(= :subagent/result (:event/type %))
                                 (event/events-for-session db parent-id)))]
        (is (= before after) "no result event appended on mismatch")))))

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
(deftest orphaned-child-found-by-recovery-helper
  (testing "orphaned child (parent Work terminal, child Work live) is found by Work-only find-orphaned-subagents"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          parent-work-id (random-uuid)
          _ (work-store/create-work! db {:work/id parent-work-id
                                         :work/type :session/run
                                         :work/state :queued
                                         :work/session-id parent-id
                                         :work/created-at (Date. 1700000000000)})
          {:keys [child/session-id child/work-id]} (subagent/spawn-subagent! db parent-id {:task "orphan-task"} [pl]
                                                                               {:parent/work-id parent-work-id})
          child-id session-id
          _ (is (= parent-work-id (:work/parent-work-id (work-store/fetch-work db work-id)))
                "child work attributed to the parent work")
          ;; settle the parent terminal; the child stays :queued (live)
          _ (work-store/dispatch-work! db parent-work-id)
          _ (work-store/wait-work! db parent-work-id)
          _ (work-store/succeed-work! db parent-work-id nil)]
      (let [orphans (recovery/find-orphaned-subagents db)]
        (is (= 1 (count orphans)) "exactly one orphan")
        (let [o (first orphans)]
          (is (= parent-id (:parent/session-id o)) "orphan parent session correct")
          (is (= child-id (:child/session-id o)) "orphan child session correct")
          (is (= parent-work-id (:parent/work-id o)) "orphan parent work correct")
          (is (= work-id (:child/work-id o)) "orphan child work correct")
          (is (= :succeeded (:parent/state o)) "parent work state :succeeded")
          (is (= :queued (:child/state o)) "child work state :queued")))
      ;; recovery cancels the orphaned child DB-first (never replays it:
      ;; the parent is terminal, so redelivery would run an orphan)
      (let [report (recovery/recover-orphaned-subagents! db)]
        (is (= 1 (count (:orphaned-subagents report))) "one orphan classified")
        (is (= [{:child/work-id work-id}] (:recovered report)) "child work recovered")
        (is (= :cancelled (:work/state (work-store/fetch-work db work-id)))
            "orphaned child work is :cancelled after recovery")
        (is (empty? (recovery/find-orphaned-subagents db)) "re-running finds no orphans"))))
  (testing "no orphan when the parent Work is still live"
    (let [db2 (fresh-db)
          parent2 (create-parent-session! db2)
          pid2 (:session/id parent2)
          pl2 (parent-lease pid2 phenotype #{:invoke})
          pwid2 (random-uuid)
          _ (work-store/create-work! db2 {:work/id pwid2
                                          :work/type :session/run
                                          :work/state :queued
                                          :work/session-id pid2
                                          :work/created-at (Date. 1700000000000)})
          {:keys [child/session-id]} (subagent/spawn-subagent! db2 pid2 {:task "live"} [pl2]
                                                                {:parent/work-id pwid2})
          _ (work-store/dispatch-work! db2 pwid2)]
      (is (empty? (recovery/find-orphaned-subagents db2))
          "no orphan when parent work still :running"))))

(deftest deliver-failure-derives-error-from-work-record
  (testing "deliver-failure! records the canonical terminal error, not the caller-supplied value"
    (let [db (fresh-db)
          parent (create-parent-session! db)
          parent-id (:session/id parent)
          pl (parent-lease parent-id phenotype #{:invoke})
          {:keys [child/session-id child/work-id]} (subagent/spawn-subagent! db parent-id {:task "doomed"} [pl])
          child-id session-id
          _ (work-store/dispatch-work! db work-id)
          prev-id (:event/id (last (event/events-for-session db child-id)))
          terminal (event/append-event! db {:session/id child-id
                                            :generation/id gen
                                            :phenotype/id phenotype
                                            :event/type :session/failed
                                            :prev/event-id prev-id
                                            :payload-ref nil
                                            :metadata {:status :failed
                                                       :error/type :test/boom
                                                       :error/artifact-ref cas-ref-good}})
          _ (work-store/fail-work! db work-id {:error/type :test/boom})
          res (subagent/deliver-failure! db parent-id child-id {:error/type :caller/says})]
      (is (= :subagent/result (:event/type res)) "delivers a result event")
      (is (= :failed (:result/status (:metadata res))) "status failed")
      (is (= :test/boom (:error/type (:error (:metadata res))))
          "error derived from the canonical terminal event, not the caller")
      (is (= cas-ref-good (:error/artifact-ref (:error (:metadata res))))
          "derived artifact ref carried")
      (is (= work-id (:child/work-id (:metadata res))) "child work id carried")
      (is (= (:event/id terminal) (:terminal/event-id (:metadata res))) "terminal event named")
      (is (:valid? (event/verify-event-chain db parent-id)) "parent chain hash valid"))))
