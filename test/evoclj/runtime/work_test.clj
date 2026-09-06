(ns evoclj.runtime.work-test
  "W1 — Work unified lifecycle: 7 states collapse 48, SM verification."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.runtime.work :as work]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.work :as work-store]
            [clojure.java.jdbc :as jdbc])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(deftest work-collapses-48-to-7
  (is (= 48 work/session-x-command-product) "Session×Command product is 48")
  (is (= 7 (work/work-states-count)) "Work has 7 states")
  (is (= "Session×Command 48 states collapses to Work 7" (work/collapse-ratio))))

(deftest work-sm-edges-legal
  (is (work/edges-legal?) "every transition endpoint inside 7-state vocabulary"))

(deftest work-sm-acyclic
  (is (work/acyclic?) "directed graph is acyclic"))

(deftest work-sm-terminals
  (is (work/terminals-sink?) "terminals have no outgoing edges"))

(deftest work-sm-paths
  (is (work/queued->succeeded-path?) "queued -> ... -> succeeded reachable")
  (is (work/queued->timed-out-path?) "queued -> running -> timed-out reachable"))

(deftest work-sm-verify-all
  (let [{:keys [pass? checks]} (work/verify-work-sm)]
    (is pass? (str "all checks pass: " checks))
    (is (:edgesLegal checks))
    (is (:acyclic checks))
    (is (:terminalsSink checks))
    (is (:queuedToSucceededPath checks))
    (is (:queuedToTimedOutPath checks))))

(deftest work-store-lifecycle
  (let [tmp (str (Files/createTempFile "work-test-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec tmp)]
    (migrate/migrate! db)
    ;; seed via artifact helper like scheduler
    (let [gen-id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          res-id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          phen-id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"]
      (evoclj.store.artifact/ensure-artifact! db gen-id "application/octet-stream" 0)
      (evoclj.store.artifact/ensure-artifact! db res-id "application/edn" 0)
      (evoclj.store.artifact/ensure-artifact! db phen-id "application/edn" 0)
      (evoclj.store.artifact/ensure-genome! db gen-id))
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations {:id "gen-1" :genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :resolution_id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :sessions {:id (str #uuid "00000000-0000-4000-a000-000000000000") :generation_id "gen-1" :genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :resolution_id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :phenotype_id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" :state "created" :created_at "2025-01-01T00:00:00Z"}))
    (let [sid #uuid "00000000-0000-4000-a000-000000000000"
          wid (java.util.UUID/randomUUID)]
      (work-store/create-work! db {:work/id wid :work/type :test :work/state :queued :work/session-id sid})
      (is (= :queued (:work/state (work-store/fetch-work db wid))))
      (work-store/dispatch-work! db wid)
      (is (= :running (:work/state (work-store/fetch-work db wid))))
      (work-store/wait-work! db wid)
      (is (= :waiting (:work/state (work-store/fetch-work db wid))))
      (work-store/succeed-work! db wid nil)
      (is (= :succeeded (:work/state (work-store/fetch-work db wid))))
      (is (work/terminal? :succeeded))
      (is (not (work/valid-transition? :succeeded :running))))
    (try (clojure.java.io/delete-file tmp) (catch Exception _ nil))))

(defn- sweep-db!
  "A migrated temp db with one seeded session; returns [db sid]."
  []
  (let [tmp (str (Files/createTempFile "work-sweep-test-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec tmp)]
    (migrate/migrate! db)
    (let [gen-id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          res-id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          phen-id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"]
      (evoclj.store.artifact/ensure-artifact! db gen-id "application/octet-stream" 0)
      (evoclj.store.artifact/ensure-artifact! db res-id "application/edn" 0)
      (evoclj.store.artifact/ensure-artifact! db phen-id "application/edn" 0)
      (evoclj.store.artifact/ensure-genome! db gen-id))
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations {:id "gen-1" :genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :resolution_id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :sessions {:id (str #uuid "00000000-0000-4000-a000-000000000001") :generation_id "gen-1" :genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :resolution_id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :phenotype_id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc" :state "created" :created_at "2025-01-01T00:00:00Z"}))
    [db #uuid "00000000-0000-4000-a000-000000000001" tmp]))

(deftest sweep-expired-deadlines-drives-timeout-terminals
  (let [[db sid tmp] (sweep-db!)
        past (java.util.Date. 1000000000000)
        future (java.util.Date. (+ (System/currentTimeMillis) 3600000))
        expired-queued (java.util.UUID/randomUUID)
        expired-running (java.util.UUID/randomUUID)
        expired-waiting (java.util.UUID/randomUUID)
        live-queued (java.util.UUID/randomUUID)
        no-deadline (java.util.UUID/randomUUID)
        already-terminal (java.util.UUID/randomUUID)]
    (try
      (work-store/create-work! db {:work/id expired-queued :work/type :test :work/state :queued :work/session-id sid :work/deadline past})
      (work-store/create-work! db {:work/id expired-running :work/type :test :work/state :queued :work/session-id sid :work/deadline past})
      (work-store/dispatch-work! db expired-running)
      (work-store/create-work! db {:work/id expired-waiting :work/type :test :work/state :queued :work/session-id sid :work/deadline past})
      (work-store/dispatch-work! db expired-waiting)
      (work-store/wait-work! db expired-waiting)
      (work-store/create-work! db {:work/id live-queued :work/type :test :work/state :queued :work/session-id sid :work/deadline future})
      (work-store/create-work! db {:work/id no-deadline :work/type :test :work/state :queued :work/session-id sid})
      (work-store/create-work! db {:work/id already-terminal :work/type :test :work/state :queued :work/session-id sid :work/deadline past})
      (work-store/dispatch-work! db already-terminal)
      (work-store/succeed-work! db already-terminal nil)
      (let [report (work-store/sweep-expired-deadlines! db)]
        (is (= 3 (count (:swept report))) "only the three expired non-terminal works are swept")
        (is (= #{expired-running expired-waiting} (set (map :work/id (:timed-out report)))) "running/waiting -> timed-out")
        (is (= [expired-queued] (mapv :work/id (:failed report))) "queued -> failed")
        (is (every? #(= work-store/sweep-deadline-error (:recovery/error %)) (:swept report)) "report carries the typed marker")
        (is (= :failed (:work/state (work-store/fetch-work db expired-queued))))
        (is (= :timed-out (:work/state (work-store/fetch-work db expired-running))))
        (is (= :timed-out (:work/state (work-store/fetch-work db expired-waiting))))
        (is (= :queued (:work/state (work-store/fetch-work db live-queued))) "future deadline untouched")
        (is (= :queued (:work/state (work-store/fetch-work db no-deadline))) "no deadline untouched")
        (is (= :succeeded (:work/state (work-store/fetch-work db already-terminal))) "terminal untouched"))
      (let [report2 (work-store/sweep-expired-deadlines! db)]
        (is (empty? (:swept report2)) "re-run is a no-op — swept rows are terminal now"))
      (finally (try (clojure.java.io/delete-file tmp) (catch Exception _ nil))))))
