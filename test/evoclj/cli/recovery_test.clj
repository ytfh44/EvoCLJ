(ns evoclj.cli.recovery-test
  "Feature O3: the `evoclj recovery` command surfaces the store's
  recovery scan report (orphaned sessions, missing artifacts,
  invalid chains, stale candidates)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [evoclj.cli.recovery :as rec]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-rec-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec p)]
    (migrate/migrate! db)
    db))

(defn- report-for [db]
  (rec/recovery-scan!
   {:overrides {:store/sqlite db
                :store/cas {:root (str (Files/createTempDirectory
                                         "evoclj-rec-cas-"
                                         (make-array FileAttribute 0)))
                            :verify false}}}))

(defn- seed-orphan! [db]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :generations
                  {:id "g1"
                   :genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                   :resolution_id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                   :parent_id nil :state "active" :current 1
                   :created_at "2025-01-01T00:00:00Z"})
    (jdbc/insert! conn :sessions
                  {:id "00000000-0000-0000-0000-0000000000a1" :generation_id "g1"
                   :genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                   :resolution_id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                   :phenotype_id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                   :state "running"
                   :created_at "2025-01-01T00:00:00Z"})))

(deftest recovery-scan-empty-store
  (testing "an empty migrated store scans clean"
    (let [report (report-for (temp-db))]
      (is (empty? (:orphaned-sessions report)))
      (is (empty? (:missing-artifacts report)))
      (is (empty? (:invalid-event-chains report)))
      (is (empty? (:stale-candidates report))))))

(deftest recovery-scan-reports-orphaned-session
  (testing "a session left in :running without a terminal event is
            reported as orphaned"
    (let [db (temp-db)
          _ (seed-orphan! db)
          report (report-for db)]
      (is (= 1 (count (:orphaned-sessions report))))
      (is (= (java.util.UUID/fromString "00000000-0000-0000-0000-0000000000a1")
             (get-in report [:orphaned-sessions 0 :session/id])))
      (is (= :running (get-in report [:orphaned-sessions 0 :state]))))))
