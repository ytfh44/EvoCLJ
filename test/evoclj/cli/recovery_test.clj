(ns evoclj.cli.recovery-test
  "Feature O3: the `evoclj recovery` command surfaces the store recovery scan report (orphaned Works, missing artifacts, invalid chains, stale candidates)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [evoclj.cli.recovery :as rec]
            [evoclj.genome.hash :as genome-hash]
            [evoclj.store.cas :as cas]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-rec-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec p)]
    (migrate/migrate! db)
    db))

(defn- report-for [db cas-root]
  (rec/recovery-scan!
   {:overrides {:store/sqlite db
                :store/cas {:root cas-root
                            :verify false}}}))

(defn- canonical-genome-body
  "A minimal canonical Genome index body (path + NUL + file digest + LF)."
  []
  (let [manifest "{:genome/format 1}\n"
        digest (genome-hash/text-digest manifest)]
    (str "manifest.edn\u0000" digest "\n")))

(defn- seed-orphan! [db cas-store]
  (let [sid "00000000-0000-0000-0000-0000000000a1"
        work-id "00000000-0000-0000-0000-0000000000b1"
        genome-body (canonical-genome-body)
        genome-bytes (.getBytes genome-body java.nio.charset.StandardCharsets/UTF_8)
        genome (:artifact/id
                (cas/put-bytes! cas-store genome-bytes {}))
        resolution "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        phenotype "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"]
    (artifact/ensure-artifact! db genome "application/octet-stream"
                                (alength ^bytes genome-bytes))
    (artifact/ensure-artifact! db resolution "application/edn" 0)
    (artifact/ensure-artifact! db phenotype "application/edn" 0)
    (artifact/ensure-genome! db genome)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "g1"
                     :genome_id genome
                     :resolution_id resolution
                     :parent_id nil :state "active" :current 1
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :sessions
                    {:id sid :generation_id "g1"
                     :genome_id genome
                     :resolution_id resolution
                     :phenotype_id phenotype
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :works
                    {:id work-id :type "session/run" :state "running"
                     :session_id sid
                     :created_at "2025-01-01T00:00:00Z"
                     :updated_at "2025-01-01T00:00:00Z"}))))

(deftest recovery-scan-empty-store
  (testing "an empty migrated store scans clean"
    (let [cas-root (str (Files/createTempDirectory "evoclj-rec-cas-"
                                                  (make-array FileAttribute 0)))
          report (report-for (temp-db) cas-root)]
      (is (empty? (:orphaned-works report)))
      (is (empty? (:missing-artifacts report)))
      (is (empty? (:invalid-event-chains report)))
      (is (empty? (:stale-candidates report))))))

(deftest recovery-scan-reports-orphaned-work
  (testing "a Work left in :running without a terminal state is reported as orphaned"
    (let [db (temp-db)
          cas-root (str (Files/createTempDirectory "evoclj-rec-cas-"
                                                  (make-array FileAttribute 0)))
          cas-store (cas/->cas cas-root)
          _ (seed-orphan! db cas-store)
          report (report-for db cas-root)]
      (is (= 1 (count (:orphaned-works report))))
      (is (= (java.util.UUID/fromString "00000000-0000-0000-0000-0000000000a1")
             (get-in report [:orphaned-works 0 :work/session-id])))
      (is (= :running (get-in report [:orphaned-works 0 :work/state]))))))
