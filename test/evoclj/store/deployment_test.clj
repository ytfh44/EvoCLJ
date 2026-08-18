(ns evoclj.store.deployment-test
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.deployment :as deployment])
  (:import (java.nio.file Files FileVisitOption LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

(def ^:private temp-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile
                "evoclj-deployment-" ".db"
                (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (Files/deleteIfExists (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- temp-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(deftest record-decision-persists-row
  (let [db (temp-db)]
    (is (map? (deployment/record-decision! {:sqlite db} "generation-1" :deployed nil)))
    (let [rows (jdbc/query db ["SELECT * FROM deployment_decisions"])]
      (is (= 1 (count rows)))
      (is (= "generation-1" (:generation_id (first rows))))
      (is (= "deployed" (:decision (first rows)))))))

(deftest record-decision-stores-reason
  (let [db (temp-db)]
    (deployment/record-decision! {:sqlite db} "generation-1" :rolled-back :canary-regression)
    (let [row (first (jdbc/query db ["SELECT * FROM deployment_decisions"]))]
      (is (= "rolled-back" (:decision row)))
      (is (= "canary-regression" (:reason row))))))
