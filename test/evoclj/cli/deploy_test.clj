(ns evoclj.cli.deploy-test
  "Task D1 tests: `evoclj deploy <generation-id>` — set a generation as
  the deployment target.

  The command reads an existing generation row and returns its genome id
  as the deployment target. It never moves the CURRENT pointer and never
  writes to the generations table."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.deploy :as deploy]
            [evoclj.cli.main :as main]
            [evoclj.cli.session :as session]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- temp-dir plumbing (mirrors cli_test / evolution_test) -------------------

(def ^:private temp-paths (atom []))

(defn- temp-dir
  [prefix]
  (let [d (str (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! temp-paths conj d)
    d))

(defn- delete-tree!
  [path]
  (let [p (Paths/get path (make-array String 0))]
    (when (Files/exists p (make-array LinkOption 0))
      (with-open [stream (Files/walk p (make-array FileVisitOption 0))]
        (doseq [q (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists q))))))

(defn- cleanup! []
  (doseq [p @temp-paths]
    (delete-tree! p))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- provision-deploy-store!
  "A minimal temp state dir: migrated db + one active generation row."
  []
  (let [dir (temp-dir "evoclj-deploy-state-")
        db-dir (str dir "/db")
        _ (Files/createDirectories (Paths/get db-dir (make-array String 0))
                                   (make-array FileAttribute 0))
        db (sqlite/spec (str dir "/db/evoclj.db"))]
    (migrate/migrate! db)
    (jdbc/execute! db ["INSERT INTO generations (id, genome_id, resolution_id, state, current, created_at) VALUES (?, ?, ?, ?, ?, ?)"
                       "generation-1"
                       "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                       "active"
                       1
                       "2025-01-01T00:00:00Z"])
    dir))

;; --- tests -------------------------------------------------------------------

(deftest deploy-returns-target-for-existing-generation
  (let [dir (provision-deploy-store!)
        {:keys [exit data]} (main/execute ["deploy" "generation-1"] {:state-dir dir})]
    (is (= 0 exit))
    (prn :data data)
    (is (= "generation-1" (:generation/id data)))
    (is (= "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" (:genome/id data)))
    (is (= :deployed (:status data)))))

(deftest deploy-fails-for-unknown-generation
  (let [dir (provision-deploy-store!)
        {:keys [exit data]} (main/execute ["deploy" "generation-99"] {:state-dir dir})]
    (is (= 1 exit))
    (is (= :cli/generation-not-found (:error/type data)))))

(deftest deploy-requires-positional-argument
  (let [dir (provision-deploy-store!)
        {:keys [exit data]} (main/execute ["deploy"] {:state-dir dir})]
    (is (= 1 exit))
    (is (= :cli/usage-invalid (:error/type data)))))
