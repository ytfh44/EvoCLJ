(ns evoclj.cli.deploy-test
  "component tests: `evoclj deploy <generation-id>` — set a generation as
  the deployment target.

  The command reads an existing generation row and returns its genome id
  as the deployment target. It never moves the CURRENT pointer and never
  writes to the generations table."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.deploy :as deploy]
            [evoclj.cli.main :as main]
            [evoclj.cli.session :as session]
            [evoclj.genome.hash :as genome-hash]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
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

(defn- canonical-genome-index
  []
  (let [source "(fn [input] input)\n"
        entry-digest (genome-hash/text-digest source)
        index (str "programs/fixture.clj\u0000" entry-digest "\n")
        entries [{:path "programs/fixture.clj" :digest entry-digest}]
        expected-id (genome-hash/tree-digest entries)]
    {:bytes (.getBytes index StandardCharsets/UTF_8)
     :expected-id expected-id}))

(defn- provision-deploy-store!
  "A minimal temp state dir: migrated db + one generation row (current by default)."
  ([]
   (provision-deploy-store! 1))
  ([current-flag]
   (let [dir (temp-dir "evoclj-deploy-state-")
         db-dir (str dir "/db")
         cas-root (str dir "/cas")
         _ (Files/createDirectories (Paths/get db-dir (make-array String 0))
                                    (make-array FileAttribute 0))
         db (sqlite/spec (str dir "/db/evoclj.db"))
         cas-store (cas/->cas cas-root)
         {:keys [bytes expected-id]} (canonical-genome-index)
         genome-artifact (cas/put-bytes! cas-store bytes {})
         genome-id (:artifact/id genome-artifact)
         resolution-artifact (cas/put-bytes! cas-store
                                              (.getBytes "{}" StandardCharsets/UTF_8)
                                              {:media-type "application/edn"})
         resolution-id (:artifact/id resolution-artifact)]
     (when-not (= expected-id genome-id)
       (throw (ex-info "fixture Genome index did not hash canonically"
                       {:expected expected-id :actual genome-id})))
     (migrate/migrate! db)
     (artifact/ensure-artifact! db genome-id "application/octet-stream"
                                 (:size genome-artifact))
     (artifact/ensure-genome! db genome-id)
     (artifact/ensure-artifact! db resolution-id "application/edn"
                                 (:size resolution-artifact))
     (jdbc/execute! db ["INSERT INTO generations (id, genome_id, resolution_id, state, current, created_at) VALUES (?, ?, ?, ?, ?, ?)"
                        "generation-1"
                        genome-id
                        resolution-id
                        "active"
                        current-flag
                        "2025-01-01T00:00:00Z"])
     {:state-dir dir :genome-id genome-id})))

;; --- tests -------------------------------------------------------------------
(deftest deploy-returns-target-for-existing-generation
  (let [{:keys [state-dir genome-id]} (provision-deploy-store!)
        {:keys [exit data]} (main/execute ["deploy" "generation-1"] {:state-dir state-dir})]
    (is (= 0 exit))
    (is (= "generation-1" (:generation/id data)))
    (is (= genome-id (:genome/id data)))
    (is (= :deployed (:status data)))))
(deftest deploy-fails-for-unknown-generation
  (let [{:keys [state-dir]} (provision-deploy-store!)
        {:keys [exit data]} (main/execute ["deploy" "generation-99"] {:state-dir state-dir})]
    (is (= 1 exit))
    (is (= :cli/generation-not-found (:error/type data)))))
(deftest deploy-requires-positional-argument
  (let [{:keys [state-dir]} (provision-deploy-store!)
        {:keys [exit data]} (main/execute ["deploy"] {:state-dir state-dir})]
    (is (= 1 exit))
    (is (= :cli/usage-invalid (:error/type data)))))
(deftest deploy-current-polls-read-only
  (let [{:keys [state-dir genome-id]} (provision-deploy-store!)
        {:keys [exit data]} (main/execute ["deploy" "current"] {:state-dir state-dir})]
    (is (= 0 exit))
    (is (= "generation-1" (:generation/id data)))
    (is (= genome-id (:genome/id data)))
    (is (nil? (:canary data)) "no persisted rollout state — nil canary")
    (is (string? (:timestamp data)))
    (is (not (contains? data :status)) "no deploy decision recorded")))
(deftest deploy-current-fails-without-current
  (let [{:keys [state-dir]} (provision-deploy-store! 0)
        {:keys [exit data]} (main/execute ["deploy" "current"] {:state-dir state-dir})]
    (is (= 1 exit))
    ;; Strict startup integrity rejects a store that violates CURRENT invariant 6.
    (is (= :store/integrity-failure (:error/type data)))))
