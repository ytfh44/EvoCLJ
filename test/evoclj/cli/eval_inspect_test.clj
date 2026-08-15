(ns evoclj.cli.eval-inspect-test
  "Feature V3 tests: the eval-inspect command reads one evaluation's
  full persisted record."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [evoclj.cli.eval-inspect :as ei]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-ei-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec p)]
    (migrate/migrate! db)
    db))

(defn- seed-eval! [db]
  (let [eval-id "00000000-0000-0000-0000-0000000000ee"
        cand-id "00000000-0000-0000-0000-0000000000cf"]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "gen-1"
                     :genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                     :resolution_id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                     :parent_id nil :state "active" :current 1
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :mutations
                    {:id "00000000-0000-0000-0000-0000000000ab"
                     :parent_genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                     :hypothesis_id "00000000-0000-0000-0000-0000000000ac"
                     :evidence_id "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
                     :risk "behavioral"
                     :ops (pr-str [{:op :set-edn :file "skills/a.edn" :path ["x"] :value 1}])
                     :expected_effect (pr-str {:primary-metric :task/success :direction :increase})
                     :created_at "2025-01-02T00:00:00Z"})
      (jdbc/insert! conn :candidates
                    {:id cand-id
                     :parent_generation_id "gen-1"
                     :parent_genome_id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                     :genome_id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                     :mutation_id "00000000-0000-0000-0000-0000000000ab"
                     :evidence_id "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
                     :risk "behavioral" :state "eligible"
                     :created_at "2025-01-02T00:00:00Z"})
      (jdbc/insert! conn :eval_runs
                    {:id eval-id :candidate_id cand-id
                     :parent_generation_id "gen-1"
                     :profile_id "default-v1"
                     :gates (pr-str [:G1-schema :G5-paired-selection])
                     :paired_results_ref nil
                     :summary (pr-str {:hard {:passed true}})
                     :eligibility (pr-str {:eligible? true :reasons []})
                     :status "finalized"
                     :created_at "2025-01-02T00:00:00Z"})
      (jdbc/insert! conn :eval_cases
                    {:id "00000000-0000-0000-0000-0000000000c1"
                     :eval_run_id eval-id
                     :case_ref "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
                     :created_at "2025-01-02T00:00:00Z"})
      (jdbc/insert! conn :eval_results
                    {:id "00000000-0000-0000-0000-0000000000d1"
                     :eval_run_id eval-id
                     :case_id "00000000-0000-0000-0000-0000000000c1"
                     :gate "G5-paired-selection" :passed 1
                     :metric (pr-str {:task/success {:parent 0.5 :candidate 0.5}})
                     :detail (pr-str {:delta 0.0})
                     :created_at "2025-01-02T00:00:00Z"}))
    eval-id))

(deftest eval-inspect-returns-full-record
  (testing "the record carries the run row and per-case results"
    (let [db (temp-db)
          eval-id (seed-eval! db)
          out (ei/eval-inspect!
               {:positionals [eval-id]
                :overrides {:store/sqlite db
                            :store/cas {:root (str (Files/createTempDirectory
                                                     "evoclj-ei-cas-"
                                                     (make-array FileAttribute 0)))
                                        :verify false}}})]
      (is (true? (:found out)))
      (is (= (UUID/fromString "00000000-0000-0000-0000-0000000000ee")
             (:evaluation/id out)))
      (is (= (UUID/fromString "00000000-0000-0000-0000-0000000000cf")
             (:candidate/id out)))
      (is (= :finalized (:status out)))
      (is (= true (get-in out [:eligibility :eligible?])))
      (is (= 1 (count (:case-results out))))
      (let [cr (first (:case-results out))]
        (is (= :G5-paired-selection (:gate cr)))
        (is (true? (:passed cr)))
        (is (= {:delta 0.0} (:detail cr)))))))

(deftest eval-inspect-unknown-id
  (testing "an unknown evaluation id reports :found false"
    (let [db (temp-db)
          out (ei/eval-inspect!
               {:positionals [(str (random-uuid))]
                :overrides {:store/sqlite db
                            :store/cas {:root (str (Files/createTempDirectory
                                                     "evoclj-ei-cas-"
                                                     (make-array FileAttribute 0)))
                                        :verify false}}})]
      (is (false? (:found out))))))
