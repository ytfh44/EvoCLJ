(ns evoclj.cli.eval-inspect-test
  "Feature V3 tests: the eval-inspect command reads one evaluation's
  full persisted record."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [evoclj.cli.eval-inspect :as ei]
            [evoclj.genome.hash :as genome-hash]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-ei-" ".db" (make-array FileAttribute 0)))
        cas-root (str (Files/createTempDirectory "evoclj-ei-cas-"
                                                 (make-array FileAttribute 0)))
        db (sqlite/spec p)]
    (migrate/migrate! db)
    {:db db :cas-store (cas/->cas cas-root)}))

(defn- store-genome!
  [cas-store path source]
  (let [entry-digest (genome-hash/text-digest source)
        index (str path "\u0000" entry-digest "\n")
        expected-id (genome-hash/tree-digest [{:path path :digest entry-digest}])
        stored (cas/put-bytes! cas-store
                                (.getBytes index StandardCharsets/UTF_8)
                                {})]
    (when-not (= expected-id (:artifact/id stored))
      (throw (ex-info "fixture Genome index did not hash canonically"
                      {:expected expected-id
                       :actual (:artifact/id stored)})))
    stored))

(defn- seed-eval! [db cas-store]
  (let [eval-id "00000000-0000-0000-0000-0000000000ee"
        cand-id "00000000-0000-0000-0000-0000000000cf"
        parent-artifact (store-genome! cas-store "programs/parent.clj"
                                       "(fn [input] input)\n")
        candidate-artifact (store-genome! cas-store "programs/candidate.clj"
                                          "(fn [input] (assoc input :candidate true))\n")
        parent-genome-id (:artifact/id parent-artifact)
        candidate-genome-id (:artifact/id candidate-artifact)
        resolution-artifact (cas/put-bytes! cas-store
                                             (.getBytes "{}" StandardCharsets/UTF_8)
                                             {:media-type "application/edn"})
        evidence-artifact (cas/put-bytes! cas-store
                                          (.getBytes "{:evidence true}" StandardCharsets/UTF_8)
                                          {:media-type "application/edn"})
        case-artifact (cas/put-bytes! cas-store
                                       (.getBytes "{:case true}" StandardCharsets/UTF_8)
                                       {:media-type "application/edn"})
        resolution-id (:artifact/id resolution-artifact)
        evidence-id (:artifact/id evidence-artifact)
        case-ref (:artifact/id case-artifact)]
    ;; Register every content address before inserting FK-dependent rows.
    (doseq [[artifact-id media-type size]
            [[parent-genome-id "application/octet-stream" (:size parent-artifact)]
             [candidate-genome-id "application/octet-stream" (:size candidate-artifact)]
             [resolution-id "application/edn" (:size resolution-artifact)]
             [evidence-id "application/edn" (:size evidence-artifact)]
             [case-ref "application/edn" (:size case-artifact)]]]
      (artifact/ensure-artifact! db artifact-id media-type size))
    (doseq [genome-id [parent-genome-id candidate-genome-id]]
      (artifact/ensure-genome! db genome-id))
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "gen-1"
                     :genome_id parent-genome-id
                     :resolution_id resolution-id
                     :parent_id nil :state "active" :current 1
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :mutations
                    {:id "00000000-0000-0000-0000-0000000000ab"
                     :parent_genome_id parent-genome-id
                     :hypothesis_id "00000000-0000-0000-0000-0000000000ac"
                     :evidence_id evidence-id
                     :risk "behavioral"
                     :ops (pr-str [{:op :set-edn :file "skills/a.edn" :path ["x"] :value 1}])
                     :expected_effect (pr-str {:primary-metric :task/success :direction :increase})
                     :created_at "2025-01-02T00:00:00Z"})
      (jdbc/insert! conn :candidates
                    {:id cand-id
                     :parent_generation_id "gen-1"
                     :parent_genome_id parent-genome-id
                     :genome_id candidate-genome-id
                     :mutation_id "00000000-0000-0000-0000-0000000000ab"
                     :evidence_id evidence-id
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
                     :case_ref case-ref
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
    (let [{:keys [db cas-store]} (temp-db)
          eval-id (seed-eval! db cas-store)
          out (ei/eval-inspect!
               {:positionals [eval-id]
                :overrides {:store/sqlite db
                            :store/cas cas-store}})]
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
    (let [{:keys [db cas-store]} (temp-db)
          out (ei/eval-inspect!
               {:positionals [(str (random-uuid))]
                :overrides {:store/sqlite db
                            :store/cas cas-store}})]
      (is (false? (:found out))))))
