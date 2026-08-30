(ns evoclj.cli.cost-test
  "Feature O2: the `evoclj cost` report aggregates REAL model usage
  from the generation's causal event log (provider/call-completed
  payload artifacts)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [evoclj.cli.cost :as cost]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-cost-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec p)]
    (migrate/migrate! db)
    db))

(defn- temp-cas []
  (str (Files/createTempDirectory "evoclj-cost-cas-" (make-array FileAttribute 0))))

(defn- seed!
  "A generation with two sessions: one model call (with usage) and one
  tool call (no usage), plus a session in another generation that must
  NOT leak into the report."
  [db cas-store gen-id other-gen-id]
  (let [genome-id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        resolution-id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        phenotype-id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
        model-value {:model/output {:text "hi"}
                     :usage {:model-input-tokens 100 :model-output-tokens 50}
                     :model-cost-units 0.75}
        model-ref (:artifact/id (cas/put-bytes! cas-store
                                                 (.getBytes (pr-str model-value)
                                                            java.nio.charset.StandardCharsets/UTF_8)
                                                 {}))
        tool-value {:text "echoed"}
        tool-ref (:artifact/id (cas/put-bytes! cas-store
                                               (.getBytes (pr-str tool-value)
                                                          java.nio.charset.StandardCharsets/UTF_8)
                                               {}))]
    (sqlite/with-db [conn db]
      (doseq [[artifact-id media-type]
              [[genome-id "application/octet-stream"]
               [resolution-id "application/edn"]
               [phenotype-id "application/edn"]
               [model-ref "application/edn"]
               [tool-ref "application/edn"]]]
        (jdbc/insert! conn :artifacts
                      {:hash artifact-id
                       :media_type media-type
                       :size 0
                       :created_at "2025-01-01T00:00:00Z"}))
      (jdbc/insert! conn :genomes
                    {:id genome-id
                     :created_at "2025-01-01T00:00:00Z"})
      (doseq [[id current]
              [[gen-id 1] [other-gen-id 0]]]
        (jdbc/insert! conn :generations
                      {:id id
                       :genome_id genome-id
                       :resolution_id resolution-id
                       :parent_id nil
                       :state "active"
                       :current current
                       :created_at "2025-01-01T00:00:00Z"}))
      (doseq [[id generation]
              [["s1" gen-id] ["s2" gen-id] ["s3" other-gen-id]]]
        (jdbc/insert! conn :sessions
                      {:id id
                       :generation_id generation
                       :genome_id genome-id
                       :resolution_id resolution-id
                       :phenotype_id phenotype-id
                       :state "completed"
                       :created_at "2025-01-02T00:00:00Z"}))
      (doseq [[id session generation payload-ref]
              [[1 "s1" gen-id model-ref]
               [2 "s2" gen-id tool-ref]
               [3 "s3" other-gen-id model-ref]]]
        (jdbc/insert! conn :events
                      {:id id
                       :session_id session
                       :event_seq 1
                       :generation_id generation
                       :phenotype_id phenotype-id
                       :event_type ":provider/call-completed"
                       :cause_event_id nil
                       :payload_ref payload-ref
                       :payload nil
                       :prev_hash "x"
                       :event_hash "y"
                       :created_at "2025-01-02T00:00:00Z"})))
    db))

(deftest cost-report-aggregates-model-usage
  (testing "the report sums the model counters of the generation's own
            sessions and ignores tool-only calls and other generations"
    (let [db (temp-db)
          cas-root (temp-cas)
          cas-store (cas/->cas cas-root)
          _ (seed! db cas-store "gen-1" "gen-2")
          report (cost/cost-report!
                  {:options {:generation "gen-1"}
                   :overrides {:store/sqlite db
                               :store/cas {:root cas-root :verify false}}})]
      (is (= "gen-1" (:generation/id report)))
      (is (= 2 (:sessions report)))
      (is (= 1 (:model-calls report)) "only the model call counts")
      (is (= 100 (:model-input-tokens (:usage report))))
      (is (= 50 (:model-output-tokens (:usage report))))
      (is (= 0.75 (:model-cost-units (:usage report))))
      (is (empty? (:artifact-errors report))))))

(deftest cost-report-missing-generation
  (testing "an unknown generation id is not an error (empty report)"
    (let [db (temp-db)
          cas-root (temp-cas)
          report (cost/cost-report!
                  {:options {:generation "nope"}
                   :overrides {:store/sqlite db
                               :store/cas {:root cas-root :verify false}}})]
      (is (= "nope" (:generation/id report)))
      (is (= 0 (:sessions report)))
      (is (= 0 (:model-calls report))))))
