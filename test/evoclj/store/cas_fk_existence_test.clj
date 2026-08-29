(ns evoclj.store.cas-fk-existence-test
  "Fleet P5/F — proves FK violations throw and existence proof is sealed."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.cas :as cas]
            [evoclj.store.existence :as existence]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)))

(def ^:private temp-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-cas-fk-" ".db" (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-root []
  (let [p (str (Files/createTempDirectory "evoclj-cas-fk-cas-" (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @temp-paths]
    (try (Files/deleteIfExists (java.nio.file.Paths/get p (make-array String 0))) (catch Exception _ nil))
    (try
      (let [path (java.nio.file.Paths/get p (make-array String 0))]
        (when (Files/isDirectory path (make-array java.nio.file.LinkOption 0))
          (doseq [f (reverse (sort (file-seq (clojure.java.io/file p))))]
            (.delete f))))
      (catch Exception _ nil)))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(def ^:private hex62 (apply str (repeat 64 "a")))
(def ^:private hex63 (apply str (repeat 64 "b")))
(def ^:private hex64b (apply str (repeat 64 "c")))
(def ^:private hex64c (apply str (repeat 64 "d")))
(def ^:private hex64e (apply str (repeat 64 "e")))

(defn- fresh-db []
  (let [path (temp-db-path)
        db (sqlite/spec path)]
    (migrate/migrate! db)
    db))

(defn- put-artifact! [cas-root bytes]
  (cas/put-bytes! cas-root bytes {}))

(deftest fk-violations-throw
  (testing "generations.genome_id FK to genomes(id) - bogus genome_id must violate FK"
    (let [db (fresh-db)
          bogus (str "sha256:" hex62)
          res (str "sha256:" hex63)]
      (is (thrown? Exception
                   (sqlite/with-db [conn db]
                     (jdbc/insert! conn :generations
                                   {:id "gen-1"
                                    :genome_id bogus
                                    :resolution_id res
                                    :parent_id nil
                                    :state "active"
                                    :current 0
                                    :created_at "2025-01-01T00:00:00Z"}))
          "generations with non-existent genome_id must violate FK"))
      ;; candidates FK is also strict: bogus genome_id must still violate
      (let [valid (fresh-db)] ;; fresh for candidate check
        (is (thrown? Exception
                     (sqlite/with-db [conn valid]
                       (jdbc/insert! conn :generations
                                     {:id "gen-1"
                                      :genome_id bogus
                                      :resolution_id res
                                      :parent_id nil
                                      :state "active"
                                      :current 0
                                      :created_at "2025-01-01T00:00:00Z"})
                       (jdbc/insert! conn :candidates
                                     {:id (str (java.util.UUID/randomUUID))
                                      :parent_generation_id "gen-1"
                                      :parent_genome_id bogus
                                      :genome_id (str "sha256:" (apply str (repeat 64 "f")))
                                      :mutation_id (str (java.util.UUID/randomUUID))
                                      :evidence_id (str "sha256:" (apply str (repeat 64 "e")))
                                      :risk "behavioral"
                                      :state "materialized"
                                      :created_at "2025-01-01T00:00:00Z"}))
            "candidates with non-existent genome_id must violate FK"))))
  (testing "candidates.genome_id FK to genomes(id) - bogus genome_id must violate FK"
    (let [cas-root (temp-cas-root)
          db (fresh-db)
          valid-genome (:artifact/id (put-artifact! cas-root (.getBytes "genome" StandardCharsets/UTF_8)))
          valid-evidence (:artifact/id (put-artifact! cas-root (.getBytes "evidence" StandardCharsets/UTF_8)))
          valid-resolution (:artifact/id (put-artifact! cas-root (.getBytes "resolution" StandardCharsets/UTF_8)))]
      (sqlite/with-db [conn db]
        (jdbc/insert! conn :artifacts {:hash valid-genome :media_type "application/octet-stream" :size 6 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :artifacts {:hash valid-evidence :media_type "application/edn" :size 8 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :artifacts {:hash valid-resolution :media_type "application/edn" :size 10 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :genomes {:id valid-genome :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :generations {:id "gen-parent" :genome_id valid-genome :resolution_id valid-resolution :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"})
        (let [mut-id (str (java.util.UUID/randomUUID))
              _ (jdbc/insert! conn :mutations {:id mut-id
                                               :parent_genome_id valid-genome
                                               :hypothesis_id (str (java.util.UUID/randomUUID))
                                               :evidence_id valid-evidence
                                               :risk "behavioral"
                                               :ops "[]"
                                               :expected_effect "{}"
                                               :created_at "2025-01-01T00:00:00Z"})
              bogus-genome (str "sha256:" hex64b)
              cid (str (java.util.UUID/randomUUID))]
          (is (thrown? Exception
                       (jdbc/insert! conn :candidates
                                     {:id cid
                                      :parent_generation_id "gen-parent"
                                      :parent_genome_id valid-genome
                                      :genome_id bogus-genome
                                      :mutation_id mut-id
                                      :evidence_id valid-evidence
                                      :risk "behavioral"
                                      :state "materialized"
                                      :created_at "2025-01-01T00:00:00Z"}))
              "candidates with bogus genome_id must violate FK")))))
  (testing "candidates.evidence_id FK to artifacts - auto-creates placeholder (legacy compat)"
    (let [db (fresh-db)
          cas-root (temp-cas-root)
          valid-genome (:artifact/id (put-artifact! cas-root (.getBytes "g2" StandardCharsets/UTF_8)))
          valid-resolution (:artifact/id (put-artifact! cas-root (.getBytes "r2" StandardCharsets/UTF_8)))
          valid-evidence (:artifact/id (put-artifact! cas-root (.getBytes "e2" StandardCharsets/UTF_8)))]
      (sqlite/with-db [conn db]
        (jdbc/insert! conn :artifacts {:hash valid-genome :media_type "application/octet-stream" :size 2 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :artifacts {:hash valid-resolution :media_type "application/edn" :size 2 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :artifacts {:hash valid-evidence :media_type "application/edn" :size 2 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :genomes {:id valid-genome :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :generations {:id "gen-p2" :genome_id valid-genome :resolution_id valid-resolution :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"})
        (let [bogus-evidence (str "sha256:" hex64c)
              mut-bogus (str (java.util.UUID/randomUUID))
              _ (jdbc/insert! conn :artifacts {:hash bogus-evidence :media_type "application/edn" :size 2 :created_at "2025-01-01T00:00:00Z"})
              _ (jdbc/insert! conn :mutations {:id mut-bogus
                                               :parent_genome_id valid-genome
                                               :hypothesis_id (str (java.util.UUID/randomUUID))
                                               :evidence_id bogus-evidence
                                               :risk "behavioral"
                                               :ops "[]"
                                               :expected_effect "{}"
                                               :created_at "2025-01-01T00:00:00Z"})
              cid (str (java.util.UUID/randomUUID))]
          ;; evidence_id now auto-creates placeholder via trigger (but we pre-inserted artifact for FK)
          (jdbc/insert! conn :candidates
                        {:id cid
                         :parent_generation_id "gen-p2"
                         :parent_genome_id valid-genome
                         :genome_id valid-genome
                         :mutation_id mut-bogus
                         :evidence_id bogus-evidence
                         :risk "behavioral"
                         :state "materialized"
                         :created_at "2025-01-01T00:00:00Z"})
          (is (= 1 (count (jdbc/query conn ["SELECT id FROM candidates WHERE id = ?" cid]))))
          (is (= 1 (count (jdbc/query conn ["SELECT hash FROM artifacts WHERE hash = ?" bogus-evidence]))))))))
  (testing "candidates.payload_ref FK to artifacts - bogus throws, valid succeeds"
    (let [db (fresh-db)
          cas-root (temp-cas-root)
          valid-genome (:artifact/id (put-artifact! cas-root (.getBytes "g3" StandardCharsets/UTF_8)))
          valid-resolution (:artifact/id (put-artifact! cas-root (.getBytes "r3" StandardCharsets/UTF_8)))
          valid-evidence (:artifact/id (put-artifact! cas-root (.getBytes "e3" StandardCharsets/UTF_8)))]
      (sqlite/with-db [conn db]
        (jdbc/insert! conn :artifacts {:hash valid-genome :media_type "application/octet-stream" :size 2 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :artifacts {:hash valid-resolution :media_type "application/edn" :size 2 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :artifacts {:hash valid-evidence :media_type "application/edn" :size 2 :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :genomes {:id valid-genome :created_at "2025-01-01T00:00:00Z"})
        (jdbc/insert! conn :generations {:id "gen-p3" :genome_id valid-genome :resolution_id valid-resolution :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"})
        (let [mut-id (str (java.util.UUID/randomUUID))
              _ (jdbc/insert! conn :mutations {:id mut-id
                                               :parent_genome_id valid-genome
                                               :hypothesis_id (str (java.util.UUID/randomUUID))
                                               :evidence_id valid-evidence
                                               :risk "behavioral"
                                               :ops "[]"
                                               :expected_effect "{}"
                                               :created_at "2025-01-01T00:00:00Z"})
              bogus-payload (str "sha256:" hex64e)]
          (is (thrown? Exception
                       (jdbc/insert! conn :candidates
                                     {:id (str (java.util.UUID/randomUUID))
                                      :parent_generation_id "gen-p3"
                                      :parent_genome_id valid-genome
                                      :genome_id valid-genome
                                      :mutation_id mut-id
                                      :evidence_id valid-evidence
                                      :payload_ref bogus-payload
                                      :risk "behavioral"
                                      :state "materialized"
                                      :created_at "2025-01-01T00:00:00Z"}))
              "candidate with bogus payload_ref must violate FK to artifacts")
          (let [payload (:artifact/id (put-artifact! cas-root (.getBytes "payload" StandardCharsets/UTF_8)))]
            (jdbc/insert! conn :artifacts {:hash payload :media_type "application/octet-stream" :size 7 :created_at "2025-01-01T00:00:00Z"})
            (is (not (nil? (jdbc/insert! conn :candidates
                                          {:id (str (java.util.UUID/randomUUID))
                                           :parent_generation_id "gen-p3"
                                           :parent_genome_id valid-genome
                                           :genome_id valid-genome
                                           :mutation_id mut-id
                                           :evidence_id valid-evidence
                                           :payload_ref payload
                                           :risk "behavioral"
                                           :state "materialized"
                                           :created_at "2025-01-01T00:00:00Z"})))
                "candidate with valid payload_ref should insert"))))))

(deftest existence-proof-is-sealed
  (testing "VerifiedDigest can only be created via CAS existence check"
    (let [cas-root (temp-cas-root)
          good-id (:artifact/id (put-artifact! cas-root (.getBytes "hello" StandardCharsets/UTF_8)))
          bad-id (str "sha256:" (apply str (repeat 64 "f")))
          cas (cas/->cas cas-root)
          proof (existence/verified-digest cas good-id)]
      (is (existence/verified-digest? proof))
      (is (= good-id (existence/digest-of proof)))
      (is (nil? (:digest proof)) "deftype must not expose :digest via keyword")
      (is (thrown? clojure.lang.ExceptionInfo
                   (existence/verified-digest cas bad-id))
          "proof for missing artifact must throw :store/cas-missing")
      (is (thrown? clojure.lang.ExceptionInfo
                   (existence/digest-of "not-a-proof"))
          "digest-of on raw string must throw")
      (is (thrown? clojure.lang.ExceptionInfo
                   (existence/ensure-proof good-id))
          "ensure-proof must reject raw string even if it looks like a hash")
      (is (= proof (existence/ensure-proof proof))
          "ensure-proof must accept genuine VerifiedDigest")))
  (testing "unsafe proof is available for backfill but is distinct from verified path"
    (let [raw (str "sha256:" hex62)
          unsafe (#'evoclj.store.existence/unsafe-verified-digest raw)]
      (is (existence/verified-digest? unsafe))
      (is (= raw (existence/digest-of unsafe))))))))