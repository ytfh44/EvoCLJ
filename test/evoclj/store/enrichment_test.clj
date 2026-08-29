(ns evoclj.store.enrichment-test
  "Foundation F3 tests for the versioned derived-metadata store.

  Every test runs against a FRESH migrated sqlite database in a temp
  file plus a FRESH temp CAS root (the per-run scratch isolation of
  evoclj.eval.runner/temp-stores!, but seeding no generation row — the
  enrichment store depends on no lineage). The temp database is
  migrated (evoclj.store.migrate/migrate!) before any enrichment row is
  written, and both temp paths are deleted after each test.

  Covered: put + latest round-trip with payload re-read from the CAS;
  per-(entity-kind, entity-id, kind) version counters ascending; the
  append-only invariant (direct SQL UPDATE on an inserted row is
  rejected by the SQLite trigger); store validation; request
  validation; :cause persistence; and :enrichment/payload-missing when
  the payload artifact is absent from the CAS."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.store.cas :as cas]
            [evoclj.store.enrichment :as enrich]
            [evoclj.store.enrichment-store :as es]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util Date)))

;; --- temp stores (mirrors evoclj.eval.runner temp-stores!) --------------------

(def ^:private store-paths (atom []))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifacts)."
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- temp-stores!
  "A migrated sqlite database in a temp file plus a fresh temp CAS
  root, as an EnrichmentStore handle. The temp paths are recorded for cleanup."
  []
  (let [db-path (str (Files/createTempFile "evoclj-enrichment-" ".db"
                                           (make-array FileAttribute 0)))
        cas-path (str (Files/createTempDirectory "evoclj-enrichment-cas-"
                                                 (make-array FileAttribute 0)))
        db (sqlite/spec db-path)
        cas (cas/->cas cas-path)]
    (migrate/migrate! db)
    (swap! store-paths conj db-path cas-path)
    (es/make-enrichment-store db cas)))

(defn- dispose-stores!
  "Delete the temp paths created by temp-stores! (idempotent)."
  []
  (doseq [p @store-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! store-paths []))

(use-fixtures :each (fn [f] (f) (dispose-stores!)))

;; --- request helpers -----------------------------------------------------------

(def ^:private default-request
  "A valid put-enrichment! request: a :genome entity of :case/weight
  kind with a small :payload map, an explicit nil :cause, and a fixed
  :created-at for determinism."
  {:entity/kind :genome
   :entity/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   :kind :case/weight
   :payload {:weight 0.5}
   :cause nil
   :created-at (Date. 0)})

(defn- valid-request
  "A valid put-enrichment! request merged over `overrides`."
  [& [overrides]]
  (merge default-request overrides))

;; ============================================================================
;; put -> latest / record shape / payload round-trip
;; ============================================================================

(deftest put-and-read-back-with-payload-roundtrip
  (let [store (temp-stores!)
        request (valid-request)
        rec (enrich/put-enrichment! store request)]
    (testing "latest-enrichment returns the put record"
      (let [latest (enrich/latest-enrichment store :genome
                                             (:entity/id request) :case/weight)]
        (is (some? latest))
        (is (= (:enrichment/id rec) (:enrichment/id latest)))
        (is (= 1 (:version latest)))))
    (testing "the record satisfies the enrichment contract"
      (is (uuid? (:enrichment/id rec)))
      (is (pos-int? (:version rec)))
      (is (string? (:payload-ref rec)))
      (is (re-matches #"sha256:[0-9a-f]{64}" (:payload-ref rec)))
      (is (inst? (:created-at rec))))
    (testing "the payload round-trips through the CAS"
      (is (= {:weight 0.5} (enrich/payload store rec))))))

;; ============================================================================
;; versioning: two puts same (entity-kind, entity-id, kind) -> versions 1 and 2
;; ============================================================================

(deftest versions-increment-per-key
  (let [store (temp-stores!)
        r1 (enrich/put-enrichment! store (valid-request {:payload {:weight 0.5}}))
        r2 (enrich/put-enrichment! store (valid-request {:payload {:weight 0.9}}))]
    (testing "versions are 1 then 2"
      (is (= 1 (:version r1)))
      (is (= 2 (:version r2))))
    (testing "latest is version 2"
      (is (= 2 (:version (enrich/latest-enrichment
                          store :genome (:entity/id r1) :case/weight)))))
    (testing "enrichments returns both ascending by :version"
      (let [all (enrich/enrichments store :genome (:entity/id r1) :case/weight)]
        (is (= 2 (count all)))
        (is (= [1 2] (mapv :version all)))))))

;; ============================================================================
;; independent version counters per (entity-kind, entity-id, kind)
;; ============================================================================

(deftest version-counters-are-independent
  (let [store (temp-stores!)
        _ (enrich/put-enrichment! store (valid-request {:entity/id "entity-a"
                                                        :kind :case/weight}))
        _ (enrich/put-enrichment! store (valid-request {:entity/id "entity-a"
                                                        :kind :case/weight}))
        _ (enrich/put-enrichment! store (valid-request {:entity/id "entity-b"
                                                        :kind :case/weight}))
        _ (enrich/put-enrichment! store (valid-request {:entity/id "entity-a"
                                                        :kind :hof/flag
                                                        :payload {:in? true}}))]
    (testing "a third (entity-a, :case/weight) enrichment is version 3"
      (let [r3 (enrich/put-enrichment! store (valid-request {:entity/id "entity-a"
                                                             :kind :case/weight
                                                             :payload {:weight 1.0}}))]
        (is (= 3 (:version r3)))))
    (testing "a different entity has its own counter starting at 1"
      (is (= 1 (-> (enrich/latest-enrichment store :genome "entity-b" :case/weight)
                   :version))))
    (testing "a different kind has its own counter starting at 1"
      (is (= 1 (-> (enrich/latest-enrichment store :genome "entity-a" :hof/flag)
                   :version))))
    (testing "each key's vector is independent"
      (is (= [1 2 3] (->> (enrich/enrichments store :genome "entity-a" :case/weight)
                          (mapv :version))))
      (is (= [1] (->> (enrich/enrichments store :genome "entity-b" :case/weight)
                      (mapv :version))))
      (is (= [1] (->> (enrich/enrichments store :genome "entity-a" :hof/flag)
                      (mapv :version)))))))

;; ============================================================================
;; store validation
;; ============================================================================

(deftest store-validation-errors
  (testing "a non-handle store is rejected"
    (let [e (try (enrich/put-enrichment! "not-a-map" (valid-request))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= :enrichment/store-invalid (:error/type (ex-data e))))
      (is (= :not-an-enrichment-store (:reason (ex-data e))))))
  (testing "a raw {:cas :x} map is rejected (not a handle)"
    (let [e (try (enrich/put-enrichment! {:cas :x} (valid-request))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :enrichment/store-invalid (:error/type (ex-data e))))
      (is (= :not-an-enrichment-store (:reason (ex-data e))))))
  (testing "a raw {:sqlite :x} map is rejected (not a handle)"
    (let [e (try (enrich/put-enrichment! {:sqlite :x} (valid-request))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :enrichment/store-invalid (:error/type (ex-data e))))
      (is (= :not-an-enrichment-store (:reason (ex-data e)))))))

;; ============================================================================
;; request validation
;; ============================================================================

(deftest request-validation-errors
  (let [store (temp-stores!)]
    (testing "a missing :entity/kind is rejected"
      (let [e (try (enrich/put-enrichment! store (dissoc (valid-request) :entity/kind))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :enrichment/invalid (:error/type (ex-data e))))))
    (testing "a missing :entity/id is rejected"
      (let [e (try (enrich/put-enrichment! store (dissoc (valid-request) :entity/id))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :enrichment/invalid (:error/type (ex-data e))))))
    (testing "a missing :kind is rejected"
      (let [e (try (enrich/put-enrichment! store (dissoc (valid-request) :kind))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :enrichment/invalid (:error/type (ex-data e))))))
    (testing "a non-map :payload is rejected"
      (let [e (try (enrich/put-enrichment! store (assoc (valid-request) :payload "not-a-map"))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :enrichment/invalid (:error/type (ex-data e))))))
    (testing "an unknown key is rejected (closed map)"
      (let [e (try (enrich/put-enrichment! store (assoc (valid-request) :bogus 1))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :enrichment/invalid (:error/type (ex-data e))))))))

;; ============================================================================
;; append-only proof (mirrors the events invariant)
;; ============================================================================

(deftest enrichments-are-append-only
  (let [store (temp-stores!)
        _ (enrich/put-enrichment! store (valid-request))]
    (testing "a direct UPDATE on an inserted row is rejected by the trigger"
      (is (thrown-with-msg? java.sql.SQLException #"append-only"
                            (sqlite/exec! (es/db-of store)
                                          ["UPDATE enrichments SET payload_ref = 'x'"]))))
    (testing "a direct DELETE is rejected by the trigger"
      (is (thrown-with-msg? java.sql.SQLException #"append-only"
                            (sqlite/exec! (es/db-of store)
                                          ["DELETE FROM enrichments"]))))
    (testing "the inserted row survives both rejected attempts"
      (is (= 1 (count (sqlite/query (es/db-of store)
                                    ["SELECT id FROM enrichments"])))))))

;; ============================================================================
;; cause_ref is persisted and returned
;; ============================================================================

(deftest cause-ref-is-persisted
  (let [store (temp-stores!)
        request (valid-request {:payload {:weight 0.7}
                                :cause "run/eval-1"})
        rec (enrich/put-enrichment! store request)]
    (is (= "run/eval-1" (:cause rec)))
    (is (= "run/eval-1"
           (-> (enrich/latest-enrichment store :genome (:entity/id request)
                                         :case/weight)
               :cause)))
    (testing "a record without a cause returns nil"
      (let [r2 (enrich/put-enrichment! store (valid-request {:payload {:weight 0.8}}))]
        (is (nil? (:cause r2)))))))

;; ============================================================================
;; payload-missing
;; ============================================================================

(deftest payload-missing-errors
  (testing "a fabricated record referencing an absent artifact throws"
    (let [store (temp-stores!)
          rec {:enrichment/id (java.util.UUID/randomUUID)
               :entity/kind :case
               :entity/id "case-1"
               :kind :notebook/narrative
               :version 1
               :payload-ref (str "sha256:" (apply str (repeat 64 "b")))
               :cause nil
               :created-at (Date. 0)}
          e (try (enrich/payload store rec)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= :enrichment/payload-missing (:error/type (ex-data e))))
      (is (= (:payload-ref rec) (:payload-ref (ex-data e)))))))