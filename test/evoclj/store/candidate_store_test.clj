(ns evoclj.store.candidate-store-test
  "Fleet R — handle opacity and authority confinement.

  Proves that CandidateStore/CurrentStore are narrow opaque handles that
  do NOT expose :db or :sqlite via keyword access and cannot directly
  mutate the generations CURRENT pointer. The only way to verify persistence
  is via the handle's narrow operations and direct db queries using the
  raw db spec obtained at creation time — never via (:db handle)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.store.current-store :as current-store]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.kernel.error :as err])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

(def ^:private temp-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-candidate-store-" ".db" (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @temp-paths]
    (try (Files/deleteIfExists (java.nio.file.Paths/get p (make-array String 0))) (catch Exception _ nil)))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(def ^:private hex64 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
(def ^:private parent-genome-id (str "sha256:" hex64))
(def ^:private candidate-genome-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private evidence-id (str "sha256:" (apply str (repeat 64 "e"))))
(def ^:private file-hash (str "sha256:" (apply str (repeat 64 "f"))))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "r"))))
(def ^:private generation-id "generation-1")

;; --- VerifiedDigest helpers (Fleet P5/F) ---------------------------------------
(defn- ->proof [id] (#'evoclj.store.existence/unsafe-verified-digest id))
(defn- proof-candidate [c]
  (if (and (map? c) (:candidate/genome-id c))
    (update c :candidate/genome-id ->proof)
    c))
(defn- proof-mutation [m]
  (if (map? m)
    (cond-> m
      (:parent/genome-id m) (update :parent/genome-id ->proof)
      (:evidence/id m) (update :evidence/id ->proof)
      (:payload-ref m) (update :payload-ref ->proof)
      (:candidate/payload-ref m) (update :candidate/payload-ref ->proof))
    m))
(defn- materialize-with-proof! [store candidate mutation]
  (candidate/materialize-candidate! store (proof-candidate candidate) (proof-mutation mutation)))

(defn- fresh-db []
  (let [path (temp-db-path)
        db (sqlite/spec path)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :artifacts {:hash parent-genome-id :media_type "application/octet-stream" :size 64 :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :artifacts {:hash candidate-genome-id :media_type "application/octet-stream" :size 64 :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :artifacts {:hash resolution-id :media_type "application/edn" :size 64 :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :artifacts {:hash evidence-id :media_type "application/edn" :size 64 :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :artifacts {:hash file-hash :media_type "application/edn" :size 64 :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :genomes {:id parent-genome-id :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :genomes {:id candidate-genome-id :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id parent-genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    db))

;; ---------------------------------------------------------------------------
;; Opacity
;; ---------------------------------------------------------------------------

(deftest candidate-store-is-opaque
  (let [db (fresh-db)
        h (candidate-store/make-candidate-store db)]
    (testing "handle is a CandidateStore and does not expose :db or :sqlite via keyword"
      (is (instance? evoclj.store.candidate_store.CandidateStore h))
      (is (nil? (:db h)) "handle must not expose :db — deftype keyword access is nil")
      (is (nil? (:sqlite h)) "handle must not expose :sqlite")
      ;; contains? on deftype may throw or return false; either way :db not present
      (is (nil? (:db h)))
      (is (nil? (:sqlite h))))
    (testing "direct generations read via :sqlite/:db on handle is impossible"
      (is (nil? (:sqlite h)))
      (is (nil? (:db h)))
      ;; sqlite/query on nil db should throw — proves raw authority not present on handle
      (is (thrown? Exception
                   (sqlite/query (:sqlite h) ["SELECT * FROM generations WHERE id = ?" generation-id])))
      (is (thrown? Exception
                   (sqlite/query (:db h) ["SELECT * FROM generations WHERE id = ?" generation-id]))))
    (testing "only candidate-store does jdbc on candidates/mutations — generations current is untouched by candidate ops"
      (let [mut {:mutation/id (UUID/randomUUID)
                 :parent/genome-id parent-genome-id
                 :hypothesis/id (UUID/randomUUID)
                 :evidence/id evidence-id
                 :risk :behavioral
                 :ops [{:op :set-edn :file "skills/debugging.edn" :path [:workflow :before-edit] :expect/hash file-hash :value [:reproduce :localize]}]
                 :expected-effect {:primary-metric :task/success :direction :increase}}
            cand (candidate/create-candidate
                  {:parent/generation-id generation-id
                   :parent/genome-id parent-genome-id
                   :candidate/genome-id candidate-genome-id
                   :mutation/id (:mutation/id mut)
                   :evidence/id evidence-id
                   :risk :behavioral})
            materialized (materialize-with-proof! h cand mut)]
        (is (= :materialized (:state materialized)))
        ;; CURRENT still 1 — candidate path never touched generations.current, verify via raw db
        (let [row (first (sqlite/query db ["SELECT current FROM generations WHERE id = ?" generation-id]))]
          (is (= 1 (:current row)) "candidate materialization must not move CURRENT"))))
    (testing "candidate handle cannot update generations.current without raw db"
      (is (thrown? Exception
                   (sqlite/exec! (:sqlite h) ["UPDATE generations SET current = 0 WHERE id = ?" generation-id])))
      ;; Verify current is still 1 via raw db — proves no accidental mutation via handle
      (let [row (first (sqlite/query db ["SELECT current FROM generations WHERE id = ?" generation-id]))]
        (is (= 1 (:current row)))))
    (testing "candidate-store does not expose generation-current operations"
      (is (nil? (some #{"current-generation" "cas-current!" "read-current"}
                      (map str (keys (ns-publics 'evoclj.store.candidate-store)))))))))

(deftest current-store-is-opaque
  (let [db (fresh-db)
        h (current-store/make-current-store db)]
    (testing "handle is a CurrentStore and does not expose :db or :sqlite"
      (is (instance? evoclj.store.current_store.CurrentStore h))
      (is (nil? (:db h)))
      (is (nil? (:sqlite h))))
    (testing "current-generation via handle reads the seeded CURRENT row"
      (let [row (current-store/current-generation h)]
        (is (some? row))
        (is (= generation-id (:id row)))
        (is (= 1 (:current row)))))
    (testing "direct generations update via candidate handle is not possible — current-store is the only narrow authority"
      (let [candidate-h (candidate-store/make-candidate-store db)]
        ;; candidate handle has no current-generation var
        (is (not (contains? (set (keys (ns-publics 'evoclj.store.candidate-store))) 'current-generation)))
        ;; current-store does have it
        (is (contains? (set (keys (ns-publics 'evoclj.store.current-store))) 'current-generation))))
    (testing "(:db handle) is nil for CurrentStore as well"
      (is (nil? (:db h)))
      (is (nil? (:sqlite h))))))

(deftest candidate-ns-rejects-legacy-map
  (let [db (fresh-db)
        cas-root (str (Files/createTempDirectory "evoclj-candidate-store-cas-" (make-array FileAttribute 0)))
        _ (swap! temp-paths conj cas-root)
        legacy {:sqlite db :cas {:root cas-root}}
        h (candidate-store/make-candidate-store db)
        mut {:mutation/id (UUID/randomUUID)
             :parent/genome-id parent-genome-id
             :hypothesis/id (UUID/randomUUID)
             :evidence/id evidence-id
             :risk :behavioral
             :ops [{:op :set-edn :file "skills/debugging.edn" :path [:workflow :before-edit] :expect/hash file-hash :value [:reproduce :localize]}]
             :expected-effect {:primary-metric :task/success :direction :increase}}
        cand (candidate/create-candidate
              {:parent/generation-id generation-id
               :parent/genome-id parent-genome-id
               :candidate/genome-id candidate-genome-id
               :mutation/id (:mutation/id mut)
               :evidence/id evidence-id
               :risk :behavioral})]
    (testing "new handle works directly"
      (let [m (materialize-with-proof! h cand mut)]
        (is (= :materialized (:state m)))))
    (testing "legacy {:sqlite :cas} map is rejected with :candidate/store-invalid"
      (let [mut2 (assoc mut :mutation/id (UUID/randomUUID))
            cand2 (candidate/create-candidate
                   {:parent/generation-id generation-id
                    :parent/genome-id parent-genome-id
                    :candidate/genome-id candidate-genome-id
                    :mutation/id (:mutation/id mut2)
                    :evidence/id evidence-id
                    :risk :behavioral})]
        (is (= :candidate/store-invalid (:error/type (try (materialize-with-proof! legacy cand2 mut2) (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (is (thrown? clojure.lang.ExceptionInfo (materialize-with-proof! legacy cand2 mut2)))
        (is (thrown? clojure.lang.ExceptionInfo (candidate/find-candidate legacy cand2)))
        (is (thrown? clojure.lang.ExceptionInfo (candidate/find-candidates-by-parent legacy parent-genome-id)))))))