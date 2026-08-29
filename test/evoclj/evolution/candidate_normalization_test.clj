(ns evoclj.evolution.candidate-normalization-test
  "S3 Normalization — proves risk/evidence mismatch unrepresentable via normalized path.

  Candidate previously duplicated Mutation fields (parent_genome_id, evidence_id,
  risk). S3 makes mutation the definition: candidate stores only
  mutation_id + genome_id + parent_generation_id + state, and the duplicate
  fields are DERIVED via JOIN mutations at read time (candidates_normalized
  view) and normalized at write time (store derives, DB triggers enforce).

  This test proves:
  - Via the normalized API, a candidate with mismatched risk/evidence/parent
    is normalized to the mutation's values — mismatch cannot be persisted.
  - The DB triggers enforce equality on raw inserts (mismatch aborts).
  - The JOIN-derived view always returns the mutation's values, not the
    physical columns' stale values (when bypassed).
  - Definition > validation: mutation is the source."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private hex64 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
(def ^:private parent-genome-id (str "sha256:" hex64))
(def ^:private candidate-genome-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private evidence-id (str "sha256:" (apply str (repeat 64 "e"))))
(def ^:private other-evidence (str "sha256:" (apply str (repeat 64 "d"))))
(def ^:private file-hash (str "sha256:" (apply str (repeat 64 "f"))))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "r"))))
(def ^:private generation-id "generation-1")

(defn- uuid [n] (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- mutation* [& [overrides]]
  (merge {:mutation/id (uuid 1)
          :parent/genome-id parent-genome-id
          :hypothesis/id (uuid 2)
          :evidence/id evidence-id
          :risk :behavioral
          :ops [{:op :set-edn :file "skills/debugging.edn" :path [:workflow :before-edit] :expect/hash file-hash :value [:reproduce :localize]}]
          :expected-effect {:primary-metric :task/success :direction :increase}}
         overrides))

(defn- candidate-request [& [overrides]]
  (merge {:parent/generation-id generation-id
          :parent/genome-id parent-genome-id
          :candidate/genome-id candidate-genome-id
          :mutation/id (uuid 1)
          :evidence/id evidence-id
          :risk :behavioral}
         overrides))

(def ^:private temp-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-candidate-norm-" ".db" (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir []
  (let [d (Files/createTempDirectory "evoclj-candidate-norm-cas-" (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree! [path]
  (let [p (java.nio.file.Paths/get path (make-array String 0))]
    (when (Files/exists p (make-array java.nio.file.LinkOption 0))
      (with-open [stream (Files/walk p (make-array java.nio.file.FileVisitOption 0))]
        (doseq [pp (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists pp))))))

(defn- cleanup! []
  (doseq [p @temp-paths] (delete-tree! p))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

;; --- Fleet P5/F VerifiedDigest helpers (artifacts/genomes FK at rest + proof at boundary) ---
(defn- ->proof [id] (#'evoclj.store.existence/unsafe-verified-digest id))
(defn- proof-candidate [c]
  (cond-> c
    (:candidate/genome-id c) (update :candidate/genome-id ->proof)
    (:candidate/payload-ref c) (update :candidate/payload-ref ->proof)
    (:payload-ref c) (update :payload-ref ->proof)))
(defn- proof-mutation [m]
  (cond-> m
    (:parent/genome-id m) (update :parent/genome-id ->proof)
    (:evidence/id m) (update :evidence/id ->proof)
    (:payload-ref m) (update :payload-ref ->proof)))
(defn- materialize-with-proof! [store c m]
  (candidate/materialize-candidate! store (proof-candidate c) (proof-mutation m)))
(defn- store-materialize-with-proof! [store c m]
  (candidate-store/materialize! store (proof-candidate c) (proof-mutation m)))

(defn- fresh-store []
  (let [path (temp-db-path)
        db (sqlite/spec path)
        other-parent (str "sha256:" (apply str (repeat 64 "b")))]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      ;; Fleet P5/F FK (009) + 011: artifacts/genomes must exist before generations/candidates
      (doseq [h [parent-genome-id candidate-genome-id resolution-id evidence-id other-evidence file-hash other-parent]]
        (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, ?, ?, ?)" h "text/plain" 0 "2025-01-01T00:00:00Z"]))
      (doseq [g [parent-genome-id candidate-genome-id other-parent]]
        (jdbc/execute! conn ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES (?, ?)" g "2025-01-01T00:00:00Z"]))
      (jdbc/insert! conn :generations
                    {:id generation-id :genome_id parent-genome-id :resolution_id resolution-id
                     :parent_id nil :state "active" :current 1 :created_at "2025-01-01T00:00:00Z"}))
    {:db db :handle (candidate-store/make-candidate-store db)}))

;; ---- Tests ----

(deftest s3-normalized-mismatch-is-unrepresentable
  (testing "Via normalized API, risk/evidence mismatch is normalized to mutation (not stored)"
    (let [store (fresh-store)
          m (mutation* {:risk :behavioral :evidence/id evidence-id})
          ;; Candidate claims different risk and evidence — old code would throw :candidate/risk-mismatch
          c (candidate/create-candidate (candidate-request {:risk :parameter :evidence/id other-evidence}))
          ;; S3: materialize normalizes — no throw, persisted equals mutation
          persisted (materialize-with-proof! (:handle store) c m)]
      (is (= :behavioral (:risk persisted)) "risk derived from mutation, not candidate")
      (is (= evidence-id (:evidence/id persisted)) "evidence derived from mutation")
      (is (= parent-genome-id (:parent/genome-id persisted)) "parent derived from mutation")
      ;; Also verify via JOIN read-back
      (let [found (candidate/find-candidate (:handle store) (:candidate/id persisted))]
        (is (= :behavioral (:risk found)))
        (is (= evidence-id (:evidence/id found))))
      ;; Verify physical columns also equal mutation (store derived)
      (let [row (first (sqlite/query (:db store) ["SELECT risk, evidence_id, parent_genome_id FROM candidates WHERE id = ?" (str (:candidate/id persisted))]))]
        (is (= "behavioral" (:risk row)))
        (is (= evidence-id (:evidence_id row)))
        (is (= parent-genome-id (:parent_genome_id row))))))

  (testing "Parent-genome mismatch via normalized path is also normalized"
    (let [store (fresh-store)
          other-parent (str "sha256:" (apply str (repeat 64 "b")))
          m (mutation* {:parent/genome-id parent-genome-id})
          ;; Candidate claims different parent but same generation (generation-1 matches mutation's parent)
          ;; S3 normalizes parent to mutation's value, so FK stays valid
          c (candidate/create-candidate (candidate-request {:parent/genome-id other-parent}))
          persisted (materialize-with-proof! (:handle store) c m)]
      ;; Normalized to mutation's parent, not candidate's
      (is (= parent-genome-id (:parent/genome-id persisted)))
      (is (= parent-genome-id (:parent/genome-id (candidate/find-candidate (:handle store) (:candidate/id persisted)))))))

  (testing "Mutation-id mismatch still throws (FK identity remains checked)"
    (let [store (fresh-store)
          m (mutation* {:mutation/id (uuid 77)})
          c (candidate/create-candidate (candidate-request {:mutation/id (uuid 1)}))]
      (is (= :candidate/mutation-mismatch
             (:error/type (ex-data (try (materialize-with-proof! (:handle store) c m) (catch clojure.lang.ExceptionInfo e e))))))))

  (testing "Store-level derivation: candidate-store/materialize! derives even when candidate map is mismatched"
    (let [store (fresh-store)
          m (mutation* {:risk :topology})
          mismatched {:candidate/id (uuid 99)
                      :parent/generation-id generation-id
                      :parent/genome-id parent-genome-id
                      :candidate/genome-id candidate-genome-id
                      :mutation/id (uuid 1)
                      :evidence/id other-evidence ;; mismatched
                      :risk :parameter            ;; mismatched
                      :state :proposed
                      :created-at (java.util.Date.)}
          persisted (store-materialize-with-proof! (:handle store) mismatched m)]
      (is (= :topology (:risk persisted)) "store derives risk from mutation")
      (is (= evidence-id (:evidence/id persisted)) "store derives evidence from mutation"))))

(deftest s3-db-triggers-enforce-mismatch
  (testing "Raw SQL insert with mismatched risk/evidence/parent aborts via trigger"
    (let [store (fresh-store)
          m (mutation* {:risk :behavioral})
          c (candidate/create-candidate (candidate-request))
          _ (materialize-with-proof! (:handle store) c m)
          ;; Try raw mismatched insert bypassing store — should abort
          raw-id (str (uuid 999))
          try-insert (fn [risk evidence parent]
                       (try
                         (sqlite/with-db [conn (:db store)]
                           (jdbc/insert! conn :candidates
                                         {:id raw-id
                                          :parent_generation_id generation-id
                                          :parent_genome_id parent
                                          :genome_id candidate-genome-id
                                          :mutation_id (str (uuid 1))
                                          :evidence_id evidence
                                          :risk risk
                                          :state "materialized"
                                          :created_at "2025-01-03T00:00:00Z"}))
                         :inserted
                         (catch Exception e
                           (.getMessage e))))]
      (is (re-find #"must equal mutations" (str (try-insert "parameter" evidence-id parent-genome-id)))
          "risk mismatch aborts")
      (is (re-find #"must equal mutations" (str (try-insert "behavioral" other-evidence parent-genome-id)))
          "evidence mismatch aborts")
      (is (re-find #"must equal mutations" (str (try-insert "behavioral" evidence-id (str "sha256:" (apply str (repeat 64 "b"))))))
          "parent mismatch aborts")))

  (testing "candidates_normalized view derives via JOIN"
    (let [store (fresh-store)
          m (mutation* {:risk :meta})
          c (candidate/create-candidate (candidate-request {:risk :meta}))
          persisted (materialize-with-proof! (:handle store) c m)
          view-row (first (sqlite/query (:db store) ["SELECT risk, evidence_id, parent_genome_id FROM candidates_normalized WHERE id = ?" (str (:candidate/id persisted))]))]
      (is (= "meta" (:risk view-row)))
      (is (= evidence-id (:evidence_id view-row)))
      (is (= parent-genome-id (:parent_genome_id view-row))))))

(deftest s3-join-derived-read
  (testing "find-candidate and find-candidates-by-parent return JOIN-derived values"
    (let [store (fresh-store)
          m1 (mutation* {:mutation/id (uuid 10) :risk :parameter})
          m2 (assoc (mutation* {:mutation/id (uuid 11) :risk :topology}) :ops [{:op :set-edn :file "skills/debugging.edn" :path [:workflow :before-edit] :expect/hash file-hash :value [:other]}])
          c1 (materialize-with-proof! (:handle store) (candidate/create-candidate (candidate-request {:mutation/id (uuid 10) :risk :parameter})) m1)
          c2 (materialize-with-proof! (:handle store) (candidate/create-candidate (candidate-request {:mutation/id (uuid 11) :risk :topology})) m2)
          by-parent (candidate/find-candidates-by-parent (:handle store) parent-genome-id)]
      (is (= 2 (count by-parent)))
      (is (= #{:parameter :topology} (set (map :risk by-parent))))
      (is (= evidence-id (:evidence/id (candidate/find-candidate (:handle store) (:candidate/id c1)))))
      (is (= evidence-id (:evidence/id (candidate/find-candidate (:handle store) (:candidate/id c2))))))))
