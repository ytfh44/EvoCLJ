(ns evoclj.evolution.candidate-test
  "component tests for Candidate records (no activation rights).

  The Candidate is the evolution subsystem's immutable successor
  record: it names the parent generation and parent Genome, the
  content-addressed candidate Genome, the mutation and evidence that
  justify it, its risk class, and its place in the state machine
  (:proposed → :materialized → :evaluation-pending; the :evaluated /
  :invalid transitions arrive with the evaluator in M8). This task
  CREATES and PERSISTS those records — it grants no activation rights:
  nothing here may read or write the generations CURRENT pointer or
  depend on a promotion/current namespace (Global Constraint 15 keeps
  promotion a separate subsystem).

  The four normative scenarios, in the task's numbered order:

  - Step 1: creation records parent generation, parent Genome ID,
    mutation ID, evidence ID, candidate Genome ID, and risk, as a
    :proposed candidate with a fresh uuid and timestamp.
  - Step 2: the Candidate API surface contains NO function that
    changes the current generation — asserted by construction (the
    exact public var set, an activation-vocabulary deny-list over
    ns-publics, and no promotion/current namespace dependency) and
    behaviorally (materialization and transitions never touch the
    generations row's :current flag).
  - Step 3: duplicate deterministic materialization of the same
    parent+mutation dedupes to ONE auditable candidate row.
    Uniqueness rule: (parent-genome-id, mutation-hash), where
    mutation-hash is a content hash of the Mutation IR EXCLUDING the
    random :mutation/id — so even two proposals of identical content
    (different uuids) land on the same candidate, while different
    content (or a different parent) yields a separate candidate.
  - Step 4: persistence — materialize-candidate! writes the row
    (state 'materialized' in the component vocabulary), the
    :materialized → :evaluation-pending transition is a
    compare-and-set, and read-back round-trips the record.

  FIXTURE DESIGN: the parent generation row is seeded exactly like the
  component end-to-end test (current = 1 — the seed generation IS the
  CURRENT pointer), so the composite FK (parent_generation_id,
  parent_genome_id) and Database Invariant 8 are exercised against a
  real CURRENT row, and the Step 2/4 assertions can prove the
  candidate pipeline leaves CURRENT untouched. Mutations are
  hand-built fixtures (component style); candidate records come from
  create-candidate."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.candidate-states :as cstates]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixture identity --------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private parent-genome-id
  "The parent Genome the candidate materializes from."
  (str "sha256:" hex64))

(def ^:private candidate-genome-id
  "The content-addressed candidate Genome (component patch output)."
  (str "sha256:" (apply str (repeat 64 "c"))))

(def ^:private evidence-id
  "ArtifactId of the frozen evidence pack the mutation answers."
  (str "sha256:" (apply str (repeat 64 "e"))))

(def ^:private file-hash
  "The :expect/hash preimage digest of the fixture op's target file."
  (str "sha256:" (apply str (repeat 64 "f"))))

(def ^:private resolution-id
  "A compiled ResolutionId for the seeded generation row."
  (str "sha256:" (apply str (repeat 64 "r"))))

(def ^:private generation-id
  "The parent generation's stable id (the seeded CURRENT row)."
  "generation-1")

(defn- uuid
  "A fixed, readable UUID for fixture ids."
  [n]
  (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- mutation*
  "A schema-plausible Mutation IR fixture (component shape) carrying one
  :set-edn op; an optional override map wins."
  [& [overrides]]
  (merge {:mutation/id (uuid 1)
          :parent/genome-id parent-genome-id
          :hypothesis/id (uuid 2)
          :evidence/id evidence-id
          :risk :behavioral
          :ops [{:op :set-edn
                 :file "skills/debugging.edn"
                 :path [:workflow :before-edit]
                 :expect/hash file-hash
                 :value [:reproduce :localize]}]
          :expected-effect {:primary-metric :task/success
                            :direction :increase}}
         overrides))

(defn- candidate-request
  "A valid create-candidate request for the fixture parent+mutation;
  an optional override map wins."
  [& [overrides]]
  (merge {:parent/generation-id generation-id
          :parent/genome-id parent-genome-id
          :candidate/genome-id candidate-genome-id
          :mutation/id (uuid 1)
          :evidence/id evidence-id
          :risk :behavioral}
         overrides))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil
  when nothing is thrown."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

;; --- temp stores (test temp dirs only) ---------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-candidate-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-candidate-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifact trees)."
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-store
  "A migrated temp database seeded with the parent generation row
  (current = 1, Database Invariant 6) plus a temp CAS root. Returns
  a map {:db <sqlite spec> :handle <CandidateStore> :cas <CAS>}.
  Business code must use :handle; :db is for test verification only."
  []
  (let [path (temp-db-path)
        db (sqlite/spec path)
        cas-root (temp-cas-dir)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id parent-genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    {:db db :handle (candidate-store/make-candidate-store db) :cas (cas/->cas cas-root)}))

(defn- current-flag
  "The generations :current value for `generation-id` — the CURRENT
  pointer (Database Invariant 6). Takes raw db spec for verification."
  [db]
  (:current (first (sqlite/query db
                                 ["SELECT current FROM generations WHERE id = ?"
                                  generation-id]))))

;; ============================================================================
;; Step 1 — candidate creation records the full candidate identity
;; ============================================================================

(deftest step-1-creation-records-the-full-candidate-identity
  (let [c (candidate/create-candidate (candidate-request))]
    (testing "creation records every normative field"
      (is (uuid? (:candidate/id c)))
      (is (= generation-id (:parent/generation-id c)))
      (is (= parent-genome-id (:parent/genome-id c)))
      (is (= candidate-genome-id (:candidate/genome-id c)))
      (is (= (uuid 1) (:mutation/id c)))
      (is (= evidence-id (:evidence/id c)))
      (is (= :behavioral (:risk c)))
      (is (= :proposed (:state c)))
      (is (instance? java.util.Date (:created-at c))))
    (testing "two creations are distinct records (fresh candidate uuid)"
      (is (not= (:candidate/id c)
                (:candidate/id (candidate/create-candidate (candidate-request))))))))

(deftest step-1-creation-is-a-closed-validated-contract
  (testing "unknown keys are rejected (closed map)"
    (is (= :candidate/invalid
           (thrown-error-type
            #(candidate/create-candidate (assoc (candidate-request) :bogus 1))))))
  (testing "identity fields are validated at the trust boundary"
    (is (= :candidate/invalid
           (thrown-error-type
            #(candidate/create-candidate (dissoc (candidate-request) :parent/genome-id)))))
    (is (= :candidate/invalid
           (thrown-error-type
            #(candidate/create-candidate (assoc (candidate-request) :parent/genome-id "G42")))))
    (is (= :candidate/invalid
           (thrown-error-type
            #(candidate/create-candidate (assoc (candidate-request)
                                                :candidate/genome-id "sha256:zzz")))))
    (is (= :candidate/invalid
           (thrown-error-type
            #(candidate/create-candidate (assoc (candidate-request) :mutation/id "nope")))))
    (is (= :candidate/invalid
           (thrown-error-type
            #(candidate/create-candidate (assoc (candidate-request)
                                                :evidence/id "sha256:zzz")))))
    (is (= :candidate/invalid
           (thrown-error-type
            #(candidate/create-candidate (assoc (candidate-request) :risk :explosive))))))
  (testing "the optional :created-at is honored"
    (let [ts (java.util.Date. 123456789)
          c (candidate/create-candidate (assoc (candidate-request) :created-at ts))]
      (is (= ts (:created-at c))))))

;; ============================================================================
;; Step 2 — the Candidate API has no function that changes current generation
;; ============================================================================

(deftest step-2-the-candidate-api-cannot-change-the-current-generation
  (let [publics (ns-publics 'evoclj.evolution.candidate)
        names (set (map (comp name key) publics))]
    (testing "the public surface is exactly the documented candidate API"
      (is (= #{"create-candidate" "materialize-candidate!"
               "transition-candidate!" "mark-evaluation-pending!"
               "find-candidate" "find-candidates-by-parent"
               "mutation-hash" "dedupe-key"
               "states" "transitions"
               "CandidateSchema" "CreateCandidateRequest"}
             names)))
    (testing "no public function name claims promotion, activation, or CURRENT changes"
      (is (not-any? #(re-find #"(?i)promot|activ|current|deploy|rollback|canary|switch|pointer" %)
                    names)))
    (testing "no dependency on promotion or current-generation namespaces"
      (is (not-any? #(re-find #"(?i)promotion|current" (str %))
                    (map str (keys (ns-aliases 'evoclj.evolution.candidate))))))
    (testing "the state machine surface is the canonical closed machine (Fleet S2 single source)"
      (is (= candidate/states evoclj.evolution.candidate-states/candidate-states))
      (is (= candidate/transitions evoclj.evolution.candidate-states/candidate-transitions)))))

;; ============================================================================
;; Step 3 — the uniqueness rule: (parent-genome-id, mutation-hash)
;; ============================================================================

(deftest step-3-the-uniqueness-rule-is-parent-genome-plus-mutation-hash
  (let [m (mutation*)]
    (testing "mutation-hash is a canonical content digest"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (candidate/mutation-hash m))))
    (testing "the same mutation content hashes identically regardless of its random id"
      (is (= (candidate/mutation-hash m)
             (candidate/mutation-hash (assoc m :mutation/id (uuid 99))))))
    (testing "different mutation content hashes differently"
      (is (not= (candidate/mutation-hash m)
                (candidate/mutation-hash (assoc-in m [:ops 0 :value] [:different])))))
    (testing "the dedupe key is exactly (parent-genome-id, mutation-hash)"
      (is (= {:parent/genome-id parent-genome-id
              :mutation/hash (candidate/mutation-hash m)}
             (candidate/dedupe-key parent-genome-id m))))))

(deftest step-3-duplicate-materialization-dedupes-to-one-candidate
  (let [store (fresh-store)
        m (mutation*)]
    (testing "the same parent+mutation materialized twice is one auditable candidate"
      (let [c1 (candidate/create-candidate (candidate-request))
            c2 (candidate/create-candidate (candidate-request)) ; fresh uuid, same identity
            m1 (candidate/materialize-candidate! (:handle store) c1 m)
            m2 (candidate/materialize-candidate! (:handle store) c2 m)]
        (is (not= (:candidate/id c1) (:candidate/id c2))
            "the two CREATION records are distinct")
        (is (= (:candidate/id m1) (:candidate/id m2))
            "the second materialization returns the SAME candidate")
        (is (= :materialized (:state m1)))
        (is (= 1 (count (candidate/find-candidates-by-parent (:handle store) parent-genome-id)))
            "exactly one row — auditable, deduplicated")
        (is (= (:candidate/id m1)
               (:candidate/id (candidate/find-candidate (:handle store) (:candidate/id m1))))
            "the persisted row resolves by id")))
    (testing "identical mutation CONTENT under a different uuid also dedupes"
      (let [m2 (assoc m :mutation/id (uuid 42))
            c (candidate/create-candidate (assoc (candidate-request)
                                                 :mutation/id (uuid 42)))
            m3 (candidate/materialize-candidate! (:handle store) c m2)
            first-id (:candidate/id (first (candidate/find-candidates-by-parent (:handle store) parent-genome-id)))]
        (is (= first-id (:candidate/id m3))
            "re-proposing the same mutation content lands on the SAME candidate")
        (is (= 1 (count (candidate/find-candidates-by-parent (:handle store) parent-genome-id)))
            "still exactly one candidate row")
        (is (= 2 (count (sqlite/query (:db store)
                                      ["SELECT * FROM mutations WHERE parent_genome_id = ?"
                                       parent-genome-id])))
            "both PROPOSALS stay durable — the candidate dedupes, the proposals are auditable")))))

(deftest step-3-distinct-mutations-and-parents-are-distinct-candidates
  (let [store (fresh-store)
        m (mutation*)]
    (testing "different mutation content under the same parent is a separate candidate"
      (let [m-other (assoc-in m [:ops 0 :value] [:different])
            c1 (candidate/materialize-candidate! (:handle store) (candidate/create-candidate (candidate-request)) m)
            c2 (candidate/materialize-candidate! (:handle store) (candidate/create-candidate (candidate-request)) m-other)]
        (is (not= (:candidate/id c1) (:candidate/id c2)))
        (is (= 2 (count (candidate/find-candidates-by-parent (:handle store) parent-genome-id))))))
    (testing "a mutation whose parent differs yields a separate candidate
              under the other parent (and never touches CURRENT)"
      (let [other-genome (str "sha256:" (apply str (repeat 64 "b")))
            other-generation "generation-2"
            _ (sqlite/with-db [conn (:db store)]
                (jdbc/insert! conn :generations
                              {:id other-generation
                               :genome_id other-genome
                               :resolution_id resolution-id
                               :parent_id generation-id
                               :state "active"
                               :current 0
                               :created_at "2025-01-02T00:00:00Z"}))
            m-other (assoc m :parent/genome-id other-genome
                             :mutation/id (uuid 77))
            c1 (candidate/materialize-candidate! (:handle store) (candidate/create-candidate (candidate-request)) m)
            c2 (candidate/materialize-candidate! (:handle store) (candidate/create-candidate
                       {:parent/generation-id other-generation
                        :parent/genome-id other-genome
                        :candidate/genome-id candidate-genome-id
                        :mutation/id (uuid 77)
                        :evidence/id evidence-id
                        :risk :behavioral})
                m-other)]
        (is (not= (:candidate/id c1) (:candidate/id c2)))
        (is (= 2 (count (candidate/find-candidates-by-parent (:handle store) parent-genome-id)))
            "the first scenario's two parent-genome candidates persist")
        (is (= 1 (count (candidate/find-candidates-by-parent (:handle store) other-genome))))
        (is (= 1 (current-flag (:db store)))
            "the CURRENT pointer is untouched by either materialization")))))

;; ============================================================================
;; Step 4 — persistence, lineage integrity, and the evaluation-pending CAS
;; ============================================================================

(deftest step-4-materialization-persists-the-candidate-row
  (let [store (fresh-store)
        m (mutation*)
        c (candidate/create-candidate (candidate-request))
        persisted (candidate/materialize-candidate! (:handle store) c m)]
    (testing "the returned candidate is :materialized with the same identity"
      (is (= (:candidate/id c) (:candidate/id persisted)))
      (is (= :materialized (:state persisted))))
    (testing "the row is persisted with the component vocabulary and full lineage"
      (let [rows (sqlite/query (:db store)
                               ["SELECT * FROM candidates WHERE id = ?"
                                (str (:candidate/id c))])]
        (is (= 1 (count rows)))
        (let [row (first rows)]
          (is (= "materialized" (:state row)))
          (is (= generation-id (:parent_generation_id row)))
          (is (= parent-genome-id (:parent_genome_id row)))
          (is (= candidate-genome-id (:genome_id row)))
          (is (= (str (uuid 1)) (:mutation_id row)))
          (is (= evidence-id (:evidence_id row)))
          (is (= "behavioral" (:risk row))))))
    (testing "the mutation-row lineage precondition was materialized too (FK)"
      (is (= 1 (count (sqlite/query (:db store)
                                    ["SELECT * FROM mutations WHERE id = ?"
                                     (str (uuid 1))])))))
    (testing "read-back round-trips the record"
      (is (= persisted (candidate/find-candidate (:handle store) (:candidate/id c)))))
    (testing "the CURRENT pointer is untouched (no activation rights)"
      (is (= 1 (current-flag (:db store)))))))

(deftest step-4-evaluation-pending-is-a-compare-and-set-transition
  (let [store (fresh-store)
        m (mutation*)
        c (candidate/materialize-candidate! (:handle store) (candidate/create-candidate (candidate-request)) m)]
    (testing "a materialized candidate moves to :evaluation-pending"
      (let [pending (candidate/mark-evaluation-pending! (:handle store) (:candidate/id c))]
        (is (= :evaluation-pending (:state pending)))
        (is (= :evaluation-pending
               (:state (candidate/find-candidate (:handle store) (:candidate/id c)))))
        (is (= "evaluating"
               (:state (first (sqlite/query (:db store)
                                            ["SELECT state FROM candidates WHERE id = ?"
                                             (str (:candidate/id c))])))))))
    (testing "re-transitioning from the wrong expected state fails"
      (is (= :candidate/invalid-transition
             (thrown-error-type #(candidate/mark-evaluation-pending! (:handle store)
                                                                    (:candidate/id c)))))
      (is (= :candidate/invalid-transition
             (thrown-error-type #(candidate/transition-candidate! (:handle store) (:candidate/id c) :proposed :materialized)))))
    (testing "a target state with no 5.1 vocabulary value is not persistable"
      (is (= :candidate/invalid-transition
             (thrown-error-type #(candidate/transition-candidate! (:handle store) (:candidate/id c) :materialized :proposed)))))
    (testing "transitioning an unknown candidate fails"
      (is (= :candidate/not-found
             (thrown-error-type #(candidate/mark-evaluation-pending! (:handle store) (uuid 999))))))
    (testing "find-candidate on an unknown id returns nil"
      (is (nil? (candidate/find-candidate (:handle store) (uuid 999)))))
    (testing "the CURRENT pointer is untouched by the transition"
      (is (= 1 (current-flag (:db store)))))))

(deftest step-4-materialization-enforces-record-mutation-agreement
  (let [store (fresh-store)
        c (candidate/create-candidate (candidate-request))]
    (testing "a candidate whose evidence disagrees with the mutation is rejected"
      (is (= :candidate/evidence-mismatch
             (thrown-error-type
              #(candidate/materialize-candidate! (:handle store) c (mutation* {:evidence/id (str "sha256:" (apply str (repeat 64 "d")))}))))))
    (testing "a candidate whose parent Genome disagrees is rejected"
      (is (= :candidate/parent-mismatch
             (thrown-error-type
              #(candidate/materialize-candidate! (:handle store) c (assoc (mutation*)
                               :parent/genome-id (str "sha256:" (apply str (repeat 64 "b")))))))))
    (testing "a candidate whose mutation id disagrees is rejected"
      (is (= :candidate/mutation-mismatch
             (thrown-error-type
              #(candidate/materialize-candidate! (:handle store) c (mutation* {:mutation/id (uuid 77)}))))))
    (testing "a candidate whose risk disagrees is rejected"
      (is (= :candidate/risk-mismatch
             (thrown-error-type
              #(candidate/materialize-candidate! (:handle store) c (mutation* {:risk :parameter}))))))
    (testing "a non-proposed candidate cannot be materialized"
      (is (= :candidate/not-proposed
             (thrown-error-type
              #(candidate/materialize-candidate! (:handle store) (assoc c :state :materialized) (mutation*))))))
    (testing "a mutation that is not a map is rejected"
      (is (= :candidate/mutation-invalid
             (thrown-error-type
              #(candidate/materialize-candidate! (:handle store) c {:not :a-mutation})))))))

(deftest step-4-the-store-boundary-is-validated
  (let [c (candidate/create-candidate (candidate-request))
        m (mutation*)]
    (is (= :candidate/store-invalid
           (thrown-error-type #(candidate/materialize-candidate! nil c m))))
    (is (= :candidate/store-invalid
           (thrown-error-type #(candidate/materialize-candidate! {} c m))))
    (is (= :candidate/store-invalid
           (thrown-error-type
            #(candidate/materialize-candidate!
              {:db (sqlite/spec (temp-db-path))} c m))))
    (is (= :candidate/store-invalid
           (thrown-error-type
            #(candidate/find-candidate {} (uuid 1)))))))