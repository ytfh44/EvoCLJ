(ns evoclj.promotion.lineage-test
  "component tests: build lineage reconstruction.

  lineage/lineage reconstructs the complete evolutionary history of a
  generation from the store tables (generations + candidates +
  mutations + eval_runs + promotions). The fixture (built via the store
  tables, exactly as the task prescribes) is:

      G1 ──(rejected)────▶ C2   (never became a generation)
      G1 ──(promoted)────▶ G3 ──(promoted)────▶ G4
                                                     │
      after the rollback: G3 is CURRENT (:active), G4 is :rolled-back,
      G1 is :superseded.

  The task's numbered steps:

  - Step 1: build the fixture lineage in a temp store and reconstruct
    it — G1 (seed, nil parent/mutation/evidence/evaluation/promotion,
    two children), G3 (parent G1, :promoted, one child), G4 (parent
    G3, :promoted, no children, state :rolled-back after the rollback),
    and CURRENT back on G3.
  - Step 2: the lineage reports REJECTED branches, not only winners —
    walking :children recursively, the rejected candidate branch
    appears as a child of G1 with its rejection promotion record
    (:decision :rejected, :generation nil, no children).
  - Step 3: every edge carries the mutation/evaluation/promotion
    evidence needed to explain it — each child record (G3, G4, and the
    rejected branch) has all five evidence fields, and the cross-field
    links are consistent (promotion ↔ evaluation ↔ candidate,
    mutation's parent genome ↔ the parent generation's genome,
    mutation's evidence id ↔ the evidence reference).
  - Step 4: strict-mode integrity verification over referenced
    artifacts while reconstructing — the technique of
    evoclj.store.recovery (cas/exists? then a VERIFYING re-hash read).
    Strict mode (default) throws :lineage/integrity-failure carrying
    the finding when a referenced Genome/evidence artifact is missing
    or corrupt; lenient mode ({:strict? false}) annotates the affected
    node with :integrity [findings] and completes the reconstruction.

  Fresh temp databases are migrated from the classpath migrations and
  deleted after every test."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.lineage :as lineage]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixtures -------------------------------------------------------

(def ^:private t1 "2025-01-01T00:00:00Z")
(def ^:private t2 "2025-01-01T00:01:00Z")
(def ^:private t3 "2025-01-01T00:02:00Z")
(def ^:private t4 "2025-01-01T00:03:00Z")

(def ^:private res-1 (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private res-3 (str "sha256:" (apply str (repeat 64 "d"))))
(def ^:private res-4 (str "sha256:" (apply str (repeat 64 "e"))))

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db-path
  "A throwaway SQLite file in the system temp dir."
  []
  (let [p (str (Files/createTempFile
                "evoclj-lineage-" ".db"
                (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-cas-root
  "A throwaway CAS root directory in the system temp dir."
  []
  (let [p (str (Files/createTempDirectory
                "evoclj-lineage-cas-"
                (make-array FileAttribute 0)))]
    (swap! cas-roots conj p)
    p))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifact
  bodies/meta files)."
  [p]
  (let [path (Paths/get p (make-array String 0))]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (with-open [stream (Files/walk path (make-array java.nio.file.FileVisitOption 0))]
        (doseq [q (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists q))))))

(defn- cleanup!
  "Delete every temp db file and CAS root created during this run."
  []
  (doseq [p @db-paths]
    (Files/deleteIfExists (Paths/get p (make-array String 0))))
  (doseq [p @cas-roots]
    (delete-tree! p))
  (reset! db-paths [])
  (reset! cas-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- put!
  "Put `body` into the CAS; returns its content address."
  [cas body]
  (:artifact/id (cas/put-bytes! cas (.getBytes body StandardCharsets/UTF_8) {})))

;; --- the component fixture (built via the store tables) ----------------------

(defn- insert-mutation!
  "Insert one mutation row; returns nothing."
  [conn m parent-genome evidence risk ops expected ts]
  (jdbc/insert! conn :mutations
                {:id (str m)
                 :parent_genome_id parent-genome
                 :hypothesis_id (str (random-uuid))
                 :evidence_id evidence
                 :risk risk
                 :ops (pr-str ops)
                 :expected_effect (pr-str expected)
                 :created_at ts}))

(defn- insert-candidate!
  "Insert one candidate row (state 'rejected' or 'promoted')."
  [conn c parent-gen parent-genome genome mutation evidence risk state ts]
  (jdbc/insert! conn :candidates
                {:id (str c)
                 :parent_generation_id parent-gen
                 :parent_genome_id parent-genome
                 :genome_id genome
                 :mutation_id (str mutation)
                 :evidence_id evidence
                 :risk risk
                 :state state
                 :created_at ts}))

(defn- insert-evaluation!
  "Insert one FINALIZED eval_runs row carrying `eligibility`."
  [conn e c parent-gen gates summary eligibility paired ts]
  (jdbc/insert! conn :eval_runs
                {:id (str e)
                 :candidate_id (str c)
                 :parent_generation_id parent-gen
                 :profile_id ":fixture"
                 :gates (pr-str gates)
                 :paired_results_ref paired
                 :summary (pr-str summary)
                 :eligibility (pr-str eligibility)
                 :status "finalized"
                 :created_at ts}))

(defn- insert-promotion!
  "Insert one promotion decision row (`decision` is the component row
  value: 'promoted' or 'rejected')."
  [conn p c e from-gen to-gen decision reason ts]
  (jdbc/insert! conn :promotions
                {:id (str p)
                 :candidate_id (str c)
                 :evaluation_id (str e)
                 :from_generation_id from-gen
                 :to_generation_id to-gen
                 :decision decision
                 :reason (pr-str reason)
                 :created_at ts}))

(defn- fixture
  "Build the component fixture lineage in a fresh temp store. Every
  referenced artifact (Genomes, evidence packs, a paired-results body)
  is a real CAS body, so strict-mode integrity verification passes.

  Final store state AFTER the G3 → G4 → rollback-to-G3 arc: G1
  :superseded ('retired'), G3 :active with current = 1, G4
  :rolled-back. The rejected candidate C2 (from G1) never became a
  generation; its rejection is a promotions row with decision
  'rejected' from G1 to G1 (the pointer did not move — the no-move
  target is the parent generation, which satisfies the FK and records
  the rejection).

  Returns {:db <spec> :cas <cas> :cas-root <path> :ids {...}}."
  []
  (let [db (fresh-db)
        cas-root (temp-cas-root)
        cas (cas/->cas cas-root)
        ;; --- artifacts (Global Constraint 21: rows reference hashes) ---
        genome-1 (put! cas "genome-1 seed genome body")
        genome-2 (put! cas "genome-2 rejected candidate genome body")
        genome-3 (put! cas "genome-3 promoted candidate genome body")
        genome-4 (put! cas "genome-4 promoted candidate genome body")
        evidence-2 (put! cas (pr-str {:evidence/pack :rejection-fixture
                                      :episodes 3 :failures 2}))
        evidence-3 (put! cas (pr-str {:evidence/pack :promotion-fixture-g1
                                      :episodes 3 :failures 0}))
        evidence-4 (put! cas (pr-str {:evidence/pack :promotion-fixture-g3
                                      :episodes 2 :failures 0}))
        paired-2 (put! cas (pr-str {:paired/results :fixture}))
        ;; --- ids ---
        c2 (random-uuid) c3 (random-uuid) c4 (random-uuid)
        m2 (random-uuid) m3 (random-uuid) m4 (random-uuid)
        e2 (random-uuid) e3 (random-uuid) e4 (random-uuid)
        p2 (random-uuid) p3 (random-uuid) p4 (random-uuid)]
    (sqlite/with-db [conn db]
      ;; generations: parents before children (FK); final state AFTER
      ;; the rollback
      (jdbc/insert! conn :generations
                    {:id "generation-1" :genome_id genome-1 :resolution_id res-1
                     :parent_id nil :state "retired" :current 0 :created_at t1})
      (jdbc/insert! conn :generations
                    {:id "generation-3" :genome_id genome-3 :resolution_id res-3
                     :parent_id "generation-1" :state "active" :current 1 :created_at t3})
      (jdbc/insert! conn :generations
                    {:id "generation-4" :genome_id genome-4 :resolution_id res-4
                     :parent_id "generation-3" :state "rolled-back" :current 0 :created_at t4})
      ;; mutations
      (insert-mutation! conn m2 genome-1 evidence-2 "parameter"
                        [{:op :set-parameter :path ["router" "tool-a" "weight"]
                          :value 0.1}]
                        {:effect "fewer class-B failures with tool A"} t2)
      (insert-mutation! conn m3 genome-1 evidence-3 "behavioral"
                        [{:op :replace-route :path ["router"] :from "tool-a" :to "tool-b"}]
                        {:effect "class-B requests route to tool B"} t2)
      (insert-mutation! conn m4 genome-3 evidence-4 "program"
                        [{:op :insert-case :path ["router" "exceptions"] :case "class-C"}]
                        {:effect "class-C requests handled"} t4)
      ;; candidates (Invariant 8: parent genome must match the row)
      (insert-candidate! conn c2 "generation-1" genome-1 genome-2 m2 evidence-2
                         "parameter" "rejected" t2)
      (insert-candidate! conn c3 "generation-1" genome-1 genome-3 m3 evidence-3
                         "behavioral" "promoted" t2)
      (insert-candidate! conn c4 "generation-3" genome-3 genome-4 m4 evidence-4
                         "program" "promoted" t4)
      ;; finalized evaluations
      (insert-evaluation! conn e2 c2 "generation-1"
                          [{:gate :hard :metric :failure-rate}]
                          {:hard {:failure-rate 0.4} :utility {} :cost {}}
                          {:eligible? false
                           :reasons [{:gate :hard :detail "failure rate above threshold"}]}
                          paired-2 t2)
      (insert-evaluation! conn e3 c3 "generation-1"
                          [{:gate :hard :metric :failure-rate}]
                          {:hard {:failure-rate 0.0} :utility {:success 0.95} :cost {}}
                          {:eligible? true :reasons []}
                          nil t2)
      (insert-evaluation! conn e4 c4 "generation-3"
                          [{:gate :hard :metric :failure-rate}]
                          {:hard {:failure-rate 0.0} :utility {:success 0.98} :cost {}}
                          {:eligible? true :reasons []}
                          nil t4)
      ;; promotion decisions (the rejected branch records no pointer
      ;; move: from G1 to G1)
      (insert-promotion! conn p2 c2 e2 "generation-1" "generation-1" "rejected"
                         {:eligibility {:eligible? false
                                        :reasons [{:gate :hard :detail "failure rate above threshold"}]}}
                         t2)
      (insert-promotion! conn p3 c3 e3 "generation-1" "generation-3" "promoted"
                         {:expected-parent "generation-1"
                          :eligibility {:eligible? true :reasons []}} t3)
      (insert-promotion! conn p4 c4 e4 "generation-3" "generation-4" "promoted"
                         {:expected-parent "generation-3"
                          :eligibility {:eligible? true :reasons []}} t4))
    {:db db :cas cas :cas-root cas-root
     :ids {:candidates {:c2 c2 :c3 c3 :c4 c4}
           :evaluations {:e2 e2 :e3 e3 :e4 e4}
           :promotions {:p2 p2 :p3 p3 :p4 p4}
           :genomes {:g1 genome-1 :g2 genome-2 :g3 genome-3 :g4 genome-4}
           :evidence {:e2 evidence-2 :e3 evidence-3 :e4 evidence-4}}}))

(defn- store
  "The lineage store map {:sqlite db :cas cas}."
  [{:keys [db cas]}]
  {:sqlite db :cas cas})

(defn- walk-nodes
  "Depth-first walk of a lineage tree (node + all :children)."
  [node]
  (cons node (mapcat walk-nodes (:children node))))

;; --- CAS tampering helpers (Step 4) ----------------------------------------

(defn- artifact-body
  "The CAS body path for `artifact-id` under `cas-root`."
  [cas-root artifact-id]
  (let [hex (subs artifact-id 7)
        shard (subs hex 0 2)]
    (Paths/get (str cas-root "/sha256/" shard "/" hex "/body")
               (make-array String 0))))

(defn- delete-artifact!
  "Delete the body of `artifact-id` (existence + integrity check fails)."
  [cas-root artifact-id]
  (let [path (artifact-body cas-root artifact-id)]
    (when (Files/exists path (make-array java.nio.file.LinkOption 0))
      (Files/delete path))))

(defn- corrupt-artifact!
  "Overwrite the body of `artifact-id` with bytes that no longer match
  its content id (the verifying re-hash read fails)."
  [cas-root artifact-id]
  (Files/write (artifact-body cas-root artifact-id)
               (.getBytes "corrupted body" StandardCharsets/UTF_8)
               (make-array java.nio.file.OpenOption 0)))

;; --- Step 1: the fixture lineage reconstructs ------------------------------

(deftest step-1-fixture-lineage-reconstructs
  (let [{:keys [cas-root] :as fx} (fixture)
        s (store fx)
        g1 (lineage/lineage s "generation-1")
        g3 (lineage/lineage s "generation-3")
        g4 (lineage/lineage s "generation-4")]
    (testing "G1 is the seed: full lineage fields nil, two children"
      (is (= "generation-1" (:generation/id (:generation g1))))
      (is (= :superseded (:state (:generation g1))))  ; displaced by G3
      (is (nil? (:parent g1)))
      (is (nil? (:mutation g1)))
      (is (nil? (:evidence g1)))
      (is (nil? (:evaluation g1)))
      (is (nil? (:promotion g1)))
      (is (= 2 (count (:children g1)))))
    (testing "G3 was promoted from G1 and reactivated by the rollback"
      (is (= "generation-3" (:generation/id (:generation g3))))
      (is (= :active (:state (:generation g3))))
      (is (= "generation-1" (:generation/id (:parent g3))))
      (is (= :promoted (:decision (:promotion g3))))
      (is (= 1 (count (:children g3)))))
    (testing "G4 was promoted from G3, then rolled back"
      (is (= "generation-4" (:generation/id (:generation g4))))
      (is (= :rolled-back (:state (:generation g4))))
      (is (= "generation-3" (:generation/id (:parent g4))))
      (is (= :promoted (:decision (:promotion g4))))  ; history of how G4 came to be
      (is (empty? (:children g4))))
    (testing "after the rollback CURRENT is back on G3"
      (is (= "generation-3" (:id (current/current-generation (:db fx))))))
    (testing "the reconstruction verifies every referenced artifact (strict default)"
      (is (= 4 (count (walk-nodes g1))))
      (is (= "generation-4"
             (:generation/id (:generation (nth (walk-nodes g1) 2))))))))

;; --- Step 2: rejected branches are reported, not only winners --------------

(deftest step-2-rejected-branches-are-reported
  (let [fx (fixture)
        s (store fx)
        g1 (lineage/lineage s "generation-1")
        rejected (filter #(= :rejected (:decision (:promotion %)))
                         (:children g1))]
    (testing "a rejected candidate branch appears as a child of its parent generation"
      (is (= 1 (count rejected)))
      (let [branch (first rejected)]
        (is (nil? (:generation branch)))          ; never became a generation
        (is (empty? (:children branch)))
        (is (= "generation-1" (:generation/id (:parent branch))))
        ;; the rejection promotion record explains the branch
        (is (= :rejected (:decision (:promotion branch))))
        (is (= (str (:c2 (:candidates (:ids fx))))
               (str (:candidate/id (:promotion branch)))))
        (is (= "generation-1" (:from-generation (:promotion branch))))
        ;; the pointer did not move: from == to for a rejection
        (is (= "generation-1" (:to-generation (:promotion branch))))))
    (testing "winners are not the only children reported"
      (is (= 2 (count (:children g1))))
      (is (some #(= "generation-3" (:generation/id (:generation %)))
                (:children g1))))
    (testing "the rejected branch belongs to G1, not to any winner's lineage"
      (let [g3 (lineage/lineage s "generation-3")
            g4 (first (:children g3))]
        (is (not-any? #(= :rejected (:decision (:promotion %)))
                      (walk-nodes g4)))))))

;; --- Step 3: every edge carries the evidence needed to explain it ----------

(deftest step-3-every-edge-carries-its-evidence
  (let [fx (fixture)
        s (store fx)
        g1 (lineage/lineage s "generation-1")
        edges (filter #(some? (:promotion %)) (walk-nodes g1))]
    (testing "three edges: G1→G3, G1→C2(rejected), G3→G4"
      (is (= 3 (count edges))))
    (doseq [edge edges]
      (testing (str "edge " (:from-generation (:promotion edge))
                    " -> " (:to-generation (:promotion edge)))
        (is (some? (:mutation edge)))
        (is (some? (:evidence edge)))
        (is (some? (:evaluation edge)))
        (is (some? (:promotion edge)))
        ;; promotion ↔ evaluation ↔ candidate links (Invariant 5)
        (is (= (:evaluation/id (:evaluation edge))
               (:evaluation/id (:promotion edge))))
        (is (= (:candidate/id (:evaluation edge))
               (:candidate/id (:promotion edge))))
        ;; the mutation's evidence pack is the node's evidence reference
        (is (= (:evidence/id (:evidence edge))
               (:evidence/id (:mutation edge))))
        ;; the mutation's parent genome is the parent generation's genome
        (is (= (:genome/id (:parent edge))
               (:parent/genome-id (:mutation edge))))
        ;; a generation edge names the child as its :to; a rejected
        ;; branch has no generation and its :to is the no-move parent
        (if (:generation edge)
          (is (= (:generation/id (:generation edge))
                 (:to-generation (:promotion edge))))
          (is (= :rejected (:decision (:promotion edge)))))))
    (testing "the eligibility judgment that drove each decision is retained"
      (let [rejected (first (filter #(= :rejected (:decision (:promotion %)))
                                    (walk-nodes g1)))
            promoted (first (filter #(= "generation-3"
                                        (:generation/id (:generation %)))
                                    (walk-nodes g1)))]
        (is (false? (:eligible? (:eligibility (:evaluation rejected)))))
        (is (true? (:eligible? (:eligibility (:evaluation promoted)))))))))

;; --- Step 4: strict-mode integrity verification over referenced artifacts --

(deftest step-4-strict-integrity-fails-closed
  (let [{:keys [cas-root] :as fx} (fixture)
        s (store fx)
        g4-genome (get-in (:ids fx) [:genomes :g4])]
    (testing "a missing referenced artifact throws :lineage/integrity-failure (strict default)"
      (delete-artifact! cas-root g4-genome)
      (let [e (is (thrown? clojure.lang.ExceptionInfo
                           (lineage/lineage s "generation-1")))]
        (is (= :lineage/integrity-failure (:error/type (ex-data e))))
        (is (= g4-genome (get-in (ex-data e) [:finding :artifact/id])))
        (is (= :artifact-missing (get-in (ex-data e) [:finding :kind])))))
    (testing "a corrupt referenced artifact (re-hash mismatch) also fails closed"
      (let [{:keys [cas-root] :as fx2} (fixture)
            s2 (store fx2)
            g3-genome (get-in (:ids fx2) [:genomes :g3])]
        (corrupt-artifact! cas-root g3-genome)
        (let [e (is (thrown? clojure.lang.ExceptionInfo
                             (lineage/lineage s2 "generation-1")))]
          (is (= :lineage/integrity-failure (:error/type (ex-data e))))
          (is (= :artifact-corrupt (get-in (ex-data e) [:finding :kind])))
          (is (= g3-genome (get-in (ex-data e) [:finding :artifact/id]))))))))

(deftest step-4-lenient-mode-annotates-and-completes
  (let [{:keys [cas-root] :as fx} (fixture)
        s (store fx)
        g4-genome (get-in (:ids fx) [:genomes :g4])]
    (delete-artifact! cas-root g4-genome)
    (testing "lenient mode annotates the affected node and completes the reconstruction"
      (let [g1 (lineage/lineage s "generation-1" {:strict? false})
            g4 (first (filter #(= "generation-4" (:generation/id (:generation %)))
                              (walk-nodes g1)))]
        (is (some? g4))
        (is (= 1 (count (:integrity g4))))
        (is (= g4-genome (get-in g4 [:integrity 0 :artifact/id])))
        (is (= :artifact-missing (get-in g4 [:integrity 0 :kind])))
        ;; every OTHER node is intact (no annotation) and the tree is complete
        (is (= 4 (count (walk-nodes g1))))
        (is (every? #(not (contains? % :integrity))
                    (remove #(identical? g4 %) (walk-nodes g1))))))))

;; --- store argument forms ---------------------------------------------------

(deftest store-without-cas-reconstructs-from-rows-alone
  (let [{:keys [cas-root] :as fx} (fixture)
        db (:db fx)]
    (testing "a bare db (no :cas) skips artifact verification and reconstructs"
      (delete-artifact! cas-root (get-in (:ids fx) [:genomes :g4]))
      (let [g1 (lineage/lineage db "generation-1")]
        (is (= "generation-1" (:generation/id (:generation g1))))
        (is (= 2 (count (:children g1))))))
    (testing "an unknown generation id is a typed error"
      (let [e (is (thrown? clojure.lang.ExceptionInfo
                           (lineage/lineage db "generation-999")))]
        (is (= :lineage/generation-not-found (:error/type (ex-data e))))))))
