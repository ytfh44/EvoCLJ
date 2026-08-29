(ns evoclj.cli.promotion-test
  "component tests: `evoclj lineage` — the per-generation lineage report
  with candidate diffs and provenance (roadmap O5).

  The lineage reconstruction itself (promotion.lineage/lineage) is
  tested in evoclj.promotion.lineage-test; this suite tests the CLI
  REPORT built on top of it (evoclj.cli.promotion/lineage!):

  * known lineage renders correct per-generation entries — the seed
    entry carries its genome id and no parent/diff/reason; the
    promoted generation entry carries BOTH genome ids, the promotion
    reason, and the evidence provenance refs; a rejected candidate
    branch carries the rejection reason and its provenance (and no
    diff — its candidate Genome is not exposed by the lineage record);
  * diff stats are accurate — the file-level diff summary vs the
    parent (counts + changed file paths) computed from the CLI genome
    store bundles matches the known parent/candidate fixture exactly;
  * provenance refs resolve to stored evidence — every :cas/refs and
    :evidence/id content address reads back through a VERIFYING CAS
    (re-hash on read) and the evidence pack body round-trips as EDN;
  * EDN default (round-trippable through clojure.edn/read-string) and
    the --pretty renderer both work through evoclj.cli.main/run;
  * typed failures stay typed: an unknown generation is
    :lineage/generation-not-found; a generation whose Genome bundle is
    missing from the CLI store fails closed with :cli/genome-not-found
    (the diff cannot be shown — the E3 candidate-inspect precedent).

  Fixture lineage (built through the store tables exactly as the task
  prescribes, with REAL Genome bundles and REAL CAS artifacts so the
  lineage strict-mode integrity verification passes):

      G1 (seed) ──(rejected)──▶ C2   (never became a generation)
      G1 ──(promoted)─────────▶ G3   (CURRENT after the promotion)

  Every referenced artifact — the Genome index bodies, the evidence
  packs, the paired-results body — is stored in the state-dir CAS
  under its content address (Global Constraint 21)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.main :as main]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption OpenOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; --- temp-dir plumbing (mirrors cli_test / evolution_test) ------------------

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

(defn- dash [id] (str/replace id ":" "-"))

(def ^:private catalog-fast-fail
  "Keep host startup hermetic and fast: no real models.dev fetch (a
  bounded local connection-refused attempt falls back to
  :catalog/unavailable — the shipped offline behavior)."
  {:modelsdev/catalog {:url "http://127.0.0.1:9/catalog.json"
                       :timeout-ms 1}})

(defn- put-edn!
  "Store an EDN value in the CAS; returns its content address."
  [cas-store v]
  (:artifact/id
   (cas/put-bytes! cas-store
                   (.getBytes (pr-str v) StandardCharsets/UTF_8)
                   {})))

(defn- genome-index-body
  "The canonical CAS body of a loaded Genome — the exact serialization
  of evoclj.genome.hash/tree-digest (path + NUL + digest + LF per
  entry, sorted bytewise) whose SHA-256 is the genome's content
  address. This is the body the lineage integrity re-hash reads back."
  [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by first gpath/bytewise-compare (:files loaded)))))

;; --- the D2 fixture (real bundles + real CAS artifacts) ---------------------

(def ^:private manifest-source
  (pr-str {:genome/format 1
           :agent/id :main
           :agent/entry :graph/main
           :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
           :modules {:topology "topology.edn"
                     :models "models.edn"
                     :memory "memory.edn"
                     :evolution "evolution.edn"}
           :capabilities/requested #{:model/call}
           :evolution {:max-risk :topology
                       :mutable #{:parameters :prompts :skills :programs :topology}}
           :metadata {:name "d2-lineage-fixture"}}))

(def ^:private evolution-source "{:evolution {}}\n")
(def ^:private memory-source "{:memory {}}\n")
(def ^:private models-source "{:models {:planner {:alias :reasoning/high}}}\n")
(def ^:private topology-source
  "{:graph/id :graph/main\n :entry :node/planner\n :nodes\n {:node/planner {:node/type :llm :model :planner :next :node/finish}\n  :node/finish {:node/type :emit}}\n :limits {:max-steps 64}}\n")
(def ^:private route-source
  "(defn run\n  \"Route one task.\"\n  [x]\n  x)\n")

(def ^:private parent-files
  {"manifest.edn" manifest-source
   "evolution.edn" evolution-source
   "memory.edn" memory-source
   "models.edn" models-source
   "topology.edn" topology-source
   "skills/debugging.edn" "{:workflow {:before-edit []}}\n"
   "skills/notes.txt" "alpha\nbeta\ngamma\n"
   "skills/extra.txt" "extra\n"
   "programs/route.clj" route-source})

(def ^:private candidate-files
  "The promoted candidate G3 vs the parent G1: two changed files, one
  added, one removed (the exact diff of the E3 fixture, so the
  expected stats are known: files 4, added 1, removed 1, changed 2,
  insertions 4, deletions 3)."
  (-> parent-files
      (assoc "skills/debugging.edn" "{:workflow {:before-edit [:x]}}\n"
             "skills/notes.txt" "alpha\nbeta\nGAMMA\ndelta\n"
             "skills/new.txt" "fresh\n")
      (dissoc "skills/extra.txt")))

(def ^:private rejected-files
  "The rejected candidate C2 (never diffed — its Genome is not exposed
  by the lineage record; any loadable bundle works)."
  (assoc parent-files
         "skills/notes.txt" "alpha\nbeta\ngamma!\n"))

(defn- write-bundle!
  [dir files]
  (doseq [[rel content] files]
    (let [p (.resolve (Paths/get dir (make-array String 0)) rel)]
      (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
      (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                   (make-array OpenOption 0))))
  dir)

(defn- provision-lineage-store!
  "A temp state dir provisioned like a real host deployment: migrated
  db, the G1 → G3 promoted lineage with a rejected candidate branch
  (C2), real Genome bundles in the CLI genome store (parent under
  genomes/, candidates under candidates/), and every referenced
  artifact (Genome index bodies, evidence packs, a paired-results
  body) in the state-dir CAS so the lineage strict-mode integrity
  verification passes and the provenance refs resolve to stored
  evidence. Returns a context map."
  []
  (let [dir (temp-dir "evoclj-d2-state-")
        _ (Files/createDirectories (Paths/get (str dir "/db") (make-array String 0))
                                   (make-array FileAttribute 0))
        db-path (str dir "/db/evoclj.db")
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        cas-root (str dir "/cas")
        cas-store (cas/->cas cas-root)
        scratch (temp-dir "evoclj-d2-src-")
        parent-root (write-bundle! (str scratch "/parent") parent-files)
        candidate-root (write-bundle! (str scratch "/candidate") candidate-files)
        rejected-root (write-bundle! (str scratch "/rejected") rejected-files)
        parent (load/load-genome parent-root)
        candidate (load/load-genome candidate-root)
        rejected (load/load-genome rejected-root)
        parent-id (:genome/id parent)
        candidate-id (:genome/id candidate)
        rejected-id (:genome/id rejected)
        _ (Files/createDirectories (Paths/get (str dir "/genomes") (make-array String 0))
                                   (make-array FileAttribute 0))
        _ (Files/createDirectories (Paths/get (str dir "/candidates") (make-array String 0))
                                   (make-array FileAttribute 0))
        _ (Files/move (Paths/get parent-root (make-array String 0))
                      (Paths/get (str dir "/genomes/" (dash parent-id)) (make-array String 0))
                      (make-array java.nio.file.CopyOption 0))
        _ (Files/move (Paths/get candidate-root (make-array String 0))
                      (Paths/get (str dir "/candidates/" (dash candidate-id)) (make-array String 0))
                      (make-array java.nio.file.CopyOption 0))
        _ (Files/move (Paths/get rejected-root (make-array String 0))
                      (Paths/get (str dir "/candidates/" (dash rejected-id)) (make-array String 0))
                      (make-array java.nio.file.CopyOption 0))
        ;; CAS artifacts under their content addresses (Global
        ;; Constraint 21): the Genome index bodies, the evidence packs,
        ;; and the rejected evaluation's paired-results body.
        _ (cas/put-bytes! cas-store (.getBytes (genome-index-body parent)
                                               StandardCharsets/UTF_8) {})
        _ (cas/put-bytes! cas-store (.getBytes (genome-index-body candidate)
                                               StandardCharsets/UTF_8) {})
        _ (cas/put-bytes! cas-store (.getBytes (genome-index-body rejected)
                                               StandardCharsets/UTF_8) {})
        evidence-2 (put-edn! cas-store {:evidence/pack :rejection-fixture
                                        :episodes 3 :failures 2})
        evidence-3 (put-edn! cas-store {:evidence/pack :promotion-fixture
                                        :episodes 3 :failures 0})
        paired-2 (put-edn! cas-store {:paired/results :fixture})
        ;; row ids
        c2 (random-uuid) c3 (random-uuid)
        m2 (random-uuid) m3 (random-uuid)
        e2 (random-uuid) e3 (random-uuid)
        p2 (random-uuid) p3 (random-uuid)
        t1 "2025-01-01T00:00:00Z"
        t2 "2025-01-01T00:01:00Z"
        t3 "2025-01-01T00:02:00Z"]
    (sqlite/with-db [conn db]
      ;; Fleet P5/F + 011 FK: ensure artifacts/genomes exist before FK-dependent rows
      (let [res-c (str "sha256:" (apply str (repeat 64 "c")))
            res-d (str "sha256:" (apply str (repeat 64 "d")))]
        (doseq [h [parent-id candidate-id rejected-id res-c res-d evidence-2 evidence-3 paired-2]]
          (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, ?, ?, ?)" h "application/octet-stream" 0 t1]))
        (doseq [g [parent-id candidate-id rejected-id]]
          (jdbc/execute! conn ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES (?, ?)" g t1])))
      (jdbc/insert! conn :generations
                    {:id "generation-1" :genome_id parent-id
                     :resolution_id (str "sha256:" (apply str (repeat 64 "c")))
                     :parent_id nil :state "retired" :current 0 :created_at t1})
      (jdbc/insert! conn :generations
                    {:id "generation-3" :genome_id candidate-id
                     :resolution_id (str "sha256:" (apply str (repeat 64 "d")))
                     :parent_id "generation-1" :state "active" :current 1 :created_at t3})
      (jdbc/insert! conn :mutations
                    {:id (str m2) :parent_genome_id parent-id
                     :hypothesis_id (str (random-uuid)) :evidence_id evidence-2
                     :risk "parameter"
                     :ops (pr-str [{:op :set-parameter :path ["router" "tool-a" "weight"]
                                    :value 0.1}])
                     :expected_effect (pr-str {:primary-metric :task/success
                                               :direction :increase})
                     :created_at t2})
      (jdbc/insert! conn :mutations
                    {:id (str m3) :parent_genome_id parent-id
                     :hypothesis_id (str (random-uuid)) :evidence_id evidence-3
                     :risk "behavioral"
                     :ops (pr-str [{:op :set-edn :file "skills/debugging.edn"
                                    :path [:workflow :before-edit] :value [:x]}])
                     :expected_effect (pr-str {:primary-metric :task/success
                                               :direction :increase})
                     :created_at t2})
      (jdbc/insert! conn :candidates
                    {:id (str c2) :parent_generation_id "generation-1"
                     :parent_genome_id parent-id :genome_id rejected-id
                     :mutation_id (str m2) :evidence_id evidence-2
                     :risk "parameter" :state "rejected" :created_at t2})
      (jdbc/insert! conn :candidates
                    {:id (str c3) :parent_generation_id "generation-1"
                     :parent_genome_id parent-id :genome_id candidate-id
                     :mutation_id (str m3) :evidence_id evidence-3
                     :risk "behavioral" :state "promoted" :created_at t2})
      (jdbc/insert! conn :eval_runs
                    {:id (str e2) :candidate_id (str c2)
                     :parent_generation_id "generation-1"
                     :profile_id ":fixture"
                     :gates (pr-str [{:gate :hard :metric :failure-rate}])
                     :paired_results_ref paired-2
                     :summary (pr-str {:hard {:failure-rate 0.4} :utility {} :cost {}})
                     :eligibility (pr-str {:eligible? false
                                           :reasons [{:gate :hard
                                                      :detail "failure rate above threshold"}]})
                     :status "finalized" :created_at t2})
      (jdbc/insert! conn :eval_runs
                    {:id (str e3) :candidate_id (str c3)
                     :parent_generation_id "generation-1"
                     :profile_id ":fixture"
                     :gates (pr-str [{:gate :hard :metric :failure-rate}])
                     :paired_results_ref nil
                     :summary (pr-str {:hard {:failure-rate 0.0}
                                       :utility {:success 0.95} :cost {}})
                     :eligibility (pr-str {:eligible? true :reasons []})
                     :status "finalized" :created_at t2})
      (jdbc/insert! conn :promotions
                    {:id (str p2) :candidate_id (str c2) :evaluation_id (str e2)
                     :from_generation_id "generation-1" :to_generation_id "generation-1"
                     :decision "rejected"
                     :reason (pr-str {:eligibility {:eligible? false
                                                    :reasons [{:gate :hard
                                                               :detail "failure rate above threshold"}]}})
                     :created_at t2})
      (jdbc/insert! conn :promotions
                    {:id (str p3) :candidate_id (str c3) :evaluation_id (str e3)
                     :from_generation_id "generation-1" :to_generation_id "generation-3"
                     :decision "promoted"
                     :reason (pr-str {:expected-parent "generation-1"
                                      :eligibility {:eligible? true :reasons []}})
                     :created_at t3}))
    {:state-dir dir :db db :cas-root cas-root
     :ids {:genomes {:g1 parent-id :g3 candidate-id :c2 rejected-id}
           :evidence {:e2 evidence-2 :e3 evidence-3}
           :paired {:e2 paired-2}}}))

(defn- walk-nodes
  "Depth-first walk of a lineage tree (node + all :children)."
  [node]
  (cons node (mapcat walk-nodes (:children node))))

(defn- run-lineage!
  "Run `evoclj lineage <id>` against the fixture state dir, returning
  the parsed EDN data."
  [ctx generation & [extra-args]]
  (let [out (java.io.StringWriter.)]
    (binding [*out* out]
      (is (= 0 (main/run (into ["lineage" generation] extra-args)
                         {:state-dir (:state-dir ctx)
                          :overrides catalog-fast-fail}))))
    (edn/read-string (str out))))

;; ============================================================================
;; the per-generation report — EDN default shape
;; ============================================================================

(deftest lineage-renders-correct-per-generation-entries
  (let [ctx (provision-lineage-store!)
        data (run-lineage! ctx "generation-1")]
    (testing "the seed entry carries its genome id and no parent/diff/reason"
      (is (= "generation-1" (:generation/id data)))
      (is (= (get-in ctx [:ids :genomes :g1]) (:genome/id data)))
      (is (nil? (:parent/genome-id data)))
      (is (nil? (:parent/generation-id data)))
      (is (nil? (:promotion/reason data)))
      (is (not (contains? data :diff)))
      (is (= {:evidence/id nil
              :cas/refs [(get-in ctx [:ids :genomes :g1])]}
             (:provenance data))))
    (testing "two children: the promoted generation and the rejected branch"
      (is (= 2 (count (:children data))))
      (let [g3 (first (filter #(= "generation-3" (:generation/id %))
                              (:children data)))
            rejected (first (filter #(nil? (:generation/id %))
                                    (:children data)))]
        (testing "the promoted generation entry carries both genome ids, the reason, and the provenance"
          (is (= (get-in ctx [:ids :genomes :g3]) (:genome/id g3)))
          (is (= "generation-1" (:parent/generation-id g3)))
          (is (= (get-in ctx [:ids :genomes :g1]) (:parent/genome-id g3)))
          (is (= :promoted (:decision (:promotion g3))))
          (is (= {:expected-parent "generation-1"
                  :eligibility {:eligible? true :reasons []}}
                 (:promotion/reason g3)))
          (is (= {:evidence/id (get-in ctx [:ids :evidence :e3])
                  :cas/refs [(get-in ctx [:ids :genomes :g3])
                             (get-in ctx [:ids :genomes :g1])
                             (get-in ctx [:ids :evidence :e3])]}
                 (:provenance g3))))
        (testing "the rejected branch has no generation and no diff, but the rejection reason and provenance"
          (is (nil? (:generation rejected)))
          (is (nil? (:genome/id rejected)))
          (is (= (get-in ctx [:ids :genomes :g1]) (:parent/genome-id rejected)))
          (is (not (contains? rejected :diff)))
          (is (= {:eligibility {:eligible? false
                                :reasons [{:gate :hard
                                           :detail "failure rate above threshold"}]}}
                 (:promotion/reason rejected)))
          (is (= {:evidence/id (get-in ctx [:ids :evidence :e2])
                  :cas/refs [(get-in ctx [:ids :genomes :g1])
                             (get-in ctx [:ids :evidence :e2])
                             (get-in ctx [:ids :paired :e2])]}
                 (:provenance rejected))))))))

;; ============================================================================
;; diff stats accuracy
;; ============================================================================

(deftest lineage-diff-stats-are-accurate
  (let [ctx (provision-lineage-store!)
        data (run-lineage! ctx "generation-1")
        g3 (first (filter #(= "generation-3" (:generation/id %))
                          (:children data)))]
    (testing "the file-level diff summary vs the parent is exact"
      (is (= {:parent/genome-id (get-in ctx [:ids :genomes :g1])
              :genome/id (get-in ctx [:ids :genomes :g3])
              :diff/identical? false
              :stats {:files 4 :added 1 :removed 1 :changed 2
                      :insertions 4 :deletions 3}
              :files ["skills/debugging.edn" "skills/extra.txt"
                      "skills/new.txt" "skills/notes.txt"]}
             (:diff g3))))
    (testing "only generations with a parent carry a diff (the seed and the
              rejected branch have none)"
      (is (= #{"generation-3"}
             (set (keep (fn [n] (when (contains? n :diff)
                                  (:generation/id n)))
                        (walk-nodes data))))))))

;; ============================================================================
;; provenance refs resolve to stored evidence
;; ============================================================================

(deftest lineage-provenance-refs-resolve-to-stored-evidence
  (let [ctx (provision-lineage-store!)
        data (run-lineage! ctx "generation-1")
        v-cas (cas/->cas (:cas-root ctx) {:verify true})]
    (doseq [n (walk-nodes data)]
      (testing (str "every provenance ref of node " (:generation/id n)
                    " resolves through the CAS (verifying re-hash read)")
        (is (seq (:cas/refs (:provenance n))))
        (doseq [ref (:cas/refs (:provenance n))]
          (is (cas/exists? v-cas ref))
          (is (pos? (alength ^bytes (cas/get-bytes v-cas ref)))
              "the body re-hashes to the ref (a corrupt body would throw :store/cas-corrupt)")))
      (when-let [eid (get-in n [:provenance :evidence/id])]
        (testing (str "the evidence pack ref of node " (:generation/id n)
                      " resolves to the stored evidence pack")
          (let [pack (edn/read-string
                      (String. ^bytes (cas/get-bytes v-cas eid)
                               StandardCharsets/UTF_8))]
            (is (map? pack))
            (is (some? (:evidence/pack pack)))))))))

;; ============================================================================
;; --pretty renderer and typed failures
;; ============================================================================

(deftest lineage-pretty-renderer
  (let [ctx (provision-lineage-store!)
        out (java.io.StringWriter.)]
    (testing "--pretty renders a human form of the per-generation report"
      (binding [*out* out]
        (is (= 0 (main/run ["lineage" "generation-1" "--pretty"]
                           {:state-dir (:state-dir ctx)
                            :overrides catalog-fast-fail}))))
      (let [s (str out)]
        (is (str/includes? s ":generation/id:"))
        (is (str/includes? s ":genome/id:"))
        (is (str/includes? s ":promotion/reason:"))
        (is (str/includes? s ":provenance:"))
        (is (str/includes? s ":diff"))
        (is (str/includes? s "skills/notes.txt"))))))

(deftest lineage-unknown-generation-typed-error
  (let [ctx (provision-lineage-store!)]
    (testing "an unknown generation id is the lineage typed error, not a stack trace"
      (let [{:keys [exit data]} (main/execute ["lineage" "generation-999"]
                                              {:state-dir (:state-dir ctx)
                                               :overrides catalog-fast-fail})]
        (is (= 1 exit))
        (is (= :lineage/generation-not-found (:error/type data)))))))

(deftest lineage-missing-bundle-fails-closed
  (let [ctx (provision-lineage-store!)
        g3-id (get-in ctx [:ids :genomes :g3])]
    (delete-tree! (str (:state-dir ctx) "/candidates/" (dash g3-id)))
    (testing "a generation Genome with no bundle in the CLI store fails
              closed with :cli/genome-not-found (the diff cannot be shown)"
      (let [{:keys [exit data]} (main/execute ["lineage" "generation-1"]
                                              {:state-dir (:state-dir ctx)
                                               :overrides catalog-fast-fail})]
        (is (= 1 exit))
        (is (= :cli/genome-not-found (:error/type data)))))))