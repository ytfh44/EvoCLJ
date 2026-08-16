(ns evoclj.cli.evolution-test
  "Roadmap E3 tests: `candidate inspect --diff <id>` — the per-file
  line diff of a candidate Genome vs its parent Genome (the candidate
  diff report, Task E3).

  The diff core (evoclj.cli.evolution/diff-genomes) is tested on
  hand-built loaded-Genome maps: a diff of two known genomes shows
  exactly the changed files and their line hunks, unrelated (identical)
  files are absent, and identical genomes yield an empty diff. The CLI
  wiring is tested end-to-end through evoclj.cli.main/run on a
  provisioned temp state dir (migrated db + generation/mutation/
  candidate rows + real genome bundles in the CLI genome store),
  asserting BOTH output shapes: the default EDN (round-trippable
  through clojure.edn/read-string) and the --pretty renderer.

  Note on argument order: the CLI parser treats an unknown option as
  value-consuming when a non-option token follows it, so the read-only
  `--diff` flag must trail the candidate id:
  `candidate inspect <candidate-id> --diff`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.evolution :as evolution]
            [evoclj.cli.main :as main]
            [evoclj.genome.load :as load]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption OpenOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; --- temp-dir plumbing (mirrors cli_test) -----------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-dir
  [prefix]
  (let [d (str (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! temp-paths conj d)
    d))

(defn- delete-tree!
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup! []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- dash [id] (str/replace id ":" "-"))

(defn- text-bytes
  "A loaded-Genome :bytes value (an immutable vector of bytes) for `s`."
  [s]
  (vec (.getBytes ^String s StandardCharsets/UTF_8)))

(defn- genome
  "A loaded-Genome-shaped map for the pure diff tests: only :genome/id
  and :files are read by diff-genomes."
  [id files]
  {:genome/id id
   :files (into {}
                (map (fn [[p s]] [p {:bytes (text-bytes s)}]))
                files)})

;; ============================================================================
;; the diff core — diff-genomes (pure, on loaded-Genome maps)
;; ============================================================================

(deftest diff-genomes-shows-exactly-the-changed-files-and-lines
  (let [parent (genome "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                       {"manifest.edn" "{:genome/format 1}\n"
                        "skills/debugging.edn" "{:workflow\n {:before-edit []}}\n"
                        "skills/notes.txt" "alpha\nbeta\ngamma\n"
                        "programs/route.clj" "(defn run [x] x)\n"})
        candidate (genome "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                          {"manifest.edn" "{:genome/format 1}\n"
                           "skills/debugging.edn" "{:workflow\n {:before-edit [:x]}}\n"
                           "skills/notes.txt" "alpha\nbeta\ngamma\n"
                           "programs/route.clj" "(defn run [x] x)\n"})
        out (evolution/diff-genomes parent candidate)]
    (testing "only the changed file appears; identical files are absent"
      (is (= ["skills/debugging.edn"] (mapv :file (:diff/files out)))))
    (testing "the changed file carries exactly its one line hunk"
      (let [f (first (:diff/files out))]
        (is (= :changed (:status f)))
        (is (= 1 (count (:hunks f))))
        (let [h (first (:hunks f))]
          (is (= 2 (:left/start h)))
          (is (= 2 (:right/start h)))
          (is (= [{:number 2 :text " {:before-edit []}}"}]
                 (:left/lines h)))
          (is (= [{:number 2 :text " {:before-edit [:x]}}"}]
                 (:right/lines h))))))
    (testing "the diff is not identical"
      (is (false? (:diff/identical? out))))))

(deftest diff-genomes-added-removed-and-changed-files
  (let [parent (genome "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                       {"skills/notes.txt" "alpha\nbeta\ngamma\n"
                        "skills/old.txt" "keep\n"})
        candidate (genome "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                          {"skills/notes.txt" "alpha\nbeta\nGAMMA\ndelta\n"
                           "skills/new.txt" "fresh\n"})
        by-file (into {}
                      (map (fn [f] [(:file f) f]))
                      (:diff/files (evolution/diff-genomes parent candidate)))]
    (testing "the removed file is reported fully on the left"
      (is (= :removed (:status (get by-file "skills/old.txt"))))
      (let [h (first (:hunks (get by-file "skills/old.txt")))]
        (is (nil? (:right/start h)))
        (is (= [{:number 1 :text "keep"}] (:left/lines h)))
        (is (empty? (:right/lines h)))))
    (testing "the added file is reported fully on the right"
      (is (= :added (:status (get by-file "skills/new.txt"))))
      (let [h (first (:hunks (get by-file "skills/new.txt")))]
        (is (nil? (:left/start h)))
        (is (empty? (:left/lines h)))
        (is (= [{:number 1 :text "fresh"}] (:right/lines h)))))
    (testing "the changed file's first hunk carries the replacement and the append"
      (is (= :changed (:status (get by-file "skills/notes.txt"))))
      (let [h (first (:hunks (get by-file "skills/notes.txt")))]
        (is (= 3 (:left/start h)))
        (is (= 3 (:right/start h)))
        (is (= [{:number 3 :text "gamma"}] (:left/lines h)))
        (is (= [{:number 3 :text "GAMMA"}
                {:number 4 :text "delta"}]
               (:right/lines h)))))))

(deftest diff-genomes-identical-genomes-empty-diff
  (let [g (genome "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                  {"manifest.edn" "{:genome/format 1}\n"
                   "skills/debugging.edn" "{:workflow\n {:before-edit []}}\n"
                   "skills/notes.txt" "alpha\nbeta\ngamma\n"})]
    (testing "the same genome against itself is :diff/identical? with no files"
      (let [out (evolution/diff-genomes g g)]
        (is (true? (:diff/identical? out)))
        (is (empty? (:diff/files out)))))
    (testing "two genomes with identical files (different ids) are also empty"
      (let [other (assoc g :genome/id
                         "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")]
        (is (empty? (:diff/files (evolution/diff-genomes g other))))))))

;; ============================================================================
;; the CLI wiring — candidate inspect <id> --diff through main/run
;; ============================================================================

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
           :metadata {:name "e3-diff-fixture"}}))

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
  (-> parent-files
      (assoc "skills/debugging.edn" "{:workflow {:before-edit [:x]}}\n"
             "skills/notes.txt" "alpha\nbeta\nGAMMA\ndelta\n"
             "skills/new.txt" "fresh\n")
      (dissoc "skills/extra.txt")))

(defn- write-bundle!
  "Write `files` (rel path -> UTF-8 content) under `dir`, creating
  parents."
  [dir files]
  (doseq [[rel content] files]
    (let [p (.resolve (Paths/get dir (make-array String 0)) rel)]
      (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
      (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                   (make-array OpenOption 0))))
  dir)

(defn- provision-diff-store!
  "A temp state dir provisioned like a real host deployment: migrated
  db, the generation-1 row, real parent/candidate Genome bundles in
  the CLI genome store (named by their content addresses), and one
  candidate row referencing both. Returns a context map."
  []
  (let [dir (temp-dir "evoclj-e3-state-")
        _ (Files/createDirectories (Paths/get (str dir "/db") (make-array String 0))
                                   (make-array FileAttribute 0))
        db-path (str dir "/db/evoclj.db")
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        scratch (temp-dir "evoclj-e3-src-")
        parent-root (write-bundle! (str scratch "/parent") parent-files)
        candidate-root (write-bundle! (str scratch "/candidate") candidate-files)
        parent (load/load-genome parent-root)
        candidate (load/load-genome candidate-root)
        parent-id (:genome/id parent)
        candidate-id (:genome/id candidate)
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
        cand-row-id (str (UUID/randomUUID))
        mutation-id (str (UUID/randomUUID))
        evidence-id (str "sha256:" (apply str (repeat 64 "d")))]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id "generation-1"
                     :genome_id parent-id
                     :resolution_id (str "sha256:" (apply str (repeat 64 "c")))
                     :parent_id nil :state "active" :current 1
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :mutations
                    {:id mutation-id
                     :parent_genome_id parent-id
                     :hypothesis_id (str (UUID/randomUUID))
                     :evidence_id evidence-id
                     :risk "behavioral"
                     :ops (pr-str [{:op :set-edn :file "skills/debugging.edn"
                                    :path [:workflow :before-edit] :value [:x]}])
                     :expected_effect (pr-str {:primary-metric :task/success
                                               :direction :increase})
                     :created_at "2025-01-02T00:00:00Z"})
      (jdbc/insert! conn :candidates
                    {:id cand-row-id
                     :parent_generation_id "generation-1"
                     :parent_genome_id parent-id
                     :genome_id candidate-id
                     :mutation_id mutation-id
                     :evidence_id evidence-id
                     :risk "behavioral" :state "materialized"
                     :created_at "2025-01-02T00:00:00Z"}))
    {:state-dir dir :db db :candidate-id cand-row-id
     :parent-id parent-id :candidate-genome-id candidate-id}))

(deftest candidate-inspect-diff-edn-default-shape
  (let [ctx (provision-diff-store!)
        dir (:state-dir ctx)
        out (java.io.StringWriter.)]
    (testing "the default output is EDN and round-trips"
      (binding [*out* out]
        (is (= 0 (main/run ["candidate" "inspect" (:candidate-id ctx) "--diff"]
                           {:state-dir dir}))))
      (let [data (edn/read-string (str out))
            by-file (into {} (map (fn [f] [(:file f) f])) (:diff/files data))]
        (is (= (UUID/fromString (:candidate-id ctx)) (:candidate/id data)))
        (is (= (:parent-id ctx) (:parent/genome-id data)))
        (is (= (:candidate-genome-id ctx) (:candidate/genome-id data)))
        (is (false? (:diff/identical? data)))
        (testing "exactly the four differing paths are reported"
          (is (= #{"skills/debugging.edn" "skills/notes.txt"
                   "skills/new.txt" "skills/extra.txt"}
                 (set (map :file (:diff/files data))))))
        (testing "the changed EDN file carries its line hunk"
          (let [h (first (:hunks (get by-file "skills/debugging.edn")))]
            (is (= 1 (:left/start h)))
            (is (= 1 (:right/start h)))
            (is (= [{:number 1 :text "{:workflow {:before-edit []}}"}]
                   (:left/lines h)))
            (is (= [{:number 1 :text "{:workflow {:before-edit [:x]}}"}]
                   (:right/lines h)))))
        (testing "the changed text file's first hunk carries the replacement and the append"
          (let [h (first (:hunks (get by-file "skills/notes.txt")))]
            (is (= 3 (:left/start h)))
            (is (= 3 (:right/start h)))
            (is (= [{:number 3 :text "gamma"}] (:left/lines h)))
            (is (= [{:number 3 :text "GAMMA"}
                    {:number 4 :text "delta"}]
                   (:right/lines h)))))
        (testing "added and removed files are present with the right status"
          (is (= :added (:status (get by-file "skills/new.txt"))))
          (is (= :removed (:status (get by-file "skills/extra.txt")))))
        (testing "unrelated identical files are absent"
          (is (nil? (get by-file "manifest.edn")))
          (is (nil? (get by-file "topology.edn")))
          (is (nil? (get by-file "programs/route.clj"))))))))

(deftest candidate-inspect-diff-pretty-renderer
  (let [ctx (provision-diff-store!)
        dir (:state-dir ctx)
        out (java.io.StringWriter.)]
    (testing "--pretty renders a human form with the diff"
      (binding [*out* out]
        (is (= 0 (main/run ["candidate" "inspect" (:candidate-id ctx)
                            "--diff" "--pretty"]
                           {:state-dir dir}))))
      (let [s (str out)]
        (is (str/includes? s "diff/files"))
        (is (str/includes? s "skills/debugging.edn"))
        (is (str/includes? s "skills/new.txt"))))))

(deftest candidate-inspect-without-diff-keeps-the-concise-shape
  (let [ctx (provision-diff-store!)
        dir (:state-dir ctx)]
    (testing "candidate inspect without --diff returns the Candidate record"
      (let [{:keys [exit data]} (main/execute ["candidate" "inspect" (:candidate-id ctx)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (not (contains? data :diff/files)))
        (is (not (contains? data :diff/identical?)))
        (is (= (UUID/fromString (:candidate-id ctx)) (:candidate/id data)))))))
