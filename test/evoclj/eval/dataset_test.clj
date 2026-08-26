(ns evoclj.eval.dataset-test
  "component tests for the evaluation profile and physically separated
  datasets (evoclj.eval.profile / evoclj.eval.dataset).

  The four normative scenarios, in the task's numbered order:

  - Step 1 (mount/access): the profile mounts the three datasets from
    THREE physically distinct roots, and candidate workspace
    construction (build-candidate-workspace!) stages ONLY the
    Evolution dataset. The workspace directory listing must NOT
    contain evals/selection or evals/audit, and the workspace root
    must live under the candidate staging root — never under the evals
    tree.
  - Step 2 (adapter isolation): evolution adapters receive only
    artifact refs explicitly copied into their evidence pack — never
    case bodies, never a dataset loader handle. Constructor-level
    isolation: building the evolution system requires and receives no
    selection/audit loader; the evolution-facing input
    (evolution-input) is pure data with no fn values at all.
  - Step 3 (selection loads only in evaluator): selection case BODIES
    exist only behind selection-loader, the evaluator-only API. The
    evolution boundary never carries them — not as bodies, not as
    refs, not as loader handles.
  - Step 4 (audit absent from evolution): the audit set is absent
    from ordinary evolution execution. It is reachable only through
    the explicit operator-only audit-cases accessor, and never
    appears in evolution input, evolution refs, or the workspace.

  FIXTURE DESIGN: each dataset is a temp directory carrying a
  README.md manifest plus deterministic .edn case files, so the three
  datasets are physically separate and the case bodies are fully
  under the test's control. The test passes an explicit roots registry
  everywhere; the repo-root dataset-roots defaults are exercised only
  by the profile-shape assertions. All temp dirs live under the
  system temp dir and are deleted after every test."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.dataset :as dataset]
            [evoclj.eval.profile :as profile])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- temp roots --------------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-dir
  "A fresh empty temp directory (system temp)."
  [prefix]
  (let [d (Files/createTempDirectory prefix (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
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

(defn- write-file!
  "Write `content` (a string) to `path` as UTF-8 with LF line
  endings."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p
                 (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(def ^:private case-files
  "Case payloads per dataset. The .edn files are the case bodies; the
  README.md files are the dataset manifests (documentation only)."
  {:evolution {"README.md" "# Evolution dataset\n\nEvidence cases for the evolution loop.\n"
               "cases.edn" "{:case/id :case/evolve-1\n :body {:prompt \"improve G1\" :expected :pass}}\n"}
   :selection {"README.md" "# Selection dataset\n\nPaired-comparison cases, kernel-only visibility.\n"
               "cases.edn" "{:case/id :case/selection-1\n :body {:prompt \"paired task\" :expected :pass\n        :secret \"selection body must never reach evolution\"}}\n"}
   :audit {"README.md" "# Audit dataset\n\nOperator-only audit cases.\n"
           "cases.edn" "{:case/id :case/audit-1\n :body {:prompt \"audit probe\" :expected :pass}}\n"}})

(defn- seed-dataset!
  "Write the fixture files for one dataset into `root`."
  [root files]
  (doseq [[name content] files]
    (write-file! (str root java.io.File/separator name) content)))

(defn- fixture-roots
  "Three physically separate temp dataset roots plus a temp staging
  root, seeded with the fixture cases. Returns
  {:roots {source -> root} :staging <path>}."
  []
  (let [evo (temp-dir "evoclj-evals-evolution-")
        sel (temp-dir "evoclj-evals-selection-")
        aud (temp-dir "evoclj-evals-audit-")
        staging (temp-dir "evoclj-eval-staging-")]
    (seed-dataset! evo (:evolution case-files))
    (seed-dataset! sel (:selection case-files))
    (seed-dataset! aud (:audit case-files))
    {:roots {:evals/evolution (str evo)
             :evals/selection (str sel)
             :evals/audit (str aud)}
     :staging (str staging)}))

(defn- fixture-profile
  "The normative :default-v1 profile shape pointing at the fixture
  roots."
  []
  {:eval/profile-id :default-v1
   :evolution-set {:source :evals/evolution}
   :selection-set {:source :evals/selection :visibility :kernel-only}
   :audit-set {:source :evals/audit :visibility :operator-only}
   :repetitions 1
   :promotion {:strategy :paired-comparison}})

;; --- helpers ----------------------------------------------------------------

(defn- relative-entries
  "Every entry under `root` (directories and files) as sorted relative
  paths using \"/\" separators."
  [root]
  (let [base (Paths/get root (make-array String 0))]
    (->> (with-open [stream (Files/walk base (make-array FileVisitOption 0))]
           (doall (iterator-seq (.iterator stream))))
         (map (fn [^java.nio.file.Path p]
                (str/replace (str (.relativize base p))
                             java.io.File/separator "/")))
         (remove #(or (str/blank? %) (= "." %)))
         sort
         vec)))

(defn- has-fn-value?
  "True when `x` (deeply, incl. inside maps/vectors) contains any fn."
  [x]
  (cond
    (fn? x) true
    (map? x) (some has-fn-value? (concat (keys x) (vals x)))
    (coll? x) (some has-fn-value? x)
    :else false))

(defn- thrown-ex-data
  "The ex-data of the typed ExceptionInfo thrown by `f`, or nil when
  `f` returns normally (no throw)."
  [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- edn-round-trip
  "pr-str + edn/read-string round trip — a serializability probe for
  plain-data values (GC-22)."
  [x]
  (edn/read-string (pr-str x)))

(defn- empty-roots
  "Three physically separate temp dataset roots, each holding ONLY a
  README.md manifest and ZERO .edn case files — genuinely empty
  datasets (the V1 empty-dataset state). Returns the source -> root
  registry."
  []
  (let [evo (temp-dir "evoclj-evals-empty-evo-")
        sel (temp-dir "evoclj-evals-empty-sel-")
        aud (temp-dir "evoclj-evals-empty-aud-")]
    ;; README.md only — no *.edn case file in any root
    (seed-dataset! evo {"README.md" "# Evolution dataset\n"})
    (seed-dataset! sel {"README.md" "# Selection dataset\n"})
    (seed-dataset! aud {"README.md" "# Audit dataset\n"})
    {:evals/evolution (str evo)
     :evals/selection (str sel)
     :evals/audit (str aud)}))

;; --- Step 1: mount/access — workspace excludes Selection and Audit ----------

(deftest workspace-excludes-selection-and-audit
  (testing "the profile is the normative Malli-validated shape"
    (is (profile/profile? (fixture-profile)))
    (is (profile/profile? profile/default-v1))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"does not satisfy the evaluation profile contract"
                          (profile/validate-profile!
                           (assoc (fixture-profile) :repetitions 0)))))
  (testing "the profile mounts three physically distinct dataset roots"
    (let [{:keys [roots]} (fixture-roots)
          p (fixture-profile)]
      (is (= (str (dataset/dataset-root :evals/evolution roots))
             (dataset/dataset-root (:source (:evolution-set p)) roots)))
      (is (= (str (dataset/dataset-root :evals/selection roots))
             (dataset/dataset-root (:source (:selection-set p)) roots)))
      (is (= (str (dataset/dataset-root :evals/audit roots))
             (dataset/dataset-root (:source (:audit-set p)) roots)))
      (is (apply distinct? (map #(str (dataset/dataset-root % roots))
                                [:evals/evolution :evals/selection :evals/audit])))))
  (testing "candidate workspace construction stages ONLY the evolution dataset"
    (let [{:keys [roots staging]} (fixture-roots)
          ws (dataset/build-candidate-workspace! staging roots)
          ws-root (:workspace/root ws)
          entries (:workspace/entries ws)
          evo-root (str (dataset/dataset-root :evals/evolution roots))
          sel-entry? (fn [e] (str/starts-with? e "evals/selection"))
          aud-entry? (fn [e] (str/starts-with? e "evals/audit"))]
      ;; the evolution dataset IS mounted
      (is (some #(str/starts-with? % "evals/evolution") entries))
      (is (some #(str/ends-with? % "README.md") entries))
      ;; the Selection and Audit directories are absent from the listing
      (is (not-any? sel-entry? entries)
          "workspace listing must not contain evals/selection")
      (is (not-any? aud-entry? entries)
          "workspace listing must not contain evals/audit")
      ;; ... and physically absent on disk under the workspace
      (is (not (Files/exists (Paths/get (str ws-root java.io.File/separator
                                             "evals" java.io.File/separator "selection")
                                        (make-array String 0))
                             (make-array LinkOption 0))))
      (is (not (Files/exists (Paths/get (str ws-root java.io.File/separator
                                             "evals" java.io.File/separator "audit")
                                        (make-array String 0))
                             (make-array LinkOption 0))))
      ;; the workspace root lives under the candidate staging root,
      ;; never under the evals tree
      (is (.startsWith (Paths/get ws-root (make-array String 0))
                       (Paths/get staging (make-array String 0)))
          "workspace root must be under the candidate staging root")
      (is (not (str/starts-with? ws-root evo-root))
          "workspace root must not be under the evals tree")
      (is (not= ws-root evo-root)))))

;; --- Step 2: evolution adapters receive only artifact refs ------------------

(deftest evolution-adapters-receive-refs-only
  (let [{:keys [roots]} (fixture-roots)
        p (fixture-profile)
        refs (dataset/evolution-case-refs p roots)
        input (dataset/evolution-input p roots)]
    (testing "evolution adapters receive artifact refs, never case bodies"
      (is (vector? refs))
      (is (seq refs))
      (doseq [r refs]
        (is (= #{:case/id :artifact-ref} (set (keys r)))
            "a ref carries only the case id and its content address")
        (is (keyword? (:case/id r)))
        (is (str/starts-with? (:artifact-ref r) "sha256:"))))
    (testing "the refs are the content addresses of the evolution cases"
      (let [bodies (dataset/load-cases
                    (str (dataset/dataset-root :evals/evolution roots)))]
        (is (= (set (map :case/id refs)) (set (map :case/id bodies))))
        (is (not-any? #(contains? % :body) refs))))
    (testing "constructor-level isolation: building the evolution system"
      (testing "requires and receives no selection/audit loader"
        ;; the documented evoclj.evolution.core system map carries no
        ;; dataset loader keys; constructing it needs none of them
        (let [system {:store {:sqlite :fixture :cas :fixture}
                      :genome-loader (fn [] {:genome/id :fixture})
                      :candidates-dir (str (:staging (fixture-roots)))
                      :diagnostician {:diagnose (fn [_ _] {:diagnosis/id :fixture})}
                      :mutator {:propose-mutations (fn [_ _] [])}
                      :budget-profile {:hard-limit 1}
                      :programs-registry []
                      :event-sink (fn [_])
                      :phase-hook (fn [_])}
              loader-keys #{:selection-loader :audit-loader
                            :selection-set-loader :audit-set-loader
                            :selection-loader-fn :audit-loader-fn}]
          (is (map? system))
          (is (empty? (clojure.set/intersection loader-keys (set (keys system))))
              "the evolution system carries no selection/audit loader")))
      (testing "the evolution input is pure data — no loader handles anywhere"
        (is (not (has-fn-value? input)))
        (is (not (contains? input :selection-loader)))
        (is (not (contains? input :audit-loader)))
        (is (= (:eval/profile-id input) :default-v1))))))

;; --- Step 3: selection case bodies load only in evaluator code --------------

(deftest selection-loads-only-in-evaluator
  (let [{:keys [roots]} (fixture-roots)
        p (fixture-profile)
        input (dataset/evolution-input p roots)
        refs (dataset/evolution-case-refs p roots)]
    (testing "the dataset API exposes selection cases only via a loader"
      (let [loader (dataset/selection-loader p roots)]
        (is (fn? loader)
            "selection cases are reachable only through the loader fn")
        (let [cases (loader)]
          (is (vector? cases))
          (is (seq cases))
          (is (= :case/selection-1 (:case/id (first cases))))
          (is (contains? (first cases) :body)
              "the loader returns the selection case BODIES"))))
    (testing "the evolution boundary never carries selection bodies"
      (let [body (:body (second (:evolution/refs (dataset/evolution-input p roots))))]
        (is (nil? body)))
      (is (not-any? #(str/includes? (pr-str %) "selection body must never reach evolution")
                    (map pr-str (mapcat identity (:evolution/refs input))))
          "no selection body leaks into the evolution-facing refs")
      (is (not (str/includes? (pr-str input) "paired task"))
          "no selection body leaks into the evolution input"))
    (testing "selection bodies are absent from the evolution refs"
      (is (not-any? #(= :case/selection-1 (:case/id %)) refs)))))

;; --- Step 4: audit set absent from ordinary evolution execution -------------

(deftest audit-absent-from-evolution-execution
  (let [{:keys [roots staging]} (fixture-roots)
        p (fixture-profile)
        input (dataset/evolution-input p roots)
        refs (dataset/evolution-case-refs p roots)]
    (testing "the audit set is absent from the evolution input"
      (is (not (contains? input :audit-set)))
      (is (not (contains? input :audit/cases)))
      (is (not (str/includes? (pr-str input) "audit probe"))))
    (testing "the audit set is absent from the evolution refs"
      (is (not-any? #(= :case/audit-1 (:case/id %)) refs))
      (is (not (str/includes? (pr-str refs) "audit probe"))))
    (testing "the audit set is absent from the candidate workspace"
      (let [entries (:workspace/entries (dataset/build-candidate-workspace! staging roots))]
        (is (not-any? #(str/starts-with? % "evals/audit") entries))))
    (testing "the audit set is reachable only via the operator-only accessor"
      (let [audit (dataset/audit-cases p roots)]
        (is (vector? audit))
        (is (= :case/audit-1 (:case/id (first audit))))
        (is (contains? (first audit) :body))))))

;; ============================================================================
;; V1 — empty dataset: running evals on an empty dataset must NOT silently
;; no-op/mislead. Each eval-facing dataset accessor that must produce cases
;; fails closed with the explicit typed :dataset/empty marker.
;; ============================================================================

(deftest empty-evolution-dataset-fails-closed
  (let [roots (empty-roots)
        p (fixture-profile)]
    (testing "evolution-case-refs fails closed with :dataset/empty"
      (let [data (thrown-ex-data #(dataset/evolution-case-refs p roots))]
        (is (= :dataset/empty (:error/type data)))
        (is (= :evals/evolution (:dataset/source data)))
        (is (= 0 (:case-count data)))))
    (testing "evolution-input fails closed with :dataset/empty"
      (let [data (thrown-ex-data #(dataset/evolution-input p roots))]
        (is (= :dataset/empty (:error/type data)))
        (is (= :evals/evolution (:dataset/source data)))))))

(deftest empty-selection-dataset-fails-closed
  (let [roots (empty-roots)
        p (fixture-profile)]
    (testing "the selection-loader fn fails closed with :dataset/empty when
              the selection dataset has no cases"
      (let [loader (dataset/selection-loader p roots)
            data (thrown-ex-data #(loader))]
        (is (= :dataset/empty (:error/type data)))
        (is (= :evals/selection (:dataset/source data)))))))

(deftest empty-audit-dataset-fails-closed
  (let [roots (empty-roots)
        p (fixture-profile)]
    (testing "audit-cases fails closed with :dataset/empty"
      (let [data (thrown-ex-data #(dataset/audit-cases p roots))]
        (is (= :dataset/empty (:error/type data)))
        (is (= :evals/audit (:dataset/source data)))))))

(deftest empty-dataset-marker-is-typed-and-serializable
  (let [roots (empty-roots)
        p (fixture-profile)
        data (thrown-ex-data #(dataset/evolution-case-refs p roots))]
    ;; GC-22: the marker is plain serializable data (round-trips through
    ;; pr-str / edn/read-string), never a handler or a loader handle.
    (is (keyword? (:error/type data)))
    (is (= :dataset/empty (edn-round-trip (:error/type data))))
    (is (= (:dataset/source data) (edn-round-trip (:dataset/source data))))
    (is (= (:case-count data) (edn-round-trip (:case-count data))))))

(deftest dataset-loader-error-is-typed
  (let [root (temp-dir "evoclj-evals-malformed-")
        p (fixture-profile)
        roots {:evals/evolution (str root)
               :evals/selection (str root)
               :evals/audit (str root)}]
    ;; a malformed case file (not a single case map) is a LOADER ERROR,
    ;; distinct from an empty dataset — it must surface typed
    ;; :dataset/case-invalid, never as an empty/no-op result.
    (write-file! (str root java.io.File/separator "bad.edn")
                 "[1 2 3]\n")
    (testing "a malformed case file fails closed with :dataset/case-invalid"
      (let [data (thrown-ex-data #(dataset/evolution-case-refs p roots))]
        (is (= :dataset/case-invalid (:error/type data)))
        (is (= (str root java.io.File/separator "bad.edn")
               (:file data)))))
    (testing "an unknown source fails closed with :dataset/source-unknown"
      ;; :evals/nope is NOT a key of `roots` — a genuinely unknown source
      (let [data (thrown-ex-data
                  #(dataset/dataset-root :evals/nope
                                         {:evals/evolution (str root)
                                          :evals/selection (str root)
                                          :evals/audit (str root)}))]
        (is (= :dataset/source-unknown (:error/type data)))))))

;; --- README truthfulness (V1): doc must not claim a dataset that does not
;; --- exist, and must describe the empty-dataset behavior accurately. --------

(deftest dataset-readmes-are-truthful
  (testing "each repo-root dataset README truthfully documents the
            empty-dataset behavior (:dataset/empty) rather than claiming
            case files that are not present"
    (doseq [[source root] [[:evals/evolution "evals/evolution"]
                           [:evals/selection "evals/selection"]
                           [:evals/audit "evals/audit"]]]
      (testing (str source)
        (let [readme (slurp (str root "/README.md"))]
          ;; the README must document the explicit empty-dataset marker so
          ;; behavior and docs agree (doc/behavior consistency)
          (is (str/includes? readme ":dataset/empty")
              (str "README must document the :dataset/empty empty-dataset behavior")))))))
