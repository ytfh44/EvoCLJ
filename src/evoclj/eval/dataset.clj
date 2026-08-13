(ns evoclj.eval.dataset
  "Physically separated evaluation datasets (Task 8.1).

  The three datasets — Evolution, Selection, Audit — live in THREE
  DISTINCT filesystem roots (by default evals/evolution,
  evals/selection, evals/audit at the repository root; the repo-root
  dirs carry README.md manifests and the .edn case files that later
  milestones add). The separation is PHYSICAL, not a convention: a
  profile's :source keywords resolve through the dataset-roots
  registry to distinct paths, and every accessor takes an explicit
  roots registry so tests (and deployments) mount the three datasets
  wherever they must.

  THE BOUNDARY (Global Constraints 11, 12, 23) — who may see what:

  - Evolution adapters (Diagnostician, Mutator — evoclj.evolution.*)
    receive ONLY artifact refs: {:case/id k :artifact-ref
    \"sha256:...\"} content addresses of the evolution cases, the
    refs an evidence pack copies. They never receive case bodies and
    never receive a dataset loader handle. evolution-input is the
    COMPLETE evolution-facing construction for a profile, and it is
    pure data — no fn values at all (Global Constraint 22).
  - Selection case bodies are loaded ONLY by evaluator code, after
    candidate materialization, through selection-loader — the loader
    fn is the ONLY dataset API surface that reveals selection bodies,
    and it is never part of the evolution boundary. The selection set
    is :kernel-only by profile contract.
  - The audit set is OPERATOR-only and absent from ordinary automated
    evolution execution entirely: it never appears in evolution
    input, in evolution refs, or in a candidate workspace. It is
    reachable only through the explicit audit-cases accessor.
  - Candidate workspaces (build-candidate-workspace!) stage ONLY the
    evolution dataset under a candidate staging root; the selection
    and audit directories are never mounted into a workspace.

  Error contract (Global Constraint 22 — plain serializable data):
  :dataset/source-unknown, :dataset/case-invalid,
  :dataset/workspace-invalid. Profile contract violations surface as
  :eval/profile-invalid from evoclj.eval.profile."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.eval.profile :as profile]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- the physical dataset registry ------------------------------------------

(def dataset-roots
  "The default source -> physical root registry. The three datasets
  are physically separated directories at the repository root; the
  README.md in each root documents the dataset and its visibility.
  Callers may pass an explicit roots map to every accessor."
  {:evals/evolution "evals/evolution"
   :evals/selection "evals/selection"
   :evals/audit "evals/audit"})

(defn dataset-root
  "The physical root path for a dataset :source keyword. `roots` is
  the source -> path registry (default dataset-roots). Unknown sources
  fail loudly with :dataset/source-unknown."
  ([source] (dataset-root source dataset-roots))
  ([source roots]
   (or (get roots source)
       (throw (err/error :dataset/source-unknown
                         "no physical dataset root for this source"
                         {:source source :known-sources (vec (sort (keys roots)))})))))

;; --- case loading ------------------------------------------------------------

(defn- case-file?
  "A case file: a *.edn file (README.md and other manifests are not
  cases)."
  [^String name]
  (str/ends-with? name ".edn"))

(defn- read-case-file
  "Read one .edn case file: the file must hold a single case map
  carrying a :case/id. Anything else is a corrupted dataset and fails
  loudly (:dataset/case-invalid)."
  [^java.nio.file.Path path]
  (let [content (slurp (str path))
        value (edn/read-string content)]
    (when-not (map? value)
      (throw (err/error :dataset/case-invalid
                        "a case file must hold a single case map"
                        {:file (str path) :value (err/sanitize value)})))
    (when-not (keyword? (:case/id value))
      (throw (err/error :dataset/case-invalid
                        "a case map must carry a keyword :case/id"
                        {:file (str path) :case/id (:case/id value)})))
    value))

(defn load-cases
  "Load every case record from a dataset root: the root's *.edn files,
  each holding one case map {:case/id keyword ...}, in deterministic
  filename order. Returns a vector of case maps (bodies included).

  This is the low-level reader. Evolution must never call it on the
  selection or audit roots — see selection-loader and audit-cases,
  which are the only sanctioned entry points to those bodies."
  [root]
  (let [dir (Paths/get root (make-array String 0))]
    (when-not (Files/isDirectory dir (make-array LinkOption 0))
      (throw (err/error :dataset/case-invalid
                        "dataset root is not a directory"
                        {:root root})))
    (->> (Files/list dir)
         (.iterator)
         (iterator-seq)
         (map (fn [^java.nio.file.Path p] [(.getFileName p) p]))
         (filter (fn [[n _]] (case-file? (str n))))
         (sort-by (fn [[n _]] (str n)))
         (mapv (fn [[_ p]] (read-case-file p))))))

;; --- content-addressed refs --------------------------------------------------

(defn- canonical
  "Deterministic EDN form for hashing: maps sorted by their pr-str key
  form, sets by their pr-str element form, collections realized
  eagerly (the same convention as evoclj.evolution.evidence)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- case-ref
  "The artifact ref for one case: the deterministic content address of
  its body (Global Constraint 21 — large payloads live by content
  hash; the ref carries no body bytes). :case/id is proposal
  metadata, not content, so the ref is a pure function of the body."
  [case]
  {:case/id (:case/id case)
   :artifact-ref (hash/text-digest (pr-str (canonical (dissoc case :case/id))))})

(defn case-refs
  "Content-addressed artifact refs for a vector of case maps — no
  bodies, no loader handles. This is exactly what evolution adapters
  may see (Step 2)."
  [cases]
  (mapv case-ref cases))

(defn- dataset-case-refs
  "Artifact refs of one dataset by source, read through its root."
  [source roots]
  (case-refs (load-cases (dataset-root source roots))))

;; --- Step 2: the evolution boundary ------------------------------------------

(defn evolution-case-refs
  "The Evolution dataset as artifact refs ONLY — the input an evidence
  pack copies (Step 2). No case bodies and no loader handles ever
  cross this boundary; constructing it requires no selection/audit
  loader."
  ([profile] (evolution-case-refs profile dataset-roots))
  ([profile roots]
   (profile/validate-profile! profile)
   (dataset-case-refs (get-in profile [:evolution-set :source]) roots)))

(defn evolution-input
  "The COMPLETE evolution-facing dataset construction for a profile
  (Steps 2/3/4). Pure data — plain EDN-safe Clojure values only, no fn
  values anywhere (Global Constraint 22): the evolution set plus the
  artifact refs an evidence pack copies. The selection set is NOT
  mounted here (no bodies, no refs, no loader) and the audit set is
  absent from ordinary evolution execution entirely. Constructor-level
  isolation: building this input requires and receives no
  selection/audit loader."
  ([profile] (evolution-input profile dataset-roots))
  ([profile roots]
   (profile/validate-profile! profile)
   (let [refs (evolution-case-refs profile roots)]
     {:eval/profile-id (:eval/profile-id profile)
      :evolution-set (:evolution-set profile)
      :evolution/refs refs
      :evolution/summary {:cases (count refs)}})))

;; --- Step 3: selection bodies load only in evaluator code --------------------

(defn selection-loader
  "EVALUATOR-ONLY: a loader fn for the Selection dataset case BODIES.

  The returned fn is the ONLY dataset API surface that reveals
  selection bodies. It is deliberately NOT part of the evolution
  boundary: evolution-input and evolution-case-refs never carry it,
  and the evoclj.evolution.* namespaces have no dependency on this
  namespace at all (Global Constraint 11). It must be invoked by
  evaluator code only, after candidate materialization. Invoking the
  loader loads the selection cases fresh from the selection root."
  ([profile] (selection-loader profile dataset-roots))
  ([profile roots]
   (profile/validate-profile! profile)
   (let [root (dataset-root (get-in profile [:selection-set :source]) roots)]
     (fn []
       (load-cases root)))))

;; --- Step 4: the audit set is operator-only ----------------------------------

(defn audit-cases
  "OPERATOR-ONLY access to the Audit dataset case bodies.

  The audit set is absent from ordinary automated evolution execution
  entirely (Step 4): it never appears in evolution-input, in
  evolution-case-refs, or in a candidate workspace. This explicit
  accessor is the only sanctioned way to reach audit cases, for
  operator-run audits."
  ([profile] (audit-cases profile dataset-roots))
  ([profile roots]
   (profile/validate-profile! profile)
   (load-cases (dataset-root (get-in profile [:audit-set :source]) roots))))

;; --- Step 1: candidate workspace construction --------------------------------

(defn- write-file!
  "Write `content` as UTF-8 to `path`, creating parent directories.
  Used for the workspace manifest."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(defn- workspace-entries
  "Every entry under `root` (directories and files) as sorted relative
  paths using \"/\" separators — the directory listing a workspace
  consumer (and the Step 1 test) inspects."
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

(defn- copy-tree!
  "Copy every file under `src` into `dst`, preserving relative paths."
  [src dst]
  (let [src-path (Paths/get src (make-array String 0))
        dst-path (Paths/get dst (make-array String 0))]
    (with-open [stream (Files/walk src-path (make-array FileVisitOption 0))]
      (doseq [^java.nio.file.Path p (iterator-seq (.iterator stream))]
        (when (Files/isRegularFile p (make-array LinkOption 0))
          (let [rel (.relativize src-path p)
                target (.resolve dst-path rel)]
            (Files/createDirectories (.getParent target)
                                     (make-array FileAttribute 0))
            (Files/copy p target (make-array java.nio.file.CopyOption 0))))))))

(defn build-candidate-workspace!
  "Construct the candidate evaluation workspace under `staging-root`
  (Global Constraints 11, 23).

  A fresh workspace directory is created under the candidate staging
  root — NEVER under the evals tree — and ONLY the Evolution dataset
  is mounted into it (as evals/evolution, mirroring the repository
  layout). The Selection and Audit datasets are never staged into a
  workspace: their directories do not exist on disk under the
  workspace and do not appear in the returned listing. The workspace
  carries a README.md manifest naming its staging root, so any
  consumer can verify provenance.

  Returns {:workspace/root <path> :workspace/entries [rel-path ...]}.

  Typed errors: :dataset/workspace-invalid (:reason :staging-not-a-dir
  :evolution-root-missing)."
  ([staging-root] (build-candidate-workspace! staging-root dataset-roots))
  ([staging-root roots]
   (let [staging (Paths/get staging-root (make-array String 0))]
     (when-not (Files/isDirectory staging (make-array LinkOption 0))
       (throw (err/error :dataset/workspace-invalid
                         "candidate staging root is not a directory"
                         {:reason :staging-not-a-dir :staging-root staging-root})))
     (let [evo-root (dataset-root :evals/evolution roots)
           evo-path (Paths/get evo-root (make-array String 0))]
       (when-not (Files/isDirectory evo-path (make-array LinkOption 0))
         (throw (err/error :dataset/workspace-invalid
                           "evolution dataset root is missing"
                           {:reason :evolution-root-missing :root evo-root})))
       (let [ws (Files/createTempDirectory staging "workspace-"
                                          (make-array FileAttribute 0))
             ws-root (str ws)
             ws-evals (str ws java.io.File/separator "evals")
             ws-evo (str ws-evals java.io.File/separator "evolution")]
         (Files/createDirectories (Paths/get ws-evo (make-array String 0))
                                  (make-array FileAttribute 0))
         (copy-tree! evo-root ws-evo)
         (write-file! (str ws-root java.io.File/separator "README.md")
                      (str "# Candidate evaluation workspace\n\n"
                           "Staging root: " staging-root "\n"
                           "This workspace mounts the Evolution dataset only "
                           "(evals/evolution). The Selection and Audit datasets "
                           "are never staged into candidate workspaces.\n"))
         {:workspace/root ws-root
          :workspace/entries (workspace-entries ws-root)})))))

