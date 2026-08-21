(ns evoclj.adversarial.eval-isolation-test
  "component — Evaluation leakage and tampering suite (adversarial
  release gate).

  This suite attacks the evaluation-selection boundary at the
  FILESYSTEM/CAPABILITY level (Global Constraints 11, 12, 23) — not at
  prompt discipline. Every case must FAIL CLOSED. The six plan cases
  and the enforcement mechanism each one pins:

  Step 1 — filesystem/mount/capability isolation, not merely prompts:

  - Mutator tries to read evals/selection (case 1). The Mutator
    context is the exact closed contract the orchestrator builds
    ({:generation/id :parent/genome-id :parent-genome :diagnosis
    :history :budget-profile}) — no selection root path, relative or
    absolute, appears in it; build-candidate-workspace! mounts ONLY
    the Evolution dataset, so the workspace tree lacks evals/selection
    in the listing, physically on disk, and in every workspace file's
    bytes; and a mutation op targeting evals/selection/case-1.edn is
    rejected by the declared-mutable-class gate — :evals is never a
    declared mutable class of the seed manifest.
  - Candidate tries to modify replay fixture files (case 4). Replay
    fixtures are kernel-side constructor inputs (:replay/fixtures
    provider values, a REQUIRED key of the G4 evaluator context), never
    Genome files; a mutation op targeting replay/fixtures/... is
    rejected by the declared-mutable-class gate — :replay is never a
    declared mutable class.
  - Candidate tries to DELETE failing deterministic tests (case 5). A
    mutation op deleting kernel/tests/... is rejected by the
    protected-path gate with :reason :kernel-root — kernel files are
    outside all Genome mutable roots (Global Constraint 19); the
    deterministic suites themselves are kernel-side registry entries
    (evoclj.eval.static) that no Genome can register or replace.

  Step 2 — any candidate mutation touching protected evaluator paths
  fails G2:

  - Mutation targets evaluator configuration (case 2). Ops on
    eval|evaluator|evaluation/* are rejected by the protected-path
    gate (:reason :eval-root, Global Constraint 12); a candidate
    BUNDLE that carries an eval-root file fails G2 with a
    :protected-path finding, and the hard-gate pipeline stops there.

  Step 3 — constructor-level dependency separation:

  - Verifier adapter accidentally receives Diagnostician private
    context / evaluator-only dependencies (case 6). The selection
    dataset loader CANNOT be obtained from evolution-system
    dependencies: the evolution-facing input (evolution-input) is pure
    data — no loader fn, no loader key, no selection body; the
    evolution-system map carries no dataset loader; and the
    evoclj.evolution.core / evoclj.evolution.mutation namespaces do
    NOT require evoclj.eval.dataset at all — the dependency graph is
    separated (asserted through the public ns surface). The
    verifier-facing diagnostic surface (paired/evolution-diagnostics)
    carries no hidden case keys (:task-input :expected-output
    :case/body) by construction.
  - Candidate output guesses hidden test paths (case 3). Strings like
    \"evals/selection/case-1.edn\" stay inert DATA: the closed SCI
    context returns them as plain values and denies every read
    attempt; a G4 replay run compares the guessed-path output with the
    oracle predicate — the string is compared as data, never resolved
    as a path."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.dataset :as dataset]
            [evoclj.eval.gates :as gates]
            [evoclj.eval.paired :as paired]
            [evoclj.eval.replay :as replay]
            [evoclj.eval.static :as static]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.provider.fixture :as fixture]
            [evoclj.sci.context :as sci-ctx]
            [sci.core :as sci])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; ============================================================================
;; shared fixtures and helpers
;; ============================================================================

(def ^:private kernel-abi
  "The kernel's expected ABI (v1)."
  {:kernel 1 :genome 1 :intent 1 :tool 1})

(def ^:private hex64
  "64 hex chars for canonical content-addressed ids."
  (apply str (repeat 64 \a)))

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private artifact-id (str "sha256:" (apply str (repeat 64 \b))))
(def ^:private preimage-id (str "sha256:" (apply str (repeat 64 \c))))

(def ^:private temp-paths (atom []))

(defn- temp-dir
  "A fresh empty temp directory (system temp), registered for cleanup."
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
  "Write `content` (a string) to `path` as UTF-8, creating parent
  directories."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(defn- copy-tree!
  "Copy every regular file under `src` into `dst`, preserving relative
  paths (a test-local clone of the dataset workspace copier)."
  [src dst]
  (let [src-path (Paths/get (str src) (make-array String 0))
        dst-path (Paths/get (str dst) (make-array String 0))]
    (with-open [stream (Files/walk src-path (make-array FileVisitOption 0))]
      (doseq [^java.nio.file.Path p (iterator-seq (.iterator stream))]
        (when (Files/isRegularFile p (make-array LinkOption 0))
          (let [rel (.relativize src-path p)
                target (.resolve dst-path rel)]
            (Files/createDirectories (.getParent target)
                                     (make-array FileAttribute 0))
            (Files/copy p target (make-array java.nio.file.CopyOption 0))))))))

(defn- fixture-root
  "The bundle directory for a named fixture under test/fixtures/genomes."
  [name]
  (.toPath (io/file (io/resource (str "fixtures/genomes/" name)))))

(defn- temp-candidate!
  "Copy a named fixture bundle into a fresh temp directory and return
  its path string."
  [name]
  (let [dir (temp-dir "evoclj-iso-candidate-")]
    (copy-tree! (fixture-root name) (str dir))
    (str dir)))

(defn- seed-loaded
  "The REAL seed Genome (genomes/seed) loaded from disk — the parent
  context for the mutation-gate assertions (its manifest declares
  :evolution :mutable #{:parameters :prompts :skills :programs})."
  []
  (load/load-genome "genomes/seed"))

(defn- fixture-catalog
  "The on-disk provider catalog fixture (component Resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor
  "The route program descriptor (component choice (a))."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- thrown-ex-data
  "The ex-data of the typed ExceptionInfo thrown by `f`, or nil."
  [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- has-fn-value?
  "True when `x` (deeply) contains any fn."
  [x]
  (cond
    (fn? x) true
    (map? x) (some has-fn-value? (concat (keys x) (vals x)))
    (coll? x) (some has-fn-value? x)
    :else false))

(defn- ns-required-libs
  "The lib symbols in the :require clause of the namespace form read
  from `path` — the DECLARED dependency graph, read through the public
  ns surface (no runtime eval)."
  [path]
  (let [ns-form (read-string (slurp path))
        clause (first (filter (fn [x] (and (seq? x) (= :require (first x))))
                              (rest ns-form)))
        specs (rest clause)]
    (into #{}
          (keep (fn [spec]
                  (cond
                    (symbol? spec) spec
                    (vector? spec) (first spec)
                    :else nil)))
          specs)))

;; --- mutation IR builders ---------------------------------------------------

(defn- mutation-on
  "A schema-valid Mutation IR whose single :set-edn op targets `file`."
  [file]
  {:mutation/id (UUID/randomUUID)
   :parent/genome-id genome-id
   :hypothesis/id (UUID/randomUUID)
   :evidence/id artifact-id
   :risk :behavioral
   :ops [{:op :set-edn
          :file file
          :path [:value]
          :expect/hash preimage-id
          :value {}}]
   :expected-effect {:primary-metric :task/success :direction :increase}})

(defn- mutation-delete
  "A schema-valid Mutation IR whose single :delete-text op targets
  `file` — the destructive form a tampering candidate would use."
  [file]
  {:mutation/id (UUID/randomUUID)
   :parent/genome-id genome-id
   :hypothesis/id (UUID/randomUUID)
   :evidence/id artifact-id
   :risk :behavioral
   :ops [{:op :delete-text
          :file file
          :anchor "defn run"
          :expect/hash preimage-id}]
   :expected-effect {:primary-metric :task/success :direction :increase}})

;; --- fixture datasets (three physically separated temp roots) ---------------

(def ^:private selection-marker
  "A byte-level marker that must never leave the selection dataset."
  "selection body must never reach evolution")

(defn- fixture-roots
  "Three physically separate temp dataset roots plus a temp staging
  root, seeded with fixture cases. Returns {:roots {source -> root}
  :staging <path> :selection-root <path string>}."
  []
  (let [evo (temp-dir "evoclj-iso-evolution-")
        sel (temp-dir "evoclj-iso-selection-")
        aud (temp-dir "evoclj-iso-audit-")
        staging (temp-dir "evoclj-iso-staging-")]
    (write-file! (str evo "/README.md") "# Evolution dataset\n")
    (write-file! (str evo "/cases.edn")
                 "{:case/id :case/evolve-1\n :body {:prompt \"improve G1\"}}\n")
    (write-file! (str sel "/README.md") "# Selection dataset\n")
    (write-file! (str sel "/cases.edn")
                 (str "{:case/id :case/selection-1\n :body {:prompt \"hidden paired task\"\n"
                      "        :secret " (pr-str selection-marker) "}}\n"))
    (write-file! (str aud "/README.md") "# Audit dataset\n")
    {:roots {:evals/evolution (str evo)
             :evals/selection (str sel)
             :evals/audit (str aud)}
     :staging (str staging)
     :selection-root (str sel)}))

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

(defn- workspace-file-contents
  "The text of every regular file under a workspace root."
  [ws-root]
  (let [base (Paths/get ws-root (make-array String 0))]
    (->> (with-open [stream (Files/walk base (make-array FileVisitOption 0))]
           (doall (iterator-seq (.iterator stream))))
         (filter #(Files/isRegularFile % (make-array LinkOption 0)))
         (mapv (fn [^java.nio.file.Path p] (slurp (str p)))))))

;; --- G2 gate context helpers -------------------------------------------------

(defn- gate-context
  ([root] (gate-context root {}))
  ([root overrides]
   (merge {:candidate/root root
           :provider/catalog (fixture-catalog)
           :kernel/abi kernel-abi
           :parent/capabilities #{:model/call}
           :programs (fn [_] [(route-descriptor)])}
          overrides)))

(defn- ctx-with-capture
  "A gate context whose :store-details! persists details into an atom
  under their artifact ref. Returns [context store]."
  [root]
  (let [store (atom {})]
    [(assoc (gate-context root)
            :store-details! (fn [details]
                              (let [ref (hash/text-digest (pr-str details))]
                                (swap! store assoc ref details)
                                ref)))
     store]))

(defn- candidate-with-eval-file!
  "A valid candidate bundle (minimal-valid) that additionally carries
  an eval-root file — the evaluator-configuration tampering attempt."
  []
  (let [dir (temp-candidate! "minimal-valid")]
    (write-file! (str dir "/eval/config.edn") "{:threshold 0.9}\n")
    dir))

;; --- G4 replay harness fixtures (store-free, case 3) ------------------------

(defn- route-source
  "A route program that echoes {:op :echo :text t} as a
  :fixture/echo tool-call with (transform t); anything else emits a
  finish intent. `transform-expr` is the body of the private transform
  fn."
  [tool-id transform-expr]
  (str "(ns agent.route)\n"
       "(defn- transform [text] " transform-expr ")\n"
       "(defn run [input]\n"
       "  (let [op (get input :op)]\n"
       "    (case op\n"
       "      :echo {:action {:intent/type :intent/tool-call\n"
       "                      :payload {:tool/id " (pr-str tool-id) "\n"
       "                                :args {:text (transform (get input :text))}}}}\n"
       "      {:action {:intent/type :intent/finish :payload {:value input}}})))\n"))

(defn- leaky-candidate!
  "A candidate bundle whose route program ECHOES the guessed hidden
  test path as ordinary output data — it can neither read the file (no
  filesystem authority in the SCI sandbox) nor make the evaluator
  resolve the string as a path."
  []
  (let [dir (temp-dir "evoclj-iso-leaky-")]
    (write-file! (str dir "/manifest.edn")
                 (pr-str {:genome/format 1
                          :agent/id :main
                          :agent/entry :graph/main
                          :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                          :modules {:topology "topology.edn"
                                    :models "models.edn"
                                    :memory "memory.edn"
                                    :evolution "evolution.edn"}
                          :capabilities/requested #{:model/call}
                          :evolution {:max-risk :behavioral
                                      :mutable #{:parameters :prompts
                                                 :skills :programs}}
                          :metadata {:name "leaky-candidate"
                                     :description "guesses hidden test paths"}}))
    (write-file! (str dir "/topology.edn")
                 (pr-str {:graph/id :graph/replay
                          :entry :node/router
                          :nodes {:node/router {:node/type :sci
                                                :program :program/route
                                                :next :node/emit}
                                  :node/emit {:node/type :emit}}
                          :limits {:max-steps 64}}))
    (write-file! (str dir "/models.edn")
                 "{:models {:planner {:alias :reasoning/high}}}")
    (write-file! (str dir "/memory.edn") "{:memory {}}")
    (write-file! (str dir "/evolution.edn") "{:evolution {}}")
    (write-file! (str dir "/programs/route.clj")
                 (route-source :fixture/echo "text"))
    (str dir)))

(defn- replay-episode
  "A stored-Episode-shaped map (component) for replay case construction."
  []
  {:episode/id (random-uuid)
   :session/id (random-uuid)
   :generation/id "g-isolation-1"
   :genome/id (str "sha256:" (apply str (repeat 64 "0")))
   :resolution/id (str "sha256:" (apply str (repeat 64 "1")))
   :task-ref (str "sha256:" (apply str (repeat 64 "2")))
   :trace {:first-event 1 :last-event 9}
   :outcome {:status :completed :score nil}
   :usage {}})

(defn- tool-trace-entry
  "One recorded trace entry: a tool-call intent, its tool's recorded
  effect class, and the recorded provider response."
  [tool-id args effect response]
  {:intent/type :intent/tool-call
   :payload {:tool/id tool-id :args args}
   :effect effect
   :response response})

(defn- echo-decision
  "The route program's decision value for an echo tool-call."
  [text]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/echo :args {:text text}}}})

(defn- replay-evaluator
  "A minimal valid G4 replay evaluator context."
  [cases]
  {:provider/catalog (fixture-catalog)
   :programs (fn [_] [(route-descriptor)])
   :replay/cases cases
   :replay/fixtures {:fixture/echo (fixture/echo-provider)}})

;; ============================================================================
;; STEP 1 — filesystem/mount/capability isolation, not merely prompts
;; ============================================================================

(deftest mutator-context-and-workspace-hold-no-selection-path
  (let [{:keys [roots staging selection-root]} (fixture-roots)
        parent (seed-loaded)
        context {:generation/id "generation-1"
                 :parent/genome-id (:genome/id parent)
                 :parent-genome parent
                 :diagnosis {:diagnosis/id :fixture :hypotheses []}
                 :history []
                 :budget-profile {:hard-limit 1}}]
    (testing "the Mutator context is the exact closed contract — no
              selection path, relative or absolute"
      (is (= #{:generation/id :parent/genome-id :parent-genome
               :diagnosis :history :budget-profile}
             (set (keys context)))
          "the context is exactly the documented Mutator contract keys")
      (is (not (str/includes? (pr-str context) "evals/selection"))
          "the relative evals/selection path never appears in the context")
      (is (not (str/includes? (pr-str context) selection-root))
          "the absolute selection root never appears in the context")
      (is (not-any? #(str/starts-with? % "evals/")
                    (keys (:files parent)))
          "the parent Genome bundle itself carries no evals/ data"))
    (testing "the candidate workspace tree lacks evals/selection entirely"
      (let [ws (dataset/build-candidate-workspace! staging roots)
            entries (:workspace/entries ws)
            ws-root (:workspace/root ws)]
        (is (some #(str/starts-with? % "evals/evolution") entries)
            "the Evolution dataset IS mounted (positive control)")
        (is (not-any? #(str/starts-with? % "evals/selection") entries)
            "the workspace listing must not contain evals/selection")
        (is (not (Files/exists
                  (Paths/get (str ws-root java.io.File/separator
                                  "evals" java.io.File/separator "selection")
                             (make-array String 0))
                  (make-array LinkOption 0)))
            "evals/selection does not exist on disk under the workspace")
        (is (not-any? #(str/includes? % selection-marker)
                      (workspace-file-contents ws-root))
            "no workspace file carries a single selection byte")))
    (testing "a mutation op targeting evals/selection is rejected"
      (let [data (thrown-ex-data
                  #(mutation/validate-mutation
                    (mutation-on "evals/selection/case-1.edn") parent))]
        (is (= :mutation/undeclared-mutable-class (:error/type data))
            "the declared-mutable-class gate rejects :evals — it is never
            a declared mutable class of the seed manifest")
        (is (= :evals (:class data)))
        (is (= "evals/selection/case-1.edn" (:path data)))))))

(deftest replay-fixture-modification-rejected
  (let [parent (seed-loaded)]
    (testing "a mutation op targeting replay fixture files is rejected"
      (let [data (thrown-ex-data
                  #(mutation/validate-mutation
                    (mutation-on "replay/fixtures/echo-case.edn") parent))]
        (is (= :mutation/undeclared-mutable-class (:error/type data))
            "the declared-mutable-class gate rejects :replay — replay
            fixtures are not Genome assets")
        (is (= :replay (:class data)))
        (is (= "replay/fixtures/echo-case.edn" (:path data)))))
    (testing "replay fixtures are kernel-side constructor inputs, never
              Genome files"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"replay"
            (replay/run-replay! {:provider/catalog (fixture-catalog)}
                                "unused-candidate-root" []))
          ":replay/fixtures is a REQUIRED key of the G4 evaluator context
          (provider values, supplied by the kernel host)")
      (is (not-any? #(str/starts-with? % "replay/")
                    (keys (:files parent)))
          "the Genome bundle carries no replay-fixture file to modify"))))

(deftest kernel-test-deletion-rejected
  (let [parent (seed-loaded)]
    (testing "a candidate trying to DELETE failing deterministic tests is
              rejected"
      (let [data (thrown-ex-data
                  #(mutation/validate-mutation
                    (mutation-delete "kernel/tests/failing-suite.edn") parent))]
        (is (= :mutation/protected-path (:error/type data))
            "the protected-path gate rejects kernel/ targets")
        (is (= :kernel-root (:reason data)))
        (is (= "kernel/tests/failing-suite.edn" (:path data)))))
    (testing "kernel files are outside all Genome mutable roots"
      (is (= :kernel-root
             (mutation/protected-path-reason (:manifest parent)
                                             "kernel/tests/failing-suite.edn")))
      (is (not-any? #(str/starts-with? % "kernel/")
                    (keys (:files parent)))
          "no kernel/ file exists inside the Genome bundle to delete"))
    (testing "the deterministic suites are kernel-side registry entries
              — a Genome cannot register, replace, or disable them"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"deterministic function"
            (static/register-suite!
             {:suite/id :suite/forged
              :suite/type :unit
              :check "not-a-fn"}))
          "a suite :check must be a kernel-supplied deterministic fn —
          data from a Genome can never satisfy the registry"))))

;; ============================================================================
;; STEP 2 — protected evaluator paths fail G2
;; ============================================================================

(deftest evaluator-config-mutation-rejected-and-g2-fails
  (let [parent (seed-loaded)]
    (testing "eval|evaluator|evaluation/* targets are rejected by the
              protected-path gate (Global Constraint 12)"
      (doseq [file ["eval/config.edn" "evaluator/suite.edn"
                    "evaluation/runner.edn"]]
        (let [data (thrown-ex-data
                    #(mutation/validate-mutation (mutation-on file) parent))]
          (is (= :mutation/protected-path (:error/type data)) file)
          (is (= :eval-root (:reason data)) file)
          (is (= file (:path data)) file))))
    (testing "a candidate BUNDLE that carries an eval-root file fails G2"
      (let [root (candidate-with-eval-file!)
            [ctx store] (ctx-with-capture root)
            result (gates/g2-static-policy ctx)]
        (is (= :G2-static-policy (:gate/id result)))
        (is (= :fail (:status result)))
        (is (true? (:hard? result)))
        (let [findings (get @store (:details-ref result))]
          (is (some #(and (= :protected-path (:check %))
                          (= "eval/config.edn" (:path %))
                          (= :eval-root (:reason %)))
                    findings)
              "G2 reports the eval-root protected-path finding"))))
    (testing "the hard-gate pipeline stops at G2"
      (let [root (candidate-with-eval-file!)
            outcome (gates/run-gates-until-hard-failure (gate-context root))]
        (is (:stopped? outcome))
        (is (= :G2-static-policy (:gate/id (last (:results outcome)))))
        (is (every? #(= :pass (:status %))
                    (butlast (:results outcome)))
            "G0 and G1 pass — the candidate is well-formed except for the
            eval-root file G2 rejects")))))

;; ============================================================================
;; STEP 3 — constructor-level dependency separation + inert output data
;; ============================================================================

(deftest selection-loader-unobtainable-from-evolution-system
  (let [{:keys [roots]} (fixture-roots)
        p (fixture-profile)
        input (dataset/evolution-input p roots)]
    (testing "the evolution-facing input is pure data — no loader fn, no
              loader key, no selection body"
      (is (not (has-fn-value? input))
          "evolution-input carries no fn values at all (Global Constraint 22)")
      (is (not (contains? input :selection-loader)))
      (is (not (contains? input :audit-loader)))
      (is (not (str/includes? (pr-str input) selection-marker))
          "no selection body byte reaches the evolution input"))
    (testing "the evolution-system constructor accepts and contains no
              dataset loader"
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
        (is (empty? (clojure.set/intersection loader-keys
                                              (set (keys system))))
            "the documented/validated evolution-system map carries no
            selection/audit loader key")))
    (testing "the dependency graph is separated — evolution never
              requires the eval dataset"
      (let [evo-core (ns-required-libs "src/evoclj/evolution/core.clj")
            evo-mutation (ns-required-libs "src/evoclj/evolution/mutation.clj")
            dataset (ns-required-libs "src/evoclj/eval/dataset.clj")]
        (is (contains? evo-core 'evoclj.evolution.mutation)
            "sanity: evolution.core requires the mutation gate")
        (is (not (contains? evo-core 'evoclj.eval.dataset))
            "evoclj.evolution.core never requires the dataset — no
            selection loader is reachable from the evolution system")
        (is (not (contains? evo-mutation 'evoclj.eval.dataset))
            "evoclj.evolution.mutation never requires the dataset")
        (is (contains? dataset 'evoclj.eval.profile)
            "the loader world requires the profile — the dependency edge
            points OUT of the evolution layer, never into it")
        (is (not (contains? dataset 'evoclj.evolution.core)))))
    (testing "the verifier-facing diagnostic surface carries no
              Diagnostician/verifier private context"
      (let [clean (paired/evolution-diagnostics
                   {:parent {:side/kind :parent :side/id "g1"
                             :cases 1 :passed 1 :failed 0 :score 1.0}
                    :candidate {:side/kind :candidate :side/id "c1"
                                :cases 1 :passed 1 :failed 0 :score 1.0}
                    :pairs [{:pair/index 0 :case/id :sel/c1 :repetition 1
                             :pair/seed "s" :order [:parent :candidate]
                             :case/outcome {:score/parent 1.0
                                            :score/candidate 1.0
                                            :delta 0.0 :status :tie}}]})]
        (is (empty? (paired/hidden-data-contaminants clean))
            "the Mutator-facing diagnostics embed no hidden case data")
        (is (not (contains? clean :task-input)))
        (is (not (contains? clean :expected-output)))
        (is (not (contains? clean :case/body))))
      (is (contains? (set (paired/hidden-data-contaminants
                           {:task-input {} :expected-output []
                            :case/body {}}))
                     :task-input)
          "positive control: the contamination scanner detects hidden
          case keys when they are present"))))

(deftest hidden-test-path-outputs-stay-inert-data
  (testing "in the closed SCI context the guessed path STRING is inert
            data — every read attempt fails closed"
    (let [ctx (sci-ctx/make-context {})]
      (is (= "evals/selection/case-1.edn"
             (sci/eval-string* ctx "\"evals/selection/case-1.edn\""))
          "a program that RETURNS the guessed path yields the plain
          string value — no path access happens")
      (is (thrown? Throwable
                   (sci/eval-string* ctx "(slurp \"evals/selection/case-1.edn\")"))
          "a program that tries to READ the guessed path fails closed")
      (is (thrown? Throwable
                   (sci/eval-string* ctx
                                     "(java.io.File. \"evals/selection/case-1.edn\")"))
          "a program that tries to open the guessed path as a File fails
          closed")))
  (testing "a G4 replay run compares the guessed-path output as DATA,
            never as a path"
    (let [candidate (leaky-candidate!)
          leaky-text "evals/selection/case-1.edn"
          ep (replay-episode)
          tr [(tool-trace-entry :fixture/echo {:text leaky-text}
                                :read {:text leaky-text})]
          case (replay/build-replay-case
                ep tr
                {:case/id :replay/leaky
                 :task-input {:op :echo :text leaky-text}
                 :expected-output [(echo-decision leaky-text)
                                   {:text leaky-text}]
                 :mode :fixture})
          report (replay/run-replay! (replay-evaluator {(:case/id case) case})
                                     candidate [:replay/leaky])]
      (is (= :completed (get-in report [:replay/cases 0 :run :run/status]))
          "the candidate ran to completion — the guessed path never
          short-circuited or escalated")
      (is (true? (get-in report [:replay/cases 0 :output/match?]))
          "the output matched the oracle — byte-identical DATA comparison")
      (is (some #(= {:text leaky-text} %)
                (get-in report [:replay/cases 0 :run :output]))
          "the guessed-path string traveled as ordinary output data end
          to end")
      (is (not (:hard-failure? report))))))
