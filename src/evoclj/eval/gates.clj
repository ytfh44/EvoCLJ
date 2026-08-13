(ns evoclj.eval.gates
  "Candidate evaluation gates G0–G3 (Task 8.2).

  The four gates are the deterministic, policy-first front door of
  candidate evaluation. Each gate is a callable that returns the
  NORMATIVE gate result map:

      {:gate/id     :G0-parse | :G1-schema-abi | :G2-static-policy
                    | :G3-deterministic-suites
       :status      :pass | :fail | :error
       :hard?       true
       :details-ref <artifact ref string or nil>
       :duration-ms <long>}

  - :status :pass — the gate's check found no violation.
  - :status :fail — a DETERMINISTIC rejection: a typed ExceptionInfo
    (load/compile/schema/topology/policy errors) or a findings value
    returned by a check. The details are persisted and referenced.
  - :status :error — an unexpected (non-ExceptionInfo) throwable; the
    gate is still hard and still stops the pipeline.

  All four gates are :hard? true: a non-pass stops later effectful
  gates (orchestration in Task 8.7 uses run-gates-until-hard-failure).
  Details are persisted through the context's :store-details! fn
  (default: a content-hash ref over the pr-str of the details, per
  Global Constraint 21 — the v0 pipeline keeps the results vector as
  the persistence unit and the refs point at the detail bodies).

  G0 (parse): loads the candidate bundle FROM SCRATCH with
  evoclj.genome.load/load-genome and compiles it with
  evoclj.compiler.core/compile-genome on the candidate's own files —
  never a cached Mutator claim (Global Constraints 4, 6). The program
  registry (Task 2.3 choice (a)) is supplied by the context resolver.

  G1 (schema + ABI): revalidates the manifest schema explicitly and
  requires the candidate's :abi to equal the kernel's expected ABI.

  G2 (static policy): the candidate's own files are scanned with the
  protected-path rules of evoclj.evolution.mutation (any kernel-root,
  eval-root, or capability-root file rejects — Global Constraints 12,
  19); :capabilities/requested must be a SUBSET of the parent's (no
  new capabilities); every program source is scanned with
  evoclj.compiler.program/policy-violation; the topology is
  re-compiled by evoclj.compiler.topology/compile-topology; and the
  eval-root file check doubles as the evaluator-mutation assertion
  (the candidate's own mutation ops are already validated by Task 7.x;
  what G2 asserts is that no eval-root files exist in the candidate
  Genome at all).

  G3 (deterministic suites): runs the suites registered in
  evoclj.eval.static (kernel-side, NOT genome-mutable) against the
  candidate's loaded data in a FRESH temp workspace (Global
  Constraint 23). A suite failure is recorded as a :fail gate with the
  per-suite findings; a throwing suite is recorded as a per-suite
  :error.

  Context contract (validated by every gate):

      {:candidate/root     <path or string>        ; the candidate bundle dir
       :provider/catalog   <provider catalog map>  ; for compile-genome
       :kernel/abi         {:kernel N :genome N :intent N :tool N}
       :parent/capabilities #{keyword ...}         ; parent's :capabilities/requested
       :programs           (fn [loaded] [descriptors]) | [descriptors] ; default []
       :store-details!     (fn [details] ref)      ; default content-hash ref
       :workspace/root     <staging dir>           ; optional; G3 workspace parent}

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/gate-context-invalid from validate-context!; every gate
  converts typed failures into :fail results with persisted details."
  (:require [clojure.edn :as edn]
            [evoclj.kernel.error :as err]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.schema :as schema]
            [evoclj.compiler.core :as compiler]
            [evoclj.compiler.program :as program]
            [evoclj.compiler.topology :as topology]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.eval.static :as static])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- context contract ------------------------------------------------------

(def ^:private required-context-keys
  "Keys every gate needs. :programs, :store-details!, and
  :workspace/root are optional and defaulted."
  [:candidate/root :provider/catalog :kernel/abi :parent/capabilities])

(defn- validate-context!
  [ctx]
  (when-not (map? ctx)
    (throw (err/error :eval/gate-context-invalid
                      "gate context must be a map"
                      {:reason :not-a-map :value (err/sanitize ctx)})))
  (doseq [k required-context-keys]
    (when-not (contains? ctx k)
      (throw (err/error :eval/gate-context-invalid
                        "gate context is missing a required key"
                        {:reason :missing-key :key k})))))

(defn- program-registry
  "The candidate's program descriptor registry: the context's
  :programs value, called with the loaded genome when it is a fn,
  returned as-is when it is a vector, and empty by default."
  [ctx loaded]
  (let [p (:programs ctx)]
    (cond
      (fn? p) (p loaded)
      (nil? p) []
      :else p)))

(defn- default-store!
  "The default details store: a content-hash artifact ref over the
  deterministic serialization of the details (Global Constraint 21)."
  [details]
  (hash/text-digest (pr-str details)))

;; --- the result wrapper ----------------------------------------------------

(defn- run-wrapped
  "Run one raw gate check inside the normative result envelope.

  `check` receives the validated context and returns nil on pass or a
  details value on :fail. A typed ExceptionInfo (deterministic
  rejection) becomes :fail with its error data; any other throwable
  becomes :error. Details are persisted through :store-details! and
  referenced by the returned ref."
  [gate-id check ctx]
  (validate-context! ctx)
  (let [t0 (System/nanoTime)
        store (or (:store-details! ctx) default-store!)
        finish (fn [status details]
                 {:gate/id gate-id
                  :status status
                  :hard? true
                  :details-ref (when details (store details))
                  :duration-ms (quot (- (System/nanoTime) t0) 1000000)})]
    (try
      (let [details (check ctx)]
        (if details
          (finish :fail details)
          (finish :pass nil)))
      (catch clojure.lang.ExceptionInfo e
        (finish :fail (err/error-data e)))
      (catch Throwable t
        (finish :error {:error/message (.getMessage t)
                        :error/class (.getName (.getClass t))})))))

;; --- G0: parse from scratch ------------------------------------------------

(defn- check-g0
  "Load the candidate bundle from disk and compile it from scratch —
  no cached Mutator claims. Any load/compile rejection is a typed
  ExceptionInfo and becomes a :fail through the wrapper."
  [ctx]
  (let [loaded (load/load-genome (:candidate/root ctx))
        registry (program-registry ctx loaded)]
    (compiler/compile-genome (assoc loaded :programs registry)
                             (:provider/catalog ctx))
    nil))

(defn g0-parse
  "G0: parse gate. Re-loads the candidate Genome from scratch and
  re-compiles it (evoclj.genome.load/load-genome +
  evoclj.compiler.core/compile-genome)."
  [ctx]
  (run-wrapped :G0-parse check-g0 ctx))

;; --- G1: schema + ABI ------------------------------------------------------

(defn- check-g1
  "Revalidate the manifest schema and require ABI equality against the
  kernel's expected ABI."
  [ctx]
  (let [loaded (load/load-genome (:candidate/root ctx))
        manifest (:manifest loaded)
        _ (schema/validate-manifest manifest)
        expected (:kernel/abi ctx)]
    (when-not (= expected (:abi manifest))
      {:check :abi-mismatch
       :expected expected
       :candidate (:abi manifest)})))

(defn g1-schema-abi
  "G1: schema/ABI gate. Revalidates the manifest schema and the ABI
  compatibility (:abi equality against the kernel's expected ABI)."
  [ctx]
  (run-wrapped :G1-schema-abi check-g1 ctx))

;; --- G2: static policy -----------------------------------------------------

(def ^:private content-protection-reasons
  "The protected-path reasons that reject a candidate FILE (as opposed
  to a mutation op targeting a file). The candidate's own manifest.edn
  (:kernel-file) and its declared evolution module (:evolution-root)
  are legitimate parts of every bundle; kernel-root, eval-root, and
  capability-root files are never (Global Constraints 12, 19)."
  #{:kernel-root :eval-root :capability-root})

(defn- protected-path-findings
  "Candidate files that live on a kernel-protected path, using the
  same protected-path rules as evolution.mutation."
  [manifest loaded]
  (into []
        (keep (fn [path]
                (let [reason (mutation/protected-path-reason manifest path)]
                  (when (contains? content-protection-reasons reason)
                    {:check :protected-path :path path :reason reason}))))
        (sort (keys (:files loaded)))))

(defn- capability-findings
  "Requested capability expansion: the candidate's
  :capabilities/requested must be a SUBSET of the parent's — no new
  capabilities (Global Constraint 9: adding a visible action must not
  grant authority)."
  [ctx manifest]
  (let [parent (set (:parent/capabilities ctx))
        candidate (set (:capabilities/requested manifest))
        added (vec (sort (remove parent candidate)))]
    (when (seq added)
      [{:check :capability-expansion
        :parent parent
        :candidate candidate
        :added added}])))

(defn- program-surface-findings
  "Forbidden program surfaces: every program in the candidate's
  registry is scanned with compiler.program's static policy scan. An
  unreadable program file and an unparseable source are violations
  too."
  [ctx loaded]
  (into []
        (keep (fn [descriptor]
                (let [pid (:program/id descriptor)
                      file (:file descriptor)
                      payload (get-in loaded [:files file])]
                  (cond
                    (nil? payload)
                    {:check :forbidden-program-surface
                     :program/id pid :file file :reason :file-missing}

                    :else
                    (let [source (String. ^bytes (byte-array (:bytes payload))
                                          StandardCharsets/UTF_8)
                          violation (try (program/policy-violation source)
                                         (catch Exception e
                                           {:reason :unparseable
                                            :message (.getMessage e)}))]
                      (when violation
                        {:check :forbidden-program-surface
                         :program/id pid :file file :violation violation}))))))
        (program-registry ctx loaded)))

(defn- topology-findings
  "Invalid topology: re-compile the candidate's declared topology
  module with compile-topology. Invalid topologies are already
  rejected at G0; this is the static-policy re-validation."
  [loaded]
  (let [path (get-in loaded [:manifest :modules :topology])
        payload (get-in loaded [:files path])]
    (if (nil? payload)
      [{:check :invalid-topology :reason :topology-file-missing :file path}]
      (let [source (String. ^bytes (byte-array (:bytes payload))
                            StandardCharsets/UTF_8)]
        (try
          (topology/compile-topology (edn/read-string source))
          []
          (catch Exception e
            [{:check :invalid-topology
              :error/type (or (:error/type (ex-data e)) :topology/invalid)
              :message (.getMessage e)}]))))))

(defn- check-g2
  "Static policy scan of the candidate's OWN files: protected paths,
  requested-capability subset, forbidden program surfaces, topology
  validity, and (via the :eval-root protected-path rule) the absence
  of evaluator-mutation targets."
  [ctx]
  (let [loaded (load/load-genome (:candidate/root ctx))
        manifest (:manifest loaded)
        findings (vec (concat (protected-path-findings manifest loaded)
                              (capability-findings ctx manifest)
                              (program-surface-findings ctx loaded)
                              (topology-findings loaded)))]
    (when (seq findings) findings)))

(defn g2-static-policy
  "G2: static policy gate. Protected paths, requested capability
  expansion, forbidden program surfaces, invalid topology, and
  evaluator-mutation attempts."
  [ctx]
  (run-wrapped :G2-static-policy check-g2 ctx))

;; --- G3: deterministic suites ----------------------------------------------

(defn- candidate-workspace
  "Materialize the candidate's loaded files into a FRESH temp
  workspace (Global Constraint 23 — never the production generation).
  Returns {:workspace/root <path> :workspace/entries [rel-path ...]}."
  [ctx loaded]
  (let [base (or (:workspace/root ctx) (System/getProperty "java.io.tmpdir"))
        root (Files/createTempDirectory (Paths/get base (make-array String 0))
                                        "candidate-ws-"
                                        (make-array FileAttribute 0))
        root-path (str root)]
    (doseq [[path {:keys [bytes]}] (:files loaded)]
      (let [target (.resolve root (Paths/get path (make-array String 0)))]
        (Files/createDirectories (.getParent target)
                                 (make-array FileAttribute 0))
        (Files/write target (byte-array bytes)
                     (make-array java.nio.file.OpenOption 0))))
    {:workspace/root root-path
     :workspace/entries (vec (sort (keys (:files loaded))))}))

(defn- check-g3
  "Run every registered deterministic suite (evoclj.eval.static) in a
  fresh candidate workspace against the candidate's loaded data. Each
  suite returns nil (pass) or a failure map; a throwing suite is
  recorded as a per-suite :error."
  [ctx]
  (let [loaded (load/load-genome (:candidate/root ctx))
        ws (candidate-workspace ctx loaded)
        candidate {:candidate/loaded loaded
                   :candidate/root (:candidate/root ctx)
                   :workspace ws}
        failures (into []
                       (keep (fn [suite]
                               (try
                                 (let [outcome ((:check suite) candidate)]
                                   (when outcome
                                     (assoc outcome :suite/id (:suite/id suite))))
                                 (catch Throwable t
                                   {:suite/id (:suite/id suite)
                                    :status :error
                                    :error/message (.getMessage t)
                                    :error/class (.getName (.getClass t))}))))
                       (static/registered-suites))]
    (when (seq failures) failures)))

(defn g3-deterministic-suites
  "G3: deterministic suites gate. Runs the registered unit/property
  suites (kernel-side registry, NOT genome-mutable) in a fresh
  candidate workspace."
  [ctx]
  (run-wrapped :G3-deterministic-suites check-g3 ctx))

;; --- orchestration ---------------------------------------------------------

(def gate-order
  "The gate execution order. Every gate is :hard? true — a non-pass
  result stops the pipeline before any later effectful gate runs
  (Task 8.7 consumes this)."
  [{:gate/id :G0-parse :run g0-parse}
   {:gate/id :G1-schema-abi :run g1-schema-abi}
   {:gate/id :G2-static-policy :run g2-static-policy}
   {:gate/id :G3-deterministic-suites :run g3-deterministic-suites}])

(defn run-gates-until-hard-failure
  "Run the four gates in order until a hard failure.

  Returns {:results [<gate result> ...] :stopped? bool}: :results
  holds every gate result produced so far and :stopped? is true when a
  non-pass gate ended the run (its result is the last element). A
  clean run produces four :pass results and :stopped? false."
  [ctx]
  (loop [steps gate-order
         results []]
    (if-let [{:keys [run]} (first steps)]
      (let [result (run ctx)
            results' (conj results result)]
        (if (= :pass (:status result))
          (recur (rest steps) results')
          {:results results' :stopped? true}))
      {:results results :stopped? false})))
