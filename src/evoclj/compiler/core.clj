(ns evoclj.compiler.core
  "Orchestrate Genome compilation into a pure CompiledGenome and derive
  the I1 identity split (component).

  compile-genome is ORCHESTRATION ONLY: it composes the focused modules
  (evoclj.genome.load already produced the loaded Genome;
  evoclj.genome.schema/validate-manifest, evoclj.compiler.resolution/
  resolve-models, evoclj.compiler.topology/compile-topology, and
  evoclj.compiler.program/compile-program-descriptor own every
  validation rule). This namespace adds no validation logic of its own
  beyond orchestration glue: reading the two declared modules
  (topology, models) out of the immutable in-memory :files (never from
  disk, and never executed), attaching the program registry, checking
  topology program references resolve, and computing the ProgramImage
  identity.

  Identity split (I1):

    ProgramImageId (CodeImageId) = H(kernel-abi || genome-id || resolution-id)
    DeploymentId   = H(ProgramImage || bindings || authority)
    ExecutionId    = UUID per activation
    RuntimeImageId = H(ProgramImage || runtime-descriptor)
  ProgramImageId (persisted as :code/id, the historical CodeImageId key)
  identifies the ABI-compatible program and NOTHING more: identical ABI,
  Genome, and Resolution always yield identical ProgramImageId, but the
  same ProgramImageId NEVER implies the same execution semantics — a
  different kernel implementation, interpreter build, or provider
  adapter build behind the same interface versions executes the same
  program differently (SaaS model endpoints change without notice, so
  the same logical binding NEVER implies the same model function).
  Same-execution-semantics claims require the RuntimeImageId
  (runtime-image-id in this namespace), recorded on execution records,
  together with the ExecutionEnvironment observational provenance
  (evoclj.compiler.resolution/execution-environment). DeploymentId binds
  the ProgramImage to a concrete deployment's host bindings and authority
  (capability leases). ExecutionId is a fresh UUID per activation:
  two Executions with the same ProgramImage share :code/id but have
  distinct :execution/id. PhenotypeId legacy alias is removed
  (one-time break compat).
  The CompiledGenome is pure, fully serializable EDN data (Global
  Constraint 22): program descriptors carry :source/digest references
  and never the source bytes, and no :files payload or byte array
  appears in the output. Compilation performs no IO of its own.

  Error types introduced here: :compiler/invalid (:reason
  distinguishes :invalid-loaded-genome, :module-file-missing,
  :module-parse-error, :invalid-program-registry,
  :duplicate-program-id) and :compiler/program-unresolved. Errors from
  the focused modules (:genome/schema-invalid, :resolution/*,
  :topology/*, :program/*) pass through unchanged."
  (:require [clojure.edn :as edn]
            [evoclj.capability.core :as capability]
            [evoclj.kernel.error :as err]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.schema :as schema]
            [evoclj.genome.types :as types]
            [evoclj.compiler.resolution :as resolution]
            [evoclj.compiler.topology :as topology]
            [evoclj.compiler.program :as program])
  (:import (java.nio.charset StandardCharsets)))

;; --- input shape validation ------------------------------------------------

(defn- validate-loaded-genome!
  "Validate the orchestration input shape: a loaded-Genome value with a
  canonical :genome/id, an immutable :files map, and a schema-valid
  manifest (the focused evoclj.genome.schema owns the manifest rule)."
  [loaded-genome]
  (when-not (map? loaded-genome)
    (throw (err/error :compiler/invalid
                      "compile-genome expects a loaded genome map"
                      {:reason :invalid-loaded-genome
                       :value (err/sanitize loaded-genome)})))
  (when-not (types/genome-id? (:genome/id loaded-genome))
    (throw (err/error :compiler/invalid
                      "loaded genome must carry a canonical :genome/id"
                      {:reason :invalid-loaded-genome
                       :value (err/sanitize (:genome/id loaded-genome))})))
  (when-not (map? (:files loaded-genome))
    (throw (err/error :compiler/invalid
                      "loaded genome must carry a :files map"
                      {:reason :invalid-loaded-genome
                       :value (err/sanitize (:files loaded-genome))})))
  (schema/validate-manifest (:manifest loaded-genome))
  loaded-genome)

;; --- declared module decoding ---------------------------------------------

(defn- module-value
  "Decode one declared EDN module from the immutable in-memory :files
  payload (never from disk) and parse it with clojure.edn/read-string
  (never clojure.core/read-string: no reader-eval, no evaluation). The
  module must be declared by the manifest and present in :files; a
  parse failure is wrapped as :compiler/invalid :module-parse-error."
  [loaded-genome module-k]
  (let [path (get-in loaded-genome [:manifest :modules module-k])]
    (when-not (string? path)
      (throw (err/error :compiler/invalid
                        "manifest does not declare the module path"
                        {:reason :module-file-missing :module module-k
                         :value (err/sanitize path)})))
    (when-not (contains? (:files loaded-genome) path)
      (throw (err/error :compiler/invalid
                        "declared module is absent from the loaded genome :files"
                        {:reason :module-file-missing :module module-k :path path})))
    (let [ba (byte-array (get-in loaded-genome [:files path :bytes]))]
      (try
        (edn/read-string (String. ba StandardCharsets/UTF_8))
        (catch Exception e
          (throw (err/error :compiler/invalid
                            "declared EDN module failed to parse"
                            {:reason :module-parse-error :module module-k
                             :path path :message (.getMessage e)})))))))

;; --- program registry (component choice (a)) --------------------------------

(defn- compile-programs
  "Compile the in-memory program descriptor registry into a sorted map
  of :program/id to ProgramDescriptor, rejecting duplicate ids.
  Descriptor shape and file/path/entry/policy validation stay entirely
  in evoclj.compiler.program/compile-program-descriptor."
  [loaded-genome]
  (let [registry (or (:programs loaded-genome) [])]
    (when-not (sequential? registry)
      (throw (err/error :compiler/invalid
                        "program registry must be a sequential collection of descriptors"
                        {:reason :invalid-program-registry
                         :value (err/sanitize registry)})))
    (let [compiled (mapv #(program/compile-program-descriptor % loaded-genome)
                         registry)
          dupes (->> compiled
                     (group-by :program/id)
                     (filter #(> (count (val %)) 1)))]
      (when (seq dupes)
        (throw (err/error :compiler/invalid
                          "duplicate program ids in the program registry"
                          {:reason :duplicate-program-id
                           :program-ids (mapv key dupes)})))
      (into (sorted-map) (map (fn [d] [(:program/id d) d])) compiled))))

(defn- check-topology-programs!
  "Fail closed when a compiled topology references a program without
  a compiled descriptor. :sci nodes use :program; Loop Regions use
  :until. Both are definition-level program references and must resolve
  before a phenotype can be instantiated."
  [compiled-topology programs]
  (doseq [[node-id node] (:nodes compiled-topology)
          :let [pid (case (:node/type node)
                      :sci (:program node)
                      :loop (:until node)
                      nil)]
          :when pid]
    (when-not (contains? programs pid)
      (throw (err/error :compiler/program-unresolved
                        "topology references a program with no compiled descriptor"
                        {:program-id pid
                         :node/id node-id
                         :node/type (:node/type node)})))))

;; --- I1 identity -----------------------------------------------------------

(defn- canonical-edn-value
  "Recursively normalize values so maps are sorted and collections order is
  deterministic."
  [v]
  (cond
    (map? v) (into (sorted-map) (map (fn [[k val]] [k (canonical-edn-value val)])) v)
    (vector? v) (mapv canonical-edn-value v)
    (set? v) (vec (sort-by pr-str (map canonical-edn-value v)))
    (seq? v) (mapv canonical-edn-value v)
    :else v))

(defn- canonical-edn-string
  "Canonical deterministic EDN serialization: sorted keys, deterministic
  collection forms, pr-str."
  [v]
  (pr-str (canonical-edn-value v)))

(defn code-id
  "The canonical ProgramImageId (historical name CodeImageId):
  sha256:<64 hex> over the canonical serialization of kernel-abi ||
  genome-id || resolution-id. Identifies the ABI-compatible program
  ONLY — the same id NEVER implies the same execution semantics (see
  the namespace docstring and runtime-image-id). Byte-identical formula,
  unchanged."
  [abi genome-id resolution-id]
  (hash/text-digest (str (canonical-edn-string abi) genome-id resolution-id)))

(defn deployment-id
  "Derive the DeploymentId from the ProgramImage id, bindings, and authority:
  DeploymentId = SHA256(program-image-id || canonical(bindings) || canonical(authority)).
  bindings is a collection of [type id digest] or similar; authority is a
  collection of leases or authority tokens. Both are canonicalized via
  sorted pr-str. Binds the program to a deployment — still not an
  execution-semantics identity (see runtime-image-id)."
  [code-image-id bindings authority]
  (hash/text-digest
   (str (or code-image-id "")
        (canonical-edn-string (vec (sort-by pr-str (or bindings []))))
        (canonical-edn-string (vec (sort-by pr-str (or authority [])))))))

;; --- RuntimeImage identity (Program vs Runtime split) -------------------------

(def interpreter-build-id
  "The interpreter build behind every SCI execution, as a stable string:
  the org.babashka/sci Maven pin in deps.edn. This constant MUST be bumped
  together with that pin; no automated check enforces the match (known
  residual — compilation performs no IO, so the pin cannot be read here)."
  "sci-0.15.58")

(def kernel-build-unresolved
  "Marker carried in the runtime descriptor where a stable kernel build id
  would go. The kernel ABI map (e.g. {:kernel 1 :genome 1 :intent 1 :tool 1})
  is an INTERFACE version only: it cannot distinguish two different kernel
  implementations behind the same interface. No version file, build stamp,
  or content hash of the kernel implementation exists in-repo, so the
  descriptor records this marker honestly instead of inventing a hermetic
  id (known residual)."
  :kernel/build-unresolved)

(defn adapter-builds-from-resolution
  "The provider adapter builds named by a Resolution: a sorted map of
  model-name to its :adapter-version string. Entries without an explicit
  :adapter-version record :adapter/unversioned. Known residuals: the
  OpenAI/Anthropic adapter namespaces carry no separate code-build
  constant, and SaaS endpoint builds are unobservable from the compile
  side — the logical binding name is all the program side can see, which
  is exactly why the same logical binding NEVER implies the same model
  function."
  [resolution]
  (into (sorted-map)
        (map (fn [[model-name entry]]
               [model-name (or (:adapter-version entry) :adapter/unversioned)]))
        (:models resolution)))

(defn default-runtime-descriptor
  "The runtime descriptor for one execution of `compiled` (a compile-genome
  result): the kernel ABI (interface version only, plus the
  unresolved-build marker), the interpreter build id, and the adapter
  builds named by the compiled Resolution. Pure data. Callers simulating
  an implementation change (kernel, interpreter, or adapter swap behind
  unchanged interfaces) override the relevant field before hashing."
  [compiled]
  {:kernel/abi (:abi compiled)
   :kernel/build kernel-build-unresolved
   :interpreter/build interpreter-build-id
   :adapter/builds (adapter-builds-from-resolution (:resolution compiled))})

(defn runtime-image-id
  "The RuntimeImageId: sha256:<64 hex> over the canonical serialization of
  runtime-descriptor || program-image-id, where runtime-descriptor is a
  default-runtime-descriptor map (or a caller override simulating an
  implementation change) and program-image-id is the :code/id ProgramImage.
  The SAME ProgramImage under DIFFERENT runtime descriptors yields
  DIFFERENT RuntimeImageIds; the same pair always yields the same id. A
  RuntimeImageId still does NOT imply identical SaaS model behavior —
  pair it with the ExecutionEnvironment observational provenance
  (evoclj.compiler.resolution/execution-environment) for that."
  [program-image-id runtime-descriptor]
  (types/code-id program-image-id)
  (hash/text-digest (str (canonical-edn-string runtime-descriptor) program-image-id)))
;; --- public entry point ----------------------------------------------------

(defn compile-genome
  "Compile a loaded Genome into the pure CompiledGenome and derive the
  I1 identity (component). Orchestration only — every validation
  rule lives in the focused modules called here.

  `loaded-genome` is the evoclj.genome.load result, optionally carrying
  the in-memory program descriptor registry under :programs (a
  sequential collection of component descriptor maps). `provider-catalog`
  is the map of alias keyword to concrete provider entry consumed by
  evoclj.compiler.resolution/resolve-models.

  The declared :topology and :models modules are decoded from the
  immutable in-memory :files payloads (no IO, no evaluation) and
  compiled by evoclj.compiler.topology/compile-topology and
  evoclj.compiler.resolution/resolve-models; the manifest is
  re-validated by evoclj.genome.schema/validate-manifest; every
  registry descriptor is compiled by
  evoclj.compiler.program/compile-program-descriptor; and every program
  referenced by a :sci or Loop :until node in the compiled topology must
  have a compiled descriptor. The topology's static Effects must also be
  declared by the manifest's Requested capability set
  (Effects ⊆ Requested); runtime lease checks complete the upper bound.

  Returns a pure data map with exactly the normative CompiledGenome key
  set (I1 Data Contracts): :code/id (ProgramImageId, historical
  CodeImageId key), :code/genome-id,
  :code/resolution-id, :deployment/id (DeploymentId with empty
  bindings/authority for pure compile), :execution/id (fresh UUID per
  compile), :abi, :manifest, :topology, :effects,
  :programs (sorted :program/id => ProgramDescriptor),
  :requested-capabilities, and :resolution. :code/id is
  sha256:<64 hex> over ABI || genome-id || resolution-id and identifies
  the ABI-compatible program ONLY — never execution semantics.
  :deployment/id
  is SHA256(code-id || canonical(bindings) || canonical(authority)) with
  empty bindings/authority at compile time. :execution/id is a fresh
  random UUID per compilation. The RuntimeImageId is deliberately NOT a
  compile-output key (the key set stays exactly normative): execution
  records derive it via runtime-image-id from this map. The result
  round-trips through pr-str /
  clojure.edn read-string and contains no raw source bytes or byte arrays
  (Global Constraint 22). PhenotypeId legacy alias is removed.

  Throws ExceptionInfo with a stable :error/type. Errors from the
  focused modules pass through unchanged (:genome/schema-invalid,
  :resolution/invalid, :resolution/alias-missing,
  :resolution/secret-key, :topology/invalid, :topology/cycle,
  :program/invalid, :program/path-invalid, :program/file-missing,
  :program/parse-error, :program/policy-violation). Orchestration glue
  throws :compiler/invalid (:reason :invalid-loaded-genome,
  :module-file-missing, :module-parse-error, :invalid-program-registry,
  :duplicate-program-id) or :compiler/program-unresolved."
  [loaded-genome provider-catalog]
  (validate-loaded-genome! loaded-genome)
  (let [manifest (:manifest loaded-genome)
        abi (:abi manifest)
        requested-capabilities (:capabilities/requested manifest)
        compiled-topology (topology/compile-topology
                           (module-value loaded-genome :topology))
        _ (capability/validate-effect-lattice!
           (:effects compiled-topology)
           requested-capabilities)
        resolution-map (resolution/resolve-models
                        (module-value loaded-genome :models)
                        provider-catalog)
        programs (compile-programs loaded-genome)
        _ (check-topology-programs! compiled-topology programs)
        genome-id (:genome/id loaded-genome)
        resolution-id (:resolution/id resolution-map)
        cid (code-id abi genome-id resolution-id)
        did (deployment-id cid [] [])
        eid (java.util.UUID/randomUUID)]
    (into (sorted-map)
          {:code/id cid
           :code/genome-id genome-id
           :code/resolution-id resolution-id
           :deployment/id did
           :execution/id eid
           :abi abi
           :manifest manifest
           :topology compiled-topology
           :effects (:effects compiled-topology)
           :programs programs
           :requested-capabilities requested-capabilities
           :resolution resolution-map})))
