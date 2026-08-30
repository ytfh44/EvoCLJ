(ns evoclj.evolution.mutation
  "Mutation IR validation: schema plus patch preconditions (component).

  This namespace is DATA VALIDATION ONLY — applying mutations to a
  parent Genome is component (evoclj.genome.patch). `validate-mutation`
  runs, in order:

  1. The schema gates (evoclj.evolution.mutation-schema): each op's
     shape first (so a malformed op surfaces as :mutation/op-invalid
     with the precise op), then the closed Mutation envelope. The op
     schemas enforce the Step 2 :expect/hash requirement on every
     destructive/replace op (:set-edn :delete-edn :replace-text
     :delete-text :replace-form :delete-form :remove-node
     :remove-edge :update-node) — a stale patch must never silently
     apply to a different parent.
  2. The path gate (Step 3), reusing
     evoclj.genome.path/allowed-genome-path? for every op's :file:
     the path must canonicalize to a relative Genome path (no
     absolute paths, drive letters, `.`/`..` traversal, empty
     components, or symlink escapes) — anything else is a path
     escaping the Genome root and fails closed.
  3. The protected-path gate (Step 3): operations may not target
     kernel files, evaluation roots, protected Genome paths, or
     capability-root data. The protected set:

       :kernel-file     — manifest.edn, the Genome manifest / trust
                          boundary (carries :abi and
                          :capabilities/requested; Global Constraint
                          19)
       :kernel-root     — any path under a kernel/ prefix (kernel
                          modules are outside all Genome mutable
                          roots)
       :eval-root       — eval/, evaluator/, evaluation/ prefixes
                          (Global Constraint 12: a candidate MUST NOT
                          modify the evaluator that judges it)
       :capability-root — capability/, capabilities/ prefixes
                          (authority data is never agent-mutable,
                          Global Constraint 19)
       :evolution-root  — the manifest's declared :modules :evolution
                          file (evolution policy is R4 :meta — not
                          enabled in v0 — and a candidate must not
                          rewrite the rules that bound it)

  4. The mutable-class gate (Step 3): when the parent manifest is
     supplied, each op's target file must belong to a DECLARED
     mutable asset class — the :file's first path component, with a
     root-level extension stripped (\"skills/debugging.edn\" →
     :skills, \"topology.edn\" → :topology) — as declared by the
     manifest's :evolution :mutable set. A mutation may therefore
     never touch topology structure, model/memory modules, or any
     other undeclared class (\"topology structure outside declared
     mutable classes\" is rejected here).

  Signature: `validate-mutation` takes the mutation and an optional
  parent context — the loaded Genome map ({:manifest ... :genome/root
  ...}, as produced by evoclj.genome.load) or a bare manifest map.
  The :genome/root anchors the symlink escape check; the :manifest
  drives the evolution-root and mutable-class gates. With no context,
  path safety and protected paths still apply (fail-closed), while
  the class gate is skipped (the declared set is unknown).

  Error contract (Global Constraint 22 — plain serializable data):
  :mutation/invalid, :mutation/op-invalid (humanized Malli
  explanations), :mutation/path-invalid, :mutation/protected-path
  (with :reason), :mutation/undeclared-mutable-class (with :class and
  :declared).

  component adds the dual-parent crossover mutation (host opt-in):
  `crossover` recombines TWO parent Genomes into one child by
  topology-aware recombination — split parent A's topology at a node,
  take that node's subtree from parent B, re-resolve dependencies, and
  gate the child through the topology compiler. The op is pure and
  deterministic, and is NOT part of the default mutation distribution
  (see `default-op-distribution`) — a host opts in by calling
  `crossover` directly; a :crossover op never appears in a Mutation
  IR's :ops."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.compiler.topology :as topology]
            [evoclj.evolution.mutation-schema :as ms]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.path :as path]
            [evoclj.genome.patch-edn :as patch-edn]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Paths)))

;; ----------------------------------------------------------------------
;; S4 — RawMutation vs ValidatedMutation (definition > validation)
;; ----------------------------------------------------------------------

;; Sealed types via closure + private field: the secret object is never stored
;; in a var and cannot be retrieved via #'var. The factory closes over the
;; secret and stores it in a private field; validated-mutation? checks
;; instance? and identical? on the field via direct field access, not a var.

(deftype MutableAssetRef [parent_genome_id canonical_path ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :parent-genome-id parent_genome_id
      :canonical-path canonical_path
      notFound))
  Object
  (toString [this] (str "MutableAssetRef[" canonical_path "]")))
(alter-meta! #'->MutableAssetRef assoc :private true)

(let [asset-ref-secret (Object.)]
  (defn- ->mutable-asset-ref
    "Create a sealed MutableAssetRef — private, only called from validate-mutation."
    [parent-id canonical]
    (MutableAssetRef. parent-id canonical asset-ref-secret))

  (defn mutable-asset-ref?
    "True when x is a sealed MutableAssetRef produced by validate-mutation."
    [x]
    (and (instance? MutableAssetRef x)
         (identical? (.-secret ^MutableAssetRef x) asset-ref-secret))))

(deftype VerifiedDigest [digest ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :digest digest
      notFound))
  Object
  (toString [this] (str "VerifiedDigest[" digest "]")))
(alter-meta! #'->VerifiedDigest assoc :private true)

(let [verified-secret (Object.)]
  (defn- ->verified-digest
    "Construct a sealed VerifiedDigest after checking the digest is a canonical artifact-id."
    [d]
    (when d
      (when-not (types/artifact-id? d)
        (throw (err/error :mutation/hash-invalid
                          "VerifiedDigest must be sha256:<64 hex>"
                          {:digest d})))
      (VerifiedDigest. d verified-secret)))

  (defn verified-digest?
    "True when x is a sealed VerifiedDigest."
    [x]
    (and (instance? VerifiedDigest x)
         (identical? (.-secret ^VerifiedDigest x) verified-secret))))

(deftype ValidatedMutation [raw_mutation canonical_ops asset_refs verified_digests ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :raw-mutation raw_mutation
      :canonical-ops canonical_ops
      :asset-refs asset_refs
      :verified-digests verified_digests
      notFound))
  clojure.lang.Counted
  (count [this] 4)
  Object
  (toString [this] (str "ValidatedMutation[" (:mutation/id raw_mutation) "]")))
(alter-meta! #'->ValidatedMutation assoc :private true)

(let [validated-secret (Object.)]
  (defn validated-mutation?
    "True when x is a ValidatedMutation — sealed, only produced by validate-mutation."
    [x]
    (and (instance? ValidatedMutation x)
         (identical? (.-secret ^ValidatedMutation x) validated-secret)))

  (defn- make-validated-mutation
    [raw canonical refs digests]
    (ValidatedMutation. raw canonical refs digests validated-secret))

  (defn validated->raw
    [vm]
    (when (validated-mutation? vm)
      (.-raw_mutation ^ValidatedMutation vm)))

  (defn validated-ops
    [vm]
    (when (validated-mutation? vm)
      (.-canonical_ops ^ValidatedMutation vm)))

  (defn validated-refs
    [vm]
    (when (validated-mutation? vm)
      (.-asset_refs ^ValidatedMutation vm)))

  (defn validated-digests
    [vm]
    (when (validated-mutation? vm)
      (.-verified_digests ^ValidatedMutation vm))))

(defn raw-mutation?
  [x]
  (and (map? x)
       (not (validated-mutation? x))
       (contains? x :mutation/id)
       (vector? (:ops x))
       (every? (fn [op] (string? (:file op))) (:ops x))))

;; --- path safety (Step 3: reuse evoclj.genome.path) -------------------------

(defn- canonical-file
  "Normalize an op's :file to its canonical relative form. Throws
  :mutation/path-invalid for anything that is not a canonical relative
  Genome path (absolute, drive letter, `.`/`..` traversal, empty
  component, NUL byte) — the same failure class as a path escaping
  the Genome root."
  [file]
  (try
    (path/normalize-relative-path file)
    (catch clojure.lang.ExceptionInfo _
      (throw (err/error :mutation/path-invalid
                        "mutation :file must be a canonical relative Genome path"
                        {:path file})))))

(defn- check-resolves-inside-root!
  "Require the canonical path to resolve inside the Genome root:
  allowed-genome-path? additionally rejects any component that is a
  symbolic link on disk (a Genome bundle must not reach outside
  itself). Fail-closed: any error is treated as disallowed."
  [base canonical]
  (when-not (path/allowed-genome-path?
             (or base (Paths/get "." (make-array String 0)))
             canonical)
    (throw (err/error :mutation/path-invalid
                      "mutation :file must resolve inside the Genome root"
                      {:path canonical}))))

;; --- protected Genome paths (Step 3) ----------------------------------------

(def ^:private eval-root-components
  "First path components reserved as evaluation roots: a candidate must
  not modify the evaluator that judges it (Global Constraint 12)."
  #{"eval" "evaluator" "evaluation"})

(def ^:private capability-root-components
  "First path components reserved as capability/authority roots:
  authority data is never agent-mutable (Global Constraint 19)."
  #{"capability" "capabilities"})

(defn protected-path-reason
  "The protection reason for a canonical relative Genome path, or nil.

  Reasons: :kernel-file (manifest.edn), :kernel-root (kernel/
  prefix), :eval-root (eval|evaluator|evaluation/ prefix),
  :capability-root (capability|capabilities/ prefix), :evolution-root
  (the manifest's declared :modules :evolution file)."
  [manifest path]
  (let [first-component (first (str/split path #"/"))]
    (cond
      (= path "manifest.edn") :kernel-file
      (= first-component "kernel") :kernel-root
      (contains? eval-root-components first-component) :eval-root
      (contains? capability-root-components first-component) :capability-root
      (= path (some-> manifest :modules :evolution path/normalize-relative-path))
      :evolution-root
      :else nil)))

(defn- check-not-protected!
  [manifest canonical]
  (when-let [reason (protected-path-reason manifest canonical)]
    (throw (err/error :mutation/protected-path
                      "mutation targets a kernel-protected Genome path"
                      {:path canonical :reason reason}))))

;; --- the mutable-class gate (Step 3) ----------------------------------------

(defn- path-class
  "The mutable asset class of a canonical relative path: its first
  path component, with a root-level file extension stripped.
  \"skills/debugging.edn\" → :skills; \"topology.edn\" → :topology;
  \"programs/route.clj\" → :programs."
  [path]
  (let [first-component (first (str/split path #"/"))]
    (keyword (str/replace first-component #"\.[^.]+$" ""))))

(defn- manifest-of
  "Extract the manifest from a parent context: the :manifest of a
  loaded Genome map, the map itself when it is a bare manifest, else
  nil."
  [x]
  (cond
    (nil? x) nil
    (contains? x :manifest) (:manifest x)
    (map? x) x
    :else nil))

(defn- check-declared-mutable-class!
  "Require the target file's asset class to be declared mutable by the
  parent manifest's :evolution :mutable set. Skipped when no manifest
  is supplied (the declared set is unknown)."
  [manifest canonical]
  (when manifest
    (let [declared (get-in manifest [:evolution :mutable])
          cls (path-class canonical)]
      (when (and (set? declared) (not (contains? declared cls)))
        (throw (err/error :mutation/undeclared-mutable-class
                          "mutation targets an undeclared mutable asset class"
                          {:path canonical
                           :class cls
                           :declared (vec (sort declared))}))))))

;; --- public entry point -----------------------------------------------------

(defn validate-mutation
  "Validate a Mutation IR against the schema AND the patch
  preconditions (component) and return a ValidatedMutation.

  S4: RawMutation (file : String) is validated into ValidatedMutation
  (MutableAssetRef + VerifiedDigest). The ValidatedMutation bundles:

    :raw-mutation   — the original RawMutation map (file is String)
    :canonical-ops  — ops where :file is the canonical relative path
    :asset-refs     — vector<MutableAssetRef> (parent-genome-id + canonical-path)
    :verified-digests— vector<VerifiedDigest|nil> (one per op)

  Only constructible via this function, which checks, in order:

    - canonical path (normalize-relative-path + allowed-genome-path? / symlink)
    - protected path (manifest.edn, kernel/, eval/, capability/, evolution)
    - mutable class (declared :evolution :mutable)
    - hash (VerifiedDigest must be sha256:<64 hex>)

  With no context, applies the schema gate plus path-safety and protected-path gates.
  With a parent context — the loaded parent Genome map
  ({:genome/id :manifest :genome/root :files}) or a bare manifest map — additionally anchors
  the symlink-escape check at :genome/root and enforces the declared-mutable-class gate.

  Returns a ValidatedMutation when valid. Throws ExceptionInfo with
  a stable :error/type: :mutation/invalid, :mutation/op-invalid,
  :mutation/path-invalid, :mutation/protected-path,
  :mutation/undeclared-mutable-class, or :mutation/hash-invalid."
  ([mutation]
   (validate-mutation mutation nil))
  ([mutation parent-context]
   (doseq [op (:ops mutation)]
     (ms/validate-op op))
   (ms/validate-mutation mutation)
   (let [manifest (manifest-of parent-context)
         base (some-> parent-context :genome/root)
         parent-id (or (some-> parent-context :genome/id)
                       (:parent/genome-id mutation))
         validated-pairs (mapv (fn [op]
                                 (let [canonical (canonical-file (:file op))]
                                   (check-resolves-inside-root! base canonical)
                                   (check-not-protected! manifest canonical)
                                   (check-declared-mutable-class! manifest canonical)
                                   [canonical (->verified-digest (:expect/hash op))]))
                               (:ops mutation))
         canonicals (mapv first validated-pairs)
         verified-digests (mapv second validated-pairs)
         canonical-ops (mapv (fn [op canonical] (assoc op :file canonical))
                             (:ops mutation) canonicals)
         asset-refs (mapv (fn [canonical] (->mutable-asset-ref parent-id canonical))
                          canonicals)]
     (make-validated-mutation mutation canonical-ops asset-refs verified-digests))))


(def default-op-distribution
  "The default mutation op distribution — the closed thirteen-op
  language of component that default mutators may propose from
  (identical to the OpSchema :multi dispatch of
  evoclj.evolution.mutation-schema). :crossover is DELIBERATELY
  ABSENT: dual-parent recombination (component) is a host-opt-in
  operation reachable ONLY through the explicit `crossover` entry
  point below. It is never part of a single-parent Mutation IR and no
  default mutator ever proposes it."
  #{:set-edn :delete-edn :insert-text :replace-text :delete-text
    :replace-form :insert-form :delete-form :add-node :remove-node
    :add-edge :remove-edge :update-node})

(defn- crossover-error!
  "Throw the stable component typed error (Global Constraint 22:
  plain serializable data)."
  [reason data]
  (throw (err/error :evolution/crossover-invalid
                    "crossover rejected the parent combination"
                    (assoc data :reason reason))))

(defn- next-and-body
  "The node ids `node` reaches directly: a sequential :next successor,
  a Loop's :exit successor, and a Loop's :body region root. :until is a
  program id, not a node id, and never participates in graph closure."
  [node]
  (cond-> []
    (:next node) (conj (:next node))
    (:exit node) (conj (:exit node))
    (:body node) (conj (:body node))))

(defn- subtree-ids
  "The ids of every node in `topology` reachable from `root` following
  sequential :next plus Loop :body/:exit edges — the node's downstream
  Region closure. A Loop body can close back to the loop, so traversal is
  guarded by a seen-set and terminates. `root` must be declared; callers
  validate both parents through compile-topology first."
  [topology root]
  (loop [todo [root]
         seen #{}]
    (if-let [id (first todo)]
      (if (contains? seen id)
        (recur (rest todo) seen)
        (recur (into (rest todo) (next-and-body (get (:nodes topology) id)))
               (conj seen id)))
      seen)))

(defn- resolve-retained-edges
  "Re-resolve retained parent-A nodes after the splice: any :next, :exit,
  or :body edge that pointed into parent A's removed subtree is re-pointed
  at `cut`, the graft root always present in the child. Nodes without the
  key are untouched (nil is never a set member)."
  [retained a-subtree cut]
  (into {}
        (map (fn [[id node]]
               [id (cond-> node
                     (contains? a-subtree (:next node)) (assoc :next cut)
                     (contains? a-subtree (:exit node)) (assoc :exit cut)
                     (contains? a-subtree (:body node)) (assoc :body cut))]))
        retained))

(defn crossover-topologies
  "Recombine two parent topology values into one child topology value
  (component).

  Semantics: split parent A's topology at `cut` (a declared node id),
  remove A's Region closure at `cut` (everything reachable from `cut` via
  sequential :next and Loop :body/:exit edges), and graft parent B's
  Region closure at `cut` in its place. Dependencies are re-resolved
  deterministically:

  - retained A nodes whose :next/:exit/:body pointed into A's removed
    closure are re-pointed at `cut` (the graft root, always present);
  - B's Region closure is taken wholesale (edges inside it stay inside);
  - the entry is kept unless it was inside A's removed closure, in which
    case it becomes `cut` (in a valid topology the entry can only be
    downstream of `cut` when it is `cut`).

  The child MUST satisfy compiler topology validity: it is gated
  through evoclj.compiler.topology/compile-topology and, on failure,
  the combination is rejected with :evolution/crossover-invalid
  :reason :child-invalid (defense-in-depth — the gate never assumes
  the splice is safe).

  Pure and deterministic (Global Constraints 1 and 6): the child is a
  pure function of the two parent values and `cut`; identical inputs
  yield the identical child topology.

  Throws :evolution/crossover-invalid with :reason :parent-invalid (a
  parent fails compile-topology; :parent names :a/:b and :cause
  carries the wrapped topology error data), :cut-node-invalid,
  :cut-node-missing-a, :cut-node-missing-b, or :child-invalid."
  [topology-a topology-b cut]
  (when-not (keyword? cut)
    (crossover-error! :cut-node-invalid {:cut (err/sanitize cut)}))
  (letfn [(validated [label t]
            (try
              (topology/compile-topology t)
              t
              (catch clojure.lang.ExceptionInfo e
                (crossover-error! :parent-invalid
                                  {:parent label
                                   :cause (ex-data e)}))))]
    (let [a (validated :a topology-a)
          b (validated :b topology-b)
          nodes-a (:nodes a)
          nodes-b (:nodes b)]
      (when-not (contains? nodes-a cut)
        (crossover-error! :cut-node-missing-a {:cut cut}))
      (when-not (contains? nodes-b cut)
        (crossover-error! :cut-node-missing-b {:cut cut}))
      (let [a-subtree (subtree-ids a cut)
            b-subtree (subtree-ids b cut)
            retained (into {}
                           (remove (fn [[id _]] (contains? a-subtree id)))
                           nodes-a)
            retained' (resolve-retained-edges retained a-subtree cut)
            grafted (select-keys nodes-b b-subtree)
            entry (if (contains? a-subtree (:entry a))
                    cut
                    (:entry a))
            child (-> a
                      (assoc :nodes (merge retained' grafted))
                      (assoc :entry entry))]
        (try
          (topology/compile-topology child)
          (catch clojure.lang.ExceptionInfo e
            (crossover-error! :child-invalid
                              {:cut cut :cause (ex-data e)})))
        child))))

(defn- topology-file-of
  "The declared topology module path of a parent context, or nil."
  [ctx]
  (some-> ctx :manifest :modules :topology))

(defn- topology-value-of
  "Parse a parent context's topology module value from its :files
  payload (clojure.edn/read-string — never clojure.core read-string,
  Global Constraint 22). Throws the component typed error for a
  malformed context, a missing topology file, or an unparseable
  topology."
  [ctx label]
  (when-not (and (map? ctx) (map? (:manifest ctx)) (map? (:files ctx)))
    (crossover-error! :parent-context-invalid {:parent label}))
  (let [file (topology-file-of ctx)]
    (when-not file
      (crossover-error! :parent-context-invalid
                        {:parent label :reason :no-topology-module}))
    (let [entry (get-in ctx [:files file])]
      (when-not (and entry (= :edn (:kind entry)))
        (crossover-error! :topology-file-missing
                          {:parent label :file file}))
      (try
        (let [v (edn/read-string
                 (String. (byte-array (:bytes entry)) StandardCharsets/UTF_8))]
          (when-not (map? v)
            (crossover-error! :topology-unparseable
                              {:parent label :file file}))
          v)
        (catch Exception e
          (crossover-error! :topology-unparseable
                            {:parent label :file file
                             :message (.getMessage e)}))))))

(defn crossover
  "Dual-parent crossover mutation (component, host opt-in).

  Produces a valid child Genome from two parent Genome contexts by
  topology-aware recombination: split parent A's topology at the
  option's :cut/node, take that node's subtree from parent B, and
  re-resolve dependencies (see `crossover-topologies` for the exact
  rules). The child MUST satisfy compiler topology validity — enforced
  by the same gate.

  `parent-a` and `parent-b` are loaded-Genome-shaped contexts
  ({:manifest ... :files {...}} — the shape produced by
  evoclj.genome.load/load-genome and consumed by
  evoclj.genome.patch/apply-mutation). Each parent's topology module
  is read from its OWN manifest's :modules :topology.

  Returns the child as an immutable in-memory Genome map {:genome/id
  :manifest :files}: parent A's manifest and non-topology files are
  kept; the topology module is re-serialized in the patch pipeline's
  canonical EDN form with its digest recomputed, and :genome/id is
  recomputed as the canonical tree digest over the child's files
  (identical to load-genome's address). Pure: no store, no filesystem;
  identical inputs yield the identical child (Global Constraints 1 and
  6).

  NOT part of the default mutation distribution (see
  `default-op-distribution`): :crossover is absent from the Mutation
  IR op language and only reachable through this explicit entry point
  — a host opts in by calling it directly.

  Throws :evolution/crossover-invalid (the :reason set of
  `crossover-topologies` plus :options-invalid, :parent-context-invalid,
  :topology-file-missing, :topology-unparseable)."
  [parent-a parent-b opts]
  (when-not (and (map? opts) (keyword? (:cut/node opts)))
    (crossover-error! :options-invalid {:opts (err/sanitize opts)}))
  (let [cut (:cut/node opts)
        topo-a (topology-value-of parent-a :a)
        topo-b (topology-value-of parent-b :b)
        child-topology (crossover-topologies topo-a topo-b cut)
        file (topology-file-of parent-a)
        text (str (patch-edn/canonical-str child-topology) "\n")
        digest (hash/text-digest text)
        files (assoc (:files parent-a) file
                     {:digest digest
                      :bytes (vec (.getBytes text StandardCharsets/UTF_8))
                      :kind :edn})
        id (hash/tree-digest (mapv (fn [[p {:keys [digest]}]]
                                     {:path p :digest digest})
                                   files))]
    {:genome/id id
     :manifest (:manifest parent-a)
     :files files}))