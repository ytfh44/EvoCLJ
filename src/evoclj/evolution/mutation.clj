(ns evoclj.evolution.mutation
  "Mutation IR validation: schema plus patch preconditions (Task 7.3).

  This namespace is DATA VALIDATION ONLY — applying mutations to a
  parent Genome is Task 7.4 (evoclj.genome.patch). `validate-mutation`
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

  Task E-cross adds the dual-parent crossover mutation (host opt-in):
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
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Paths)))

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
  preconditions (Task 7.3).

  With no context, applies the schema gate (envelope, all op variants,
  Step 2 :expect/hash) plus the path-safety and protected-path gates.
  With a parent context — the loaded parent Genome map
  ({:genome/id :manifest :genome/root :files}, as produced by
  evoclj.genome.load) or a bare manifest map — additionally anchors
  the symlink-escape check at :genome/root and enforces the
  declared-mutable-class gate from :manifest's :evolution :mutable.

  Returns the mutation unchanged when valid. Throws ExceptionInfo with
  a stable :error/type: :mutation/invalid, :mutation/op-invalid,
  :mutation/path-invalid, :mutation/protected-path, or
  :mutation/undeclared-mutable-class."
  ([mutation]
   (validate-mutation mutation nil))
  ([mutation parent-context]
   ;; Op-level shape first, so a malformed op inside an otherwise valid
   ;; envelope surfaces as :mutation/op-invalid (with the precise op)
   ;; rather than being swallowed by the envelope gate.
   (doseq [op (:ops mutation)]
     (ms/validate-op op))
   (ms/validate-mutation mutation)
   (let [manifest (manifest-of parent-context)
         base (some-> parent-context :genome/root)]
     (doseq [op (:ops mutation)]
       (let [canonical (canonical-file (:file op))]
         (check-resolves-inside-root! base canonical)
         (check-not-protected! manifest canonical)
         (check-declared-mutable-class! manifest canonical))))
   mutation))

;; ============================================================================
;; Task E-cross — dual-parent crossover (host opt-in)
;;
;; The crossover mutation recombines TWO parent Genomes into one child
;; by topology-aware recombination: split parent A's topology at a
;; node (the cut), take that node's subtree from parent B, re-resolve
;; dependencies, and gate the child through the topology compiler
;; (Global Constraint: the child must satisfy compiler topology
;; validity). It is PURE and DETERMINISTIC (Global Constraints 1 and
;; 6) and is NOT part of the default mutation distribution — a host
;; opts in by calling `crossover` directly; :crossover never appears
;; in a Mutation IR's :ops and is rejected by the default op schema.
;; ============================================================================

(def default-op-distribution
  "The default mutation op distribution — the closed thirteen-op
  language of Task 7.3 that default mutators may propose from
  (identical to the OpSchema :multi dispatch of
  evoclj.evolution.mutation-schema). :crossover is DELIBERATELY
  ABSENT: dual-parent recombination (Task E-cross) is a host-opt-in
  operation reachable ONLY through the explicit `crossover` entry
  point below. It is never part of a single-parent Mutation IR and no
  default mutator ever proposes it."
  #{:set-edn :delete-edn :insert-text :replace-text :delete-text
    :replace-form :insert-form :delete-form :add-node :remove-node
    :add-edge :remove-edge :update-node})

(defn- crossover-error!
  "Throw the stable Task E-cross typed error (Global Constraint 22:
  plain serializable data)."
  [reason data]
  (throw (err/error :evolution/crossover-invalid
                    "crossover rejected the parent combination"
                    (assoc data :reason reason))))

(defn- next-and-body
  "The node ids `node` reaches directly: its :next successor plus its
  :body (the :loop body node id). :until is a program id, not a node
  id, and never participates in the graph closure."
  [node]
  (cond-> []
    (:next node) (conj (:next node))
    (:body node) (conj (:body node))))

(defn- subtree-ids
  "The ids of every node in `topology` reachable from `root` following
  :next and :body edges — the node's downstream subtree (a :loop
  node's :body closes back to the loop, so the traversal is guarded by
  a seen-set and terminates). `root` must be a declared node; the
  callers validate both parents through compile-topology first."
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
  "Re-resolve the retained parent-A nodes after the splice: any :next
  or :body that pointed into parent A's removed subtree is re-pointed
  at `cut` — the root of the grafted subtree, always present in the
  child — so no dangling edge survives. Nodes without the key are
  untouched (nil is never a set member)."
  [retained a-subtree cut]
  (into {}
        (map (fn [[id node]]
               [id (cond-> node
                     (contains? a-subtree (:next node)) (assoc :next cut)
                     (contains? a-subtree (:body node)) (assoc :body cut))]))
        retained))

(defn crossover-topologies
  "Recombine two parent topology values into one child topology value
  (Task E-cross).

  Semantics: split parent A's topology at `cut` (a declared node id),
  remove A's subtree at `cut` (everything reachable from `cut` via
  :next/:body edges), and graft parent B's subtree at `cut` in its
  place. Dependencies are re-resolved deterministically:

  - retained A nodes whose :next/:body pointed into A's removed
    subtree are re-pointed at `cut` (the graft root, always present);
  - B's subtree is taken wholesale (by closure every edge inside B's
    subtree stays inside it);
  - the entry is kept unless it was inside A's removed subtree, in
    which case it becomes `cut` (in a valid topology the entry can
    only be downstream of `cut` when it IS `cut`).

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
  Global Constraint 22). Throws the Task E-cross typed error for a
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
  "Dual-parent crossover mutation (Task E-cross, host opt-in).

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
