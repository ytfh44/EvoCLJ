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
  :declared)."
  (:require [clojure.string :as str]
            [evoclj.evolution.mutation-schema :as ms]
            [evoclj.genome.path :as path]
            [evoclj.kernel.error :as err])
  (:import (java.nio.file Paths)))

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
