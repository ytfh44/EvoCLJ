(ns evoclj.evolution.guard
  "Ownership guard: Evolution may only mutate Genome-owned assets.

  Installed external Skills (~/.agents/skills, project/.agents/skills
  discovered via SkillSource) are NOT part of Genome. The mutation engine
  does not know how to reject them via an explicit external-skill check;
  the boundary is that it simply never receives that write target —
  allowlist validation via declared mutable classes rejects any external
  path before it reaches the engine.

  Vendored skills live under Genome skills/<name>/* and share the
  existing :skills mutable class, so they are evolvable via the normal
  Mutation IR. Bundle files all participate in Genome identity
  (tree-digest includes every file), so vendored skill files are
  content-addressed like any other genome asset."
  (:require [clojure.string :as str]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.kernel.error :as err]))

(defn- path-class
  [path]
  (let [first-component (first (str/split path #"/"))]
    (keyword (str/replace first-component #"\.[^.]+$" ""))))

(defn allowed-genome-target?
  "True if file path's asset class is declared mutable in manifest.

  manifest – genome manifest map with :evolution :mutable set
  file-path – canonical relative genome path string"
  [manifest file-path]
  (let [cls (path-class file-path)
        declared (get-in manifest [:evolution :mutable])]
    (and (set? declared) (contains? declared cls))))

(defn validate-mutation-ownership!
  "Validate that mutation only targets Genome-owned assets.

  Delegates to evoclj.evolution.mutation/validate-mutation which
  enforces: canonical relative path, no escape/symlink, protected-path,
  and declared-mutable-class (allowlist). External skills are outside the
  genome root and their first path component (e.g. .agents) is not in the
  declared mutable set, so they fail closed via :mutation/undeclared-mutable-class
  or :mutation/path-invalid — the engine never sees an explicit
  'external skill' case.

  parent-context – loaded genome map {:manifest ... :genome/root ... :files ...}
                   or bare manifest map.

  Returns mutation unchanged when valid, else throws typed error."
  [mutation parent-context]
  (mutation/validate-mutation mutation parent-context))

(defn genome-owned?
  "True if file-path exists as a genome-owned asset class and would be
  accepted by the allowlist (does not check file existence on disk, only
  manifest ownership). Useful as a pre-filter before proposing mutations."
  [manifest file-path]
  (try
    (let [cls (path-class file-path)]
      (contains? (get-in manifest [:evolution :mutable]) cls))
    (catch Exception _ false)))
