(ns evoclj.security.patch-lint
  "Static pre-application lint of a Mutation IR's ops against protected
  path prefixes and allowed asset classes (Trust & Hygiene F7).

  The evolution Mutation IR (:ops of :file-targeted operations, shaped by
  evoclj.evolution.mutation-schema) is applied by the patch compiler to
  real Genome files. Before any op is applied, this module lint-checks
  each op's :file against two trust bounds:

  - PROTECTED PATH PREFIXES — first-path-segment prefixes that must
    never be mutated by a candidate (kernel, capability, promotion,
    store, compiler, sci by default). A target on such a prefix is a
    :fatal finding.
  - ALLOWED ASSET CLASSES — an optional declared allowlist of asset
    classes (the :file's first path segment, with a root-level extension
    stripped: \"skills/debugging.edn\" -> \"skills\"). When a wantlist is
    supplied and an op's class is not in it, the op is flagged :warn for
    operator review.

  Path canonicalization is replicated locally (this module does not read
  evoclj.genome.path): backslash separators are normalized to \"/\", a
  leading \"./\" is stripped, duplicate and boundary separators are
  collapsed, and the asset class is the FIRST path segment with a
  root-level file extension stripped (\"skills/debugging.edn\" ->
  \"skills\", \"topology.edn\" -> \"topology\", \"programs/route.clj\" ->
  \"programs\").

  Findings are DATA for operator review — `lint-patch` returns all
  findings and never throws on them; `lint-patch!` throws only when a
  :fatal finding exists, so the application gate (arriving with the
  feature) can fail closed.

  Rules (evaluated in order, first match wins per op):
    1. an op without a :file -> :lint/missing-file (:fatal).
    2. the document's first path segment is in :protected-prefixes ->
       :lint/protected-path (:fatal).
    3. :allowed-classes provided and the op's class not in it ->
       :lint/undeclared-class (:warn).
    4. otherwise no finding for that op.

  Error contract (Global Constraint 22 — plain serializable data):
  :security/patch-lint-invalid (mutation not a map, :ops not sequential,
  or opts not a map), :security/patch-lint-fatal (with :findings in the
  ex-data when any :fatal finding exists)."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err]))

(def default-opts
  "The defaults when `<opts>` is absent: protected first-path prefixes
  never treated as mutable, and no asset-class allowlist (rule 3
  disabled until the caller declares one)."
  {:protected-prefixes ["kernel" "capability" "promotion" "store"
                        "compiler" "sci"]
   :allowed-classes nil})

;; --- local path canonicalization --------------------------------------------

(defn- canonical-file
  "Normalize a mutation op's :file to its canonical relative form:
  backslash separators become \"/\", a leading \"./\" is stripped, and
  duplicate/boundary separators are collapsed so the first path segment
  is well-defined. Returns a string (\"\" for an effectively-empty
  remainder)."
  [file]
  (-> file
      (str/replace "\\" "/")
      (str/replace #"(?m)^\./" "")
      (str/replace #"/{2,}" "/")
      (str/replace #"^/+|/+$" "")))

(defn- first-segment
  "The first path component of a canonical path, or nil for an empty
  path."
  [canonical]
  (first (str/split canonical #"/")))

(defn- path-class
  "The asset class of a canonical path: its first path segment with a
  root-level file extension stripped. \"skills/debugging.edn\" ->
  \"skills\"; \"topology.edn\" -> \"topology\"; \"programs/route.clj\" ->
  \"programs\". A first segment, if any, is returned unmodified."
  [canonical]
  (let [seg (first-segment canonical)]
    (when seg
      (str/replace seg #"\.[^.]+$" ""))))

;; --- findings ----------------------------------------------------------------

(defn- missing-file-finding
  "Rule 1: an op with no :file cannot be scoped to a safe target. The op
  is included (sanitized) so operators can inspect it without trusting
  raw input."
  [i op]
  {:lint/op-index i
   :lint/rule :lint/missing-file
   :lint/level :fatal
   :lint/file nil
   :lint/detail {:op (err/sanitize op)}})

(defn- protected-path-finding
  "Rule 2: the canonical path's first segment is a protected prefix — a
  candidate must never rewrite it."
  [i canonical class]
  {:lint/op-index i
   :lint/rule :lint/protected-path
   :lint/level :fatal
   :lint/file canonical
   :lint/detail {:class class}})

(defn- undeclared-class-finding
  "Rule 3: the target class is not in the caller's :allowed-classes."
  [i canonical class allowed]
  {:lint/op-index i
   :lint/rule :lint/undeclared-class
   :lint/level :warn
   :lint/file canonical
   :lint/detail {:class class
                 :allowed (vec (sort allowed))}})

;; --- the lint ----------------------------------------------------------------

(defn lint-patch
  "Lint a Mutation IR's ops against the trust bounds. `mutation` is a map
  with a sequential :ops (each op shaped by
  evoclj.evolution.mutation-schema). `opts` (defaults when absent) may
  override :protected-prefixes and :allowed-classes. Returns a vector of
  findings (DATA for operator review) — it never throws on findings.

  Throws :security/patch-lint-invalid when mutation is not a map, :ops is
  not sequential, or opts is not a map."
  ([mutation] (lint-patch mutation nil))
  ([mutation opts]
   (when-not (map? mutation)
     (throw (err/error :security/patch-lint-invalid
                       "mutation must be a map"
                       {:problem :mutation})))
   (when-not (sequential? (:ops mutation))
     (throw (err/error :security/patch-lint-invalid
                       "mutation :ops must be a sequential collection"
                       {:problem :ops})))
   (when (and (some? opts) (not (map? opts)))
     (throw (err/error :security/patch-lint-invalid
                       "lint opts must be a map"
                       {:problem :opts})))
   (let [{:keys [protected-prefixes allowed-classes]} (merge default-opts (or opts {}))
         protected (set protected-prefixes)
         lint-one
         (fn [[i op]]
           (if-not (:file op)
             (missing-file-finding i op)
             (let [canonical (canonical-file (:file op))
                   seg (first-segment canonical)
                   class (path-class canonical)]
               (cond
                 (contains? protected seg)
                 (protected-path-finding i canonical class)

                 (and allowed-classes
                      (not (contains? (set allowed-classes) class)))
                 (undeclared-class-finding i canonical class allowed-classes)

                 :else nil))))]
     (->> (map-indexed vector (:ops mutation))
          (keep lint-one)
          (vec)))))

(defn lint-patch!
  "Lint a Mutation IR and fail closed on :fatal findings. Returns the
  findings (all non-fatal, since any :fatal finding would have thrown)
  when nothing is fatal.

  Throws :security/patch-lint-fatal with {:findings [...]} in the
  ex-data when any op produced a :fatal finding; throws
  :security/patch-lint-invalid for malformed inputs as in lint-patch."
  ([mutation] (lint-patch! mutation nil))
  ([mutation opts]
   (let [findings (lint-patch mutation opts)
         fatal (filterv #(= :fatal (:lint/level %)) findings)]
     (when (seq fatal)
       (throw (err/error :security/patch-lint-fatal
                         "mutation fails static patch lint"
                         {:findings findings})))
     findings)))
