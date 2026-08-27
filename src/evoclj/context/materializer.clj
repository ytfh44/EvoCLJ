(ns evoclj.context.materializer
  "EffectiveContext = Materialize(History, ActiveBindings, CatalogProjection, HostPolicy)

  The materializer reads exact content for each active binding from its
  immutable artifact/tree via CAS — never from the current catalog
  projection. This guarantees pinning: if a binding was activated at
  revision A, and the catalog later moves to B, materialization still
  returns A.

  History is the compressed conversation history (string) produced by
  the compression subsystem (History -> compressed History). The
  materializer does NOT invoke compression; it only combines history
  with segments.

  Content resolution is per-binding and tree-aware (WO-S1):
   - a binding carrying `:binding/descriptor {:type :cas-tree-file
     :path \"SKILL.md\"}` reads that FILE out of the CAS TREE named by
     its `:revision/id` (the tree manifest id), never the manifest as a
     leaf blob;
   - a binding with `:binding/descriptor {:type :cas-leaf}` (or no
     descriptor) is resolved generically: the artifact at `:revision/id`
     is read and, if it turns out to be a tree manifest, its SKILL.md
     is hydrated; otherwise it is treated as a leaf content string.
  Fail-closed (INV-04): an unreadable, missing, or non-treelike blob
  throws a typed error; the materializer never substitutes cached or
  degraded content.

  CAS resolution is polymorphic (INV-05 single implementation — no
  test-only fn injection, see INV-09):
   - if cas is a map with :root (CAS config), use evoclj.store.cas/get-bytes
   - if cas is a map artifact-id -> content string, look up
   - if cas is a string/Path/File (CAS root), use evoclj.store.cas/get-bytes
   - if cas is nil, try to look up :segment/content already on binding (for tests)

  HostPolicy filtering is applied before materialization."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.context.segment :as segment]
            [evoclj.context.policy :as policy]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)))

;; ---------------------------------------------------------------------------
;; CAS fetch helper
;; ---------------------------------------------------------------------------

(defn- bytes->string
  "Decode a byte array as UTF-8."
  [ba]
  (String. ^bytes ba StandardCharsets/UTF_8))

(defn- fetch-leaf-cas
  "Fetch the content string for `revision-id` via a real CAS resolver
  (config map, artifact map, or root path). Returns a string. Throws a
  typed error when the artifact is missing, when no resolver is
  supplied, or when the resolver shape is unsupported — never a silent
  nil. No fn-shaped resolver is accepted (INV-09: cas-fn banned)."
  [cas revision-id]
  (cond
    (nil? cas)
    (throw (err/error :context/materializer-missing-cas "CAS resolver required to fetch immutable artifact" {:revision-id revision-id}))

    (and (map? cas) (contains? cas :root))
    ;; CAS config map {:root ... :verify ...}
    (let [cas-ns (try (requiring-resolve 'evoclj.store.cas/get-bytes) (catch Exception _ nil))]
      (if cas-ns
        (bytes->string (cas-ns cas revision-id))
        (throw (err/error :context/materializer-missing-cas "evoclj.store.cas/get-bytes not available" {:revision-id revision-id}))))

    (and (map? cas) (contains? cas revision-id))
    ;; plain map artifact-id -> content
    (let [c (get cas revision-id)]
      (if (string? c) c
          (throw (err/error :context/materializer-missing
                            (str "CAS map missing content for " revision-id)
                            {:revision-id revision-id :value c}))))

    (map? cas)
    ;; generic map that may contain artifact-id keys, try lookup
    (if-let [c (get cas revision-id)]
      (if (string? c) c
          (bytes->string c))
      (throw (err/error :context/materializer-missing
                        (str "CAS map missing content for " revision-id)
                        {:revision-id revision-id})))

    (or (string? cas) (instance? java.io.File cas) (instance? java.nio.file.Path cas))
    (let [cas-ns (try (requiring-resolve 'evoclj.store.cas/get-bytes) (catch Exception _ nil))]
      (if cas-ns
        (bytes->string (cas-ns cas revision-id))
        (throw (err/error :context/materializer-missing-cas "evoclj.store.cas/get-bytes not available" {:revision-id revision-id}))))

    :else
    (throw (err/error :context/materializer-missing-cas "unsupported CAS resolver type" {:cas-type (type cas) :revision-id revision-id}))))

;; ---------------------------------------------------------------------------
;; Tree recognition (WO-S1: generic materializer detects a tree blob)
;; ---------------------------------------------------------------------------

(defn- tree-manifest?
  "True when the content string `s` parses as a CAS tree manifest
  (a map carrying :tree/version and :entries). Reproduces the shape
  produced by evoclj.fs.snapshot/snapshot-tree!; used only as a
  conservative discriminator, never as the source of content."
  [s]
  (when (string? s)
    (try
      (let [v (edn/read-string s)]
        (and (map? v)
             (contains? v :tree/version)
             (contains? v :entries)))
      (catch Exception _ false))))

(defn- tree-file-missing!
  "Throw typed :context/tree-file-missing (INV-04 fail-closed)."
  [tree-id path]
  (throw (err/error :context/tree-file-missing
                    "requested file is absent from the CAS tree"
                    {:tree/id tree-id :path path})))

(defn- hydrate-tree-by-path
  "Read `path` out of the CAS tree named by `tree-id`, returning its
  content string. The manifest is loaded from CAS (never from the
  catalog). A missing file, missing tree, or non-map manifest throws a
  typed error (fail-closed)."
  [cas tree-id path]
  (when-not cas
    (throw (err/error :context/materializer-missing-cas "CAS resolver required to materialize tree" {:tree/id tree-id :path path})))
  (let [manifest (snapshot/load-tree cas tree-id)]
    (when-not (map? manifest)
      (tree-file-missing! tree-id path))
    (let [ba (snapshot/get-file-bytes cas manifest path)]
      (when-not ba
        (tree-file-missing! tree-id path))
      (bytes->string ba))))

(defn- hydrate-tree-blob
  "Generic hydration: given the manifest string already fetched for
  `revision-id`, parse it and read SKILL.md out of that tree using the
  production snapshot reader (no second manifest fetch). Fail-closed on
  any missing/absent entry."
  [cas revision-id manifest-string]
  (let [manifest (edn/read-string manifest-string)]
    (when-not (map? manifest)
      (tree-file-missing! revision-id "SKILL.md"))
    (let [ba (snapshot/get-file-bytes cas manifest "SKILL.md")]
      (if ba
        (bytes->string ba)
        (tree-file-missing! revision-id "SKILL.md")))))

;; ---------------------------------------------------------------------------
;; Per-binding content resolution
;; ---------------------------------------------------------------------------

(defn- descriptor-kind
  "The explicit materializer kind on a binding, or nil when absent."
  [binding]
  (get-in binding [:binding/descriptor :type]))

(defn- resolve-content
  "Resolve content for a binding via CAS, tree-aware (WO-S1).

  Routing:
   - :binding/descriptor {:type :cas-tree-file :path <path>} -> read that
     file from the CAS tree at :revision/id;
   - :binding/descriptor {:type :cas-leaf} (or absent) -> generic: read
     the blob at :revision/id, hydrate SKILL.md when it is a tree
     manifest, otherwise treat it as a leaf string.

  Fail-closed: missing artifact, missing tree file, or an unsupported
  descriptor kind throw a typed error. The only inline-content fallback
  is the pre-resolved `:binding/content` carried by a binding when no CAS
  resolver is supplied (test helper), and even then a binding with
  neither CAS nor content fails closed."
  [binding cas]
  (let [revision-id (:revision/id binding)
        desc (:binding/descriptor binding)
        kind (when (map? desc) (:type desc))]
    (cond
      (= :cas-tree-file kind)
      (if cas
        (hydrate-tree-by-path cas revision-id (or (:path desc) "SKILL.md"))
        (throw (err/error :context/materializer-missing-cas
                          "CAS resolver required to materialize tree-file binding"
                          {:revision-id revision-id :type :cas-tree-file})))

      (= :cas-leaf kind)
      (if cas
        (fetch-leaf-cas cas revision-id)
        (if-let [c (:binding/content binding)]
          c
          (throw (err/error :context/materializer-missing-cas
                            "CAS resolver required to fetch immutable artifact"
                            {:revision-id revision-id}))))

      (some? kind)
      ;; an explicit but unknown descriptor kind is a defect, never a
      ;; silent heuristic fallback
      (throw (err/error :context/materializer-invalid
                        "unsupported binding materializer descriptor kind"
                        {:descriptor desc :revision-id revision-id}))

      :else
      ;; generic detection: tree blob vs leaf
      (if cas
        (let [raw (fetch-leaf-cas cas revision-id)]
          (if (tree-manifest? raw)
            (hydrate-tree-blob cas revision-id raw)
            raw))
        (if-let [c (:binding/content binding)]
          c
          (throw (err/error :context/materializer-missing-cas
                            "CAS resolver required to fetch immutable artifact"
                            {:revision-id revision-id})))))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn materialize
  "Materialize EffectiveContext from history, active bindings, catalog, policy, and CAS.

  Args map:
    :history   — string, compressed history (from compression loop)
    :bindings  — collection of ContextBinding maps (active)
    :catalog   — CatalogProjection (map or fn), currently unused for content but validated
    :policy    — HostPolicy map or nil (allow all)
    :cas       — CAS resolver (config map, artifact map, or CAS root/path)

  Returns:
    {:effective/history string
     :effective/segments [Segment ...]   ; materialized, policy-filtered, CAS-resolved
     :effective/context-string string     ; segments + history combined
     :effective/bindings [...] }          ; the filtered bindings

  Each segment's content is fetched via CAS from binding's :revision/id,
  never from catalog's current revision. Tree-backed bindings are
  hydrated from their tree file (WO-S1)."
  [{:keys [history bindings catalog policy cas]}]
  (when-not (or (nil? history) (string? history))
    (throw (err/error :context/materializer-invalid "history must be string or nil" {:history history})))
  (let [history (or history "")
        bindings (or bindings [])
        ;; policy filtering
        filtered (policy/filter-bindings policy bindings)
        ;; materialize each binding via CAS
        segments (mapv (fn [b]
                         (let [content (resolve-content b cas)]
                           (segment/segment-from-binding b content)))
                       filtered)
        ;; effective context string: segments injected before history
        ;; This keeps history compression isolated: compression never saw segments.
        context-str (if (seq segments)
                      (str (str/join "\n\n" (map :segment/content segments))
                           "\n\n--- HISTORY ---\n"
                           history)
                      history)]
    {:effective/history history
     :effective/segments segments
     :effective/context-string context-str
     :effective/bindings (vec filtered)
     :effective/catalog catalog}))

(defn effective-context
  "Alias for materialize, same signature.
  Kept for spec wording: EffectiveContext = Materialize(...)"
  [args]
  (materialize args))
