(ns evoclj.genome.patch-edn
  "EDN and topology graph patch operations for component

  This namespace owns the EDN value ops (:set-edn :delete-edn) and the
  topology graph ops (:add-node :remove-node :add-edge :remove-edge
  :update-node). Each `apply-op` takes the CURRENT text of the target
  file plus the op map and returns the new text, so ops compose
  sequentially: the second op on a file sees the first op's output.

  Determinism (Global Constraints 1 and 6): every result is
  re-serialized by `canonical-str`, a deterministic canonical EDN
  writer — maps print with their entries sorted by the canonical string
  of the key, sets print with their elements sorted, and all scalars
  print via pr-str — so the same logical value always yields the same
  bytes, and therefore the same Genome hash, on every application.

  Topology graph ops edit the topology module as plain EDN and then
  validate the result through evoclj.compiler.topology/compile-topology
  (the plan's Step 7 rule: graph ops must validate their result via the
  topology compiler). A result the compiler rejects fails closed with
  the compiler's own typed error (:topology/invalid, :topology/cycle)
  before any candidate directory is finalized.

  Error contract: :patch/edn-invalid (the file does not parse as EDN),
  :patch/edn-path-invalid (a :path cannot be navigated on the current
  value), :patch/edge-not-found (a :remove-edge target does not exist),
  and the propagated :topology/* errors from the result gate."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.compiler.topology :as topology]
            [evoclj.kernel.error :as err]))

;; --- deterministic canonical EDN serialization -----------------------------

(declare canonical-str)

(defn- canonical-key
  "Sort key for map entries: the canonical string of the key."
  [k]
  (canonical-str k))

(defn- canonical-pair [k v]
  (str (canonical-str k) " " (canonical-str v)))

(defn canonical-str
  "Serialize `v` as deterministic canonical EDN text.

  Equal logical values always serialize to equal strings: map entries
  are sorted by the canonical string of their key, set elements are
  sorted by their canonical string, sequential collections keep their
  order (vectors print as [..], lists as (..)), and scalars print via
  pr-str. The output contains no host-specific or order-dependent
  information, so two equal values — from any EDN key order — produce
  identical bytes (Global Constraint 6)."
  [v]
  (cond
    (map? v) (str "{" (str/join " " (map (fn [[k x]] (canonical-pair k x))
                                         (sort-by (comp canonical-key key) v)))
                  "}")
    (set? v) (str "#{" (str/join " " (sort-by canonical-str v)) "}")
    (vector? v) (str "[" (str/join " " (map canonical-str v)) "]")
    (seq? v) (str "(" (str/join " " (map canonical-str v)) ")")
    :else (pr-str v)))

;; --- EDN parsing and path navigation ---------------------------------------

(defn- parse-edn!
  "Parse `content` with clojure.edn/read-string (never clojure.core
  read-string — no reader-eval, Global Constraint 22)."
  [content path]
  (try
    (edn/read-string content)
    (catch Exception e
      (throw (err/error :patch/edn-invalid
                        "target file is not valid EDN"
                        {:path path :message (.getMessage e)})))))

(defn- path-invalid!
  [path value]
  (throw (err/error :patch/edn-path-invalid
                    "EDN :path cannot be navigated on the current value"
                    {:path path :value (err/sanitize value)})))

(defn- delete-in
  "Delete the value selected by `path` from `m`. Path elements are
  keywords/strings (map keys) or positive integers (vector indices);
  the last element selects the entry to remove. Navigation into a
  non-map/non-vector intermediate fails closed."
  [m path]
  (if (= 1 (count path))
    (let [k (first path)]
      (cond
        (map? m) (dissoc m k)
        (vector? m) (into [] (keep-indexed (fn [i v] (when-not (= i k) v))) m)
        :else (path-invalid! path m)))
    (let [k (first path)
          sub (get m k)]
      (if (or (map? sub) (vector? sub))
        (assoc m k (delete-in sub (subvec path 1)))
        (path-invalid! path m)))))

(defn- canonical-edn-text
  "Serialize a validated value as canonical EDN with a trailing LF, the
  deterministic byte form used for every EDN file in a candidate."
  [v]
  (str (canonical-str v) "\n"))

;; --- EDN value ops ---------------------------------------------------------

(defn- set-edn-op [content op]
  (let [value (parse-edn! content (:file op))
        updated (try
                  (assoc-in value (:path op) (:value op))
                  (catch Exception e
                    (throw (err/error :patch/edn-path-invalid
                                      "EDN :path cannot be navigated on the current value"
                                      {:path (:path op)
                                       :message (.getMessage e)}))))]
    (canonical-edn-text updated)))

(defn- delete-edn-op [content op]
  (let [value (parse-edn! content (:file op))]
    (canonical-edn-text (delete-in value (:path op)))))

;; --- topology graph ops ----------------------------------------------------

(defn- topology-of
  "Parse the topology module value; it must be a map."
  [content file]
  (let [v (parse-edn! content file)]
    (when-not (map? v)
      (throw (err/error :patch/op-invalid
                        "topology module is not an EDN map"
                        {:path file})))
    v))

(defn- validate-topology!
  "Gate the edited topology through the topology compiler. The compiler
  rejects malformed graphs with stable :topology/* errors (Global
  Constraint 22: fail-closed at the boundary, never mid-task)."
  [topology-value]
  (topology/compile-topology topology-value)
  topology-value)

(defn- add-node-op [content op]
  (let [t (topology-of content (:file op))
        node (:node op)
        id (:node/id node)
        updated (update t :nodes (fn [nodes] (assoc (or nodes {}) id (merge node {:node/id id}))))]
    (canonical-edn-text (validate-topology! updated))))

(defn- remove-node-op [content op]
  (let [t (topology-of content (:file op))
        updated (update t :nodes (fn [nodes] (dissoc (or nodes {}) (:node/id op))))]
    (canonical-edn-text (validate-topology! updated))))

(defn- add-edge-op [content op]
  (let [t (topology-of content (:file op))
        {:keys [from to]} (:edge op)
        updated (assoc-in t [:nodes from :next] to)]
    (canonical-edn-text (validate-topology! updated))))

(defn- remove-edge-op [content op]
  (let [t (topology-of content (:file op))
        {:keys [from to]} (:edge op)
        node (get-in t [:nodes from])]
    (when (or (nil? node) (not= (:next node) to))
      (throw (err/error :patch/edge-not-found
                        "the edge to remove does not exist on the current topology"
                        {:from from :to to})))
    (canonical-edn-text (validate-topology! (update-in t [:nodes from] dissoc :next)))))

(defn- update-node-op [content op]
  (let [t (topology-of content (:file op))
        id (:node/id op)
        node (get-in t [:nodes id])]
    (when (nil? node)
      (throw (err/error :patch/op-invalid
                        "update-node targets an undeclared node"
                        {:node/id id})))
    (let [updated (assoc-in t [:nodes id]
                            (merge node (select-keys (:value op) (:update/keys op))))]
      (canonical-edn-text (validate-topology! updated)))))

;; --- public entry point ----------------------------------------------------

(defn apply-op
  "Apply one EDN or topology op to `content` (the current text of the
  op's target file). Returns the new text, or throws ExceptionInfo with
  a stable :error/type (see the namespace docstring)."
  [content op]
  (case (:op op)
    :set-edn (set-edn-op content op)
    :delete-edn (delete-edn-op content op)
    :add-node (add-node-op content op)
    :remove-node (remove-node-op content op)
    :add-edge (add-edge-op content op)
    :remove-edge (remove-edge-op content op)
    :update-node (update-node-op content op)
    (throw (err/error :patch/op-invalid
                      "unknown EDN/topology op"
                      {:op (:op op)}))))
