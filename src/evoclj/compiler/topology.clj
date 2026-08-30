(ns evoclj.compiler.topology
  "Topology IR validation and compilation (component).

  compile-topology turns a Genome's topology.edn value into validated,
  normalized adjacency IR that the runtime can execute without ever
  discovering malformed graph structure mid-task (component acceptance):

    {:graph/id :graph/main
     :entry :node/planner
     :nodes {node-id {:node/id node-id :node/type t ...}} ; sorted map
     :adjacency {node-id [successor-ids]}                 ; sorted map
     :limits {:max-steps 64}}

  Rules (Step 2): arbitrary graph cycles are rejected; only explicit
  :loop nodes may iterate. A cycle in the :next graph that contains no
  :loop node throws :topology/cycle. The :loop node's normative shape
  (component) is validated here: :body (the node id iterated — must be
  a declared node, :reason :dangling-body), :until (a keyword program
  id), a positive integer :max-iterations (:reason
  :invalid-max-iterations), and :next (the exit node). The :body edge
  is the sanctioned iteration edge; the body node's :next back to the
  :loop closes the iteration at runtime under the :max-iterations
  cap.

  Properties:

  - The compiled value is pure, fully serializable EDN data (Global
    Constraint 22): every map is a sorted map, every node id is a
    keyword, and the result round-trips through pr-str /
    clojure.edn read-string.
  - Compilation is order-independent (Step 4): equal topologies in
    different EDN key order compile to equal values with identical
    serialization.
  - Node ids that would silently collapse when merged (duplicate ids)
    are rejected rather than dropped (Global Constraints 6 and 22: no
    silent data loss at a boundary).

  Compositional typing (PLT4):
    Each node carries :input-schema/:output-schema — keyword references
    that resolve via the closed Malli registry (evoclj.store.schema,
    Definition > validation: phantom keywords unrepresentable). After
    resolution the compiled node stores the Malli value under
    :schema/input, :schema/output, :input-schema/schema etc., plus the
    original keyword. The typing judgment is Gamma ; epsilon |- n : A -> B
    (node n in context Gamma with empty effect epsilon has input type A and
    output type B). The sequence rule is:
      Gamma |- n1 : A -> B   Gamma |- n2 : B -> C
      ------------------------------------------- (Seq)
      Gamma |- n1;n2 : A -> C
    i.e., for every edge A :next -> B, the compiler checks output(A) <: input(B)
    via malli subtype check. A mismatched edge is rejected at compile time with :topology/type-mismatch
    (Definition > validation: mismatched edge unrepresentable). Without
    explicit schemas a node defaults to :schema/any (top), so existing
    untyped topologies remain compatible.

  Error types: :topology/invalid (malformed shapes, unknown node type,
  missing entry node, dangling :next, dangling :body, duplicate node
  ids, missing required node keys, invalid :limits, invalid
  :max-iterations — distinguished by :reason), :topology/cycle (a
  raw cycle without a :loop node, with the sorted cycle node ids in
  :nodes), and :topology/unsupported-node-type (a syntactically known
  node type with no runtime handler — e.g. :route — rejected at
  compile time because Definition > validation: only executable types
  are representable via compile; the :reason is
  :unsupported-node-type and :node/type carries the offending type)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.store.schema :as schema]
            [malli.core :as m]))

(def syntax-node-types
  "The normative v0 syntax node type set — every type the compiler knows syntactically (definition).
  :route is syntactically valid but not executable without a handler; see executable-node-types."
  #{:llm :sci :tool :route :loop :emit :memory/read :memory/write})

(def executable-node-types
  "The subset of syntax-node-types the runtime can execute today (handler exists).
  Definition > validation: only executable types are representable via compile; :route is
  syntax-only until its handler lands, so compile rejects it with :topology/unsupported-node-type
  unless the caller provides an expanded runtime feature set that includes it."
  #{:llm :sci :tool :loop :emit :memory/read :memory/write})

(def supported-node-types
  "Legacy alias for syntax-node-types. Prefer syntax-node-types / executable-node-types.
  Kept for compatibility; new code should use executable-node-types for the runtime feature set
  and syntax-node-types for the full syntactic set."
  syntax-node-types)

(def ^:private required-keys
  "Per-type keys a node must declare. :loop carries its normative component shape: :body (the iterated node id), :until (the done? program
  id), a positive integer :max-iterations (checked in validate-node!),
  and :next (the exit node)."
  {:llm #{:model}
   :sci #{:program}
   :tool #{:tool}
   :route #{:next}
   :loop #{:next :body :until :max-iterations}
   :emit #{}
   :memory/read #{:memory}
   :memory/write #{:memory}})

(def ^:private attribute-keys
  "Keys whose value must be a keyword when present."
  [:model :program :tool :memory :next :body :until])

(def ^:private schema-keys
  "Optional typing keys whose value must be a registered schema keyword."
  [:input-schema :output-schema])

(def ^:private canonical-compare
  "Total order over canonical EDN scalar keys. Uses compare when the
  keys are mutually comparable; otherwise falls back to the canonical
  string form so mixed key types can never throw a host exception."
  (fn [a b]
    (let [c (try (compare a b)
                 (catch Exception _ ::incomparable))]
      (if (keyword? c)
        (compare (pr-str a) (pr-str b))
        (if (neg? c) -1 (if (pos? c) 1 0))))))

;; --- PLT4 typing helpers ----------------------------------------------------

(defn- resolve-node-schema!
  "Resolve schema keyword kw via closed registry, fail-closed for topology."
  [kw field node-id]
  (let [s (schema/resolve-schema kw)]
    (when-not s
      (throw (err/error :topology/invalid
                        (str "unknown schema keyword " kw " — not registered")
                        {:reason :unknown-schema :field field :node-id node-id :schema kw
                         :registered (vec (sort (keys (schema/schema-registry))))})))
    (try (m/schema s) (catch Exception _ (throw (err/error :topology/invalid "resolved schema is not a valid Malli schema" {:reason :invalid-schema :field field :node-id node-id :schema kw :value (err/sanitize s)})))) s))

(defn- schema-form
  "Return the normalized Malli form for a registered schema value, or nil
  when the value cannot be interpreted as a Malli schema."
  [schema-value]
  (try
    (m/form (m/schema schema-value))
    (catch Exception _ nil)))

(defn- map-entry-info
  "Decode one Malli map entry into [key {:optional? bool :schema form}]."
  [entry]
  (let [[key & tail] entry
        properties (when (map? (first tail)) (first tail))
        schema-value (if properties (second tail) (first tail))]
    [key {:optional? (true? (:optional properties))
          :schema (schema-form schema-value)}]))

(defn- map-form-parts
  "Return [properties entries] for a normalized Malli :map form."
  [form]
  (let [properties (if (map? (second form)) (second form) {})
        entries (if (map? (second form)) (nnext form) (next form))]
    [properties entries]))

(declare subtype-form?)

(defn- map-subtype?
  "Check structural subtyping for Malli map forms. An open output map is
  not a subtype of a closed input map because it may carry unknown keys;
  required input entries must be required and compatible in the output."
  [output-form input-form]
  (let [[output-properties output-entries] (map-form-parts output-form)
        [input-properties input-entries] (map-form-parts input-form)
        output-entries (into {} (map map-entry-info) output-entries)
        input-entries (into {} (map map-entry-info) input-entries)
        output-closed? (true? (:closed output-properties))
        input-closed? (true? (:closed input-properties))
        input-compatible?
        (every?
         (fn [[key {:keys [optional? schema]}]]
           (if-let [{output-optional? :optional?
                     output-schema :schema} (get output-entries key)]
             (and (or optional? (not output-optional?))
                  (subtype-form? output-schema schema))
             optional?))
         input-entries)
        no-unknown-keys?
        (or (not input-closed?)
            (and output-closed?
                 (every? #(contains? input-entries (key %)) output-entries)))]
    (and input-compatible? no-unknown-keys?)))

(defn- subtype-form?
  "A conservative structural Malli subtype relation for the closed registry.
  Equality and :any are handled for every schema; common collection forms
  are checked recursively, and unsupported forms fail closed."
  [output-form input-form]
  (cond
    (nil? output-form) false
    (nil? input-form) false
    (= input-form :any) true
    (= input-form [:any]) true
    (= output-form input-form) true
    (and (vector? input-form) (= :or (first input-form)))
    (some #(subtype-form? output-form %) (next input-form))
    (and (vector? output-form) (= :or (first output-form)))
    (every? #(subtype-form? % input-form) (next output-form))
    (and (vector? output-form) (vector? input-form))
    (case [(first output-form) (first input-form)]
      [:map :map] (map-subtype? output-form input-form)
      [:vector :vector] (subtype-form? (second output-form) (second input-form))
      [:set :set] (subtype-form? (second output-form) (second input-form))
      [:maybe :maybe] (subtype-form? (second output-form) (second input-form))
      [:maybe :or] (subtype-form? output-form input-form)
      [:or :or] (every? #(some (fn [candidate]
                                  (subtype-form? candidate (second input-form)))
                                (next input-form))
                        (next output-form))
      [:or :any] true
      [:enum :enum] (every? (set (next input-form)) (next output-form))
      [:= :=] (= (second output-form) (second input-form))
      [:= :enum] (contains? (set (next input-form)) (second output-form))
      [:and :and] (every? (fn [candidate]
                             (some #(subtype-form? candidate %)
                                   (next input-form)))
                           (next output-form))
      false)
    (and (= output-form :int) (= input-form :double)) false
    :else false))

(defn- subtype?
  "Check output(A) <: input(B) via Malli forms. The relation is
  structural for the registered primitive and collection schemas: equality,
  :any as top, and recursive map/vector/set/union checks."
  [output-schema input-schema]
  (subtype-form? (schema-form output-schema)
                 (schema-form input-schema)))

(defn- check-typing!
  "Enforce compositional edge typing: for every :next and :body edge, check output(from) <: input(to) via malli.
  Gamma |- n1 : A -> B   Gamma |- n2 : B -> C
  ------------------------------------------- (Seq)
  Gamma |- n1;n2 : A -> C
  i.e., for edge A :next/:body -> B, require output(A) <: input(B)."
  [node-map]
  (doseq [[from-id node] node-map]
    (doseq [edge-key [:next :body]]
      (when-let [to-id (get node edge-key)]
        (when-let [to-node (get node-map to-id)]
          (let [from-kw (or (:output-schema node) :schema/any)
                to-kw   (or (:input-schema to-node) :schema/any)
                from-schema (or (:schema/output node)
                                (:output-schema/schema node)
                                (:resolved/output-schema node)
                                (when (:output-schema node) (resolve-node-schema! (:output-schema node) :output-schema from-id))
                                (schema/resolve-schema :schema/any))
                to-schema   (or (:schema/input to-node)
                                (:input-schema/schema to-node)
                                (:resolved/input-schema to-node)
                                (when (:input-schema to-node) (resolve-node-schema! (:input-schema to-node) :input-schema to-id))
                                (schema/resolve-schema :schema/any))]
            (when-not (subtype? from-schema to-schema)
              (throw (err/error :topology/type-mismatch
                                (str "edge type mismatch " from-id " -> " to-id " (" from-kw " -> " to-kw ")")
                                {:reason :type-mismatch
                                 :from from-id
                                 :to to-id
                                 :edge edge-key
                                 :output-schema from-kw
                                 :input-schema to-kw
                                 :output-type from-schema
                                 :input-type to-schema
                                 :output-schema/schema from-schema
                                 :input-schema/schema to-schema})))))))))

;; --- top-level shape validation -------------------------------------------

(defn- validate-shape!
  [topology]
  (when-not (map? topology)
    (throw (err/error :topology/invalid
                      "topology must be a map"
                      {:reason :invalid-topology :value (err/sanitize topology)})))
  (when-not (keyword? (:graph/id topology))
    (throw (err/error :topology/invalid
                      "topology must declare a keyword :graph/id"
                      {:reason :invalid-graph-id :value (:graph/id topology)})))
  (when-not (keyword? (:entry topology))
    (throw (err/error :topology/invalid
                      "topology must declare a keyword :entry"
                      {:reason :invalid-entry :value (:entry topology)})))
  (let [limits (:limits topology)]
    (when (and limits
               (not (and (map? limits)
                         (or (not (contains? limits :max-steps))
                             (pos-int? (:max-steps limits))))))
      (throw (err/error :topology/invalid
                        ":limits must be a map with a positive integer :max-steps"
                        {:reason :invalid-limits :value limits})))))

;; --- node entry collection (duplicate detection) ---------------------------

(defn- node-entries
  "Collect :nodes (a map or a vector of [node-id node] pairs) into a
  vector of [id node] plain vectors. The pair form is the only way to
  express duplicate ids in pure EDN (map literals silently collapse), so
  it is merged with an explicit duplicate check: colliding ids throw
  :topology/invalid :reason :duplicate-node-id rather than silently
  dropping a node."
  [nodes]
  (when-not (or (map? nodes) (vector? nodes))
    (throw (err/error :topology/invalid
                      ":nodes must be a map or a vector of [node-id node] pairs"
                      {:reason :invalid-nodes :value (err/sanitize nodes)})))
  (let [raw (if (map? nodes) (seq nodes) nodes)
        entries (mapv (fn [entry]
                        (when-not (= 2 (count entry))
                          (throw (err/error :topology/invalid
                                            "node entries must be [node-id node] pairs"
                                            {:reason :invalid-node-entry
                                             :value (err/sanitize entry)})))
                        (let [[id node] entry]
                          (when-not (keyword? id)
                            (throw (err/error :topology/invalid
                                              "node id must be a keyword"
                                              {:reason :invalid-node-id :node-id id})))
                          (when-not (map? node)
                            (throw (err/error :topology/invalid
                                              "node must be a map"
                                              {:reason :invalid-node :node-id id
                                               :value (err/sanitize node)})))
                          [id node]))
                      raw)
        dupes (->> entries (group-by first) (filter #(> (count (val %)) 1)))]
    (when (seq dupes)
      (throw (err/error :topology/invalid
                        "duplicate node ids after merge"
                        {:reason :duplicate-node-id
                         :node-ids (mapv key dupes)})))
    entries))

;; --- per-node validation ---------------------------------------------------

(defn- validate-node!
  [id node executable-types]
  (let [t (:node/type node)]
    (when-not (contains? syntax-node-types t)
      (throw (err/error :topology/invalid
                        "unsupported node type"
                        {:reason :unknown-node-type :node-id id :node/type t})))
    (when-not (contains? executable-types t)
      (throw (err/error :topology/unsupported-node-type
                        "node type has no runtime handler (Definition > validation)"
                        {:reason :unsupported-node-type :node-id id :node/type t})))
    (doseq [k attribute-keys]
      (when (and (contains? node k) (not (keyword? (get node k))))
        (throw (err/error :topology/invalid
                          "node attribute must be a keyword"
                          {:reason :invalid-attribute :node-id id :key k
                           :value (err/sanitize (get node k))}))))
    (doseq [k schema-keys]
      (when (contains? node k)
        (let [v (get node k)]
          (when-not (keyword? v) (throw (err/error :topology/invalid "node schema must be a keyword" {:reason :invalid-attribute :node-id id :key k :value (err/sanitize v)})))
          (resolve-node-schema! v k id))))
    (doseq [k (get required-keys t)]
      (when-not (contains? node k)
        (throw (err/error :topology/invalid
                          "node missing required key"
                          {:reason :missing-required-key :node-id id
                           :node/type t :key k}))))
    (when (and (= :loop t)
               (not (pos-int? (:max-iterations node))))
      (throw (err/error :topology/invalid
                        "a :loop node must carry a positive integer :max-iterations"
                        {:reason :invalid-max-iterations :node-id id
                         :value (err/sanitize (:max-iterations node))})))))

;; --- edge validation -------------------------------------------------------

(defn- check-nexts!
  "Reject any :next edge that points to an undeclared node id, and any
  :loop node whose :body points to an undeclared node id (component:
  the :body edge is the sanctioned iteration edge, so a dangling
  :body must fail at compile time, never mid-task)."
  [entries node-ids]
  (doseq [[id node] entries]
    (when-let [nxt (:next node)]
      (when-not (contains? node-ids nxt)
        (throw (err/error :topology/invalid
                          "node :next points to an undeclared node"
                          {:reason :dangling-next :node-id id :next nxt}))))
    (when-let [body (:body node)]
      (when-not (contains? node-ids body)
        (throw (err/error :topology/invalid
                          "a :loop node :body points to an undeclared node"
                          {:reason :dangling-body :node-id id :body body}))))))

(defn- check-entry!
  [entry node-ids]
  (when-not (contains? node-ids entry)
    (throw (err/error :topology/invalid
                      "entry node is not declared in :nodes"
                      {:reason :missing-entry :entry entry}))))

;; --- cycle detection (Step 2) ---------------------------------------------

(defn- walk-chain
  "Walk the functional :next graph starting at `start` without revisiting
  already-analyzed nodes. Returns [newly-visited-ids cycles] where each
  cycle is a vector of node ids in walk order."
  [start next-of visited]
  (loop [id start
         seen []           ; ordered ids on the current walk
         seen-set #{}
         cycles []]
    (cond
      (nil? id)
      [(into visited seen) cycles]

      (contains? seen-set id)
      (let [idx (count (take-while #(not= % id) seen))
            cycle (subvec seen idx)]
        [(into visited seen) (conj cycles cycle)])

      (contains? visited id)
      [(into visited seen) cycles]

      :else
      (recur (next-of id) (conj seen id) (conj seen-set id) cycles))))

(defn- check-cycles!
  "Reject every cycle in the :next graph that contains no :loop node
  (Step 2: arbitrary graph cycles are rejected; only explicit :loop
  nodes may iterate). Offending cycle ids are reported sorted so error
  data is deterministic."
  [entries node-map]
  (let [next-of (fn [id] (:next (get node-map id)))
        cycles (loop [todo (seq (map first entries))
                      visited #{}
                      acc []]
                 (if-let [start (first todo)]
                   (if (contains? visited start)
                     (recur (next todo) visited acc)
                     (let [[new-visited new-cycles] (walk-chain start next-of visited)]
                       (recur (next todo) new-visited (into acc new-cycles))))
                   acc))]
    (doseq [cycle cycles]
      (when-not (some #(= :loop (get-in node-map [% :node/type])) cycle)
        (throw (err/error :topology/cycle
                          "arbitrary graph cycles are rejected; only :loop nodes may iterate"
                          {:nodes (vec (sort cycle))}))))))

;; --- normalization ---------------------------------------------------------

(defn- normalize-nodes
  "Sorted map of node id to its normalized node map (keys sorted,
  :node/id injected so every node is self-describing)."
  [entries]
  (into (sorted-map-by canonical-compare)
        (map (fn [[id node]]
               [id (into (sorted-map-by canonical-compare)
                         (assoc node :node/id id))]))
        entries))

(defn- inject-typing!
  "PLT4: resolve :input-schema/:output-schema via closed registry and inject resolved Malli values."
  [node-map]
  (into (sorted-map-by canonical-compare)
        (map (fn [[id node]]
               (let [in-kw (or (:input-schema node) :schema/any)
                     out-kw (or (:output-schema node) :schema/any)
                     in-schema (resolve-node-schema! in-kw :input-schema id)
                     out-schema (resolve-node-schema! out-kw :output-schema id)]
                 [id (into (sorted-map-by canonical-compare)
                           (assoc node :input-schema in-kw :output-schema out-kw :schema/input in-schema :schema/output out-schema :input-schema/schema in-schema :output-schema/schema out-schema :resolved/input-schema in-schema :resolved/output-schema out-schema))]))
        node-map)))

(defn- build-adjacency
  "Sorted map of node id to the vector of successor node ids reachable
  via :next (empty vector for terminal nodes)."
  [node-map]
  (into (sorted-map-by canonical-compare)
        (map (fn [[id node]]
               [id (if-let [nxt (:next node)] [nxt] [])]))
        node-map))

;; --- public entry point ----------------------------------------------------

(defn compile-topology
  "Validate a Genome topology value and compile it into normalized
  adjacency IR.

  `topology` is the topology.edn value: {:graph/id <keyword> :entry
  <node-id> :nodes {node-id {:node/type <v0 type> ...}} :limits
  {:max-steps <pos-int>}}. :nodes may also be a vector of [node-id node]
  pairs, which is how duplicate ids can be expressed in pure EDN; they
  are rejected with :reason :duplicate-node-id.

  `executable-types` is the runtime feature set — the set of node types
  the target runtime can execute (handler exists). Definition >
  validation: only executable types are representable via compile. By
  default it is executable-node-types, so a topology containing :route
  (syntax-only, no handler) fails with :topology/unsupported-node-type
  unless the caller provides an expanded set that includes :route.

  Compositional typing (PLT4): each node declares optional :input-schema
  and :output-schema keywords (resolved via the closed schema registry,
  defaulting to :schema/any when absent). For every edge A :next -> B
  (and :body) the compiler checks output(A) <: input(B) via malli
  subtype (m/validate); a mismatched edge throws
  :topology/type-mismatch :reason :type-mismatch (Definition > validation).
  Judgment: Gamma ; epsilon |- n : A -> B, with Seq rule Gamma |- n1:A->B, n2:B->C => n1;n2:A->C.

  Returns a pure data map {:graph/id ... :entry ... :nodes {node-id
  {:node/id ... :node/type ... :input-schema ... :output-schema ... :schema/input ...}} :adjacency {node-id [successors]}
  :limits {...}} with every map sorted, so identical logical topologies
  compile to equal values with identical serialization regardless of
  EDN key order (Step 4).

  Throws ExceptionInfo with a stable :error/type: :topology/invalid
  (malformed shapes; the :reason distinguishes :invalid-topology,
  :invalid-graph-id, :invalid-entry, :invalid-limits, :invalid-nodes,
  :invalid-node-entry, :invalid-node-id, :invalid-node,
  :unknown-node-type, :invalid-attribute, :missing-required-key,
  :dangling-next, :dangling-body, :invalid-max-iterations,
  :missing-entry, :duplicate-node-id, :unknown-schema), :topology/cycle (a raw cycle
  with no :loop node; :nodes holds the sorted cycle ids), :topology/type-mismatch
  (edge output not subtype of input, :reason :type-mismatch, :from :to, :output-type :input-type),
  or :topology/unsupported-node-type (a syntactically known type with no
  runtime handler; :reason :unsupported-node-type, :node/type carries
  the offending type; :route is the canonical example)."
  ([topology] (compile-topology topology executable-node-types))
  ([topology executable-types]
   (when-not (set? executable-types)
     (throw (err/error :topology/invalid
                       "executable-types must be a set of keywords"
                       {:reason :invalid-executable-types :value (err/sanitize executable-types)})))
   (when-not (every? keyword? executable-types)
     (throw (err/error :topology/invalid
                       "executable-types must be a set of keywords"
                       {:reason :invalid-executable-types :value (err/sanitize executable-types)})))
   (validate-shape! topology)
   (let [entries (node-entries (:nodes topology))
         node-map (normalize-nodes entries)
         node-ids (set (keys node-map))
         entry (:entry topology)]
     (check-entry! entry node-ids)
     (doseq [[id node] entries]
       (validate-node! id node executable-types))
     (check-nexts! entries node-ids)
     (check-cycles! entries node-map)
     ;; PLT4: inject typing (defaults to :schema/any) and enforce edge typing
     ;; The typing judgment is Gamma ; epsilon |- n : A -> B, with Seq rule.
     (let [typed-map (inject-typing! node-map)]
       (check-typing! typed-map)
       (into (sorted-map-by canonical-compare)
             {:graph/id (:graph/id topology)
              :entry entry
              :nodes typed-map
              :adjacency (build-adjacency typed-map)
              :limits (or (:limits topology) {})})))))