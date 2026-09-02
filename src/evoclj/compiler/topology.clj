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

  Rules (Step 2 / PLT2): sequential control flow uses :next edges.
  A :loop node is an explicit Region/Loop with :body, :exit, :until,
  and positive :max-iterations. Its body region must return to the
  loop through ordinary :next edges; :exit is the only normal successor.
  The normal control-flow graph follows :next for sequential nodes and
  :exit for loop nodes, so an arbitrary cycle — including an exit-edge
  bypass L -> X -> L — is rejected. The :body edge is represented in
  the loop region and is never treated as a free graph successor.

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
  (:require [evoclj.capability.core :as capability]
            [evoclj.kernel.error :as err]
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

(def ^:private required-keys
  "Per-type keys a node must declare. A :loop carries an explicit
  Region/Loop shape: :body is the iterated node id, :exit is the normal
  successor, :until is the done? program id, and :max-iterations is a
  positive integer."
  {:llm #{:model}
   :sci #{:program}
   :tool #{:tool}
   :route #{:next}
   :loop #{:exit :body :until :max-iterations}
   :emit #{}
   :memory/read #{:memory}
   :memory/write #{:memory}})

(def ^:private attribute-keys
  "Keys whose value must be a keyword when present."
  [:model :program :tool :memory :next :exit :body :until])

(def ^:private schema-keys
  "Optional typing keys whose value must be a registered schema keyword."
  [:input-schema :output-schema])

(def RegionSchema
  "The pure Region language used by the compiled topology. Seq and Branch
  are reserved structural forms; Loop is the currently executable form."
  [:multi {:dispatch :region/type}
   [:seq [:map {:closed true}
          [:region/type [:= :seq]]
          [:entry keyword?]
          [:nodes [:vector keyword?]]]]
   [:branch [:map {:closed true}
             [:region/type [:= :branch]]
             [:branches [:vector [:vector keyword?]]]]]
   [:loop [:map {:closed true}
           [:region/type [:= :loop]]
           [:body keyword?]
           [:exit keyword?]
           [:until keyword?]
           [:max pos-int?]]]])

(def Region
  "Alias for RegionSchema for callers that refer to the IR union as Region."
  RegionSchema)

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

(defn- map-of-subtype?
  "Check subtyping for homogeneous Malli :map-of schemas and for a
  closed explicit :map flowing into :map-of. An open explicit map can
  contain keys outside the target key schema, so it is not accepted."
  [output-form input-form]
  (let [input-key (second input-form)
        input-value (nth input-form 2)]
    (if (= :map-of (first output-form))
      (and (subtype-form? (second output-form) input-key)
           (subtype-form? (nth output-form 2) input-value))
      (let [[output-properties output-entries] (map-form-parts output-form)
            output-closed? (true? (:closed output-properties))
            output-entries (map map-entry-info output-entries)]
        (and output-closed?
             (every? (fn [[key {:keys [schema]}]]
                       (and (subtype-form? (schema-form key) input-key)
                            (subtype-form? schema input-value)))
                     output-entries))))))

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
    (and (vector? output-form) (= :or (first output-form))
         (vector? input-form) (= :or (first input-form)))
    (every? (fn [output-alternative]
              (some #(subtype-form? output-alternative %)
                    (next input-form)))
            (next output-form))
    (and (vector? input-form) (= :or (first input-form)))
    (some #(subtype-form? output-form %) (next input-form))
    (and (vector? output-form) (= :or (first output-form)))
    (every? #(subtype-form? % input-form) (next output-form))
    (and (vector? output-form) (vector? input-form))
    (case [(first output-form) (first input-form)]
      [:map :map] (map-subtype? output-form input-form)
      [:map :map-of] (map-of-subtype? output-form input-form)
      [:map-of :map-of] (map-of-subtype? output-form input-form)
      [:vector :vector] (subtype-form? (second output-form) (second input-form))
      [:set :set] (subtype-form? (second output-form) (second input-form))
      [:maybe :maybe] (subtype-form? (second output-form) (second input-form))
      [:maybe :or] (subtype-form? output-form input-form)
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
  "Enforce compositional edge typing. Sequential nodes use :next;
  Loop nodes use :body and :exit. Every edge requires
  output(from) <: input(to) under the PLT4 typing judgment."
  [node-map]
  (doseq [[from-id node] node-map]
    (let [edge-keys (if (= :loop (:node/type node))
                      [:body :exit]
                      [:next])]
      (doseq [edge-key edge-keys]
        (when-let [to-id (get node edge-key)]
          (when-let [to-node (get node-map to-id)]
            (let [from-kw (or (:output-schema node) :schema/any)
                  to-kw (or (:input-schema to-node) :schema/any)
                  from-schema (or (:schema/output node)
                                  (:output-schema/schema node)
                                  (:resolved/output-schema node)
                                  (when (:output-schema node)
                                    (resolve-node-schema!
                                     (:output-schema node) :output-schema from-id))
                                  (schema/resolve-schema :schema/any))
                  to-schema (or (:schema/input to-node)
                                (:input-schema/schema to-node)
                                (:resolved/input-schema to-node)
                                (when (:input-schema to-node)
                                  (resolve-node-schema!
                                   (:input-schema to-node) :input-schema to-id))
                                (schema/resolve-schema :schema/any))]
              (when-not (subtype? from-schema to-schema)
                (throw (err/error
                        :topology/type-mismatch
                        (str "edge type mismatch " from-id " -> " to-id
                             " (" from-kw " -> " to-kw ")")
                        {:reason :type-mismatch
                         :from from-id
                         :to to-id
                         :edge edge-key
                         :output-schema from-kw
                         :input-schema to-kw
                         :output-type from-schema
                         :input-type to-schema
                         :output-schema/schema from-schema
                         :input-schema/schema to-schema}))))))))))

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
                         :value (err/sanitize (:max-iterations node))})))

    (when (and (= :loop t) (contains? node :next))
      (throw (err/error :topology/invalid
                        "a :loop node must use :exit; :next is a sequential edge"
                        {:reason :loop-next-forbidden
                         :node-id id
                         :node/type t
                         :key :next})))
    node))

;; --- edge validation -------------------------------------------------------

(defn- check-nexts!
  "Reject dangling normal and loop-region edges. Sequential nodes use
  :next; explicit loops use :body and :exit."
  [entries node-ids]
  (doseq [[id node] entries]
    (let [edge-keys (if (= :loop (:node/type node))
                      [:body :exit]
                      [:next])]
      (doseq [edge-key edge-keys]
        (when-let [target (get node edge-key)]
          (when-not (contains? node-ids target)
            (throw (err/error :topology/invalid
                              "topology edge points to an undeclared node"
                              {:reason (if (= :body edge-key)
                                         :dangling-body
                                         :dangling-next)
                               :node-id id
                               edge-key target}))))))))

(defn- check-entry!
  [entry node-ids]
  (when-not (contains? node-ids entry)
    (throw (err/error :topology/invalid
                      "entry node is not declared in :nodes"
                      {:reason :missing-entry :entry entry}))))

;; --- cycle and Region validation (PLT2) ------------------------------------

(defn- normal-successor
  "The normal control-flow successor. A sequential node follows :next;
  a loop follows :exit. The :body edge is validated as a Region edge and
  is intentionally excluded from this graph."
  [node]
  (when node
    (if (= :loop (:node/type node))
      (:exit node)
      (:next node))))

(defn- invalid-loop-region!
  "Throw a deterministic error for a loop body that is not a bounded
  Region returning to its owning loop."
  [loop-id loop-node path detail]
  (throw (err/error :topology/invalid
                    "loop body must form a bounded region returning to its loop"
                    {:reason :invalid-loop-region
                     :detail detail
                     :loop/id loop-id
                     :body (:body loop-node)
                     :exit (:exit loop-node)
                     :path (vec path)})))

(defn- check-loop-regions!
  "Validate every Loop Region independently of the normal graph. Starting
  at :body, normal successors must eventually return to the owning loop;
  a body cannot be the loop itself, end at nil, revisit a node, or include
  the loop's :exit node."
  [node-map]
  (doseq [[loop-id loop-node] node-map
          :when (= :loop (:node/type loop-node))]
    (let [body (:body loop-node)
          exit (:exit loop-node)]
      (when (= body loop-id)
        (invalid-loop-region! loop-id loop-node [] :body-is-loop))
      (loop [id body
             path []
             seen #{}]
        (cond
          (= id loop-id)
          (when (some #{exit} path)
            (invalid-loop-region! loop-id loop-node path :body-exit-overlap))

          (nil? id)
          (invalid-loop-region! loop-id loop-node path :body-does-not-return)

          (contains? seen id)
          (invalid-loop-region! loop-id loop-node path :body-cycle)

          :else
          (if-let [node (get node-map id)]
            (recur (normal-successor node)
                   (conj path id)
                   (conj seen id))
            (invalid-loop-region! loop-id loop-node path :body-target-missing)))))))

(defn- walk-chain
  "Walk the functional normal control-flow graph starting at `start`
  without revisiting already-analyzed nodes. Returns [newly-visited-ids
  cycles] where each cycle is a vector of node ids in walk order."
  [start next-of visited]
  (loop [id start
         seen []
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
  "Reject every cycle in the normal control-flow graph. Since Loop nodes
  follow :exit here and their :body edge is excluded, both arbitrary
  sequential cycles and the illegal exit-edge bypass L -> X -> L are
  rejected. The controlled body -> ... -> L return is checked separately
  by check-loop-regions!."
  [entries node-map]
  (let [next-of (fn [id] (normal-successor (get node-map id)))
        cycles (loop [todo (seq (map first entries))
                      visited #{}
                      acc []]
                 (if-let [start (first todo)]
                   (if (contains? visited start)
                     (recur (next todo) visited acc)
                     (let [[new-visited new-cycles]
                           (walk-chain start next-of visited)]
                       (recur (next todo)
                              new-visited
                              (into acc new-cycles))))
                   acc))]
    (doseq [cycle cycles]
      (throw (err/error :topology/cycle
                        "normal control-flow cycles are rejected; Loop iterations must return through :body"
                        {:reason (if (some #(= :loop
                                                (get-in node-map [% :node/type]))
                                           cycle)
                                   :loop-control-cycle
                                   :cycle)
                         :nodes (vec (sort cycle))})))))

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

(defn- build-regions
  "Compile the explicit Loop Regions into a deterministic map keyed by
  their owning loop node id. The descriptor uses :max rather than the
  source node's :max-iterations so the Region language is independent
  of the node encoding."
  [node-map]
  (into (sorted-map-by canonical-compare)
        (keep (fn [[id node]]
                (when (= :loop (:node/type node))
                  [id (into (sorted-map-by canonical-compare)
                            {:region/type :loop
                             :body (:body node)
                             :exit (:exit node)
                             :until (:until node)
                             :max (:max-iterations node)})])))
        node-map))

(defn- build-adjacency
  "Sorted map of normal successor ids. Sequential nodes use :next;
  Loop nodes expose only their :exit. Their controlled :body edge lives
  in :regions and is not a free adjacency edge."
  [node-map]
  (into (sorted-map-by canonical-compare)
        (map (fn [[id node]]
               [id (if-let [nxt (normal-successor node)] [nxt] [])]))
        node-map))

;; --- public entry point ----------------------------------------------------

(defn compile-topology
  "Validate a Genome topology value and compile it into normalized
  adjacency IR.

  `topology` is the topology.edn value: {:graph/id <keyword> :entry
  <node-id> :nodes {node-id {:node/type <v0 type> ...}} :limits
  {:max-steps <pos-int>}}. Sequential nodes use :next; Loop nodes use
  :body, :exit, :until, and :max-iterations. :nodes may also be a
  vector of [node-id node] pairs, which is how duplicate ids can be
  expressed in pure EDN; they are rejected with :reason
  :duplicate-node-id.

  `executable-types` is the runtime feature set — the set of node types
  the target runtime can execute (handler exists). `capability-context`
  is optional {:requested <set> :granted <set>} and enables the full
  Effects ⊆ Requested ⊆ Granted check at compile time.

  Compositional typing (PLT4): each node declares optional :input-schema
  and :output-schema keywords (resolved via the closed schema registry,
  defaulting to :schema/any when absent). Sequential :next edges and a
  Loop's :body/:exit edges are checked with output(A) <: input(B).
  The normalized typing judgment is Gamma ; epsilon |- n : A -> B,
  with the Seq rule and explicit Loop Region back-edge validation.

  Returns a pure data map {:graph/id ... :entry ... :nodes {node-id
  {:node/id ... :node/type ... :input-schema ... :output-schema ... :schema/input ...}}
  :regions {loop-id {:region/type :loop :body ... :exit ... :until ... :max ...}}
  :effects #{...} :adjacency {node-id [normal-successors]} :limits {...}}
  with every map sorted and deterministic.

  Throws ExceptionInfo with a stable :error/type: :topology/invalid
  (malformed shapes; :reason distinguishes :invalid-topology,
  :invalid-graph-id, :invalid-entry, :invalid-limits, :invalid-nodes,
  :invalid-node-entry, :invalid-node-id, :invalid-node,
  :unknown-node-type, :invalid-attribute, :missing-required-key,
  :dangling-next, :dangling-body, :invalid-max-iterations,
  :loop-next-forbidden, :invalid-loop-region, :missing-entry,
  :duplicate-node-id, :unknown-schema, :invalid-capability-context),
  :topology/cycle (a normal control-flow cycle with sorted :nodes),
  :topology/type-mismatch (edge output not subtype of input, with
  :reason :type-mismatch, :from, :to, and :edge), or
  :topology/unsupported-node-type (a syntactically known type with no
  runtime handler; :reason :unsupported-node-type, :node/type carries
  the offending type; :route is the canonical example)."
  ([topology] (compile-topology topology executable-node-types nil))
  ([topology executable-types]
   (compile-topology topology executable-types nil))
  ([topology executable-types capability-context]
   (when-not (set? executable-types)
     (throw (err/error :topology/invalid
                       "executable-types must be a set of keywords"
                       {:reason :invalid-executable-types
                        :value (err/sanitize executable-types)})))
   (when-not (every? keyword? executable-types)
     (throw (err/error :topology/invalid
                       "executable-types must be a set of keywords"
                       {:reason :invalid-executable-types
                        :value (err/sanitize executable-types)})))
   (when (and capability-context (not (map? capability-context)))
     (throw (err/error :topology/invalid
                       "capability-context must be a map or nil"
                       {:reason :invalid-capability-context
                        :value (err/sanitize capability-context)})))
   (validate-shape! topology)
   (let [entries (node-entries (:nodes topology))
         node-map (normalize-nodes entries)
         node-ids (set (keys node-map))
         entry (:entry topology)]
     (check-entry! entry node-ids)
     (doseq [[id node] entries]
       (validate-node! id node executable-types))
     (check-nexts! entries node-ids)
     (check-loop-regions! node-map)
     (check-cycles! entries node-map)
     (let [typed-map (inject-typing! node-map)
           regions (build-regions typed-map)
           effects (capability/topology-effects {:nodes typed-map})
           compiled (into (sorted-map-by canonical-compare)
                          {:graph/id (:graph/id topology)
                           :entry entry
                           :nodes typed-map
                           :regions regions
                           :effects effects
                           :adjacency (build-adjacency typed-map)
                           :limits (or (:limits topology) {})})]
       (check-typing! typed-map)
       (when capability-context
         (capability/validate-effect-lattice!
          effects
          (or (:requested capability-context)
              (:requested-capabilities capability-context))
          (or (:granted capability-context)
              (:granted-effects capability-context))))
       compiled))))