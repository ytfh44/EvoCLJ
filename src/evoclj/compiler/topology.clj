(ns evoclj.compiler.topology
  "Topology IR validation and compilation (Task 2.2).

  compile-topology turns a Genome's topology.edn value into validated,
  normalized adjacency IR that the runtime can execute without ever
  discovering malformed graph structure mid-task (Task 2.2 acceptance):

    {:graph/id :graph/main
     :entry :node/planner
     :nodes {node-id {:node/id node-id :node/type t ...}} ; sorted map
     :adjacency {node-id [successor-ids]}                 ; sorted map
     :limits {:max-steps 64}}

  Rules (Step 2): arbitrary graph cycles are rejected; only explicit
  :loop nodes may iterate. A cycle in the :next graph that contains no
  :loop node throws :topology/cycle. The :loop node's normative shape
  (Task 6.4) is validated here: :body (the node id iterated — must be
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

  Error types: :topology/invalid (malformed shapes, unknown node type,
  missing entry node, dangling :next, dangling :body, duplicate node
  ids, missing required node keys, invalid :limits, invalid
  :max-iterations — distinguished by :reason) and :topology/cycle (a
  raw cycle without a :loop node, with the sorted cycle node ids in
  :nodes)."
  (:require [evoclj.kernel.error :as err]))

(def supported-node-types
  "The normative v0 node type set."
  #{:llm :sci :tool :route :loop :emit :memory/read :memory/write})

(def ^:private required-keys
  "Per-type keys a node must declare. :loop carries its normative Task
  6.4 shape: :body (the iterated node id), :until (the done? program
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
  [id node]
  (let [t (:node/type node)]
    (when-not (contains? supported-node-types t)
      (throw (err/error :topology/invalid
                        "unsupported node type"
                        {:reason :unknown-node-type :node-id id :node/type t})))
    (doseq [k attribute-keys]
      (when (and (contains? node k) (not (keyword? (get node k))))
        (throw (err/error :topology/invalid
                          "node attribute must be a keyword"
                          {:reason :invalid-attribute :node-id id :key k
                           :value (err/sanitize (get node k))}))))
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
  :loop node whose :body points to an undeclared node id (Task 6.4:
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

  Returns a pure data map {:graph/id ... :entry ... :nodes {node-id
  {:node/id ... :node/type ...}} :adjacency {node-id [successors]}
  :limits {...}} with every map sorted, so identical logical topologies
  compile to equal values with identical serialization regardless of
  EDN key order (Step 4).

  Throws ExceptionInfo with a stable :error/type: :topology/invalid
  (malformed shapes; the :reason distinguishes :invalid-topology,
  :invalid-graph-id, :invalid-entry, :invalid-limits, :invalid-nodes,
  :invalid-node-entry, :invalid-node-id, :invalid-node,
  :unknown-node-type, :invalid-attribute, :missing-required-key,
  :dangling-next, :dangling-body, :invalid-max-iterations,
  :missing-entry, :duplicate-node-id) or
  :topology/cycle (a raw cycle with no :loop node; :nodes holds the
  sorted cycle ids)."
  [topology]
  (validate-shape! topology)
  (let [entries (node-entries (:nodes topology))
        node-map (normalize-nodes entries)
        node-ids (set (keys node-map))
        entry (:entry topology)]
    (check-entry! entry node-ids)
    (doseq [[id node] entries]
      (validate-node! id node))
    (check-nexts! entries node-ids)
    (check-cycles! entries node-map)
    (into (sorted-map-by canonical-compare)
          {:graph/id (:graph/id topology)
           :entry entry
           :nodes node-map
           :adjacency (build-adjacency node-map)
           :limits (or (:limits topology) {})})))
