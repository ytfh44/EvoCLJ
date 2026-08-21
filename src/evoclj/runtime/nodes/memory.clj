(ns evoclj.runtime.nodes.memory
  "The :memory/read and :memory/write node handlers (feature R1).

  A :memory node only EMITS a typed intent — it never touches the
  store or the provider (Global Constraint 8: every external effect
  must cross the kernel-owned Intent/Capability Broker, so the component scheduler dispatches the emitted intent). The handler builds a
  validated :intent/memory-read or :intent/memory-write from the node
  config and the runtime-state, with attribution from runtime-state and
  the input-event (:session/id, :phenotype/id, :node/id,
  :cause/event-id — Global Constraint 20), via the pure
  evoclj.intent.core/memory-read / memory-write constructors. The
  result is a :continue transition with no outputs (the provider value
  is fed back by the scheduler as the next node's input) and the
  node's :next as the successor.

  NODE SHAPE. The node's memory key is the topology-declared :memory
  attribute (a keyword, required by evoclj.compiler.topology). For a
  :memory/read node the key names the episodic slot to read; an
  optional :limit node key (a non-negative integer) is forwarded as
  :memory/limit. For a :memory/write node the value to store comes from
  the node's :value key; when :value is absent the handler falls back
  to the PREVIOUS output (the last element of runtime-state :outputs,
  i.e. the input-event's :payload) so a :memory/write can persist the
  most recent step's value without duplicating it in the topology.
  Episodic memory writes stay distinct from procedural Genome changes
  (Global Constraint 10)."
  (:require [evoclj.intent.core :as intent]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.node :as node]))

(defn- memory-key
  "The node's episodic memory key: the topology-declared :memory
  attribute, required to be a keyword (the compiler's attribute rule)."
  [node]
  (let [k (:memory node)]
    (when-not (keyword? k)
      (throw (err/error :node/invalid
                        "a memory node must carry a keyword :memory key"
                        {:reason :invalid-attribute
                         :node/type (:node/type node)
                         :key k
                         :value (err/sanitize k)})))
    k))

(defn- memory-write-value
  "The value a :memory/write node stores: the node's :value key when
  present, otherwise the PREVIOUS output (the last accumulated output,
  which is the current input-event's :payload)."
  [node runtime-state input-event]
  (if (contains? node :value)
    (:value node)
    (:payload input-event)))

(defn- memory-read-intent
  [runtime-state input-event node key budget]
  (intent/memory-read
   (:session/id runtime-state)
   (:phenotype/id runtime-state)
   (:node/id runtime-state)
   (:event/id input-event)
   (cond-> {:memory/key key}
     (contains? node :limit) (assoc :memory/limit (:limit node)))
   budget))

(defn- memory-write-intent
  [runtime-state input-event node key value budget]
  (intent/memory-write
   (:session/id runtime-state)
   (:phenotype/id runtime-state)
   (:node/id runtime-state)
   (:event/id input-event)
   {:memory/key key :memory/content value}
   budget))

(defn- validate-limit!
  "A :memory/read node's :limit must be a non-negative integer."
  [node]
  (when (contains? node :limit)
    (let [l (:limit node)]
      (when-not (and (int? l) (not (neg? l)))
        (throw (err/error :node/invalid
                          "a :memory/read node :limit must be a non-negative integer"
                          {:reason :invalid-attribute
                           :node/type :memory/read
                           :key :limit
                           :value (err/sanitize l)}))))))

(defn- memory-read-handler
  "The trusted :memory/read node handler."
  []
  (reify node/NodeHandler
    (step [_ runtime-state node input-event]
      (node/validate-runtime-state! runtime-state)
      (node/validate-node! node :memory/read)
      (node/validate-input-event! input-event)
      (validate-limit! node)
      (let [budget (or (:budget runtime-state) node/default-budget)
            intent (memory-read-intent runtime-state input-event node
                                       (memory-key node) budget)]
        (node/validate-transition!
         {:transition/status :continue
          :outputs []
          :intents [intent]
          :next (node/successor node)})))))

(defn- memory-write-handler
  "The trusted :memory/write node handler."
  []
  (reify node/NodeHandler
    (step [_ runtime-state node input-event]
      (node/validate-runtime-state! runtime-state)
      (node/validate-node! node :memory/write)
      (node/validate-input-event! input-event)
      (let [budget (or (:budget runtime-state) node/default-budget)
            value (memory-write-value node runtime-state input-event)
            intent (memory-write-intent runtime-state input-event node
                                        (memory-key node) value budget)]
        (node/validate-transition!
         {:transition/status :continue
          :outputs []
          :intents [intent]
          :next (node/successor node)})))))

(defn read-handler
  "Construct the trusted :memory/read node handler."
  []
  (memory-read-handler))

(defn write-handler
  "Construct the trusted :memory/write node handler."
  []
  (memory-write-handler))
