(ns evoclj.runtime.nodes.tool
  "The :tool node handler (Task 6.2).

  A :tool node only EMITS a typed intent — it never calls the provider
  (Global Constraint 8: every external effect must cross the
  kernel-owned Intent/Capability Broker, so the Task 6.3 scheduler
  dispatches the emitted intent). The handler builds a validated
  :intent/tool-call from the node config (:tool — the tool id) and the
  input-event's :payload (the args map), with attribution from
  runtime-state and the input-event (:session/id, :phenotype/id,
  :node/id, :cause/event-id — Global Constraint 20), via the pure
  evoclj.intent.core/tool-call constructor. The result is a :continue
  transition with no outputs and the node's :next as the successor."
  (:require [evoclj.intent.core :as intent]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.node :as node]))

(defn tool-handler
  "Construct the trusted :tool node handler.

  The node must carry :tool (a keyword — the :tool/id to request) and
  the input-event's :payload must be the args map ({:text \"hi\"} for
  :fixture/echo). Returns

    {:transition/status :continue
     :outputs []
     :intents [<validated :intent/tool-call>]
     :next [<node's :next>]}       ; [] when the node declares none

  validated against the shared transition schema."
  []
  (reify node/NodeHandler
    (step [_ runtime-state node input-event]
      (node/validate-runtime-state! runtime-state)
      (node/validate-node! node :tool)
      (node/validate-input-event! input-event)
      (let [args (:payload input-event)]
        (when-not (map? args)
          (throw (err/error :node/input-invalid
                            "tool node input payload must be the args map"
                            {:reason :args-invalid
                             :node/type :tool
                             :value (err/sanitize args)})))
        (let [budget (or (:budget runtime-state) node/default-budget)
              intent (intent/tool-call
                      (:session/id runtime-state)
                      (:phenotype/id runtime-state)
                      (:node/id runtime-state)
                      (:event/id input-event)
                      {:tool/id (:tool node) :args args}
                      budget)]
          (node/validate-transition!
           {:transition/status :continue
            :outputs []
            :intents [intent]
            :next (node/successor node)}))))))
