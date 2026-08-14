(ns evoclj.runtime.nodes.llm
  "The :llm node handler (post-v0 extension 1 — real model nodes).

  A :llm node only EMITS a typed :intent/model-call intent — it never
  calls a provider (Global Constraint 8: every external effect must
  cross the kernel-owned Intent/Capability Broker, so the Task 6.3
  scheduler dispatches the emitted intent). The node config declares
  the model ALIAS (:model, e.g. :planner); the handler resolves it
  against the phenotype's compiled Resolution
  (runtime-state :compiled :resolution :models) to the concrete full
  models.dev id (e.g. deepseek/deepseek-v4-flash) that the model
  registry keys on.

  The input-event payload becomes a single :user message. Optional
  node keys: :system (a system prompt string) and :options (call
  options such as {:temperature 0.5 :max-tokens 1024}). Attribution
  comes from runtime-state and the input-event (Global Constraint
  20)."
  (:require [evoclj.intent.core :as intent]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.node :as node]))

(defn llm-handler
  "Construct the trusted :llm node handler.

  The node must carry :model (the models.edn alias keyword). Returns
  a :continue transition carrying the validated :intent/model-call
  intent whose payload :model/id is the RESOLVED full models.dev id."
  []
  (reify node/NodeHandler
    (step [_ runtime-state node input-event]
      (node/validate-runtime-state! runtime-state)
      (node/validate-node! node :llm)
      (node/validate-input-event! input-event)
      (let [alias (:model node)
            resolution (get-in runtime-state [:compiled :resolution :models])
            resolved (get resolution alias)]
        (when-not (and (map? resolved) (:provider-model resolved))
          (throw (err/error :node/invalid
                            "llm node model alias is unresolved"
                            {:reason :model-unresolved :model alias})))
        (let [budget (or (:budget runtime-state) node/default-budget)
              user-msg {:role :user :content (str (:payload input-event))}
              messages (if (:system node)
                         [{:role :system :content (:system node)} user-msg]
                         [user-msg])
              payload {:model/id (:provider-model resolved)
                       :messages messages
                       :options (:options node)}
              intent (intent/model-call
                      (:session/id runtime-state)
                      (:phenotype/id runtime-state)
                      (:node/id runtime-state)
                      (:event/id input-event)
                      payload
                      budget)]
          (node/validate-transition!
           {:transition/status :continue
            :outputs []
            :intents [intent]
            :next (node/successor node)}))))))
