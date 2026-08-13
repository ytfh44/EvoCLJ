(ns evoclj.runtime.nodes.emit
  "The :emit node handler (Task 6.2).

  :emit is TERMINAL: it completes the session with the accumulated
  outputs. The handler is pure — it reads the accumulated :outputs out
  of runtime-state and returns a :complete transition carrying them as
  the final session result (no :next, no intents). It never calls
  providers, the broker, or the store (Global Constraints 8, 20, 22);
  the input-event is ignored (a terminal node needs no input).

  The scheduler (Task 6.3) must treat a :complete transition as the
  end of the session and record its :outputs as the session result."
  (:require [evoclj.runtime.node :as node]))

(defn emit-handler
  "Construct the trusted :emit node handler. The handler validates
  runtime-state and the node, then returns

    {:transition/status :complete
     :outputs <accumulated outputs>
     :intents []}

  The result is validated against the shared transition schema before
  it is returned."
  []
  (reify node/NodeHandler
    (step [_ runtime-state node input-event]
      (node/validate-runtime-state! runtime-state)
      (node/validate-node! node :emit)
      (node/validate-transition!
       {:transition/status :complete
        :outputs (vec (:outputs runtime-state))
        :intents []}))))
