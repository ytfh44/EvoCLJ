(ns evoclj.runtime.assembler
  "RequestAssembler — trusted runtime that synthesizes PreparedModelCall
  from BaseModelCall plus dynamic environment.

  Node declares BaseModelCall, host decides final world. The assembler
  merges history, active bindings, catalog, and tool catalog binding
  into a wire-ready request. Tool catalog is pinned for a whole
  tool-loop; context is rebuilt each round so activate_skill becomes
  visible immediately."
  (:require [evoclj.context.materializer :as mat]
            [evoclj.environment.revision :as rev]))

(defn base->prepared
  "Assemble PreparedModelCall.

  base-call: {:base/messages [...] :requested-tools ... :options ...}
  session-bindings: seq of ContextBinding
  catalog: map source-id -> revision-id (current catalog)
  tool-catalog-binding: {:binding/id :revision/ids ...} or nil
  history: string or vector of messages
  opts: {:cas <cas-config-or-fn> :policy <host-policy>}"

  ([base-call session-bindings catalog tool-catalog-binding]
   (base->prepared base-call session-bindings catalog tool-catalog-binding nil {}))
  ([base-call session-bindings catalog tool-catalog-binding history]
   (base->prepared base-call session-bindings catalog tool-catalog-binding history {}))
  ([base-call session-bindings catalog tool-catalog-binding history {:keys [cas policy]}]
   (let [base-messages (:base/messages base-call (:messages base-call []))
         requested-tools (:requested-tools base-call (:tools base-call))
         ;; tool catalog: use provided binding or derive from catalog
         tool-catalog (or tool-catalog-binding
                          {:binding/id (random-uuid)
                           :revision-ids catalog})
         ;; context materialization: rebuild each time
         effective (if (and (seq session-bindings) cas)
                     (mat/materialize {:history history
                                       :bindings session-bindings
                                       :catalog catalog
                                       :policy policy
                                       :cas cas})
                     {:effective/history history
                      :effective/segments (mapv (fn [b] {:segment/logical-id (:logical/id b)
                                                         :segment/revision-id (:revision/id b)
                                                         :segment/content (str "binding:" (:logical/id b))})
                                                session-bindings)
                      :effective/bindings session-bindings})
         segments (:effective/segments effective [])
         ;; inject segments as system messages before base messages
         seg-messages (mapv (fn [seg] {:role "system"
                                       :content (or (:segment/content seg) (str seg))})
                            segments)
         messages (into (vec seg-messages) (vec base-messages))
         ;; tool-map: wire name -> binding (for scheduler)
         tool-map (into {} (map (fn [t] [(:tool/id t) t]) (or requested-tools [])))
         manifest {:context/manifest-version 1
                   :bindings (mapv (fn [b] {:binding/id (:binding/id b)
                                            :logical/id (:logical/id b)
                                            :revision/id (:revision/id b)})
                                   session-bindings)
                   :tool-catalog tool-catalog
                   :history (if (string? history) {:text history} {:messages history})}]
     {:messages messages
      :tools (or requested-tools [])
      :tool-map tool-map
      :context/manifest manifest
      :tool-catalog/binding tool-catalog
      :environment/provenance {:catalog catalog
                               :tool-catalog tool-catalog
                               :bindings manifest}
      :base base-call
      :effective effective})))

(defn pin-catalog
  "Capture tool catalog binding at start of a tool-loop. Returns the
  binding to be pinned and reused across rounds."
  [catalog]
  {:binding/id (random-uuid)
   :revision-ids catalog
   :captured-at (System/currentTimeMillis)})

(defn rebuild-context
  "Rebuild only the context portion for next round, keeping pinned
  tool catalog. Used when activate_skill was called in previous round."
  [prepared session-bindings catalog history opts]
  (let [base (:base prepared)
        pinned (:tool-catalog/binding prepared)]
    (base->prepared base session-bindings catalog pinned history opts)))
