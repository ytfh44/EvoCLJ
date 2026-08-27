(ns evoclj.runtime.assembler
  "RequestAssembler — trusted runtime that synthesizes PreparedModelCall
  from BaseModelCall plus dynamic environment.

  Node declares BaseModelCall, host decides final world. The assembler
  merges history, active bindings, catalog, and tool catalog binding
  into a wire-ready request. Tool catalog is pinned for a whole
  tool-loop; context is rebuilt each round so activate_skill becomes
  visible immediately."
  (:require [evoclj.context.materializer :as mat]
            [evoclj.context.prompt-trust :as trust]
            [evoclj.environment.revision :as rev]
            [evoclj.kernel.error :as err]))

(defn base->prepared
  "Assemble PreparedModelCall.

  base-call: {:base/messages [...] :requested-tools ... :options ...}
  session-bindings: seq of ContextBinding
  catalog: map source-id -> revision-id (current catalog)
  tool-catalog-binding: {:binding/id :revision/ids ...} or nil
  history: string or vector of messages
  opts: {:cas <cas-config-or-map-or-path> :policy <host-policy>}

  Returns a PreparedModelCall map that additionally carries
  :prompt/provenance — a structured header attributing each message to a
  source/trust level (:kernel/:extra/:user/:model). Kernel instructions
  are always emitted first and are non-overridable by lower-trust
  content (S13)."

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
         effective (if (seq session-bindings)
                     (if cas
                       (mat/materialize {:history history
                                         :bindings session-bindings
                                         :catalog catalog
                                         :policy policy
                                         :cas cas})
                       ;; WO-S1 / INV-04: an unresolved placeholder (a
                       ;; skill/artifact reference we cannot resolve) FAILS
                       ;; CLOSED — never emit a degraded "binding:..."
                       ;; placeholder segment.
                        (throw (err/error :assembler/placeholder-unresolved
                                          "cannot resolve session binding placeholders without a CAS resolver"
                                          {:bindings (mapv :logical/id session-bindings)})))
                     {:effective/history history
                      :effective/segments []
                      :effective/bindings []})
         segments (:effective/segments effective [])
         ;; S13 PROVENANCE + KERNEL PRIORITY: the trusted assembler must
         ;; (1) tag each message with a provenance header and (2) emit
         ;; kernel instructions first, before any lower-trust extra/user
         ;; content. base-call messages are classified as :kernel/:user/
         ;; :model by role; skill segments are :extra. `prioritized-prompt`
         ;; orders them kernel > extra > user > model and builds the
         ;; :prompt/provenance header; it fails closed (typed) when a
         ;; message cannot be attributed (INV-04/09 style).
         seg-messages (mapv (fn [seg] {:role "system"
                                       :content (or (:segment/content seg) (str seg))})
                            segments)
         {messages :messages provenance :prompt/provenance} (trust/prioritized-prompt
                                                             (merge (trust/split-base-messages base-messages)
                                                                    {:extra seg-messages}))
         ;; tool-map: wire name -> binding (for scheduler). The scheduler
         ;; resolves model-requested calls by the WIRE function name
         ;; (:tool/name tc), and the wire name is the declaration's :name
         ;; (see evoclj.provider.openai/wire-tools and the scheduler's
         ;; tool-map-of) — never :tool/id (the EvoCLJ tool identity), which
         ;; is a separate key (:tool) that must not double as the wire name.
         tool-map (into {} (map (fn [t] [(:name t) t]) (or requested-tools [])))
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
      :prompt/provenance provenance
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

(defn capture-tool-catalog-binding
  "Alias for pin-catalog for scheduler compatibility."
  [catalog]
  (pin-catalog catalog))

(defn base-call-from-intent
  "Extract BaseModelCall from an intent. Tolerates both new and legacy payload shapes."
  [intent]
  {:base/messages (or (get-in intent [:payload :base/messages])
                      (get-in intent [:payload :messages]) [])
   :requested-tools (or (get-in intent [:payload :requested-tools])
                        (get-in intent [:payload :tools]) [])
   :options (or (get-in intent [:payload :options]) {})
   :model/id (get-in intent [:payload :model/id])})

(defn assemble
  "Scheduler-facing wrapper for base->prepared. Takes base-call and opts map with
  :session-bindings, :tool-catalog/binding, :cas, :history, :policy, :catalog."
  [base-call opts]
  (let [session-bindings (:session-bindings opts)
        tool-binding (:tool-catalog/binding opts)
        catalog (:catalog opts)
        cas (:cas opts)
        history (:history opts)
        policy (:policy opts)]
    (base->prepared base-call (or session-bindings []) (or catalog {}) tool-binding (or history "") {:cas cas :policy policy})))

(defn rebuild-context
  "Rebuild only the context portion for next round, keeping pinned
  tool catalog. Used when activate_skill was called in previous round."
  [prepared session-bindings catalog history opts]
  (let [base (:base prepared)
        pinned (:tool-catalog/binding prepared)]
    (base->prepared base session-bindings catalog pinned history opts)))
