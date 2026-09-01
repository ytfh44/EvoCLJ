(ns evoclj.capability.core
  "Sealed CapabilityHandle — activation rights as capability, not fn?

  Fleet S5 (DAG S5/S6): replaces the fn? capability escape with a sealed
  handle. Activation rights are represented as a deftype value that can
  only be produced via the authorized path (broker authorize). Arbitrary
  fns are never accepted — activation only via handle, checked via
  capability-handle? (instance? + identical? on private secret).

  Pattern mirrors S4 ValidatedMutation: the secret object is never stored
  in a var and cannot be retrieved via #'var. The factory closes over the
  secret and stores it in a private field; capability-handle? checks
  instance? and identical? on the field via direct field access.

  Definition > validation: the handle is the definition; a raw fn is not a
  handle and fails closed with :capability/handle-invalid."
  (:require [clojure.set :as set]
            [evoclj.capability.schema :as lease-schema]
            [evoclj.kernel.error :as err]))
;; ----------------------------------------------------------------------
;; Sealed CapabilityHandle (S5)
;; ----------------------------------------------------------------------

(deftype CapabilityHandle [handle-id principal resource action lease-id ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :handle/id handle-id
      :principal principal
      :subject principal
      :resource resource
      :action action
      :lease-id lease-id
      notFound))
  clojure.lang.Counted
  (count [this] 5)
  Object
  (toString [this] (str "CapabilityHandle[" handle-id "->" lease-id "]")))
(alter-meta! #'->CapabilityHandle assoc :private true)

(let [capability-secret (Object.)]
  (defn- ->capability-handle
    "Private factory — only called from authorize path."
    [handle-id principal resource action lease-id]
    (CapabilityHandle. handle-id principal resource action lease-id capability-secret))

  (defn capability-handle?
    "True when x is a sealed CapabilityHandle produced via the authorized
    capability path. Arbitrary fns, maps, or records are never handles."
    [x]
    (and (instance? CapabilityHandle x)
         (identical? (.-secret ^CapabilityHandle x) capability-secret)))

  (defn make-capability-handle
    "Authorized construction of a CapabilityHandle for an allowed decision.
    Validates that lease-id is a uuid and principal/resource/action are
    present; throws :capability/handle-invalid on malformed input.
    This is the ONLY public way to obtain a handle — callers cannot
    synthesize a handle via map or fn."
    [{:keys [handle-id principal subject resource action lease-id]}]
    (let [p (or principal subject)]
      (when-not (uuid? handle-id)
        (throw (err/error :capability/handle-invalid "handle-id must be uuid" {:value handle-id})))
      (when-not (uuid? lease-id)
        (throw (err/error :capability/handle-invalid "lease-id must be uuid" {:value lease-id})))
      (when-not (map? p)
        (throw (err/error :capability/handle-invalid "principal must be map" {:value p})))
      (when-not (map? resource)
        (throw (err/error :capability/handle-invalid "resource must be map" {:value resource})))
      (when-not (keyword? action)
        (throw (err/error :capability/handle-invalid "action must be keyword" {:value action})))
      (->capability-handle handle-id p resource action lease-id)))

  (defn handle->lease-id
    "Extract lease-id from a sealed handle, or nil if not a handle."
    [h]
    (when (capability-handle? h)
      (.-lease_id ^CapabilityHandle h)))

  (defn handle->principal
    "Extract principal from a sealed handle, or nil if not a handle."
    [h]
    (when (capability-handle? h)
      (.-principal ^CapabilityHandle h)))

  (defn handle->subject
    "Deprecated alias for handle->principal."
    [h]
    (handle->principal h)))

(defn assert-capability-handle!
  "Fail-closed guard: throw :capability/handle-invalid when h is not a
  sealed CapabilityHandle. Activation paths must call this before
  invoking any effect — a raw fn never passes."
  [h]
  (when-not (capability-handle? h)
    (throw (err/error :capability/handle-invalid
                      "activation requires a sealed CapabilityHandle, not a raw fn or map"
                      {:value (try (str (type h)) (catch Exception _ "unknown"))})))
h)

;; ----------------------------------------------------------------------
;; Effect capability lattice (PLT5)
;; ----------------------------------------------------------------------

(defn node-effects
  "The set of effects a runtime node can produce, given its descriptor.
  Pure nodes produce no effect; tool nodes produce #{:tool/call}; llm
  nodes produce #{:model/call}."
  [node]
  (let [node-type (:node/type node)]
    (cond
      (= :tool node-type) #{:tool/call}
      (= :llm node-type) #{:model/call}
      (= :memory/read node-type) #{:memory/read}
      (= :memory/write node-type) #{:memory/write}
      :else #{})))

(defn topology-effects
  "The union of effects for all nodes in a compiled topology."
  [topology]
  (reduce into #{} (map node-effects (vals (:nodes topology)))))

(defn intent-effect
  "The effect keyword for an intent, or nil when no effect."
  [intent]
  (case (:intent/type intent)
    :intent/tool-call :tool/call
    :intent/model-call :model/call
    :intent/memory-read :memory/read
    :intent/memory-write :memory/write
    nil))

(defn lease-effects
  "The effect set a lease grants (derived from resource kind + action).
  Tool leases grant :tool/call; model leases grant :model/call; filesystem
  leases grant filesystem effects per action."
  [lease]
  (lease-schema/validate-lease lease)
  (let [kind (get-in lease [:resource :kind])
        actions (:actions lease)]
    (case kind
      :tool (when (contains? actions :invoke) #{:tool/call})
      :model (when (contains? actions :invoke) #{:model/call})
      :filesystem (set (map (fn [a] (keyword "filesystem" (name a))) actions))
      :filesystem/path (set (map (fn [a] (keyword "filesystem" (name a))) actions))
      :memory (when (contains? actions :invoke) #{:memory/read :memory/write})
      :else #{})))
(defn granted-effects
  "Union of effects granted by a collection of leases."
  [leases]
  (reduce into #{} (map lease-effects leases)))

(defn- validate-effect-set!
  [effects]
  (when-not (and (set? effects) (every? keyword? effects))
    (throw (err/error :capability/effect-invalid
                      "effects must be a set of keywords"
                      {:value effects})))
  effects)

(defn- sorted-effects
  [effects]
  (vec (sort-by pr-str effects)))

(defn validate-effect-lattice!
  "Validate that `effects` ⊆ `requested` ⊆ `granted`.
  Throws :capability/lattice-invalid with :reason :effect-not-requested
  or :requested-not-granted."
  [effects requested granted]
  (validate-effect-set! effects)
  (validate-effect-set! requested)
  (validate-effect-set! granted)
  (let [missing-req (set/difference effects requested)]
    (when (seq missing-req)
      (throw (err/error :capability/lattice-invalid
                        "effect not requested"
                        {:reason :effect-not-requested
                         :missing (vec (sort missing-req))}))))
  (let [missing-grant (set/difference requested granted)]
    (when (seq missing-grant)
      (throw (err/error :capability/lattice-invalid
                        "requested not granted"
                        {:reason :requested-not-granted
                         :missing (vec (sort missing-grant))})))
    {:effects effects :requested requested :granted granted}))
