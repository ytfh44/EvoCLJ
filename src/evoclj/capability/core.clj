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
  (:require [evoclj.capability.schema :as lease-schema]
            [evoclj.kernel.error :as err]))

;; ----------------------------------------------------------------------
;; Sealed CapabilityHandle (S5)
;; ----------------------------------------------------------------------

(deftype CapabilityHandle [handle-id subject resource action lease-id ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :handle/id handle-id
      :subject subject
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
    [handle-id subject resource action lease-id]
    (CapabilityHandle. handle-id subject resource action lease-id capability-secret))

  (defn capability-handle?
    "True when x is a sealed CapabilityHandle produced via the authorized
    capability path. Arbitrary fns, maps, or records are never handles."
    [x]
    (and (instance? CapabilityHandle x)
         (identical? (.-secret ^CapabilityHandle x) capability-secret)))

  (defn make-capability-handle
    "Authorized construction of a CapabilityHandle for an allowed decision.
    Validates that lease-id is a uuid and subject/resource/action are
    present; throws :capability/handle-invalid on malformed input.
    This is the ONLY public way to obtain a handle — callers cannot
    synthesize a handle via map or fn."
    [{:keys [handle-id subject resource action lease-id]}]
    (when-not (uuid? handle-id)
      (throw (err/error :capability/handle-invalid "handle-id must be uuid" {:value handle-id})))
    (when-not (uuid? lease-id)
      (throw (err/error :capability/handle-invalid "lease-id must be uuid" {:value lease-id})))
    (when-not (map? subject)
      (throw (err/error :capability/handle-invalid "subject must be map" {:value subject})))
    (when-not (map? resource)
      (throw (err/error :capability/handle-invalid "resource must be map" {:value resource})))
    (when-not (keyword? action)
      (throw (err/error :capability/handle-invalid "action must be keyword" {:value action})))
    (->capability-handle handle-id subject resource action lease-id))

  (defn handle->lease-id
    "Extract lease-id from a sealed handle, or nil if not a handle."
    [h]
    (when (capability-handle? h)
      (.-lease_id ^CapabilityHandle h)))

  (defn handle->subject
    "Extract subject from a sealed handle, or nil if not a handle."
    [h]
    (when (capability-handle? h)
      (.-subject ^CapabilityHandle h))))

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
  "Return the static effect capabilities required by one topology node.
  These are categories, not grants: exact resource coverage remains the
  broker's lease decision."
  [node]
  (let [node-type (:node/type node)]
    (cond-> #{}
      (= :llm node-type) (conj :model/call)
      (and (= :llm node-type) (seq (:tools node))) (conj :tool/call)
      (= :tool node-type) (conj :tool/call)
      (= :memory/read node-type) (conj :memory/read)
      (= :memory/write node-type) (conj :memory/write))))

(defn topology-effects
  "Return the deterministic static effect set for a topology or a
  compiled topology. A topology's :nodes map is the only source used."
  [topology]
  (let [nodes (:nodes topology)]
    (if (map? nodes)
      (into #{} (mapcat node-effects) (vals nodes))
      #{})))

(defn intent-effect
  "Return the effect capability category for a validated intent, or nil
  for intents that do not cross an external effect boundary."
  [intent]
  (case (:intent/type intent)
    :intent/model-call :model/call
    :intent/tool-call :tool/call
    :intent/memory-read :memory/read
    :intent/memory-write :memory/write
    nil))

(defn lease-effects
  "Return the effect categories granted by one validated capability lease.
  Resource identity is deliberately not collapsed here; the broker still
  enforces exact subject/resource coverage for every request."
  [lease]
  (lease-schema/validate-lease lease)
  (let [kind (get-in lease [:resource :kind])
        actions (:actions lease)]
    (cond
      (and (= :tool kind) (contains? actions :invoke))
      #{:tool/call}

      (and (= :model kind) (contains? actions :invoke))
      #{:model/call}

      (= :memory kind)
      (cond-> #{}
        (contains? actions :read) (conj :memory/read)
        (contains? actions :write) (conj :memory/write))

      (contains? #{:filesystem :filesystem/path} kind)
      (into #{} (map #(keyword "filesystem" (name %))) actions)

      :else #{})))

(defn granted-effects
  "Return the union of effect categories represented by a collection of
  validated host-owned leases."
  [leases]
  (reduce into #{} (map lease-effects leases)))

(defn- validate-effect-set!
  [label effects]
  (when-not (set? effects)
    (throw (err/error :capability/lattice-invalid
                      "capability lattice members must be sets of keywords"
                      {:reason :invalid-set :set label :value effects})))
  (when-not (every? keyword? effects)
    (throw (err/error :capability/lattice-invalid
                      "capability lattice members must be sets of keywords"
                      {:reason :invalid-set :set label :value effects})))
  effects)

(defn- sorted-effects
  [effects]
  (vec (sort-by pr-str effects)))

(defn validate-effect-lattice!
  "Validate the three-layer capability lattice:

      Effects ⊆ Requested ⊆ Granted

  The two-argument form validates only the compile-time lower inclusion
  (Effects ⊆ Requested). The three-argument form also checks the runtime
  lease-derived Granted set. A nil Granted value explicitly means that
  no runtime grant set is available yet; an empty set means no effects are
  granted. Returns the normalized set map or throws
  :capability/lattice-invalid with a deterministic missing set."
  ([effects requested]
   (validate-effect-lattice! effects requested nil))
  ([effects requested granted]
   (let [effects (validate-effect-set! :effects effects)
         requested (validate-effect-set! :requested requested)
         granted (when (some? granted)
                   (validate-effect-set! :granted granted))
         missing-requested (remove requested effects)
         missing-granted (when granted (remove granted requested))]
     (when (seq missing-requested)
       (throw (err/error :capability/lattice-invalid
                         "topology effects must be declared in Requested"
                         {:reason :effect-not-requested
                          :effects (sorted-effects effects)
                          :requested (sorted-effects requested)
                          :missing (sorted-effects missing-requested)})))
     (when (seq missing-granted)
       (throw (err/error :capability/lattice-invalid
                         "Requested capabilities must be present in Granted"
                         {:reason :requested-not-granted
                          :requested (sorted-effects requested)
                          :granted (sorted-effects granted)
                          :missing (sorted-effects missing-granted)})))
     {:effects effects :requested requested :granted granted})))
