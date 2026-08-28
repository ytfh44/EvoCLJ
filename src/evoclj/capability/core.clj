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
  (:require [evoclj.kernel.error :as err]))

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
