(ns evoclj.promotion.activation
  "Sealed ActivationHandle — activation only via handle, not raw fn.

  Fleet S5 (DAG S5/S6): activation rights are not a fn? check. A raw fn
  is an ambient capability escape (any caller can synthesize a fn that
  captures authority). Instead, activation requires a sealed handle that
  can only be produced via the authorized promotion path.

  Pattern mirrors S4 ValidatedMutation and S5 CapabilityHandle: deftype
  with private secret, factory closed over secret, predicate checks
  identical?.

  Definition > validation: the handle is the definition; activation
  validates handle? and rejects a raw fn with :promotion/activation-denied
  (fail-closed). The handle carries the generation id it authorizes."
  (:require [evoclj.kernel.error :as err]))

;; ----------------------------------------------------------------------
;; Sealed ActivationHandle (S5)
;; ----------------------------------------------------------------------

(deftype ActivationHandle [generation-id handle-id ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :generation/id generation-id
      :handle/id handle-id
      notFound))
  Object
  (toString [this] (str "ActivationHandle[" generation-id "]")))
(alter-meta! #'->ActivationHandle assoc :private true)

(let [activation-secret (Object.)]
  (defn- ->activation-handle
    "Private sealed factory."
    [generation-id handle-id]
    (ActivationHandle. generation-id handle-id activation-secret))

  (defn activation-handle?
    "True when x is a sealed ActivationHandle produced via the authorized
    path. A raw fn is never an activation handle."
    [x]
    (and (instance? ActivationHandle x)
         (identical? (.-secret ^ActivationHandle x) activation-secret)))

  (defn make-activation-handle
    "Authorized construction of an ActivationHandle for the given
    generation. This is the ONLY way to obtain activation rights —
    callers cannot synthesize rights via (fn [] ...)."
    [generation-id]
    (when-not (string? generation-id)
      (throw (err/error :promotion/activation-denied "generation-id must be string" {:value generation-id})))
    (->activation-handle generation-id (java.util.UUID/randomUUID)))

  (defn handle-generation
    "Return generation-id of a sealed handle, or nil if not a handle."
    [h]
    (when (activation-handle? h)
      (.-generation_id ^ActivationHandle h))))

(defn assert-activation-handle!
  "Fail-closed guard: throw :promotion/activation-denied when h is not a
  sealed ActivationHandle. Promotion entry points must call this; a raw
  fn or map never passes."
  [h]
  (when-not (activation-handle? h)
    (throw (err/error :promotion/activation-denied
                      "activation requires a sealed ActivationHandle, not a raw fn"
                      {:value (try (str (type h)) (catch Exception _ "unknown"))
                       :hint "obtain a handle via make-activation-handle through the authorized kernel path"})))
  h)

(defn activate-with-handle
  "Invoke f only when handle is a sealed ActivationHandle. Throws
  :promotion/activation-denied for a raw fn or non-handle value.
  This replaces bare (fn? f) (f) checks — activation only via handle."
  [handle f & args]
  (assert-activation-handle! handle)
  (apply f args))
