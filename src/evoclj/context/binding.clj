(ns evoclj.context.binding
  "ContextBinding — session's activated exact revision.

  A ContextBinding pins a logical context (e.g., [:skill \"debugging\"] )
  to an exact immutable revision. It is created from a ContextOffer at
  activation time and never mutates to track catalog movement.

  Shape:
    {:binding/id          #uuid \"…\"               ; unique binding instance
     :logical/id          [:skill \"debugging\"]    ; logical identifier
     :revision/id         \"sha256:…\"              ; content identity (CAS key)
     :bundle/id           \"bundle:…\"              ; bundle that was activated
     :scope               :session                  ; always :session in v0
     :state               :active                   ; :active | :inactive
     :binding/activated-at <long millis>}           ; when activated

  Store: in-memory atom registry for session bindings.
  Materializer reads content via CAS from :revision/id, not from catalog."
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Binding validation
;; ---------------------------------------------------------------------------

(defn binding?
  "True when x is a valid ContextBinding map."
  [x]
  (and (map? x)
       (uuid? (:binding/id x))
       (vector? (:logical/id x))
       (string? (:revision/id x))
       (types/artifact-id? (:revision/id x))
       (string? (:bundle/id x))
       (= :session (:scope x))
       (= :active (:state x))
       (int? (:binding/activated-at x))))

(defn validate-binding
  "Validate binding, throwing :context/binding-invalid on failure."
  [b]
  (when-not (uuid? (:binding/id b))
    (throw (err/error :context/binding-invalid "binding/id must be uuid" {:binding b})))
  (when-not (vector? (:logical/id b))
    (throw (err/error :context/binding-invalid "logical/id must be vector" {:binding b})))
  (when-not (types/artifact-id? (:revision/id b))
    (throw (err/error :context/binding-invalid "revision/id must be sha256:<64 hex>" {:binding b})))
  (when-not (and (string? (:bundle/id b)) (seq (:bundle/id b)))
    (throw (err/error :context/binding-invalid "bundle/id must be non-empty string" {:binding b})))
  (when-not (= :session (:scope b))
    (throw (err/error :context/binding-invalid "scope must be :session" {:binding b})))
  (when-not (= :active (:state b))
    (throw (err/error :context/binding-invalid "state must be :active" {:binding b})))
  b)

(defn make-binding
  "Create a ContextBinding from explicit parts. Validates and returns binding."
  [{:keys [logical-id revision-id bundle-id scope state activated-at binding-id]}]
  (let [b {:binding/id (or binding-id (random-uuid))
           :logical/id logical-id
           :revision/id revision-id
           :bundle/id bundle-id
           :scope (or scope :session)
           :state (or state :active)
           :binding/activated-at (or activated-at (System/currentTimeMillis))}]
    (validate-binding b)
    b))

(defn binding-from-offer
  "Create a ContextBinding from a ContextOffer.
  Offer must contain :offer/logical-id, :offer/revision-id, :offer/bundle-id.
  Returns a new active session binding pinned to offer's revision."
  [offer]
  (when-not (map? offer)
    (throw (err/error :context/binding-invalid "offer must be a map" {:offer offer})))
  (let [logical-id (:offer/logical-id offer)
        revision-id (:offer/revision-id offer)
        bundle-id (:offer/bundle-id offer)]
    (when-not logical-id
      (throw (err/error :context/binding-invalid "offer missing :offer/logical-id" {:offer offer})))
    (when-not revision-id
      (throw (err/error :context/binding-invalid "offer missing :offer/revision-id" {:offer offer})))
    (when-not bundle-id
      (throw (err/error :context/binding-invalid "offer missing :offer/bundle-id" {:offer offer})))
    (make-binding {:logical-id logical-id
                   :revision-id revision-id
                   :bundle-id bundle-id})))

;; ---------------------------------------------------------------------------
;; In-memory binding store
;; ---------------------------------------------------------------------------

(defn create-store
  "Create an in-memory binding store (atom).
  State shape: {:by-logical {logical-id -> binding}
                :by-id {binding-id -> binding}}"
  []
  (atom {:by-logical {} :by-id {}}))

(defn activate!
  "Activate an offer in store. Creates a binding pinned to offer's revision.
  If a binding for same logical-id already exists, it is replaced (new binding/id).
  Returns the new binding."
  [store offer]
  (when-not (instance? clojure.lang.Atom store)
    (throw (err/error :context/binding-invalid "store must be an atom" {:store store})))
  (let [binding (binding-from-offer offer)
        logical-id (:logical/id binding)
        binding-id (:binding/id binding)]
    (swap! store (fn [s]
                   (-> s
                       (assoc-in [:by-logical logical-id] binding)
                       (assoc-in [:by-id binding-id] binding))))
    binding))

(defn deactivate!
  "Deactivate binding for logical-id. Removes it from store.
  Returns the removed binding or nil."
  [store logical-id]
  (when-not (instance? clojure.lang.Atom store)
    (throw (err/error :context/binding-invalid "store must be an atom" {:store store})))
  (let [prev (get-in @store [:by-logical logical-id])]
    (when prev
      (swap! store (fn [s]
                     (-> s
                         (update :by-logical dissoc logical-id)
                         (update :by-id dissoc (:binding/id prev))))))
    prev))

(defn get-binding
  "Get active binding for logical-id, or nil."
  [store logical-id]
  (get-in @store [:by-logical logical-id]))

(defn list-active
  "Return seq of active bindings (state :active)."
  [store]
  (vals (:by-logical @store)))

(defn get-by-id
  "Get binding by binding/id, or nil."
  [store binding-id]
  (get-in @store [:by-id binding-id]))

(defn clear!
  "Remove all bindings (for test teardown)."
  [store]
  (reset! store {:by-logical {} :by-id {}}))
