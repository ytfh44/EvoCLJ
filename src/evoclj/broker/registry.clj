(ns evoclj.broker.registry
  "Closed ResourceKindRegistry — definition > validation with explicit allowlist.

  Fleet S6 (DAG S5/S6): broker registry made closed. Previously the broker
  accepted an arbitrary keyword map as :registry (open), allowing callers
  to authorize any ad-hoc resource kind without definition. Now the registry
  is a sealed type whose keys must belong to the explicit allowlist
  allowed-resource-kinds. The allowlist is the single definition; validation
  checks membership, never an open keyword.

  Pattern mirrors S4/S5 sealed handles: deftype with private secret, factory
  closed over secret, predicate checks identical?.

  Definition > validation: allowed-resource-kinds is the definition; a registry
  containing an arbitrary keyword not in the set is rejected at construction
  with :registry/invalid-kind, and authorize denies unknown kinds with
  :capability/unknown-resource-kind (fail-closed, never implicit allow)."
  (:require [evoclj.kernel.error :as err]))

;; ----------------------------------------------------------------------
;; Closed allowlist — single source (definition > validation)
;; ----------------------------------------------------------------------

(def allowed-resource-kinds
  "The closed set of resource kinds the broker may authorize. Explicit
  allowlist — not arbitrary keyword. Add a new kind here (definition),
  then add its target vector in default-entries. Validation checks
  membership; an unlisted kind is not a registry entry."
  #{:tool :model :memory :filesystem :filesystem/path})

(def ^:private default-entries
  "Built-in entries for the closed allowlist. Each key is in
  allowed-resource-kinds; each value is a vector of authorization targets."
  {:tool [{:source :request :action-from :request}]
   :model [{:source :request :action-from :request}]
   :memory [{:source :request :action-from :request}]
   :filesystem [{:source :request :action-from :request}]
   :filesystem/path [{:source :tool :action-from :intent}
                     {:source :request :action-from :request}]})

;; ----------------------------------------------------------------------
;; Sealed registry type
;; ----------------------------------------------------------------------

(deftype ResourceKindRegistry [entries ^:private secret]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k notFound]
    (case k
      :entries entries
      :allowed-kinds allowed-resource-kinds
      notFound))
  clojure.lang.Counted
  (count [this] (count entries))
  clojure.lang.IPersistentMap
  (assoc [this k v] (throw (UnsupportedOperationException. "ResourceKindRegistry is immutable; use registry-assoc")))
  (without [this k] (throw (UnsupportedOperationException. "ResourceKindRegistry is immutable")))
  Object
  (toString [this] (str "ResourceKindRegistry" (keys entries))))
(alter-meta! #'->ResourceKindRegistry assoc :private true)

(let [registry-secret (Object.)]
  (defn- ->registry
    "Private sealed factory."
    [entries]
    (ResourceKindRegistry. entries registry-secret))

  (defn registry?
    "True when x is a sealed ResourceKindRegistry produced via the closed
    allowlist path."
    [x]
    (and (instance? ResourceKindRegistry x)
         (identical? (.-secret ^ResourceKindRegistry x) registry-secret)))

  (defn make-registry
    "Create a sealed registry from an entries map. Validates that every
    key is in allowed-resource-kinds (closed set) and every value is a
    vector of targets. Throws :registry/invalid-kind or :registry/invalid-entry
    on violation — an arbitrary keyword is never accepted."
    [entries]
    (when-not (map? entries)
      (throw (err/error :registry/invalid-kind "registry entries must be a map" {:value entries})))
    (doseq [[k v] entries]
      (when-not (contains? allowed-resource-kinds k)
        (throw (err/error :registry/invalid-kind
                          (str "resource kind not in closed allowlist: " k)
                          {:kind k :allowed allowed-resource-kinds})))
      (when-not (and (vector? v) (seq v))
        (throw (err/error :registry/invalid-entry "registry entry must be non-empty vector" {:kind k :value v})))
      (doseq [t v]
        (when-not (and (map? t) (contains? #{:request :tool} (:source t)) (contains? #{:request :intent} (:action-from t)))
          (throw (err/error :registry/invalid-entry "invalid target shape" {:kind k :target t})))))
    (->registry entries))

  (defn default-registry
    "The sealed default registry (the built-in closed entries)."
    []
    (->registry default-entries))

  (defn registry-entries
    "Return the entries map of a sealed registry, or nil if not a registry."
    [r]
    (when (registry? r)
      (.-entries ^ResourceKindRegistry r)))

  (defn registry-get
    "Get the target vector for kind in a sealed registry, or nil."
    [r kind]
    (when (registry? r)
      (get (.-entries ^ResourceKindRegistry r) kind)))

  (defn registry-contains?
    "True when sealed registry contains kind."
    [r kind]
    (boolean (registry-get r kind))))

(defn assert-registry!
  "Fail-closed guard: throw :registry/invalid-kind when r is not a sealed
  registry. Accepts nil (meaning use default) as valid."
  [r]
  (when (and (some? r) (not (registry? r)))
    (throw (err/error :registry/invalid-kind
                      "registry must be a sealed ResourceKindRegistry from evoclj.broker.registry, not an arbitrary map or keyword map"
                      {:value (try (str (type r)) (catch Exception _ "unknown"))})))
  r)
