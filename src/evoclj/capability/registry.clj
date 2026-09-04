(ns evoclj.capability.registry
  "Sealed descriptor registry shared by the ResourceKindDescriptor and
  ConstraintDescriptor registries (C1/C3 sealing).

  Lifecycle: boot -> build -> (optional add!*) -> seal. Once sealed, the
  registry is immutable for the execution lifetime: add!/remove! throw a
  typed :capability/registry-sealed error, so a descriptor registered (or
  worse, removed) at runtime can never change the meaning of an already
  persisted Authority lease between T1 and T2.

  The registry is a deftype holding an immutable key/validate pair and a
  single atom (described as {:descriptors map, :sealed? bool}) behind a
  private secret (mirrors S4/S5 sealed handles and evoclj.broker.registry).
  Mutation is reachable only through add!/remove!, which consult the sealed
  flag; seal-registry! is a one-way fold. Tests that need a custom descriptor
  build a SEPARATE (unsealed) registry via build-registry, rather than
  mutating any global."
  (:require [evoclj.kernel.error :as err]))

;; ----------------------------------------------------------------------
;; Sealed registry type
;; ----------------------------------------------------------------------

(deftype DescriptorRegistry [key-fn
                             validate-fn
                             ^:private state
                             ^:private secret])
(alter-meta! #'->DescriptorRegistry assoc :private true)

(let [secret (Object.)]
  (defn- ->registry
    [key-fn validate-fn descriptors]
    (DescriptorRegistry. key-fn validate-fn
                         (atom {:descriptors descriptors :sealed? false})
                         secret))

  (defn registry?
    "True when x is a DescriptorRegistry produced by this namespace's factory."
    [x]
    (and (instance? DescriptorRegistry x)
         (identical? (.-secret ^DescriptorRegistry x) secret)))

  (defn add!
    "Add / override a descriptor in an UNSEALED registry. Returns the descriptor.

    Throws :capability/registry-sealed when the registry is sealed, else
    validates via validate-fn (throwing :capability/invalid-descriptor) and
    keys via key-fn."
    [^DescriptorRegistry r descriptor]
    (let [st ^clojure.lang.IAtom (.-state r)]
      (when (:sealed? @st)
        (throw (err/error :capability/registry-sealed
                          "registry is sealed; descriptors cannot change for the execution lifetime"
                          {})))
      ((.-validate-fn r) descriptor)
      (let [k ((.-key-fn r) descriptor)]
        (swap! st update :descriptors assoc k descriptor))
      descriptor))

  (defn build-registry
    "Build a mutable (UNSEALED) descriptor registry seeded from `descriptors`.

    `key-fn` maps a descriptor to its registry key (a keyword); `validate-fn`
    throws :capability/invalid-descriptor for an invalid descriptor (must not
    satisfy its protocol, or a non-keyword key). Each descriptor is validated
    and keyed at build time.

    Grow it with add!, then freeze it with seal-registry!. Any descriptor
    added after seal throws :capability/registry-sealed."
    [key-fn validate-fn descriptors]
    (let [r (->registry key-fn validate-fn {})]
      (doseq [d descriptors]
        (add! r d))
      r))

  (defn remove!
    "Remove a descriptor by key from an UNSEALED registry. Returns nil.

    Throws :capability/registry-sealed when the registry is sealed."
    [^DescriptorRegistry r k]
    (let [st ^clojure.lang.IAtom (.-state r)]
      (when (:sealed? @st)
        (throw (err/error :capability/registry-sealed
                          "registry is sealed; descriptors cannot change for the execution lifetime"
                          {:key k})))
      (swap! st update :descriptors dissoc k)
      nil))

  (defn seal-registry!
    "One-way seal. Once sealed, add!/remove! throw :capability/registry-sealed.
    Throws :capability/registry-sealed if the registry is already sealed."
    [^DescriptorRegistry r]
    (let [st ^clojure.lang.IAtom (.-state r)]
      (when (:sealed? @st)
        (throw (err/error :capability/registry-sealed "registry already sealed" {})))
      (swap! st assoc :sealed? true))
    r)

  (defn sealed?
    "True when the registry has been sealed (immutable)."
    [^DescriptorRegistry r]
    (boolean (:sealed? @^clojure.lang.IAtom (.-state r))))

  (defn get-descriptor
    "Descriptor for key, or nil when absent."
    [^DescriptorRegistry r k]
    (get (:descriptors @^clojure.lang.IAtom (.-state r)) k))

  (defn all-descriptors
    "Immutable map of key -> descriptor."
    [^DescriptorRegistry r]
    (:descriptors @^clojure.lang.IAtom (.-state r)))

  (defn allowed-keys
    "Set of registered keys."
    [^DescriptorRegistry r]
    (set (keys (:descriptors @^clojure.lang.IAtom (.-state r)))))

  (defn contains-key?
    "True when the registry holds key."
    [^DescriptorRegistry r k]
    (contains? (:descriptors @^clojure.lang.IAtom (.-state r)) k)))