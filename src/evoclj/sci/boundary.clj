(ns evoclj.sci.boundary
  "The EDN-safe boundary between evolvable code and the trusted kernel
  (Task 3.2).

  Global Constraint 22 requires that only validated Clojure data cross
  Genome/SCI/Intent/Event boundaries: raw Java objects, lazy sequences,
  futures, and open resources must never cross. This namespace enforces
  that boundary with four functions:

  - (edn-safe? x) — a pure recursive predicate. True when x is plain
    EDN data: nil, booleans, numbers, strings, keywords, symbols, chars,
    UUIDs, #inst dates, and vectors/lists/maps/sets whose elements are
    recursively EDN-safe. It NEVER realizes a lazy sequence: any seq
    that is not a proper persistent list is not EDN-safe (an unrealized
    LazySeq is a suspended computation, not data). Records are never
    EDN-safe because they do not round-trip through
    clojure.edn/read-string without a registered reader.

  - (materialize-edn x) — recursively converts x into plain, fully
    realized EDN-safe data under explicit limits and REJECTS what
    cannot be materialized with a typed error. Defaults: :max-depth 64
    nested collection levels, :max-size 100000 elements per collection.
    Lazy sequences are realized under the size limit and returned as
    proper lists — an infinite sequence such as (range) is cut off by
    the limit and rejected with :edn/size-exceeded; it is never
    returned lazily and never allowed to hang the boundary (Task Step
    4). Values that cannot be represented as EDN at all — functions,
    atoms, promises, futures, delays, Clojure/SCI vars, records (unless
    explicitly registered), and other Java objects such as File or
    InputStream — are rejected with :edn/unsupported carrying a
    :reason classifying the offender plus its :path and :depth. Records
    may be explicitly registered via the :allowed-records option (a set
    of record type symbols or classes); a registered record materializes
    as a plain map.

  - (validate-program-input schema x) / (validate-program-output schema
    x) — validate a value against a Malli schema at the program
    boundary. The schema is passed as a schema VALUE (keyword
    schema-registry lookup is a later task). A value that is not
    EDN-safe is rejected before schema checking; otherwise the schema
    decides. On success the value is returned unchanged — validation
    never coerces (matching the project's trust-boundary convention
    from Task 1.2). On failure a typed :program/input-invalid /
    :program/output-invalid error is thrown with a fully serializable
    Malli explanation.

  Error contract (Global Constraint 22): every error thrown here is an
  ExceptionInfo whose ex-data is plain, sanitized, serializable data —
  :edn/unsupported, :edn/size-exceeded, :edn/depth-exceeded,
  :edn/limits-invalid, :program/schema-invalid, :program/input-invalid,
  :program/output-invalid. Offending values appear only through
  evoclj.kernel.error/sanitize, so every error round-trips through
  pr-str / clojure.edn read-string."
  (:require [clojure.main :as main]
            [evoclj.kernel.error :as err]
            [malli.core :as m]
            ;; Loads the sci.lang namespace so the sci.lang.Var deftype
            ;; class below exists when this namespace compiles: SCI var
            ;; objects are a rejected boundary value and must be
            ;; detectable by type.
            [sci.lang]))

;; --- limits and primitive classification -----------------------------------

(def ^:private ^:const default-max-depth 64)
(def ^:private ^:const default-max-size 100000)

(defn- edn-primitive?
  "True for the leaf values EDN can represent directly with no
  conversion: nil, booleans, numbers (integers, floats, ratios, BigInt,
  BigDecimal, bytes, shorts), strings, keywords, symbols, chars, UUIDs,
  and exact java.util.Date values (the #inst tagged literal — subclasses
  such as java.sql.Timestamp are rejected because they do not round-trip
  cleanly). Everything else is either a collection to descend into or a
  rejected value."
  [x]
  (or (nil? x)
      (boolean? x)
      (number? x)
      (string? x)
      (keyword? x)
      (symbol? x)
      (char? x)
      (uuid? x)
      (= java.util.Date (class x))))

(defn edn-safe?
  "True when x is plain, fully realized EDN data: nil, booleans,
  numbers, strings, keywords, symbols, chars, UUIDs, #inst dates, and
  vectors/lists/maps/sets whose elements are recursively EDN-safe.

  Lazy and other non-list sequences are never EDN-safe and are never
  realized by this predicate — an unrealized LazySeq could be infinite,
  and checking its type is sufficient. Records are never EDN-safe
  because they do not round-trip through clojure.edn/read-string
  without a registered reader. Everything else — Java objects,
  functions, vars, atoms, promises, futures, delays — is false.

  See materialize-edn for the coercive, limit-enforcing side of the
  boundary."
  [x]
  (cond
    (edn-primitive? x) true
    (record? x) false
    (map? x) (every? (fn [[k v]] (and (edn-safe? k) (edn-safe? v))) x)
    (vector? x) (every? edn-safe? x)
    (set? x) (every? edn-safe? x)
    (list? x) (every? edn-safe? x)
    :else false))

;; --- materialization helpers -----------------------------------------------

(defn- limits-of
  "The :limits subset of an options map, included in every error."
  [opts]
  (select-keys opts [:max-depth :max-size]))

(defn- sanitized-path
  "Sanitize a path vector so an error never carries a raw offending
  value (e.g. a lazy map key)."
  [path]
  (mapv err/sanitize path))

(defn- unsupported-error
  [x reason depth path opts]
  (err/error :edn/unsupported
             (str "value of kind " (name reason)
                  " cannot cross the EDN-safe boundary")
             {:reason reason
              :value (err/sanitize x)
              :path (sanitized-path path)
              :depth depth
              :limits (limits-of opts)}))

(defn- size-error
  [coll size limit path opts]
  (err/error :edn/size-exceeded
             (str "collection of size " size " exceeds the maximum "
                  limit " elements")
             {:limit limit :found size
              :path (sanitized-path path)
              :value (err/sanitize coll)
              :limits (limits-of opts)}))

(defn- depth-error
  [coll depth limit path opts]
  (err/error :edn/depth-exceeded
             (str "nesting depth " depth " exceeds the maximum "
                  limit " collection levels")
             {:limit limit :depth depth
              :path (sanitized-path path)
              :value (err/sanitize coll)
              :limits (limits-of opts)}))

(defn- record-type-sym
  "The natural fully qualified type symbol of a record, e.g.
  'my.ns/Point. The JVM class name munges '-' to '_' (class names
  cannot contain hyphens), so the .getName form is demunged back to
  the name the record was defined with."
  [x]
  (symbol (main/demunge (.getName (class x)))))

(defn- record-allowed?
  "True when the record x is explicitly registered via the
  :allowed-records option (a set of record type symbols or classes)."
  [x {:keys [allowed-records]}]
  (boolean
   (and allowed-records
        (or (contains? allowed-records (class x))
            (contains? allowed-records (record-type-sym x))))))

(defn- unsupported-reason
  "Classify an un-materializable value for the :reason of the typed
  error.

  In Clojure 1.12 there is no clojure.lang.Promise class: (promise)
  returns a reify implementing clojure.lang.IPending (a suspended
  computation that blocks on deref). IPending is therefore the
  detection for promises; delays are classified more specifically by
  the clojure.lang.Delay check above, and lazy sequences never reach
  this function because materialize* routes every seq to its own
  realization branch."
  [x]
  (cond
    (fn? x) :function
    (instance? clojure.lang.IAtom x) :atom
    (instance? java.util.concurrent.Future x) :future
    (instance? clojure.lang.Delay x) :delay
    (instance? clojure.lang.IPending x) :promise
    (instance? clojure.lang.Var x) :var
    (instance? sci.lang.Var x) :sci-var
    (record? x) :record
    :else :java-object))

(defn- check-depth!
  "Throw :edn/depth-exceeded when a collection appears at depth >=
  max-depth (the root value is at depth 0, so max-depth 64 admits 64
  nested collection levels)."
  [coll depth {:keys [max-depth] :as opts} path]
  (when (>= depth max-depth)
    (throw (depth-error coll depth max-depth path opts))))

(declare materialize*)

(defn- materialize-coll
  "Materialize a fully realized, counted collection: a vector, proper
  list, map, set, or an explicitly registered record treated as a map.
  Enforces the depth and size limits, then materializes every element
  recursively."
  [coll depth path {:keys [max-size] :as opts}]
  (check-depth! coll depth opts path)
  (let [n (count coll)]
    (when (> n max-size)
      (throw (size-error coll n max-size path opts))))
  (cond
    (map? coll)
    (into {}
          (map (fn [[k v]]
                 [(materialize* k (inc depth) (conj path k) opts)
                  (materialize* v (inc depth) (conj path k) opts)]))
          coll)
    (set? coll)
    (into #{} (map #(materialize* % (inc depth) (conj path %) opts)) coll)
    (vector? coll)
    (into []
          (map-indexed (fn [i v]
                         (materialize* v (inc depth) (conj path i) opts)))
          coll)
    (list? coll)
    (apply list (map #(materialize* % (inc depth) (conj path %) opts) coll))
    :else
    (throw (unsupported-error coll :java-object depth path opts))))

(defn- materialize-seq
  "Realize a (possibly lazy, possibly infinite) sequence under the size
  limit and return a proper list. Realization is bounded: at most
  max-size + 1 elements are pulled before the limit check fires, so an
  infinite sequence is rejected with :edn/size-exceeded instead of
  hanging or escaping as an unrealized lazy value. Nested laziness is
  realized recursively by materialize*."
  [s depth path {:keys [max-size] :as opts}]
  (check-depth! s depth opts path)
  (let [realized (into [] (take (inc max-size)) s)]
    (when (> (count realized) max-size)
      (throw (size-error s (count realized) max-size path opts)))
    (apply list
           (map #(materialize* % (inc depth) (conj path %) opts) realized))))

(defn- materialize*
  [x depth path {:keys [max-depth max-size] :as opts}]
  (cond
    (edn-primitive? x) x
    (record? x)
    (if (record-allowed? x opts)
      (materialize-coll x depth path opts)
      (throw (unsupported-error x :record depth path opts)))
    (map? x) (materialize-coll x depth path opts)
    (vector? x) (materialize-coll x depth path opts)
    (set? x) (materialize-coll x depth path opts)
    (list? x) (materialize-coll x depth path opts)
    (seq? x) (materialize-seq x depth path opts)
    :else (throw (unsupported-error x (unsupported-reason x) depth path opts))))

;; --- options validation -----------------------------------------------------

(defn- validate-opts!
  "Validate the materialize-edn options map: :max-depth and :max-size
  must be non-negative integers, :allowed-records must be a set."
  [opts]
  (when-not (map? opts)
    (throw (err/error :edn/limits-invalid
                      "materialize-edn options must be a map"
                      {:reason :invalid-opts :value (err/sanitize opts)})))
  (doseq [k [:max-depth :max-size]]
    (let [v (get opts k)]
      (when (and (some? v) (not (and (integer? v) (not (neg? v)))))
        (throw (err/error :edn/limits-invalid
                          (str k " must be a non-negative integer")
                          {:reason :invalid-limit :key k
                           :value (err/sanitize v)})))))
  (let [allowed (:allowed-records opts)]
    (when (and (some? allowed) (not (set? allowed)))
      (throw (err/error :edn/limits-invalid
                        ":allowed-records must be a set of record type symbols or classes"
                        {:reason :invalid-allowed-records
                         :value (err/sanitize allowed)}))))
  opts)

;; --- public entry points ---------------------------------------------------

(defn materialize-edn
  "Recursively convert x into plain, fully realized EDN-safe data,
  enforcing explicit limits and rejecting what cannot be materialized.

  Arity 1 uses the defaults :max-depth 64 and :max-size 100000. Arity 2
  accepts an options map:

  - :max-depth — maximum number of nested collection levels. A
    collection may appear at depths 0..max-depth-1 (the root value is
    at depth 0); deeper nesting throws :edn/depth-exceeded. Default 64.
  - :max-size — maximum number of elements per collection. Larger
    collections throw :edn/size-exceeded. Default 100000.
  - :allowed-records — a set of record type symbols or classes
    explicitly registered as materializable; a registered record is
    converted to a plain map. Any other record throws :edn/unsupported.

  Vectors, lists, maps, sets, and registered records are materialized
  element-wise. Sequences — including lazy sequences — are realized
  under the size limit and returned as proper lists, so an infinite
  sequence is rejected with :edn/size-exceeded rather than hanging or
  escaping as an unrealized lazy value (Task Step 4). Values that
  cannot be represented as EDN — functions, atoms, promises, futures,
  delays, Clojure/SCI vars, unregistered records, and other Java
  objects such as File or InputStream — throw :edn/unsupported with a
  :reason classifying the offender, its :path, and :depth.

  Every thrown error is an ExceptionInfo whose ex-data is plain,
  sanitized, serializable data (Global Constraint 22)."
  ([x] (materialize-edn x {}))
  ([x opts]
   (let [opts (merge {:max-depth default-max-depth
                      :max-size default-max-size}
                     (validate-opts! (or opts {})))]
     (materialize* x 0 [] opts))))

;; --- schema validation at the program boundary -----------------------------

(defn- ensure-valid-schema!
  "Throw :program/schema-invalid when schema is not a valid Malli schema
  value (a schema form such as [:map ...], a built-in keyword schema
  such as :string/:any, or a compiled Schema)."
  [schema]
  (try
    (m/schema schema)
    (catch clojure.lang.ExceptionInfo _
      (throw (err/error :program/schema-invalid
                        "program schema is not a valid Malli schema"
                        {:reason :invalid-schema
                         :value (err/sanitize schema)})))))

(defn- validate-program-value
  [error-type label schema x]
  (ensure-valid-schema! schema)
  (when-not (edn-safe? x)
    (throw (err/error error-type
                      (str label " is not EDN-safe; materialize-edn first")
                      {:reason :not-edn-safe
                       :value (err/sanitize x)})))
  (if (m/validate schema x)
    x
    (throw (err/error error-type
                      (str label " failed schema validation")
                      {:reason :schema-invalid
                       :value (err/sanitize x)
                       :explanation (err/sanitize (m/explain schema x))}))))

(defn validate-program-input
  "Validate program `input` against the Malli `schema` at the trust
  boundary (Global Constraint 22).

  `schema` is a Malli schema VALUE — e.g. [:map [:text :string]] or the
  keyword schema :string — passed directly; keyword schema-registry
  lookup is a later task. Returns the input unchanged when it is
  EDN-safe and matches the schema; validation never coerces. Throws
  :program/input-invalid otherwise (:reason :not-edn-safe, or
  :schema-invalid with a fully serializable :explanation), or
  :program/schema-invalid when `schema` is not a valid Malli schema
  value."
  [schema x]
  (validate-program-value :program/input-invalid "program input" schema x))

(defn validate-program-output
  "Validate program `output` against the Malli `schema` at the trust
  boundary (Global Constraint 22).

  `schema` is a Malli schema VALUE — e.g. [:map [:ok :boolean]] or the
  keyword schema :any — passed directly; keyword schema-registry lookup
  is a later task. Returns the output unchanged when it is EDN-safe and
  matches the schema; validation never coerces. Throws
  :program/output-invalid otherwise (:reason :not-edn-safe, or
  :schema-invalid with a fully serializable :explanation), or
  :program/schema-invalid when `schema` is not a valid Malli schema
  value."
  [schema x]
  (validate-program-value :program/output-invalid "program output" schema x))
