(ns evoclj.context.compression.error
  "Typed error contract for the EvoCLJ context subsystem.

  Every failure in context compression carries a stable machine-readable
  :error/type keyword so callers can distinguish failure classes without
  parsing exception strings. Per Global Constraint 22, only validated,
  fully serializable Clojure data may cross boundaries: `error-data`
  realizes and sanitizes anything that would otherwise escape as a
  Throwable, class object, lazy sequence, function, or opaque Java value.

  This namespace is self-contained: the context subsystem must not depend
  on the kernel for its error contract. The kernel may depend on context,
  but not vice versa.")

;; ----------------------------------------------------------------------
;; Error type constants (namespaced keywords with docstrings)
;; ----------------------------------------------------------------------

(def ^:const compression-invalid
  "`:context/compression-invalid` — The compression model produced something
  unusable: a malformed envelope, an unknown schema version, or a result
  that violates the envelope contract. Fail closed: never silently degrade."
  :context/compression-invalid)

(def ^:const trigger-invalid
  "`:context/trigger-invalid` — The compression trigger configuration is
  malformed or contains invalid parameters that cannot be interpreted."
  :context/trigger-invalid)

(def ^:const apply-invalid
  "`:context/apply-invalid` — Applying an envelope to a context failed.
  Examples: the envelope is not EDN-safe, or the fresh-tail boundary
  is violated during application."
  :context/apply-invalid)

(def ^:const residue-invalid
  "`:context/residue-invalid` — A residue entry is malformed: missing
  required fields, wrong types, or a structure that violates the residue
  schema."
  :context/residue-invalid)

(def ^:const provenance-invalid
  "`:context/provenance-invalid` — A provenance claim cannot be traced to
  its source, or the provenance chain is broken or circular."
  :context/provenance-invalid)

(def ^:const crosscheck-mismatch
  "`:context/crosscheck-mismatch` — The envelope's structured fields
  disagree with the registered structured-field source of truth, and
  the disagreement is NOT auto-correctable. Requires manual
  intervention."
  :context/crosscheck-mismatch)

(def ^:const idempotency-violation
  "`:context/idempotency-violation` — Re-compression lost or corrupted a
  core field, or dropped a residue entry that was present in the original
  envelope. The recompressed output is not semantically equivalent to
  the input."
  :context/idempotency-violation)

(def ^:const eval-invalid
  "`:context/eval-invalid` — An eval case is malformed: missing expression,
  unsupported eval type, or a structure that violates the eval schema."
  :context/eval-invalid)

;; ----------------------------------------------------------------------
;; Core error contract
;; ----------------------------------------------------------------------

(defn error
  "Build an ExceptionInfo carrying a stable `:error/type` in its ex-data.

  `type` is a namespaced keyword identifying the failure class,
  `message` a human-readable string, and `data` the contextual payload.
  A caller-supplied `:error/type` inside `data` is always overridden so
  the contract type cannot be shadowed. `data` is stored as given; call
  `error-data` when the error must cross a serialization boundary."
  [type message data]
  (ex-info message (assoc data :error/type type)))

(def ^:private ^:const max-depth 32)
(def ^:private ^:const max-collection-size 1024)

(declare sanitize*)

(defn- sanitize-throwable
  "Convert a Throwable into a plain serializable error map."
  [^Throwable t depth]
  (if (>= depth max-depth)
    :evoclj.context.error/depth-exceeded
    (let [data (ex-data t)]
      {:error/type (or (:error/type data) :error/unknown)
       :error/message (.getMessage t)
       :error/class (.getName (.getClass t))
       :error/data (if data (sanitize* (dissoc data :error/type) (inc depth)) nil)
       :error/cause (if-let [c (.getCause t)]
                      (sanitize-throwable c (inc depth))
                      nil)})))

(defn- sanitize-map [m depth]
  (into {}
        (map (fn [[k v]] [(sanitize* k (inc depth)) (sanitize* v (inc depth))]))
        (take max-collection-size m)))

(defn- sanitize-set [s depth]
  (into #{} (map #(sanitize* % (inc depth))) (take max-collection-size s)))

(defn- sanitize-coll [c depth]
  (mapv #(sanitize* % (inc depth)) (take max-collection-size c)))

(defn- sanitize*
  [v depth]
  (cond
    (or (nil? v) (true? v) (false? v)) v
    (number? v) v
    (string? v) v
    (keyword? v) v
    (symbol? v) v
    (char? v) v
    (uuid? v) v
    (instance? java.util.Date v) v
    (instance? Throwable v) (sanitize-throwable v (inc depth))
    (class? v) (symbol (.getName ^Class v))
    (fn? v) :evoclj.context.error/fn
    (map? v) (if (< depth max-depth) (sanitize-map v depth) :evoclj.context.error/depth-exceeded)
    (set? v) (if (< depth max-depth) (sanitize-set v depth) :evoclj.context.error/depth-exceeded)
    (seq? v) (if (< depth max-depth) (sanitize-coll v depth) :evoclj.context.error/depth-exceeded)
    (vector? v) (if (< depth max-depth) (sanitize-coll v depth) :evoclj.context.error/depth-exceeded)
    :else (str v)))

(defn sanitize
  "Return a bounded, fully serializable EDN value for `v`.

  Values that cannot be represented in EDN are replaced rather than
  dropped: functions become ::fn, classes become their name symbol,
  throwables become structured error maps, other opaque Java objects
  become strings, and lazy sequences are realized up to a bounded depth
  and collection size. The result always round-trips through
  pr-str / clojure.edn read-string."
  [v]
  (sanitize* v 0))

(defn error-data
  "Extract fully serializable error data from a Throwable.

  Returns a plain map with `:error/type`, `:error/message`,
  `:error/class` (the Java class name as a string), `:error/data`
  (sanitized ex-data without the promoted :error/type), and
  `:error/cause` (recursively). Contains no Throwable object, class
  object, lazy sequence, or function, and round-trips through
  pr-str / clojure.edn read-string."
  [throwable]
  (sanitize-throwable throwable 0))
