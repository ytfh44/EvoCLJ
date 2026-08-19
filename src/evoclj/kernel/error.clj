(ns evoclj.kernel.error
  "Typed error contract for EvoCLJ.

  Every failure crossing a module boundary carries a stable
  machine-readable :error/type keyword so callers can distinguish
  failure classes without parsing exception strings. Per Global
  Constraint 22, only validated, fully serializable Clojure data may
  cross boundaries: `error-data` realizes and sanitizes anything that
  would otherwise escape as a Throwable, class object, lazy sequence,
  function, or opaque Java value.")

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

(def ^:private secret-keys
  "Keywords whose values are treated as secrets during sanitization.
   Used by `sanitize-map` to redact transport-config and similar maps
   before they cross serialization boundaries."
  #{:api-key :apiKey :api_secret :apikey
    :token :access-token :accessToken :refresh-token :refreshToken
    :password :passwd :pwd :secret
    :authorization :authorization-header :bearer
    :cookie
    :private-key :privateKey :private_key
    :client-secret :clientSecret
    :x-api-key :x-api-secret})

(defn- secret-key?
  [k]
  (and (keyword? k)
       (contains? secret-keys k)))

(declare sanitize*)

(defn- sanitize-throwable
  "Convert a Throwable into a plain serializable error map."
  [^Throwable t depth]
  (if (>= depth max-depth)
    ::depth-exceeded
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
        (map (fn [[k v]]
               [(sanitize* k (inc depth))
                (if (secret-key? k)
                  "[REDACTED]"
                  (sanitize* v (inc depth)))]))
        (take max-collection-size m)))

(defn- sanitize-set [s depth]
  (into #{} (map #(sanitize* % (inc depth))) (take max-collection-size s)))

(defn- sanitize-coll [c depth]
  (mapv #(sanitize* % (inc depth)) (take max-collection-size c)))

(defn- sanitize* [v depth]
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
    (fn? v) ::fn
    (map? v) (if (< depth max-depth) (sanitize-map v depth) ::depth-exceeded)
    (set? v) (if (< depth max-depth) (sanitize-set v depth) ::depth-exceeded)
    (seq? v) (if (< depth max-depth) (sanitize-coll v depth) ::depth-exceeded)
    (vector? v) (if (< depth max-depth) (sanitize-coll v depth) ::depth-exceeded)
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
