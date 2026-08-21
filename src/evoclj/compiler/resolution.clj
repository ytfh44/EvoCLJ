(ns evoclj.compiler.resolution
  "Define Resolution and provider alias resolution (component).

  resolve-models turns a Genome's models.edn config plus a provider
  catalog into a pure data Resolution:

    {:resolution/id \"sha256:<64 hex>\"
     :models {:planner {:alias :reasoning/high
                        :provider :fixture
                        :provider-model \"fixture-model-v1\"
                        :adapter-version \"1\"}}}

  Properties:

  - Resolution is pure, fully serializable EDN data (Global Constraint
    22): no secrets, lazy sequences, functions, or host objects ever
    appear. Secret-looking keys (:api-key, :token, :password, :secret
    and their spellings) in resolved data are rejected with a typed
    :resolution/secret-key error.
  - The Resolution ID follows the same canonical deterministic EDN
    conventions as evoclj.genome.hash: the resolved :models value is
    recursively normalized (maps and sets sorted) into a canonical EDN
    form and hashed with hash/text-digest (UTF-8 bytes, CRLF/CR
    normalized to LF), yielding a \"sha256:<64 hex>\" ID validated by
    evoclj.genome.types/resolution-id. Identical logical models config
    plus catalog therefore yield identical IDs; different concrete
    provider-model/provider/adapter-version entries yield different IDs
    even for the same Genome (Global Constraints 1, 2, 6).

  Error types: :resolution/invalid (malformed input shapes or non-EDN
  values), :resolution/alias-missing (alias requested by the Genome is
  absent from the catalog), :resolution/secret-key (secret-looking key
  in resolved data)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]))

;; --- secret-looking key detection -------------------------------------------

(def ^:private secret-component-pattern
  "Matches a secret-looking whole component in a keyword's name or
  namespace: api-key/apikey/api_key, token, password, secret, credential
  (case-insensitive), delimited by start/end or one of [-_. /]."
  #"(?i)(^|[-_./])(api-?key|apikey|token|password|secret|credential)([-_./]|$)")

(defn- secret-key?
  "True when `k` is a keyword whose name or namespace contains a
  secret-looking component. Covers the normative rejections :api-key,
  :token, :password, :secret plus their spellings (e.g. :api_key,
  :access-token, :client_secret) while leaving legit fields such as
  :provider-model and :adapter-version alone."
  [k]
  (and (keyword? k)
       (boolean (re-find secret-component-pattern
                         (str (when-let [ns (namespace k)] (str ns "/"))
                              (name k))))))

(defn- check-secret-keys!
  "Reject secret-looking keys in resolved data with a typed
  :resolution/secret-key error carrying the offending :key."
  [model-name alias entry]
  (when (secret-key? model-name)
    (throw (err/error :resolution/secret-key
                      "secret-looking model name in resolved data"
                      {:key model-name :model model-name})))
  (doseq [k (keys entry)]
    (when (secret-key? k)
      (throw (err/error :resolution/secret-key
                        "secret-looking key in resolved provider entry"
                        {:key k :model model-name :alias alias})))))

;; --- canonical deterministic EDN normalization ------------------------------

(def ^:private canonical-compare
  "Total order over canonical EDN scalar keys. Uses compare when the
  keys are mutually comparable; otherwise falls back to the canonical
  string form so mixed key types can never throw a host exception."
  (fn [a b]
    (let [c (try (compare a b)
                 (catch Exception _ ::incomparable))]
      (if (keyword? c)
        (compare (pr-str a) (pr-str b))
        (if (neg? c) -1 (if (pos? c) 1 0))))))

(defn- canonical-edn
  "Recursively normalize a pure EDN value into its canonical
  deterministic form: maps become sorted maps, sets become sorted sets,
  vectors are mapped element-wise, and only EDN scalars (nil, booleans,
  numbers, strings, keywords, symbols, chars, UUIDs) are accepted.
  Anything else — functions, lazy sequences, class objects, opaque host
  values — throws :resolution/invalid (Global Constraint 22). The
  canonical form round-trips through pr-str / clojure.edn read-string
  and prints identically every time, which is what makes the Resolution
  ID deterministic (same conventions as evoclj.genome.hash)."
  [v]
  (cond
    (or (nil? v) (true? v) (false? v)) v
    (number? v) v
    (string? v) v
    (keyword? v) v
    (symbol? v) v
    (char? v) v
    (uuid? v) v
    (map? v) (into (sorted-map-by canonical-compare)
                   (map (fn [[k val]] [(canonical-edn k) (canonical-edn val)]))
                   v)
    (vector? v) (mapv canonical-edn v)
    (set? v) (into (sorted-set-by canonical-compare) (map canonical-edn) v)
    :else (throw (err/error :resolution/invalid
                            "resolved data must contain only pure EDN values"
                            {:reason :non-edn-value :value (err/sanitize v)}))))

(defn- resolution-digest
  "The canonical Resolution ID: sha256:<64 hex> over the canonical EDN
  serialization of the resolved :models value, via the same text hashing
  used for Genome content (UTF-8 bytes, CRLF/CR normalized to LF)."
  [models]
  (types/resolution-id
   (hash/text-digest (pr-str (canonical-edn models)))))

;; --- input shape validation -------------------------------------------------

(defn- validate-shapes!
  "Require models-config to be a map with a :models map and the provider
  catalog to be a map of alias keyword to concrete provider entry."
  [models-config provider-catalog]
  (when-not (and (map? models-config)
                 (map? (:models models-config)))
    (throw (err/error :resolution/invalid
                      "models config must be a map with a :models map"
                      {:reason :invalid-models-config
                       :value models-config})))
  (when-not (map? provider-catalog)
    (throw (err/error :resolution/invalid
                      "provider catalog must be a map of alias to concrete provider entry"
                      {:reason :invalid-provider-catalog
                       :value provider-catalog}))))

;; --- public entry point -----------------------------------------------------

(defn resolve-models
  "Resolve a Genome's models.edn config against a provider catalog.

  `models-config` is the models.edn value: {:models {model-name
  {:alias alias-keyword} ...}}. `provider-catalog` maps each alias
  keyword to a concrete entry {:provider ... :provider-model ...
  :adapter-version ...}.

  Returns a pure data Resolution map: {:resolution/id \"sha256:<64
  hex>\" :models {model-name {:alias ... :provider ... :provider-model
  ... :adapter-version ...} ...}} with :models sorted by model name.
  The ID is deterministic over the canonical EDN of the resolved
  :models value, so identical config plus catalog always produce the
  same ID and different concrete model IDs produce different IDs.

  Throws ExceptionInfo with a stable :error/type: :resolution/invalid
  (malformed input shapes or non-EDN values), :resolution/alias-missing
  (an alias requested by the Genome is absent from the catalog), or
  :resolution/secret-key (a secret-looking key in resolved data)."
  [models-config provider-catalog]
  (validate-shapes! models-config provider-catalog)
  (let [models
        (into (sorted-map-by canonical-compare)
              (map (fn [[model-name model-entry]]
                     (when-not (map? model-entry)
                       (throw (err/error :resolution/invalid
                                         "model config entry must be a map"
                                         {:reason :invalid-model-entry
                                          :model model-name
                                          :value model-entry})))
                     (let [alias (:alias model-entry)]
                       (when-not (keyword? alias)
                         (throw (err/error :resolution/invalid
                                           "model config entry must declare a keyword :alias"
                                           {:reason :missing-alias
                                            :model model-name
                                            :value model-entry})))
                       (when-not (contains? provider-catalog alias)
                         (throw (err/error :resolution/alias-missing
                                           "provider catalog has no entry for the requested model alias"
                                           {:alias alias :model model-name})))
                       (let [entry (get provider-catalog alias)]
                         (when-not (map? entry)
                           (throw (err/error :resolution/invalid
                                             "provider catalog alias entry must be a map"
                                             {:reason :invalid-alias-entry
                                              :alias alias
                                              :value entry})))
                         (check-secret-keys! model-name alias entry)
                         [model-name
                          (into (sorted-map-by canonical-compare)
                                (assoc entry :alias alias))])))
              (:models models-config)))]
    {:resolution/id (resolution-digest models)
     :models models}))
