(ns evoclj.capability.semantic
  "Closed, versioned semantic capability types.

  Provider names are only lookup keys. Authorization consumes the frozen
  canonical semantic spec, never the provider name. The registry is static
  EDN data so semantic authority cannot be supplied by executable or dynamic
  input."
  (:require [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]))

(def semantic-version 1)
(def mapping-version 1)

(def effects #{:communication/send})
(def risks #{:internal :external :public})
(def ^:private risk-rank {:internal 0 :external 1 :public 2})

(def semantic-mappings
  "The closed provider-id -> canonical semantic-spec registry.

  Aliases are explicit entries, not normalization rules. This keeps a new
  provider spelling from silently acquiring an existing capability."
  (let [send {:semantic/version semantic-version
              :semantic/effect :communication/send
              :semantic/risk :external}
        public-send (assoc send :semantic/risk :public)]
    {:gmail.send send
     :gmail/send send
     :slack.post send
     :slack/post send
     :twitter.post public-send
     :twitter/post public-send}))

(def canonical-mappings semantic-mappings)

(defn semantic-opted-in?
  "True only for descriptors that explicitly request semantic validation."
  [descriptor]
  (= :required (:semantic/validation descriptor)))

(defn- fail!
  [type message data]
  (throw (err/error type message data)))

(defn normalize-spec
  "Validate and canonicalize a semantic spec. Unknown keys, versions,
  effects, and risks fail closed."
  [spec]
  (when-not (map? spec)
    (fail! :capability/semantic-invalid
           "semantic capability spec must be a map"
           {:spec (err/sanitize spec)}))
  (let [allowed #{:semantic/version :semantic/effect :semantic/risk}
        extra (seq (remove allowed (keys spec)))
        version (:semantic/version spec)
        effect (:semantic/effect spec)
        risk (:semantic/risk spec)]
    (when extra
      (fail! :capability/semantic-invalid
             "semantic capability spec contains an unknown key"
             {:keys (vec extra)}))
    (when (not= semantic-version version)
      (fail! :capability/semantic-stale
             "semantic capability spec version is not current"
             {:expected semantic-version :actual version}))
    (when-not (contains? effects effect)
      (fail! :capability/semantic-unknown
             "semantic capability effect is not registered"
             {:effect effect}))
    (when-not (contains? risks risk)
      (fail! :capability/semantic-unknown
             "semantic capability risk is not registered"
             {:risk risk}))
    {:semantic/version semantic-version
     :semantic/effect effect
     :semantic/risk risk}))

(def normalize-semantic-spec normalize-spec)

(defn spec-digest
  "Digest the exact canonical semantic spec, independently of a provider."
  [spec]
  (hash/text-digest (pr-str (into (sorted-map) (normalize-spec spec)))))

(defn mapping-for
  "Return the static mapping for a provider tool id, or nil."
  [tool-id]
  (get semantic-mappings tool-id))

(defn- mapping-version-for
  [descriptor]
  (or (:semantic/mapping-version descriptor) mapping-version))

(defn semantic-spec-for
  "Resolve the canonical semantic spec for an opted-in descriptor.

  A descriptor may repeat the expected canonical spec for integrity, but it
  may not define a new mapping. A missing, stale, or mismatching mapping is a
  typed deny at the binding boundary."
  [descriptor]
  (when (semantic-opted-in? descriptor)
    (let [tool-id (:tool/id descriptor)
          mapped (mapping-for tool-id)
          expected-version (mapping-version-for descriptor)
          declared (:semantic/spec descriptor)]
      (when-not (= mapping-version expected-version)
        (fail! :capability/semantic-stale
               "semantic provider mapping is stale"
               {:tool/id tool-id
                :expected mapping-version
                :actual expected-version}))
      (when-not mapped
        (fail! :capability/semantic-unknown
               "no semantic mapping exists for opted-in provider"
               {:tool/id tool-id}))
      (let [canonical (normalize-spec mapped)]
        (when (some? declared)
          (when-not (= canonical (normalize-spec declared))
            (fail! :capability/semantic-mismatch
                   "declared semantic spec does not match the canonical mapping"
                   {:tool/id tool-id
                    :declared (err/sanitize declared)
                    :mapped canonical})))
        canonical))))

(defn freeze-descriptor
  "Return nil for legacy descriptors, otherwise the frozen semantic spec and
  digest used by authorization and journaling."
  [descriptor]
  (when (semantic-opted-in? descriptor)
    (let [spec (semantic-spec-for descriptor)]
      {:semantic/spec spec
       :semantic/digest (spec-digest spec)})))

(defn subsumes?
  "True when a granted semantic spec covers the requested semantic spec.
  Missing, malformed, unknown, or stale specs never cover anything."
  [granted requested]
  (try
    (let [g (normalize-spec granted)
          r (normalize-spec requested)]
      (and (= (:semantic/effect g) (:semantic/effect r))
           (>= (get risk-rank (:semantic/risk g) -1)
               (get risk-rank (:semantic/risk r) -1))))
    (catch clojure.lang.ExceptionInfo _ false)))

(defn request-resource
  "Attach a frozen semantic spec to a normalized request resource."
  [resource frozen]
  (if frozen
    (assoc resource :semantic/spec (:semantic/spec frozen)
           :semantic/digest (:semantic/digest frozen))
    resource))
