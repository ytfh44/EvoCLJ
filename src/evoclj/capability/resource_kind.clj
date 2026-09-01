(ns evoclj.capability.resource-kind
  "ResourceKindDescriptor — open registration, closed installation (C1).

  C1 replaces the 5-file hardcoded allowlist (schema/lease/policy/broker/SQL/store)
  with a single Descriptor registry. Each ResourceKind is a closed installation
  entry but modular in definition: adding a new kind requires only defining a
  descriptor and registering it — no changes to lease/policy/broker/store.

  Descriptor contract (per DAG C1):

    kind                  - keyword, e.g. :tool
    schema                - Malli schema for the resource map
    canonicalize          - resource -> canonical resource (idempotent)
    covers?               - (granted, requested, action) -> bool
    attenuates?           - (parent-resource, child-resource) -> bool
    meet                  - (a, b) -> resource | nil (GLB)
    serialize             - resource -> EDN string for DB
    deserialize           - EDN string -> resource
    allowed-actions       - set of keywords
    authorization-targets - vector of broker targets [{:source :request/:tool :action-from :request/:intent}]

  See docs/formal for GC coverage. All built-in filesystem path helpers are
  owned here, not in lease, so lease dispatches uniformly."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol ResourceKindDescriptor
  (kind [this] "Keyword identifying the kind, e.g. :tool")
  (resource-schema [this] "Malli schema for the resource map")
  (canonicalize [this resource] "Canonicalize resource to its normal form (idempotent).")
  (covers? [this granted requested action] "True when granted resource covers requested for action.")
  (attenuates? [this parent child] "True when child is narrower than parent (resource attenuation).")
  (meet [this a b] "Greatest lower bound of a and b, or nil when disjoint.")
  (serialize [this resource] "Serialize canonical resource to EDN string for DB.")
  (deserialize [this s] "Deserialize EDN string to resource map.")
  (allowed-actions [this] "Set of allowed action keywords for this kind.")
  (authorization-targets [this] "Vector of broker authorization targets for this kind."))

;; Backcompat aliases matching the spec wording
;; spec says schema/canonicalize/covers?/attenuates?/meet/serialize/allowed-actions/authorization-targets
;; Provide `schema` alias to resource-schema.
(defn schema
  "Alias for resource-schema — satisfies C1 wording schema/canonicalize/..."
  [d] (resource-schema d))

;; ---------------------------------------------------------------------------
;; Shared pure helpers (previously in lease)
;; ---------------------------------------------------------------------------

(defn canonicalize-path
  "Resolve a path string to canonical form by dropping empty and \".\" and
  popping \"..\" segments. Returns nil for non-string."
  [s]
  (when (string? s)
    (let [absolute? (str/starts-with? s "/")
          segments (->> (str/split s #"/")
                        (remove #{"" "."})
                        (reduce (fn [acc seg]
                                  (if (= seg "..")
                                    (if (seq acc) (pop acc) acc)
                                    (conj acc seg)))
                                []))]
      (str (when absolute? "/") (str/join "/" segments)))))

(defn- path-inside?
  [root path]
  (let [r (canonicalize-path root)
        p (canonicalize-path path)]
    (and r p
         (or (= r "/") (= r p)
             (str/starts-with? p (str r "/"))))))

(defn- canonicalize-mount-path
  [s]
  (when (string? s)
    (let [clean (str/replace s "\\" "/")
          segments (->> (str/split clean #"/")
                        (remove #{"" "."})
                        (reduce (fn [acc seg]
                                  (if (= seg "..")
                                    (if (seq acc) (pop acc) acc)
                                    (conj acc seg)))
                                []))]
      (str/join "/" segments))))

(defn- mount-path-inside?
  [grant-path req-path]
  (let [g (canonicalize-mount-path (or grant-path ""))
        p (canonicalize-mount-path (or req-path ""))]
    (or (= g "") (= g p) (str/starts-with? p (str g "/")))))

;; ---------------------------------------------------------------------------
;; Built-in descriptors
;; ---------------------------------------------------------------------------

(defrecord ToolDescriptor []
  ResourceKindDescriptor
  (kind [_] :tool)
  (resource-schema [_] [:map {:closed false} [:kind [:= :tool]] [:id keyword?]])
  (canonicalize [_ r] (when (map? r) (select-keys r [:kind :id])))
  (covers? [_ granted requested _action]
    (and (keyword? (:id granted))
         (= (:id granted) (:id requested))))
  (attenuates? [this parent child]
    ;; tool attenuates only when same id (no narrowing); treat as covers in both directions
    (and (covers? this parent child nil)
         (covers? this child parent nil)))
  (meet [_ a b]
    (when (= (:id a) (:id b)) a))
  (serialize [_ r] (pr-str (select-keys r [:kind :id])))
  (deserialize [_ s] (try (edn/read-string s) (catch Exception _ nil)))
  (allowed-actions [_] #{:invoke :read :write})
  (authorization-targets [_] [{:source :request :action-from :request}]))

(defrecord MemoryDescriptor []
  ResourceKindDescriptor
  (kind [_] :memory)
  (resource-schema [_] [:map {:closed false} [:kind [:= :memory]] [:id keyword?]])
  (canonicalize [_ r] (when (map? r) (select-keys r [:kind :id])))
  (covers? [_ granted requested _] (and (keyword? (:id granted)) (= (:id granted) (:id requested))))
  (attenuates? [this parent child] (and (covers? this parent child nil) (covers? this child parent nil)))
  (meet [_ a b] (when (= (:id a) (:id b)) a))
  (serialize [_ r] (pr-str (select-keys r [:kind :id])))
  (deserialize [_ s] (try (edn/read-string s) (catch Exception _ nil)))
  (allowed-actions [_] #{:invoke})
  (authorization-targets [_] [{:source :request :action-from :request}]))

(defrecord ModelDescriptor []
  ResourceKindDescriptor
  (kind [_] :model)
  (resource-schema [_] [:map {:closed false} [:kind [:= :model]] [:id some?]])
  (canonicalize [_ r] (when (map? r) (select-keys r [:kind :id])))
  (covers? [_ granted requested _]
    (and (:id granted)
         (let [g (str (:id granted)) n (str (:id requested))]
           (or (= g n)
               (and (str/ends-with? g "/*")
                    (str/starts-with? n (subs g 0 (dec (count g)))))))))
  (attenuates? [this parent child] (covers? this parent child nil))
  (meet [_ a b]
    (let [g (str (:id a)) n (str (:id b))]
      (cond
        (= g n) a
        (str/starts-with? n (str g "/")) b ; b more specific could be child of prefix
        (str/starts-with? g (str n "/")) a
        (and (str/ends-with? g "/*") (str/starts-with? n (subs g 0 (dec (count g))))) b
        (and (str/ends-with? n "/*") (str/starts-with? g (subs n 0 (dec (count n))))) a
        :else nil)))
  (serialize [_ r] (pr-str (select-keys r [:kind :id])))
  (deserialize [_ s] (try (edn/read-string s) (catch Exception _ nil)))
  (allowed-actions [_] #{:invoke})
  (authorization-targets [_] [{:source :request :action-from :request}]))

(defrecord FilesystemDescriptor []
  ResourceKindDescriptor
  (kind [_] :filesystem)
  (resource-schema [_] [:map {:closed false} [:kind [:= :filesystem]] [:path string?]])
  (canonicalize [_ r]
    (when (map? r)
      {:kind :filesystem :path (canonicalize-path (:path r))}))
  (covers? [_ granted requested _]
    (path-inside? (:path granted) (:path requested)))
  (attenuates? [this parent child] (covers? this parent child nil))
  (meet [_ a b]
    (let [pa (:path a) pb (:path b)
          ca (canonicalize-path pa) cb (canonicalize-path pb)]
      (cond
        (path-inside? ca cb) b
        (path-inside? cb ca) a
        :else nil)))
  (serialize [_ r] (pr-str {:kind :filesystem :path (canonicalize-path (:path r))}))
  (deserialize [_ s] (try (edn/read-string s) (catch Exception _ nil)))
  (allowed-actions [_] #{:invoke :read :list :stat :write :create :delete})
  (authorization-targets [_] [{:source :request :action-from :request}]))

(defrecord FilesystemPathDescriptor []
  ResourceKindDescriptor
  (kind [_] :filesystem/path)
  (resource-schema [_] [:map {:closed false} [:kind [:= :filesystem/path]] [:path string?]])
  (canonicalize [_ r]
    (when (map? r)
      (cond-> {:kind :filesystem/path :path (if (contains? r :mount/id)
                                              (canonicalize-mount-path (:path r))
                                              (canonicalize-path (:path r)))}
        (contains? r :mount/id) (assoc :mount/id (:mount/id r)))))
  (covers? [_ granted requested _]
    (if (contains? granted :mount/id)
      (and (= (:mount/id granted) (:mount/id requested))
           (mount-path-inside? (:path granted) (:path requested)))
      (path-inside? (:path granted) (:path requested))))
  (attenuates? [this parent child] (covers? this parent child nil))
  (meet [_ a b]
    (let [ma? (contains? a :mount/id) mb? (contains? b :mount/id)]
      (cond
        (and ma? mb? (not= (:mount/id a) (:mount/id b))) nil
        ma? (when (mount-path-inside? (:path a) (:path b)) b)
        mb? (when (mount-path-inside? (:path b) (:path a)) a)
        :else (let [ca (canonicalize-path (:path a)) cb (canonicalize-path (:path b))]
                (cond
                  (path-inside? ca cb) b
                  (path-inside? cb ca) a
                  :else nil)))))
  (serialize [_ r]
    (pr-str (cond-> {:kind :filesystem/path :path (if (contains? r :mount/id)
                                                     (canonicalize-mount-path (:path r))
                                                     (canonicalize-path (:path r)))}
              (contains? r :mount/id) (assoc :mount/id (:mount/id r)))))
  (deserialize [_ s] (try (edn/read-string s) (catch Exception _ nil)))
  (allowed-actions [_] #{:invoke :read :list :stat :write :create :delete})
  (authorization-targets [_] [{:source :tool :action-from :intent}
                               {:source :request :action-from :request}]))

;; ---------------------------------------------------------------------------
;; Registry — closed installation, modular definition
;; ---------------------------------------------------------------------------

(defonce ^:private registry-atom
  ;; Closed installation registry: kind keyword -> descriptor.
  ;; Populated with built-ins at load; additional kinds are added by defining
  ;; a descriptor and calling register! — no changes to lease/policy/broker/store.
  (atom {}))

(defn register!
  "Register a ResourceKindDescriptor. The descriptor's (kind) is its key.
  Throws if descriptor does not satisfy the protocol or kind is not a keyword."
  [descriptor]
  (when-not (satisfies? ResourceKindDescriptor descriptor)
    (throw (err/error :capability/invalid-descriptor
                      "descriptor must satisfy ResourceKindDescriptor"
                      {:value (try (str (type descriptor)) (catch Exception _ "unknown"))})))
  (let [k (kind descriptor)]
    (when-not (keyword? k)
      (throw (err/error :capability/invalid-descriptor
                        "descriptor kind must be a keyword"
                        {:kind k})))
    (swap! registry-atom assoc k descriptor)
    descriptor))

(defn unregister!
  "Remove a descriptor for kind (test helper)."
  [k]
  (swap! registry-atom dissoc k) nil)

(defn get-descriptor
  "Get descriptor for kind keyword, or nil."
  [k] (get @registry-atom k))

(defn all-descriptors
  "Return map of kind -> descriptor."
  [] @registry-atom)

(defn allowed-kinds
  "Set of registered kind keywords."
  [] (set (keys @registry-atom)))

(defn allowed-actions-by-kind
  "Map of kind -> allowed-actions set derived from descriptors."
  [] (into {} (map (fn [[k d]] [k (allowed-actions d)]) @registry-atom)))

(defn authorization-targets-for
  "Vector of broker targets for kind, or nil when unknown."
  [k] (some-> (get-descriptor k) authorization-targets))

;; ---------------------------------------------------------------------------
;; Helpers for lease/policy/broker dispatch
;; ---------------------------------------------------------------------------

(defn canonicalize-resource
  "Canonicalize a resource map via its descriptor; returns canonical resource
  or the original map when no descriptor is registered (fail-closed downstream)."
  [resource]
  (if-let [d (and (map? resource) (get-descriptor (:kind resource)))]
    (or (canonicalize d resource) resource)
    resource))

(defn covers-resource?
  "Descriptor-dispatched covers? — true when granted covers requested for action.
  Fail-closed when no descriptor or kind mismatch."
  [granted requested action]
  (let [gk (:kind granted) rk (:kind requested)]
    (and (= gk rk)
         (if-let [d (get-descriptor gk)]
           (boolean (covers? d granted requested action))
           false))))

(defn serialize-resource
  "Serialize a resource map to EDN string via its descriptor (or pr-str fallback)."
  [resource]
  (if-let [d (and (map? resource) (get-descriptor (:kind resource)))]
    (serialize d resource)
    (pr-str resource)))

(defn deserialize-resource
  "Deserialize an EDN string (or already-parsed map) to a resource map.
  Tries the kind's descriptor deserialize when the kind is registered,
  else returns the parsed map (or nil on garbage)."
  [s]
  (when s
    (let [m (if (map? s) s (try (edn/read-string s) (catch Exception _ nil)))]
      (if (and (map? m) (:kind m) (get-descriptor (:kind m)))
        (if (map? s)
          m
          (or (try (deserialize (get-descriptor (:kind m)) s) (catch Exception _ nil)) m))
        m))))

;; ---------------------------------------------------------------------------
;; Install built-ins
;; ---------------------------------------------------------------------------

(doseq [d [(map->ToolDescriptor {})
           (map->MemoryDescriptor {})
           (map->ModelDescriptor {})
           (map->FilesystemDescriptor {})
           (map->FilesystemPathDescriptor {})]]
  (register! d))
