(ns evoclj.eval.snapshot
  "EnvironmentSnapshot for paired evaluation.

  At eval start the current revision set is frozen into a snapshot
  {:environment/id <uuid> :sources {source-id revision-id}}.
  Parent and candidate executions pin to that captured environment E,
  so a live registry refresh does not create a treatment-effect
  difference between sides. The environment identity belongs to the
  experiment condition and never participates in phenotype hashing."
  (:require [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]))

(defn make-snapshot
  "Create a snapshot from an explicit sources map of source-id to
  revision-id string. Validates shape and assigns a fresh
  :environment/id and :captured-at."
  [sources]
  (when-not (map? sources)
    (throw (ex-info "sources must be a map" {:sources sources})))
  (doseq [[k v] sources]
    (when-not (keyword? k)
      (throw (ex-info "source key must be keyword" {:key k})))
    (when-not (and (string? v) (re-matches #"^sha256:[0-9a-f]{64}$" v))
      (throw (ex-info "revision id must be sha256:<64 hex>" {:key k :value v}))))
  {:environment/id (random-uuid)
   :sources (into {} sources)
   :captured-at (System/currentTimeMillis)})

(defn snapshot?
  "True when x looks like an EnvironmentSnapshot."
  [x]
  (and (map? x)
       (uuid? (:environment/id x))
       (map? (:sources x))
       (every? keyword? (keys (:sources x)))
       (every? #(and (string? %) (re-matches #"^sha256:[0-9a-f]{64}$" %))
               (vals (:sources x)))))

(defn revision-for
  "Pinned revision id for source-id inside snapshot, or nil."
  [snapshot source-id]
  (get-in snapshot [:sources source-id]))

(defn pinned-sources
  "The frozen sources map from snapshot."
  [snapshot]
  (:sources snapshot))

(defn- per-source-revision-id
  "The revision id a registry per-source entry currently serves, or nil.

  Two shapes exist: the E1/E2 registry stores the revision RECORD under
  :current, the E4 pinned state stores the revision ID string. :last-good
  is the fallback in both (a source whose current publication is absent
  still serves its last good revision)."
  [entry]
  (let [cur (or (:current entry) (:last-good entry))]
    (cond
      (string? cur) cur
      (map? cur) (or (:revision/id cur) (:revision-id cur))
      :else nil)))

(defn live-sources
  "Current live revision set from a registry atom.

  Reads the registry's :per-source entries when present (:current,
  falling back to :last-good — the revision each source last served);
  otherwise reads :history for the per-source latest, falling back to
  :current and to the live source payload when a source has not yet
  been published."
  [registry]
  (when-not (instance? clojure.lang.Atom registry)
    (throw (ex-info "registry must be an atom" {:registry registry})))
  (let [state @registry
        per-source (:per-source state)
        history (:history state)
        sources (:sources state)
        from-per-source (reduce-kv (fn [m sid entry]
                                     (if-let [rid (per-source-revision-id entry)]
                                       (assoc m sid rid)
                                       m))
                                   {}
                                   (or per-source {}))
        latest (cond
                 (seq from-per-source)
                 from-per-source

                 (seq history)
                 (reduce (fn [m r] (assoc m (:source/id r) (:revision/id r)))
                         {} history)

                 (:current state)
                 (if-let [rid (per-source-revision-id state)]
                   {(:source/id (:current state)) rid}
                   {})

                 :else {})
        full (reduce (fn [m [sid src]]
                       (if (contains? m sid)
                         m
                         (let [snap (try (src/snapshot! src) (catch Exception _ nil))
                               payload (:payload snap)
                               rid (when payload (rev/payload->id payload))]
                           (if rid (assoc m sid rid) m))))
                     latest
                     sources)]
    full))

(defn capture-snapshot
  "Freeze the current revision set from registry into a new
  EnvironmentSnapshot. The returned snapshot is immutable and can be
  shared by parent and candidate executions."
  [registry]
  (when-not (instance? clojure.lang.Atom registry)
    (throw (ex-info "registry must be an atom" {:registry registry})))
  (let [sources (live-sources registry)]
    {:environment/id (random-uuid)
     :sources sources
     :captured-at (System/currentTimeMillis)}))

;; RuntimeImage / ExecutionEnvironment identity lives in the compiler:
;; `evoclj.compiler.core/runtime-image-id` (RuntimeImageId) and
;; `evoclj.compiler.resolution/return-fingerprint` (returned-bytes
;; fingerprint). Eval call sites use those directly — see
;; `evoclj.eval.runner/runtime-identity`, which calls the compiler's
;; function. There is deliberately no eval-side mirror: a second copy
;; can only drift, and the eval side would then split identity from the
;; compiler's.
