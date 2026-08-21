(ns evoclj.eval.snapshot
  "EnvironmentSnapshot for paired evaluation.

  At eval start the current revision set is frozen into a snapshot
  {:environment/id <uuid> :sources {source-id revision-id}}.
  Parent and candidate executions pin to that captured environment E,
  so a live registry refresh does not create a treatment-effect
  difference between sides. The environment identity belongs to the
  experiment condition and never participates in phenotype hashing."
  (:require [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.genome.hash :as hash]))

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

(defn live-sources
  "Current live revision set from a registry atom.
  Reads history for per-source latest, falling back to current and to
  live source payload when a source has not yet been published."
  [registry]
  (when-not (instance? clojure.lang.Atom registry)
    (throw (ex-info "registry must be an atom" {:registry registry})))
  (let [state @registry
        history (:history state)
        sources (:sources state)
        latest (reduce (fn [m r] (assoc m (:source/id r) (:revision/id r))) {} history)
        latest (if (seq latest)
                 latest
                 (if-let [cur (:current state)]
                   {(:source/id cur) (:revision/id cur)}
                   {}))
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

(defn capture-from-revisions
  "Capture from an already resolved sources map. Useful when the caller
  already has the revision set without a registry handle."
  [sources]
  (make-snapshot sources))

(defn phenotype-id
  "Phenotype identity derived from abi, genome-id and resolution-id
  only. Environment snapshot intentionally excluded - changing the
  environment does not change the agent compile identity."
  [abi genome-id resolution-id]
  (when-not (map? abi)
    (throw (ex-info "abi must be map" {:abi abi})))
  (when-not (re-matches #"^sha256:[0-9a-f]{64}$" genome-id)
    (throw (ex-info "genome-id must be sha256" {:genome-id genome-id})))
  (when-not (re-matches #"^sha256:[0-9a-f]{64}$" resolution-id)
    (throw (ex-info "resolution-id must be sha256" {:resolution-id resolution-id})))
  (hash/text-digest (str (pr-str (into (sorted-map) abi)) genome-id resolution-id)))
