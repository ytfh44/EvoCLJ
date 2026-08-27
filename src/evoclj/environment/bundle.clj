(ns evoclj.environment.bundle
  "SurfaceBundle and publication transaction.

  SurfaceBundle shape:
    {:bundle/id :revision/id :logical/id :surfaces [...]}

  E3 OWNERSHIP: every published bundle/surface-set has a clear owner — the
  logical source (:logical/id) that produced it. Ownership is stamped onto
  the registry's surface index (:logical/id + :bundle/id per stored surface)
  and enforced on update: only the owner can re-publish/replace its own
  surface ids; a DIFFERENT logical id claiming an owned surface id or an
  owned bundle id is a typed :bundle/collision and fails closed (no silent
  overwrite). Legitimate re-publication by the SAME owner — a content
  update / new revision, each its own content-addressed bundle id — always
  succeeds. Ownership survives across refreshes; readers surface-owner /
  bundle-owner expose the stamped owner.

  Sibling surfaces must be co-versioned (same revision/id). For Skill,
  Context and Directory are bound together atomically; no half-bound
  state where context succeeds but mount fails.

  Publication transaction constructs the entire candidate bundle, checks
  collision (duplicate surface ids), checks descriptor validity, checks
  index projection, then performs a single atomic swap to publish
  revision + surfaces + indexes. Any failure leaves no partial surface set.

  Integrates with EnvironmentRegistry created via
  evoclj.environment.registry/create-registry and reuses
  evoclj.environment.revision for content identity."
  (:require [evoclj.environment.bounded :as bounded]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.surface :as surf]
            [evoclj.kernel.error :as err]))

(defn bundle?
  [x]
  (and (map? x)
       (contains? x :bundle/id)
       (contains? x :revision/id)
       (contains? x :logical/id)
       (contains? x :surfaces)
       (vector? (:surfaces x))))

(defn- ensure-revision-on-surfaces
  [surfaces revision-id]
  (mapv (fn [s]
          (if (:revision/id s)
            (if (= (:revision/id s) revision-id)
              s
              (throw (err/error :bundle/co-version-violation
                                "sibling surfaces must share same revision/id"
                                {:surface s :expected revision-id})))
            (assoc s :revision/id revision-id)))
        surfaces))

(defn validate-bundle
  "Validate bundle structure and co-versioning. Throws typed error on failure."
  [bundle]
  (when-not (bundle? bundle)
    (throw (err/error :bundle/invalid "bundle missing required keys or surfaces not vector" {:bundle bundle})))
  (when-not (string? (:revision/id bundle))
    (throw (err/error :bundle/invalid "bundle :revision/id must be string" {:bundle bundle})))
  (when-not (:bundle/id bundle)
    (throw (err/error :bundle/invalid "bundle missing :bundle/id" {:bundle bundle})))
  (when-not (:logical/id bundle)
    (throw (err/error :bundle/invalid "bundle missing :logical/id" {:bundle bundle})))
  (let [surfaces (:surfaces bundle)]
    (doseq [s surfaces]
      (surf/validate-surface s))
    ;; sibling co-versioned
    (let [revs (set (map :revision/id surfaces))]
      (when (and (seq surfaces) (> (count revs) 1))
        (throw (err/error :bundle/co-version-violation
                          "sibling surfaces must be co-versioned (same revision/id)"
                          {:revision-ids revs :bundle bundle})))
      (when (and (seq surfaces) (not= (first revs) (:revision/id bundle)))
        (throw (err/error :bundle/co-version-violation
                          "bundle revision/id must match surfaces revision/id"
                          {:bundle-rev (:revision/id bundle) :surface-revs revs}))))
    ;; collision within bundle: duplicate surface ids
    (let [ids (map :surface/id surfaces)
          distinct-ids (set ids)]
      (when (not= (count ids) (count distinct-ids))
        (throw (err/error :bundle/collision "duplicate surface ids within bundle"
                          {:surface-ids ids :bundle bundle}))))
    bundle))

(defn make-bundle
  "Construct a SurfaceBundle. Ensures sibling surfaces share revision/id.
   If surfaces lack :revision/id they are stamped with revision-id.
   Throws if co-versioning violated."
  [{:keys [bundle-id revision-id logical-id surfaces]}]
  (when-not revision-id
    (throw (err/error :bundle/invalid "make-bundle requires :revision-id" {})))
  (when-not logical-id
    (throw (err/error :bundle/invalid "make-bundle requires :logical-id" {})))
  (when-not bundle-id
    (throw (err/error :bundle/invalid "make-bundle requires :bundle-id" {})))
  (let [surfaces (vec (or surfaces []))
        stamped (ensure-revision-on-surfaces surfaces revision-id)
        bundle {:bundle/id bundle-id
                :revision/id revision-id
                :logical/id logical-id
                :surfaces stamped}]
    (validate-bundle bundle)
    bundle))

(defn- check-collision-against-registry
  "E3 ownership-aware collision check against the *current* registry state,
  before any mutation.

  OWNERSHIP MODEL: every published bundle/surface-set is owned by the
  logical source that produced it (:logical/id — the stable per-source
  identity). :bundle/id and :surface/id are claimed resources. Only the
  owner may re-publish or replace its own claims; a DIFFERENT logical id
  claiming an owned resource is a typed :bundle/collision and fails the
  whole publication (no silent overwrite, no torn state).

  Fail-closed semantics:
    - bundle/id already present and bound to a DIFFERENT :logical/id:
      cross-owner takeover of the bundle id -> typed :bundle/collision.
    - bundle/id present, SAME owner, different :revision/id: a published
      content-addressed bundle id may never be rebound to different
      content -> typed :bundle/collision.
    - bundle/id present, SAME owner, same :revision/id, but a different
      surface-id set: contradictory content under one content address ->
      typed :bundle/collision.
    - surface id already bound (stamped) to a DIFFERENT :logical/id than
      the candidate: cross-owner takeover of the surface id -> typed
      :bundle/collision. Rebinding the SAME owner's surface id to a new
      revision (content update / new revision of the same source) is the
      legitimate update case and is allowed."
  [registry bundle]
  (let [bundles (or (:bundles @registry) {})
        existing (get bundles (:bundle/id bundle))
        candidate-rev (:revision/id bundle)
        lid (:logical/id bundle)]
    ;; 1a. bundle/id ownership conflict: claimed by a different source
    (when (and existing (not= (:logical/id existing) lid))
      (throw (err/error :bundle/collision "bundle id already owned by a different source"
                        {:bundle/id (:bundle/id bundle)
                         :owner-logical/id (:logical/id existing)
                         :candidate-logical/id lid
                         :existing-revision/id (:revision/id existing)
                         :candidate-revision/id candidate-rev})))
    ;; 1b. same owner rebinding a published content-addressed bundle id to
    ;;     different content.
    (when (and existing (not= candidate-rev (:revision/id existing)))
      (throw (err/error :bundle/collision "bundle id already exists with different content"
                        {:bundle/id (:bundle/id bundle)
                         :existing-revision/id (:revision/id existing)
                         :candidate-revision/id candidate-rev})))
    ;; 1c. same owner, same content identity, but a different surface-id set:
    ;;     contradictory content under a single content address.
    (when (and existing
               (not= (vec (sort-by str (map :surface/id (:surfaces bundle))))
                     (vec (sort-by str (map :surface/id (:surfaces existing))))))
      (throw (err/error :bundle/collision "bundle id already exists with a different surface set"
                        {:bundle/id (:bundle/id bundle)
                         :owner-logical/id lid
                         :existing-surface-ids (vec (sort-by str (map :surface/id (:surfaces existing))))
                         :candidate-surface-ids (vec (sort-by str (map :surface/id (:surfaces bundle))))})))
    ;; 2. surface-id ownership conflict across DISTINCT sources.
    ;; A surface id whose existing owner shares THIS candidate's :logical/id
    ;; (same source rebinding its stable surface to a new revision) is allowed.
    ;; A surface id already bound to a DIFFERENT :logical/id is a genuine
    ;; ownership conflict and must fail.
    (let [existing-surfaces (or (:surfaces @registry) {})
          new-ids (set (map :surface/id (:surfaces bundle)))]
      (doseq [sid new-ids]
        (when-let [bound (get existing-surfaces sid)]
          ;; owned by a different source -> conflict.
          (when-not (= (:logical/id bound) lid)
            (throw (err/error :bundle/collision "surface id already bound to a different source"
                              {:surface/id sid
                               :bound-logical/id (:logical/id bound)
                               :candidate-logical/id lid
                               :bound-bundle/id (:bundle/id bound)
                               :candidate-bundle/id (:bundle/id bundle)}))))))
    nil))

(defn- check-index-projection
  [bundle]
  ;; Index projection must be derivable: every surface has :surface/id and
  ;; bundle has :bundle/id and :logical/id. If any missing, fail.
  (doseq [s (:surfaces bundle)]
    (when-not (:surface/id s)
      (throw (err/error :bundle/index-projection-failed "surface missing :surface/id for index"
                        {:surface s}))))
  (when-not (:bundle/id bundle)
    (throw (err/error :bundle/index-projection-failed "bundle missing :bundle/id for index" {:bundle bundle})))
  (when-not (:logical/id bundle)
    (throw (err/error :bundle/index-projection-failed "bundle missing :logical/id for index" {:bundle bundle})))
  ;; project indexes and ensure no nil keys. Each surface in the surface-index
  ;; is stamped with its owning :logical/id (stable per-source identity) and
  ;; :bundle/id so the collision check (and the published registry state) can
  ;; tell "same source, new revision" (allowed) from "another source owns this
  ;; surface id" (collision). bundle/id changes with content, so :logical/id
  ;; is the stable ownership key.
  (let [bid (:bundle/id bundle)
        lid (:logical/id bundle)
        surface-index (into {} (map (fn [s] [(:surface/id s) (assoc s :logical/id lid :bundle/id bid)]) (:surfaces bundle)))
        bundle-index {bid bundle}
        logical-index {lid bid}]
    (when (some nil? (keys surface-index))
      (throw (err/error :bundle/index-projection-failed "surface index contains nil key" {:bundle bundle})))
    {:surface-index surface-index :bundle-index bundle-index :logical-index logical-index}))

(defn prepare-bundle
  "Pure preparation step of the bundle publication transaction.

  Resolves/constructs the candidate bundle and runs all pre-mutation checks
  (validate bundle + co-versioning, check collision against the *current*
  registry state, validate each surface, check index projection). Returns a
  data map describing the publication:

    {:status :noop|:published
     :bundle <resolved bundle>
     :revision-id <content identity of the bundle>
     :indexes {:surface-index :bundle-index :logical-index}
     :revision <the revision value that would be published (for :published)
      :prev-seq <seq of the registry at prepare time (for seq allocation>}

  This function performs NO mutation of the registry. It may throw a typed
  error (e.g. :bundle/collision, :bundle/co-version-violation,
  :bundle/index-projection-failed) — that throw is the fail-closed signal used
  by the Source -> Revision -> Projector -> Bundle single-transaction path:
  if preparation fails, NOTHING is published.

  Callers that want a standalone atomic publication use `publish-bundle!`,
  which wraps this in a single swap."
  [registry bundle-or-opts]
  (let [registry (or registry (throw (err/error :bundle/invalid-registry "registry required" {})))
        ;; construct candidate bundle if opts given
        bundle (if (and (map? bundle-or-opts) (contains? bundle-or-opts :bundle/id) (contains? bundle-or-opts :surfaces))
                 bundle-or-opts
                 ;; allow opts map with :logical/id :payload :surfaces etc to construct bundle+revision internally
                 (let [{:keys [logical-id payload surfaces bundle-id revision-id]} bundle-or-opts]
                   (if (and logical-id payload)
                     (let [rev-id (or revision-id (rev/payload->id payload))
                           bid (or bundle-id (str "bundle:" rev-id ":" (str logical-id)))]
                       (make-bundle {:bundle-id bid :revision-id rev-id :logical-id logical-id :surfaces surfaces}))
                     (throw (err/error :bundle/invalid "publish-bundle! requires bundle map or {:logical-id :payload :surfaces}" {:opts bundle-or-opts})))))
        ;; 1-4 checks before mutation
        _ (validate-bundle bundle)
        _ (check-collision-against-registry registry bundle)
        ;; descriptor validity already done in validate-bundle, but explicit for ordering
        _ (doseq [s (:surfaces bundle)] (surf/validate-surface s))
        indexes (check-index-projection bundle)
        candidate-id (:revision/id bundle)
        cur (:current @registry)
        cur-id (:revision/id cur)
        prev-seq (:seq @registry)
        source-id (or (:logical/id bundle) :bundle/source)
        ;; Source-bundle-opts (e.g. from a LiveSource's project) may carry the
        ;; raw source :payload. When present we publish it as the revision
        ;; :payload to preserve the E1 single-source contract (the revision
        ;; :payload is the source payload, see registry-test). For the raw
        ;; bundle-map path (publish-bundle! with a constructed SurfaceBundle)
        ;; there is no separate :payload, so we fall back to the bundle-derived
        ;; payload (bundle/id + logical/id + surfaces), matching E1 publish-bundle!.
        src-payload (:payload bundle-or-opts)]
    (if (and cur-id
             (= candidate-id cur-id)
             ;; E3 OWNERSHIP GUARD: the early :noop is legitimate only when the
             ;; registry's current revision was published by THIS bundle's own
             ;; logical source. Another source publishing byte-identical content
             ;; must still publish its OWN revision — adopting the other source's
             ;; revision object here would install foreign attribution (the
             ;; adopted :source/id would name a different logical id).
             (= (:source/id cur) source-id))
      {:status :noop
       :bundle bundle
       :revision-id candidate-id
       :indexes indexes
       :revision cur
       :prev-seq prev-seq}
      (let [next-seq (inc (or prev-seq 0))
            payload (if (some? src-payload)
                      src-payload
                      {:bundle/id (:bundle/id bundle) :logical/id (:logical/id bundle) :surfaces (:surfaces bundle)})
            new-rev (-> (rev/make-revision source-id payload next-seq)
                        (assoc :revision/id candidate-id))]
        {:status :published
         :bundle bundle
         :revision-id candidate-id
         :indexes indexes
         :revision new-rev
         :prev-seq prev-seq}))))

(defn publish-bundle!
  "Publication transaction for a SurfaceBundle.

  Steps, in order, before any mutation:
    1. validate bundle and co-versioning
    2. check collision: E3 ownership enforcement (cross-owner surface-id or
       bundle-id claims are typed :bundle/collision; same-owner re-publish of
       a new revision is allowed) plus duplicate surface ids within bundle
    3. check descriptor validity (each surface validated)
    4. check index projection (indexes derivable)

  Then a single atomic swap publishes revision + surfaces + indexes.
  Any failure must leave registry with no partial surface set.
  Returns {:status :published :bundle bundle :revision revision}
  or throws typed error. On collision the whole publication fails.
  Reuses revision for content identity and seq for publication order.

  This is the standalone path. The Source -> Revision -> Projector -> Bundle
  single-transaction path (evoclj.environment.registry/refresh!) prepares all
  projected bundles via `prepare-bundle` and applies them in ONE swap, so a
  mid-chain failure leaves no torn bundle state."
  [registry bundle-or-opts]
  (let [registry (or registry (throw (err/error :bundle/invalid-registry "registry required" {})))
        lock (:lock @registry)]
    (when-not lock
      (throw (err/error :bundle/invalid-registry "registry missing :lock (not created via create-registry?)" {:registry registry})))
    (locking lock
      (let [{:keys [status bundle revision revision-id prev-seq indexes]} (prepare-bundle registry bundle-or-opts)]
        (if (= :noop status)
          {:status :noop :bundle bundle :revision revision}
          (loop []
            (let [cur-state @registry
                  cur-seq (:seq cur-state)]
              (if (not= cur-seq prev-seq)
                ;; concurrent publication happened; re-check candidate identity
                (let [new-cur (:current cur-state)
                      new-id (:revision/id new-cur)]
                  (if (= revision-id new-id)
                    {:status :noop :bundle bundle :revision new-cur}
                    (throw (err/error :bundle/concurrent-modification "concurrent publication modified registry" {:bundle bundle}))))
                (let [new-surface-index (merge (or (:surfaces cur-state) {}) (:surface-index indexes))
                      new-bundles (assoc (or (:bundles cur-state) {}) (:bundle/id bundle) bundle)
                      new-bundle-index (merge (or (:bundle-index cur-state) {}) (:bundle-index indexes))
                      new-logical-index (merge (or (:logical-index cur-state) {}) (:logical-index indexes))
                      next-seq (:revision/seq revision)
                      max-history (get-in cur-state [:bounds :max-history])
                      new-state (-> cur-state
                                    (assoc :current revision :last-good revision :seq next-seq :status :ok :dirty? false :last-refresh-error nil)
                                    (assoc :bundles new-bundles
                                           :surfaces new-surface-index
                                           :bundle-index new-bundle-index
                                           :logical-index new-logical-index
                                           :indexes indexes)
                                    (update :history #(bounded/keep-recent (conj (or % []) revision) max-history))
                                    (update :bundle-history #(bounded/keep-recent (conj (or % []) bundle) max-history)))]
                  (if (compare-and-set! registry cur-state new-state)
                    (do
                      (doseq [[_ listener] (:listeners cur-state)]
                        (try (listener {:prev (:current cur-state) :curr revision :bundle bundle}) (catch Exception _ nil)))
                      {:status :published :bundle bundle :revision revision})
                    (recur)))))))))))

(defn publish-surfaces!
  "Convenience: publish a set of peer surfaces atomically as a bundle.
  Constructs bundle from logical-id, payload, and surfaces, then delegates
  to publish-bundle!. Ensures two surfaces in same bundle share revision/id."
  [registry {:keys [logical-id payload surfaces bundle-id]}]
  (when-not logical-id
    (throw (err/error :bundle/invalid "publish-surfaces! requires :logical-id" {})))
  (when-not payload
    (throw (err/error :bundle/invalid "publish-surfaces! requires :payload" {})))
  (let [rev-id (rev/payload->id payload)
        bid (or bundle-id (str "bundle:" rev-id ":" (str logical-id)))
        bundle (make-bundle {:bundle-id bid :revision-id rev-id :logical-id logical-id :surfaces surfaces})]
    (publish-bundle! registry bundle)))

(defn list-bundles [registry] (vals (or (:bundles @registry) {})))
(defn get-bundle [registry bundle-id] (get-in @registry [:bundles bundle-id]))
(defn list-surfaces [registry] (vals (or (:surfaces @registry) {})))
(defn get-surface [registry surface-id] (get-in @registry [:surfaces surface-id]))

(defn surface-owner
  "E3 OWNERSHIP reader: the :logical/id that owns a published surface id,
  or nil when the surface id is not published."
  [registry surface-id]
  (:logical/id (get-in @registry [:surfaces surface-id])))

(defn bundle-owner
  "E3 OWNERSHIP reader: the :logical/id that owns a published bundle id,
  or nil when the bundle id is not published."
  [registry bundle-id]
  (:logical/id (get-in @registry [:bundles bundle-id])))
