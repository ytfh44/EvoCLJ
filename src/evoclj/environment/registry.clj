(ns evoclj.environment.registry
  "EnvironmentRegistry - minimal in-memory registry for LiveSource.

  Pipeline: invalidate -> mark dirty -> single-flight snapshot ->
  validate -> derive candidate -> atomic swap. Generation (seq) only
  increments on successful publish. Failure keeps last-good, marks
  degraded and dirty. Identical content returns noop without new seq.
  Listeners receive publication diff, not raw file events.

  E1 (per-source registry): state is held PER SOURCE. Each registered
  source owns its own {:current :last-good :seq :history :status
  :last-refresh-error} entry under :per-source, keyed by source id.
  A parameterless refresh! (no source-id) re-syncs EVERY registered
  source; an explicit refresh! with a source-id updates only that source.
  The top-level :current/:last-good/:seq/:history/:status remain a derived
  aggregate (latest published revision across all sources) so the
  single-source contract and bundle.clj publication keep working.

  E2 (single-transaction Source -> Revision -> Projector -> Bundle): the
  chain for each source is
      (snapshot! src)   ; PURE: read only, no publication
      (project src snap); PURE: snapshot -> candidate bundle opts (may throw)
      (prepare-bundle registry proj) ; PURE: validate + index, NO mutation
  and ALL prepared bundles are then applied in ONE atomic swap. A throw at
  any step (snapshot failure, projector failure, bundle-prepare failure)
  leaves that source's prior published revision intact (fail-closed); the
  swap never installs a torn half-published bundle. snapshot! is purified
  (INV-06): it performs no registry mutation, no counter advance, no
  publication — that belongs to this transaction boundary.

  E4 (EnvironmentSnapshot): evoclj.environment.snapshot/pin! captures the
  full publication state as an immutable EnvironmentSnapshot and
  rebuild-root-manifest deterministically derives the canonical top-level
  view from that pinned value alone. This namespace exposes aggregate-view
  so BOTH the live swap path and the pinned-snapshot rebuild share ONE
  aggregation implementation (INV-05).

  E6 (dynamic environment host componentization): the registry becomes a
  real Integrant host component (evoclj.kernel.system
  :environment/registry). This namespace supplies the two pieces the host
  lifecycle needs: valid-registry? (fail-closed validation of an INJECTED
  registry value at component-build time) and shutdown! (clean, idempotent
  teardown — closes every held source-subscription handle and resets state;
  called by the host's halt-key!).

  S10 (source removal tombstone / catalog projection from latest payload):
  remove-source! records a tombstone (source id marked removed), releases the
  removed source's subscription handle + source record, DROPS its owned
  logical-ids/bundles/surfaces from the publication indexes, and recomputes
  the aggregate from the REMAINING sources. A removed source is NOT
  re-instantiated by a later refresh and its artifacts never surface again in
  the catalog projection (never stale/dead). Re-registering the same
  source-id clears the tombstone and produces a fresh per-source entry.
  catalog-projection derives the available view from the registry's published
  indexes plus explicit :removed tombstone marks. Removing a source that was
  never registered fails closed :environment/no-source; re-removing an
  already-removed source is idempotent."
  (:require [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.environment.bundle :as bundle]
            [evoclj.kernel.error :as err]
            [evoclj.support.failpoint :as fault]))

(declare refresh! refresh-async!)

(defn- initial-state
  "The fresh registry state shape. ONE implementation (INV-05): both
   create-registry and shutdown! derive their value from this fn."
  []
  {:sources {}
   :per-source {}
   :source-subs {}
   :current nil
   :last-good nil
   :seq 0
   :status :ok
   :dirty? false
   :last-refresh-error nil
   :listeners {}
   :history []
   :tombstones {}})

(defn create-registry []
  (let [lock (Object.)]
    (atom (assoc (initial-state) :lock lock))))

(defn valid-registry?
  "True when x is an EnvironmentRegistry atom created by
   create-registry (an atom whose value carries the registry state
   shape: :sources, :per-source and the publication :lock). WO-E6: lets
   host components validate an INJECTED registry value fail-closed at
   build time instead of crashing with an untyped error deep inside a
   swap!. evoclj.environment.snapshot performs the equivalent structural
   gate at pin time; bundle/publish-bundle! checks :lock likewise — same
   condition, layer-appropriate enforcement points."
  [x]
  (and (instance? clojure.lang.Atom x)
       (map? @x)
       (contains? @x :sources)
       (contains? @x :per-source)
       (some? (:lock @x))))

(defn shutdown!
  "WO-E6 (host componentization): tear the registry down CLEANLY. In order:

     1. close every held source-subscription handle (:source-subs), so a
        registered source stops calling back into this registry. For a
        registered McpSource this ALSO removes its M17 invalidate callback
        from the shared manager (the handle IS the manager subscription);
     2. drop every listener (:listeners);
     3. reset the atom to the fresh initial-state shape (same values as a
        newly created registry), PRESERVING the lock object identity so any
        racing access keeps locking correctly instead of NPE-ing.

   Idempotent: a second call closes nothing and resets an already-empty
   state. Returns nil (the halt-key! contract)."
  [registry]
  (when (valid-registry? registry)
    (doseq [handle (vals (:source-subs @registry))]
      (try (when-let [close! (:close! handle)] (close!))
           (catch Throwable _ nil)))
    (let [lock (:lock @registry)]
      (reset! registry (assoc (initial-state) :lock lock))))
  nil)

(defn- registry-lock [registry]
  (:lock @registry))

(defn- source-id-of [source]
  (or (:source/id source)
      (:source-id source)
      (try (:source/id (src/snapshot! source))
           (catch Exception _ nil))))

(defn register-source!
  [registry source]
  (when-not (satisfies? src/LiveSource source)
    (throw (err/error :environment/invalid-source "source must satisfy LiveSource" {:source source})))
  (let [cn (.getName (class source))]
    (when-not (or (.contains cn "FakeSource") (.contains cn "StaticSource") (.contains cn "McpSource")
                  (.contains cn "SkillSource") (.contains cn "Skill") (.contains cn "skill"))
      (throw (err/error :environment/unsupported-source "only FakeSource, StaticSource, McpSource and SkillSource are supported" {:source-type cn}))))
  (let [sid (source-id-of source)]
    (when-not sid
      (throw (err/error :environment/invalid-source "source snapshot must contain :source/id" {})))
    (swap! registry (fn [s]
                      (-> s
                          ;; S10: re-registering a removed source-id clears its
                          ;; tombstone -> a FRESH per-source entry (never a
                          ;; resurrected stale one).
                          (update :tombstones dissoc sid)
                          (assoc-in [:sources sid] source)
                          (assoc-in [:per-source sid] {:current nil
                                                        :last-good nil
                                                        :seq 0
                                                        :history []
                                                        :status :ok
                                                        :last-refresh-error nil
                                                        :owned-logical-ids #{}}))))
    (let [handle (src/subscribe! source (fn [] (swap! registry assoc :dirty? true) (refresh-async! registry sid)))]
      (swap! registry assoc-in [:source-subs sid] handle))
    sid))

;; --- top-level aggregate accessors (single-source contract preserved) -------

(defn current
  ([registry] (:current @registry))
  ([registry _source-id] (:current @registry)))

(defn last-good
  ([registry] (:last-good @registry))
  ([registry _source-id] (:last-good @registry)))

(defn status [registry]
  (let [s @registry]
    {:status (:status s) :dirty? (:dirty? s) :last-refresh-error (:last-refresh-error s) :seq (:seq s)}))

;; --- per-source accessors (E1) ----------------------------------------------

(defn source-state
  "Return the per-source state map for sid, or nil if not registered."
  [registry sid]
  (get-in @registry [:per-source sid]))

(defn source-current
  "Per-source current revision, or nil."
  [registry sid]
  (:current (source-state registry sid)))

(defn source-last-good
  "Per-source last-good revision, or nil."
  [registry sid]
  (:last-good (source-state registry sid)))

(defn source-seq
  "Per-source monotonic seq, or nil if not registered."
  [registry sid]
  (:seq (source-state registry sid)))

(defn source-status
  "Per-source status, or nil if not registered."
  [registry sid]
  (:status (source-state registry sid)))

(defn subscribe [registry listener-fn]
  (let [id (random-uuid)
        handle {:subscription/id id :close! (fn [] (swap! registry update :listeners dissoc id))}]
    (swap! registry assoc-in [:listeners id] listener-fn)
    handle))

(defn subscribe! [registry listener-fn]
  (subscribe registry listener-fn))

(defn- validate-snapshot [snapshot]
  (when-not (map? snapshot)
    (throw (err/error :environment/invalid-snapshot "snapshot must be a map" {:snapshot snapshot})))
  (when-not (:source/id snapshot)
    (throw (err/error :environment/invalid-snapshot "snapshot missing :source/id" {:snapshot snapshot})))
  (when-not (contains? snapshot :payload)
    (throw (err/error :environment/invalid-snapshot "snapshot missing :payload" {:snapshot snapshot})))
  snapshot)

;; ---------------------------------------------------------------------------
;; E2 — single-transaction Source -> Revision -> Projector -> Bundle
;; ---------------------------------------------------------------------------

(defn- normalize-project
  "Normalize a project result into a (possibly empty) vector of bundle-opts
  maps. project may return nil (no bundle), a single bundle-opts map, or a
  vector/collection of bundle-opts maps (e.g. one bundle per discovered
  skill)."
  [proj]
  (cond
    (nil? proj) []
    (map? proj) [proj]
    (sequential? proj) (vec proj)
    :else (throw (err/error :environment/invalid-project "project returned unsupported value" {:project proj}))))

(defn- plan-source-chain
  "Run the Source -> Revision -> Projector -> Bundle chain for ONE source,
  fail-closed and WITHOUT any registry mutation.

  `entry` is this source's per-source state (:per-source sid) captured at the
  start of the transaction. It is used ONLY for the fail-closed noop check: a
  source is a noop when every projected bundle's content identity equals that
  source's OWN published current revision/id — never the cross-source top-level
  aggregate. (Comparing against the top-level aggregate would make an unchanged
  sibling source look like a change and tear the atomic semantics.)

  Returns a map:
    {:sid sid
     :projected? boolean
     :preps <vector of prepare-bundle results>
     :noop? boolean            ; all projected bundles equal THIS source's current published revisions
     :error <Throwable | nil> ; set if snapshot/project/prepare threw
     :error-data <... | nil>}

  snapshot! is pure, project is pure, and prepare-bundle performs only checks
  (no publish). The actual publication happens later in a single atomic swap
  owned by refresh!. If any step throws, :error is set and the chain is left
  uncommitted for this source (prior published state preserved)."
  [registry src entry sid opts]
  (try
    (fault/trigger! opts :after-snapshot)
    (let [snapshot (src/snapshot! src)]
      (validate-snapshot snapshot)
      (fault/trigger! opts :after-validate)
      (let [proj (src/project src snapshot)]
        (fault/trigger! opts :after-project)
        (let [bundle-opts (normalize-project proj)]
          (if (empty? bundle-opts)
            {:sid sid :projected? false :preps [] :noop? true :error nil :error-data nil}
            (let [preps (mapv (fn [bo]
                                (fault/trigger! opts :after-bundle-publish)
                                (bundle/prepare-bundle registry bo))
                              bundle-opts)
                  cur-rev-id (:revision/id (:current entry))
                  noop? (every? #(= (:revision-id %) cur-rev-id) preps)]
              {:sid sid :projected? true :preps preps :noop? noop? :error nil :error-data nil})))))
    (catch Throwable e
      {:sid sid :projected? false :preps [] :noop? false :error e :error-data (err/error-data e)})))

(defn- advance-per-source-entry
  "Return an updated per-source entry after a successful (non-noop) publish of
  the plan's prepared bundles. Pure: returns the new entry map. Tracks the
  set of logical-ids this source published (:owned-logical-ids) so a later
  S10 removal can drop exactly its own artifacts."
  [entry plan]
  (let [next-seq (inc (or (:seq entry) 0))
        rev (assoc (:revision (last (:preps plan))) :revision/seq next-seq)
        owned (into (or (:owned-logical-ids entry) #{})
                    (map (fn [prep] (some-> prep :bundle :logical/id)) (:preps plan)))]
    (-> entry
        (assoc :current rev :last-good rev :seq next-seq :status :ok :last-refresh-error nil
               :owned-logical-ids owned)
        (update :history (fnil conj []) rev))))

(defn- advance-all
  "Advance per-source entries for all non-noop, non-error plans. Pure."
  [s per-src plans]
  (reduce (fn [acc [sid plan]]
            (if (or (:noop? plan) (some? (:error plan)))
              acc
              (assoc-in acc [:per-source sid] (advance-per-source-entry (get per-src sid) plan))))
          s
          plans))

(defn- install-prep
  "Merge one prepared bundle's surfaces/indexes/bundle into registry state s.
  Pure: returns the updated state map."
  [s prep]
  (let [indexes (:indexes prep)
        bundle (:bundle prep)
        new-surface-index (merge (or (:surfaces s) {}) (:surface-index indexes))
        new-bundles (assoc (or (:bundles s) {}) (:bundle/id bundle) bundle)
        new-bundle-index (merge (or (:bundle-index s) {}) (:bundle-index indexes))
        new-logical-index (merge (or (:logical-index s) {}) (:logical-index indexes))
        rev (:revision prep)
        with-indexes (assoc s
                            :surfaces new-surface-index
                            :bundles new-bundles
                            :bundle-index new-bundle-index
                            :logical-index new-logical-index
                            :indexes indexes)
        with-history (update with-indexes :history (fnil conj []) rev)]
    (update with-history :bundle-history (fnil conj []) bundle)))

(defn- mark-degraded
  "Mark per-source entries whose chain errored as :degraded, keeping last-good.
  Pure."
  [s plans]
  (reduce (fn [acc [sid plan]]
            (if (some? (:error plan))
              (update-in acc [:per-source sid]
                         (fn [e] (assoc e :status :degraded :last-refresh-error (:error-data plan))))
              acc))
          s
          plans))

(defn- compute-aggregate
  "Recompute the top-level :current/:last-good/:seq from per-source state.
  Pure."
  [per-src]
  (reduce (fn [acc [sid e]]
            (let [sq (:seq e)]
              (if (or (nil? (:current acc)) (> sq (:seq acc)))
                {:current (:current e) :last-good (:last-good e) :seq sq}
                acc)))
          {:current nil :last-good nil :seq -1}
          per-src))

(defn aggregate-view
  "E4 public pure view over per-source state: recompute the canonical
  top-level aggregate {:current :last-good :seq} exactly as the publication
  swap path does (this is the SAME implementation — INV-05, no parallel
  copy). evoclj.environment.snapshot uses it both to verify a registry is
  internally consistent at pin time and to prove a pinned EnvironmentSnapshot
  is self-describing: the recorded aggregate must follow from the pinned
  per-source entries alone."
  [per-src]
  (compute-aggregate per-src))

;; ---------------------------------------------------------------------------
;; S10 — source removal tombstone + catalog projection from latest payload
;; ---------------------------------------------------------------------------

(defn- drop-logical-ids
  "Remove `owned` logical-ids from a logical-index map keyed by logical-id."
  [m owned]
  (reduce dissoc (or m {}) owned))

(defn- drop-bundles-owned-by
  "Remove entries whose value bundle is owned by one of `owned` logical-ids
   from a bundle-id -> bundle map."
  [m owned]
  (into {} (remove (fn [[_ b]] (contains? owned (:logical/id b)))) (or m {})))

(defn- drop-surfaces-owned-by
  "Remove entries whose value surface is owned by one of `owned` logical-ids
   from a surface-id -> surface map."
  [m owned]
  (into {} (remove (fn [[_ s]] (contains? owned (:logical/id s)))) (or m {})))

(defn- drop-source-artifacts
  "Pure: drop a removed source's owned logical-ids/bundles/surfaces from the
   publication indexes, so a later catalog projection is never stale/dead.
   `owned` is the set of logical-ids that source published."
  [state owned]
  (let [owned (set owned)]
    (-> state
        (update :logical-index drop-logical-ids owned)
        (update :bundles drop-bundles-owned-by owned)
        (update :bundle-index drop-bundles-owned-by owned)
        (update :surfaces drop-surfaces-owned-by owned)
        (update :indexes (fn [idx]
                           (when (map? idx)
                             (-> idx
                                 (update :logical-index drop-logical-ids owned)
                                 (update :bundle-index drop-bundles-owned-by owned)
                                 (update :surface-index drop-surfaces-owned-by owned))))))))

(defn- close-best-effort!
  [x]
  (when x
    (try (x) (catch Throwable _ nil))))

(defn remove-source!
  "S10 source removal tombstone. Remove a registered source:

   - record a tombstone (source id marked removed) with an explicit marker;
   - release its subscription handle and the source record itself
     (revoke/cleanup semantics);
   - DROP its owned logical-ids/bundles/surfaces from the publication indexes
     so it no longer appears in the catalog projection (never stale/dead);
   - recompute the aggregate (:current/:last-good/:seq) from the REMAINING
     sources only, keeping it consistent so a later snapshot pin! stays
     coherent.

   Fail-closed / typed:
     - removing a source that was never registered throws
       :environment/no-source.
     - removing an already-removed source is IDEMPOTENT (returns the existing
       tombstone, no throw).

   A removed source is NOT re-instantiated by a later refresh (it is removed
   from :sources), and its artifacts no longer surface in the catalog
   projection. Re-registering the same source-id clears the tombstone and
   produces a fresh per-source entry (see register-source!).

   Returns {:status :removed :source/id sid :removed-logical-ids [..]
            :tombstone {:source/id sid :removed-at n ...}}."
  [registry source-id]
  (let [lock (registry-lock registry)]
    (locking lock
      (let [state @registry
            sources (:sources state)
            per-src (:per-source state)]
        (cond
          ;; already removed -> idempotent, return the same tombstone
          (contains? (:tombstones state) source-id)
          (let [tomb (get-in state [:tombstones source-id])]
            {:status :removed :idempotent? true :source/id source-id
             :removed-logical-ids (:removed-logical-ids tomb)
             :tombstone tomb})

          ;; never registered -> fail closed typed
          (not (contains? sources source-id))
          (throw (err/error :environment/no-source "cannot remove source: not registered"
                            {:source/id source-id}))

          :else
          (let [entry (get per-src source-id)
                owned (set (or (:owned-logical-ids entry) #{}))
                sub-handle (get-in state [:source-subs source-id])
                ;; revoke/cleanup: release the subscription handle then the
                ;; source record, best-effort and idempotent.
                _ (when sub-handle
                    (close-best-effort! (:close! sub-handle)))
                _ (when-let [src (get sources source-id)]
                    (close-best-effort! #(src/close! src)))
                now (System/currentTimeMillis)
                tomb {:source/id source-id :removed-at now
                      :removed-logical-ids (vec owned)}
                base (-> state
                         (assoc-in [:tombstones source-id] tomb)
                         (update :sources dissoc source-id)
                         (update :source-subs dissoc source-id)
                         (update :per-source dissoc source-id))
                removed-state (drop-source-artifacts base owned)
                top (compute-aggregate (:per-source removed-state))
                ents (vals (:per-source removed-state))
                any-degraded (boolean (some #(= :degraded (:status %)) ents))
                err-data (some #(:last-refresh-error %)
                               (filter #(= :degraded (:status %)) ents))
                final-state (-> removed-state
                                (assoc :current (:current top)
                                       :last-good (:last-good top)
                                       :seq (max 0 (or (:seq top) -1)))
                                (assoc :status (if any-degraded :degraded :ok)
                                       :dirty? false
                                       :last-refresh-error err-data))]
            (reset! registry final-state)
            {:status :removed :source/id source-id
             :removed-logical-ids (vec owned)
             :tombstone tomb}))))))

(defn source-removed?
  "S10: true when source-id has been removed (tombstoned)."
  [registry source-id]
  (contains? (:tombstones @registry) source-id))

(defn removed-sources
  "S10: the set of source ids currently marked removed (tombstoned)."
  [registry]
  (set (keys (:tombstones @registry))))

(defn tombstone
  "S10: the removal tombstone record for source-id, or nil when not removed."
  [registry source-id]
  (get-in @registry [:tombstones source-id]))

(defn catalog-projection
  "S10 catalog projection — the derived view of the currently available
   tools/skills/surfaces, recomputed from the MOST RECENT payload of the
   remaining (non-removed) sources. Derived purely from the registry's
   published indexes: removed-source artifacts have already been dropped by
   remove-source! so they are never stale/dead, and a removed source is
   surfaced ONLY as an explicit tombstone mark under :removed.

   Returns:
     {:logical-index {lid {:logical/id lid :bundle/id bid :revision/id rid}}
      :bundles        {bundle-id bundle}
      :surfaces       {surface-id surface}
      :removed        {source-id tombstone}}"
  [registry]
  (let [state @registry
        bundles (or (:bundles state) {})]
    {:logical-index (into (sorted-map-by (fn [a b] (< (compare (str a) (str b)) 0)))
                          (map (fn [[lid bid]]
                                 [lid {:logical/id lid
                                       :bundle/id bid
                                       :revision/id (some-> (get bundles bid) :revision/id)}]))
                          (or (:logical-index state) {}))
     :bundles bundles
     :surfaces (or (:surfaces state) {})
     :removed (or (:tombstones state) {})}))

(defn- apply-chain-swap!
  "Apply all successfully-prepared per-source chains in ONE atomic swap.
  `plans` is a map sid -> plan from plan-source-chain. Each non-noop,
  non-error plan contributes its prepared bundles' surfaces/indexes/bundles,
  and its per-source entry advances seq + current/last-good. This is the
  single transaction boundary: a source whose chain errored is simply not in
  the published set, so its prior published revision is intact; prepared
  bundles from healthy sources all land together, never torn."
  [registry state plans]
  (let [per-src (:per-source state)
        any-error (some #(some? (:error %)) (vals plans))
        published-preps (mapcat (fn [[_ p]] (when (and (nil? (:error p)) (not (:noop? p)))
                                            (:preps p)))
                                plans)]
    (swap! registry (fn [s]
                      (let [s1 (advance-all s per-src plans)
                            s2 (reduce install-prep s1 published-preps)
                            s3 (mark-degraded s2 plans)
                            new-per-src (:per-source s3)
                            top (compute-aggregate new-per-src)]
                        (assoc s3
                               :current (:current top)
                               :last-good (:last-good top)
                               :seq (max (:seq s3) (max 0 (:seq top)))
                               :status (if any-error :degraded :ok)
                               :dirty? (boolean any-error)
                               :last-refresh-error (when any-error
                                                     (some #(:error-data %) (vals plans))))))))
    ;; notify listeners for each published plan
    (doseq [[sid plan] plans]
      (when (and (nil? (:error plan)) (not (:noop? plan)))
        (let [prev (:current (get (:per-source state) sid))]
          (doseq [prep (:preps plan)]
            (let [curr (:revision prep)]
              (doseq [listener (vals (:listeners state))]
                (try (listener {:prev prev :curr curr :bundle (:bundle prep)}) (catch Exception _ nil)))))))))

(defn- per-source-results
  "Build the :per-source result map for refresh! from the per-source plans.
  Pure."
  [plans]
  (reduce (fn [m [sid p]]
            (let [published? (and (seq (:preps p)) (not (:noop? p)))
                  status (cond (some? (:error p)) :error
                               published? :published
                               (and (seq (:preps p)) (:noop? p)) :noop
                               :else :noop)]
              (cond-> (assoc m sid {:status status})
                published? (assoc-in [sid :revision] (:revision (last (:preps p))))
                (some? (:error p)) (assoc-in [sid :error-data] (:error-data p)))))
          {}
          plans))

(defn refresh!
  ([registry]
   (refresh! registry nil))
  ([registry source-id]
   (refresh! registry source-id nil))
  ([registry source-id {:as opts}]
   (let [lock (registry-lock registry)]
     (locking lock
       (swap! registry assoc :dirty? true)
       (let [state @registry
             sources (:sources state)
             per-src (:per-source state)
             target-ids (if source-id
                          (if (contains? sources source-id)
                            [source-id]
                            (throw (err/error :environment/no-source "no such source registered" {:source-id source-id})))
                          (vec (keys sources)))]
         (when (empty? target-ids)
           (throw (err/error :environment/no-source "no source registered" {:source-id source-id})))
         ;; Step 1-3 (snapshot -> project -> prepare) for every target source,
         ;; fail-closed and WITHOUT mutation.
         (let [plans (reduce (fn [m sid]
                               (assoc m sid (plan-source-chain registry (get sources sid) (get per-src sid) sid opts)))
                             {} target-ids)
               any-error (some #(some? (:error %)) (vals plans))
               any-published (some (fn [p] (and (seq (:preps p)) (not (:noop? p)) (nil? (:error p)))) (vals plans))
               ;; Step 4: single atomic swap applies all prepared bundles together.
               _ (apply-chain-swap! registry state plans)
               post-error (try
                            (fault/trigger! opts :mid-publish)
                            nil
                            (catch Throwable e e))]
           (when post-error
             (swap! registry assoc :status :degraded :last-refresh-error (err/error-data post-error)))
           (let [single? (= 1 (count target-ids))
                 single-plan (when single? (val (first plans)))
                 status (if post-error
                          :error
                          (if single?
                            (let [p (val (first plans))]
                              (cond (some? (:error p)) :error
                                    (and (seq (:preps p)) (not (:noop? p))) :published
                                    :else :noop))
                            (cond any-error :partial
                                  any-published :published-all
                                  :else :noop-all)))
                 error-ex (or post-error
                             (when single? (:error single-plan))
                             nil)]
             {:status status
              :error error-ex
              :revision (if single?
                          (or (when-let [preps (:preps single-plan)]
                                (when (seq preps) (:revision (last preps))))
                              (when-let [entry (get per-src (:sid single-plan))]
                                (:current entry)))
                          (some (fn [p] (when-let [preps (:preps p)]
                                          (when (seq preps) (:revision (last preps))))) (vals plans)))
              :error-data (if single?
                            (:error-data single-plan)
                            (some :error-data (vals plans)))
              :per-source (per-source-results plans)})))))))

(defn refresh-async!
  ([registry] (refresh-async! registry nil))
  ([registry source-id] (future (refresh! registry source-id))))
