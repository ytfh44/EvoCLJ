(ns evoclj.environment.snapshot
  "E4 — EnvironmentSnapshot: COMPLETE pin + root-manifest rebuild.

   An EnvironmentSnapshot is an immutable, self-describing value that pins
   the FULL publication state of an EnvironmentRegistry at one instant:
   every per-source entry (:current :last-good :seq :history :status
   :last-refresh-error), the canonical top-level aggregate, and all
   bundle/surface indexes (:bundles :surfaces :bundle-index :logical-index
   :indexes :history :bundle-history).

   PIN IS PURE (INV-06-aligned): pin! performs ZERO registry mutation — it
   reads the registry once under its own publication lock and returns a
   value. Because Clojure's persistent structures are immutable, later
   registry mutations (new revisions, degradation, direct source churn)
   can never leak into a pinned snapshot; live handles that COULD leak
   mutable state (:sources records holding atoms, :source-subs, :listeners,
   :lock) and transient scheduling flags (:dirty?, top-level
   :last-refresh-error) are deliberately excluded.

   ROOT MANIFEST REBUILD: rebuild-root-manifest is a PURE function of the
   pinned snapshot alone — manifest = f(pinned snapshot). It deterministically
   derives the canonical top-level view: the current revision per source plus
   the aggregate. The derivation reuses the registry's ONE aggregate
   implementation via evoclj.environment.registry/aggregate-view (INV-05),
   then verifies it against the aggregate recorded in the snapshot. Same
   snapshot -> identical manifest; rebuilding after registry churn yields the
   PINNED view, never the mutated live one.

   FAIL-CLOSED / TYPED:
     - pin! on nil / a non-registry atom throws :environment/invalid-registry.
     - pin! on a registry whose recorded top-level aggregate does NOT follow
       from its per-source state (a torn or divergent state, e.g. produced by
       mixing standalone publish-bundle! with the E1 per-source contract)
       refuses to pin with :environment/snapshot-inconsistent.
     - rebuild-root-manifest on structurally invalid input (nil, junk,
       unknown version, missing identity, malformed per-source entries)
       throws :environment/invalid-environment-snapshot.
     - rebuild-root-manifest where the pinned per-source entries no longer
       imply the recorded aggregate (corruption/tampering) throws
       :environment/snapshot-inconsistent.

   Snapshot identity (:snapshot/id) is a sha256 over the deterministic DATA
   projection of the pin: version, aggregate trio, full per-source entries
   and the sorted key sets of the surface/bundle/logical indexes. Surface and
   bundle VALUES contain materializer closures whose printed form is not
   stable, so they enter the identity through their content-addressed key
   sets only — the same convention revision payloads already rely on."
  (:require [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.kernel.error :as err]))

(def ^:private snapshot-version 1)
(def ^:private manifest-version 1)

(def ^:private entry-keys
  "The complete per-source entry fields a pin captures."
  [:current :last-good :seq :history :status :last-refresh-error])

(defn- now-millis [] (System/currentTimeMillis))

(defn- normalize-aggregate
  "compute-aggregate yields :seq -1 as its empty sentinel; the canonical
  baseline for 'nothing ever published' is seq 0 (matching a freshly created
  registry), so normalize exactly that case. Pure."
  [agg]
  (let [sq (or (:seq agg) 0)]
    (if (neg? sq) (assoc agg :seq 0) agg)))

(defn- aggregate-projection
  "Project an aggregate whose :current/:last-good are full Revision values
  onto the canonical IDENTITY view: revision ids plus seq. This is the form
  both the pinned snapshot and the rebuilt manifest record. Pure."
  [agg]
  {:current (some-> (:current agg) :revision/id)
   :last-good (some-> (:last-good agg) :revision/id)
   :seq (or (:seq agg) 0)})

(defn environment-snapshot?
  "True when x carries the EnvironmentSnapshot envelope (version, identity,
  per-source state and aggregate)."
  [x]
  (and (map? x)
       (= snapshot-version (:environment/snapshot-version x))
       (string? (:snapshot/id x))
       (map? (:per-source x))
       (map? (:aggregate x))))

;; ---------------------------------------------------------------------------
;; pin! — capture the complete, immutable EnvironmentSnapshot
;; ---------------------------------------------------------------------------

(defn- validate-registry-arg!
  "Validate the argument is a real EnvironmentRegistry atom whose current
  value has registry shape; returns the observed value. Typed
  :environment/invalid-registry on anything else (fail-closed, no NPE)."
  [registry]
  (when-not (and (instance? clojure.lang.Atom registry)
                 (map? @registry))
    (throw (err/error :environment/invalid-registry
                      "pin! requires an EnvironmentRegistry atom created by create-registry"
                      {:registry-class (some-> registry class .getName)})))
  (let [state @registry]
    (when-not (and (contains? state :per-source)
                   (contains? state :sources)
                   (some? (:lock state)))
      (throw (err/error :environment/invalid-registry
                        "value lacks EnvironmentRegistry shape (missing :per-source/:sources/:lock)"
                        {:keys (vec (keys state))})))
    state))

(defn- locked-state!
  "Re-read the registry WHILE HOLDING its publication lock, so a pin can
  never interleave with a mid-flight refresh! chain (defense in depth:
  atoms already guarantee tear-free single reads)."
  [registry]
  (locking (:lock @registry)
    @registry))

(defn- captured-top-aggregate
  "The canonical top-level trio exactly as recorded in the registry state."
  [state]
  {:current (:current state)
   :last-good (:last-good state)
   :seq (or (:seq state) 0)})

(defn- snapshot-id
  "Content identity of the pin: sha256 over the deterministic data projection
  (version, aggregate trio, full per-source entries, sorted index key sets)."
  [{:keys [aggregate per-source]} surfaces bundles logical-index]
  (rev/payload->id
   (sorted-map
    :version snapshot-version
    :aggregate (sorted-map :current (:current aggregate)
                           :last-good (:last-good aggregate)
                           :seq (:seq aggregate))
    :per-source (into (sorted-map-by (fn [a b] (< (compare (str a) (str b)) 0)))
                      per-source)
    :surface-ids (vec (sort-by str (keys surfaces)))
    :bundle-ids (vec (sort-by str (keys bundles)))
    :logical-ids (vec (sort-by str (keys logical-index))))))

(defn pin!
  "Pin the FULL publication state of `registry` into an immutable, self-
  describing EnvironmentSnapshot. Pure with respect to the registry: zero
  mutation, zero counter advance, zero publication (INV-06 discipline).

  Returns:
    {:environment/snapshot-version 1
     :snapshot/id   \"sha256:<hex>\"   ; content identity of the pin
     :pinned-at     <millis>
     :per-source    {sid {:current :last-good :seq :history :status
                          :last-refresh-error}}
     :aggregate     {:current <revision-id|nil> :last-good <revision-id|nil>
                     :seq n}                    ; canonical top-level view
     :registry-status                  :ok | :degraded at pin time
     :bundles :surfaces :bundle-index :logical-index :indexes
     :history :bundle-history}

  Fail-closed: typed :environment/invalid-registry for non-registry input;
  typed :environment/snapshot-inconsistent when the recorded top-level
  aggregate does not follow from the per-source state (torn/divergent
  registry — nothing is pinned from an incoherent world)."
  [registry]
  (validate-registry-arg! registry)
  (let [state (locked-state! registry)
        per-src (into (sorted-map-by (fn [a b] (< (compare (str a) (str b)) 0)))
                      (map (fn [[sid e]] [sid (select-keys e entry-keys)]))
                      (:per-source state))
        captured-top (aggregate-projection (captured-top-aggregate state))
        recomputed (aggregate-projection
                    (normalize-aggregate (reg/aggregate-view (:per-source state))))]
    ;; Self-description gate: a coherent registry's top-level aggregate ALWAYS
    ;; follows from its per-source entries (the swap path guarantees it). If
    ;; it does not, the state is torn/divergent — refuse to pin it.
    (when-not (= recomputed captured-top)
      (throw (err/error :environment/snapshot-inconsistent
                        "registry top-level aggregate disagrees with per-source state; refusing to pin a torn environment"
                        {:captured captured-top :recomputed recomputed})))
    (let [snap {:environment/snapshot-version snapshot-version
                :pinned-at (now-millis)
                :per-source per-src
                :aggregate recomputed
                :registry-status (or (:status state) :ok)
                :bundles (or (:bundles state) {})
                :surfaces (or (:surfaces state) {})
                :bundle-index (or (:bundle-index state) {})
                :logical-index (or (:logical-index state) {})
                :indexes (:indexes state)
                :history (or (:history state) [])
                :bundle-history (or (:bundle-history state) [])}]
      (assoc snap :snapshot/id (snapshot-id snap
                                            (:surfaces snap)
                                            (:bundles snap)
                                            (:logical-index snap))))))

;; ---------------------------------------------------------------------------
;; rebuild-root-manifest — pure derivation of the canonical top-level view
;; ---------------------------------------------------------------------------

(defn- validate-snapshot-shape!
  "Structural validation of snapshot input. Throws typed
  :environment/invalid-environment-snapshot on anything that is not an
  EnvironmentSnapshot envelope."
  [snapshot]
  (when-not (map? snapshot)
    (throw (err/error :environment/invalid-environment-snapshot
                      "environment snapshot must be a map" {:input-class (some-> snapshot class .getName)})))
  (when-not (= snapshot-version (:environment/snapshot-version snapshot))
    (throw (err/error :environment/invalid-environment-snapshot
                      "unsupported or missing :environment/snapshot-version"
                      {:version (:environment/snapshot-version snapshot)})))
  (when-not (string? (:snapshot/id snapshot))
    (throw (err/error :environment/invalid-environment-snapshot
                      "environment snapshot missing :snapshot/id" {})))
  (when-not (map? (:per-source snapshot))
    (throw (err/error :environment/invalid-environment-snapshot
                      "environment snapshot missing :per-source map" {})))
  (doseq [[sid e] (:per-source snapshot)]
    (when-not (and (map? e)
                   (integer? (:seq e))
                   (contains? e :current)
                   (contains? e :last-good))
      (throw (err/error :environment/invalid-environment-snapshot
                        "malformed per-source entry"
                        {:source/id sid :entry-keys (vec (keys e))}))))
  (when-not (map? (:aggregate snapshot))
    (throw (err/error :environment/invalid-environment-snapshot
                      "environment snapshot missing :aggregate map" {})))
  snapshot)

(defn- manifest-source-entry
  "Deterministic projection of one pinned per-source entry onto the manifest:
  the CURRENT revision identity plus the source status. Sources registered
  but never published project their status only (no revision yet). Pure."
  [entry]
  (let [cur (:current entry)]
    (cond-> {:status (or (:status entry) :ok)}
      (some? cur) (assoc :revision/id (:revision/id cur)
                         :revision/seq (:revision/seq cur)))))

(defn rebuild-root-manifest
  "PURE: deterministically rebuild the ROOT MANIFEST — the canonical
  top-level view (current revision per source + aggregate) — from a pinned
  EnvironmentSnapshot alone. manifest = f(pinned snapshot): no registry, no
  live state, no clock. Rebuilding twice yields an identical value;
  rebuilding after arbitrary registry churn yields the PINNED view.

  Returns:
    {:root-manifest/version 1
     :root-manifest/snapshot-id <pinned :snapshot/id>
     :sources    {sid {:status :ok, :revision/id \"sha256:..\", :revision/seq n}}
                 ; sorted by source id for byte-stable derivations
     :aggregate  {:current <revision-id|nil> :last-good <revision-id|nil>
                  :seq n :source-count n}}

  Fail-closed: structural corruption -> :environment/invalid-environment-
  snapshot; semantic tampering (the pinned per-source entries no longer
  imply the recorded aggregate) -> :environment/snapshot-inconsistent."
  [snapshot]
  (validate-snapshot-shape! snapshot)
  (let [per-src (:per-source snapshot)
        rebuilt (aggregate-projection
                 (normalize-aggregate (reg/aggregate-view per-src)))
        recorded (select-keys (:aggregate snapshot) [:current :last-good :seq])]
    ;; Self-description proof: the manifest derivation uses ONLY pinned data;
    ;; if what it derives disagrees with what the snapshot recorded, the
    ;; snapshot was corrupted or tampered with — fail closed rather than
    ;; serve an unfaithful view.
    (when-not (= rebuilt recorded)
      (throw (err/error :environment/snapshot-inconsistent
                        "pinned snapshot is internally inconsistent: the recorded aggregate does not follow from the pinned per-source state"
                        {:recorded recorded :rebuilt rebuilt})))
    {:root-manifest/version manifest-version
     :root-manifest/snapshot-id (:snapshot/id snapshot)
     :sources (into (sorted-map-by (fn [a b] (< (compare (str a) (str b)) 0)))
                    (map (fn [[sid e]] [sid (manifest-source-entry e)]))
                    per-src)
     :aggregate (assoc rebuilt :source-count (count per-src))}))
