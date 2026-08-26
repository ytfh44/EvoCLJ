(ns evoclj.environment.e5-test
  "E5 — GC roots + tombstone compaction + bounded history (post-deferred).

   Behavioral contract (not shape-only), driven through production paths:
   - GC ROOTS: gc! keeps the set of reachable roots (live sources' current
     published bundles/surfaces) and reclaims unreachable garbage left behind
     by content updates (orphaned bundles, and surfaces that changed identity).
   - TOMBSTONE COMPACTION: the removal-tombstone set (:max-tombstones) stays
     bounded; the most recent N survive and the oldest are compacted away,
     deterministically.
   - BOUNDED HISTORY: per-source :history and aggregate :history /
     :bundle-history keep at most :max-history entries (most recent kept,
     oldest evicted); the bound is configurable.
   - FAULTS (>=2): gc! on an inconsistent index fails closed typed; overrunning
     the history bound deterministically evicts the oldest.
   - CONCURRENCY: gc! and refresh! race safely (shared-state invariant).
   - REGRESSION/DOC: the default bounds preserve the legacy unbounded behavior
     within the tested range; bounded controls align with the documented
     contract."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.surface :as surf]
            [evoclj.environment.source :as src]))

;; --- helpers ----------------------------------------------------------------

(defn- error-type
  "Return the :error/type carried by the ExceptionInfo thrown by thunk, else nil."
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

(defn- publish-sequence!
  "Publish payloads in order against an already-registered source."
  [registry source payloads]
  (doseq [v payloads]
    (fake/set-payload! source v)
    (reg/refresh! registry))
  registry)

;; A real LiveSource that emits a NEW surface-id per revision, so an old
;; surface becomes orphaned (proving gc! reclaims surfaces too). Class name
;; satisfies register-source!'s allowlist.
(defrecord FakeSourceNewSurface [source-id state]
  src/LiveSource
  (snapshot! [this]
    {:source/id source-id :payload (:payload @state) :captured-at 0})
  (project [this snapshot]
    (let [sid (:source/id snapshot)
          p (:payload snapshot)]
      {:logical-id sid
       :source-id sid
       :payload p
       :surfaces [(surf/make-context-surface
                    {:id (keyword (name sid) (str "ctx-" p))
                     :descriptor {:name (str sid) :payload p}
                     :materializer (fn ([] p) ([_ _] p) ([_ _ _] p))})]}))
  (subscribe! [this _invalidate-fn]
    {:subscription/id (random-uuid) :close! (fn [] nil)})
  (close! [this] nil))

;; --- RED 1: happy — gc! keeps roots, reclaims orphaned bundles --------------

(deftest gc-keeps-reachable-roots-reclaims-orphaned-bundles
  (testing "gc! keeps the current bundle of a live source and reclaims old ones"
    (let [s (fake/make-fake-source :test/a "v1")
          registry (reg/create-registry)]
      (reg/register-source! registry s)
      (publish-sequence! registry s ["v1" "v2" "v3"])
      (is (= 3 (count (:bundles @registry))) "three bundles accumulated before GC")
      (is (= (rev/payload->id "v3") (:revision/id (reg/current registry)))
          "current revision is the newest")
      (let [res (reg/gc! registry)]
        (is (= :ok (:status res)))
        (is (= 2 (:gc/reclaimed-bundles res)) "two orphaned bundles reclaimed"))
      (let [proj (reg/catalog-projection registry)]
        (is (= #{:test/a} (set (keys (:logical-index proj)))) "logical-index retains the root")
        (is (= 1 (count (:bundles proj))) "only the reachable bundle survives")
        (is (= (rev/payload->id "v3") (:revision/id (reg/current registry)))
            "current revision untouched by GC")
        (is (= (rev/payload->id "v3") (get-in proj [:logical-index :test/a :revision/id]))
            "root still points at the current bundle")
        (is (every? #(= :test/a (:logical/id %)) (vals (:bundles proj)))
            "every survivor belongs to a live root")))))

;; --- RED: gc! also keeps multiple sources' roots ----------------------------

(deftest gc-keeps-roots-across-multiple-sources
  (testing "gc! retains the current bundle of every live source"
    (let [a (fake/make-fake-source :test/a "A1")
          b (fake/make-fake-source :test/b "B1")
          registry (reg/create-registry)]
      (reg/register-source! registry a)
      (reg/register-source! registry b)
      (reg/refresh! registry)
      ;; A publishes a second revision -> A's first bundle becomes orphaned
      (fake/set-payload! a "A2")
      (reg/refresh! registry :test/a)
      (is (= 3 (count (:bundles @registry))) "A1+A2+B1 all present before GC")
      (reg/gc! registry)
      (let [proj (reg/catalog-projection registry)]
        (is (= #{:test/a :test/b} (set (keys (:logical-index proj)))) "both roots retained")
        (is (= 2 (count (:bundles proj))) "one bundle per live root")
        (is (= (rev/payload->id "A2") (get-in proj [:logical-index :test/a :revision/id]))
            "A points at its newest bundle")
        (is (= (rev/payload->id "B1") (get-in proj [:logical-index :test/b :revision/id]))
            "B points at its bundle")
        (is (not (reg/source-removed? registry :test/a)) "A still live")
        (is (not (reg/source-removed? registry :test/b)) "B still live")))))

;; --- RED: gc! reclaims orphaned surfaces (surface identity changed) ---------

(deftest gc-reclaims-orphaned-surfaces
  (testing "surfaces left behind by a changing surface-id are reclaimed by gc!"
    (let [s (->FakeSourceNewSurface :test/a (atom {:payload "v1"}))
          registry (reg/create-registry)]
      (reg/register-source! registry s)
      (reg/refresh! registry)                       ; publishes :a/ctx-v1
      (swap! (:state s) assoc :payload "v2")
      (reg/refresh! registry)                       ; publishes :a/ctx-v2
      (is (= 2 (count (:surfaces @registry))) "two surfaces accumulated (new surface-id each revision)")
      (let [res (reg/gc! registry)]
        (is (= 1 (:gc/reclaimed-surfaces res)) "one orphaned surface reclaimed")
        (is (= 1 (count (:surfaces @registry))) "only the reachable surface survives")
        (is (= (keyword (name :test/a) "ctx-v2") (:surface/id (first (vals (:surfaces @registry)))))
            "the newest surface is retained")))))

;; --- RED 2: branch — tombstones are bounded and oldest compacted away -------

(deftest tombstones-bounded-and-compact-oldest
  (testing "removal tombstones are bounded; the oldest are compacted away deterministically"
    (let [registry (reg/create-registry {:bounds {:max-tombstones 2}})]
      (doseq [[id p] [[:test/a "A"] [:test/b "B"] [:test/c "C"]]]
        (reg/register-source! registry (fake/make-fake-source id p))
        (reg/refresh! registry))
      (reg/remove-source! registry :test/a)
      (reg/remove-source! registry :test/b)
      (reg/remove-source! registry :test/c)
      (is (= 2 (count (:tombstones @registry))) "tombstone set bounded to 2")
      (is (= #{:test/b :test/c} (reg/removed-sources registry))
          "the two most recent removals survive; the oldest was compacted")
      (is (not (reg/source-removed? registry :test/a))
          "the oldest tombstone was compacted away")
      (is (true? (reg/source-removed? registry :test/b)))
      (is (true? (reg/source-removed? registry :test/c))))))

;; --- RED 3: branch — history is bounded (per-source + aggregate) ------------

(deftest history-bounded-keeps-most-recent-n
  (testing "per-source and aggregate history/bundle-history are bounded to :max-history"
    (let [s (fake/make-fake-source :test/a "v1")
          registry (reg/create-registry {:bounds {:max-history 3}})]
      (reg/register-source! registry s)
      (publish-sequence! registry s ["v1" "v2" "v3" "v4" "v5"])
      (let [entry (reg/source-state registry :test/a)
            hist (mapv :revision/id (:history entry))
            top-hist (mapv :revision/id (:history @registry))
            bundle-hist (mapv :revision/id (:bundle-history @registry))]
        (is (= 3 (count (:history entry))) "per-source history capped at 3")
        (is (= [(rev/payload->id "v3") (rev/payload->id "v4") (rev/payload->id "v5")] hist)
            "oldest evicted, newest retained in order")
        (is (= (rev/payload->id "v5") (:revision/id (:current entry)))
            "current still the newest revision")
        (is (= 3 (count (:history @registry))) "aggregate history capped at 3")
        (is (= 3 (count (:bundle-history @registry))) "bundle-history capped at 3")
        (is (= (rev/payload->id "v5") (:revision/id (reg/current registry)))
            "top-level current remains the newest")))))

;; --- RED 4 (fault): gc! on an inconsistent index fails closed typed ---------

(deftest gc-conflict-fails-closed-typed
  (testing "gc! on an inconsistent index fails closed and mutates nothing"
    (let [s (fake/make-fake-source :test/a "v1")
          registry (reg/create-registry)]
      (reg/register-source! registry s)
      (reg/refresh! registry)
      (let [before (:bundles @registry)]
        ;; inject a dangling logical-index entry referencing a missing bundle
        (swap! registry assoc-in [:logical-index :ghost/lid] "bundle:MISSING")
        (is (= :environment/gc-inconsistent
               (error-type #(reg/gc! registry)))
            "gc! throws typed on inconsistent index")
        (is (= before (:bundles @registry)) "no bundle dropped by the failed gc!")
        (is (contains? (:logical-index @registry) :ghost/lid)
            "inconsistent entry not silently removed")))))

(deftest gc-on-invalid-registry-fails-closed-typed
  (testing "gc! on a non-registry value is fail-closed typed"
    (is (= :environment/invalid-registry
           (error-type #(reg/gc! (atom {})))) "gc! validates the registry first")))

;; --- RED 5 (fault): history overflow deterministically evicts the oldest ----

(deftest history-overflow-evicts-oldest-deterministically
  (testing "overrunning the history bound deterministically evicts the oldest"
    (let [s (fake/make-fake-source :test/a "v1")
          registry (reg/create-registry {:bounds {:max-history 5}})]
      (reg/register-source! registry s)
      (publish-sequence! registry s ["v1" "v2" "v3" "v4" "v5" "v6" "v7" "v8"])
      (let [hist (vec (map :revision/id (:history (reg/source-state registry :test/a))))
            expected (mapv rev/payload->id ["v4" "v5" "v6" "v7" "v8"])]
        (is (= 5 (count hist)) "history stays at the bound")
        (is (= expected hist) "exactly the 5 newest retained, oldest 3 evicted")
        (is (not (contains? (set hist) (rev/payload->id "v1"))) "v1 evicted")
        (is (contains? (set hist) (rev/payload->id "v8")) "v8 retained")
        (is (= (rev/payload->id "v8") (:revision/id (reg/current registry)))
            "current points to the newest retained")))))

;; --- RED 6 (concurrency): gc! and refresh! race safely ----------------------

(deftest concurrent-gc-refresh-race-safe
  (testing "concurrent gc! and refresh! never corrupt the registry"
    (let [a (fake/make-fake-source :test/a "A")
          b (fake/make-fake-source :test/b "B")
          registry (reg/create-registry)]
      (reg/register-source! registry a)
      (reg/register-source! registry b)
      (reg/refresh! registry)
      (let [barrier (promise)
            f1 (future (deref barrier 5000 ::timeout)
                       (fake/set-payload! a "A2")
                       (reg/refresh! registry))
            f2 (future (deref barrier 5000 ::timeout)
                       (reg/gc! registry))
            _ (deliver barrier true)
            r1 @f1
            r2 @f2]
        (is (some? r1) "refresh completed")
        (is (= :ok (:status r2)) "gc completed")
        (is (reg/valid-registry? registry) "registry still valid")
        (is (some? (reg/current registry)) "aggregate current present")
        (is (not (reg/source-removed? registry :test/a)) "A still live")
        (is (not (reg/source-removed? registry :test/b)) "B still live")
        ;; invariant: every logical-index value references an existing bundle
        (let [state @registry
              lids (or (:logical-index state) {})
              bundles (or (:bundles state) {})]
          (is (every? #(contains? bundles %) (vals lids))
              "no dangling logical-index -> bundle reference after the race"))
        ;; per-source seq stays consistent with the bounded history
        (doseq [sid [:test/a :test/b]]
          (let [e (reg/source-state registry sid)]
            (is (int? (:seq e)) (str sid " seq realized"))
            (is (<= (count (:history e)) (:seq e)) (str sid " history bounded"))))))))

;; --- RED 7: doc/behavior consistency — default bounds preserve legacy -------

(deftest doc-default-bounds-preserve-legacy-contract
  (testing "the default retention bounds preserve the legacy unbounded behavior"
    (let [s (fake/make-fake-source :test/a "v1")
          registry (reg/create-registry)   ; default bounds
          _ (reg/register-source! registry s)
          _ (publish-sequence! registry s ["v1" "v2" "v3" "v4" "v5"])]
      (is (= 5 (count (:history @registry))) "aggregate history not truncated by default")
      (is (= 5 (count (:history (reg/source-state registry :test/a))))
          "per-source history not truncated by default")
      (is (= 5 (count (:bundles @registry))) "bundles accumulate by default")
      (is (= 5 (:seq (reg/source-state registry :test/a))) "seq unchanged by default bounds"))))

(deftest doc-bounds-are-configurable-and-typed
  (testing "registry bounds are exposed and configurable per registry"
    (let [registry (reg/create-registry {:bounds {:max-history 7 :max-tombstones 3}})]
      (is (= 7 (get-in @registry [:bounds :max-history])) "max-history configured")
      (is (= 3 (get-in @registry [:bounds :max-tombstones])) "max-tombstones configured")
      ;; default registry carries defaults
      (is (= 128 (get-in @(reg/create-registry) [:bounds :max-history]))
          "default max-history is 128"))))
