(ns evoclj.environment.s10-test
  "S10 — source removal tombstone + catalog projection from most recent payload.

   Behavioral contract (not shape-only), exercised through production paths:
   - remove-source! records a tombstone (source id marked removed) and DROPS the
     removed source's artifacts (logical-ids/bundles/surfaces) from the catalog
     projection — removed artifacts are never stale/dead.
   - the catalog projection reflects the MOST RECENT payload of the remaining
     sources; a refresh after a payload change updates the projection.
   - re-adding a removed source clears the tombstone and produces a FRESH entry
     (seq restarts, artifacts re-published).
   - removing a source that was never registered is fail-closed typed
     (:environment/no-source).
   - removing an already-removed source is idempotent (no throw, same tombstone).
   - concurrent removal + refresh is race-safe (no torn/corrupt state)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]))

(defn- error-type
  "Return the :error/type carried by the ExceptionInfo thrown by thunk, else nil."
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

;; --- RED: happy path — removal tombstones + drops artifacts ------------------

(deftest remove-source-tombstones-and-drops-artifacts
  (testing "removing a source records a tombstone and drops its artifacts"
    (let [source (fake/make-fake-source :test/a "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          _ (reg/refresh! registry)
          proj-before (reg/catalog-projection registry)]
      (is (contains? (:logical-index proj-before) :test/a) "source A in catalog before removal")
      (is (seq (:bundles proj-before)) "A's bundle present before removal")
      (let [res (reg/remove-source! registry :test/a)]
        (is (= :removed (:status res)) "removal reports :removed")
        (is (= :test/a (:source/id res)) "removal identifies the removed source")
        (is (contains? (set (:removed-logical-ids res)) :test/a)
            "tombstone result carries the owned logical-id")
        (is (reg/source-removed? registry :test/a) "source marked removed")
        (let [proj (reg/catalog-projection registry)]
          (is (not (contains? (:logical-index proj) :test/a)) "A dropped from catalog projection")
          (is (empty? (:bundles proj)) "A's bundles dropped from catalog")
          (is (empty? (:surfaces proj)) "A's surfaces dropped from catalog"))
        (is (nil? (reg/current registry)) "aggregate current nil after removing the only source")
        (is (= 0 (:seq @registry)) "aggregate seq normalized to 0 for an empty registry")))))

;; --- RED: catalog projection from the MOST RECENT payload --------------------

(deftest catalog-projection-reflects-latest-payload
  (testing "catalog projection reflects the most recent payload of remaining sources"
    (let [a (fake/make-fake-source :test/a "A1")
          b (fake/make-fake-source :test/b "B1")
          registry (reg/create-registry)
          _ (reg/register-source! registry a)
          _ (reg/register-source! registry b)
          _ (reg/refresh! registry)
          _ (fake/set-payload! a "A2")
          _ (reg/refresh! registry :test/a)
          proj (reg/catalog-projection registry)]
      (is (= (rev/payload->id "A2") (get-in proj [:logical-index :test/a :revision/id]))
          "A's entry reflects its latest payload after refresh")
      (is (= (rev/payload->id "B1") (get-in proj [:logical-index :test/b :revision/id]))
          "B's entry reflects its latest payload")
      ;; removing A must not disturb B's most-recent payload in the projection
      (reg/remove-source! registry :test/a)
      (let [proj2 (reg/catalog-projection registry)]
        (is (not (contains? (:logical-index proj2) :test/a)) "A dropped after removal")
        (is (= (rev/payload->id "B1") (get-in proj2 [:logical-index :test/b :revision/id]))
            "B stays with its most-recent payload")
        ;; a later refresh of B updates the projection (never stale/dead)
        (fake/set-payload! b "B2")
        (reg/refresh! registry :test/b)
        (let [proj3 (reg/catalog-projection registry)]
          (is (= (rev/payload->id "B2") (get-in proj3 [:logical-index :test/b :revision/id]))
              "B's projection follows a later refresh"))))))

;; --- RED: re-addition of a source produces a fresh entry ---------------------

(deftest re-add-removed-source-produces-fresh-entry
  (testing "re-adding a removed source clears the tombstone and produces a fresh entry"
    (let [a1 (fake/make-fake-source :test/a "A1")
          registry (reg/create-registry)
          _ (reg/register-source! registry a1)
          _ (reg/refresh! registry)
          seq1 (get-in @registry [:per-source :test/a :seq])
          _ (reg/remove-source! registry :test/a)]
      (is (= 1 seq1) "initial publish advanced seq to 1")
      (is (reg/source-removed? registry :test/a) "tombstoned before re-add")
      (let [a2 (fake/make-fake-source :test/a "A2")
            _ (reg/register-source! registry a2)
            _ (reg/refresh! registry)]
        (is (not (reg/source-removed? registry :test/a)) "tombstone cleared on re-add")
        (is (= 1 (get-in @registry [:per-source :test/a :seq])) "fresh entry seq restarts at 1")
        (let [proj (reg/catalog-projection registry)]
          (is (contains? (:logical-index proj) :test/a) "re-added source appears in catalog")
          (is (= (rev/payload->id "A2") (get-in proj [:logical-index :test/a :revision/id]))
              "re-added entry carries the fresh payload"))))))

;; --- RED: fault — removing a source that was never registered is typed -------

(deftest remove-unknown-source-throws-typed
  (testing "removing a source that was never registered is fail-closed typed"
    (let [registry (reg/create-registry)
          _ (reg/register-source! registry (fake/make-fake-source :test/known "x"))]
      (is (= :environment/no-source
             (error-type #(reg/remove-source! registry :test/never-there)))
          "removing an unknown source throws :environment/no-source"))))

;; --- RED: fault — removing an already-removed source is idempotent -----------

(deftest remove-already-removed-source-idempotent
  (testing "removing an already-removed source is idempotent"
    (let [a (fake/make-fake-source :test/a "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry a)
          _ (reg/refresh! registry)
          first-removal (reg/remove-source! registry :test/a)
          second-removal (reg/remove-source! registry :test/a)]
      (is (= :removed (:status second-removal)))
      (is (true? (:idempotent? second-removal)))
      (is (= (:tombstone first-removal) (:tombstone second-removal))
          "the same tombstone is returned on the re-removal")
      (is (reg/source-removed? registry :test/a)))))

;; --- RED: concurrency — removal + refresh race safe --------------------------

(deftest concurrent-removal-refresh-race-safe
  (testing "concurrent removal and refresh do not corrupt registry state"
    (let [a (fake/make-fake-source :test/a "A")
          b (fake/make-fake-source :test/b "B")
          registry (reg/create-registry)
          _ (reg/register-source! registry a)
          _ (reg/register-source! registry b)
          _ (reg/refresh! registry)
          barrier (promise)
          f1 (future (deref barrier 5000 ::timeout)
                     (fake/set-payload! a "A2")
                     (reg/refresh! registry))
          f2 (future (deref barrier 5000 ::timeout)
                     (reg/remove-source! registry :test/a))
          _ (deliver barrier true)
          r1 @f1
          r2 @f2]
      (is (some? r1) "refresh completed")
      (is (= :removed (:status r2)) "removal completed and tombstoned")
      (is (reg/source-removed? registry :test/a) "A tombstoned")
      (is (not (reg/source-removed? registry :test/b)) "B not touched")
      (is (not (contains? (get-in @registry [:logical-index]) :test/a)) "A not in logical-index")
      (is (contains? (get-in @registry [:logical-index]) :test/b) "B in logical-index")
      (is (some? (:current @registry)) "aggregate current still present after the race"))))

;; --- RED: doc/behavior consistency — a removed source stays removed ----------

(deftest removed-source-stays-removed-under-refresh
  (testing "a removed source is never re-instantiated by a refresh"
    (let [a (fake/make-fake-source :test/a "A")
          b (fake/make-fake-source :test/b "B")
          registry (reg/create-registry)
          _ (reg/register-source! registry a)
          _ (reg/register-source! registry b)
          _ (reg/refresh! registry)
          _ (reg/remove-source! registry :test/a)
          _ (fake/set-payload! b "B2")
          _ (reg/refresh! registry)]
      (is (reg/source-removed? registry :test/a) "A stays removed")
      (is (not (contains? (:sources @registry) :test/a)) "A not among live sources")
      (is (not (contains? (:logical-index (reg/catalog-projection registry)) :test/a))
          "A stays out of the catalog projection")
      (is (= (rev/payload->id "B2")
             (get-in (reg/catalog-projection registry) [:logical-index :test/b :revision/id]))
          "B refreshed independently"))))
