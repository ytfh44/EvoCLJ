(ns evoclj.environment.registry-e1-test
  "E1 — per-source registry: each source has its own current/last-good/seq,
   and the parameterless (no source-id) refresh! flushes ALL sources.

   Behavioral contract (not shape-only):
   - register-source! stores a per-source entry carrying :current, :last-good,
     and a monotonically increasing :seq.
   - refresh! with an explicit source-id updates ONLY that source's state.
   - refresh! with NO source-id (parameterless) re-syncs EVERY registered
     source: every source's current/last-good/seq advance in lock-step.
   - a source whose snapshot! throws keeps its previous last-good while the
     other sources still refresh (fail-closed per source).
   - concurrent parameterless refreshes must not corrupt any source's state
     nor drop or double any source's seq."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.static :as static]))

;; --- helpers ----------------------------------------------------------------

(defn- fresh-multi
  "Build a registry with n fake sources A/B/... identified by :test/a etc."
  [& payloads]
  (let [registry (reg/create-registry)
        ids (mapv (fn [i] (keyword "test" (str (char (+ (int \a) i)))))
                  (range (count payloads)))
        sources (mapv (fn [id p] (fake/make-fake-source id p)) ids payloads)]
    (doseq [s sources] (reg/register-source! registry s))
    {:registry registry :ids ids :sources sources}))

(defn- per-source-state
  [registry sid]
  (get-in @registry [:per-source sid]))

;; --- RED: per-source state is present after registration + refresh -----------

(deftest per-source-state-present-after-refresh
  (testing "each source carries its own current/last-good/seq"
    (let [{:keys [registry ids]} (fresh-multi "A" "B" "C")]
      ;; parameterless refresh flushes all three
      (let [res (reg/refresh! registry)]
        (is (= :published-all (:status res)) "parameterless refresh reports all published")
        (is (= 3 (count (:per-source res))) "result carries per-source entries"))
      (doseq [sid ids]
        (let [entry (per-source-state registry sid)]
          (is (some? (:current entry)) "source has current")
          (is (some? (:last-good entry)) "source has last-good")
          (is (int? (:seq entry)) "source has seq")
          (is (= (:current entry) (:last-good entry)) "current == last-good after clean publish")
          (is (= 1 (:seq entry)) "first publish seq is 1"))))))

;; --- RED: parameterless refresh flushes all sources (happy path) -------------

(deftest parameterless-refresh-flushes-all-sources
  (testing "parameterless refresh updates current/last-good/seq for every source"
    (let [{:keys [registry ids sources]} (fresh-multi "A" "B")]
      (reg/refresh! registry)
      (let [seqs-before (mapv #(:seq (per-source-state registry %)) ids)]
        ;; mutate every source's payload
        (doseq [[s p] (map vector sources ["A2" "B2"])]
          (fake/set-payload! s p))
        (let [res (reg/refresh! registry)
              seqs-after (mapv #(:seq (per-source-state registry %)) ids)
              cur-ids-after (mapv #(:revision/id (:current (per-source-state registry %))) ids)]
          (is (= :published-all (:status res)))
          ;; every source advanced exactly one seq
          (is (= seqs-after (mapv inc seqs-before)) "all seqs advanced by 1")
          ;; every source's current reflects the new payload
          (is (= (rev/payload->id "A2") (nth cur-ids-after 0)))
          (is (= (rev/payload->id "B2") (nth cur-ids-after 1)))
          ;; every source's current == last-good (clean publish)
          (doseq [sid ids]
            (let [entry (per-source-state registry sid)]
              (is (= (:current entry) (:last-good entry))))))))))

;; --- RED: explicit single-source refresh touches only that source -----------

(deftest explicit-single-refresh-isolated
  (testing "refresh! with a source-id updates only that source"
    (let [{:keys [registry ids sources]} (fresh-multi "A" "B")]
      (reg/refresh! registry)
      (fake/set-payload! (first sources) "A2")
      (fake/set-payload! (second sources) "B2")
      (let [res (reg/refresh! registry :test/a)
            a (per-source-state registry :test/a)
            b (per-source-state registry :test/b)]
        (is (= :published (:status res)))
        (is (= 2 (:seq a)) "source A advanced")
        (is (= (rev/payload->id "A2") (:revision/id (:current a))))
        (is (= 1 (:seq b)) "source B untouched")
        (is (= (rev/payload->id "B") (:revision/id (:current b))))))))

;; --- RED: seq advances (new branch) ------------------------------------------

(deftest seq-advances-per-source-monotonically
  (testing "per-source seq is monotonic and independent per source"
    (let [{:keys [registry ids sources]} (fresh-multi "A" "B")]
      (reg/refresh! registry)
      (fake/set-payload! (first sources) "A2")
      (reg/refresh! registry :test/a)
      (fake/set-payload! (second sources) "B2")
      (reg/refresh! registry :test/b)
      (fake/set-payload! (first sources) "A3")
      (reg/refresh! registry :test/a)
      (let [a (per-source-state registry :test/a)
            b (per-source-state registry :test/b)]
        (is (= 3 (:seq a)) "A advanced 3 times")
        (is (= 2 (:seq b)) "B advanced 2 times")
        (is (> (:seq a) (:seq b)) "seq independent per source")))))

;; --- RED: fault case — one source errors, others still refresh --------------

(deftest fault-source-keeps-last-good-others-refresh
  (testing "a source whose snapshot! throws keeps last-good; siblings still refresh"
    (let [{:keys [registry ids sources]} (fresh-multi "A" "B")]
      (reg/refresh! registry)
      ;; now A's payload changes, B is made to fail
      (fake/set-payload! (first sources) "A2")
      (fake/set-failure! (second sources) (ex-info "boom" {:error/type :fake/boom}))
      (let [res (reg/refresh! registry)
            a (per-source-state registry :test/a)
            b (per-source-state registry :test/b)]
        (is (= :partial (:status res)) "parameterless refresh reports partial failure")
        (is (contains? (set (keys (:per-source res))) :test/a))
        (is (contains? (set (keys (:per-source res))) :test/b))
        ;; A published, advanced
        (is (= 2 (:seq a)))
        (is (= (rev/payload->id "A2") (:revision/id (:current a))))
        ;; B kept its last-good from the previous clean publish, did NOT advance
        (is (= 1 (:seq b)) "failed source keeps prior seq")
        (is (= (rev/payload->id "B") (:revision/id (:last-good b))) "last-good preserved")
        (is (= (rev/payload->id "B") (:revision/id (:current b))) "current preserved on failure")
        ;; registry-level status reflects degradation but the healthy source still refreshed
        (is (= :degraded (:status (reg/status registry))))))))

;; --- RED: concurrent parameterless refresh safe ------------------------------

(deftest concurrent-parameterless-refresh-safe
  (testing "concurrent parameterless refreshes keep each source's state consistent"
    (let [{:keys [registry ids sources]} (fresh-multi "A" "B" "C")]
      (reg/refresh! registry)
      ;; Part 1: concurrent no-op refreshes must not corrupt or double-count.
      (let [noop-futures (doall (for [i (range 20)] (future (reg/refresh! registry))))]
        (doseq [f noop-futures] (deref f))
        (doseq [sid ids]
          (let [e (per-source-state registry sid)]
            (is (= 1 (:seq e)))
            (is (= 1 (count (:history e))))
            (is (rev/revision? (:current e))))))
      ;; Part 2: changing payloads concurrently must stay fail-closed/monotonic.
      (let [reachable (set (map #(rev/payload->id (str "v" %)) (range 10)))
            change-futures (doall (for [round (range 10)]
                                   (future
                                     (doseq [s sources] (fake/set-payload! s (str "v" round)))
                                     (reg/refresh! registry))))]
        (doseq [f change-futures] (deref f))
        (doseq [sid ids]
          (let [e (per-source-state registry sid)]
            (is (int? (:seq e)))
            (is (<= 1 (:seq e) 11))
            (is (= (count (:history e)) (:seq e)))
            (is (rev/revision? (:current e)))
            (is (rev/revision? (:last-good e)))
            (is (contains? reachable (:revision/id (:current e))))))))))

;; --- RED: regression — old single-set registry behavior is gone --------------

(deftest regression-single-set-current-removed
  (testing "top-level :current is no longer the only current; per-source is authoritative"
    (let [{:keys [registry ids sources]} (fresh-multi "A" "B")]
      ;; with two sources, a single-source refresh of A must NOT make the
      ;; top-level current equal B's payload (never refreshed), proving current
      ;; is no longer a single global slot for sources.
      (reg/refresh! registry :test/a)
      (let [top-cur (reg/current registry)]
        (is (not= (rev/payload->id "B") (:revision/id top-cur))
            "top-level current must not be the un-refreshed source B")))))

;; --- RED: doc/behavior consistency — idempotent identical content -----------

(deftest doc-per-source-identical-content-noop
  (testing "refreshing an unchanged source is a per-source noop (seq unchanged)"
    (let [{:keys [registry ids]} (fresh-multi "A" "B")]
      (reg/refresh! registry)
      (let [seqs-before (mapv #(:seq (per-source-state registry %)) ids)
            res (reg/refresh! registry)
            seqs-after (mapv #(:seq (per-source-state registry %)) ids)]
        (is (= :noop-all (:status res)) "nothing changed -> noop-all")
        (is (= seqs-before seqs-after) "no seq churn on identical content")))))
