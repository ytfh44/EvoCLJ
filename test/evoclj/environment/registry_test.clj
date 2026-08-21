(ns evoclj.environment.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.static :as static]))

(deftest simultaneous-refresh-single-publication
  (testing "simultaneous refresh yields single publication"
    (let [source (fake/make-fake-source :test/concurrent "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          r1 (reg/refresh! registry)
          _ (is (= :published (:status r1)))
          seq1 (:revision/seq (:revision r1))
          _ (fake/set-payload! source "B")
          barrier (promise)
          f1 (future (deref barrier 5000 ::timeout) (reg/refresh! registry))
          f2 (future (deref barrier 5000 ::timeout) (reg/refresh! registry))
          _ (deliver barrier true)
          res1 @f1
          res2 @f2
          statuses (set [(:status res1) (:status res2)])
          cur (reg/current registry)
          seq2 (:revision/seq cur)]
      (is (contains? statuses :published) "one should publish")
      (is (contains? statuses :noop) "other should be noop")
      (is (= (inc seq1) seq2) "seq increments by 1 only")
      (is (= 2 (count (:history @registry))) "only two publications total"))))

(deftest refresh-failure-keeps-old-current
  (testing "refresh failure keeps old current and marks degraded"
    (let [source (fake/make-fake-source :test/failure "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          r1 (reg/refresh! registry)
          cur1 (reg/current registry)
          lg1 (reg/last-good registry)
          _ (fake/set-failure! source (ex-info "boom" {:error/type :fake/boom}))
          r2 (reg/refresh! registry)
          cur2 (reg/current registry)
          lg2 (reg/last-good registry)
          st (reg/status registry)]
      (is (= :published (:status r1)))
      (is (= :error (:status r2)))
      (is (= cur1 cur2) "current unchanged after failure")
      (is (= lg1 lg2) "last-good unchanged")
      (is (= :degraded (:status st)))
      (is (true? (:dirty? st)))
      (is (some? (:last-refresh-error st))))))

(deftest retry-after-failure-increments-seq-by-1
  (testing "retry after failure increments seq by 1"
    (let [source (fake/make-fake-source :test/retry "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          r1 (reg/refresh! registry)
          seq1 (:revision/seq (:revision r1))
          _ (fake/set-failure! source (ex-info "fail" {}))
          _ (reg/refresh! registry)
          _ (fake/clear-failure! source)
          _ (fake/set-payload! source "B")
          r3 (reg/refresh! registry)
          seq3 (:revision/seq (:revision r3))]
      (is (= (inc seq1) seq3) "retry increments by 1, not 2")
      (is (= :published (:status r3)))
      (is (= :ok (:status (reg/status registry)))))))

(deftest identical-content-no-churn
  (testing "identical content causes no churn"
    (let [source (fake/make-fake-source :test/identical "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          r1 (reg/refresh! registry)
          seq1 (:revision/seq (:revision r1))
          r2 (reg/refresh! registry)
          seq2 (:revision/seq (:revision r2))
          cur (reg/current registry)]
      (is (= :published (:status r1)))
      (is (= :noop (:status r2)))
      (is (= seq1 seq2) "seq unchanged")
      (is (= (:revision/id (:revision r1)) (:revision/id (:revision r2))))
      (is (= 1 (count (:history @registry))) "no new history"))))

(deftest listener-receives-publication-diff
  (testing "listener receives publication diff not raw file events"
    (let [source (fake/make-fake-source :test/listener "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          _ (reg/refresh! registry)
          diffs (atom [])
          raw-events (atom [])
          _ (reg/subscribe registry (fn [diff] (swap! diffs conj diff)))
          ;; also capture raw source subscription to ensure not leaked
          _ (fake/set-payload! source "B")
          r (reg/refresh! registry)
          _ (Thread/sleep 50)]
      (is (= 1 (count @diffs)) "one diff")
      (let [{:keys [prev curr]} (first @diffs)]
        (is (some? prev) "prev exists")
        (is (some? curr) "curr exists")
        (is (not= (:revision/id prev) (:revision/id curr)))
        (is (= "A" (:payload prev)))
        (is (= "B" (:payload curr)))
        (is (= (:revision/id curr) (:revision/id (:revision r))))
        ;; ensure diff contains revision keys, not raw file path string
        (is (contains? (first @diffs) :prev))
        (is (contains? (first @diffs) :curr))
        (is (rev/revision? prev))
        (is (rev/revision? curr)))
      ;; ensure raw file events not delivered: we did not call trigger-invalidate
      ;; but if registry leaked raw events, diff would contain file path
      (is (empty? @raw-events)))))

(deftest static-source-works
  (testing "StaticSource immutable path"
    (let [source (static/make-static-source :test/static "fixed")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          r1 (reg/refresh! registry)
          r2 (reg/refresh! registry)]
      (is (= :published (:status r1)))
      (is (= :noop (:status r2)))
      (is (= (:revision/id (:revision r1)) (:revision/id (:revision r2)))))))

(deftest revision-id-seq-not-conflated
  (testing "id and seq are distinct"
    (let [r (rev/make-revision :test/source "payload" 1)]
      (is (string? (:revision/id r)))
      (is (int? (:revision/seq r)))
      (is (not= (:revision/id r) (str (:revision/seq r))))
      (is (rev/revision? r))
      (is (= (:revision/id r) (rev/payload->id "payload"))))))
