(ns evoclj.context.registry-test
  (:require [clojure.test :as t]
            [evoclj.context.registry :as registry]))

;; ---------------------------------------------------------------------------
;; Test archiver
;; ---------------------------------------------------------------------------

(defrecord TestArchiver [id desc serialized]
  registry/CompacterArchive
  (archive-manifest [_]
    {:archiver/id id
     :archiver/description desc
     :archiver/serialized serialized}))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(t/deftest register-adds-archiver
  (registry/clear-registry!)
  (let [a (registry/register! (->TestArchiver :test/foo "desc" {:n 1}))]
    (t/is (registry/registered? a))))

(t/deftest unregister-removes-archiver
  (registry/clear-registry!)
  (let [a (registry/register! (->TestArchiver :test/foo "desc" {:n 1}))]
    (registry/unregister! a)
    (t/is (not (registry/registered? a)))))

(t/deftest archiver-reports-returns-manifests
  (registry/clear-registry!)
  (registry/register! (->TestArchiver :test/a "A" {:x 1}))
  (registry/register! (->TestArchiver :test/b "B" {:y 2}))
  (let [reports (registry/archiver-reports)]
    (t/is (= 2 (count reports)))
    (t/is (= :test/a (:archiver/id (first reports))))
    (t/is (= :test/b (:archiver/id (second reports))))))

(t/deftest clear-registry-empties-state
  (registry/clear-registry!)
  (registry/register! (->TestArchiver :test/x "X" {}))
  (registry/clear-registry!)
  (t/is (empty? (registry/archiver-reports))))

(t/deftest duplicate-registration-allowed
  (registry/clear-registry!)
  (let [a (->TestArchiver :test/d "D" {})]
    (registry/register! a)
    (registry/register! a)
    (t/is (= 2 (count (registry/archiver-reports))))))

(t/run-tests)
