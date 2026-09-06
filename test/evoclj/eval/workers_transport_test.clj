(ns evoclj.eval.workers-transport-test
  "Tests for run-batch-with-transport! (S3-4)."
  (:require [clojure.test :refer [deftest is]]
            [evoclj.eval.worker-transport :as wt]
            [evoclj.eval.workers :as workers]))

(defn- tasks
  [n]
  (mapv (fn [i] {:task/id (keyword (str "t" i))}) (range n)))

(defn- ok-runner
  [task]
  {:task/result (inc (:task/index task))})

(deftest run-batch-with-transport-local-matches-run-batch
  (let [transport (wt/local-worker-transport ok-runner)
        tasks (tasks 5)
        result (workers/run-batch-with-transport! transport tasks {:concurrency 1})
        expected (workers/run-batch! ok-runner tasks {:concurrency 1})]
    (is (= (:batch/completed expected) (:batch/completed result)))
    (is (= (:batch/failed expected) (:batch/failed result)))
    (is (= (:batch/cancelled expected) (:batch/cancelled result)))
    ;; wall-ms varies by machine load; ignore it for equivalence
    (is (= (dissoc (:batch/stats expected) :wall-ms)
           (dissoc (:batch/stats result) :wall-ms)))
    (.shutdown (:executor transport))))

(deftest run-batch-with-transport-remote-files-failures-with-owner
  (let [transport (wt/->RemoteWorkerTransport "http://example.com" nil)
        result (workers/run-batch-with-transport! transport (tasks 3) {:concurrency 1})]
    (is (empty? (:batch/completed result)) "remote tasks must never complete silently")
    (is (= 3 (count (:batch/failed result))))
    (is (every? #(= :eval/remote-transport-unsupported (:error/type %)) (:batch/failed result)))
    (is (= 3 (get-in result [:batch/stats :failed])))))
