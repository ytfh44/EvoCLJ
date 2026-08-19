(ns evoclj.eval.worker-transport-test
  "Tests for WorkerTransport protocol and implementations (S3-4)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.worker-transport :as wt]))

(defn- ok-runner
  [task]
  (inc (:task/index task)))

(defn- failing-runner
  [task]
  (if (= 2 (:task/index task))
    (throw (ex-info "boom" {:error/type :test/boom :detail (:task/index task)}))
    (inc (:task/index task))))

(deftest local-worker-transport-submits-and-completes
  (let [transport (evoclj.eval.worker-transport/local-worker-transport ok-runner)
        result (evoclj.eval.worker-transport/submit-task transport {:task/id :t0 :task/index 0})]
    (is (= :t0 (:task/id result)))
    (is (= :completed (:status result)))
    (is (= 1 (:task/result result)))
    (.shutdown (:executor transport))))

(deftest local-worker-transport-handles-failures
  (let [transport (wt/local-worker-transport failing-runner)
        result (wt/submit-task transport {:task/id :t2 :task/index 2})]
    (is (= :t2 (:task/id result)))
    (is (= :failed (:status result)))
    (is (= :test/boom (:error/type result)))
    (.shutdown (:executor transport))))

(deftest remote-worker-transport-returns-skipped-stub
  (let [transport (wt/->RemoteWorkerTransport "http://example.com" nil)
        result (wt/submit-task transport {:task/id :t0})]
    (is (= :t0 (:task/id result)))
    (is (= :skipped (:status result)))
    (is (= :not-implemented (:reason result)))))
