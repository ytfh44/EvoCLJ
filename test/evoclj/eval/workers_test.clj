(ns evoclj.eval.workers-test
  "Foundation F4 — isolated evaluation worker pool (batch executor).

  run-batch! executes a SEQUENTIAL collection of task maps against an
  arbitrary task-runner under bounded concurrency, with per-task timeout,
  early exit, per-task error isolation, and structured EDN-safe results.
  These tests exercise the executor EXCLUSIVELY with fast FAKE task
  runners (immediate returns, deliberate throws, sleeps) — never with the
  real scheduler/run-side! — so the pool semantics are validated cheaply
  and deterministically. side-task-runner is tested only for presence,
  arity, and closure, never invoked (see the namespace docstring for why:
  it drives real scheduler work against temp stores with nothing to
  dispose in a test context, which would be slow and side-effecting)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.workers :as workers]))

;; --- helpers -----------------------------------------------------------------

(defn- tasks
  "`n` fake tasks with ids :t0 ... :t<n-1> and no payload keys."
  [n]
  (mapv (fn [i] {:task/id (keyword (str "t" i))})
        (range n)))

(defn- ok-runner
  "A task-runner returning (inc index) for every task."
  [task]
  (inc (:task/index task)))

(defn- failing-runner
  "A task-runner that throws :test/boom for :task/index 2 and returns a
  result for every other task. The throwable is an ExceptionInfo carrying
  :error/type :test/boom to assert the contract type is preserved."
  [task]
  (if (= (:task/index task) 2)
    (throw (ex-info "boom" {:error/type :test/boom :detail (:task/index task)}))
    {:task/result (inc (:task/index task))}))

(defn- sleep-runner
  "A task-runner that sleeps `ms` then returns a result."
  [ms]
  (fn [_task]
    (Thread/sleep ms)
    {:task/result :done}))

(defn- error-data-shot
  "Grab the :error/type of the FIRST :eval/workers-invalid thrown by a
  zero-arg thunk, or fail the test if none is thrown."
  [thunk]
  (try
    (thunk)
    (is false "expected :eval/workers-invalid to be thrown")
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

;; --- all-success ----------------------------------------------------------------

(deftest all-success-reports-completed-sorted
  (let [result (workers/run-batch! ok-runner (tasks 5) {:concurrency 1})]
    (testing "every task completes"
      (is (= 5 (:total (:batch/stats result))))
      (is (= 5 (:ok (:batch/stats result))))
      (is (= 0 (:failed (:batch/stats result))))
      (is (= 0 (:cancelled (:batch/stats result))))
      (is (= [] (:batch/failed result)))
      (is (= [] (:batch/cancelled result))))
    (testing "results are sorted by :task/index"
      (is (= [0 1 2 3 4] (mapv :task/index (:batch/completed result))))
      (is (= [:t0 :t1 :t2 :t3 :t4]
             (mapv :task/id (:batch/completed result))))
      (is (= [1 2 3 4 5] (mapv :task/result (:batch/completed result)))))
    (testing "wall time is non-negative"
      (is (and (int? (:wall-ms (:batch/stats result)))
               (not (neg? (:wall-ms (:batch/stats result)))))))))

;; --- one-failing ---------------------------------------------------------------

(deftest one-failing-is-isolated
  (let [result (workers/run-batch! failing-runner (tasks 5) {:concurrency 1})]
    (testing "the throwing task lands in :batch/failed with its type preserved"
      (is (= 1 (:failed (:batch/stats result))))
      (is (= 1 (count (:batch/failed result))))
      (let [f (first (:batch/failed result))]
        (is (= 2 (:task/index f)))
        (is (= :t2 (:task/id f)))
        (is (= :test/boom (:error/type f)))
        (is (= "boom" (:error/message f)))
        (is (= {:detail 2} (:error/data f)))))
    (testing "other tasks still complete; none are cancelled"
      (is (= 4 (:ok (:batch/stats result))))
      (is (= 0 (:cancelled (:batch/stats result))))
      (is (= 4 (count (:batch/completed result))))
      (is (= [0 1 3 4] (mapv :task/index (:batch/completed result)))))
    (testing "total reconciles"
      (let [{:keys [total ok failed cancelled]} (:batch/stats result)]
        (is (= total (+ ok failed cancelled)))))))

;; --- unknown-throw-style ---------------------------------------------------------

(deftest non-tagged-throw-fails-closed
  (let [runner (fn [task]
                 (if (= 1 (:task/index task))
                   (throw (RuntimeException. "untagged"))
                   {:task/result :ok}))]
    (let [result (workers/run-batch! runner (tasks 2) {:concurrency 1})]
      (is (= 1 (count (:batch/failed result))))
      (is (= :eval/worker-task-failed (:error/type (first (:batch/failed result)))))
      (is (= 1 (:ok (:batch/stats result)))))))

;; --- timeout ---------------------------------------------------------------

(deftest timeout-is-isolated-and-bounded
  (let [slow (sleep-runner 200)
        start (System/nanoTime)
        result (workers/run-batch! slow (tasks 2)
                                   {:concurrency 1 :timeout-ms 50})
        wall-ms (/ (- (System/nanoTime) start) 1000000)]
    (testing "each task exceeding the timeout reports :eval/worker-timeout"
      (is (= 2 (count (:batch/failed result))))
      (is (every? #(= :eval/worker-timeout (:error/type %))
                  (:batch/failed result)))
      (is (= [0 1] (mapv :task/index (:batch/failed result)))))
    (testing "no tasks complete or cancel"
      (is (= 0 (:ok (:batch/stats result))))
      (is (= 0 (:cancelled (:batch/stats result)))))
    (testing "the batch returns well under the serial sum of the sleeps"
      (is (< wall-ms 400)
          (str "batch took " wall-ms "ms; expected under 400ms"))
      (is (<= 50 wall-ms)
          (str "timeout floor respected: " wall-ms "ms")))))

;; --- early-exit -------------------------------------------------------------

(deftest early-exit-cancels-queued-tasks
  ;; 10 immediate tasks, concurrency 1 so they run strictly one at a time
  ;; in index order; early-exit fires after the 3rd completion.
  (let [completed-count (volatile! 0)
        result (workers/run-batch!
                (fn [_task]
                  {:task/result (vswap! completed-count inc)})
                (tasks 10)
                {:concurrency 1
                 :early-exit (fn [completed]
                               (>= (count completed) 3))})
        indices (mapv :task/index (:batch/completed result))]
    (testing "exactly the first 3 complete (a strict prefix with concurrency 1)"
      (is (= [0 1 2] indices))
      (is (= 3 (:ok (:batch/stats result)))))
    (testing "the remaining 7 are cancelled with :reason :early-exit"
      (is (= 7 (:cancelled (:batch/stats result))))
      (is (= [3 4 5 6 7 8 9]
             (mapv :task/index (:batch/cancelled result))))
      (is (every? #(= :early-exit (:reason %)) (:batch/cancelled result))))
    (testing "nothing failed and totals reconcile"
      (is (= 0 (:failed (:batch/stats result))))
      (is (= 10 (:total (:batch/stats result)))))))

(deftest early-exit-with-concurrency-keeps-running-tasks
  ;; concurrency 2: after the 3rd completion the 4th is already running and
  ;; finishes normally; only strictly-queued tasks are cancelled.
  (let [result (workers/run-batch!
                (fn [_task]
                  {:task/result :done})
                (tasks 10)
                {:concurrency 2
                 :early-exit (fn [completed]
                               (>= (count completed) 3))})
        completed-count (count (:batch/completed result))
        cancelled-count (count (:batch/cancelled result))]
    ;; at least 3 and at most 3 + (concurrency - 1) may complete
    (is (<= 3 completed-count))
    (is (<= completed-count 4)
        (str "completed=" completed-count))
    (is (= 10 (+ completed-count (count (:batch/failed result))
                 cancelled-count)))
    (testing "no duplicates across buckets by :task/index"
      (let [all (concat (map :task/index (:batch/completed result))
                        (map :task/index (:batch/cancelled result))
                        (map :task/index (:batch/failed result)))]
        (is (= 10 (count (set all))))
        (is (= (set (range 10)) (set all)))))
    (is (= :early-exit (:reason (first (:batch/cancelled result)))))))

;; --- concurrency bound -----------------------------------------------------------

(deftest concurrency-is-bounded
  ;; 6 tasks each sleeping 50ms, concurrency 2. A runner records the live
  ;; concurrent count; the max must never exceed 2, and wall time should
  ;; reflect real parallelism (far less than 6*50ms serial).
  (let [live (atom 0)
        max-live (atom 0)
        runner (fn [_task]
                 (let [n (swap! live inc)]
                   (swap! max-live max n)
                   (try
                     (Thread/sleep 50)
                     {:task/result :done}
                     (finally
                       (swap! live dec)))))
        start (System/nanoTime)
        result (workers/run-batch! runner (tasks 6) {:concurrency 2})
        wall-ms (/ (- (System/nanoTime) start) 1000000)]
    (testing "never more than the configured concurrency run at once"
      (is (= 2 @max-live) (str "max concurrent = " @max-live)))
    (testing "all tasks complete"
      (is (= 6 (:ok (:batch/stats result))))
      (is (= [0 1 2 3 4 5] (mapv :task/index (:batch/completed result)))))
    (testing "parallelism is real: far under the 300ms serial sum"
      (is (< wall-ms 250) (str "wall=" wall-ms "ms")))))

;; --- invalid arguments ------------------------------------------------------------

(deftest invalid-args-are-typed
  (testing "non-sequential tasks"
    (is (= :eval/workers-invalid
           (error-data-shot #(workers/run-batch! ok-runner {:task/id :x} {})))))
  (testing "task without :task/id"
    (is (= :eval/workers-invalid
           (error-data-shot #(workers/run-batch! ok-runner [{}] {})))))
  (testing "concurrency 0"
    (is (= :eval/workers-invalid
           (error-data-shot #(workers/run-batch! ok-runner (tasks 1)
                                                {:concurrency 0})))))
  (testing "concurrency negative"
    (is (= :eval/workers-invalid
           (error-data-shot #(workers/run-batch! ok-runner (tasks 1)
                                                {:concurrency -2})))))
  (testing "task-runner is not a fn"
    (is (= :eval/workers-invalid
           (error-data-shot #(workers/run-batch! :not-a-fn (tasks 1) {})))))
  (testing "the :reason is carried on the error data"
    (try
      (workers/run-batch! ok-runner (tasks 1) {:concurrency 0})
      (catch clojure.lang.ExceptionInfo e
        (is (= :bad-concurrency (:reason (ex-data e))))))))

;; --- side-task-runner ------------------------------------------------------------

(deftest side-task-runner-is-a-closing-fn
  ;; The returned fn is NOT pointed at the real scheduler here: invoked
  ;; with a task lacking :genome/root it fails closed with
  ;; :eval/paired-genome-unresolved BEFORE any scheduler/temp-store work
  ;; (run-side! validates the root first), which is a safe, side-effect-free
  ;; proof of arity 1 and of the wiring into run-side!. Full execution is
  ;; exercised only by the real evaluation harness.
  (let [f (workers/side-task-runner {:marker :closed-over})]
    (testing "it returns a fn of one arg that fails closed before scheduler work"
      (is (fn? f))
      (try
        (f {:task/id :x})
        (is false "expected :eval/paired-genome-unresolved")
        (catch clojure.lang.ExceptionInfo e
          (is (= :eval/paired-genome-unresolved (:error/type (ex-data e))))))))
  (testing "each call builds a fresh closure (capturing the evaluator)"
    (is (not (identical? (workers/side-task-runner {:a 1})
                         (workers/side-task-runner {:a 2}))))))
