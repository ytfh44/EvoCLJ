(ns evoclj.eval.workers
  "Foundation F4 — isolated evaluation worker pool.

  run-batch! executes a SEQUENTIAL collection of task maps against an
  arbitrary `task-runner` function under bounded concurrency, with
  per-task timeout, early exit, per-task error isolation, and fully
  structured, EDN-safe results. One `run-batch!` call owns one bounded
  thread pool (java.util.concurrent.Executors/newFixedThreadPool) sized
  to the :concurrency option; the pool is ALWAYS shut down in a finally
  block so no thread leaks across calls. Threads are internal machinery;
  only DATA (task maps and result maps) ever crosses the public boundary
  (Global Constraint 22).

  This is the substrate for distributed evaluation, curriculum early
  exit, and worst-case isolation: it makes NO decisions about WHAT a
  task is — a `task-runner` may wrap run-side! (see side-task-runner
  below), a remote dispatch, or any pure function. Per-task isolation is
  structural: every task runs inside its own Future whose throwable is
  caught before it can pass, so one task's failure never escapes into
  another task's outcome or into the caller.

  Semantics:

  * Each input task is indexed by its ORIGINAL position as :task/index.
    Results are reported SORTED by :task/index, so the output is stable
    and deterministic regardless of scheduling order.
  * :batch/completed — tasks whose runner returned without throwing.
  * :batch/failed — tasks whose runner threw (the throwable is caught
    and sanitized into :error/type / :error/message / :error/data) or
    that exceeded :timeout-ms (reported as :eval/worker-timeout).
  * :batch/cancelled — tasks that were still QUEUED (not started) at the
    moment :early-exit became true. Already-running tasks are allowed to
    finish normally and report as completed/failed.
  * :batch/stats — {:total :ok :failed :cancelled :wall-ms}.

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/workers-invalid, thrown before any work is submitted, with
  :reason in [:tasks-not-sequential :task-not-a-map :task-missing-id
  :bad-concurrency :bad-timeout :runner-not-a-fn]."

  (:require [evoclj.eval.runner :as runner]
            [evoclj.eval.worker-transport :as wt]
            [evoclj.kernel.error :as err])
  (:import (java.util.concurrent Callable CancellationException Executors
                                  Future TimeUnit TimeoutException)
           [evoclj.eval.worker_transport LocalWorkerTransport]))

;; --- the task contract -------------------------------------------------------

(def TaskSchema
  "The normative task schema. Each task is a map whose only fixed keys are
  :task/id (any EDN-safe value) and :task/index (an integer). The map is
  OPEN — task-runner-specific payload keys (e.g. :side/kind, :genome/root,
  :seed) are permitted and passed through to the runner as-is. :task/index
  is supplied by run-batch!, so callers ordinarily provide only :task/id
  plus their payload keys (:task/index is accepted if present but is
  overwritten with the original position)."
  [:map {:closed false}
   [:task/id any?]
   [:task/index int?]])

;; --- argument validation -----------------------------------------------------

(defn- bad!
  "Throw :eval/workers-invalid carrying the given `reason` keyword."
  [reason]
  (throw (err/error :eval/workers-invalid
                    "invalid run-batch! invocation"
                    {:reason reason})))

(defn- validate-concurrency!
  "Validate :concurrency: a strictly positive integer (else
  :bad-concurrency)."
  [concurrency]
  (when-not (and (integer? concurrency) (pos? concurrency))
    (bad! :bad-concurrency))
  concurrency)

(defn- validate-timeout!
  "Validate the OPTIONAL :timeout-ms: when present it must be a strictly
  positive integer (a zero or negative cap could never be deref'd
  sanely). nil is accepted (no time limit). Returns the validated value."
  [timeout-ms]
  (when (and (some? timeout-ms)
             (not (and (integer? timeout-ms) (pos? timeout-ms))))
    (bad! :bad-timeout))
  timeout-ms)

(defn- validate-tasks!
  "Validate `tasks`: a sequential collection of maps, each carrying a
  :task/id. Returns the same tasks vectorized with :task/index force-set
  to each task's ORIGINAL position (a stable, caller-independent key).
  Rejects a non-sequential arg, a non-map task, or a task lacking
  :task/id — all reported as :eval/workers-invalid with the matching
  :reason."
  [tasks]
  (let [seqable? (and (sequential? tasks)
                      (not (map? tasks))
                      (not (set? tasks)))]
    (when-not seqable?
      (bad! :tasks-not-sequential))
    (let [tasks (vec tasks)]
      (doseq [t tasks]
        (when-not (map? t)
          (bad! :task-not-a-map))
        (when-not (contains? t :task/id)
          (bad! :task-missing-id)))
      (mapv (fn [idx t] (assoc t :task/index idx))
            (range) tasks))))

(defn- validate-runner!
  "Validate that `task-runner` is a fn (else :runner-not-a-fn)."
  [task-runner]
  (when-not (fn? task-runner)
    (bad! :runner-not-a-fn))
  task-runner)

(defn validate-tasks
  "Public wrapper around validate-tasks!. Returns the validated tasks
  vectorized with :task/index set to each task's original position.
  Throws :eval/workers-invalid on malformed input."
  [tasks]
  (validate-tasks! tasks))

;; --- per-task result construction ---------------------------------------------

(defn- task-index
  "The original :task/index of a task map."
  [task]
  (:task/index task))

(defn- task-id
  "The :task/id of a task map."
  [task]
  (:task/id task))

(defn- completed-entry
  "Build a :batch/completed entry from a task and its EDN-safe result."
  [task result]
  {:task/index (task-index task)
   :task/id (task-id task)
   :task/result result})

(defn- failure-entry
  "Build a :batch/failed entry from a task and the Throwable its runner
  threw. :error/type is taken from the throwable's ex-data ONLY when it
  is an ExceptionInfo carrying one; any other throwable fails closed as
  :eval/worker-task-failed (err/error-data's generic :error/unknown for
  untagged throwables is deliberately NOT trusted — the tagged type is a
  contract the runner opted into). The message and data are extracted
  through err/error-data (sanitized), so no throwable, class, lazy seq,
  or fn ever crosses the boundary (Global Constraint 22)."
  [task throwable]
  (let [ed (try
             (err/error-data throwable)
             (catch Throwable _ {:error/type :eval/worker-task-failed
                                 :error/message (str throwable)
                                 :error/data {}}))
        error-type (if (instance? clojure.lang.ExceptionInfo throwable)
                     (or (:error/type ed) :eval/worker-task-failed)
                     :eval/worker-task-failed)]
    {:task/index (task-index task)
     :task/id (task-id task)
     :error/type error-type
     :error/message (or (:error/message ed) (str throwable))
     :error/data (or (:error/data ed) {})}))

(defn- timeout-entry
  "Build a :batch/failed entry for a task that exceeded :timeout-ms."
  [task timeout-ms]
  {:task/index (task-index task)
   :task/id (task-id task)
   :error/type :eval/worker-timeout
   :error/message "task exceeded the per-task timeout"
   :error/data {:timeout-ms timeout-ms}})

(defn- cancelled-entry
  "Build a :batch/cancelled entry for a task that was still queued when
  :early-exit became true."
  [task]
  {:task/index (task-index task)
   :task/id (task-id task)
   :reason :early-exit})

;; --- future settlement ---------------------------------------------------------

(defn- run-task-future
  "The body of one task's Future: invoke the runner, catch ANY throwable
  so it is contained (never propagated to another task or the caller),
  and return a small tagged map. Only DATA crosses back into the batch
  loop; the throwable is held in the tagged map and converted to an
  EDN-safe failed entry there."
  [task-runner task]
  (fn []
    (try
      {:worker/status :done
       :task/result (task-runner task)}
      (catch Throwable t
        {:worker/status :error
         :throwable t}))))

(defn- deref-task-future
  "Deref one task's Future with the optional `timeout-ms` cap. Returns the
  tagged worker outcome map ({:worker/status :done :task/result ...} or
  {:worker/status :error :throwable ...}), or the keyword :timeout /
  :cancelled. No side effects on the accumulators."
  [fut timeout-ms]
  (try
    (if timeout-ms
      (.get ^Future fut (long timeout-ms) TimeUnit/MILLISECONDS)
      (.get ^Future fut))
    (catch CancellationException _ :cancelled)
    (catch TimeoutException _ :timeout)))

(defn- record-outcome!
  "Fold one deref outcome for `task` into the batch accumulators.
  Returns ::done for a genuine :done outcome (the caller may then
  re-check early-exit), nil otherwise."
  [task out timeout-ms completed failed cancelled]
  (cond
    (= out :cancelled)
    (do (swap! cancelled conj (cancelled-entry task)) nil)

    (= out :timeout)
    (do (swap! failed conj (timeout-entry task timeout-ms)) nil)

    (= (:worker/status out) :done)
    (do (swap! completed conj (completed-entry task (:task/result out)))
        ::done)

    :else
    (do (swap! failed conj (failure-entry task (:throwable out))) nil)))

;; --- the batch executor --------------------------------------------------------

(defn run-batch!
  "Execute a batch of tasks against `task-runner` with bounded concurrency.

  `tasks` is a sequential collection of task maps, each carrying a :task/id
  (EDN-safe). Each is indexed by its ORIGINAL position (:task/index) and
  results are reported sorted by :task/index. `task-runner` is
  (fn [task-map] -> EDN-safe result); it may throw — the throwable is caught
  per task and becomes a :batch/failed entry.

  `opts` keys:

      :concurrency  pos-int, default 1; the bounded pool size. Must be > 0
                    (:eval/workers-invalid :bad-concurrency).
      :timeout-ms   optional pos-int; a per-task wall-clock cap applied via
                    the task Future's deref. A task exceeding it is reported
                    as :eval/worker-timeout.
      :early-exit   optional (fn [completed-results-so-far] -> boolean),
                    checked after each task COMPLETES successfully with the
                    :task/result values of the completions so far (in
                    completion order). When it returns true, tasks still
                    QUEUED become :batch/cancelled with :reason :early-exit;
                    already-running tasks are allowed to finish normally and
                    report as completed/failed (never cancelled).

  Returns the batch result map:

      {:batch/completed [{:task/index int? :task/id any? :task/result any?} ...]
       :batch/failed    [{:task/index int? :task/id any? :error/type keyword?
                          :error/message string? :error/data map?} ...]
       :batch/cancelled [{:task/index int? :task/id any? :reason :early-exit} ...]
       :batch/stats     {:total int? :ok int? :failed int? :cancelled int?
                         :wall-ms int?}}

  Implementation notes: a fixed pool sized to :concurrency running a
  SLIDING WINDOW — at most :concurrency futures are in flight, tasks are
  submitted as slots free up (each task's exception contained inside its
  own Future, never another task's), and the lowest in-flight index is
  settled first (deterministic order). deref applies the :timeout-ms cap
  when present; early-exit cancels only not-yet-started futures
  (Future/cancel with mayInterruptIfRunning=false) and never submits the
  remaining tasks, so in-flight tasks finish and never-submitted tasks
  report :batch/cancelled; and the pool is ALWAYS shut down in a finally
  block."
  [task-runner tasks opts]
  (validate-runner! task-runner)
  (let [tasks (validate-tasks! tasks)
        concurrency (validate-concurrency! (or (:concurrency opts) 1))
        timeout-ms (validate-timeout! (:timeout-ms opts))
        early-exit (:early-exit opts)
        total (count tasks)
        started (System/nanoTime)
        completed (atom [])
        failed (atom [])
        cancelled (atom [])
        exit? (atom false)
        pool (Executors/newFixedThreadPool concurrency)]
    (try
      ;; SLIDING WINDOW: at most :concurrency futures are in flight at any
      ;; moment. Tasks are submitted only as slots free up, so an early
      ;; exit can leave never-submitted tasks untouched (they become
      ;; :batch/cancelled without ever running) — a batch with an
      ;; immediate runner and concurrency 1 completes a strict prefix, and
      ;; the pool worker can never race ahead of the settle loop. The
      ;; LOWEST in-flight index is settled first, so settlement order is
      ;; deterministic (sorted-by-index results).
      (loop [next 0, inflight {}]
        (cond
          ;; everything settled, nothing left to submit
          (and (empty? inflight) (>= next total))
          nil

          ;; early exit signalled: cancel not-yet-started in-flight
          ;; futures (Future/cancel with mayInterruptIfRunning=false —
          ;; running ones finish and report normally), record every
          ;; never-submitted task as cancelled, and stop.
          @exit?
          (do (doseq [[i f] (sort-by key inflight)]
                (if (.cancel ^Future f false)
                  (swap! cancelled conj (cancelled-entry (nth tasks i)))
                  (record-outcome! (nth tasks i)
                                   (deref-task-future f timeout-ms)
                                   timeout-ms completed failed cancelled)))
              (doseq [i (range next total)]
                (swap! cancelled conj (cancelled-entry (nth tasks i))))
              nil)

          :else
          (let [room (- concurrency (count inflight))
                will-submit (min room (- total next))
                indices (range next (+ next will-submit))
                inflight (reduce (fn [m i]
                                   ;; ^Callable is REQUIRED: a Clojure fn
                                   ;; implements BOTH Runnable and Callable,
                                   ;; and an untyped .submit picks the
                                   ;; Runnable overload — the task's return
                                   ;; value would be discarded and .get
                                   ;; would return nil, so every task would
                                   ;; report as failed.
                                   (assoc m i (.submit pool ^Callable
                                                       (run-task-future task-runner
                                                                        (nth tasks i)))))
                                 inflight indices)
                next (+ next will-submit)
                [idx fut] (first (sort-by key inflight))
                out (deref-task-future fut timeout-ms)
                inflight (dissoc inflight idx)
                task (nth tasks idx)]
            (when (and (record-outcome! task out timeout-ms
                                        completed failed cancelled)
                       (fn? early-exit)
                       (not @exit?)
                       (early-exit (mapv :task/result @completed)))
              (reset! exit? true))
            (recur next inflight))))
      (let [completed-sorted (sort-by :task/index @completed)
            failed-sorted (sort-by :task/index @failed)
            cancelled-sorted (sort-by :task/index @cancelled)]
        {:batch/completed completed-sorted
         :batch/failed failed-sorted
         :batch/cancelled cancelled-sorted
         :batch/stats {:total total
                       :ok (count @completed)
                       :failed (count @failed)
                       :cancelled (count @cancelled)
                       :wall-ms (int (/ (- (System/nanoTime) started)
                                        1000000))}})
      (finally
        (.shutdownNow pool)))))

;; --- distributed transport batch ------------------------------------------------

(defn run-batch-with-transport!
  "Execute a batch of tasks against a WorkerTransport.

  For LocalWorkerTransport, delegates to run-batch! using the transport's
  task-runner (preserving bounded concurrency, per-task timeout, early
  exit, and error isolation). For other transports, submits each task
  individually via submit-task and collects results into the standard
  batch result map."
  [transport tasks opts]
  (if (instance? LocalWorkerTransport transport)
    (run-batch! (:task-runner transport) tasks opts)
    (let [tasks (validate-tasks tasks)
          total (count tasks)
          completed (atom [])
          failed (atom [])]
      (doseq [task tasks]
        (let [result (wt/submit-task transport task)]
          (case (:status result)
            :completed (swap! completed conj {:task/index (:task/index task)
                                               :task/id (:task/id task)
                                               :task/result (:task/result result)})
            :failed (swap! failed conj {:task/index (:task/index task)
                                         :task/id (:task/id task)
                                         :error/type (:error/type result)
                                         :error/message (:error/message result)
                                         :error/data (or (:error/data result) {})})
            (swap! completed conj {:task/index (:task/index task)
                                   :task/id (:task/id task)
                                   :task/result result}))))
      {:batch/completed (sort-by :task/index @completed)
       :batch/failed (sort-by :task/index @failed)
       :batch/cancelled []
       :batch/stats {:total total
                      :ok (count @completed)
                      :failed (count @failed)
                      :cancelled 0
                      :wall-ms 0}})))

;; --- side-task-runner adapter ----------------------------------------------------

(defn side-task-runner
  "Adapt a G5 evaluator context into a task-runner fn for run-batch!.

  Returns a fn of one arg (a task map) that calls
  (run-side! evaluator {:genome/root ... :side/kind ... :side/id ...
  :generation/id ...} case-map seed) and returns the side result map as-is.
  The task payload keys are :side/kind, :side/id, :generation/id,
  :genome/root, :case-map, and :seed. run-side! ITSELF performs full
  isolation (fresh throwaway stores per side — Global Constraints
  11/12/23); the pool only adds bounded concurrency, per-task timeout,
  early exit, and error isolation on top.

  The returned fn is NOT invoked in unit tests: it drives the real
  scheduler against temp stores (real model work with nothing to dispose
  in a throwaway context). Tests assert only its presence, arity, and that
  it closes over the evaluator."
  [evaluator]
  (fn [task]
    (let [root (:genome/root task)
          kind (:side/kind task)
          side-id (:side/id task)
          generation-id (:generation/id task)]
      (runner/run-side!
       evaluator
       {:genome/root root
        :side/kind kind
        :side/id side-id
        :generation/id generation-id}
       (:case-map task)
       (:seed task)))))
