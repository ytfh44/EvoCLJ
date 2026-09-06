(ns evoclj.eval.worker-transport
  "Distributed worker transport foundation (S3-4).

  WorkerTransport is the protocol abstraction for submitting evaluation
  tasks to different execution backends. LocalWorkerTransport wraps the
  existing local run-batch! semantics; RemoteWorkerTransport is an
  explicitly unsupported backend: submit-task returns :failed with
  :eval/remote-transport-unsupported and its owner, so
  run-batch-with-transport! files remote tasks under :batch/failed with
  attribution instead of silently completing them as :skipped.")

(defprotocol WorkerTransport
  (submit-task [transport task]
    "Submit `task` to `transport`. Returns a future-like result map:

     {:task/id    ...                            ; the task's :task/id
      :status     :completed|:failed|:skipped|:remote ...
      :task/result ...}          ; present when :completed

  For failures the map carries the typed error:

     {:error/type    keyword?
      :error/message string?
      :error/data    map?}"))

(defrecord LocalWorkerTransport [task-runner ^java.util.concurrent.ExecutorService executor]
  WorkerTransport
  (submit-task [this task]
    (let [fut (.submit ^java.util.concurrent.ExecutorService executor
                       ^java.util.concurrent.Callable
                       (fn []
                         (try
                           {:worker/status :done
                            :task/result (task-runner task)}
                           (catch Throwable t
                             {:worker/status :error
                              :throwable t}))))]
      (try
        (let [out (.get ^java.util.concurrent.Future fut)]
          (if (= (:worker/status out) :done)
            {:task/id (:task/id task)
             :status :completed
             :task/result (:task/result out)}
            {:task/id (:task/id task)
             :status :failed
             :error/type (or (-> out :throwable ex-data :error/type)
                             :eval/worker-task-failed)
             :error/message (str (:throwable out))
             :error/data (or (-> out :throwable ex-data) {})}))
        (catch java.util.concurrent.ExecutionException e
          {:task/id (:task/id task)
           :status :failed
           :error/type (or (-> e .getCause ex-data :error/type)
                           :eval/worker-task-failed)
           :error/message (str (.getCause e))
           :error/data (or (-> e .getCause ex-data) {})})
        (catch Throwable t
          {:task/id (:task/id task)
           :status :failed
           :error/type :eval/worker-task-failed
           :error/message (str t)})))))

(defn local-worker-transport
  "Build a LocalWorkerTransport running tasks through `task-runner` on a
  single-threaded executor."
  [task-runner]
  (->LocalWorkerTransport task-runner
                          (java.util.concurrent.Executors/newSingleThreadExecutor)))

(def ^:const remote-transport-owner
  "Owner of the remote worker transport backend (triage target for S3 follow-ups)."
  :eval/workers)

(defrecord RemoteWorkerTransport [endpoint client]
  WorkerTransport
  (submit-task [_ task]
    {:task/id (:task/id task)
     :status :failed
     :error/type :eval/remote-transport-unsupported
     :error/message (str "remote worker transport is not implemented (endpoint " endpoint
                         "); use LocalWorkerTransport or own this backend (" remote-transport-owner ")")
     :error/data {:owner remote-transport-owner
                  :endpoint endpoint}}))
