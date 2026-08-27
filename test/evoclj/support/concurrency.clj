(ns evoclj.support.concurrency
  "WO-T4 — deterministic concurrency test primitives. Lives on the TEST
  classpath by design: it exists so suites (M1/M3/E2/B1 and friends)
  stop hand-rolling latches and start sharing ONE disciplined kit.

  THE FAILURE DISCIPLINE (确定性失败优先): a primitive that cannot
  deliver its normative outcome FAILS LOUDLY AND BOUNDED — timeouts are
  marked or thrown WITH diagnostics, never silently swallowed, never
  allowed to hang the suite, and never convertible into an accidental
  pass. Worker threads are daemons and interrupted best-effort on
  timeout, so even an unkillable thunk cannot keep the JVM alive.

  Primitives:
    raced                      — concurrent thunks, CountDownLatch start
                                 gate, per-thread result maps
    barrier-call               — k participants rendezvous at a
                                 CyclicBarrier before any of them runs
    eventually                 — polling assertion with diagnostic throw
    with-thread-dump-on-timeout — debug wrapper: dump stacks, then fail
    thread-dump!               — print every live thread's stack to *err*"
  (:import (java.util.concurrent CountDownLatch CyclicBarrier TimeUnit)))

(def default-timeout-ms
  "Default overall budget in milliseconds (10 s) used by `raced`,
  `barrier-call` and `eventually` when no :timeout-ms is supplied."
  10000)

(def default-interval-ms
  "Default polling interval in milliseconds (50 ms) used by
  `eventually` when no :interval-ms is supplied."
  50)

;; ---------------------------------------------------------------------------
;; shared internals
;; ---------------------------------------------------------------------------

(defn- now-ms []
  (quot (System/nanoTime) 1000000))

(defn- check-timeout-ms!
  "Validate a millisecond budget; return it as a long. A malformed
  budget is a caller bug — fail deterministically BEFORE doing work."
  [label timeout-ms]
  (when-not (and (number? timeout-ms) (pos? timeout-ms))
    (throw (ex-info (str label ": timeout-ms must be a positive number of
milliseconds, got " (pr-str timeout-ms))
                    {:error/type :concurrency/invalid-arguments
                     :argument :timeout-ms
                     :value timeout-ms})))
  (long timeout-ms))

(defn- checked-thunks!
  "Validate that `fns` is a finite collection of function values; return
  it as a vector. Malformed entries are caller bugs — fail up front,
  per entry, instead of an ArityException deep inside some thread."
  [label fns]
  (let [thunks (vec fns)]
    (doseq [[i f] (map-indexed vector thunks)]
      (when-not (fn? f)
        (throw (ex-info (str label ": entry " i " is not a function: "
                             (pr-str f))
                        {:error/type :concurrency/invalid-arguments
                         :index i
                         :entry f}))))
    thunks))

(defn- deliver-outcome!
  "Run `f`, delivering exactly one outcome map to promise `p`:
  {:status :result :value v} or {:status :thrown :value throwable}.
  ANY Throwable is captured — Errors included — so worker failures stay
  DATA for the caller to assert on instead of crashing the suite."
  [p f]
  (try
    (deliver p {:status :result :value (f)})
    (catch Throwable t
      (deliver p {:status :thrown :value t}))))

(defn- collect!
  "Join every started worker against ONE overall wall-clock deadline
  (`timeout-ms` measured from here); return the aligned result vector.
  Workers still alive at the deadline are interrupted (best effort) and
  reported {:thread ... :status :timeout :value nil}; they are daemon
  threads, so an unresponsive one can delay nothing but itself.
  Thread.join establishes the happens-before edge guaranteeing the
  delivered outcome is visible once the thread is not alive."
  [workers ^long timeout-ms]
  (let [deadline (+ (now-ms) timeout-ms)]
    (mapv (fn [{:keys [thread outcome]}]
            (let [^Thread thread thread
                  remaining (max 0 (- deadline (now-ms)))]
              ;; Thread/join(0) means FOREVER — never call it with 0
              (when (pos? remaining)
                (.join thread remaining))
              (if (.isAlive thread)
                (do (.interrupt thread)
                    {:thread (.getName thread)
                     :status :timeout
                     :value nil})
                (let [{:keys [status value]} @outcome]
                  {:thread (.getName thread)
                   :status status
                   :value value}))))
          workers)))

;; ---------------------------------------------------------------------------
;; raced
;; ---------------------------------------------------------------------------

(defn raced
  "Run every zero-argument thunk in `fns` concurrently, all released
  together through a java.util.concurrent.CountDownLatch start gate —
  bodies begin at the common signal, not whenever their threads happen
  to get scheduled.

  Returns one map PER INPUT POSITION (aligned by thread/index, NOT by
  completion order):

    {:thread \"evoclj-raced-i\"              ;; worker thread name
     :status :result | :thrown | :timeout
     :value   return-value | Throwable | nil}

  * :result  — the thunk returned; :value is its return value.
  * :thrown  — the thunk threw ANY Throwable (Errors included); :value
               is the ORIGINAL throwable, unwrapped. raced NEVER
               rethrows: failures are data the caller asserts on.
  * :timeout — the thunk did not finish inside the budget; the worker
               was interrupted best-effort and abandoned (daemon), so
               the suite can never hang on it. :value is nil.

  Opts: :timeout-ms — overall wall-clock budget from the common
  release; every thread gets whatever remains of that one budget.
  Defaults to default-timeout-ms (10 s).

  Empty `fns` yields []. Bodies run on fresh daemon threads WITHOUT the
  caller's thread-local bindings (dynamic vars, *out*, ...)."
  [fns & {:keys [timeout-ms]}]
  (let [thunks (checked-thunks! "raced" fns)
        n (count thunks)
        ^long budget (cond (nil? timeout-ms) default-timeout-ms
                           :else (check-timeout-ms! "raced" timeout-ms))
        ^CountDownLatch gate (CountDownLatch. 1)
        workers (mapv (fn [i]
                        (let [f (nth thunks i)
                              p (promise)
                              body (fn []
                                     (if (.await gate budget TimeUnit/MILLISECONDS)
                                       (deliver-outcome! p f)
                                       (deliver p {:status :timeout :value nil})))
                              t (Thread. ^Runnable body (str "evoclj-raced-" i))]
                          (.setDaemon t true)
                          (.start t)
                          {:thread t :outcome p}))
                      (range n))]
    (.countDown gate)
    (collect! workers budget)))

;; ---------------------------------------------------------------------------
;; barrier-call
;; ---------------------------------------------------------------------------

(defn barrier-call
  "Run `k` participant thunks (`k` = (count thunks) ≥ 1), each on its
  own daemon thread, each waiting at a java.util.concurrent.CyclicBarrier
  so that NO participant proceeds past the rendezvous point N (the
  instant just before its own thunk runs) until ALL k have arrived —
  then everyone is released together.

  Where `raced` synchronizes the START signal only, `barrier-call`
  additionally proves full ARRIVAL: the release cannot happen early or
  partially because the barrier itself is the gate. Use it to bootstrap
  genuinely simultaneous interleavings (e.g. N threads racing on one
  atom); `(repeat k f)` gives k identical participants.

  Returns exactly the same aligned per-thread maps as `raced`:
    {:thread \"evoclj-barrier-i\" :status :result|:thrown|:timeout :value ...}

  A participant whose barrier await times out — or that sees the
  barrier broken by a sibling's timeout — reports :thrown carrying the
  TimeoutException/BrokenBarrierException: a rendezvous that never
  happened is a FAILURE, never a silently sequential run.

  Opts: :timeout-ms — one shared wall-clock budget covering BOTH the
  rendezvous and the join (same semantics as in raced). Defaults to
  default-timeout-ms (10 s). Thunks must be a finite collection of
  functions; bodies run without the caller's thread-locals."
  [thunks & {:keys [timeout-ms]}]
  (let [thunks (checked-thunks! "barrier-call" thunks)
        k (count thunks)
        _ (when (zero? k)
            (throw (ex-info "barrier-call: needs at least one participant"
                            {:error/type :concurrency/invalid-arguments
                             :participants 0})))
        ^long budget (cond (nil? timeout-ms) default-timeout-ms
                           :else (check-timeout-ms! "barrier-call" timeout-ms))
        ^CyclicBarrier barrier (CyclicBarrier. k)
        workers (mapv (fn [i]
                        (let [f (nth thunks i)
                              p (promise)
                              body (fn []
                                     (try
                                       ;; arrival index discarded; throws
                                       ;; Timeout/BrokenBarrier/Interrupted
                                       (.await barrier budget TimeUnit/MILLISECONDS)
                                       (deliver-outcome! p f)
                                       (catch Throwable t
                                         (deliver p {:status :thrown :value t}))))
                              t (Thread. ^Runnable body (str "evoclj-barrier-" i))]
                          (.setDaemon t true)
                          (.start t)
                          {:thread t :outcome p}))
                      (range k))]
    (collect! workers budget)))

;; ---------------------------------------------------------------------------
;; eventually
;; ---------------------------------------------------------------------------

(defn eventually
  "Poll `(pred)` until it returns a truthy value and RETURN that value;
  anything else fails deterministically.

  Polling: first probe immediately, then sleep :interval-ms (default
  default-interval-ms = 50 ms) between probes. If pred has not turned
  truthy by the :timeout-ms budget (default default-timeout-ms = 10 s),
  THROW ex-info carrying full diagnostics:

    {:error/type  :eventually/timeout
     :timeout-ms  ...  :interval-ms  ...
     :attempts    n                  ;; probes made (>= 1)
     :elapsed-ms  ...
     :last-value  <final falsey answer>}

  CONTRACT — READ BEFORE RELYING ON THE TIMEOUT:
  pred must not block indefinitely.
  Probes run synchronously on the caller's thread and the
  :timeout-ms budget is checked only BETWEEN probes; nothing in this
  kit interrupts a predicate that has stopped answering, and the
  interval sleeps are not covered by any external watchdog either. If
  pred itself can hang, bound it at the call site (e.g. run it under
  raced and assert on the outcome map).

  A pred that THROWS propagates immediately — real errors are not
  retried into a slow timeout. Like every primitive in this kit:
  success is the only way through; a timeout can never masquerade as
  one."
  [pred & {:keys [timeout-ms interval-ms]}]
  (when-not (fn? pred)
    (throw (ex-info (str "eventually: predicate must be a function, got "
                         (pr-str pred))
                    {:error/type :concurrency/invalid-arguments
                     :predicate pred})))
  (let [^long budget (cond (nil? timeout-ms) default-timeout-ms
                           :else (check-timeout-ms! "eventually" timeout-ms))
        ^long interval (cond (nil? interval-ms) default-interval-ms
                             :else (check-timeout-ms! "eventually" interval-ms))
        t0 (now-ms)
        deadline (+ t0 budget)]
    (loop [attempts 0]
      (let [answer (pred)]                       ;; throws propagate by design
        (if answer
          answer
          (let [remaining (- deadline (now-ms))]
            (if (pos? remaining)
              (do (Thread/sleep (long (min interval remaining)))
                  (recur (inc attempts)))
              ;; :attempts counts PROBES MADE, per the docstring. The
              ;; accumulator above tallies sleeps taken; the final falsey
              ;; probe is one more — hence (inc attempts). A bare count
              ;; here under-reports the real probe total by one.
              (throw (ex-info (format "eventually: predicate stayed falsey for %d ms across %d attempts (interval %d ms); last value: %s"
                                      budget (inc attempts) interval (pr-str answer))
                              {:error/type :eventually/timeout
                               :timeout-ms budget
                               :interval-ms interval
                               :attempts (inc attempts)
                               :elapsed-ms (- (now-ms) t0)
                               :last-value answer})))))))))

;; ---------------------------------------------------------------------------
;; debugging aid
;; ---------------------------------------------------------------------------

(defn thread-dump!
  "Print name/id/state plus full stack trace of every live thread to
  *err*. Pure diagnostics — reads snapshots, never throws on busy
  threads. Used automatically by `with-thread-dump-on-timeout`."
  []
  (binding [*out* *err*]
    (println "\n---- evoclj.support.concurrency/thread-dump! ----")
    (doseq [^Thread t (sort-by #(.getName ^Thread %)
                               (keys (Thread/getAllStackTraces)))
            :let [frames (get (Thread/getAllStackTraces) t)]]
      (println (format "\"%s\" id=%d state=%s%s"
                       (.getName t) (.getId t) (.getState t)
                       (if (.isDaemon t) " daemon" "")))
      (doseq [^StackTraceElement f frames]
        (println (str "    at " f))))
    (flush)))

(defmacro with-thread-dump-on-timeout
  "Debug aid wrapping `body`: run it on a dedicated daemon thread with
  `timeout-ms` as its whole budget.

  * finishes in time -> returns the body's value;
  * body throws      -> the original Throwable is rethrown unchanged
                        (no dump — that failure needs no forensics);
  * exceeds budget   -> print a FULL thread dump via `thread-dump!` to
                        *err*, then throw ex-info
                        {:error/type :concurrency/timeout :timeout-ms ...}
                        — the suite fails WITH evidence, never hangs.

  NOTE: the body executes WITHOUT the caller's thread-local bindings
  (dynamic vars, *out*, ...) — same caveat as `raced`."
  [timeout-ms & body]
  `(let [budget# ~timeout-ms
         results# (raced [(fn [] ~@body)] :timeout-ms budget#)
         r# (nth results# 0)]
     (case (:status r#)
       :result (:value r#)
       :thrown (throw ^Throwable (:value r#))
       :timeout (do (thread-dump!)
                    (throw (ex-info (str "with-thread-dump-on-timeout: body did not finish within "
                                         budget#
                                         " ms — thread dump printed to *err*")
                                    {:error/type :concurrency/timeout
                                     :timeout-ms budget#}))))))
