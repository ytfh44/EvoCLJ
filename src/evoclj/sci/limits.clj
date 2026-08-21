(ns evoclj.sci.limits
  "Deterministic resource limits for evolvable SCI execution (component).

  Execution limits are three non-negative integer budgets:

  - :wall-ms — wall-clock deadline, checked with System/nanoTime;
  - :max-steps — step budget; one step is one SCI :interrupt-fn
    invocation, i.e. one interpreted fn or loop entry;
  - :max-output-nodes — output size cap, applied when the result is
    materialized through evoclj.sci.boundary/materialize-edn (as its
    per-collection :max-size).

  Enforcement is interrupt-fn-only, so no per-call worker or watchdog
  threads are ever created: make-interrupt-fn builds a zero-arg host
  fn that SCI invokes on every interpreted fn/loop entry; the fn
  counts steps and checks the deadline, and throws
  sci.interrupt/interrupt! — the typed interrupt that sandboxed code
  cannot catch with try/catch — carrying :error/type
  :sci/limit-exceeded and a :limit key identifying which budget fired.
  Because the check runs on the executing thread, an infinite
  loop/recur is interrupted in place and no thread is left behind.

  All values are plain serializable Clojure data (Global Constraint
  22). Errors follow evoclj.kernel.error conventions; malformed limits
  throw :sci/limits-invalid."
  (:require [evoclj.kernel.error :as err]
            [sci.interrupt :as interrupt]))

(def default-limits
  "The effective limits used for any budget the caller does not supply:
  :wall-ms 1000 (one second wall-clock), :max-steps 100000,
  :max-output-nodes 100000."
  {:wall-ms 1000
   :max-steps 100000
   :max-output-nodes 100000})

(def ^:private limit-keys
  "The closed key set of an execution limits map; anything else is
  rejected (closed maps at trust boundaries, component convention)."
  #{:wall-ms :max-steps :max-output-nodes})

(defn validate-limits!
  "Validate `limits` and return the effective limits.

  `limits` may be nil (all defaults apply) or a map whose keys are a
  subset of :wall-ms, :max-steps, :max-output-nodes with non-negative
  integer values. Unknown keys and non-integer or negative values
  throw ExceptionInfo with :error/type :sci/limits-invalid. Returns
  (merge default-limits limits) — supplied values over the defaults;
  validation never coerces."
  [limits]
  (when-not (or (nil? limits) (map? limits))
    (throw (err/error :sci/limits-invalid
                      "execution limits must be a map (or nil for defaults)"
                      {:reason :invalid-limits :value (err/sanitize limits)})))
  (let [limits (or limits {})]
    (when-let [extra (seq (remove limit-keys (keys limits)))]
      (throw (err/error :sci/limits-invalid
                        "execution limits contains unknown keys"
                        {:reason :unknown-limit-key :keys (vec extra)})))
    (doseq [k limit-keys]
      (let [v (get limits k)]
        (when (and (some? v) (not (and (integer? v) (not (neg? v)))))
          (throw (err/error :sci/limits-invalid
                            (str k " must be a non-negative integer")
                            {:reason :invalid-limit :key k :value (err/sanitize v)})))))
    (merge default-limits limits)))

(defn make-interrupt-fn
  "Build the SCI :interrupt-fn check for ONE execution run.

  `steps` is an atom of integers owned by the caller for the duration
  of the run — it counts interrupt-fn invocations (interpreted fn/loop
  entries) and is reported in the run's :usage. `limits` must be the
  validated effective limits map from validate-limits!.

  Returns a zero-arg fn. On every call it increments `steps`, then
  throws sci.interrupt/interrupt! — the typed interrupt sandboxed code
  cannot catch — with :error/type :sci/limit-exceeded when the step
  budget is exceeded (:limit :max-steps) or the wall-clock deadline
  has passed (:limit :wall-ms). The fn is a plain host fn closed over
  its own per-run state; no threads are created, and each run's check
  is independent even when the same SCI context is reused across
  calls."
  [steps {:keys [wall-ms max-steps]}]
  (let [started (System/nanoTime)]
    (fn []
      (let [n (swap! steps inc)]
        (when (> n max-steps)
          (interrupt/interrupt!
           "execution step budget exceeded"
           {:error/type :sci/limit-exceeded
            :limit :max-steps
            :max-steps max-steps
            :steps n}))
        (when (>= (- (System/nanoTime) started) (* wall-ms 1000000))
          (interrupt/interrupt!
           "execution wall-clock budget exceeded"
           {:error/type :sci/limit-exceeded
            :limit :wall-ms
            :wall-ms wall-ms
            :steps n}))))))
