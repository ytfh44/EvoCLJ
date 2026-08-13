(ns evoclj.sci.execute
  "Bounded execution of evolvable programs inside the closed SCI
  context (Task 3.3).

  execute-program runs one program — a source string evaluated inside
  the closed SCI context (Task 3.1) followed by an entry invocation
  with an EDN input — under deterministic resource limits and returns
  a plain result map:

    {:status :ok :value <materialized EDN> :usage {:steps n :wall-ms ms}}
    {:status :error :error <serializable error data> :usage {...}}

  Limits (:wall-ms, :max-steps, :max-output-nodes — see
  evoclj.sci.limits) are enforced interrupt-fn-only: a fresh
  :interrupt-fn is assoc'd onto the context for this call, SCI invokes
  it on every interpreted fn/loop entry, and it throws the typed,
  sandbox-uncatchable sci.interrupt/interrupt! error when the step
  budget or wall-clock deadline is exceeded. No worker or watchdog
  threads are created (the check runs on the executing thread). The
  output value is materialized through
  evoclj.sci.boundary/materialize-edn under the :max-output-nodes size
  cap, so excessive output is a typed :edn/size-exceeded error, never
  a hang (Task 3.2). Input must be EDN-safe before it is serialized
  into the context (Global Constraint 22: an infinite lazy input would
  hang pr-str, so it is rejected, not realized).

  Every failure is converted at this boundary into stable serializable
  error data (evoclj.kernel.error conventions): budget interruptions
  carry :error/type :sci/limit-exceeded with a :limit key; boundary
  failures keep their :edn/* / :program/* types; an error a program
  raises via ex-info keeps its own :error/type; anything else is typed
  :sci/execution-error. SCI wraps exceptions from evaluated code in
  its own ex-info, so the cause chain is unwalked to find the typed
  error, and the raw :sci.impl/interrupt marker object interrupt!
  attaches is stripped before conversion.

  sci-runtime is either a bare closed SCI context (standalone use; the
  descriptor itself must carry :source and :entry) or a runtime map
  {:context <ctx> :programs {<program/id> {:source <string> :entry
  <symbol>}}} (Phenotype use — Task 3.4 wires compiled program
  descriptors into such a registry; the descriptor identifies the
  program by :program/id). A runtime is not thread-safe; it belongs to
  one Phenotype/session (Task 6.x single-session FIFO)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [evoclj.sci.limits :as limits]
            [sci.core :as sci]))

(defn- runtime-context
  "The SCI context of a sci-runtime: a runtime map {:context ctx ...}
  contributes its :context; a bare SCI context (a sci.impl.opts.Ctx
  record) is returned as-is. Throws :sci/context-invalid when neither
  shape yields a context."
  [sci-runtime]
  (let [ctx (if (and (map? sci-runtime) (contains? sci-runtime :context))
              (:context sci-runtime)
              sci-runtime)]
    (when-not (and ctx (map? ctx))
      (throw (err/error :sci/context-invalid
                        "sci-runtime must be a closed SCI context or a {:context ctx ...} map"
                        {:reason :invalid-runtime
                         :value (err/sanitize sci-runtime)})))
    ctx))

(defn- resolve-program
  "The program to execute: when the runtime carries a :programs
  registry and the descriptor's :program/id is registered there, the
  registry entry wins; otherwise the descriptor itself is the program
  (standalone mode, where the descriptor carries :source and :entry)."
  [sci-runtime descriptor]
  (let [registry (when (and (map? sci-runtime) (map? (:programs sci-runtime)))
                   (:programs sci-runtime))]
    (if (and registry (contains? registry (:program/id descriptor)))
      (get registry (:program/id descriptor))
      descriptor)))

(defn- typed-cause
  "The first Throwable on t's cause chain whose ex-data carries an
  :error/type, or nil. SCI wraps exceptions raised inside evaluated
  code in its own ex-info ({:type :sci/error ...}); the typed error
  this runtime throws — or a program raises via ex-info — lives on the
  cause, so the wrapper must be unwrapped to preserve the type."
  [^Throwable t]
  (loop [^Throwable cur t]
    (when cur
      (let [data (ex-data cur)]
        (if (and (map? data) (contains? data :error/type))
          cur
          (recur (.getCause cur)))))))

(defn- ->serializable-error
  "Convert any Throwable from an execution run into stable serializable
  error data (Global Constraint 22), reusing
  evoclj.kernel.error/error-data conventions.

  A typed cause keeps its :error/type — :sci/limit-exceeded for budget
  interruptions, :edn/* and :program/* for boundary failures, or
  whatever a program raises via ex-info. The :sci.impl/interrupt
  marker object sci.interrupt attaches to interrupt! is stripped
  (a raw Java object must not cross the boundary). Everything else is
  typed :sci/execution-error with the original throwable sanitized as
  :error/cause."
  [^Throwable t]
  (if-let [typed (typed-cause t)]
    (err/error-data
     (ex-info (.getMessage typed)
              (dissoc (ex-data typed) :sci.impl/interrupt)
              (.getCause typed)))
    (err/error-data
     (err/error :sci/execution-error
                (or (.getMessage t) "SCI execution failed")
                {:cause t}))))

(defn- elapsed-ms
  "Whole milliseconds elapsed since the nanoTime `started`."
  [started]
  (long (/ (- (System/nanoTime) started) 1000000)))

(defn execute-program
  "Execute one evolvable program inside the closed SCI context with
  deterministic resource limits (Task 3.3).

  `sci-runtime` is either a bare closed SCI context
  (evoclj.sci.context/make-context result — standalone use, where
  `program-descriptor` itself carries :source and :entry) or a runtime
  map {:context <ctx> :programs {<program/id> {:source <string> :entry
  <symbol>}}} (Phenotype use, Task 3.4; the descriptor identifies the
  program by :program/id).

  `limits` is a map of non-negative integers: :wall-ms (wall-clock
  budget, default 1000), :max-steps (fn/loop entry budget, default
  100000), :max-output-nodes (per-collection output size cap applied
  at materialization, default 100000). Unknown keys or nil are
  handled by evoclj.sci.limits/validate-limits!.

  Returns {:status :ok :value <materialized EDN> :usage {:steps n
  :wall-ms ms}} or {:status :error :error <serializable error data>
  :usage {...}} where :error round-trips through
  pr-str / clojure.edn read-string. Budget interruptions are typed
  :sci/limit-exceeded (:limit :max-steps | :wall-ms); output caps are
  typed :edn/size-exceeded; invalid input is :program/input-invalid;
  missing programs are :program/invalid; anything else is
  :sci/execution-error."
  [sci-runtime program-descriptor input limits]
  (let [started (System/nanoTime)
        steps (atom 0)]
    (try
      (let [limits (limits/validate-limits! limits)
            interrupt-fn (limits/make-interrupt-fn steps limits)
            program (resolve-program sci-runtime program-descriptor)
            source (:source program)
            entry (:entry program)]
        (when-not (and (string? source) (symbol? entry))
          (throw (err/error :program/invalid
                            "no program found for the given descriptor"
                            {:reason :program-not-found
                             :program/id (:program/id program-descriptor)})))
        (when-not (boundary/edn-safe? input)
          (throw (err/error :program/input-invalid
                            "input must be EDN-safe before crossing the SCI boundary"
                            {:reason :not-edn-safe
                             :value (err/sanitize input)})))
        (let [exec-ctx (assoc (runtime-context sci-runtime)
                              :interrupt-fn interrupt-fn)]
          (sci/eval-string* exec-ctx source)
          (let [value (sci/eval-string* exec-ctx
                                        (str "(" entry " " (pr-str input) ")"))
                value (boundary/materialize-edn
                       value {:max-size (:max-output-nodes limits)})]
            {:status :ok
             :value value
             :usage {:steps @steps
                     :wall-ms (elapsed-ms started)}})))
      (catch Throwable t
        {:status :error
         :error (->serializable-error t)
         :usage {:steps @steps
                 :wall-ms (elapsed-ms started)}}))))
