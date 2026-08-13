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
  <symbol>}}} (Phenotype use — load-program! wires compiled program
  descriptors into such a registry; the descriptor identifies the
  program by :program/id). A runtime is not thread-safe; it belongs to
  one Phenotype/session (Task 6.x single-session FIFO).

  Task 3.4 splits this one-shot flow into a load phase (load-program!,
  which evaluates the source ONCE into the context) and a call phase
  (invoke!, which looks up the registered entry and calls it with EDN
  input); execute-program remains for standalone one-shot use."
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

(defn- validate-compiled-program!
  "Validate the compiled ProgramDescriptor and source text before a
  load. The full descriptor shape (closed key set, schema keywords,
  :source/digest) is the compiler's contract
  (evoclj.compiler.program/compile-program-descriptor); this loader
  validates the fields it consumes: a map carrying a keyword
  :program/id and a qualified :entry symbol, plus a string `source`.
  Every failure throws :program/invalid with a distinguishing :reason
  (Global Constraint 22: error data is plain, sanitized Clojure data)."
  [compiled-program source]
  (when-not (map? compiled-program)
    (throw (err/error :program/invalid
                      "compiled program descriptor must be a map"
                      {:reason :invalid-descriptor
                       :value (err/sanitize compiled-program)})))
  (when-not (keyword? (:program/id compiled-program))
    (throw (err/error :program/invalid
                      "compiled program descriptor must carry a keyword :program/id"
                      {:reason :invalid-descriptor
                       :program/id (:program/id compiled-program)})))
  (let [entry (:entry compiled-program)]
    (when-not (and (symbol? entry) (namespace entry))
      (throw (err/error :program/invalid
                        "compiled program descriptor must carry a qualified :entry symbol"
                        {:reason :invalid-entry
                         :entry entry}))))
  (when-not (string? source)
    (throw (err/error :program/invalid
                      "program source must be a string"
                      {:reason :invalid-source
                       :value (err/sanitize source)})))
  compiled-program)

(def ^:private interrupt-state-key
  "The runtime map key holding the :evoclj/interrupt-state atom.
  load-program! installs it; invoke! swaps the current interrupt check
  inside it per call. It is host-side mutable state owned by the
  runtime (like the SCI context's own environment) and never crosses a
  Genome/SCI/Intent/Event boundary (Global Constraint 22)."
  :evoclj/interrupt-state)

(defn- delegating-interrupt-fn
  "A zero-arg host fn that reads the CURRENT interrupt check from the
  runtime's interrupt-state atom and delegates to it.

  SCI freezes (:interrupt-fn ctx) into every generated fn at
  DEFINITION time (sci.impl.fns/gen-fn reads it when the fn is
  created), so an interrupt assoc'd onto the context at invoke time
  would be invisible to functions already defined by load-program!.
  This delegating fn is frozen in their place at load time instead;
  it derefs the per-call check from the atom, so invoke! can swap
  deterministic limits per call while the program source is evaluated
  only once. When the atom holds nil the fn is a no-op."
  [interrupt-state]
  (fn [] (when-let [check @interrupt-state] (check))))

(defn- default-interrupt
  "A fresh interrupt check enforcing evoclj.sci.limits/default-limits,
  used to bound the load-time source evaluation (a top-level loop in a
  Genome's source must not hang the host) and as the idle value of the
  interrupt-state atom between invocations."
  []
  (limits/make-interrupt-fn (atom 0) limits/default-limits))

(defn load-program!
  "Evaluate a compiled Genome program's source ONCE into the isolated
  SCI context of a runtime and register it under its :program/id
  (Task 3.4, Global Constraints 3, 7, 23).

  `sci-runtime` is a bare closed SCI context
  (evoclj.sci.context/make-context result) or a runtime map
  {:context <ctx> :programs {<program/id> {:source <string> :entry
  <symbol>}}}. `compiled-program` is a ProgramDescriptor from
  evoclj.compiler.program (carrying at least a keyword :program/id and
  a qualified :entry symbol); `source` is the program's source text,
  decoded from the immutable Genome bundle by the caller.

  The source is evaluated with sci/eval-string* into the context's own
  SCI environment ONLY: definitions create SCI Vars in the isolated
  namespace, never host Clojure Vars, and the immutable source Genome
  is never read or written. The registry entry is
  {:source <string> :entry <symbol>} — the shape execute-program's
  :programs registry and invoke! both consume.

  Returns the runtime map with the program registered under
  (:program/id compiled-program), so callers thread it: (-> runtime
  (load-program! d source) (invoke! (:program/id d) input)). A bare
  context input is wrapped into a fresh {:context ctx :programs {}}
  runtime map. Loading the same program again re-evaluates the source
  in place, redefining its SCI vars in that context only (a successor
  Genome redefinition never touches a sibling context).

  Limit enforcement (Task 3.3) is preserved across the load/invoke
  split: SCI freezes the ctx's :interrupt-fn into every fn at
  definition time, so load-program! evaluates the source under a
  DELEGATING interrupt fn that reads the runtime's current check from
  the :evoclj/interrupt-state atom (initialized to the default limits
  so a top-level loop in the source is interrupted, not hung). invoke!
  swaps that atom per call, so already-loaded functions stay bounded
  without re-evaluating the source.

  Throws ExceptionInfo: :program/invalid for a malformed descriptor or
  source (see validate-compiled-program!), :sci/context-invalid for a
  malformed runtime, and the SCI analysis/eval errors (e.g. the
  sandbox denial of a host side effect, or the default-limits
  interrupt on a top-level loop) when the source cannot be evaluated
  inside the closed context."
  [sci-runtime compiled-program source]
  (validate-compiled-program! compiled-program source)
  (let [ctx (runtime-context sci-runtime)
        base (if (and (map? sci-runtime) (contains? sci-runtime :context))
               sci-runtime
               {:context ctx :programs {}})
        interrupt-state (or (get base interrupt-state-key)
                            (atom (default-interrupt)))
        load-ctx (assoc ctx
                        :interrupt-fn (delegating-interrupt-fn
                                       interrupt-state))]
    (sci/eval-string* load-ctx source)
    (-> base
        (assoc interrupt-state-key interrupt-state)
        (assoc-in [:programs (:program/id compiled-program)]
                  {:source source :entry (:entry compiled-program)}))))

(defn invoke!
  "Call a previously loaded program's entry inside the closed SCI
  context with validated EDN input and return validated, fully
  realized EDN output (Task 3.4).

  `sci-runtime` is a runtime map {:context <ctx> :programs {<program/id>
  {:source <string> :entry <symbol>}}} as returned by load-program!;
  `program-id` is the keyword under which the program was registered
  (e.g. :program/route); `input` is the EDN input value, validated
  EDN-safe BEFORE it is serialized into the context (Global Constraint
  22: an infinite lazy input would hang pr-str, so it is rejected, not
  realized). Arity 3 uses the default limits; arity 4 accepts a limits
  map (see evoclj.sci.limits).

  The entry symbol is looked up ONLY in the context's own SCI
  environment — the same isolated namespace the source was evaluated
  into — never in a host namespace. Limits (:wall-ms, :max-steps,
  :max-output-nodes) are enforced without re-evaluating the source:
  load-program! installed a delegating :interrupt-fn into the loaded
  functions, and this call swaps the runtime's :evoclj/interrupt-state
  atom to a fresh check for the duration of the call (restored to the
  default after), so every interpreted fn/loop entry of the loaded
  program counts steps and checks the deadline. The output is
  materialized through evoclj.sci.boundary/materialize-edn under the
  :max-output-nodes cap, so the result is fully realized EDN (no lazy
  sequences, functions, or Java objects cross the boundary).
  Schema-keyword validation of :input-schema/:output-schema awaits the
  schema registry (a later task); EDN-safety and materialization are
  the boundary checks here.

  Returns {:status :ok :value <materialized EDN> :usage {:steps n
  :wall-ms ms}} or {:status :error :error <serializable error data>
  :usage {...}}, with the same error types as execute-program:
  :program/invalid for an unregistered program-id,
  :program/input-invalid for non-EDN input, :sci/limit-exceeded for
  budget interruptions, :edn/* for boundary failures, and
  :sci/execution-error for anything else."
  ([sci-runtime program-id input]
   (invoke! sci-runtime program-id input nil))
  ([sci-runtime program-id input limits]
   (let [started (System/nanoTime)
         steps (atom 0)]
     (try
       (let [limits (limits/validate-limits! limits)
             ctx (runtime-context sci-runtime)
             registry (when (and (map? sci-runtime)
                                 (map? (:programs sci-runtime)))
                        (:programs sci-runtime))
             entry (when (and registry (contains? registry program-id))
                     (:entry (get registry program-id)))
             interrupt-state (get sci-runtime interrupt-state-key)
             current-check (limits/make-interrupt-fn steps limits)]
         (when-not (symbol? entry)
           (throw (err/error :program/invalid
                             "no program loaded for the given program id"
                             {:reason :program-not-found
                              :program/id program-id})))
         (when-not (boundary/edn-safe? input)
           (throw (err/error :program/input-invalid
                             "input must be EDN-safe before crossing the SCI boundary"
                             {:reason :not-edn-safe
                              :value (err/sanitize input)})))
         (when interrupt-state
           (reset! interrupt-state current-check))
         (try
           (let [value (sci/eval-string* ctx
                                         (str "(" entry " " (pr-str input) ")"))
                 value (boundary/materialize-edn
                        value {:max-size (:max-output-nodes limits)})]
             {:status :ok
              :value value
              :usage {:steps @steps
                      :wall-ms (elapsed-ms started)}})
           (finally
             ;; restore the idle check so a later direct evaluation in
             ;; the context (or a later call) starts from the defaults
             (when interrupt-state
               (reset! interrupt-state (default-interrupt))))))
       (catch Throwable t
         {:status :error
          :error (->serializable-error t)
          :usage {:steps @steps
                  :wall-ms (elapsed-ms started)}})))))

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
