(ns evoclj.sci.computation
  "Single Sandbox abstraction as value object (C4/D4).

  Computation is a plain, serializable value that owns the closed SCI
  execution surface and its resource budgets:

    {:computation/context  SCI context (closed, explicit :allow)
     :computation/limits   {:wall-ms :max-steps :max-output-nodes :max-tool-calls}
     :computation/boundary {:max-depth :max-size}
     :computation/programs {program-id {:source string :entry qualified-symbol}}
     :computation/interrupt-state (atom host fn) }

  The last two keys are host-side mutable state owned by the value
  (like the SCI context environment) and never cross an EDN boundary
  (GC-22). They enable the delegating interrupt that keeps already
  compiled functions bounded without re-evaluating source.

  host-surface is the SINGLE source of truth for the explicit allow
  surface (GC-07, INV-05). It merges two previously scattered allow
  sources into one validated pure data value:

    - the pure clojure.core allow list (evoclj.sci.context legacy)
    - the exposed API namespaces (evoclj.sci.expose/api-namespaces)
    - the compiler/program static checks (scan-symbol forbidden/interop
      and host-namespace rules) are expressed as denial of anything
      not in host-surface

  No other namespace defines an allow list; context and the compiler
  both delegate here, satisfying GC-07 (no ambient authority) and
  INV-05 (single implementation).

  Execution reuses the existing interrupt and boundary machinery with
  no new runtime (SCI-Clojure only, no Python/JS):

    - evoclj.sci.limits/make-interrupt-fn for the per-run step and
      wall-clock check that throws sci.interrupt/interrupt! (uncatchable
      inside SCI)
    - the delegating wrapper that lets invoke! swap the check per call
      (SCI freezes :interrupt-fn at definition time)
    - evoclj.sci.boundary/materialize-edn for the EDN-safe materialization
      cap (rejects Java objects, lazy seqs, functions, etc.)

  Public entry points:

    (make-computation config) -> Computation
    (execute computation code-string env) -> {:status :ok/:error ...}
    (load-program! computation descriptor source) -> Computation
    (invoke! computation program-id input) -> {:status ...}
    (execute-program sci-runtime descriptor input limits) -> {:status ...}

  Old evoclj.sci.execute vars remain as deprecated forwarding aliases
  (no behavior change) so the promotion is a clean cutover."
  (:require [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [evoclj.sci.expose :as expose]
            [evoclj.sci.limits :as limits]
            [sci.core :as sci]
            [sci.interrupt :as interrupt])
  (:import (java.nio.charset StandardCharsets)))

;; ---------------------------------------------------------------------------
;; Single authoritative allow surface (pure data, validated)
;; ---------------------------------------------------------------------------
(def core-allow-set
  "Explicit set of pure clojure.core symbols evolvable programs may use.
  Public so evoclj.sci.context can delegate; this is the SINGLE
  definition of the core allow list (no duplication elsewhere)."
  '#{def defn defn- ns fn fn* let let* loop loop* case case* if do quote
     recur throw
     cond and or when when-not when-let when-some if-let if-some
     -> ->> some-> some->> as-> cond-> cond->> doto
     + - * / inc dec max min mod rem quot
     = == not= < > <= >= zero? pos? neg? even? odd?
     number? integer? int? float? double? boolean? keyword? symbol?
     string? char? vector? map? set? seq? coll? sequential? associative?
     counted? empty? seqable? nil? some? true? false? not
     count first second last rest next nnext nth nthnext butlast take
     drop take-while drop-while range
     get get-in assoc assoc-in dissoc update update-in merge merge-with
     select-keys keys vals find contains? key val conj cons into vector
     vec list list* set seq mapv reverse sort sort-by
     apply comp partial constantly identity juxt complement
     map filter remove reduce reduce-kv keep keep-indexed partition
     partition-all group-by frequencies empty not-empty
     seq-to-map-for-destructuring
     str subs format name namespace keyword symbol gensym
     hash-map hash-set array-map pr-str
     ex-info ex-message ex-data})

(defn- exposed-symbols
  "Fully qualified symbols of every var in an API namespace map."
  [api-namespaces]
  (into #{}
        (for [[ns-sym varmap] api-namespaces
              [var-name _] varmap]
          (symbol (str ns-sym) (str var-name)))))

(defn- allow-set
  "Complete explicit allow set: core symbols plus every exposed API var."
  [api-namespaces]
  (into core-allow-set (exposed-symbols api-namespaces)))

(defn- validate-allow-set!
  "Validate that s is a set of symbols; throw :sci/context-invalid otherwise."
  [s]
  (when-not (and (set? s) (every? symbol? s))
    (throw (err/error :sci/context-invalid
                      "host-surface must be a set of symbols"
                      {:reason :invalid-host-surface
                       :value (err/sanitize s)})))
  s)

(def host-surface
  "Single authoritative allow surface as pure data: a validated set of
  symbols (bare core symbols plus fully qualified API symbols). Merges
  the compiler/program scan-symbol allow checks (pure core plus declared
  namespaces) and expose/api-namespaces into one host-surface def.
  No other namespace defines an allow list; all delegates check here.
  Validated at load time so an invalid surface fails fast (GC-22)."
  (validate-allow-set! (allow-set expose/api-namespaces)))

(def host-allowed-namespaces
  "Set of namespace symbols that are explicitly allowed as host
  namespaces, derived from the qualified symbols in host-surface.
  Used by the compiler static policy to decide whether a qualified
  symbol like evo.api.intent/tool-call is host-allowed."
  (into #{}
        (keep (fn [sym]
                (when-let [ns-str (namespace sym)]
                  (symbol ns-str)))
              host-surface)))

(defn allowed-symbol?
  "True when sym is explicitly allowed by host-surface. Bare symbols
  are checked directly; qualified symbols are allowed when their
  fully qualified form is in host-surface."
  [sym]
  (contains? host-surface sym))

(defn allowed-namespace?
  "True when ns-sym is an explicitly allowed host namespace (derived
  from host-surface qualified symbols)."
  [ns-sym]
  (contains? host-allowed-namespaces ns-sym))

;; ---------------------------------------------------------------------------
;; Computation value object
;; ---------------------------------------------------------------------------

(defn- validate-boundary!
  "Validate boundary opts and return sanitized map."
  [boundary]
  (when-not (or (nil? boundary) (map? boundary))
    (throw (err/error :sci/context-invalid
                      "boundary must be a map"
                      {:reason :invalid-boundary
                       :value (err/sanitize boundary)})))
  (let [b (or boundary {})]
    (doseq [k [:max-depth :max-size]]
      (let [v (get b k)]
        (when (and (some? v) (not (and (integer? v) (not (neg? v)))))
          (throw (err/error :sci/context-invalid
                            (str (name k) " must be a non-negative integer")
                            {:reason :invalid-boundary :key k :value (err/sanitize v)})))))
    (when-let [extra (seq (remove #{:max-depth :max-size} (keys b)))]
      (throw (err/error :sci/context-invalid
                        "boundary contains unknown keys"
                        {:reason :unknown-boundary-key :keys (vec extra)})))
    b))

(defn make-computation
  "Create a Computation value object.

  Config keys (all optional, validated):
    :api-namespaces - map of ns symbol to var map (extends expose/api-namespaces)
    :limits         - {:wall-ms :max-steps :max-output-nodes :max-tool-calls}
    :boundary       - {:max-depth :max-size}
    :programs       - initial program registry (for compatibility)

  Returns a plain map:

    {:computation/context        SCI context (closed, explicit allow)
     :computation/limits         validated effective limits
     :computation/boundary       validated boundary opts
     :computation/programs       {program-id {:source :entry}}
     :computation/interrupt-state (atom host fn) }

  The last two are host-side mutable state owned by the value and never
  cross an EDN boundary. Uses context semantics via sci/init with the
  single host-surface allow set, limits/make-interrupt-fn for per-run
  checks, and boundary/materialize-edn for output materialization."
  ([] (make-computation {}))
  ([config]
   (when-not (map? config)
     (throw (err/error :sci/context-invalid
                       "computation config must be a map"
                       {:reason :invalid-config :value (err/sanitize config)})))
   (let [api-namespaces (merge expose/api-namespaces (:api-namespaces config))
         limits (limits/validate-limits! (:limits config))
         ;; :max-tool-calls is an additional budget handled similarly to max-steps
         ;; but not enforced by the SCI interrupt; validated here for completeness
         _ (when-let [v (:max-tool-calls limits)]
             (when (and (some? v) (not (and (integer? v) (not (neg? v)))))
               (throw (err/error :sci/limits-invalid
                                 ":max-tool-calls must be a non-negative integer"
                                 {:reason :invalid-limit :key :max-tool-calls :value (err/sanitize v)}))))
         boundary (validate-boundary! (:boundary config))
         programs (or (:programs config) {})
         _ (when-not (map? programs)
             (throw (err/error :sci/context-invalid
                               ":programs must be a map"
                               {:reason :invalid-programs :value (err/sanitize programs)})))
         sci-ctx (sci/init {:namespaces api-namespaces
                            :classes {}
                            :allow (allow-set api-namespaces)})]
     {:computation/context sci-ctx
      :computation/limits limits
      :computation/boundary boundary
      :computation/programs programs
      :computation/interrupt-state (atom (limits/make-interrupt-fn (atom 0) limits/default-limits))})))

(defn computation?
  "True when x looks like a Computation value object."
  [x]
  (and (map? x)
       (contains? x :computation/context)
       (contains? x :computation/limits)
       (contains? x :computation/boundary)))

;; ---------------------------------------------------------------------------
;; Delegating interrupt (retained from sci/execute, reused here)
;; ---------------------------------------------------------------------------

(def ^:private interrupt-state-key :computation/interrupt-state)

(defn- delegating-interrupt-fn
  "Zero-arg host fn that delegates to the current check in interrupt-state atom.
  SCI freezes :interrupt-fn at definition time, so per-call swapping via the
  atom keeps already-defined functions bounded without re-evaluation."
  [interrupt-state]
  (fn [] (when-let [check @interrupt-state] (check))))

(defn- default-interrupt
  "Fresh interrupt check for default limits, used as idle value."
  []
  (limits/make-interrupt-fn (atom 0) limits/default-limits))

(defn- runtime-context
  "SCI context from a Computation, legacy runtime map, or bare SCI context."
  [comp-or-ctx]
  (let [ctx (cond
              (and (map? comp-or-ctx) (contains? comp-or-ctx :computation/context))
              (:computation/context comp-or-ctx)
              (and (map? comp-or-ctx) (contains? comp-or-ctx :context))
              (:context comp-or-ctx)
              :else comp-or-ctx)]
    (when-not (and ctx (map? ctx))
      (throw (err/error :sci/context-invalid
                        "runtime must be a Computation or a closed SCI context"
                        {:reason :invalid-runtime :value (err/sanitize comp-or-ctx)})))
    ctx))

(defn- elapsed-ms [started]
  (long (/ (- (System/nanoTime) started) 1000000)))

(defn- typed-cause
  [^Throwable t]
  (loop [^Throwable cur t]
    (when cur
      (let [data (ex-data cur)]
        (if (and (map? data) (contains? data :error/type))
          cur
          (recur (.getCause cur)))))))

(defn- ->serializable-error [^Throwable t]
  (if-let [typed (typed-cause t)]
    (err/error-data
     (ex-info (.getMessage typed)
              (dissoc (ex-data typed) :sci.impl/interrupt)
              (.getCause typed)))
    (err/error-data
     (err/error :sci/execution-error
                (or (.getMessage t) "SCI execution failed")
                {:cause t}))))

(defn- validate-compiled-program!
  [compiled-program source]
  (when-not (map? compiled-program)
    (throw (err/error :program/invalid
                      "compiled program descriptor must be a map"
                      {:reason :invalid-descriptor :value (err/sanitize compiled-program)})))
  (when-not (keyword? (:program/id compiled-program))
    (throw (err/error :program/invalid
                      "compiled program descriptor must carry a keyword :program/id"
                      {:reason :invalid-descriptor :program/id (:program/id compiled-program)})))
  (let [entry (:entry compiled-program)]
    (when-not (and (symbol? entry) (namespace entry))
      (throw (err/error :program/invalid
                        "compiled program descriptor must carry a qualified :entry symbol"
                        {:reason :invalid-entry :entry entry}))))
  (when-not (string? source)
    (throw (err/error :program/invalid
                      "program source must be a string"
                      {:reason :invalid-source :value (err/sanitize source)})))
  compiled-program)

(defn- resolve-program
  [comp-or-ctx descriptor]
  (let [registry (when (and (map? comp-or-ctx) (map? (:computation/programs comp-or-ctx)))
                   (:computation/programs comp-or-ctx))]
    (if (and registry (contains? registry (:program/id descriptor)))
      (get registry (:program/id descriptor))
      descriptor)))

;; ---------------------------------------------------------------------------
;; Public execution API
;; ---------------------------------------------------------------------------

(defn execute
  "Execute code-string inside Computation sandbox.

  Computation is a value object from make-computation (or a bare SCI
  context for compatibility). code-string is Clojure source to evaluate
  inside the closed context. env is an optional EDN map used as input
  bindings: when it contains an :entry qualified symbol, that entry is
  invoked with :input; otherwise the code-string evaluation result is
  returned.

  Reuses limits/make-interrupt-fn (uncatchable sci.interrupt/interrupt!)
  and boundary/materialize-edn (rejects Java objects, laziness, etc.)
  with no new runtime. Returns {:status :ok :value EDN :usage {...}}
  or {:status :error :error serializable :usage {...}}.

  Limits are taken from (:computation/limits computation) unless
  overridden by (:limits env). Boundary caps from
  (:computation/boundary computation) are applied at materialization."
  ([computation code-string] (execute computation code-string nil))
  ([computation code-string env]
   (let [started (System/nanoTime)
         steps (atom 0)]
     (try
       (let [is-comp (computation? computation)
             ctx (runtime-context computation)
             comp-limits (if is-comp (:computation/limits computation) limits/default-limits)
             comp-boundary (if is-comp (:computation/boundary computation) {})
             env-limits (when (map? env) (:limits env))
             limits (limits/validate-limits! (or env-limits comp-limits))
             interrupt-fn (limits/make-interrupt-fn steps limits)
             ;; For delegating runtime (load-program! case), swap the
             ;; interrupt-state atom; for plain context, assoc directly.
             interrupt-state (when is-comp (:computation/interrupt-state computation))
             exec-ctx (if interrupt-state
                        (do (reset! interrupt-state interrupt-fn)
                            (assoc ctx :interrupt-fn (delegating-interrupt-fn interrupt-state)))
                        (assoc ctx :interrupt-fn interrupt-fn))
             boundary-opts (merge {:max-depth 64 :max-size (:max-output-nodes limits)}
                                  comp-boundary
                                  (when (map? env) (:boundary env)))]
         (try
           (let [raw (sci/eval-string* exec-ctx code-string)
                 ;; If env requests an entry invocation, call it
                 value (if (and (map? env) (symbol? (:entry env)))
                         (let [entry (:entry env)
                               input (:input env)
                               _ (when-not (boundary/edn-safe? input)
                                   (throw (err/error :program/input-invalid
                                                     "input must be EDN-safe before crossing the SCI boundary"
                                                     {:reason :not-edn-safe :value (err/sanitize input)})))
                               v (sci/eval-string* exec-ctx (str "(" entry " " (pr-str input) ")"))]
                           v)
                         raw)
                 value (boundary/materialize-edn value {:max-depth (:max-depth boundary-opts)
                                                        :max-size (:max-size boundary-opts)
                                                        :allowed-records (:allowed-records boundary-opts)})]
             {:status :ok
              :value value
              :usage {:steps @steps :wall-ms (elapsed-ms started)}})
           (finally
             (when interrupt-state
               (reset! interrupt-state (default-interrupt))))))
       (catch Throwable t
         {:status :error
          :error (->serializable-error t)
          :usage {:steps @steps :wall-ms (elapsed-ms started)}})))))

(defn load-program!
  "Evaluate a compiled program source ONCE into the Computation context
  and register it. Delegates to the Computation value object; deprecated
  callers should use make-computation / execute directly.

  Computation is a Computation value or bare SCI context.
  compiled-program is a descriptor with :program/id and :entry.
  source is the program source string. Returns an updated Computation
  (or runtime map) with the program registered."
  [computation compiled-program source]
  (validate-compiled-program! compiled-program source)
  (let [is-comp (computation? computation)
        base (if is-comp
               computation
               (let [ctx (runtime-context computation)]
                 {:computation/context ctx
                  :computation/limits limits/default-limits
                  :computation/boundary {}
                  :computation/programs {}
                  :computation/interrupt-state (atom (default-interrupt))}))
        ctx (:computation/context base)
        interrupt-state (or (:computation/interrupt-state base) (atom (default-interrupt)))
        load-ctx (assoc ctx :interrupt-fn (delegating-interrupt-fn interrupt-state))]
    (sci/eval-string* load-ctx source)
    (-> base
        (assoc :computation/interrupt-state interrupt-state)
        (assoc-in [:computation/programs (:program/id compiled-program)]
                  {:source source :entry (:entry compiled-program)})
        ;; Also maintain legacy :programs/:context keys for callers that
        ;; expect the old runtime shape (evoclj.sci.execute compatibility)
        (assoc :context ctx)
        (assoc :programs (into {} (map (fn [[k v]] [k v]) (:computation/programs base))))
        (assoc-in [:programs (:program/id compiled-program)] {:source source :entry (:entry compiled-program)})
        (assoc-in [:computation/programs (:program/id compiled-program)] {:source source :entry (:entry compiled-program)}))))

(defn invoke!
  "Call a previously loaded program entry inside the Computation context.

  Computation is a Computation value (or legacy runtime map) as returned
  by load-program!. program-id is the keyword under which the program
  was registered. input is EDN input. Optional limits override the
  Computation limits. Returns {:status :ok :value ...} or
  {:status :error :error ...} with EDN-safe materialization and
  uncatchable interrupt enforcement."
  ([computation program-id input] (invoke! computation program-id input nil))
  ([computation program-id input limits]
   (let [started (System/nanoTime)
         steps (atom 0)]
     (try
       (let [is-comp (or (computation? computation) (contains? computation :computation/context))
             comp-limits (if is-comp
                           (or (:computation/limits computation) limits/default-limits)
                           limits/default-limits)
             effective-limits (limits/validate-limits! (or limits comp-limits))
             ctx (runtime-context computation)
             registry (or (:computation/programs computation) (:programs computation))
             entry (when (and registry (contains? registry program-id))
                     (:entry (get registry program-id)))
             interrupt-state (or (:computation/interrupt-state computation)
                                 (:evoclj/interrupt-state computation))
             current-check (limits/make-interrupt-fn steps effective-limits)]
         (when-not (symbol? entry)
           (throw (err/error :program/invalid
                             "no program loaded for the given program id"
                             {:reason :program-not-found :program/id program-id})))
         (when-not (boundary/edn-safe? input)
           (throw (err/error :program/input-invalid
                             "input must be EDN-safe before crossing the SCI boundary"
                             {:reason :not-edn-safe :value (err/sanitize input)})))
         (when interrupt-state
           (reset! interrupt-state current-check))
         (try
           (let [value (sci/eval-string* ctx (str "(" entry " " (pr-str input) ")"))
                 value (boundary/materialize-edn value {:max-size (:max-output-nodes effective-limits)})]
             {:status :ok
              :value value
              :usage {:steps @steps :wall-ms (elapsed-ms started)}})
           (finally
             (when interrupt-state
               (reset! interrupt-state (default-interrupt))))))
       (catch Throwable t
         {:status :error
          :error (->serializable-error t)
          :usage {:steps @steps :wall-ms (elapsed-ms started)}})))))

(defn execute-program
  "One-shot program execution inside a Computation or bare SCI context.

  sci-runtime is a Computation or bare SCI context (standalone) or a
  runtime map {:context ctx :programs {...}}. program-descriptor carries
  :source and :entry (or :program/id for registry lookup). input is
  EDN input, limits is optional limits map. Reuses the same interrupt
  and boundary machinery as execute/invoke! (no new threads, no
  behavior change). Returns {:status :ok :value ...} or
  {:status :error ...}."
  [sci-runtime program-descriptor input limits]
  (let [started (System/nanoTime)
        steps (atom 0)]
    (try
      (let [is-comp (computation? sci-runtime)
             comp-limits (if is-comp (:computation/limits sci-runtime) limits/default-limits)
             effective-limits (limits/validate-limits! (or limits comp-limits))
             interrupt-fn (limits/make-interrupt-fn steps effective-limits)
             program (resolve-program sci-runtime program-descriptor)
             source (:source program)
             entry (:entry program)]
        (when-not (and (string? source) (symbol? entry))
          (throw (err/error :program/invalid
                            "no program found for the given descriptor"
                            {:reason :program-not-found :program/id (:program/id program-descriptor)})))
        (when-not (boundary/edn-safe? input)
          (throw (err/error :program/input-invalid
                            "input must be EDN-safe before crossing the SCI boundary"
                            {:reason :not-edn-safe :value (err/sanitize input)})))
        (let [exec-ctx (assoc (runtime-context sci-runtime) :interrupt-fn interrupt-fn)]
          (sci/eval-string* exec-ctx source)
          (let [value (sci/eval-string* exec-ctx (str "(" entry " " (pr-str input) ")"))
                value (boundary/materialize-edn value {:max-size (:max-output-nodes effective-limits)})]
            {:status :ok
             :value value
             :usage {:steps @steps :wall-ms (elapsed-ms started)}})))
      (catch Throwable t
        {:status :error
         :error (->serializable-error t)
         :usage {:steps @steps :wall-ms (elapsed-ms started)}}))))

;; ---------------------------------------------------------------------------
;; Sandbox tool_fn injection (P8)
;; ---------------------------------------------------------------------------

(def ^:private sandbox-max-code-bytes 8192)
(def ^:private sandbox-max-tool-calls 32)
(def ^:private sandbox-wall-ms 5000)
(def ^:private sandbox-max-steps 100000)

(defn- bytes-ok?
  "Wolfram verified: code byte count within sandbox budget."
  [code-str]
  (<= (alength (.getBytes ^String code-str StandardCharsets/UTF_8)) sandbox-max-code-bytes))

(defn- calls-ok?
  "Wolfram verified: tool call count within sandbox budget."
  [n]
  (<= n sandbox-max-tool-calls))

(defn- limits-check
  "Wolfram verified lattice: bytesOk and callsOk must both hold.
  Single limitsCheck per INV-05."
  [code-str tool-call-count]
  (and (bytes-ok? code-str) (calls-ok? tool-call-count)))

(defn- ->code-bytes-error
  [code-str]
  (err/error :sci/limit-exceeded
             "code byte budget exceeded"
             {:limit :code-bytes
              :max sandbox-max-code-bytes
              :found (alength (.getBytes ^String code-str StandardCharsets/UTF_8))}))

(defn- ->tool-calls-error
  [n]
  (err/error :sci/limit-exceeded
             "tool call budget exceeded"
             {:limit :max-tool-calls
              :max sandbox-max-tool-calls
              :found n}))

(defn execute-code
  "Execute code-str inside Computation with injected toolFns.

  computation is a Computation value from make-computation. code-str
  is SCI Clojure source to evaluate. tool-fns is a map of tool-id
  (string, keyword, or symbol) to host fn of arity [args] where args
  is the EDN value passed from SCI. Each tool fn is exposed inside
  SCI as tool/<id> and must cross the broker via the pipeline when
  wired through orchestrator/make-tool-fns; here it is a plain host
  callback retained for test wiring.

  Limits (Wolfram lattice):
    - codeBytes 8192 pre-check on UTF-8 bytes of code-str
    - toolCalls 32 counted inside each tool wrapper, enforced with
      uncatchable sci.interrupt/interrupt! (same mechanism as wall/steps)
    - wall 5000 ms and steps 100000 via limits/make-interrupt-fn
      (uncatchable inside SCI)

  Args and return values are enforced EDN-safe via
  boundary/materialize-edn. Interrupts are uncatchable inside SCI and
  surface as {:status :error :error {:error/type :sci/limit-exceeded}}.
  Returns {:status :ok :value EDN :events [] :usage {:steps :wall-ms}}
  or {:status :error :error serializable :events [] :usage {...}}.
  Single limitsCheck per INV-05, GC-07 host-surface remains single source."
  ([computation code-str] (execute-code computation code-str nil))
  ([computation code-str tool-fns]
   (let [started (System/nanoTime)
         steps (atom 0)
         tool-calls (atom 0)]
     (try
       (when-not (string? code-str)
         (throw (err/error :sci/invalid-code
                           "code-str must be a string"
                           {:reason :invalid-code :value (err/sanitize code-str)})))
       (when (and (some? tool-fns) (not (map? tool-fns)))
         (throw (err/error :sci/invalid-tool-fns
                           "tool-fns must be a map of tool-id to fn"
                           {:reason :invalid-tool-fns :value (err/sanitize tool-fns)})))
       (when-not (bytes-ok? code-str)
         (throw (->code-bytes-error code-str)))
       (let [tool-fns (or tool-fns {})
             _ (doseq [[k f] tool-fns]
                 (when-not (fn? f)
                   (throw (err/error :sci/invalid-tool-fns
                                     "each tool-fn must be a fn"
                                     {:tool/id (err/sanitize k) :value (err/sanitize f)}))))
             sandbox-limits {:wall-ms sandbox-wall-ms
                             :max-steps sandbox-max-steps
                             :max-output-nodes (:max-output-nodes (:computation/limits computation) 100000)}
             effective-limits (limits/validate-limits!
                               (merge (:computation/limits computation) sandbox-limits))
             interrupt-fn (limits/make-interrupt-fn steps effective-limits)
             wrapped-tools
             (into {}
                   (for [[tool-id f] tool-fns]
                     (let [sym (symbol (name tool-id))]
                       [sym
                        (fn [arg]
                          (let [n (swap! tool-calls inc)]
                            (when-not (calls-ok? n)
                              (interrupt/interrupt!
                               "tool call budget exceeded"
                               {:error/type :sci/limit-exceeded
                                :limit :max-tool-calls
                                :max sandbox-max-tool-calls
                                :found n}))
                            (let [safe-arg (boundary/materialize-edn arg {:max-depth 64 :max-size 100000})
                                  result (f safe-arg)
                                  safe-result (boundary/materialize-edn result {:max-depth 64 :max-size 100000})]
                              safe-result)))])))
             expose-namespaces expose/api-namespaces
             tool-ns-map wrapped-tools
             namespaces (if (seq tool-ns-map)
                          (assoc expose-namespaces 'tool tool-ns-map)
                          expose-namespaces)
             tool-syms (set (map (fn [[k _]] (symbol "tool" (str k))) tool-ns-map))
             allow (into host-surface tool-syms)
             base-ctx (runtime-context computation)
             exec-ctx (assoc (sci/init {:namespaces namespaces
                                        :classes {}
                                        :allow allow})
                             :interrupt-fn interrupt-fn)
             _ (when (and (computation? computation)
                          (:computation/interrupt-state computation))
                 (reset! (:computation/interrupt-state computation) interrupt-fn))
             raw (sci/eval-string* exec-ctx code-str)
             value (boundary/materialize-edn raw {:max-depth 64 :max-size (:max-output-nodes effective-limits)})]
         {:status :ok
          :value value
          :events []
          :usage {:steps @steps :wall-ms (elapsed-ms started) :tool-calls @tool-calls}})
       (catch Throwable t
         {:status :error
          :error (->serializable-error t)
          :events []
          :usage {:steps @steps :wall-ms (elapsed-ms started) :tool-calls @tool-calls}})))))
