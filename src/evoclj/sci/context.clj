(ns evoclj.sci.context
  "Closed SCI execution context with an explicit allow surface
  (component).

  make-context builds a Babashka SCI context for evolvable Genome
  programs. On the DEFAULT surface (no caller :api-namespaces) the
  context carries no ambient host authority (Global Constraint 7):
  no filesystem, no environment, no Java interop, no process
  execution, no dynamic loading, and no arbitrary host vars. An
  EXTENDED surface (non-empty caller :api-namespaces, explicitly
  acknowledged — see make-context and trust-provenance) carries
  exactly the granted host authority, recorded on the value. The
  context is configured by explicit policy ONLY — never :allow :all:

  - :namespaces — exactly the exposed API namespaces (by default
    evoclj.sci.expose/api-namespaces, the pure evo.api.intent data
    constructors) plus whatever the caller adds via :api-namespaces
    (an acknowledged extended surface — see make-context).
    Nothing else is reachable from inside the sandbox.
  - :classes — {} (no additional classes beyond SCI's own internal
    defaults — pure-memory value classes such as String/Exception that
    SCI itself needs; they carry no ambient authority).
  - :allow — an explicit set of pure clojure.core symbols plus the
    fully qualified symbols of every exposed API namespace. Any symbol
    not enumerated is denied at analysis time (\"X is not allowed!\");
    any host symbol that does not even exist in the SCI environment
    fails resolution (\"Unable to resolve symbol: X\"). This is what
    denies System, java.io.File, Runtime, ProcessBuilder, slurp, spit,
    load-file, clojure.core/eval, require/use, read-string, atom/ref/
    delay/future/promise/agent/swap!, interop (the . and new special
    forms), and every undeclared namespace.

  Definitions made inside the context (def/defn) create SCI Vars in the
  isolated environment only; host Clojure Vars are never created or
  mutated (component 4).

  run-form evaluates a program source inside the context and then
  invokes a declared entry symbol with an EDN input value, returning
  the entry's return value. Exceptions — policy denials and program
  errors alike — propagate as clojure.lang.ExceptionInfo; converting
  them to stable serializable error data at the boundary is component

  Trust provenance, recorded ON the value: every context built by
  make-context carries a trust record in its metadata, queryable via
  trust-provenance. A default-surface context records
  {:trust/kind :default-pure} (no ambient host authority); an
  extended-surface context records {:trust/kind :extended-host
  :trust/granted #{...} :trust/provenance ...} — exactly the host
  authority it was granted and the caller's stated provenance (who
  granted, why). A non-empty caller :api-namespaces without explicit
  :trust-host-surface? true fails closed with
  :sci/untrusted-host-surface (gate implemented once in
  evoclj.sci.computation/resolve-trust; this namespace delegates).

  Deadline honesty: the :limits configuration keys are accepted and
  validated for interface stability; loading compiled programs into the
  context (component) and enforcing execution limits (component) are the
  responsibility of later milestones. Wherever :wall-ms is enforced, it
  is a COOPERATIVE, interpreter-bound deadline (fires at interpreted
  fn/loop entries; does NOT preempt a running host fn; NOT an OS-level
  hard deadline — see evoclj.sci.limits). On the default-pure surface
  it is sufficient (nothing reachable can block); on an extended
  surface it must not be relied on as isolation.

  Residual surface, documented honestly: SCI's internal default class
  map still resolves the ~15 pure-memory value classes it needs
  (String, Exception, ExceptionInfo, Integer, Double, Number, Object,
  StringWriter, StringReader, LazySeq, Delay, ArithmeticException,
  IllegalArgumentException, AssertionError,
  LineNumberingPushbackReader). Their constructor shorthand (String.
  ...) is therefore usable inside the sandbox, but every one of them is
  an in-memory value or exception class — none reaches the filesystem,
  environment, JVM runtime, processes, or network, and none can be
  reflectively exercised because the . and new special forms and all
  static access are denied by the allowlist. The SCI layer is useful
  for pure decision logic but useless as an ambient shell."
  (:require [evoclj.kernel.error :as err]
            [evoclj.sci.computation :as computation]
            [evoclj.sci.expose :as expose]
            [sci.core :as sci]))

;; --- the explicit allow surface (single source: evoclj.sci.computation) --------


;; Deprecated alias: the single authoritative core allow list lives in
;; evoclj.sci.computation/core-allow-set (C4/D4). Kept here as an alias
;; so external callers that resolved the private var continue to work,
;; but no duplicated literal exists (INV-05, GC-07).
(def ^:private core-allow-list computation/core-allow-set)

(defn- exposed-symbols
  "Deprecated forwarding to the single implementation in computation.
  Kept for internal call sites; the authoritative version is the
  computation host-surface derivation."
  [api-namespaces]
  (into #{}
        (for [[ns-sym varmap] api-namespaces
              [var-name _] varmap]
          (symbol (str ns-sym) (str var-name)))))

(defn- allow-set
  "Deprecated forwarding: delegates to the single host-surface derivation
  in evoclj.sci.computation. The authoritative allowed set is
  computation/host-surface for the default namespaces, or a computed
  union for custom api-namespaces."
  [api-namespaces]
  (into computation/core-allow-set (exposed-symbols api-namespaces)))

;; --- configuration validation ----------------------------------------------

(defn- validate-config!
  "Validate the make-context configuration shape. Every failure carries
  the stable :sci/context-invalid :error/type with a :reason
  distinguishing the offending key (Global Constraint 22: error data is
  plain, sanitized Clojure data). The :api-namespaces shape check
  delegates to evoclj.sci.computation/validate-api-namespaces! (single
  implementation, INV-05); the trust acknowledgment gate itself runs in
  make-context via computation/resolve-trust."
  [config]
  (when-not (map? config)
    (throw (err/error :sci/context-invalid
                      "make-context expects a configuration map"
                      {:reason :invalid-config
                       :value (err/sanitize config)})))
  (computation/validate-api-namespaces! (:api-namespaces config))
  (let [programs (:programs config)]
    (when-not (or (nil? programs) (map? programs) (sequential? programs))
      (throw (err/error :sci/context-invalid
                        ":programs must be nil, a map, or a sequential collection"
                        {:reason :invalid-programs
                         :value (err/sanitize programs)}))))
  (let [limits (:limits config)]
    (when-not (or (nil? limits) (map? limits))
      (throw (err/error :sci/context-invalid
                        ":limits must be a map"
                        {:reason :invalid-limits
                         :value (err/sanitize limits)}))))
  config)

;; --- public entry points ---------------------------------------------------

(defn make-context
  "Build and return a closed SCI context with an explicit allow surface.

  `config` is a map with the keys declared by the component interface:
  :programs (compiled program descriptors; accepted for interface
  stability, consumed by component), :api-namespaces (a map of namespace
  symbol to map of simple var symbol to host value, extending the
  default evo.api.intent exposure), :trust-host-surface? (explicit
  boolean acknowledgment, REQUIRED when :api-namespaces is non-empty),
  :trust-provenance (optional string or map stating who granted the
  extended surface and why), and :limits (accepted for interface
  stability, consumed by component).

  The context is initialized with explicit :namespaces / :classes /
  :allow policy ONLY — never :allow :all:

  - :namespaces merges the caller's :api-namespaces over the default
    evo.api.intent constructors from evoclj.sci.expose;
  - :classes is {} — no classes beyond SCI's internal pure-memory
    defaults;
  - :allow is the explicit pure core allowlist extended with the fully
    qualified symbols of every exposed API namespace.

  Trust: a non-empty caller :api-namespaces map is an EXTENDED host
  surface and requires :trust-host-surface? exactly true, else the
  build fails closed with :sci/untrusted-host-surface (gate implemented
  once in evoclj.sci.computation/resolve-trust; this fn delegates).
  Every returned context carries its trust provenance record in its
  metadata — query it with trust-provenance. A default-surface context
  carries no ambient host authority; an extended-surface context
  carries exactly the granted host authority named in its record.

  Returns the SCI context (usable with sci/eval-string* and
  evoclj.sci.context/run-form). Inside it, only the enumerated pure
  core symbols and the exposed API constructors resolve; every
  filesystem/environment/interop/process/loading/mutation form is
  denied at analysis time.

  Throws ExceptionInfo with :error/type :sci/context-invalid when
  `config` is malformed, and :sci/untrusted-host-surface when an
  extended surface is requested without explicit acknowledgment."
  [config]
  (validate-config! config)
  (let [trust (computation/resolve-trust config)
        caller-api (:api-namespaces config)
        namespaces (merge expose/api-namespaces caller-api)
        ctx (sci/init {:namespaces namespaces
                       :classes {}
                       :allow (allow-set namespaces)})]
    (vary-meta ctx assoc :sci/trust trust)))

(defn trust-provenance
  "Return the trust provenance record recorded on a context built by
  make-context (nil for contexts built by other means). Shape:
  {:trust/kind :default-pure | :extended-host
   :trust/granted #{<fully qualified syms granted by the caller>}
   :trust/provenance <sanitized caller :trust-provenance or nil>}.
  :default-pure means the default surface only — no ambient host
  authority. :extended-host means the value carries exactly the host
  authority named in :trust/granted. See
  evoclj.sci.computation/resolve-trust (single implementation)."
  [ctx]
  (:sci/trust (meta ctx)))

(defn run-form
  "Evaluate `source` inside the closed `ctx`, then invoke `entry` with
  `input` in that same context.

  `source` is the program's Clojure source text (evaluated with
  sci/eval-string*, which defines the program's vars in the isolated
  SCI environment — host Vars are never touched). `entry` is the
  declared entry symbol (e.g. 'agent.route/run). `input` is the EDN
  input value, serialized with pr-str and read back inside the context.

  Returns the entry's return value. Both the source evaluation and the
  entry invocation happen under the closed allow policy, so a hostile
  source is denied the same way a one-off hostile form is: policy
  violations and program errors propagate as
  clojure.lang.ExceptionInfo (stable serializable error conversion is
  component)."
  [ctx source entry input]
  (sci/eval-string* ctx source)
  (sci/eval-string* ctx (str "(" entry " " (pr-str input) ")")))
