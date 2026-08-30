(ns evoclj.runtime.phenotype
  "Phenotype construction and lifecycle (component).

  instantiate turns a CompiledGenome (evoclj.compiler.core) plus a
  runtime-deps map into a live Phenotype:

  Identity split (PLT6):

    code-id = SHA256(kernel-abi || genome-id || resolution-id)
    deployment-id = SHA256(code-id || canonical(leases) || canonical(bindings))

  CodeId identifies pure compiled code (shared by all deployments of the
  same Genome and Resolution). DeploymentId binds CodeId to concrete
  runtime leases and durable bindings.

  (instantiate compiled-genome runtime-deps)
  ;; => {:phenotype/id ...
  ;;     :code/id ...
  ;;     :deployment/id ...
  ;;     :compiled compiled-genome
  ;;     :sci-runtime ...
  ;;     :providers ...
  ;;     :capabilities ...
  ;;     :stores ...}

  THE PHENOTYPE OWNS ONE THING: its isolated SCI runtime. instantiate
  builds a fresh closed SCI context (evoclj.sci.context/make-context)
  and loads every compiled program into it via
  evoclj.sci.execute/load-program!, so the mutable SCI state (SCI Vars,
  the :programs registry, the :evoclj/interrupt-state atom) belongs to
  this Phenotype alone: two Phenotypes from one Genome share the SAME
  immutable CompiledGenome value while keeping fully isolated SCI
  contexts (Global Constraints 3, 22, 23 — a live Phenotype never
  modifies its Genome; redefining a SCI var in one Phenotype never
  touches a sibling). All external effects still cross the kernel-owned
  Intent/Capability Broker (Global Constraint 8); this namespace
  performs no effects and runs no programs.

  RUNTIME-DEPS CONTRACT (the map the host injects; designed here,
  normative for component):

    {:stores {:sqlite <db-spec-or-path>   ; OPTIONAL. Declared ONLY:
              :cas <cas-handle>}          ; instantiate never opens,
                                          ; validates, or touches these
                                          ; values beyond map shape.
     :providers {:registry <atom>}        ; REQUIRED. The provider
                                          ; registry atom from
                                          ; evoclj.provider.registry/
                                          ; create-registry. Referenced
                                          ; by identity, never replaced.
     :capabilities {:leases [<lease> ...] ; OPTIONAL (default []). v0
                                          ; CapabilityLease values,
                                          ; schema-validated here
                                          ; (fail-fast, never at first
                                          ; dispatch).
                    :usage <atom>}        ; REQUIRED. Per-:cap/id call
                                          ; counts, referenced by
                                          ; identity.
     :program-sources {<program/id>       ; REQUIRED. The source text
                       <source-string>}   ; of EVERY program in
                                          ; :programs compiled-genome,
                                          ; decoded by the host from
                                          ; its Genome bundle (the
                                          ; CompiledGenome carries only
                                          ; :source/digest references,
                                          ; Global Constraint 22).
                                          ; Missing/non-string sources
                                          ; fail closed
                                          ; (:runtime/source-missing);
                                          ; extra sources are ignored.
     :stores ...}                         ; pass-through, never opened

  The stores live in runtime-deps and belong to the host: instantiate
  opens NO database connection, NO CAS handle, NO provider, and NO
  lease — the only mutable state construction creates is the Phenotype's
  own SCI runtime. halt! therefore releases nothing external: it marks
  the Phenotype halted and is idempotent; host-owned stores, providers,
  and capabilities are never touched by either function."
  (:require [evoclj.capability.schema :as capability-schema]
            [evoclj.compiler.core :as compiler-core]
            [evoclj.kernel.error :as err]
            [evoclj.sci.context :as context]
            [evoclj.sci.execute :as execute]))
;; --- shape validation -------------------------------------------------------

(def ^:private phenotype-id-pattern #"^sha256:[0-9a-f]{64}$")

(defn- validate-compiled!
  "Validate the CompiledGenome trust boundary: a map carrying a
  canonical :compiled/phenotype-id and a :programs map. Every failure
  throws :runtime/invalid-compiled with a distinguishing :reason."
  [compiled-genome]
  (when-not (map? compiled-genome)
    (throw (err/error :runtime/invalid-compiled
                      "instantiate expects a CompiledGenome map"
                      {:reason :not-a-map
                       :value (err/sanitize compiled-genome)})))
  (let [pid (:compiled/phenotype-id compiled-genome)]
    (when-not (and (string? pid)
                   (re-matches phenotype-id-pattern pid))
      (throw (err/error :runtime/invalid-compiled
                        "CompiledGenome must carry a canonical :compiled/phenotype-id"
                        {:reason :phenotype-id-invalid
                         :value (err/sanitize pid)}))))
  (when-not (map? (:programs compiled-genome))
    (throw (err/error :runtime/invalid-compiled
                      "CompiledGenome must carry a :programs map"
                      {:reason :programs-invalid
                       :value (err/sanitize (:programs compiled-genome))})))
  compiled-genome)

(defn- deps-error
  "A :runtime/deps-invalid ExceptionInfo with the distinguishing
  :reason and the sanitized offending value."
  [reason message value]
  (err/error :runtime/deps-invalid message {:reason reason
                                            :value (err/sanitize value)}))

(defn- validate-registry!
  "The provider registry atom must be present and be an atom."
  [providers]
  (when-not (map? providers)
    (throw (deps-error :providers-missing
                       "runtime-deps must carry a :providers map with a :registry atom"
                       providers)))
  (let [registry (:registry providers)]
    (when-not (instance? clojure.lang.Atom registry)
      (throw (deps-error :registry-not-atom
                         "runtime-deps :providers :registry must be a provider registry atom"
                         registry)))))

(defn- validate-capabilities!
  "The capabilities map must carry a usage atom and an optional lease
  collection; every lease is schema-validated here so garbage never
  survives construction (fail-fast, never at first dispatch)."
  [capabilities]
  (when-not (map? capabilities)
    (throw (deps-error :capabilities-missing
                       "runtime-deps must carry a :capabilities map"
                       capabilities)))
  (let [leases (or (:leases capabilities) [])
        usage (:usage capabilities)]
    (when-not (sequential? leases)
      (throw (deps-error :invalid-lease
                         "runtime-deps :capabilities :leases must be a collection of leases"
                         leases)))
    (when-not (instance? clojure.lang.Atom usage)
      (throw (deps-error (if (nil? usage) :usage-missing :usage-not-atom)
                         "runtime-deps :capabilities :usage must be a usage atom"
                         usage)))
    (doseq [lease leases]
      (try
        (capability-schema/validate-lease lease)
        (catch clojure.lang.ExceptionInfo e
          (throw (err/error :runtime/deps-invalid
                            "runtime-deps carries a malformed capability lease"
                            {:reason :invalid-lease
                             :cause (err/error-data e)})))))))

(defn- validate-sources!
  "The :program-sources map must be present; its coverage of every
  compiled program is checked by instantiate against the compiled
  :programs (each missing program is :runtime/source-missing)."
  [program-sources]
  (when-not (map? program-sources)
    (throw (deps-error :program-sources-missing
                       "runtime-deps must carry a :program-sources map"
                       program-sources))))

(defn- validate-stores!
  "The declared stores pass through untouched; only the map shape is
  checked. instantiate NEVER opens, coerces, or inspects store values
  (they belong to the host; the component executor opens them)."
  [stores]
  (when-not (or (nil? stores) (map? stores))
    (throw (deps-error :stores-invalid
                       "runtime-deps :stores must be a map of declared store handles"
                       stores))))

(defn- validate-deps!
  "Validate the runtime-deps trust boundary. Every failure throws
  :runtime/deps-invalid (see the namespace docstring for the :reason
  codes)."
  [{:keys [stores providers capabilities program-sources]}]
  (validate-stores! stores)
  (validate-registry! providers)
  (validate-capabilities! capabilities)
  (validate-sources! program-sources))

;; --- program loading --------------------------------------------------------

(defn- load-programs!
  "Load every program of the compiled :programs into a fresh isolated
  SCI runtime, in the deterministic sorted-map order of the compiled
  :programs. Each program's source is looked up in :program-sources; a
  missing or non-string source fails closed with
  :runtime/source-missing carrying the :program/id (extra sources are
  ignored). Returns the runtime map evoclj.sci.execute/load-program!
  built."
  [programs program-sources]
  (reduce-kv
   (fn [runtime program-id descriptor]
     (let [source (get program-sources program-id)]
       (when-not (string? source)
         (throw (err/error :runtime/source-missing
                           "no source declared in runtime-deps for a compiled program"
                           {:program/id program-id})))
       (execute/load-program! runtime descriptor source)))
   {:context (context/make-context {})
    :programs {}}
   programs))

;; --- public entry points ----------------------------------------------------

(defn instantiate
  "Construct a live Phenotype from a CompiledGenome and runtime-deps
  (component).

  `compiled-genome` is the pure CompiledGenome from
  evoclj.compiler.core/compile-genome (which already carries the
  canonical :compiled/phenotype-id, Global Constraint 22: the value is
  fully serializable EDN data). `runtime-deps` is the host-injected map
  documented in the namespace docstring (stores, provider registry,
  capability leases + usage, and the program source texts).

  Construction performs NO external effects: no database connection, no
  CAS handle, no provider registration, no lease issuance — the only
  mutable state created is the Phenotype's OWN isolated SCI runtime,
  built from a fresh closed context (evoclj.sci.context) and every
  compiled program loaded into it (evoclj.sci.execute/load-program!).
  Host-owned values (:providers :registry, :capabilities :usage) are
  referenced by identity, never copied or replaced, and the declared
  :stores pass through untouched.

  Returns:

    {:phenotype/id <:compiled/phenotype-id>
     :code/id (or (:compiled/code-id compiled-genome) (:compiled/phenotype-id compiled-genome))
     :deployment/id <derived DeploymentId from code-id, leases, bindings>
     :compiled <the SAME immutable CompiledGenome value>
     :sci-runtime <fresh isolated runtime map>
     :providers <host registry map, by reference>
     :capabilities <host leases + usage map, by reference>
     :stores <declared stores, by reference>}

  Two Phenotypes from one Genome share the immutable compiled
  code data while owning independent SCI contexts and distinct
  DeploymentIds when configured with different leases or bindings.
  Constraints 3, 22, 23).

  Throws ExceptionInfo with a stable :error/type:
  :runtime/invalid-compiled, :runtime/deps-invalid, or
  :runtime/source-missing (see the namespace docstring)."
  [compiled-genome runtime-deps]
  (validate-compiled! compiled-genome)
  (validate-deps! runtime-deps)
  (let [programs (:programs compiled-genome)
        cid (or (:compiled/code-id compiled-genome)
                (:compiled/phenotype-id compiled-genome))
        leases (or (get-in runtime-deps [:capabilities :leases]) [])
        bindings (or (get-in runtime-deps [:bindings]) [])
        did (compiler-core/deployment-id cid leases bindings)]
    {:phenotype/id cid
     :code/id cid
     :deployment/id did
     :compiled compiled-genome
     :sci-runtime (load-programs! programs (:program-sources runtime-deps))
     :providers (:providers runtime-deps)
     :capabilities (:capabilities runtime-deps)
     :stores (or (:stores runtime-deps) {})}))

(defn halt!
  "Release the resources OWNED by `phenotype` (component).

  In v0 a Phenotype owns exactly one in-memory resource — its isolated
  SCI runtime — and an SCI context has no OS handle to close; the
  stores, providers, and capabilities live in runtime-deps and belong
  to the host, so halt! never touches them (construction opens no
  resources beyond what runtime-deps declares, and halt! releases no
  more than construction opened).

  halt! is IDEMPOTENT: it marks the Phenotype halted (:halted? true,
  see halted?) and returns the updated Phenotype map; calling it again
  is a no-op returning the same value."
  [phenotype]
  (assoc phenotype :halted? true))

(defn halted?
  "True when `phenotype` has been halted by halt!."
  [phenotype]
  (true? (:halted? phenotype)))
