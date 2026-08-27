(ns evoclj.kernel.system
  "Host wiring for the STABLE components (component).

  This namespace completes the wiring plan begun in
  evoclj.runtime.system (component Step 4). The four stable lifecycle
  keys — :store/sqlite, :store/cas, :provider/registry,
  :capability/broker — are OWNED by evoclj.runtime.system, whose
  init-key / halt-key! methods are the single registration for those
  keys (re-registering them here would be order-dependent and
  non-deterministic). This namespace requires that wiring and adds the
  four Milestone 9 subsystems:

    :runtime/executor    scheduler host (stores + dispatch + run-session!)
    :evolution/system    evolution-system map (component)
    :eval/system         evaluator map (component)
    :promotion/system    promotion-system map (component)

  plus the MCP / dynamic-environment components:

    :modelsdev/catalog   models.dev catalog refresh result (post-v0 ext 1)
    :model/registry      kernel-owned model registry atom (post-v0 ext 1)
    :mcp/manager         shared MCP connection pool manager (WO-M5,
                         init/halt owned here via evoclj.mcp.manager;
                         see runtime/system for the thin defmethods)
    :mcp/source          the McpSource LiveSource behind the WO-M20 switch
                         (:enabled? false ships off; the legacy static
                         :mcp/bridge provider path is untouched)
    :environment/registry  the DYNAMIC ENVIRONMENT HOST component (WO-E6):
                         the EnvironmentRegistry (E1/E2/E4) every
                         host-created source registers into; halt tears it
                         down cleanly
    :skill/source        the SkillSource LiveSource behind the WO-E6
                         switch (:enabled? false ships off), registered
                         into :environment/registry like :mcp/source

  Because the stable keys keep evoclj.runtime.system's shapes, the
  host config follows those constructors exactly (see
  evoclj.runtime.system for :store/* and :capability/broker):

    :store/sqlite       <db path string | java.jdbc spec map>
    :store/cas          {:root <dir> :verify <boolean>}
    :provider/registry  {:providers [{:provider/type :fixture/echo} ...]}
    :capability/broker  {:registry #ig/ref :provider/registry :leases [...]}
    :runtime/executor   {:scheduler {:max-steps <n>}
                         :store {:sqlite #ig/ref :store/sqlite
                                 :cas #ig/ref :store/cas}
                         :dispatch #ig/ref :capability/broker}
    :evolution/system   {:store {...} :provider-catalog {...}
                         :genome-root <dir> | :genome-loader <fn>
                         :candidates-dir <dir> :diagnostician {...}
                         :mutator <fn> | :none :budget-profile {...}
                         :programs-registry [...]
                         ;; optional LLM-driven adapters (opt-in): a
                         ;; :diagnostician/:mutator {:type :llm ...} map is
                         ;; wired through a host-built :model-call closure
                         ;; that dispatches :intent/model-call through the
                         ;; injected :capability/broker. These three keys are
                         ;; OPTIONAL; pattern-only hosts omit them entirely.
                         :model/registry <registry>   ; host-injected model registry
                         :dispatch <broker context>   ; the :capability/broker value
                         :model-lease <optional lease map>}
    :eval/system        {:store {...} :provider/catalog {...}
                         :kernel/abi {...} :profiles {...}
                         :genome/roots {...} :dataset/roots {...}
                         :selection/cases {} :selection/fixtures {}
                         :replay/cases {} :replay/fixtures {}}
    :promotion/system   {:store {...}
                         :resolution/id <sha256 id> | :derive
                         :event/session-id <uuid> | :derive}

  Genome graph nodes are NEVER Integrant components (Global
  Constraints 22, 23): :node/* topology nodes, :program/* SCI
  programs, and :graph/* entries are per-Phenotype values constructed
  inside an isolated SCI runtime, never host components. The Phenotype
  is not an Integrant component either — the :runtime/executor
  component builds per-session executor maps from a phenotype via its
  :build entry.

  init (the host's single entry point) runs ig/init, then performs the
  two host-startup steps the component methods deliberately do not:
  bring the SQLite schema up to date (migrate!) and register the
  catalog providers into the :provider/registry atom (Global
  Constraint 19: the registry is kernel-owned). halt! is a thin
  ig/halt! wrapper; every halt-key! is an honest no-op, so halt! is
  idempotent — a second halt! is safe.

  Paths in the config are resolved relative to the system.edn file's
  own directory (or the value is used as-is when absolute), and every
  scalar path can be overridden by an EVOCLJ_* environment variable
  (see load-config) so a deployment can point the host at other
  locations. Tests never go through load-config: they build the config
  map directly with temp paths (dependency injection, Step 4)."
  (:require [clojure.java.io :as io]
            [evoclj.environment.registry :as env-reg]
            [evoclj.environment.source :as env-src]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.skill.adapter :as skill-adapter]
            [clojure.string :as str]
            [evoclj.eval.profile :as profile]
            [evoclj.evolution.budget :as budget]
            [evoclj.evolution.core :as evolution]
            [evoclj.evolution.diagnose :as diagnose]
            [evoclj.evolution.llm-diagnostician :as llm-diag]
            [evoclj.evolution.llm-mutator :as llm-mut]
            [evoclj.eval.judge :as judge]
            [evoclj.genome.hash :as hash]
            [evoclj.intent.core :as intent-core]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.memory :as memory]
            [evoclj.provider.model-registry :as model-registry]
            [evoclj.provider.modelsdev :as modelsdev]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.runtime.system]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [integrant.core :as ig])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; --- component keys ----------------------------------------------------------

(def store-sqlite-key :store/sqlite)
(def store-cas-key :store/cas)
(def provider-registry-key :provider/registry)
(def capability-broker-key :capability/broker)
(def runtime-executor-key :runtime/executor)
(def evolution-system-key :evolution/system)
(def eval-system-key :eval/system)
(def promotion-system-key :promotion/system)
(def modelsdev-catalog-key :modelsdev/catalog)
(def model-registry-key :model/registry)
(def environment-registry-key :environment/registry)
(def skill-source-key :skill/source)

(def host-component-keys
  "The normative Integrant-owned host component set (component plus
   post-v0 extension 1: the models.dev catalog and the model registry,
   plus the WO-M5 MCP manager pool and the WO-E6 dynamic environment
   host: the EnvironmentRegistry component and the SkillSource switch).
   Genome graph nodes are NOT in this set."
  [store-sqlite-key store-cas-key provider-registry-key
   capability-broker-key runtime-executor-key evolution-system-key
   eval-system-key promotion-system-key modelsdev-catalog-key
   model-registry-key environment-registry-key skill-source-key])

;; --- path resolution ---------------------------------------------------------

(defn- resolve-config-path
  "Resolve a config path value against `base` (the system.edn parent
  directory) unless it is already absolute, then normalize (collapsing
  any .. segments). Returns the absolute path as a string."
  [base x]
  (let [raw (cond
              (instance? Path x) x
              (instance? java.io.File x) (.toPath ^java.io.File x)
              :else (.toPath (io/file x)))
        resolved (if (or (nil? base) (.isAbsolute raw))
                   (.toAbsolutePath raw)
                   (.resolve base raw))]
    (str (.normalize resolved))))

(def ^:private env-overrides
  "Scalar path entries overridable by environment variables, as
  [config-path env-var] pairs. The host reads these at load-config
  time so a deployment can relocate stores without editing the file."
  [[[:store/sqlite] "EVOCLJ_DB_PATH"]
   [[:store/cas :root] "EVOCLJ_CAS_ROOT"]
   [[:evolution/system :genome-root] "EVOCLJ_GENOME_ROOT"]
   [[:evolution/system :candidates-dir] "EVOCLJ_CANDIDATES_DIR"]
   [[:modelsdev/catalog :url] "EVOCLJ_CATALOG_URL"]
   [[:modelsdev/catalog :cache-dir] "EVOCLJ_CATALOG_CACHE_DIR"]])

(defn- apply-env-overrides
  "Override scalar config entries from `env` (defaults to
  (System/getenv)). Returns the config unchanged when no override is
  set."
  [config env]
  (reduce (fn [cfg [path env-var]]
            (if-let [v (get env env-var)]
              (assoc-in cfg path v)
              cfg))
          config
          env-overrides))

(defn- resolve-map-paths
  "Resolve every value of the map at config-path `path` as a path
  against `base` (used for :eval/system :genome/roots and
  :dataset/roots, whose values are paths)."
  [config path base]
  (update-in config path
             (fn [m]
               (reduce-kv (fn [acc k v]
                            (assoc acc k (resolve-config-path base v)))
                          {} m))))

(defn load-config
  "Load the host configuration.

  Reads resources/system.edn from the classpath (the repo ships the
  default at resources/system.edn), resolves every relative path
  against the system.edn file's own directory, applies EVOCLJ_* env
  overrides (or those in `env` when supplied), and returns the config
  map ready for init. Absolute paths pass through unchanged.

  Relative map-valued path entries (:eval/system :genome/roots and
  :dataset/roots) resolve the same way. Returns nil when no
  system.edn is on the classpath."
  [& [env]]
  (when-let [url (io/resource "system.edn")]
    (let [config (ig/read-string (slurp url))
          base (some-> url io/as-file .getParentFile .toPath)]
      (-> config
          (apply-env-overrides (or env (System/getenv)))
          (update :store/sqlite #(resolve-config-path base %))
          (update-in [:store/cas :root]
                     #(resolve-config-path base %))
          (update-in [:evolution/system :genome-root]
                     #(when % (resolve-config-path base %)))
          (update-in [:evolution/system :candidates-dir]
                     #(resolve-config-path base %))
          (update-in [:promotion/system :genome-root]
                     #(when % (resolve-config-path base %)))
          (update-in [:modelsdev/catalog :cache-dir]
                     #(when % (resolve-config-path base %)))
          (resolve-map-paths [:eval/system :genome/roots] base)
          (resolve-map-paths [:eval/system :dataset/roots] base)))))

;; --- host startup --------------------------------------------------------------

(defn- normalize-path
  "Coerce x (string/File/Path) into an absolute Path."
  [x]
  (let [p (cond
            (instance? Path x) x
            (instance? java.io.File x) (.toPath ^java.io.File x)
            :else (.toPath (io/file x)))]
    (if (.isAbsolute p) p (.toAbsolutePath p))))

(defn- ensure-db-dir!
  "Host-startup step 0: create the SQLite file's parent directory from
  the raw config value (a path string or a java.jdbc spec map), so
  migration and per-operation connections can open the file. SQLite
  creates the FILE itself but never a missing DIRECTORY."
  [config]
  (let [db (:store/sqlite config)
        path (if (string? db) db (:subname db))]
    (when path
      (let [dir (.getParent (normalize-path path))]
        (when dir
          (Files/createDirectories dir (make-array FileAttribute 0))))))
  config)

(defn- provider-for
  "Construct a provider instance from a catalog entry
  {:provider/type <keyword>}, injecting the resolved :store/sqlite spec
  store so kernel providers that CLOSE OVER a store can be
  built (feature R1). v0 ships the fixture adapters
  (evoclj.provider.fixture); the type keyword names the constructor.
  WO-M5: the :mcp/manager component is injected into :mcp/bridge entries,
  so pooled MCP providers share ONE host-owned connection manager whose
  lifecycle halt! owns — the bridge's lazy fallback stays reserved for
  zero-config, non-Integrant use."
  [entry store mcp-manager]
  (let [type (:provider/type entry)
        opts (dissoc entry :provider/type)]
    (case type
      :fixture/echo (fixture/echo-provider opts)
      :fixture/non-idempotent (fixture/non-idempotent-provider opts)
      ;; :memory/kv closes over the SQLite spec so its store handle never
      ;; crosses the Provider protocol boundary (feature R1).
      :memory/kv (memory/memory-provider (assoc opts :store store))
      ;; MCP bridge: remote tool provider; the host-owned pool manager is
      ;; injected (WO-M5) so its stdio children die with the system.
      :mcp/bridge (mcp-bridge/mcp-provider
                   (cond-> opts
                     (some? mcp-manager) (assoc :mcp/manager mcp-manager)))
      (throw (err/error :provider/catalog-invalid
                        (str "unknown :provider/type " type)
                        {:provider/type type
                         :value (err/sanitize entry)})))))

(defn- migrate-schema!
  "Host-startup step 1: bring the SQLite schema up to date on the
  built :store/sqlite component (a java.jdbc spec). Idempotent."
  [system]
  (migrate/migrate! (:store/sqlite system))
  system)

(defn- register-catalog-providers!
  "Host-startup step 2: register every catalog provider from the raw
  config into the built :provider/registry atom. Registration is
  fail-closed: a malformed or duplicate entry throws and changes
  nothing (Global Constraint 19 — the registry is kernel-owned). The
  resolved :store/sqlite component is injected into catalog entries
  that name store-closing providers (:memory/kv, feature R1); the
  :mcp/manager component is injected into :mcp/bridge entries (WO-M5)."
  [system config]
  (let [store (:store/sqlite system)]
    (doseq [entry (get-in config [:provider/registry :providers])]
      (registry/register! (:provider/registry system)
                          (provider-for entry store (:mcp/manager system)))))
  system)

(defn init
  "Build the host system from `config`: ig/init (refs resolved,
  init-key methods build each component), then the two host-startup
  steps — schema migration and catalog provider registration. Returns
  the resolved component map. On a startup failure the partially built
  system is halted before the error propagates."
  [config]
  (ensure-db-dir! config)
  (let [system (ig/init config)]
    (try
      (-> system
          (migrate-schema!)
          (register-catalog-providers! config))
      (catch Throwable t
        (ig/halt! system)
        (throw t)))))

(defn halt!
  "Tear the host system down: (ig/halt! system) — which halts every
  Integrant component, including the :mcp/manager pool — then, for
  zero-config compatibility (WO-M5), also shuts down the bridge's LAZY
  fallback manager used by providers built WITHOUT an injected manager.
  Both steps are idempotent, so calling halt! twice stays safe. Returns
  nil (the ig/halt! contract pinned by evoclj.kernel.system-test)."
  [system]
  (ig/halt! system)
  ;; WO-M5: no-op unless some non-injected MCP provider realized the lazy
  ;; fallback; never lets teardown errors mask the halt outcome.
  (try (mcp-bridge/shutdown-pool!) (catch Throwable _ nil))
  nil)

;; --- :store/* and :capability/broker -------------------------------------------
;; Owned by evoclj.runtime.system (single registration, deterministic);
;; see that namespace for the init-key / halt-key! methods.

;; --- :runtime/executor ------------------------------------------------------------

(defmethod ig/init-key :runtime/executor
  [_ config]
  "Build the :runtime/executor component: the scheduler HOST — the
  stores, the broker context, and the scheduler entry point. The
  executor map the scheduler actually runs (component:
  {:phenotype ... :stores ... :dispatch ...}) is assembled per session
  by :build from a compiled Phenotype, because the Phenotype is
  constructed inside an isolated SCI runtime and is never a host
  component (Global Constraints 22, 23)."

  (let [limits (or (:scheduler config) {})]
    {:scheduler scheduler/run-session!
     :stores {:sqlite (:sqlite (:store config))
              :cas (:cas (:store config))}
     :dispatch (:dispatch config)
     :limits limits
     :build (fn [phenotype]
              {:phenotype phenotype
               :stores {:sqlite (:sqlite (:store config))
                        :cas (:cas (:store config))}
               :dispatch (:dispatch config)})}))

(defmethod ig/halt-key! :runtime/executor
  [_ _component]
  "The executor host is a plain map of functions and store handles;
  nothing to close."
  nil)

;; --- :evolution/system ------------------------------------------------------------

(defn- no-op-mutator
  "The v0 default Mutator adapter: proposes nothing. A host that has
  not registered a mutator never materializes candidates (YAGNI, Global
  Constraint 24); tests inject real/fake adapters through the config."
  []
  (reify evolution/Mutator
    (propose-mutations [_ _context] nil)))

(defn- build-diagnostician
  "Build the Diagnostician from config:
    - an object already satisfying the Diagnostician protocol passes
      through unchanged (dependency injection);
    - a plain map becomes the deterministic pattern adapter
      (evoclj.evolution.diagnose);
    - a {:type :llm ...} map becomes the LLM adapter
      (evoclj.evolution.llm-diagnostician) closed over the host-built
      :model-call closure. An unknown :type or an invalid :llm config
      fails closed (:evolution/system-invalid)."
  [config model-call]
  (cond
    (and (map? config) (= :llm (:type config)))
    (let [allowed #{:type :model/id :max-hypotheses :confidence-band :system-prompt}
          unknown (remove allowed (keys config))]
      (when (seq unknown)
        (throw (err/error :evolution/system-invalid
                          "invalid :diagnostician :type :llm config — unknown keys"
                          {:reason :llm-config-invalid
                           :keys (mapv (comp str name) unknown)})))
      (llm-diag/llm-diagnostician
       (cond-> {:model-call model-call
                :model/id (:model/id config)}
         (:max-hypotheses config) (assoc :max-hypotheses (:max-hypotheses config))
         (contains? config :confidence-band)
         (assoc :confidence-band (:confidence-band config))
         (:system-prompt config) (assoc :system-prompt (:system-prompt config)))))
    (and (map? config) (contains? config :type))
    (throw (err/error :evolution/system-invalid
                      "unknown :diagnostician :type"
                      {:reason :unknown-diagnostician-type
                       :type (:type config)}))
    (satisfies? diagnose/Diagnostician config) config
    :else (diagnose/pattern-diagnostician config)))

(defn- build-mutator
  "Build the Mutator from config:
    - nil / :none yield the no-op adapter (v0 default);
    - an object already satisfying the Mutator protocol passes through
      unchanged (dependency injection — e.g. a DefaultMutator record);
      this branch is checked BEFORE the map branch because records ARE
      maps (a record would otherwise fall into the map handling and be
      rejected as an unknown :type);
    - a function passes through (wrapped into the protocol);
    - an EMPTY map means nothing configured — like :none, it yields
      the no-op adapter;
    - a NON-EMPTY map must be a {:type :llm ...} config and becomes
      the LLM adapter (evoclj.evolution.llm-mutator) closed over the
      host-built :model-call closure; a missing or unknown :type fails
      closed (:evolution/system-invalid)."
  [config model-call]
  (cond
    (nil? config) (no-op-mutator)
    (= :none config) (no-op-mutator)
    (fn? config) (reify evolution/Mutator
                   (propose-mutations [_ context]
                     (config context)))
    (satisfies? evolution/Mutator config) config
    (and (map? config) (empty? config)) (no-op-mutator)
    (map? config)
    (if (= :llm (:type config))
      (let [allowed #{:type :model/id :max-mutations :risk :system-prompt}
            unknown (remove allowed (keys config))]
        (when (seq unknown)
          (throw (err/error :evolution/system-invalid
                            "invalid :mutator :type :llm config — unknown keys"
                            {:reason :llm-config-invalid
                             :keys (mapv (comp str name) unknown)})))
        (llm-mut/llm-mutator
         (cond-> {:model-call model-call
                  :model/id (:model/id config)}
           (:max-mutations config) (assoc :max-mutations (:max-mutations config))
           (contains? config :risk) (assoc :risk (:risk config))
           (:system-prompt config) (assoc :system-prompt (:system-prompt config)))))
      (throw (err/error :evolution/system-invalid
                        "unknown :mutator :type"
                        {:reason :unknown-mutator-type
                         :type (:type config)})))
    :else (throw (err/error :evolution/system-invalid
                            "host :mutator must be a fn, :none, absent, or a {:type :llm ...} map"
                            {:value (err/sanitize config)}))))

(defn- build-model-call
  "Build the host-injected :model-call closure used by the LLM
  evolution adapters: ONE attribute :intent/model-call dispatched
  through a LOCAL broker context (the injected :capability/broker
  value, with the model registry and lease injected locally — the host
  broker context is NEVER mutated).

  Attribution is kernel-deterministic (Global Constraint 20 — every
  externally visible effect is auditable, never random):
    - a fixed session id over \"evoclj/evolution/session\";
    - a deterministic content-addressed phenotype id derived from
      \"evoclj/evolution\" (satisfies the intent PhenotypeIdSchema);
    - the :node/evolution node and a 0 cause/event-id.

  Contract (returned to the adapters): the dispatch result when
  :result/status is :ok; otherwise a thrown ExceptionInfo with a stable
  :error/type (the adapters' \"throws ExceptionInfo\" contract). EDN-safe
  only — all error data is sanitized (Global Constraint 22)."
  [dispatch-context model-registry model-lease prefix]
  (let [session-id (UUID/nameUUIDFromBytes
                    (.getBytes (str prefix "/session") StandardCharsets/UTF_8))
        phenotype-id (hash/text-digest prefix)
        local-ctx (assoc dispatch-context
                         :model-registry model-registry
                         :leases (if model-lease
                                   (conj (:leases dispatch-context) model-lease)
                                   (:leases dispatch-context)))]
    (fn [model-id messages options]
      (let [intent (intent-core/model-call
                    session-id phenotype-id :node/evolution 0
                    {:model/id model-id
                     :messages messages
                     :options options}
                    {:wall-ms 1000 :max-steps 1})
            result (dispatch/dispatch! local-ctx intent)]
        (if (= :ok (:result/status result))
          result
          (throw (err/error :evolution/model-call-failed
                            "model call failed during evolution"
                            {:error/type (:result/status result)
                             :error/message (:error/message result)
                             :error/data (err/sanitize (:error/data result))})))))))

(defmethod ig/init-key :evolution/system
  [_ config]
  "Build the :evolution/system component: an evolution-system map
  (component contract, see evoclj.evolution.core) assembled from the
  config subtree and the injected store. The provider catalog is
  plain data; the diagnostician and mutator are constructed here
  (or injected as objects/fns — Step 4). The :mutator accepts a
  Mutator protocol object (passed through unchanged), a fn (wrapped
  into the protocol), :none / absent / an EMPTY map (nothing
  configured — the no-op adapter), or a {:type :llm ...} map; a
  non-empty map without a known :type fails closed
  (:evolution/system-invalid).

  OPTIONAL LLM-DRIVEN ADAPTERS (opt-in): when :diagnostician or
  :mutator is a {:type :llm ...} map, the host builds ONCE a :model-call
  closure (build-model-call) closed over :model/registry, :dispatch
  (the :capability/broker value) and the optional :model-lease, and
  wires it into both adapters. When an :llm adapter is configured but
  :model/registry or :dispatch is missing, the host fails closed
  (:evolution/system-invalid — never silently falls back to the pattern
  adapter or the no-op mutator). These three config keys are OPTIONAL
  and only consulted when an :llm adapter is present."
  (let [diagnostician-config (:diagnostician config)
        mutator-config (:mutator config)
        llm? (or (and (map? diagnostician-config)
                      (= :llm (:type diagnostician-config)))
                 (and (map? mutator-config)
                      (= :llm (:type mutator-config))))
        model-call (when llm?
                     (do
                       (when-not (contains? config :model/registry)
                         (throw (err/error :evolution/system-invalid
                                           "an :llm evolution adapter requires :model/registry"
                                           {:reason :llm-needs-model-registry})))
                       (when-not (contains? config :dispatch)
                         (throw (err/error :evolution/system-invalid
                                           "an :llm evolution adapter requires :dispatch"
                                           {:reason :llm-needs-dispatch})))
                       (build-model-call (:dispatch config)
                                         (:model/registry config)
                                         (:model-lease config)
                                         "evoclj/evolution")))
        evo (cond-> {:store {:sqlite (:sqlite (:store config))
                             :cas (:cas (:store config))}
                     :provider-catalog (or (:provider-catalog config) {})
                     :candidates-dir (:candidates-dir config)
                     :diagnostician (build-diagnostician diagnostician-config model-call)
                     :mutator (build-mutator mutator-config model-call)
                     :budget-profile (or (:budget-profile config)
                                         budget/v0-profile)}
              (:genome-root config) (assoc :genome-root (:genome-root config))
              (:genome-loader config) (assoc :genome-loader (:genome-loader config))
              (contains? config :programs-registry)
              (assoc :programs-registry (:programs-registry config)))]
    evo))

(defmethod ig/halt-key! :evolution/system
  [_ _component]
  "The evolution-system is a plain data map; nothing to close."
  nil)

;; --- :eval/system ------------------------------------------------------------------

(defmethod ig/init-key :eval/system
  [_ config]
  "Build the :eval/system component: an evaluator map (component
  contract, see evoclj.eval.core) assembled from the config subtree
  and the injected store. Fixture maps (:selection/fixtures,
  :replay/fixtures) default to empty — v0 ships no hidden fixtures;
  the host injects fixture fns where a deployment has them.

  OPTIONAL REAL MODEL EXECUTION for :llm topologies: when the config
  carries :model/registry (the kernel-owned model registry atom,
  result of evoclj.provider.model-registry/build-model-registry) and
  :model/resource (the model lease resource template, e.g. {:kind
  :model :id \"lmstudio/*\"}), both are passed through onto the
  evaluator map, letting the G5 runner evaluate :llm-topology
  candidates against real providers. Both keys are OPTIONAL — absent,
  the shipped behavior is unchanged and an :llm genome fails closed
  with :provider/not-found :reason :no-model-registry.

  OPTIONAL LLM-AS-JUDGE (feature V1): when the config carries a :judge
  {:type :llm :model/id <string> :system-prompt <optional> :max-tokens
  <optional>} map, the host builds a model-call closure (attributed to
  \"evoclj/eval-judge\") and registers an LLM equivalence judge under
  :equivalence/llm-judge in the evaluator's :equivalence/by-keyword
  (the keyword a selection case declares via :output/equiv?). :judge
  requires :model/registry and :dispatch in the config — missing
  either fails closed with :eval/system-invalid. Absent :judge leaves
  the equivalence registry empty (shipped behavior)."
  (let [judge-config (:judge config)
        judge-registry
        (when (and (map? judge-config) (= :llm (:type judge-config)))
          (let [allowed #{:type :model/id :system-prompt :max-tokens}
                unknown (remove allowed (keys judge-config))]
            (when (seq unknown)
              (throw (err/error :eval/system-invalid
                                "invalid :judge :type :llm config — unknown keys"
                                {:reason :judge-config-invalid
                                 :keys (mapv (comp str name) unknown)})))
            (when-not (contains? config :model/registry)
              (throw (err/error :eval/system-invalid
                                "an :llm judge requires :model/registry"
                                {:reason :judge-needs-model-registry})))
            (when-not (contains? config :dispatch)
              (throw (err/error :eval/system-invalid
                                "an :llm judge requires :dispatch"
                                {:reason :judge-needs-dispatch})))
            (let [model-call (build-model-call (:dispatch config)
                                               (:model/registry config)
                                               (:model-lease config)
                                               "evoclj/eval-judge")
                  j (judge/llm-judge
                     (cond-> {:model-call model-call
                              :model/id (:model/id judge-config)}
                       (:system-prompt judge-config)
                       (assoc :system-prompt (:system-prompt judge-config))
                       (contains? judge-config :max-tokens)
                       (assoc :max-tokens (:max-tokens judge-config))))]
              (judge/merge-judge
               (or (:equivalence/by-keyword config) {})
               j))))]
    (cond-> {:store {:sqlite (:sqlite (:store config))
                     :cas (:cas (:store config))}
             :provider/catalog (or (:provider/catalog config) {})
             :kernel/abi (:kernel/abi config)
             :profiles (or (:profiles config) {"default-v1" profile/default-v1})
             :genome/roots (or (:genome/roots config) {})
             :dataset/roots (:dataset/roots config)
             :selection/cases (or (:selection/cases config) {})
             :selection/fixtures (or (:selection/fixtures config) {})
             :replay/cases (or (:replay/cases config) {})
             :replay/fixtures (or (:replay/fixtures config) {})}
      judge-registry (assoc :equivalence/by-keyword judge-registry)
      (contains? config :model/registry)
      (assoc :model/registry (:model/registry config))
      (contains? config :model/resource)
      (assoc :model/resource (:model/resource config)))))

(defmethod ig/halt-key! :eval/system
  [_ _component]
  "The evaluator is a plain data map; nothing to close."
  nil)

;; --- :promotion/system ---------------------------------------------------------------

(def ^:private seed-route-descriptor
  "The v0 seed route program descriptor (component choice (a)): the
  seed topology's :program/route entry point, carried on the loaded
  Genome under :programs. This is stable host bootstrap knowledge of
  the immutable seed bundle (genomes/seed)."
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- derive-resolution-id
  "Compile the seed Genome (from THIS subtree's :genome-root and
  :provider-catalog, or an injected :genome-loader) to derive the
  current generation's Resolution id — the value :promotion/system
  must pin. Used only when the config says :derive; tests always
  inject the id explicitly."
  [config]
  (let [loader (or (:genome-loader config)
                   (fn [] (-> (evoclj.genome.load/load-genome
                               (:genome-root config))
                              (assoc :programs [seed-route-descriptor]))))
        compiled (evoclj.compiler.core/compile-genome
                  (loader) (:provider-catalog config))]
    (:compiled/resolution-id compiled)))

(defmethod ig/init-key :promotion/system
  [_ config]
  "Build the :promotion/system component: a promotion-system map (component contract, see evoclj.promotion.promote) with the injected store.
  :resolution/id names the current generation's compiled Resolution
  (config value or :derive — derived by compiling the seed Genome);
  :event/session-id anchors :promotion/* events (config value or a
  fresh host operator session uuid; the component CLI overrides it
  with the real operator session)."
  {:store {:sqlite (:sqlite (:store config))
           :cas (:cas (:store config))}
   :resolution/id (if (= :derive (:resolution/id config))
                    (derive-resolution-id config)
                    (:resolution/id config))
   :event/session-id (if (= :derive (:event/session-id config))
                       (str (UUID/randomUUID))
                       (:event/session-id config))})

(defmethod ig/halt-key! :promotion/system
  [_ _component]
  "The promotion-system is a plain data map; nothing to close."
  nil)

;; --- :modelsdev/catalog and :model/registry (post-v0 extension 1) --------------

(defn- catalog-config
  "Translate the component config subtree (bare keys, matching the
  system.edn style of the other components) into the namespaced
  :catalog/* config the catalog service validates."
  [config]
  (into {}
        (filter (fn [[_ v]] (some? v)))
        {:catalog/url (:url config)
         :catalog/cache-dir (:cache-dir config)
         :catalog/ttl-hours (or (:ttl-hours config) 24)
         :catalog/timeout-ms (or (:timeout-ms config) 30000)
         :catalog/base-urls (:base-urls config)
         :catalog/style-overrides (:style-overrides config)
         :catalog/dialect-overrides (:dialect-overrides config)}))

(defmethod ig/init-key :modelsdev/catalog
  [_ config]
  "Build the :modelsdev/catalog component: refresh the models.dev
  catalog at startup (the operator requirement — the catalog is
  auto-updated on every startup, cached under the state dir, and the
  cached copy is used when the network is unavailable). The component
  value is the refresh result {:catalog/status ... :catalog/data ...}."
  (modelsdev/refresh-catalog! (catalog-config config)))

(defmethod ig/halt-key! :modelsdev/catalog
  [_ _component]
  "The catalog result is plain data; nothing to close."
  nil)

(defmethod ig/init-key :model/registry
  [_ config]
  "Build the :model/registry component: the kernel-owned model
  registry atom (post-v0 extension 1) from the catalog data and the
  :registry/api-keys config. The catalog component must be injected
  as :catalog."
  (let [catalog (:catalog config)
        index (get-in catalog [:catalog/data :catalog/models] {})]
    (model-registry/build-model-registry
     index
     (dissoc config :catalog))))

(defmethod ig/halt-key! :model/registry
  [_ _component]
  "The registry is a host atom; nothing to close."
  nil)

;; --- WO-E6 injected-registry guard ---------------------------------------------

(defn- ensure-injected-env-registry!
  "WO-E6 fail-closed guard over an INJECTED :environment/registry component
   value (the resolved #ig/ref). Returns the validated registry, or nil when
   the injection is optional and absent.

     - absent and `required?` -> typed :environment/registry-required
       (:skill/source requires the registry: registration into the dynamic
       environment host IS the component's purpose);
     - present but MALFORMED (anything that is not a registry atom built by
       evoclj.environment.registry/create-registry) -> typed
       :environment/invalid-registry for BOTH source kinds; a malformed
       injection must never surface as an untyped ClassCastException from a
       deep swap!."
  [component-kw value required?]
  (cond
    (nil? value)
    (when required?
      (throw (err/error :environment/registry-required
                        (str component-kw
                             " requires an injected :environment/registry (#ig/ref :environment/registry) to register its source into")
                        {:component component-kw})))
    (not (env-reg/valid-registry? value))
    (throw (err/error :environment/invalid-registry
                      (str component-kw
                           " received a malformed :environment/registry injection — it must be the registry built by evoclj.environment.registry/create-registry")
                      {:component component-kw
                       :value-class (some-> value class .getName)}))
    :else value))

;; --- :mcp/source (M20) ---------------------------------------------------------
;;
;; The MCP dynamic-environment source as a real Integrant component, behind a
;; SWITCH. The :mcp/source config carries :enabled?; the shipped
;; resources/system.edn leaves it :enabled? false (fail-safe), so the host
;; starts WITHOUT an McpSource and the legacy static MCP path (:mcp/bridge
;; providers in :provider/registry) is completely untouched. Flipping the
;; switch to true makes the production system instantiate a real
;; evoclj.mcp.source/McpSource via its own constructor (no test-only seams):
;;   - :source/id and :transport-config are REQUIRED when enabled; a missing
;;     value fails closed with :mcp/config-invalid (the host never starts
;;     with a broken source);
;;   - :manager may be injected (e.g. #ig/ref :mcp/manager) so the source
;;     shares the host-owned connection pool;
;;   - :mcp/server-id tags the discovered tools' composite [server remote]
;;     tool-ids (M12).

(defmethod ig/init-key :mcp/source
  [_ config]
  "Build the :mcp/source component: a production McpSource LiveSource when the
   switch is enabled. When :enabled? is false (the shipped default) the
   component yields nil and the system starts with no McpSource.

   INV-05 — no duplicate fail-closed logic: the :source/id and
   :transport-config REQUIRED checks are enforced by the SINGLE ownership
   point evoclj.mcp.source/make-mcp-source (it throws :mcp/config-invalid for
   either missing value). This init-key only forwards the config to the
   constructor; it does NOT re-assert those invariants. A missing value
   therefore fails closed with :mcp/config-invalid via make-mcp-source, never
   via this component.

   WO-E6 — REGISTRATION into the dynamic environment host: when the config
   ALSO carries an injected :environment/registry (#ig/ref), the built
   McpSource is REGISTERED into it via register-source!, which subscribes
   the source's invalidate callback THROUGH the registry (for an McpSource
   that callback itself routes through the shared :mcp/manager per M17, so
   tools-changed propagates: source trigger -> manager publish! -> registry
   refresh!). The injection is OPTIONAL: absent, the source runs
   UNREGISTERED and the pre-E6 behavior is preserved bit-for-bit (the M20
   fallback path). Present but MALFORMED (anything that is not a registry
   built by evoclj.environment.registry/create-registry) fails closed typed
   with :environment/invalid-registry. The component VALUE stays the bare
   McpSource record — the M20 contract is unchanged; teardown of the
   registry-side subscription belongs to the :environment/registry halt-key!,
   which runs AFTER this component halts (Integrant halts dependents first)."
  (when (:enabled? config)
    ;; WO-E6: validate the OPTIONAL registry injection BEFORE building so a
    ;; malformed value never half-builds a source.
    (let [env-registry (ensure-injected-env-registry!
                        :mcp/source (:environment/registry config) false)
          source (mcp-source/make-mcp-source
                  (cond-> {:source/id (:source/id config)
                           :transport-config (:transport-config config)
                           :mcp/server-id (:mcp/server-id config)}
                    (:manager config) (assoc :manager (:manager config))
                    (:connection/id config) (assoc :connection/id (:connection/id config))
                    (:discover-fn config) (assoc :discover-fn (:discover-fn config))))]
      ;; Register + subscribe through the dynamic environment host when one
      ;; was injected (register-source! stores the per-source entry AND the
      ;; subscription handle in the registry).
      (when env-registry
        (env-reg/register-source! env-registry source))
      source)))

(defmethod ig/halt-key! :mcp/source
  [_ source]
  "Close the McpSource if one was built (it is nil when the switch was off).
   The manager it shares is owned by :mcp/manager and is closed there."
  (when source
    (try (env-src/close! source)
         (catch Throwable _ nil)))
  nil)

;; --- :environment/registry and :skill/source (WO-E6) ----------------------------
;;
;; WO-E6 — the dynamic environment host becomes a real Integrant component.
;;
;;   :environment/registry  the EnvironmentRegistry (E1 per-source state, E2
;;     single-transaction publication, E4 snapshot/pin) built by the ONE
;;     constructor evoclj.environment.registry/create-registry. Host-created
;;     sources register INTO it (see :mcp/source above and :skill/source
;;     below), so their invalidation callbacks subscribe THROUGH it:
;;       source trigger -> (manager publish! for MCP) -> registry refresh!
;;     halt-key! tears it down cleanly and idempotently via the registry's
;;     own shutdown! (closes every held source-subscription handle — which
;;     for an McpSource unsubscribes its M17 callback from the shared
;;     manager — drops listeners, resets publication state).
;;
;;   :skill/source  an OPTIONAL switch (the M20 pattern: fail-safe shipped
;;     default). When :enabled? true the host builds a REAL SkillSource via
;;     its production constructor make-skill-source (:cas REQUIRED there —
;;     single enforcement point, INV-05) over the host's :store/cas, with
;;     the host :environment/registry as BOTH the registration target and
;;     the record's own registry, then registers it. A missing injection
;;     fails closed typed (:environment/registry-required); a malformed one
;;     fails closed typed (:environment/invalid-registry). When disabled or
;;     absent nothing is built and nothing is registered — ad-hoc/static
;;     Skill construction elsewhere stays untouched as the fallback.

(defmethod ig/init-key :environment/registry
  [_ _config]
  "Build the :environment/registry component: a fresh EnvironmentRegistry
   atom (evoclj.environment.registry/create-registry) — the dynamic
   environment host every system-created LiveSource registers into. The
   config subtree carries no options in v0; the component value IS the
   registry atom consumers already accept (E1 refresh!, E4 pin!,
   bundle/publish-bundle!, skill catalog readers)."
  (env-reg/create-registry))

(defmethod ig/halt-key! :environment/registry
  [_ registry]
  "Tear the dynamic environment host down CLEANLY (idempotent): close every
   held source-subscription handle (for a registered McpSource this removes
   its M17 invalidate callback from the shared manager), drop listeners, and
   reset the publication state — via evoclj.environment.registry/shutdown!.
   Runs AFTER the source components halt (Integrant halts dependents first),
   so no live source calls back into a dead registry. Returns nil."
  (when registry
    (env-reg/shutdown! registry))
  nil)

(defmethod ig/init-key :skill/source
  [_ config]
  "Build the :skill/source component: a production SkillSource LiveSource
   when the switch is enabled. When :enabled? is false (the SHIPPED DEFAULT,
   fail-safe) the component yields nil, nothing registers, and existing
   ad-hoc/static skill wiring is completely untouched.

   When enabled:
     - the injected :environment/registry (#ig/ref) is REQUIRED — a missing
       value fails closed typed :environment/registry-required, a malformed
       one :environment/invalid-registry (single guard:
       ensure-injected-env-registry!). Registration into the dynamic
       environment host is this component's purpose; a private orphan
       registry would silently defeat it;
     - the source is built by the SINGLE production constructor
       evoclj.skill.adapter/make-skill-source, which owns the :cas REQUIRED
       check (:skill/invalid-opts — INV-05: no duplicated validation here);
       the host registry is injected as the record's :registry too, so the
       registered target and the record's attached registry are ONE atom;
     - register-source! stores the per-source entry AND subscribes the
       invalidate callback through the registry: filesystem event ->
       source trigger -> registry refresh! -> published SurfaceBundles.

   The component VALUE is the bare SkillSource record (the M20 shape
   convention). Its teardown runs at THIS key's halt (env-src/close!);
   the registry-side subscription handle is closed by the
   :environment/registry halt-key!, which Integrant runs afterwards."
  (when (:enabled? config)
    (let [env-registry (ensure-injected-env-registry!
                        :skill/source (:environment/registry config) true)
          source (skill-adapter/make-skill-source
                  (cond-> {:source/id (:source/id config)
                           :cas (:cas config)
                           ;; ONE registry: the record's attached registry is
                           ;; the SAME atom the source registers into.
                           :registry env-registry}
                    (:roots config) (assoc :roots (:roots config))
                    (:extra-roots config) (assoc :extra-roots (:extra-roots config))
                    (contains? config :strict?) (assoc :strict? (:strict? config))))]
      (env-reg/register-source! env-registry source)
      source)))

(defmethod ig/halt-key! :skill/source
  [_ source]
  "Close the SkillSource if one was built (it is nil when the switch was
   off). The CAS handle it reads through is owned by :store/cas; the
   registry-side subscription is closed by :environment/registry's halt."
  (when source
    (try (env-src/close! source)
         (catch Throwable _ nil)))
  nil)
