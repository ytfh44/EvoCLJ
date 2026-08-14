(ns evoclj.kernel.system
  "Host wiring for the STABLE components (Task 10.1).

  This namespace completes the wiring plan begun in
  evoclj.runtime.system (Task 6.1 Step 4). The four stable lifecycle
  keys — :store/sqlite, :store/cas, :provider/registry,
  :capability/broker — are OWNED by evoclj.runtime.system, whose
  init-key / halt-key! methods are the single registration for those
  keys (re-registering them here would be order-dependent and
  non-deterministic). This namespace requires that wiring and adds the
  four Milestone 9 subsystems:

    :runtime/executor    scheduler host (stores + dispatch + run-session!)
    :evolution/system    evolution-system map (Task 7.8)
    :eval/system         evaluator map (Task 8.7)
    :promotion/system    promotion-system map (Task 9.2)

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
                         :programs-registry [...]}
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
            [clojure.string :as str]
            [evoclj.eval.profile :as profile]
            [evoclj.evolution.budget :as budget]
            [evoclj.evolution.core :as evolution]
            [evoclj.evolution.diagnose :as diagnose]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.model-registry :as model-registry]
            [evoclj.provider.modelsdev :as modelsdev]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.runtime.system]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [integrant.core :as ig])
  (:import (java.nio.file Files Path)
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

(def host-component-keys
  "The normative Integrant-owned host component set (Task 10.1 plus
  post-v0 extension 1: the models.dev catalog and the model
  registry). Genome graph nodes are NOT in this set."
  [store-sqlite-key store-cas-key provider-registry-key
   capability-broker-key runtime-executor-key evolution-system-key
   eval-system-key promotion-system-key modelsdev-catalog-key
   model-registry-key])

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
  {:provider/type <keyword>}. v0 ships the fixture adapters
  (evoclj.provider.fixture); the type keyword names the constructor."
  [entry]
  (let [type (:provider/type entry)
        opts (dissoc entry :provider/type)]
    (case type
      :fixture/echo (fixture/echo-provider opts)
      :fixture/non-idempotent (fixture/non-idempotent-provider opts)
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
  nothing (Global Constraint 19 — the registry is kernel-owned)."
  [system config]
  (doseq [entry (get-in config [:provider/registry :providers])]
    (registry/register! (:provider/registry system) (provider-for entry)))
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
  "Tear the host system down: (ig/halt! system). Every halt-key! is
  an honest no-op, so halt! is idempotent — calling it twice is safe."
  [system]
  (ig/halt! system))

;; --- :store/* and :capability/broker -------------------------------------------
;; Owned by evoclj.runtime.system (single registration, deterministic);
;; see that namespace for the init-key / halt-key! methods.

;; --- :runtime/executor ------------------------------------------------------------

(defmethod ig/init-key :runtime/executor
  [_ config]
  "Build the :runtime/executor component: the scheduler HOST — the
  stores, the broker context, and the scheduler entry point. The
  executor map the scheduler actually runs (Task 6.3:
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
  "Build the Diagnostician from config: a plain map becomes the
  deterministic pattern adapter (evoclj.evolution.diagnose), an object
  that already satisfies the protocol passes through (dependency
  injection)."
  [config]
  (if (satisfies? diagnose/Diagnostician config)
    config
    (diagnose/pattern-diagnostician config)))

(defn- build-mutator
  "Build the Mutator from config: a function passes through, :none (or
  absence) yields the no-op adapter, anything else is a config error."
  [config]
  (cond
    (nil? config) (no-op-mutator)
    (= :none config) (no-op-mutator)
    (fn? config) (reify evolution/Mutator
                   (propose-mutations [_ context]
                     (config context)))
    :else (throw (err/error :evolution/system-invalid
                            "host :mutator must be a fn, :none, or absent"
                            {:value (err/sanitize config)}))))

(defmethod ig/init-key :evolution/system
  [_ config]
  "Build the :evolution/system component: an evolution-system map
  (Task 7.8 contract, see evoclj.evolution.core) assembled from the
  config subtree and the injected store. The provider catalog is
  plain data; the diagnostician and mutator are constructed here
  (or injected as objects/fns — Step 4)."
  (let [evo (cond-> {:store {:sqlite (:sqlite (:store config))
                             :cas (:cas (:store config))}
                     :provider-catalog (or (:provider-catalog config) {})
                     :candidates-dir (:candidates-dir config)
                     :diagnostician (build-diagnostician (:diagnostician config))
                     :mutator (build-mutator (:mutator config))
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
  "Build the :eval/system component: an evaluator map (Task 8.7
  contract, see evoclj.eval.core) assembled from the config subtree
  and the injected store. Fixture maps (:selection/fixtures,
  :replay/fixtures) default to empty — v0 ships no hidden fixtures;
  the host injects fixture fns where a deployment has them."
  {:store {:sqlite (:sqlite (:store config))
           :cas (:cas (:store config))}
   :provider/catalog (or (:provider/catalog config) {})
   :kernel/abi (:kernel/abi config)
   :profiles (or (:profiles config) {"default-v1" profile/default-v1})
   :genome/roots (or (:genome/roots config) {})
   :dataset/roots (:dataset/roots config)
   :selection/cases (or (:selection/cases config) {})
   :selection/fixtures (or (:selection/fixtures config) {})
   :replay/cases (or (:replay/cases config) {})
   :replay/fixtures (or (:replay/fixtures config) {})})

(defmethod ig/halt-key! :eval/system
  [_ _component]
  "The evaluator is a plain data map; nothing to close."
  nil)

;; --- :promotion/system ---------------------------------------------------------------

(def ^:private seed-route-descriptor
  "The v0 seed route program descriptor (Task 2.3 choice (a)): the
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
  "Build the :promotion/system component: a promotion-system map (Task
  9.2 contract, see evoclj.promotion.promote) with the injected store.
  :resolution/id names the current generation's compiled Resolution
  (config value or :derive — derived by compiling the seed Genome);
  :event/session-id anchors :promotion/* events (config value or a
  fresh host operator session uuid; the Task 10.2 CLI overrides it
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
