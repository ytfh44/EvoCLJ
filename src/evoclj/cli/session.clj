(ns evoclj.cli.session
  "CLI host building and the session-facing commands (component):
  `run`, `replay`, `events`, and `capability inspect`.

  This namespace owns the CLI's HOST BUILDING (shared by all cli
  sub-namespaces): the state-dir layout, the config assembly (with the
  :overrides injection seam), and kernel.system/init. It also owns the
  CLI's ONLY raw SQL — a small set of READ-ONLY SELECTs (generations
  and candidates lookups) documented below; there is no write SQL and
  no raw JDBC anywhere in the cli layer (the by-construction
  guarantee of component Step 2).

  THE STATE-DIR LAYOUT (normative for the CLI; a real deployment
  provisions it the same way the tests do):

      <state-dir>/db/evoclj.db        SQLite store
      <state-dir>/cas/                content-addressed store
      <state-dir>/genomes/<id-as-dash>  immutable Genome bundles, one
                                        directory per content address
                                        ('sha256:' -> 'sha256-')
      <state-dir>/candidates/         evolution candidates-dir (component
                                        finalize output)
      <state-dir>/evals/evolution|selection|audit   dataset roots

  HOST CONFIG (`build-config`): the config mirrors resources/system.edn
  with every path rooted at the state dir. `:overrides` (a map of
  config-subtree -> map) is deep-merged over the base so hosts and
  tests inject what v0 ships empty: the evaluator's hidden selection/
  replay cases and fixture providers, an evolution :mutator adapter,
  and so on — the CLI itself ships no cases and no mutator (YAGNI,
  Global Constraint 24). component adds ONE built-in exception: the
  :demo profile injects the demo heuristic Mutator and the demo's
  hidden selection cases/fixtures through the SAME :overrides seam, so
  a fresh state dir + the :demo profile runs the whole demo loop
  headless.

  PUBLIC-ONLY MUTATION: every state change the CLI performs goes
  through a public subsystem API (create-session!,
  event/append-event!, propose-candidates!, evaluate-candidate!,
  promote!, rollback!). The cli namespaces never issue SQL writes and
  never touch the promotion CURRENT machinery (no
  promotion.current dependency); the ONLY SQL in the cli layer is the
  read-only SELECT helper `query-one`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [evoclj.capability.mint :as cap-mint]
            [evoclj.compiler.core :as compiler]
            [evoclj.config :as config]
            [evoclj.eval.profile :as profile]
            [evoclj.evolution.core :as evolution]
            [evoclj.evolution.demo-mutator :as demo-mutator]
            [evoclj.kernel.error :as err]
            [evoclj.kernel.system :as kernel]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as genome-path]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.store.event :as event]
            [evoclj.store.existence :as existence]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [integrant.core :as ig])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

;; ============================================================================
;; shared host knowledge (the component route descriptor + Resolution catalog)
;; ============================================================================

(def provider-catalog
  "The component Resolution provider catalog (the fixture adapters; no
  real provider credentials ever appear — Global Constraint 22)."
  {:reasoning/high {:provider :fixture
                    :provider-model "fixture-model-v1"
                    :adapter-version "1"}
   :reasoning/low {:provider :fixture
                   :provider-model "fixture-model-low"
                   :adapter-version "1"}
   :fast {:provider :fixture
          :provider-model "fixture-model-fast"
          :adapter-version "1"}})

(def route-descriptor
  "The v0 seed route program descriptor (component choice (a)): the
  immutable route entry point carried on loaded Genomes under
  :programs so they compile for execution/evolution."
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn load-genome-for-execution
  "Load a Genome bundle and attach the route program registry (component choice (a)) so it compiles for execution, evolution, and
  promotion."
  [bundle-root]
  (assoc (load/load-genome bundle-root) :programs [route-descriptor]))

;; ============================================================================
;; state-dir layout
;; ============================================================================

(defn state-dir
  "The CLI's runtime state root for `opts` (default ./evoclj-state)."
  [opts]
  (str (or (:state-dir opts) "./evoclj-state")))

(defn- dash-id [id] (str/replace id ":" "-"))

(defn genomes-dir [opts] (str (state-dir opts) "/genomes"))
(defn candidates-dir [opts] (str (state-dir opts) "/candidates"))

(defn- db-path [opts] (str (state-dir opts) "/db/evoclj.db"))
(defn- cas-root [opts] (str (state-dir opts) "/cas"))

(defn candidate-bundle-root
  "The finalized candidate bundle directory under the CLI's candidates
  dir (the component name rule: the content address with ':' replaced,
  so the name is legal on every host)."
  [opts genome-id]
  (str (candidates-dir opts) "/" (dash-id genome-id)))

(defn resolve-bundle-root
  "The bundle directory for `genome-id` in the CLI's genome store:
  <state-dir>/genomes/<id-as-dash> or <state-dir>/candidates/
  <id-as-dash>. A genome id with no stored bundle is
  :cli/genome-not-found."
  [opts genome-id]
  (let [candidates [(str (genomes-dir opts) "/" (dash-id genome-id))
                    (str (candidates-dir opts) "/" (dash-id genome-id))]
        exists? (fn [p]
                  (Files/isDirectory (Paths/get p (make-array String 0))
                                     (make-array LinkOption 0)))]
    (or (some #(when (exists? %) %) candidates)
        (throw (err/error :cli/genome-not-found
                          "no genome bundle in the CLI store for this id"
                          {:genome/id genome-id})))))

;; ============================================================================
;; the config and the host system
;; ============================================================================

(defn- deep-merge
  "Recursive merge: maps merge key-wise; anything else is replaced by
  the right-hand value (the :overrides seam)."
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

;; ============================================================================
;; the validated F5 config envelope (foundation F5, component)
;; ============================================================================

(def ^:private config-env-overrides
  "The EVOCLJ_* env vars that override scalar values of the validated
  F5 config envelope (the same env-wins-over-file seam
  kernel.system/load-config uses for the host paths). Values are EDN
  (a number, keyword, string, or map, depending on the path)."
  [[[:config/budget :max-candidates] "EVOCLJ_BUDGET_MAX_CANDIDATES"]])

(defn- config-source
  "The F5 config input for `opts`: the direct `:config` value (a map
  or EDN string — the host/test injection seam for the envelope), or
  the EVOCLJ_CONFIG env file, or nil (defaults only). A missing or
  unreadable EVOCLJ_CONFIG file throws the typed :config/invalid
  error (a CLI startup failure, never a stack trace)."
  [opts env]
  (or (:config opts)
      (when-let [f (get env "EVOCLJ_CONFIG")]
        (try
          (slurp f)
          (catch Throwable t
            (throw (err/error :config/invalid
                              (str "unable to read EVOCLJ_CONFIG file " f)
                              {:config-file f
                               :message (.getMessage t)})))))))

(defn- apply-config-env-overrides
  "Assoc-in every set EVOCLJ_* env override over `config`. Values are
  parsed as EDN; an unparseable value throws :config/invalid."
  [config env]
  (reduce (fn [cfg [path env-var]]
            (if-let [v (get env env-var)]
              (assoc-in cfg path
                        (try
                          (edn/read-string v)
                          (catch Throwable t
                            (throw (err/error :config/invalid
                                              (str "unable to parse " env-var
                                                   " as EDN: " v)
                                              {:env-var env-var
                                               :value v
                                               :message (.getMessage t)})))))
              cfg))
          config
          config-env-overrides))

(defn- active-profile-key
  "The selected config profile as a keyword — the :config/profile opts
  value or the EVOCLJ_PROFILE env — or nil when none is selected (the
  same resolution config-envelope applies)."
  [opts env]
  (some-> (or (:config/profile opts) (get env "EVOCLJ_PROFILE")) keyword))

(defn- config-envelope
  "The validated F5 config envelope for `opts` (foundation F5):
  defaults deep-merged with the config source (config/load-config),
  the selected profile applied (config/resolve-profile — the
  :config/profile opts value or the EVOCLJ_PROFILE env), then the
  EVOCLJ_* scalar overrides on top (env wins over file). The result
  is re-validated against ConfigSchema before use.

  component: the :demo profile is BUILT-IN — when selected, its profile
  map (config/demo-profile) is merged into the config source so
  resolve-profile finds it without a config file (a map source still
  wins per-key).

  Throws the typed :config/invalid error (malformed envelope,
  unparseable EDN, unknown top-level key, non-map section) and
  :config/profile-not-found (unknown profile) — the errors the CLI
  surfaces as {:exit 1 :error/type ...} instead of a stack trace."
  [opts env]
  (let [profile-key (active-profile-key opts env)
        demo? (= :demo profile-key)
        source (config-source opts env)
        source (if demo?
                 (merge (config/demo-profile)
                        (if (map? source) source {}))
                 (or source {}))
        loaded (config/load-config source)
        profiled (if profile-key
                   (config/resolve-profile loaded profile-key)
                   loaded)]
    (config/validate-config! (apply-config-env-overrides profiled env))))

(defn- demo-host-overrides
  "The :demo profile's host-injected surface (component): the built-in
  heuristic Mutator (a plain fn the kernel wraps into the Mutator
  protocol — kernel.system/build-mutator) plus the demo's hidden
  selection cases and fixture providers, injected through the SAME
  :overrides seam any host uses."
  []
  (let [m (demo-mutator/demo-mutator)]
    {:evolution/system
     {:mutator (fn [context]
                 (evolution/propose-mutations m context))}
     :eval/system
     {:selection/cases (demo-mutator/demo-selection-cases)
      :selection/fixtures (demo-mutator/demo-selection-fixtures)}}))

(defn build-config
  "Assemble the CLI host config for `opts` (:state-dir, :config,
  :config/profile, :env, :overrides). Mirrors resources/system.edn
  with every path rooted at the state dir.

  The validated F5 config envelope (foundation F5) is loaded through
  evoclj.config/load-config — defaults deep-merged over the input (a
  `:config` map or EDN string, or the EVOCLJ_CONFIG file), the
  :config/profile / EVOCLJ_PROFILE profile resolved, and the EVOCLJ_*
  scalar overrides applied on top (env wins over file). Its
  :config/budget section feeds the evolution-system's
  :budget-profile (the envelope's first CLI consumer; the base
  {:max-candidates 3} cap stays when the section is empty).

  component: selecting the :demo profile injects the built-in heuristic
  Mutator and the demo's hidden selection cases/fixture providers
  through the SAME :overrides seam below (a host's own :overrides
  still win per-key).

  `:overrides` deep-merges config subtrees (e.g.
  {:eval/system {:selection/cases ...}}) so hosts/tests inject the
  hidden cases, fixture providers, and evolution :mutator v0 ships
  empty — the highest-precedence seam, above env and file. A
  malformed envelope throws :config/invalid, surfacing through the
  CLI's typed exit contract rather than a stack trace."
  [opts]
  (let [root (state-dir opts)
        env (or (:env opts) (System/getenv))
        envelope (config-envelope opts env)
        base {:store/sqlite (db-path opts)
              :store/cas {:root (cas-root opts) :verify false}
              :provider/registry
              {:providers [{:provider/type :fixture/echo}
                           {:provider/type :fixture/non-idempotent}]}
              :capability/broker
              {:registry (ig/ref :provider/registry)
               :leases []}
              :runtime/executor
              {:scheduler {:max-steps 1000}
               :store {:sqlite (ig/ref :store/sqlite)
                       :cas (ig/ref :store/cas)}
               :dispatch (ig/ref :capability/broker)}
              :evolution/system
              {:store {:sqlite (ig/ref :store/sqlite)
                       :cas (ig/ref :store/cas)}
               :provider-catalog provider-catalog
               :candidates-dir (candidates-dir opts)
               ;; v0 ships NO :genome-root/:genome-loader (the parent
               ;; bundle is resolved per command) and a NO-OP :mutator (:none — YAGNI, Global Constraint 24; hosts inject one)
               :diagnostician {:task/success-threshold 1.0
                               :max-hypotheses 3
                               :confidence-band :medium}
               :mutator :none
               :budget-profile (merge {:max-candidates 3}
                                      (config/config-value envelope [:config/budget]))
               :programs-registry [route-descriptor]}
              :eval/system
              {:store {:sqlite (ig/ref :store/sqlite)
                       :cas (ig/ref :store/cas)}
               :provider/catalog provider-catalog
               :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
               :profiles {:default-v1 profile/default-v1}
               :genome/roots {}
               :dataset/roots {:evals/evolution (str root "/evals/evolution")
                               :evals/selection (str root "/evals/selection")
                               :evals/audit (str root "/evals/audit")}
               :selection/cases {}
               :selection/fixtures {}
               :replay/cases {}
               :replay/fixtures {}}
              :promotion/system
              {:store {:sqlite (ig/ref :store/sqlite)
                       :cas (ig/ref :store/cas)}
               ;; no :resolution/id here: the CLI builds the promotion-
               ;; system per command with the compiled resolution of the
               ;; candidate/current Genome (promote!/rollback! consume
               ;; it); a :derive would try to compile the genomes dir
               ;; as a bundle at host init
               :event/session-id :derive}
               ;; post-v0 extension 1: the models.dev catalog
               ;; (auto-refreshed at every startup, cached under the
               ;; state dir, offline fallback) and the kernel-owned
               ;; model registry built from it
               :modelsdev/catalog
               {:url "https://models.dev/api.json"
                :cache-dir (str root "/catalog")
                :ttl-hours 24
                :timeout-ms 30000}
               :model/registry
               {:catalog (ig/ref :modelsdev/catalog)
                :registry/api-keys {}}}
        ;; component: the :demo profile's host-injected surface is merged
        ;; in first, then a host's own :overrides win per-key
        overrides (merge (when (= :demo (active-profile-key opts env))
                           (demo-host-overrides))
                         (:overrides opts))]
    (if overrides (deep-merge base overrides) base)))

(defn- ensure-state-dirs!
  "Create the state-dir layout (db parent, genomes, candidates, evals)
  before the host starts; SQLite creates its file but never a missing
  directory, and the genome store must exist for bundle resolution."
  [opts]
  (doseq [d [(str (state-dir opts) "/db")
             (genomes-dir opts)
             (candidates-dir opts)
             (str (state-dir opts) "/catalog")
             (str (state-dir opts) "/evals/evolution")
             (str (state-dir opts) "/evals/selection")
             (str (state-dir opts) "/evals/audit")]]
    (Files/createDirectories (Paths/get d (make-array String 0))
                             (make-array FileAttribute 0)))
  opts)

;; ============================================================================
;; MCP provider config + registration
;; ============================================================================

(defn- validate-mcp-provider-config!
  "Validate one MCP provider config map. Throws :provider/config-invalid
  when required keys are missing or of the wrong type."
  [cfg]
  (when-not (map? cfg)
    (throw (err/error :provider/config-invalid
                      "MCP provider config must be a map"
                      {:value (err/sanitize cfg)})))
  (when-not (contains? cfg :tool/id)
    (throw (err/error :provider/config-invalid
                      "MCP provider config requires :tool/id"
                      {:value (err/sanitize cfg)})))
  (when-not (keyword? (:tool/id cfg))
    (throw (err/error :provider/config-invalid
                      "MCP provider :tool/id must be a keyword"
                      {:value (err/sanitize cfg)})))
  (when-not (contains? cfg :tool/mcp-name)
    (throw (err/error :provider/config-invalid
                      "MCP provider config requires :tool/mcp-name"
                      {:value (err/sanitize cfg)})))
  (when-not (string? (:tool/mcp-name cfg))
    (throw (err/error :provider/config-invalid
                      "MCP provider :tool/mcp-name must be a string"
                      {:value (err/sanitize cfg)})))
  cfg)

(defn register-mcp-providers!
  "Register MCP-backed providers from `mcp-provider-cfgs` (a collection
  of config maps) into the system's :provider/registry. Each config map
  must contain at least :tool/id and :tool/mcp-name, plus any other
  keys consumed by mcp-bridge/mcp-provider (e.g.
  :transport-config). Throws :provider/config-invalid on bad config;
  :provider/duplicate-tool-id from registry/register! propagates."
  [system mcp-provider-cfgs]
  (doseq [cfg mcp-provider-cfgs
          :let [validated (validate-mcp-provider-config! cfg)]]
    (registry/register! (:provider/registry system)
                        (mcp-bridge/mcp-provider validated)))
  system)

(defn mcp-providers-from-config
  "Read the optional MCP provider configs from the assembled CLI config.
  Returns the collection at [:provider/registry :mcp-providers], or nil
  when the config does not carry MCP providers."
  [config]
  (get-in config [:provider/registry :mcp-providers]))

(defn build-system
  "Build (and return) the CLI host system for `opts`: the config
  assembled by build-config, the state-dir layout created, then
  kernel.system/init (ig/init + schema migration + catalog provider
  registration). The CLI only READS the system and calls public APIs.

  When the config carries [:provider/registry :mcp-providers], each
  entry is validated and registered into the system's :provider/registry
  before the system is returned (opt-in MCP bridge integration)."
  [opts]
  (ensure-state-dirs! opts)
  (let [cfg (build-config opts)
        sys (kernel/init cfg)]
    (when-let [mcp-cfgs (mcp-providers-from-config cfg)]
      (register-mcp-providers! sys mcp-cfgs))
    sys))

(defn db-of [system] (:store/sqlite system))
(defn cas-of [system] (:store/cas system))
(defn store-of
  "The executor :stores shape {:sqlite <db> :cas <cas object>}."
  [system]
  {:sqlite (db-of system) :cas (cas-of system)})

(defn candidate-store-of
  "Return the narrow CandidateStore handle for candidate/mutation APIs.
  Runtime and promotion APIs continue to receive the executor stores map
  from store-of."
  [system]
  (candidate-store/make-candidate-store (db-of system)))

;; ============================================================================
;; the CLI's ONLY raw SQL — read-only SELECTs (no write path exists)
;; ============================================================================

(defn- query-one
  "Run ONE read-only SELECT and return its first row. This is the cli
  layer's only raw SQL surface (component Step 2's by-construction
  guarantee: no SQL writes, no raw JDBC, no promotion.current
  dependency)."
  [db sql-params]
  (first (sqlite/query db sql-params)))

(defn generation-row
  "The generations row for `generation-id`, or nil. Read-only."
  [system generation-id]
  (query-one (db-of system)
             ["SELECT * FROM generations WHERE id = ?" generation-id]))

(defn generation-by-genome-id
  "The generations row whose Genome is `genome-id`, or nil. Read-only."
  [system genome-id]
  (query-one (db-of system)
             ["SELECT * FROM generations WHERE genome_id = ?" genome-id]))

(defn current-generation-info
  "The CURRENT generation as {:generation/id :genome/id}, via the
  public recovery startup scan in lenient mode (read-only). Returns
  nil when no generation is current."
  [system]
  (let [report (recovery/startup-integrity-scan (db-of system) (cas-of system)
                                               {:strict? false})
        cg (:current-generation report)]
    (when (contains? #{:ok :missing} (:status cg))
      {:generation/id (:generation/id cg)
       :genome/id (:genome/id cg)})))

(defn generation-genome-id
  "The genome id of `generation-id` ('current' resolves through the
  public recovery scan). Throws :cli/generation-not-found when the
  generation is unknown."
  [system generation-id]
  (let [row (if (= generation-id "current")
              (current-generation-info system)
              (generation-row system generation-id))]
    (when-not row
      (throw (err/error :cli/generation-not-found
                        "no generation with this id in the store"
                        {:generation/id generation-id})))
    (or (:genome/id row) (:genome_id row))))

;; ============================================================================
;; compiled identity + operator sessions
;; ============================================================================

(defn- genome-index-body
  "Return the canonical Genome index bytes for one loaded bundle."
  [loaded]
  (apply str
         (map (fn [[path {:keys [digest]}]]
                (str path "\u0000" digest "\n"))
              (sort-by first genome-path/bytewise-compare (:files loaded)))))

(defn ensure-identity-artifacts!
  "Register compiled identity rows only after the Genome's canonical
  index body has been stored and verified in CAS. When `loaded` is
  supplied, it is the filesystem bundle just loaded by the caller and
  becomes the source of that canonical body; without it, an existing CAS
  proof is required and no placeholder Genome row is fabricated."
  ([system identity]
   (ensure-identity-artifacts! system identity nil))
  ([system identity loaded]
   (let [db (db-of system)
         genome-id (:genome/id identity)]
     (if loaded
       (let [body (.getBytes (genome-index-body loaded)
                             StandardCharsets/UTF_8)
             stored (:artifact/id (cas/put-bytes! (cas-of system) body {}))]
         (when-not (= stored genome-id)
           (throw (err/error :cli/genome-mismatch
                             "loaded Genome body does not match its identity"
                             {:genome/id genome-id
                              :stored-artifact-id stored})))
         (artifact/ensure-artifact! db genome-id
                                     "application/octet-stream"
                                     (alength body))
         (artifact/ensure-genome! db genome-id))
       (existence/verified-digest (cas-of system) genome-id))
     (artifact/ensure-artifact! db (:resolution/id identity)
                                 "application/edn" 0)
     (artifact/ensure-artifact! db (:phenotype/id identity)
                                 "application/edn" 0)
     identity)))

(defn generation-identity
  "The compiled identity of `generation-id`'s Genome:
  {:generation/id :genome/id :resolution/id :phenotype/id}, the
  genome id verified against the generation row (:cli/genome-mismatch
  when the stored bundle compiles to a different address)."
  [opts system generation-id]
  (let [row (generation-row system generation-id)]
    (when-not row
      (throw (err/error :cli/generation-not-found
                        "no generation with this id in the store"
                        {:generation/id generation-id})))
    (let [bundle-root (resolve-bundle-root opts (:genome_id row))
          loaded (load-genome-for-execution bundle-root)
          compiled (compiler/compile-genome loaded provider-catalog)]
      (when-not (= (:genome_id row) (:compiled/genome-id compiled))
        (throw (err/error :cli/genome-mismatch
                          "the stored bundle does not compile to the generation's genome id"
                          {:generation/id generation-id
                           :generation/genome-id (:genome_id row)
                           :compiled/genome-id (:compiled/genome-id compiled)})))
      (let [identity {:generation/id generation-id
                      :genome/id (:compiled/genome-id compiled)
                      :resolution/id (:compiled/resolution-id compiled)
                      :phenotype/id (:compiled/phenotype-id compiled)}]
        (ensure-identity-artifacts! system identity loaded)
        identity)))
)

(defn operator-session!
  "Create the operator session anchoring promotion/rollback events:
  pinned to `generation-id`'s compiled identity with its
  :session/created root event appended (the host's job — promote! and
  rollback! validate this anchor inside their transactions). Returns
  the session id."
  [opts system generation-id]
  (let [identity (generation-identity opts system generation-id)
        db (db-of system)
        sid (:session/id
             (session/create-session!
              db
              {:genome/id (:genome/id identity)
               :resolution/id (:resolution/id identity)
               :phenotype/id (:phenotype/id identity)
               :generation/id generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id generation-id
                          :phenotype/id (:phenotype/id identity)
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

;; ============================================================================
;; artifact + evaluator helpers
;; ============================================================================

(defn read-artifact
  "Read a CAS artifact back as EDN data."
  [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id) StandardCharsets/UTF_8)))

(defn build-evaluator
  "The component evaluator value for one candidate, assembled from the
  host's eval-system component (:kernel/abi, :profiles,
  :provider/catalog, and the hidden cases/fixtures a host injected
  through config :overrides) plus the resolved bundle roots."
  [system parent-gen-id parent-root cand candidate-root]
  (let [es (:eval/system system)]
    {:store (:store es)
     :provider/catalog (:provider/catalog es)
     :kernel/abi (:kernel/abi es)
     :profiles (:profiles es)
     :genome/roots {parent-gen-id parent-root
                    (str (:candidate/id cand)) candidate-root}
     :selection/cases (:selection/cases es)
     :selection/fixtures (:selection/fixtures es)
     :replay/cases (:replay/cases es)
     :replay/fixtures (:replay/fixtures es)
     :programs (fn [_loaded] [route-descriptor])}))

;; ============================================================================
;; `run` — execute one session pinned to a generation
;; ============================================================================
(defn- tool-lease
  "One per-session CapabilityLease granting this session+phenotype's exact id
  the tool's :invoke action for the next minute (the CLI grants
  leases ONLY for the tools the operator names with --tool; a visible
  tool never grants resource authority — Global Constraint 9).
  Delegates to evoclj.capability.mint/mint-lease! (P2 single issuance
  surface). Subject is dual-anchor {:session/id :phenotype/id} (P3).
  When `lease-registry` is supplied the lease is recorded so it can be
  revoked (P5); when nil the lease is minted without recording (backcompat)."
  ([session-id phenotype-id tool-id]
   (tool-lease session-id phenotype-id tool-id nil))
  ([session-id phenotype-id tool-id lease-registry]
   (let [now (Date.)]
     (cap-mint/mint-lease! lease-registry
                           {:cap-id (UUID/randomUUID)
                            :subject {:session/id session-id :phenotype/id phenotype-id}
                            :resource {:kind :tool :id tool-id}
                            :actions #{:invoke}
                            :constraints {:max-calls 10000}
                            :issued-at now
                            :expires-at (Date. (+ (.getTime now) 60000))}))))

(defn- model-lease
  "One per-session CapabilityLease granting this session+phenotype's exact id
  the :invoke action on ONE model resource for the next minute (the
  CLI grants leases ONLY for the models the operator names with
  --model, by their full models.dev id, e.g.
  deepseek/deepseek-v4-flash; a visible model never grants resource
  authority — Global Constraint 9). Delegates to
  evoclj.capability.mint/mint-lease! (P2 single issuance surface).
  Subject is dual-anchor {:session/id :phenotype/id} (P3).
  When `lease-registry` is supplied the lease is recorded so it can be
  revoked (P5)."
  ([session-id phenotype-id model-id]
   (model-lease session-id phenotype-id model-id nil))
  ([session-id phenotype-id model-id lease-registry]
   (let [now (Date.)]
     (cap-mint/mint-lease! lease-registry
                           {:cap-id (UUID/randomUUID)
                            :subject {:session/id session-id :phenotype/id phenotype-id}
                            :resource {:kind :model :id model-id}
                            :actions #{:invoke}
                            :constraints {:max-calls 10000}
                            :issued-at now
                            :expires-at (Date. (+ (.getTime now) 60000))}))))

(defn- program-sources
  "Decode every compiled program's source text from the immutable
  loaded bundle :files (the CompiledGenome carries only :source/digest
  references, Global Constraint 22)."
  [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded [:files (:file descriptor) :bytes]))
                        StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- read-task-file
  "Read and parse the --task EDN file (:cli/task-file-missing when it
  does not exist, :cli/task-file-invalid when it does not parse)."
  [path]
  (let [f (io/file path)]
    (when-not (.isFile f)
      (throw (err/error :cli/task-file-missing
                        "the task file does not exist"
                        {:task-file path})))
    (try
      (edn/read-string (slurp f))
      (catch Exception e
        (throw (err/error :cli/task-file-invalid
                          "the task file is not valid EDN"
                          {:task-file path
                           :message (.getMessage e)}))))))

(defn resolve-generation
  "Resolve `--genome <id|current>` to {:generation/id :genome/id
  :bundle-root}. 'current' reads the CURRENT pointer through the
  public recovery scan; an id resolves through the generations table
  (read-only)."
  [system opts genome-spec]
  (let [row (if (= genome-spec "current")
              (current-generation-info system)
              (generation-by-genome-id system genome-spec))]
    (when-not row
      (throw (err/error :cli/generation-not-found
                        "no generation found for this genome"
                        {:genome/id genome-spec})))
    (let [gen-id (if (= genome-spec "current")
                   (:generation/id row)
                   (:id row))
          genome-id (or (:genome/id row) (:genome_id row))]
      {:generation/id gen-id
       :genome/id genome-id
       :bundle-root (resolve-bundle-root opts genome-id)})))

(defn parse-tool-id
  "Parse a --tool option value (':fixture/echo' or 'fixture/echo')
  into the tool id keyword."
  [s]
  (let [t (str s)]
    (if (str/starts-with? t ":")
      (keyword (subs t 1))
      (keyword t))))

(defn run-cmd!
  "evoclj run --genome <id|current> --task <edn-file>
        [--tool <tool-id> ...]

  Execute ONE session pinned to the genome's generation through the
  real pipeline (load → compile → instantiate → create pinned session
  → run-session! → materialize Episode) and return the run result.
  Every mutation (session creation, root event, episode) goes through
  the public store/runtime APIs."
  [opts]
  (let [genome-spec (get-in opts [:options :genome])
        task-file (get-in opts [:options :task])
        _ (when-not genome-spec
            (throw (err/error :cli/usage-invalid
                              "run requires --genome <id|current>"
                              {:usage "evoclj run --genome <id|current> --task <edn-file> [--tool <tool-id> ...]"})))
        _ (when-not task-file
            (throw (err/error :cli/usage-invalid
                              "run requires --task <edn-file>"
                              {:usage "evoclj run --genome <id|current> --task <edn-file> [--tool <tool-id> ...]"})))
        tools (mapv parse-tool-id (get-in opts [:options :tool]))
        models (mapv str (get-in opts [:options :model]))
        system (build-system opts)
        task (read-task-file task-file)]
    (let [generation (resolve-generation system opts genome-spec)
          loaded (load-genome-for-execution (:bundle-root generation))
          compiled (compiler/compile-genome loaded provider-catalog)]
      (when-not (= (:genome/id generation) (:compiled/genome-id compiled))
        (throw (err/error :cli/genome-mismatch
                          "the resolved bundle does not compile to the generation's genome id"
                          {:generation/id (:generation/id generation)
                           :generation/genome-id (:genome/id generation)
                           :compiled/genome-id (:compiled/genome-id compiled)})))
      (let [_ (ensure-identity-artifacts!
                system
                {:genome/id (:compiled/genome-id compiled)
                 :resolution/id (:compiled/resolution-id compiled)
                 :phenotype/id (:compiled/phenotype-id compiled)}
                loaded)
            db (db-of system)
            cas-store (cas-of system)
            reg (:provider/registry system)
            usage (atom {})
            phenotype-id (:compiled/phenotype-id compiled)
            sid (:session/id
                 (session/create-session!
                  db
                  {:genome/id (:compiled/genome-id compiled)
                   :resolution/id (:compiled/resolution-id compiled)
                   :phenotype/id phenotype-id
                   :generation/id (:generation/id generation)}))
            lease-registry (cap-mint/create-lease-registry)
            leases (concat (mapv #(tool-lease sid phenotype-id % lease-registry) tools)
                             (mapv #(model-lease sid phenotype-id % lease-registry) models))
            leases (vec leases)
            model-reg (:model/registry system)
            ph (phenotype/instantiate
                compiled
                {:stores {:sqlite :poison :cas {:root :poison}}
                 :providers {:registry reg}
                 :capabilities {:leases leases :usage usage}
                 :program-sources (program-sources loaded compiled)})
            executor {:phenotype ph
                      :stores {:sqlite db :cas cas-store}
                      :dispatch (dispatch/make-broker-context
                                 {:registry reg :leases leases :usage usage
                                  :model-registry model-reg :lease-registry lease-registry})}]
        (event/append-event! db
                             {:session/id sid
                              :generation/id (:generation/id generation)
                              :phenotype/id phenotype-id
                              :event/type :session/created
                              :cause/event-id nil
                              :payload-ref nil
                              :metadata {}})
        (let [result (scheduler/run-session! executor sid task)
              ep (episode/materialize-episode! (:stores executor) sid)]
          {:session/id sid
           :status (:status result)
           :output-ref (:output-ref result)
           :error/artifact-ref (:error/artifact-ref result)
           :events (:event/count result)
           :episode (:episode/id ep)})))))

;; ============================================================================
;; `replay`, `events`, `capability inspect` — read-only store commands
;; ============================================================================

(defn- session-or-throw!
  "The Session contract map for `sid`, or :cli/session-not-found."
  [system sid]
  (or (session/get-session (db-of system) sid)
      (throw (err/error :cli/session-not-found
                        "no session with this id"
                        {:session/id sid}))))

(defn replay!
  "evoclj replay --session <uuid>

  Historical readback of a recorded session: its Episode (materialized
  idempotently through the public runtime API), the task input read
  from the CAS, and the accumulated output."
  [opts]
  (let [sid (get-in opts [:options :session])
        _ (when-not sid
            (throw (err/error :cli/usage-invalid
                              "replay requires --session <uuid>"
                              {:usage "evoclj replay --session <uuid>"})))
        sid (UUID/fromString (str sid))
        system (build-system opts)
        store (store-of system)
        _ (session-or-throw! system sid)
        ep (episode/materialize-episode! store sid)
        events (event/events-for-session (db-of system) sid)
        completed (some #(when (= :session/completed (:event/type %)) %) events)
        failed (some #(when (= :session/failed (:event/type %)) %) events)]
    {:session/id sid
     :episode ep
     :task-input (read-artifact store (:task-ref ep))
     :output (when-let [r (:payload-ref completed)]
               (read-artifact store r))
     :error/artifact-ref (when-let [r (:payload-ref failed)]
                           (read-artifact store r))}))

(defn event-tree
  "The session's causal trace as a NESTED tree (feature O1): each
  event becomes {:event/seq :event/type :children [<nested> ...]}
  where :children holds the events whose :cause/event-id points at
  this event. The root event (nil cause) is the tree root; orphaned
  events (a cause pointing at an unknown seq) are collected under
  :orphans. Pure data — the trace is never mutated."
  [events]
  (let [by-id (into {} (map (fn [e] [(:event/seq e) e])) events)
        roots (filter #(nil? (:cause/event-id %)) events)
        children-of (fn [seq-id]
                      (filter #(= seq-id (:cause/event-id %)) events))
        known (set (keys by-id))
        orphans (filter (fn [e]
                         (and (:cause/event-id e)
                              (not (contains? known (:cause/event-id e)))))
                       events)
        node (fn node [e]
               {:event/seq (:event/seq e)
                :event/type (:event/type e)
                :children (mapv node (children-of (:event/seq e)))})]
    {:roots (mapv node roots)
     :orphans (mapv (fn [e]
                     {:event/seq (:event/seq e)
                      :event/type (:event/type e)
                      :cause/event-id (:cause/event-id e)})
                   orphans)}))

(defn events!
  "evoclj events --session <uuid> [--tree]

  The session's full append-only causal trace (public event contract
  fields), ascending :event/seq. With --tree the trace is returned as
  a NESTED causal tree (feature O1): children are the events chained
  to their cause, so the effect protocol (:intent/proposed ->
  :intent/authorized -> :provider/call-started ->
  :provider/call-completed) is visible as parent/child structure."
  [opts]
  (let [sid (get-in opts [:options :session])
        _ (when-not sid
            (throw (err/error :cli/usage-invalid
                              "events requires --session <uuid>"
                              {:usage "evoclj events --session <uuid>"})))
        sid (UUID/fromString (str sid))
        system (build-system opts)
        _ (session-or-throw! system sid)
        events (event/events-for-session (db-of system) sid)]
    (if (get-in opts [:options :tree])
      {:session/id sid
       :tree (event-tree events)}
      {:session/id sid
       :events (mapv (fn [e]
                       (select-keys e [:event/seq :event/type :cause/event-id
                                       :payload-ref :metadata]))
                     events)})))

(defn capability-inspect!
  "evoclj capability inspect --session <uuid>

  The attributable capability facts (Global Constraint 20) of a
  session: its pinned identity plus every :intent/authorized
  (with the broker decision and lease id) and :intent/denied (with
  the reason) recorded in its causal trace."
  [opts]
  (let [sid (get-in opts [:options :session])
        _ (when-not sid
            (throw (err/error :cli/usage-invalid
                              "capability inspect requires --session <uuid>"
                              {:usage "evoclj capability inspect --session <uuid>"})))
        sid (UUID/fromString (str sid))
        system (build-system opts)
        s (session-or-throw! system sid)
        events (event/events-for-session (db-of system) sid)]
    {:session/id sid
     :session (select-keys s [:genome/id :resolution/id :phenotype/id
                              :generation/id :state])
     :capabilities/authorized
     (mapv #(get-in % [:metadata :authorization])
           (filter #(= :intent/authorized (:event/type %)) events))
     :capabilities/denied
     (mapv #(select-keys (:metadata %) [:intent/id :intent/type :reason])
           (filter #(= :intent/denied (:event/type %)) events))}))

(defn mcp-refresh-providers!
  "evoclj mcp refresh-providers (deprecated — use source refresh).

  Force a schema refresh for all registered MCP providers in the
  current host config. Each MCP bridge provider's cached
  :mcp/last-refreshed timestamp is reset, so the next describe or
  execute-request! re-fetches the descriptor from the remote server
  (when :schema/refresh-interval-ms is configured).

  Returns a map of refreshed tool ids to their current descriptors.
  Kept for backwards compatibility of direct calls; CLI now prefers
  generic source refresh."
  [opts]
  (let [system (build-system opts)
        legacy (mcp-bridge/refresh-all-mcp-providers!)
        reg (:provider/registry system)
        from-registry (when reg
                        (into {}
                              (keep (fn [[tool-id {:keys [descriptor]}]]
                                      (when (= :remote (:effect descriptor))
                                        [tool-id descriptor]))
                                    @reg)))]
    (merge legacy from-registry)))
