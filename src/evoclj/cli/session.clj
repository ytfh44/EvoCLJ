(ns evoclj.cli.session
  "CLI host building and the session-facing commands (Task 10.2):
  `run`, `replay`, `events`, and `capability inspect`.

  This namespace owns the CLI's HOST BUILDING (shared by all cli
  sub-namespaces): the state-dir layout, the config assembly (with the
  :overrides injection seam), and kernel.system/init. It also owns the
  CLI's ONLY raw SQL — a small set of READ-ONLY SELECTs (generations
  and candidates lookups) documented below; there is no write SQL and
  no raw JDBC anywhere in the cli layer (the by-construction
  guarantee of Task 10.2 Step 2).

  THE STATE-DIR LAYOUT (normative for the CLI; a real deployment
  provisions it the same way the tests do):

      <state-dir>/db/evoclj.db        SQLite store
      <state-dir>/cas/                content-addressed store
      <state-dir>/genomes/<id-as-dash>  immutable Genome bundles, one
                                        directory per content address
                                        ('sha256:' -> 'sha256-')
      <state-dir>/candidates/         evolution candidates-dir (Task 7.4
                                        finalize output)
      <state-dir>/evals/evolution|selection|audit   dataset roots

  HOST CONFIG (`build-config`): the config mirrors resources/system.edn
  with every path rooted at the state dir. `:overrides` (a map of
  config-subtree -> map) is deep-merged over the base so hosts and
  tests inject what v0 ships empty: the evaluator's hidden selection/
  replay cases and fixture providers, an evolution :mutator adapter,
  and so on — the CLI itself ships no cases and no mutator (YAGNI,
  Global Constraint 24).

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
            [evoclj.compiler.core :as compiler]
            [evoclj.eval.profile :as profile]
            [evoclj.kernel.error :as err]
            [evoclj.kernel.system :as kernel]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.genome.load :as load]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [integrant.core :as ig])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

;; ============================================================================
;; shared host knowledge (the Task 2.3 route descriptor + Resolution catalog)
;; ============================================================================

(def provider-catalog
  "The Task 2.1 Resolution provider catalog (the fixture adapters; no
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
  "The v0 seed route program descriptor (Task 2.3 choice (a)): the
  immutable route entry point carried on loaded Genomes under
  :programs so they compile for execution/evolution."
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn load-genome-for-execution
  "Load a Genome bundle and attach the route program registry (Task
  2.3 choice (a)) so it compiles for execution, evolution, and
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
  dir (the Task 7.4 name rule: the content address with ':' replaced,
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

(defn build-config
  "Assemble the CLI host config for `opts` (:state-dir, :overrides).
  Mirrors resources/system.edn with every path rooted at the state
  dir. `:overrides` deep-merges config subtrees (e.g.
  {:eval/system {:selection/cases ...}}) so hosts/tests inject the
  hidden cases, fixture providers, and evolution :mutator v0 ships
  empty."
  [opts]
  (let [root (state-dir opts)
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
               ;; bundle is resolved per command) and NO :mutator
               ;; (YAGNI, Global Constraint 24 — hosts inject one)
               :diagnostician {:task/success-threshold 1.0
                               :max-hypotheses 3
                               :confidence-band :medium}
               :mutator :none
               :budget-profile {:max-candidates 3}
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
               :event/session-id :derive}}
        overrides (:overrides opts)]
    (if overrides (deep-merge base overrides) base)))

(defn- ensure-state-dirs!
  "Create the state-dir layout (db parent, genomes, candidates, evals)
  before the host starts; SQLite creates its file but never a missing
  directory, and the genome store must exist for bundle resolution."
  [opts]
  (doseq [d [(str (state-dir opts) "/db")
             (genomes-dir opts)
             (candidates-dir opts)
             (str (state-dir opts) "/evals/evolution")
             (str (state-dir opts) "/evals/selection")
             (str (state-dir opts) "/evals/audit")]]
    (Files/createDirectories (Paths/get d (make-array String 0))
                             (make-array FileAttribute 0)))
  opts)

(defn build-system
  "Build (and return) the CLI host system for `opts`: the config
  assembled by build-config, the state-dir layout created, then
  kernel.system/init (ig/init + schema migration + catalog provider
  registration). The CLI only READS the system and calls public APIs."
  [opts]
  (ensure-state-dirs! opts)
  (kernel/init (build-config opts)))

(defn db-of [system] (:store/sqlite system))
(defn cas-of [system] (:store/cas system))
(defn store-of
  "The executor :stores shape {:sqlite <db> :cas <cas object>}."
  [system]
  {:sqlite (db-of system) :cas (cas-of system)})

;; ============================================================================
;; the CLI's ONLY raw SQL — read-only SELECTs (no write path exists)
;; ============================================================================

(defn- query-one
  "Run ONE read-only SELECT and return its first row. This is the cli
  layer's only raw SQL surface (Task 10.2 Step 2's by-construction
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
      {:generation/id generation-id
       :genome/id (:compiled/genome-id compiled)
       :resolution/id (:compiled/resolution-id compiled)
       :phenotype/id (:compiled/phenotype-id compiled)})))

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
  "The Task 8.7 evaluator value for one candidate, assembled from the
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
  "One per-session CapabilityLease granting this phenotype's exact id
  the tool's :invoke action for the next minute (the CLI grants
  leases ONLY for the tools the operator names with --tool; a visible
  tool never grants resource authority — Global Constraint 9)."
  [phenotype-id tool-id]
  (let [now (Date.)]
    {:cap/id (UUID/randomUUID)
     :subject {:phenotype/id phenotype-id}
     :resource {:kind :tool :id tool-id}
     :actions #{:invoke}
     :constraints {:max-calls 10000}
     :issued-at now
     :expires-at (Date. (+ (.getTime now) 60000))}))

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
      (let [db (db-of system)
            cas-store (cas-of system)
            reg (:provider/registry system)
            usage (atom {})
            phenotype-id (:compiled/phenotype-id compiled)
            leases (mapv #(tool-lease phenotype-id %) tools)
            ph (phenotype/instantiate
                compiled
                {:stores {:sqlite :poison :cas {:root :poison}}
                 :providers {:registry reg}
                 :capabilities {:leases leases :usage usage}
                 :program-sources (program-sources loaded compiled)})
            executor {:phenotype ph
                      :stores {:sqlite db :cas cas-store}
                      :dispatch (dispatch/make-broker-context
                                 {:registry reg :leases leases :usage usage})}
            sid (:session/id
                 (session/create-session!
                  db
                  {:genome/id (:compiled/genome-id compiled)
                   :resolution/id (:compiled/resolution-id compiled)
                   :phenotype/id phenotype-id
                   :generation/id (:generation/id generation)}))]
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

(defn events!
  "evoclj events --session <uuid>

  The session's full append-only causal trace (public event contract
  fields), ascending :event/seq."
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
    {:session/id sid
     :events (mapv (fn [e]
                     (select-keys e [:event/seq :event/type :cause/event-id
                                     :payload-ref :metadata]))
                   events)}))

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
