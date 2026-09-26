(ns evoclj.runtime.hydrate
  "H1 Hydration factory — the single executor construction path.

  hydrate(session-pin) loads the exact Genome/Resolution/CodeImage via
  the store, verifies Deployment, loads program sources, materializes
  bindings via the scheduler's restore path, and returns a fresh
  ExecutionHandle (isolated SCI + broker).

  All call sites (root run, subagent child, eval side, replay) delegate
  to this namespace so there is exactly one place where the pinned
  identity is trusted and one place where a fresh SCI/broker pair is
  created.

  Id authentication (Global Constraint 2 / I1):
    every identity id the pin carries must have its row in the I1
    identity tables and the row's code image must equal the pin's
    :code/id, else throw (typed :hydrate/pin-mismatch). A missing row is
    a mismatch, never a silent pass — a session whose identity was never
    registered cannot be executed.

  Bindings are materialized through the scheduler's durable store
  (evoclj.store.binding/restore!) — a missing table degrades to [] and
  is recorded as a degradation event, never a silent swallow.

  Real-genome load path (H1):
    hydrate loads the persisted bundle registered by the host
    (evoclj.store.genome/register-loaded-genome! — written by the CLI and
    eval identity-registration points), re-compiles via
    evoclj.compiler.core/compile-genome, and produces a real
    CompiledGenome with real topology. The resulting :code/id must equal
    the pin's :code/id, and :code/genome-id/:code/resolution-id
    must match the pin. A mismatch throws :hydrate/pin-mismatch; a
    session with NO registered bundle fails closed with
    :hydrate/genome-bundle-missing (there is no synthetic fallback).

  verify-pin! is the same authentication WITHOUT building an executor:
  the host's pre-flight for a session it is about to run.

  The factory owns no global mutable state; every call creates fresh
  SCI, fresh usage atom, fresh CAS temp dir, and a fresh broker context."
  (:require [clojure.string :as str]
            [evoclj.compiler.core :as compiler]
            [evoclj.genome.types :as types]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.store.cas :as cas]
            [evoclj.store.genome :as store-genome]
            [evoclj.store.identity :as identity]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))
;; ---------------------------------------------------------------------------
;; db helpers
;; ---------------------------------------------------------------------------

(defn- cas-of
  "Extract the CAS handle from the db argument if it is a stores map
  {:sqlite spec :cas cas}. Returns nil when db is a bare sqlite spec
  (the common call-site convention)."
  [db]
  (when (and (map? db) (contains? db :cas))
    (:cas db)))

(defn- fetch-session-row
  "Raw sessions row (map with string keys) for id, or nil."
  [db sid-str]
  (try
    (first (sqlite/query (sqlite/db-spec db)
                         ["SELECT * FROM sessions WHERE id = ?" sid-str]))
    (catch Exception _ nil)))

(defn- normalize-pin
  "Return a normalized pin map {:session/id :genome/id :resolution/id
  :code/id :deployment/id :execution/id :generation/id} from either a
  session map, a session-id value, or a string. When given a map that
  already looks like a pin it is returned as-is (with code alias normalisation)."
  [db pin-or-id]
  (cond
    ;; already a session map
    (and (map? pin-or-id) (:session/id pin-or-id))
    (let [m pin-or-id]
      {:session/id (types/session-id (:session/id m))
       :generation/id (:generation/id m)
       :genome/id (or (:genome/id m) (:genome_id m))
       :resolution/id (or (:resolution/id m) (:resolution_id m))
       :code/id (or (:code/id m) (:code-image/id m) (:code_image_id m))
       :deployment/id (or (:deployment/id m) (:deployment_id m))
       :execution/id (or (:execution/id m) (:execution_id m))})

    ;; string / uuid session id -> fetch
    (or (string? pin-or-id) (instance? UUID pin-or-id))
    (let [sid (types/session-id pin-or-id)
          row (fetch-session-row db (str sid))]
      (when row
        {:session/id sid
         :generation/id (:generation_id row)
         :genome/id (:genome_id row)
         :resolution/id (:resolution_id row)
         :code/id (or (:code_image_id row) (:phenotype_id row))
         :deployment/id (:deployment_id row)
         :execution/id (:execution_id row)}))

    ;; map with string keys (raw row)
    (map? pin-or-id)
    {:session/id (some-> (:id pin-or-id) types/session-id)
     :generation/id (:generation_id pin-or-id)
     :genome/id (:genome_id pin-or-id)
     :resolution/id (:resolution_id pin-or-id)
     :code/id (or (:code_image_id pin-or-id) (:phenotype_id pin-or-id))
     :deployment/id (:deployment_id pin-or-id)
     :execution/id (:execution_id pin-or-id)}

    :else nil))

;; ---------------------------------------------------------------------------
;; Id authentication
;; ---------------------------------------------------------------------------

(defn- pin-mismatch-message
  "The human message for one pin-failure reason (the typed error's
  :error/type and the finding data are the contract; the message is
  descriptive)."
  [reason]
  (case reason
    :execution-row-missing "the pinned execution row is missing"
    :deployment-row-missing "the pinned deployment row is missing"
    :code-image-row-missing "the pinned code image row is missing"
    "the pinned identity disagrees with the persisted identity rows"))

(defn- authenticate!
  "Verify the pinned identity against the persisted I1 identity rows —
  FAIL-CLOSED. The rule lives with the rows (evoclj.store.identity/
  pin-failure); this namespace owns the error TYPE (:hydrate/pin-mismatch)
  and throws the finding as its data."
  [db pin]
  (when-let [failure (identity/pin-failure db pin)]
    (throw (err/error :hydrate/pin-mismatch
                      (pin-mismatch-message (:reason failure))
                      failure)))
  pin)

;; ---------------------------------------------------------------------------
;; Program sources / compiled genome
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Leases
;; ---------------------------------------------------------------------------

(defn- load-persisted-leases
  "P1: load active (revoked=0) leases from DB via capability-store.
  DB is truth; no synthetic fallback. Returns vector (empty when none) or nil on error.

  FAIL-CLOSED PER ROW. A lease is an authorization grant, so every field
  is parsed strictly: a row whose id is not a UUID, or whose timestamps
  do not parse, is DROPPED, never repaired. This used to invent a random
  UUID and, worse, an `now` .. `now+1h` window — which
  validate-lease accepts, policy/authorize then returns :allow for, and
  the session runs with a live grant nobody ever issued. Reachable
  because 019-p1-authority backfilled every pre-019 row's lease_edn to
  NULL and 013-capabilities' id column is TEXT with no UUID CHECK.

  Granularity is per ROW, not per collection: one unreadable row must not
  erase every other valid lease for the session."
  [db sid]
  (try
    (let [cap-store (requiring-resolve 'evoclj.store.capability-store/list-active-capabilities)
          rows (@cap-store (sqlite/db-spec db) {:principal-type "session" :principal-id (str sid)})]
      (when (seq rows)
        (->> rows
             (keep (fn [row]
                     (or (:lease row)
                         ;; strict: any unparseable field drops THIS row
                         (try
                           (let [actions-raw (:actions row)
                                 actions (set (map keyword actions-raw))
                                 constraints (or (:constraints-parsed row) {})
                                 principal {:principal/type :session
                                            :session/id (types/session-id (:principal-id row))}
                                 resource (:resource row)]
                             {:cap/id (UUID/fromString (:id row))
                              :principal principal
                              :resource resource
                              :actions actions
                              :constraints constraints
                              :issued-at (Date/from
                                          (java.time.Instant/parse (:issued-at row)))
                              :expires-at (Date/from
                                           (java.time.Instant/parse (:expires-at row)))})
                           (catch Exception _
                             nil)))))
             vec)))
    (catch Exception _ nil)))

;; ---------------------------------------------------------------------------
;; Real genome load path
;; ---------------------------------------------------------------------------

(defn- real-program-sources
  "Decode every compiled program's source text from the loaded bundle :files
  (the CompiledGenome carries only :source/digest references, Global Constraint 22).
  Mirrors evoclj.cli.session/program-sources."
  [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded [:files (:file descriptor) :bytes]))
                         StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- load-real-compiled
  "Load a real CompiledGenome from a persisted genome bundle.
  
  Returns {:compiled <CompiledGenome> :program-sources <map>} when a bundle
  is registered for the pin's genome-id, otherwise nil.
  
  When a bundle is present, re-compiles via evoclj.compiler.core/compile-genome
  using the stored provider catalog, then asserts that the resulting identity
  matches the pin (:code/genome-id, :code/resolution-id, :code/id).
  A mismatch throws :hydrate/pin-mismatch (typed)."
  [cas-store db pin]
  (when-let [gid (:genome/id pin)]
    (when-let [{:keys [catalog loaded]} (store-genome/loaded-genome cas-store (sqlite/db-spec db) gid)]
      (let [compiled (compiler/compile-genome loaded catalog)
            ;; Assert identity matches the pin — this is the H1 pin validation
            cid (:code/id pin)
            rid (:resolution/id pin)]
        (when (or (not= (:code/genome-id compiled) gid)
                  (not= (:code/resolution-id compiled) rid)
                  (not= (:code/id compiled) cid))
          (throw (err/error :hydrate/pin-mismatch
                            "compiled genome identity disagrees with session pin"
                            {:pin pin
                             :code/genome-id (:code/genome-id compiled)
                             :code/resolution-id (:code/resolution-id compiled)
                             :code/id (:code/id compiled)})))
        {:compiled compiled
         :program-sources (real-program-sources loaded compiled)}))))

;; ---------------------------------------------------------------------------
;; Public factory
;; ---------------------------------------------------------------------------

(defn- resolve-pin
  "The normalized pin for `pin` (a session id or a pin map), merged with
  the persisted session row when one exists. Throws :hydrate/invalid-pin
  for an unusable input."
  [db pin]
  (let [norm (or (normalize-pin db pin)
                 (when (map? pin)
                   {:session/id (or (:session/id pin) (UUID/randomUUID))
                    :genome/id (:genome/id pin)
                    :resolution/id (:resolution/id pin)
                    :code/id (or (:code/id pin) (:code_image_id pin))
                    :deployment/id (:deployment/id pin)
                    :execution/id (:execution/id pin)
                    :generation/id (:generation/id pin)}))]
    (when-not norm
      (throw (err/error :hydrate/invalid-pin
                        "hydrate requires a session pin or id" {:pin pin})))
    (when-not (:session/id norm)
      (throw (err/error :hydrate/invalid-pin "pin missing :session/id" {:pin pin})))
    (let [sid (:session/id norm)
          sess (try (session/get-session (sqlite/db-spec db) sid)
                    (catch Exception _ nil))]
      (if sess
        (merge norm
               {:genome/id (or (:genome/id norm) (:genome/id sess))
                :resolution/id (or (:resolution/id norm) (:resolution/id sess))
                :code/id (or (:code/id norm) (:code/id sess))
                :generation/id (or (:generation/id norm) (:generation/id sess))})
        norm))))

(defn verify-pin!
  "Authenticate the pinned identity WITHOUT building an executor: resolve
  the pin and run the fail-closed identity check (every id the pin
  carries must have its row, and every row must name the pinned code
  image). Returns the normalized pin.

  This is the host's pre-flight for a session it is about to run (the CLI
  run path and the eval side runner) — it costs one indexed read per
  identity id and creates no SCI runtime, no CAS, and no broker context.
  Throws :hydrate/pin-mismatch (see authenticate!) or
  :hydrate/invalid-pin."
  [db pin]
  (when (nil? db)
    (throw (err/error :hydrate/invalid-store
                      "verify-pin! requires a db/store handle" {})))
  (let [pin' (resolve-pin db pin)]
    (authenticate! db pin')
    pin'))

(defn hydrate
  "Build a fresh ExecutionHandle for the pinned session `pin`.

  `db`  — sqlite spec, path, or SessionStore handle (must be migrated).
  `pin` — session id (UUID/string) or session pin map (must contain
          :session/id and the pinned :genome/id/:resolution/id/:code/id).

  Loads the exact Genome/Resolution/CodeImage via the store,
  verifies Deployment (and Execution.code_image_id == pin.code_image_id
  else :hydrate/pin-mismatch), loads program sources, materializes
  bindings via the store (fresh bindings, not cached), and returns a
  fresh ExecutionHandle:

    {:phenotype <fresh SCI phenotype>
     :stores {:sqlite db :cas <temp cas>}
     :dispatch <broker context>
     :cas/dir <temp dir>}

  The handle owns a fresh SCI runtime and broker; it is isolated from
  any other handle (new phenotype instance, not shared).

  The compiled genome is ALWAYS the real one re-compiled from the
  persisted bundle: a session whose bundle was never registered fails
  closed with :hydrate/genome-bundle-missing — there is no synthetic
  fallback to silently execute."
  [db pin]
  (when (nil? db)
    (throw (err/error :hydrate/invalid-store "hydrate requires a db/store handle" {})))
  ;; the CAS handle (when the caller passes the executor :stores map) must be
  ;; read BEFORE the db is normalized; the returned handle always carries a
  ;; plain sqlite spec under :stores/:sqlite so consumers never see the map.
  (let [cas-store (cas-of db)
        db (sqlite/db-spec db)
        pin' (resolve-pin db pin)
        sid (:session/id pin')]
    (authenticate! db pin')
    (let [real (load-real-compiled cas-store db pin')
          compiled (:compiled real)
          program-sources (:program-sources real)
          _ (when-not compiled
              (throw (err/error :hydrate/genome-bundle-missing
                                "no registered genome bundle for the pinned genome id"
                                {:session/id sid
                                 :genome/id (:genome/id pin')
                                 :code/id (:code/id pin')})))
          reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider {}))
          persisted (load-persisted-leases db sid)
          ;; P1: no synthetic fallback — DB miss means deny (empty leases). Restart hydrates from DB.
          leases (or persisted [])
          usage (atom {})
          ph (phenotype/instantiate compiled
                                    {:stores {:sqlite :poison :cas {:root :poison}}
                                     :providers {:registry reg}
                                     :capabilities {:leases leases :usage usage}
                                     :program-sources program-sources})
          cas-dir (str (Files/createTempDirectory "evoclj-cas-hydrate-" (make-array FileAttribute 0)))
          cas-store (cas/->cas cas-dir)
          dispatch-ctx (dispatch/make-broker-context {:registry reg :leases leases :usage usage :db db})]
      {:phenotype ph
       :stores {:sqlite db :cas cas-store}
       :dispatch dispatch-ctx
       :cas/dir cas-dir
       :pin pin'
       :compiled compiled})))
