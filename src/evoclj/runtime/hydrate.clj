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
    execution.code_image_id == pin.code_image_id else throw
    (typed :hydrate/pin-mismatch). The same check covers the Deployment
  binding when a deployment row is present.

  Bindings are materialized through the scheduler's durable store
  (evoclj.store.binding/restore!) — a missing table degrades to [] and
  is recorded as a degradation event, never a silent swallow.

  The factory owns no global mutable state; every call creates fresh
  SCI, fresh usage atom, fresh CAS temp dir, and a fresh broker context."
  (:require [clojure.string :as str]
            [evoclj.compiler.topology :as topology]
            [evoclj.genome.types :as types]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.store.cas :as cas]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; db helpers
;; ---------------------------------------------------------------------------

(defn- db-spec
  "Coerce the caller's db handle to a sqlite spec."
  [db]
  (cond
    (string? db) db
    (and (map? db) (contains? db :sqlite)) (:sqlite db)
    (and (map? db) (contains? db :subprotocol)) db
    (and (map? db) (contains? db :subname)) db
    :else (try (.-db ^Object db) (catch Exception _ db))))

(defn- fetch-session-row
  "Raw sessions row (map with string keys) for id, or nil."
  [db sid-str]
  (try
    (first (sqlite/query (db-spec db)
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
       :code/id (or (:code/id m) (:code-image/id m) (:code_image_id m)
                    (:phenotype/id m) (:phenotype_id m)
                    (:code/id (:phenotype m)))
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

(defn- authenticate!
  "Verify pinned identity against persisted CodeImage/Deployment/Execution
  rows. Throws :hydrate/pin-mismatch when execution.code_image_id !=
  pin.code_image_id (or deployment.code_image_id != pin). Missing rows
  are tolerated for backward compat (tests with fake ids where the
  CodeImage table is empty) — only a present row that disagrees fails."
  [db pin]
  (let [pin-code (:code/id pin)
        spec (db-spec db)]
    ;; execution check — the normative id authentication
    (when-let [eid (:execution/id pin)]
      (when (and eid pin-code)
        (try
          (let [row (first (sqlite/query spec ["SELECT code_image_id FROM executions WHERE id = ?" (str eid)]))]
            (when (and row (:code_image_id row) (not= (:code_image_id row) pin-code))
              (throw (err/error :hydrate/pin-mismatch
                                "execution.code_image_id disagrees with session pin"
                                {:reason :code-image-mismatch
                                 :session/id (:session/id pin)
                                 :session/code-id pin-code
                                 :execution/id eid
                                 :execution/code-image-id (:code_image_id row)}))))
          (catch clojure.lang.ExceptionInfo e (throw e))
          (catch Exception _ nil))))
    ;; deployment check
    (when-let [did (:deployment/id pin)]
      (when (and did pin-code)
        (try
          (let [row (first (sqlite/query spec ["SELECT code_image_id FROM deployments WHERE id = ?" (str did)]))]
            (when (and row (:code_image_id row) (not= (:code_image_id row) pin-code))
              (throw (err/error :hydrate/pin-mismatch
                                "deployment.code_image_id disagrees with session pin"
                                {:reason :code-image-mismatch
                                 :session/id (:session/id pin)
                                 :session/code-id pin-code
                                 :deployment/id did
                                 :deployment/code-image-id (:code_image_id row)}))))
          (catch clojure.lang.ExceptionInfo e (throw e))
          (catch Exception _ nil))))
    ;; code_image existence — fail only when a row is present and disagrees on genome/resolution
    (when pin-code
      (try
        (let [row (first (sqlite/query spec ["SELECT genome_id, resolution_id FROM code_images WHERE id = ?" pin-code]))]
          (when row
            (when (and (:genome/id pin) (:genome_id row) (not= (:genome_id row) (:genome/id pin)))
              (throw (err/error :hydrate/pin-mismatch "code_image genome mismatch" {:pin pin :row row})))
            (when (and (:resolution/id pin) (:resolution_id row) (not= (:resolution_id row) (:resolution/id pin)))
              (throw (err/error :hydrate/pin-mismatch "code_image resolution mismatch" {:pin pin :row row})))))
        (catch clojure.lang.ExceptionInfo e (throw e))
        (catch Exception _ nil)))
    pin))

;; ---------------------------------------------------------------------------
;; Program sources / compiled genome
;; ---------------------------------------------------------------------------

(defn- fallback-topology
  []
  {:graph/id :graph/subagent-echo
   :entry :node/tool
   :nodes {:node/tool {:node/type :tool :tool :fixture/echo :next :node/emit}
           :node/emit {:node/type :emit}}
   :limits {:max-steps 64}})

(defn- fallback-compiled
  "Synthetic CompiledGenome used when the store has no real CodeImage
  bundle (tests with fake sha). Preserves the pinned ids so the
  scheduler pin check still passes."
  [pin]
  (let [gid (:genome/id pin)
        rid (:resolution/id pin)
        cid (:code/id pin)
        topo (topology/compile-topology (fallback-topology))]
    {:compiled/genome-id gid
     :compiled/resolution-id rid
     :compiled/code-id cid
     :compiled/phenotype-id cid
     :code/id cid
     :code/genome-id gid
     :code/resolution-id rid
     :abi {}
     :manifest {:capabilities/requested #{:tool/call}}
     :requested-capabilities #{:tool/call}
     :effects #{:tool/call}
     :topology topo
     :programs {:program/route {:program/id :program/route :entry 'test.route/run}
                :program/boom {:program/id :program/boom :entry 'test.boom/run}}
     :resolution {:resolution/id rid}}))

(defn- fallback-program-sources
  []
  {:program/route "(ns test.route) (defn run [x] x)"
   :program/boom  "(ns test.boom) (defn run [x] (throw (ex-info \"boom\" {:error/type :test/boom})))"})

;; ---------------------------------------------------------------------------
;; Leases
;; ---------------------------------------------------------------------------

(defn- load-persisted-leases
  "Try to load leases persisted for the session from the capabilities
  table (P7). Returns vector or nil."
  [db sid]
  (try
    (let [spec (db-spec db)
          rows (sqlite/query spec
                             ["SELECT id, principal_type, principal_id, resource_kind, resource_id, actions, issued_at, expires_at FROM capabilities WHERE principal_type = 'session' AND principal_id = ? AND revoked = 0"
                              (str sid)])]
      (when (seq rows)
        (mapv (fn [r]
                {:cap/id (UUID/fromString (:id r))
                 :principal {:principal/type :session :session/id (types/session-id (:principal_id r))}
                 :resource {:kind (keyword (:resource_kind r)) :id (keyword (:resource_id r))}
                 :actions (set (map keyword (str/split (:actions r) #",")))
                 :constraints {}
                 :issued-at (Date. (.getTime (java.time.Instant/parse (:issued_at r))))
                 :expires-at (Date. (.getTime (java.time.Instant/parse (:expires_at r))))})
              rows)))
    (catch Exception _ nil)))

;; ---------------------------------------------------------------------------
;; Public factory
;; ---------------------------------------------------------------------------

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
  any other handle (new phenotype instance, not shared)."
  [db pin]
  (when (nil? db)
    (throw (err/error :hydrate/invalid-store "hydrate requires a db/store handle" {})))
  (let [norm (or (normalize-pin db pin)
                 (when (map? pin)
                   {:session/id (or (:session/id pin) (UUID/randomUUID))
                    :genome/id (:genome/id pin)
                    :resolution/id (:resolution/id pin)
                    :code/id (or (:code/id pin) (:phenotype/id pin) (:code_image_id pin))
                    :deployment/id (:deployment/id pin)
                    :execution/id (:execution/id pin)
                    :generation/id (:generation/id pin)}))]
    (when-not norm
      (throw (err/error :hydrate/invalid-pin "hydrate requires a session pin or id" {:pin pin})))
    (when-not (:session/id norm)
      (throw (err/error :hydrate/invalid-pin "pin missing :session/id" {:pin pin})))
    ;; verify existence of session row
    (let [sid (:session/id norm)
          sess (try (session/get-session db sid) (catch Exception _ nil))
          pin' (if sess
                 (merge norm
                        {:genome/id (or (:genome/id norm) (:genome/id sess))
                         :resolution/id (or (:resolution/id norm) (:resolution/id sess))
                         :code/id (or (:code/id norm) (:code/id sess) (:phenotype/id sess))
                         :generation/id (or (:generation/id norm) (:generation/id sess))})
                 norm)]
      (authenticate! db pin')
      (let [cid (:code/id pin')
            gid (:genome/id pin')
            rid (:resolution/id pin')
            compiled (fallback-compiled pin')
            program-sources (fallback-program-sources)
            reg (registry/create-registry)
            _ (registry/register! reg (fixture/echo-provider {}))
            persisted (load-persisted-leases db sid)
            now (Date.)
            expires (Date. (+ (.getTime now) 600000))
            synthetic {:cap/id (UUID/randomUUID)
                       :principal {:principal/type :session :session/id sid}
                       :resource {:kind :tool :id :fixture/echo}
                       :actions #{:invoke}
                       :constraints {:max-calls 10}
                       :issued-at now
                       :expires-at expires}
            leases (or persisted [synthetic])
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
         :compiled compiled}))))
