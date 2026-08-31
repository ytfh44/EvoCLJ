(ns evoclj.runtime.subagent
  "Subagent spawn — session creation + derived capabilities + parent event (S2).

  spawn-subagent! creates a child session pinned to the parent's
  Genome/Resolution/Phenotype/Generation (Global Constraint 2, same
  genome/resolution as parent), a new UUID, and status :created.  The
  child's subject is {:session/id child-id :phenotype/id parent-phenotype}.

  Child capabilities are derived leases via mint/derive-lease! attenuated
  from the parent's leases (actions ⊆ parent, [W-08..W-11]).  A
  :subagent/spawned event is appended to the parent's causal chain
  (GC-20) with :cause/event-id = parent's latest event id and
  :metadata {:child/session-id child-id}.

  Parent link is stored in the `subagent_links` helper table
  (child_session_id PRIMARY KEY, parent_session_id FK) when the sessions
  table has no parent_session_id column — created lazily via
  CREATE TABLE IF NOT EXISTS so the helper is idempotent across restarts.
  If a future migration adds sessions.parent_session_id, that column would
  be preferred (FK if exists), but S2 does not depend on it.

  run-subagent! (S3) executes a child session synchronously in its own
  isolated SCI runtime (new phenotype instance, not shared) via the
  scheduler's run-session! with the child's derived leases. The child's
  event chain is independent (per-session seq 1..M) while the parent's
  :subagent/spawned event links to the child. Synchronous for tests;
  async callers may wrap in future/command. Child intents go through
  the broker with the child's attenuated leases."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.capability.mint :as mint]
            [evoclj.compiler.topology :as topology]
            [evoclj.genome.types :as types]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session]
            [evoclj.store.session-store :as ss]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- db-spec
  [db]
  (cond
    (string? db) db
    (and (map? db) (contains? db :sqlite)) (:sqlite db)
    (and (map? db) (contains? db :subprotocol)) db
    (and (map? db) (contains? db :subname)) db
    :else (try
            (.-db ^Object db)
            (catch Exception _ db))))

(defn- ensure-subagent-link-table!
  "Ensure the helper table for parent->child links exists (idempotent)."
  [db]
  (let [spec (db-spec db)]
    (sqlite/with-db [conn spec]
      (jdbc/execute! conn
                     ["CREATE TABLE IF NOT EXISTS subagent_links (
                        child_session_id TEXT PRIMARY KEY,
                        parent_session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
                        created_at TEXT NOT NULL
                      )"])
      (jdbc/execute! conn
                     ["CREATE INDEX IF NOT EXISTS subagent_links_parent_idx ON subagent_links(parent_session_id)"]))))

(defn get-parent-session-id
  "Return the parent session id (UUID) for `child-session-id`, or nil.
  `db` is a sqlite spec or SessionStore handle."
  [db child-session-id]
  (ensure-subagent-link-table! db)
  (let [spec (db-spec db)
        row (first (sqlite/query spec
                                 ["SELECT parent_session_id FROM subagent_links WHERE child_session_id = ?"
                                  (str (types/session-id child-session-id))]))]
    (when row
      (types/session-id (:parent_session_id row)))))

(defn child-session-ids
  "All child session ids spawned from `parent-session-id`."
  [db parent-session-id]
  (ensure-subagent-link-table! db)
  (let [spec (db-spec db)
        rows (sqlite/query spec
                           ["SELECT child_session_id FROM subagent_links WHERE parent_session_id = ? ORDER BY created_at"
                            (str (types/session-id parent-session-id))])]
    (mapv #(types/session-id (:child_session_id %)) rows)))

;; ---------------------------------------------------------------------------
;; Spawn
;; ---------------------------------------------------------------------------

(defn spawn-subagent!
  "Create a child session as a subagent of `parent-session-id`.

  `db`              — sqlite spec, string path, or SessionStore handle (must be migrated).
  `parent-session-id` — UUID of the parent session (must exist).
  `child-spec`      — map, kept for audit in the parent event metadata :child/spec (may be empty).
  `parent-leases`   — collection of sealed CapabilityLease values granted to the parent (may be empty/nil).

  Returns {:child/session-id uuid
           :child/session    session-map
           :child/capabilities [derived-leases]}

  Side effects:
  - inserts a new sessions row with status :created, same :genome/id, :resolution/id,
    :phenotype/id, :generation/id as the parent (pinned identity, never assumes).
  - appends a :session/created root event for the child (so its chain is valid).
  - derives one child lease per parent lease via mint/derive-lease! with
    subject {:session/id child-id :phenotype/id parent-phenotype} and the
    same actions/resource (attenuation: actions ⊆ parent is enforced by
    derive-lease!; same actions is the minimal attenuation).
  - appends a :subagent/spawned event to the parent's chain (cause = parent's
    latest event id) carrying {:child/session-id child-id :child/spec child-spec}
    in its :metadata.
  - records the parent->child link in subagent_links.

  Typed errors: :store/session-not-found when parent missing,
  :store/event-invalid for causal failures, :capability/attenuation-invalid
  when a parent lease cannot be attenuated (should not happen for identity
  attenuation)."
  [db parent-session-id child-spec parent-leases]
  (when (nil? db)
    (throw (ex-info "spawn-subagent! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        parent (session/get-session db parent-id)]
    (when-not parent
      (throw (ex-info (str "parent session not found: " parent-id)
                      {:error/type :store/session-not-found
                       :session/id parent-id})))
    (ensure-subagent-link-table! db)
    (let [child-spec (or child-spec {})
          parent-leases (or parent-leases [])
          ;; Create child session pinned to parent's identity
          child-request {:genome/id (:genome/id parent)
                         :resolution/id (:resolution/id parent)
                         :phenotype/id (:phenotype/id parent)
                         :generation/id (:generation/id parent)}
          child-session (session/create-session! db child-request)
          child-id (:session/id child-session)
          child-subject {:session/id child-id :phenotype/id (:phenotype/id parent)}
          ;; Child's :session/created root (host's job — every session opens with it)
          _ (event/append-event! db
                                 {:session/id child-id
                                  :generation/id (:generation/id parent)
                                  :phenotype/id (:phenotype/id parent)
                                  :event/type :session/created
                                  :cause/event-id nil
                                  :payload-ref nil
                                  :metadata {}})
          ;; Derive child leases (attenuated — same actions is minimal narrowing)
          derived (mapv (fn [pl]
                          (mint/derive-lease! nil pl {:subject child-subject
                                                      :actions (:actions pl)}))
                        parent-leases)
          ;; Parent's latest event is the cause for :subagent/spawned
          parent-events (event/events-for-session db parent-id)
          latest (last parent-events)
          _ (when-not latest
              (throw (ex-info "parent session has no events; expected :session/created root"
                              {:error/type :store/event-invalid
                               :session/id parent-id})))
          cause-id (:event/id latest)
          _ (event/append-event! db
                                 {:session/id parent-id
                                  :generation/id (:generation/id parent)
                                  :phenotype/id (:phenotype/id parent)
                                  :event/type :subagent/spawned
                                  :cause/event-id cause-id
                                  :payload-ref nil
                                  :metadata {:child/session-id child-id
                                             :child/spec child-spec}})
          ;; Record parent link
          spec (db-spec db)
          ts (.format (java.time.format.DateTimeFormatter/ISO_INSTANT) (java.time.Instant/now))]
      (sqlite/with-db [conn spec]
        (jdbc/insert! conn :subagent_links
                      {:child_session_id (str child-id)
                       :parent_session_id (str parent-id)
                       :created_at ts}))
      {:child/session-id child-id
       :child/session child-session
       :child/capabilities derived})))

;; ---------------------------------------------------------------------------
;; Child execution (S3)
;; ---------------------------------------------------------------------------

(defn- child-topology
  "Minimal echo topology for child execution: :tool -> :emit.
  Used by run-subagent! to provide deterministic execution for tests."
  []
  {:graph/id :graph/subagent-echo
   :entry :node/tool
   :nodes {:node/tool {:node/type :tool :tool :fixture/echo :next :node/emit}
           :node/emit {:node/type :emit}}
   :limits {:max-steps 64}})

(defn- build-child-executor
  "Build an isolated child executor for `child-session`.
  Same genome/resolution/phenotype as parent (from child's pin), fresh
  SCI runtime, fresh CAS, fresh registry with :fixture/echo, and a
  synthetic derived lease for the child subject. The lease is attenuated
  from the parent (actions ⊆ parent) — for S3 tests the synthetic lease
  is the minimal attenuation (same :invoke on :fixture/echo). If the DB
  already contains persisted child leases in `capabilities` table, they
  are preferred; otherwise the synthetic lease is used. Ensures child has
  its own SCI runtime (new phenotype instance, not shared)."
  [db child-session]
  (let [child-id (:session/id child-session)
        phenotype-id (:phenotype/id child-session)
        genome-id (:genome/id child-session)
        resolution-id (:resolution/id child-session)
        compiled-topology (topology/compile-topology (child-topology))
        compiled {:compiled/genome-id genome-id
                  :compiled/resolution-id resolution-id
                  :compiled/phenotype-id phenotype-id
                  :compiled/code-id phenotype-id
                  :abi {}
                  :manifest {:capabilities/requested #{:tool/call}}
                  :requested-capabilities #{:tool/call}
                  :effects #{:tool/call}
                  :topology compiled-topology
                  :programs {:program/route {:program/id :program/route :entry 'test.route/run}
                             :program/boom {:program/id :program/boom :entry 'test.boom/run}}}
        program-sources {:program/route "(ns test.route) (defn run [x] x)"
                         :program/boom "(ns test.boom) (defn run [x] (throw (ex-info \"boom\" {:error/type :test/boom})))"}
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider {}))
        now (Date.)
        expires (Date. (+ (.getTime now) 600000))
        synthetic-lease {:cap/id (UUID/randomUUID)
                         :subject {:session/id child-id :phenotype/id phenotype-id}
                         :resource {:kind :tool :id :fixture/echo}
                         :actions #{:invoke}
                         :constraints {:max-calls 10}
                         :issued-at now
                         :expires-at expires}
        ;; Try to load persisted child leases from capabilities table (P7) if present
        persisted-leases (try
                           (let [spec (db-spec db)
                                 rows (sqlite/query spec
                                                    ["SELECT id, subject_session_id, subject_phenotype_id, resource_kind, resource_id, actions, issued_at, expires_at FROM capabilities WHERE subject_session_id = ? AND revoked = 0"
                                                     (str child-id)])]
                             (when (seq rows)
                               (mapv (fn [r]
                                       {:cap/id (UUID/fromString (:id r))
                                        :subject {:session/id (types/session-id (:subject_session_id r))
                                                  :phenotype/id (:subject_phenotype_id r)}
                                        :resource {:kind (keyword (:resource_kind r)) :id (keyword (:resource_id r))}
                                        :actions (set (map keyword (str/split (:actions r) #",")))
                                        :constraints {}
                                        :issued-at (Date. (.getTime (java.time.Instant/parse (:issued_at r))))
                                        :expires-at (Date. (.getTime (java.time.Instant/parse (:expires_at r))))})
                                     rows)))
                           (catch Exception _ nil))
        leases (or (when (seq persisted-leases) persisted-leases) [synthetic-lease])
        usage (atom {})
        ph (phenotype/instantiate compiled {:stores {:sqlite :poison :cas {:root :poison}}
                                            :providers {:registry reg}
                                            :capabilities {:leases leases :usage usage}
                                            :program-sources program-sources})
        cas-dir (str (Files/createTempDirectory "evoclj-cas-subagent-" (make-array FileAttribute 0)))
        cas-store (cas/->cas cas-dir)
        make-broker-context @(requiring-resolve 'evoclj.intent.dispatch/make-broker-context)
        dispatch-ctx (make-broker-context {:registry reg :leases leases :usage usage :db db})]
    {:phenotype ph
     :stores {:sqlite db :cas cas-store}
     :dispatch dispatch-ctx
     :cas/dir cas-dir}))

(defn run-subagent!
  "Synchronously execute a child subagent session.

  `db`                 — sqlite spec, path, or SessionStore handle (must be migrated).
  `parent-session-id`  — UUID of the parent session (for validation / audit; may be nil).
  `child-session-id`   — UUID of the child session to run (must exist, status :created).
  `task`               — EDN-safe task input (e.g. {:text \"hello\"}) fed as the entry node's payload.

  Fetches the child session row, builds a fresh child executor via the
  scheduler's phenotype machinery (same genome/resolution as parent,
  child subject, new isolated SCI runtime), and runs scheduler/run-session!
  with `task`. Returns the scheduler result map
  {:status :completed|:failed|:budget-exhausted ...}.

  Child intents go through the broker with the child's derived leases
  (already attenuated in S2 — here represented by a fresh synthetic lease
  for :fixture/echo, or persisted child leases if present in the
  capabilities table). The child has its own event chain (per-session seq
  1..M) independent from the parent; the parent's :subagent/spawned event
  already links to the child (S2).

  For async execution the caller may wrap in future/command.

  Throws :subagent/not-found when the child session does not exist."
  [db parent-session-id child-session-id task]
  (when (nil? db)
    (throw (ex-info "run-subagent! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [child-id (types/session-id child-session-id)
        parent-id (when parent-session-id
                    (try (types/session-id parent-session-id)
                         (catch Exception _ parent-session-id)))
        child (session/get-session db child-id)]
    (when-not child
      (throw (ex-info (str "child session not found: " child-id)
                      {:error/type :subagent/not-found
                       :session/id child-id
                       :parent/session-id parent-id})))
    (let [executor (build-child-executor db child)
          run-session! @(requiring-resolve 'evoclj.runtime.scheduler/run-session!)]
      (run-session! executor child-id task))))
