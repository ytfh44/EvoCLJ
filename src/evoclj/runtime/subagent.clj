(ns evoclj.runtime.subagent
  "Subagent spawn — session creation + derived capabilities + parent event (S2, W2).

  W2: Work is the sole durable lifecycle. Spawning a subagent also creates a
  child Work (type :subagent/run, queued -> running) that is the durable handle
  for the child's execution. No bare Future shadows Work: run-subagent! drives
  Work running->succeeded/failed via CAS, and cancellation atomically drives
  Work to cancelled via cancel-work! (compare-and-set on works.state). The DB
  row is the truth; a future is only an internal await inside run-subagent!.
  No ::last-refresh-future is stored and no raw future is leaked.

  spawn-subagent! creates a child session pinned to the parent's
  Genome/Resolution/Phenotype/Generation (Global Constraint 2, same
  genome/resolution as parent), a new UUID, and status :created.  The
  child's subject is {:principal/type :session :session/id child-id}.

  Child capabilities are derived leases via mint/derive-lease! attenuated
  from the parent's leases (actions ⊆ parent, [W-08..W-11]).  A
  :subagent/spawned event is appended to the parent's causal chain
  (GC-20) with :prev/event-id = parent's latest event id and
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
             [evoclj.kernel.error :as err]
             [evoclj.provider.fixture :as fixture]
             [evoclj.provider.protocol :as proto]
             [evoclj.provider.registry :as registry]
             [evoclj.runtime.phenotype :as phenotype]
             [evoclj.store.cas :as cas]
             [evoclj.store.event :as event]
             [evoclj.store.session :as session]
             [evoclj.store.session-store :as ss]
             [evoclj.store.sqlite :as sqlite]
             [evoclj.store.work :as work-store]
             [evoclj.tool.specs :as tool.specs]
             [malli.core :as m])
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
(def ^:const max-subagent-depth
  "Maximum nesting depth for subagent chains (S6). Parent depth +1 must be <= this."
  5)

(def ^:const max-spawns-per-parent
  "Budget cap: maximum direct children per parent (maps to :tool/budget {:max-calls 10})."
  10)

(defn subagent-depth
  "Depth of session `sid` in the subagent tree. Root (no parent) has depth 0,
  its child has depth 1, etc. Walks subagent_links via get-parent-session-id."
  [db sid]
  (loop [cur sid depth 0 seen #{}]
    (if (contains? seen cur)
      depth
      (let [parent (try (get-parent-session-id db cur) (catch Exception _ nil))]
        (if parent
          (recur parent (inc depth) (conj seen cur))
          depth)))))

(defn- check-depth-and-budget!
  "Enforce S6 depth and budget caps before spawning a child of `parent-id`.
  Throws :subagent/depth-exceeded when parent depth +1 > max-subagent-depth,
  and :subagent/budget-exceeded when parent already has max-spawns-per-parent children."
  [db parent-id]
  (let [parent-depth (subagent-depth db parent-id)
        child-depth (inc parent-depth)]
    (when (> child-depth max-subagent-depth)
      (throw (err/error :subagent/depth-exceeded
                        (str "subagent depth cap exceeded: parent depth " parent-depth " +1 > " max-subagent-depth)
                        {:parent/session-id parent-id
                         :parent/depth parent-depth
                         :child/depth child-depth
                         :max-depth max-subagent-depth})))
    (let [children (child-session-ids db parent-id)]
      (when (>= (count children) max-spawns-per-parent)
        (throw (err/error :subagent/budget-exceeded
                          (str "subagent budget cap exceeded: parent already has " (count children) " children, max " max-spawns-per-parent)
                          {:parent/session-id parent-id
                           :child/count (count children)
                           :max-calls max-spawns-per-parent})))))
  nil)
;; ---------------------------------------------------------------------------
;; S4 — global lease registry and session->leases index (cascade revoke)
;; ---------------------------------------------------------------------------

(defonce subagent-lease-registry
  (mint/create-lease-registry))

(defonce ^:private leases-by-session
  (atom {}))

(defn subagent-leases
  "Return the derived leases for `session-id` (UUID or string-coerced),
  or empty vector when none. Reads from the in-memory index populated
  by spawn-subagent!."
  [session-id]
  (let [sid (try (types/session-id session-id) (catch Exception _ session-id))]
    (get @leases-by-session sid [])))

(defn leased-session-ids
  "Return the set of session ids that currently have registered leases
  in the subagent index."
  []
  (set (keys @leases-by-session)))

(defn clear-subagent-lease-state!
  "Test helper — clear the global subagent lease registry and index.
  Safe to call between fixtures."
  []
  (reset! subagent-lease-registry {:evoclj.capability.mint/version 0})
  (reset! leases-by-session {})
  nil)


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
    subject {:principal/type :session :session/id child-id} and the
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
          ;; S6 — enforce depth/budget caps before creating the child
          _ (check-depth-and-budget! db parent-id)
          child-request {:genome/id (:genome/id parent)
                         :resolution/id (:resolution/id parent)
                         :phenotype/id (:phenotype/id parent)
                         :generation/id (:generation/id parent)}
          child-session (session/create-session! db child-request)
          child-id (:session/id child-session)
          child-principal {:principal/type :session :session/id child-id}
          ;; Child's :session/created root (host's job — every session opens with it)
          _ (event/append-event! db
                                 {:session/id child-id
                                  :generation/id (:generation/id parent)
                                  :phenotype/id (:phenotype/id parent)
                                  :event/type :session/created
                                  :prev/event-id nil
                                  :payload-ref nil
                                  :metadata {}})
          ;; P1: derive child leases durably — DB INSERT before cache (attenuated)
          derived (mapv (fn [pl]
                          (mint/derive-lease! db subagent-lease-registry pl {:principal child-principal
                                                                              :actions (:actions pl)}))
                        parent-leases)
          ;; keep session index in sync with durable leases (versioned cache mirror)
          _ (when (seq derived)
              (swap! leases-by-session update child-id (fnil into []) derived))
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
                                  :prev/event-id cause-id
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
      ;; W2: durable child Work (queued) — the SOLE execution identity for
      ;; this child. parent-work-id points at the parent's latest Work (nil
      ;; only for the root session, which has no parent Work).
      (let [parent-work-id (some-> (last (work-store/list-works spec parent-id)) :work/id)
            wid (java.util.UUID/randomUUID)]
        (work-store/create-work! spec {:work/id wid
                                       :work/type :subagent/run
                                       :work/state :queued
                                       :work/session-id child-id
                                       :work/parent-work-id parent-work-id
                                       :work/created-at (java.util.Date.)})
        wid)
      {:child/session-id child-id
       :child/session child-session
       :child/capabilities derived})))

;; ---------------------------------------------------------------------------
;; Child execution (S3)
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Child execution (S3) — H1 Hydration factory
;; ---------------------------------------------------------------------------

(defn- build-child-executor
  "Build an isolated child executor for `child-session` via the
  hydration factory. Delegates to evoclj.runtime.hydrate/hydrate so
  there is exactly one place that loads Genome/Resolution/CodeImage,
  verifies Deployment/Execution ids, loads program sources, materializes
  bindings, and creates a fresh SCI + broker pair. Synthetic
  topology/programs/CAS/fixture lease are owned by the factory, not
  copied here."
  [db child-session]
  (let [hydrate @(requiring-resolve 'evoclj.runtime.hydrate/hydrate)
        pin (if (map? child-session) child-session {:session/id child-session})]
    (hydrate db pin)))

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

  Child intents go through the broker with the child's persisted derived leases
  from the capabilities table (P1 DB truth); DB miss means deny, no synthetic lease.
  The child has its own event chain (per-session seq
  1..M) independent from the parent; the parent's :subagent/spawned event
  already links to the child (S2).

  W2: Work's running is execution; a future is only an internal await.
  run-subagent! drives the child Work (queued -> running -> succeeded/failed)
  via CAS and awaits the scheduler's internal future synchronously — no bare
  Future is leaked. For async callers, poll the Work row, not a Future.

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
          run-session! @(requiring-resolve 'evoclj.runtime.scheduler/run-session!)
          ;; W2: reuse the Work the spawn created — never mint a second one.
          ;; The child's sole execution identity is the :subagent/run Work
          ;; spawn-subagent! persisted (queued); scheduler dispatches it to
          ;; :running. Only when the child was never spawned (bare run) does
          ;; the scheduler create its own :session/run Work (work-id nil).
          child-work-id (some-> (last (work-store/list-works db child-id)) :work/id)
          ;; internal future await: Work's running is execution, future is only await
          result @(future (run-session! executor child-id task child-work-id))]
      result)))

;; ---------------------------------------------------------------------------
;; S4 — cancellation and cascade revoke
;; ---------------------------------------------------------------------------

(defn list-descendants
  "Return all descendant session ids (UUIDs) transitively spawned from
  `root-id` via subagent_links (BFS, not including `root-id`).
  Delegates to evoclj.store.session/list-descendants when available."
  [db root-id]
  (try
    (let [f (requiring-resolve 'evoclj.store.session/list-descendants)]
      (@f db root-id))
    (catch Exception _
      ;; fallback local BFS
      (ensure-subagent-link-table! db)
      (let [root-uuid (try (types/session-id root-id) (catch Exception _ root-id))
            spec (db-spec db)]
        (loop [queue [root-uuid] visited #{} result []]
          (if (empty? queue)
            result
            (let [cur (first queue)
                  rest-q (vec (rest queue))]
              (if (contains? visited cur)
                (recur rest-q visited result)
                (let [children (try
                                 (mapv #(types/session-id (:child_session_id %))
                                       (sqlite/query spec
                                                     ["SELECT child_session_id FROM subagent_links WHERE parent_session_id = ? ORDER BY created_at"
                                                      (str cur)]))
                                 (catch Exception _ []))
                      visited' (conj visited cur)]
                  (recur (into rest-q children) visited' (into result children)))))))))))

(defn- revoke-leases-for-session*
  "P1 durable-first revocation: UPDATE capabilities WHERE revoked=0 before swap! cache.
  DB is truth, memory is versioned cache. Idempotent and strict — no try/catch swallow."
  [db session-id]
  (let [sid (try (types/session-id session-id) (catch Exception _ session-id))
        mem-leases (get @leases-by-session sid [])
        registry-leases (mint/leases-for-session subagent-lease-registry sid)
        all-leases (distinct (concat mem-leases registry-leases))
        cap-ids (distinct (concat (mapv :cap/id all-leases) (mapv :cap/id mem-leases)))
        db-cap-ids (let [rows (sqlite/query (db-spec db) ["SELECT id FROM capabilities WHERE principal_type = 'session' AND principal_id = ? AND revoked = 0" (str sid)])
                         ids (mapv #(:id %) rows)]
                     ids)
        all-db-ids (distinct (concat (mapv str cap-ids) db-cap-ids))]
    ;; DURABLE FIRST: update DB rows for all ids (WHERE revoked=0 inside revoke-capability!)
    (doseq [id all-db-ids]
      (when id
        (let [revoke! (requiring-resolve 'evoclj.store.capability-store/revoke-capability!)]
          (@revoke! db (str id)))))
    ;; THEN cache: tombstone in-memory registry and session index
    (doseq [id all-db-ids]
      (let [cap-id (try (UUID/fromString (str id)) (catch Exception _ id))]
        (mint/revoke-lease! subagent-lease-registry cap-id)))
    (doseq [l all-leases]
      (when-let [cap-id (:cap/id l)]
        (mint/revoke-lease! subagent-lease-registry cap-id)))
    nil))

(defn- append-cancel-events!
  "Append :session/cancelled to `child-id` and :subagent/cancelled to
  `parent-id` (when parent provided). Uses latest event as cause where
  available. Silently no-ops when event append fails (already cancelled
  or chain inconsistency is not fatal for revocation)."
  [db parent-id child-id reason]
  ;; child event
  (try
    (let [child-events (event/events-for-session db child-id)
          cause (some-> (last child-events) :event/id)]
      (when child-events
        (try
          (event/append-event! db
                               {:session/id child-id
                                :generation/id (:generation/id (first child-events))
                                :phenotype/id (:phenotype/id (first child-events))
                                :event/type :session/cancelled
                                :prev/event-id cause
                                :payload-ref nil
                                :metadata {:reason reason}})
          (catch Exception _))))
    (catch Exception _))
  ;; parent event
  (when parent-id
    (try
      (let [parent-events (event/events-for-session db parent-id)
            cause (some-> (last parent-events) :event/id)]
        (when parent-events
          (try
            (event/append-event! db
                                 {:session/id parent-id
                                  :generation/id (:generation/id (first parent-events))
                                  :phenotype/id (:phenotype/id (first parent-events))
                                  :event/type :subagent/cancelled
                                  :prev/event-id cause
                                  :payload-ref nil
                                  :metadata {:child/session-id child-id
                                             :reason reason}})
            (catch Exception _))))
      (catch Exception _)))
  nil)

(defn- session-work-state
  "The session's sole execution Work state, or nil when it has no Work yet
  (W2: Work is the sole durable lifecycle — a Session carries no runtime
  state of its own)."
  [db session-id]
  (some-> (last (work-store/list-works db session-id)) :work/state))

(defn cancel-subagent!
  "Cancel a single child subagent session `child-session-id` spawned from
  `parent-session-id`. Cascade: also cancels all transitive descendants
  - drive each target session's Work to :cancelled (CAS on works.state —
    idempotent: already :cancelled is a no-op, other terminal Work states
    are left as-is);

  Effects per target session in the subtree:
  - revoke all its leases via capability/mint revoke-lease! (fail-closed:
    next broker authorize with that lease yields :capability/revoked);
  - revoke corresponding rows in capabilities table when present;
  - mark session as :cancelled via store/session transition (idempotent —
    already :cancelled is a no-op, other terminal states are left as-is);
  - append :session/cancelled to the child's event chain and
    :subagent/cancelled to the immediate parent's chain (best-effort).

  `reason` is a keyword :user-request | :parent-cancel | :timeout or
  any EDN-safe value, stored in event metadata.

  Returns {:cancelled [session-ids] :already-cancelled? bool}.
  Throws :subagent/not-found when child missing, :store/session-not-found
  when parent missing (if parent-id supplied)."
  [db parent-session-id child-session-id reason]
  (when (nil? db)
    (throw (ex-info "cancel-subagent! requires a db/store handle" {:error/type :store/session-invalid})))
  (ensure-subagent-link-table! db)
  (let [child-id (types/session-id child-session-id)
        parent-id (when parent-session-id
                    (try (types/session-id parent-session-id)
                         (catch Exception _ parent-session-id)))
        child (session/get-session db child-id)]
    (when-not child
      (throw (ex-info (str "child session not found: " child-id)
                      {:error/type :subagent/not-found
                       :session/id child-id})))
    (when (and parent-id (not (session/get-session db parent-id)))
      (throw (ex-info (str "parent session not found: " parent-id)
                      {:error/type :store/session-not-found
                       :session/id parent-id})))
    ;; idempotent: if child already cancelled, no-op (still return)
    (if (= :cancelled (session-work-state db child-id))
      {:cancelled [] :already-cancelled? true :child/session-id child-id}
      (let [;; collect subtree: child plus all its descendants
            descendants (list-descendants db child-id)
            targets (into [child-id] descendants)]
        ;; revoke leases for each target
        (doseq [tid targets]
          (revoke-leases-for-session* db tid))
        ;; Work is the sole graceful-cancel authority: atomically drive each
        ;; target's Work to :cancelled (CAS on works.state — the DB row is the
        ;; truth). No Session transition is written.
        (doseq [tid targets]
          ;; W2: Work cancel is CAS on works.state — the DB row is the truth
          (try
            (let [works (work-store/list-works db tid)]
              (doseq [w works]
                (try (work-store/cancel-work! db (:work/id w)) (catch Exception _ nil))))
            (catch Exception _ nil)))
        ;; append events: for the direct child, use supplied parent-id;
        ;; for deeper descendants, append with their immediate parent link
        (append-cancel-events! db parent-id child-id (or reason :user-request))
        (doseq [tid descendants]
          (let [p (try (get-parent-session-id db tid) (catch Exception _ nil))]
            (append-cancel-events! db p tid (or reason :parent-cancel))))
        {:cancelled targets :already-cancelled? false :child/session-id child-id}))))

(defn cancel-subagent-tree!
  "Cascade-cancel the entire subtree rooted at `root-session-id`
  (including root and all transitive descendants via subagent_links).
  Revokes leases and marks each session :cancelled (idempotent).

  `reason` stored in event metadata (default :user-request).
  Returns {:cancelled [session-ids]}. Throws when root not found."
  [db root-session-id reason]
  (when (nil? db)
    (throw (ex-info "cancel-subagent-tree! requires a db/store handle" {:error/type :store/session-invalid})))
  (ensure-subagent-link-table! db)
  (let [root-id (types/session-id root-session-id)
        root (session/get-session db root-id)]
    (when-not root
      (throw (ex-info (str "root session not found: " root-id)
                      {:error/type :store/session-not-found
                       :session/id root-id})))
    (if (= :cancelled (session-work-state db root-id))
      {:cancelled [] :already-cancelled? true :root/session-id root-id}
      (let [descendants (list-descendants db root-id)
            targets (into [root-id] descendants)]
        (doseq [tid targets]
          (revoke-leases-for-session* db tid))
        (doseq [tid targets]
          (try
            (let [works (work-store/list-works db tid)]
              (doseq [w works]
                (try (work-store/cancel-work! db (:work/id w)) (catch Exception _ nil))))
            (catch Exception _ nil)))
        ;; append events for each target (child + its parent)
        (doseq [tid targets]
          (let [p (try (get-parent-session-id db tid) (catch Exception _ nil))]
            (append-cancel-events! db p tid (or reason :parent-cancel))))
        ;; also ensure root's parent gets :subagent/cancelled if root is itself a child
        {:cancelled targets :already-cancelled? false :root/session-id root-id}))))
;; ---------------------------------------------------------------------------
;; S5 — result delivery to parent chain
;; ---------------------------------------------------------------------------

(defn- sha256-cas-ref?
  [s]
  (and (string? s) (boolean (re-matches #"^sha256:[0-9a-f]{64}$" s))))

(defn deliver-result!
  "Deliver a successful child subagent result to its parent's causal chain.

  `db`                — sqlite spec, path, or SessionStore handle (must be migrated).
  `parent-session-id` — UUID of the parent session (must exist).
  `child-session-id`  — UUID of the child session (must be :completed).
  `cas-ref`           — sha256:<64 hex> CAS reference for the child's result artifact.

  E1: appends a :subagent/result event to the parent's chain with
  `:prev/event-id = parent's latest` and
  `:causal-links #{ {:from <child-terminal-event-id> :type :subagent/result} }`
  so cross-session causality is explicit in the graph, not overloaded
  onto prev.

  Typed errors: :store/session-invalid when db nil, :store/session-not-found
  when parent missing, :subagent/not-found when child missing,
  :subagent/not-completed when child is not :completed, :store/cas-invalid
  when cas-ref is not sha256:<64 hex>."
  [db parent-session-id child-session-id cas-ref]
  (when (nil? db)
    (throw (ex-info "deliver-result! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        child-id (types/session-id child-session-id)]
    (when-not (sha256-cas-ref? cas-ref)
      (throw (ex-info (str "invalid cas-ref: " cas-ref)
                      {:error/type :store/cas-invalid
                       :cas-ref cas-ref})))
    (let [child (session/get-session db child-id)]
      (when-not child
        (throw (ex-info (str "child session not found: " child-id)
                        {:error/type :subagent/not-found
                         :session/id child-id})))
      (when-not (= :succeeded (session-work-state db child-id))
        (throw (ex-info (str "child not completed: " child-id " work=" (session-work-state db child-id))
                        {:error/type :subagent/not-completed
                         :session/id child-id
                         :work/state (session-work-state db child-id)})))
      (let [parent (session/get-session db parent-id)]
        (when-not parent
          (throw (ex-info (str "parent session not found: " parent-id)
                          {:error/type :store/session-not-found
                           :session/id parent-id})))
        (let [parent-events (event/events-for-session db parent-id)
              _ (when (empty? parent-events)
                  (throw (ex-info "parent session has no events"
                                  {:error/type :store/event-invalid
                                   :session/id parent-id})))
              prev-id (:event/id (last parent-events))
              child-events (event/events-for-session db child-id)
              child-terminal (last child-events)
              child-terminal-id (when child-terminal (:event/id child-terminal))
              causal-links (if child-terminal-id
                             #{{:from child-terminal-id :type :subagent/result}}
                             #{})]
          (event/append-event! db
                               {:session/id parent-id
                                :generation/id (:generation/id parent)
                                :phenotype/id (:phenotype/id parent)
                                :event/type :subagent/result
                                :prev/event-id prev-id
                                :causal-links causal-links
                                :payload-ref nil
                                :metadata {:child/session-id child-id
                                           :result/cas-ref cas-ref
                                           :result/status :succeeded}}))))))

(defn deliver-failure!
  "Deliver a failed child subagent result to its parent's causal chain.

  `db`                — sqlite spec, path, or SessionStore handle.
  `parent-session-id` — UUID of the parent session (must exist).
  `child-session-id`  — UUID of the child session (must be :failed).
  `error`             — EDN-safe error data (e.g. {:error/type :foo :error/message \"boom\"}).

  E1: appends :subagent/result with prev = parent latest and
  causal-links #{ {:from <child-terminal> :type :subagent/result} }.
  Typed errors mirror deliver-result! but with :subagent/not-failed when child not failed."
  [db parent-session-id child-session-id error]
  (when (nil? db)
    (throw (ex-info "deliver-failure! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        child-id (types/session-id child-session-id)]
    (let [child (session/get-session db child-id)]
      (when-not child
        (throw (ex-info (str "child session not found: " child-id)
                        {:error/type :subagent/not-found
                         :session/id child-id})))
      (when-not (= :failed (session-work-state db child-id))
        (throw (ex-info (str "child not failed: " child-id " work=" (session-work-state db child-id))
                        {:error/type :subagent/not-failed
                         :session/id child-id
                         :work/state (session-work-state db child-id)})))
      (let [parent (session/get-session db parent-id)]
        (when-not parent
          (throw (ex-info (str "parent session not found: " parent-id)
                          {:error/type :store/session-not-found
                           :session/id parent-id})))
        (let [parent-events (event/events-for-session db parent-id)
              _ (when (empty? parent-events)
                  (throw (ex-info "parent session has no events"
                                  {:error/type :store/event-invalid
                                   :session/id parent-id})))
              prev-id (:event/id (last parent-events))
              child-events (event/events-for-session db child-id)
              child-terminal (last child-events)
              child-terminal-id (when child-terminal (:event/id child-terminal))
              causal-links (if child-terminal-id
                             #{{:from child-terminal-id :type :subagent/result}}
                             #{})]
          (event/append-event! db
                               {:session/id parent-id
                                :generation/id (:generation/id parent)
                                :phenotype/id (:phenotype/id parent)
                                :event/type :subagent/result
                                :prev/event-id prev-id
                                :causal-links causal-links
                                :payload-ref nil
                                :metadata {:child/session-id child-id
                                           :result/status :failed
                                           :error error}}))))))

(def AgentSpawnArgsSchema
  "Malli input schema for :agent/spawn (model-facing). :task is required,
  :capabilities is an optional vector of capability hint strings."
  [:map {:closed true}
   [:task string?]
   [:capabilities {:optional true} [:vector string?]]])

(def AgentSpawnOutputSchema
  "Malli output schema for :agent/spawn."
  [:map {:closed false}
   [:child/session-id uuid?]
   [:child/capabilities {:optional true} [:vector :map]]])

(def AgentStatusArgsSchema
  "Malli input schema for :agent/status."
  [:map {:closed true}
   [:session-id string?]])

(def AgentStatusOutputSchema
  "Malli output schema for :agent/status — at minimum the session id and state."
  [:map {:closed false}
   [:session/id {:optional true} uuid?]
   [:state {:optional true} keyword?]])

(def agent-spawn-tool-descriptor
  "The v0 tool descriptor of :agent/spawn. :effect :pure — spawn is persisted
  as a session row + causal event before returning; depth/budget caps are fail-closed."
  {:tool/id :agent/spawn
   :tool/description "Spawn a subagent session"
   :tool/parameters {:type "object"
                     :properties {:task {:type "string"
                                        :description "Task text for the child subagent"}
                                 :capabilities {:type "array"
                                                :description "Optional capability hints"
                                                :items {:type "string"}}}
                     :required ["task"]}
   :tool/budget {:max-calls 10}
   :effect :pure
   :input-schema AgentSpawnArgsSchema
   :output-schema AgentSpawnOutputSchema
   :required-action :invoke
   :lease/resource {:kind :tool :id :agent/spawn}
   :tool/audience #{:model}})

(def agent-status-tool-descriptor
  "The v0 tool descriptor of :agent/status."
  {:tool/id :agent/status
   :tool/description "Query subagent status"
   :tool/parameters {:type "object"
                     :properties {:session-id {:type "string"
                                              :description "Child session id (uuid string)"}}
                     :required ["session-id"]}
   :effect :pure
   :input-schema AgentStatusArgsSchema
   :output-schema AgentStatusOutputSchema
   :required-action :invoke
   :lease/resource {:kind :tool :id :agent/status}
   :tool/audience #{:model}})

;; Reference the single source in tool.specs so S6 does not duplicate
;; the canonical C-Tool definitions (tool.specs is the single source of truth).
;; These defs simply alias tool.specs for callers that prefer the subagent namespace.
(def canonical-agent-spawn-tool tool.specs/agent-spawn-tool)
(def canonical-agent-status-tool tool.specs/agent-status-tool)

(def agent-spawn-tool-catalog-entry
  "Wire declaration of :agent/spawn for the model and the tool loop
  ({:name :description :parameters :tool} — :tool maps wire name back to
  EvoCLJ tool id the scheduler executes through the broker)."
  {:name "agent_spawn"
   :description "Spawn a subagent session"
   :parameters {:type "object"
                :properties {:task {:type "string"
                                   :description "Task text for the child subagent"}
                            :capabilities {:type "array"
                                           :description "Optional capability hints"
                                           :items {:type "string"}}}
                :required ["task"]}
   :tool :agent/spawn})

(def agent-status-tool-catalog-entry
  "Wire declaration of :agent/status."
  {:name "agent_status"
   :description "Query subagent status"
   :parameters {:type "object"
                :properties {:session-id {:type "string"
                                         :description "Child session id (uuid string)"}}
                :required ["session-id"]}
   :tool :agent/status})

(def subagent-tool-catalog
  "The tool catalog the scheduler's tool loop consumes for subagents:
  the two S6 wire tools, in the wire form ({:name :description :parameters :tool})."
  [agent-spawn-tool-catalog-entry agent-status-tool-catalog-entry])

;; --- providers (broker-executable) ----------------------------------------

(defn- tool-args
  "Extract :args map from a tool-call intent payload. Shared helper mirrors
  evolution_tools/tool-args — same contract, same error type."
  [intent]
  (let [payload (:payload intent)]
    (when-not (and (map? payload) (contains? payload :args))
      (throw (err/error :provider/input-invalid
                        "tool-call payload must carry an :args map"
                        {:value (err/sanitize payload)})))
    (:args payload)))

(defn- validate-args!
  "Validate args against descriptor's :input-schema (EDN-safe + malli)."
  [descriptor args]
  (when-not (m/validate (:input-schema descriptor) args)
    (throw (err/error :provider/input-invalid
                      "tool input failed input-schema validation"
                      {:tool/id (:tool/id descriptor)
                       :value (err/sanitize args)
                       :explanation (err/sanitize (m/explain (:input-schema descriptor) args))}))))

(defn agent-spawn-provider
  "Build the kernel-owned :agent/spawn provider (component).

  `db`        — sqlite spec / path / SessionStore handle (must be migrated).
  `parent-session-id` — the parent session id that spawns are attributed to
  (captured closed over). For tool-loop usage the provider is closed over
  the executor's session/pin and reads :session/id from the intent when
  available; the closed-over parent is the fallback.

  normalize-request validates args against AgentSpawnArgsSchema and returns
  the canonical resource {:kind :tool :id :agent/spawn}.
  execute-request! calls spawn-subagent! with the task and returns
  {:child/session-id <uuid>} (EDN-safe). Depth/budget caps are enforced by
  spawn-subagent! itself."
  ([db] (agent-spawn-provider db nil))
  ([db parent-session-id]
   (reify proto/Provider
     (describe [_] agent-spawn-tool-descriptor)
     (normalize-request [_ intent]
       (let [args (tool-args intent)
             _ (validate-args! agent-spawn-tool-descriptor args)]
         {:tool/id :agent/spawn
          :resource {:kind :tool :id :agent/spawn}
          :args args
          :parent/session-id (or (:session/id intent) parent-session-id)}))
     (execute-request! [_ authorized-request]
       (let [args (:args authorized-request)
             parent-id (or (:parent/session-id authorized-request)
                           (:parent/session-id args)
                           parent-session-id
                           (throw (err/error :provider/request-invalid
                                             "agent/spawn requires parent session id (intent :session/id or closed-over parent)"
                                             {:value (err/sanitize authorized-request)})))
             task (:task args)
             ;; child-spec carries the task text; extra keys are passed through for audit
             child-spec (merge {:task task} (dissoc args :task))
             ;; The leases for attenuation come from the provider's closed-over db state:
             ;; if no explicit parent-leases are available, pass [] — spawn still creates
             ;; the session + parent link + event, just with no derived leases.
             res (spawn-subagent! db parent-id child-spec [])]
         {:child/session-id (:child/session-id res)
          :child/capabilities (:child/capabilities res)})))))

(defn agent-status-provider
  "Build the kernel-owned :agent/status provider (component).

  `db` — sqlite handle. normalize validates args, execute returns the
  session map's public fields + child/depth info when available."
  [db]
  (reify proto/Provider
    (describe [_] agent-status-tool-descriptor)
    (normalize-request [_ intent]
      (let [args (tool-args intent)
            _ (validate-args! agent-status-tool-descriptor args)]
        {:tool/id :agent/status
         :resource {:kind :tool :id :agent/status}
         :args args}))
    (execute-request! [_ authorized-request]
      (let [sid-str (get-in authorized-request [:args :session-id])
            sid (try (types/session-id sid-str) (catch Exception _ sid-str))
            sess (try (session/get-session db sid) (catch Exception _ nil))]
        (if-not sess
          {:found false :reason :session-not-found :session/id sid}
          {:found true
           :session/id (:session/id sess)
           :state (session-work-state db (:session/id sess))
           :phenotype/id (:phenotype/id sess)
           :depth (try (subagent-depth db sid) (catch Exception _ nil))
           :children (try (child-session-ids db sid) (catch Exception _ []))})))))
