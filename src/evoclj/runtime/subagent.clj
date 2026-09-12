(ns evoclj.runtime.subagent
  "Subagent spawn — session creation + derived capabilities + parent event (S2, W2).

  W2: Work is the sole durable lifecycle. Spawning a subagent also creates a
  child Work (type :subagent/run, queued -> running) that is the durable handle
  for the child's execution. No bare Future shadows Work: run-subagent! drives
  Work running->succeeded/failed via CAS, and cancellation atomically drives
  Work to cancelled via cancel-work! (compare-and-set on works.state). The DB
  row is the truth; a future is only an internal await inside run-subagent!.
  No ::last-refresh-future is stored and no raw future is leaked.

  spawn-subagent! creates a child Session pinned to the parent's
  Genome/Resolution/Phenotype/Generation (Global Constraint 2, same
  genome/resolution as parent), a new UUID, and one queued child Work.
  Session is immutable identity; the child Work is the lifecycle handle.
  The child's subject is {:principal/type :session :session/id child-id}.

  Child capabilities are derived leases via mint/derive-lease! attenuated
  from the parent's leases (actions ⊆ parent, [W-08..W-11]).  A
  :subagent/spawned event is appended to the parent's causal chain
  (GC-20) with :prev/event-id = parent's latest event id and
  :metadata {:child/session-id child-id}.


  run-subagent! (S3) executes a child session synchronously in its own
  isolated SCI runtime (new phenotype instance, not shared) via the
  scheduler's run-session! with the child's derived leases. The child's
  event chain is independent (per-session seq 1..M) while the parent's
  event records the spawn cause and task bind; the Work.parent_work_id edge
  is the sole durable topology link. Synchronous for tests;
  async callers may wrap in future/command. Child intents go through
  the broker with the child's attenuated leases."
   (:require [clojure.java.jdbc :as jdbc]
             [evoclj.compiler.topology :as topology]
             [evoclj.genome.hash :as hash]
             [evoclj.genome.types :as types]
             [evoclj.kernel.error :as err]
             [evoclj.provider.fixture :as fixture]
             [evoclj.provider.protocol :as proto]
             [evoclj.provider.registry :as registry]
             [evoclj.runtime.phenotype :as phenotype]
             [evoclj.runtime.subagent-capability :as caps]
             [evoclj.runtime.subagent-cancel :as cancel]
             [evoclj.runtime.subagent-lease :as lease]
             [evoclj.store.cas :as cas]
             [evoclj.store.event :as event]
             [evoclj.store.event-schema :as es]
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
;; Plans and delivery records (F1)
;; ---------------------------------------------------------------------------

(defrecord SpawnPlan [child-pin derived-grants task-bind deadline])
(defrecord CancelPlan [target-work-closure revoke-set event-edges])
(defrecord Delivery [child-terminal parent-work causal-link payload-ref])

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(declare auto-deliver-child-terminal!)


(def ^:const max-subagent-depth
  "Maximum nesting depth for subagent chains (S6). Parent depth +1 must be <= this.
  Fail-closed backstop; task-level planning (:task + :capabilities) is primary."
  5)

(def ^:const max-spawns-per-parent
  "Budget cap: maximum direct children per parent (maps to :tool/budget {:max-calls 10}).
  Fail-closed backstop; task-level planning (:task + :capabilities) is primary."
  10)

(defn subagent-depth
  "Depth of session `sid` in the subagent tree. Root (no parent) has depth 0,
  its child has depth 1, etc. Walks the Work graph via work-store/get-parent-session-id."
  [db sid]
  (loop [cur sid depth 0 seen #{}]
    (if (contains? seen cur)
      depth
      (let [parent (try (work-store/get-parent-session-id db cur) (catch Exception _ nil))]
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
    (let [children (work-store/child-session-ids db parent-id)]
      (when (>= (count children) max-spawns-per-parent)
        (throw (err/error :subagent/budget-exceeded
                          (str "subagent budget cap exceeded: parent already has " (count children) " children, max " max-spawns-per-parent)
                          {:parent/session-id parent-id
                           :child/count (count children)
                           :max-calls max-spawns-per-parent})))))
  nil)
;; ---------------------------------------------------------------------------
;; S4 — global lease registry (owned by evoclj.runtime.subagent-lease)
;; ---------------------------------------------------------------------------
;; The registry lives in its own namespace so evoclj.runtime.subagent-cancel
;; can require it directly instead of reaching it reflectively (INV-05).
;; Re-exported here because this is the namespace callers and tests hold.

(def subagent-lease-registry
  "The global subagent lease registry (evoclj.runtime.subagent-lease/...)."
  lease/subagent-lease-registry)

(defn clear-subagent-lease-state!
  "Test helper — clear the global subagent lease registry.
  Delegates to evoclj.runtime.subagent-lease."
  []
  (lease/clear-subagent-lease-state!))


;; ---------------------------------------------------------------------------
;; Task digest bind + deadline (W2: the child Work carries the spawn-time
;; task digest as :work/payload-ref and an optional :work/deadline, so a
;; queued Work is replayable and the executed task is auditable against
;; the spawn-time bind without a CAS handle at spawn time).
;; ---------------------------------------------------------------------------

(defn task-digest
  "Content digest (\"sha256:<hex>\") of the EDN task value: sha256 over the
  exact UTF-8 bytes of (pr-str task) — the same bytes the scheduler's CAS
  put-payload! hashes, so a spawn-time bind equals the run-time
  :session/started payload-ref for the same task value. Nil task binds nil."
  [task]
  (when (some? task)
    (hash/file-digest (.getBytes (pr-str task) java.nio.charset.StandardCharsets/UTF_8))))

(defn coerce-deadline
  "Coerce `d` (nil, java.util.Date, java.time.Instant, ISO-8601 string, or
  epoch millis) to a java.util.Date, or nil when `d` is nil. Throws
  :store/work-invalid for anything else (fail closed — a deadline must be
  an unambiguous instant)."
  [d]
  (cond
    (nil? d) nil
    (instance? Date d) d
    (instance? java.time.Instant d) (Date/from ^java.time.Instant d)
    (integer? d) (Date. ^long (long d))
    (string? d) (Date/from (java.time.Instant/parse d))
    :else (throw (err/error :store/work-invalid "deadline must be a Date, Instant, ISO-8601 string, or epoch millis"
                            {:value (err/sanitize d)}))))

;; ---------------------------------------------------------------------------
;; Spawn
;; ---------------------------------------------------------------------------

(defn- resolve-parent-work-id!
  "Resolve the parent Work id for a spawn of `parent-id`.

  When `work-id` is supplied (the :intent/subagent-spawn path), it must
  exist and belong to `parent-id` (W2: a spawn is caused by exactly one
  parent Work) — otherwise throw :store/work-not-found (missing) or
  :store/work-invalid (belongs to another session). When nil (the
  :agent/spawn tool path, whose model-facing args carry no Work), fall
  back to the parent's latest Work (nil for a root session with none)."
  [db parent-id work-id]
  (if (nil? work-id)
    (some-> (last (work-store/list-works (sqlite/db-spec db) parent-id)) :work/id)
    (let [w (work-store/fetch-work (sqlite/db-spec db) work-id)]
      (when-not w
        (throw (err/error :store/work-not-found
                          (str "parent work not found: " work-id)
                          {:work/id work-id
                           :parent/session-id parent-id})))
      (when (not= parent-id (:work/session-id w))
        (throw (err/error :store/work-invalid
                          "parent work belongs to another session"
                          {:reason :parent-work-mismatch
                           :work/id (:work/id w)
                           :work/session-id (:work/session-id w)
                           :parent/session-id parent-id})))
      (:work/id w))))

(defn spawn-subagent!
  "Create a child session as a subagent of `parent-session-id`.

  `db`              — sqlite spec, string path, or SessionStore handle (must be migrated).
  `parent-session-id` — UUID of the parent session (must exist).
  `child-spec`      — map, kept for audit in the parent event metadata :child/spec (may be empty).
  Its :capabilities (vector of hint strings, e.g. [\"tool:fixture/echo\"]) is a
  NARROWING request: each parent lease meets the request via Grant meet and
  disjoint leases are dropped. The spawn right (:agent/spawn) is denied
  unless explicitly requested. Task-level planning (:task + :capabilities)
  is primary; depth/fanout caps are fail-closed backstops.

  `parent-leases`   — collection of sealed CapabilityLease values granted to the parent (may be empty/nil).

  Returns {:child/session-id uuid
           :child/work-id    uuid (the single queued :subagent/run Work)
           :child/session    session-map
           :child/capabilities [derived-leases]}

  Side effects:
  - inserts an immutable Session identity row with the same :genome/id,
    :resolution/id, :phenotype/id, and :generation/id as the parent
    (pinned identity, never assumes).
  - appends a :session/created root event for the child (so its chain is valid).
  - derives child leases via derive-child-leases (Grant meet, not identity)
  with subject {:principal/type :session :session/id child-id}: with no
  parsable request one attenuated lease per (non-spawn) parent lease;
  with a request only the meets survive (fewer requested caps = fewer
  child leases; attenuation ⊆ parent enforced by derive-lease!).
  - appends a :subagent/spawned event to the parent's chain (cause = parent's
    latest event id) carrying {:child/session-id child-id :child/spec
    child-spec :task/digest <sha256-or-nil>} in its :metadata.
  - creates exactly ONE child Work (:subagent/run, :queued) carrying the
    spawn-time task digest as :work/payload-ref and the spawn deadline
    (child-spec :deadline or opts :deadline) as :work/deadline — the
    durable handle the run, status, cancel, and replay paths resolve.

  Typed errors: :store/session-not-found when parent missing,
  :store/work-not-found when an explicit :parent/work-id is missing,
  :store/work-invalid when it belongs to another session,
  :store/event-invalid for causal failures, :capability/attenuation-invalid
  when a parent lease cannot be attenuated (should not happen for meet
  derivation, which narrows by construction).

  `opts` (optional) carries :parent/work-id — the parent Work the spawn
  is attributed to — and :deadline (fallback when child-spec carries none).
  The 4-arity keeps the legacy latest-Work fallback for the :agent/spawn
  tool path; the :intent/subagent-spawn path requires an explicit
  :parent/work-id (enforced at dispatch, never the latest-Work heuristic)."
  ([db parent-session-id child-spec parent-leases]
   (spawn-subagent! db parent-session-id child-spec parent-leases nil))
  ([db parent-session-id child-spec parent-leases opts]
  (when (nil? db)
    (throw (ex-info "spawn-subagent! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        parent (session/get-session db parent-id)]
    (when-not parent
      (throw (ex-info (str "parent session not found: " parent-id)
                      {:error/type :store/session-not-found
                       :session/id parent-id})))
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
          ;; P1: derive child leases durably — DB INSERT before cache (meet, narrowed)
          derived (caps/derive-child-leases db subagent-lease-registry parent-leases
                                            child-spec child-principal)
          parent-events (event/events-for-session db parent-id)
          latest (last parent-events)
          _ (when-not latest
              (throw (ex-info "parent session has no events; expected :session/created root"
                              {:error/type :store/event-invalid
                               :session/id parent-id})))
          cause-id (:event/id latest)
          spawn-digest (task-digest (:task child-spec))
          deadline (coerce-deadline (or (:deadline child-spec) (:deadline opts)))
          plan (->SpawnPlan child-request derived spawn-digest deadline)
          _ (event/append-event! db
                                 {:session/id parent-id
                                  :generation/id (:generation/id parent)
                                  :phenotype/id (:phenotype/id parent)
                                  :event/type :subagent/spawned
                                  :prev/event-id cause-id
                                  :payload-ref nil
                                  :metadata {:child/session-id child-id
                                             :child/spec child-spec
                                             :task/digest (:task-bind plan)}})
          spec (sqlite/db-spec db)]
      ;; W2: durable child Work (queued) — the SOLE execution identity for
      ;; this child. parent-work-id is the explicit :parent/work-id when
      ;; supplied, else the parent's latest Work (nil only for the root
      ;; session, which has no parent Work). Exactly one Work per spawn:
      ;; the run/status/cancel/replay paths resolve this row and never
      ;; mint a second one.
      (let [parent-work-id (resolve-parent-work-id! db parent-id (:parent/work-id opts))
            wid (java.util.UUID/randomUUID)]
        (work-store/create-work! spec (cond-> {:work/id wid
                                               :work/type :subagent/run
                                               :work/state :queued
                                               :work/session-id child-id
                                               :work/parent-work-id parent-work-id
                                               :work/created-at (java.util.Date.)}
                                        (:task-bind plan) (assoc :work/payload-ref (:task-bind plan))
                                        (:deadline plan) (assoc :work/deadline (:deadline plan))))
        ;; the spawn event above was appended before the Work id existed;
        ;; the returned map (and the Work row itself) is the durable handle.
        ;; create-work! returns the raw works row (keys :id, not :work/id);
        ;; the handle is the minted wid above.
        {:child/session-id child-id
         :child/work-id wid
         :child/session child-session
         :child/capabilities (:derived-grants plan)})))))

;; ---------------------------------------------------------------------------
;; Child execution (S3)
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Child execution (S3) — H1 Hydration factory
;; ---------------------------------------------------------------------------

(declare agent-spawn-provider agent-status-provider agent-cancel-provider subagent-tool-catalog)
(defn- build-child-executor
  "Build an isolated child executor for `child-session` via the
  hydration factory. Delegates to evoclj.runtime.hydrate/hydrate so
  there is exactly one place that loads Genome/Resolution/CodeImage,
  verifies Deployment/Execution ids, loads program sources, materializes
  bindings, and creates a fresh SCI + broker pair. Synthetic
  topology/programs/CAS/fixture lease are owned by the factory, not
  copied here.

  Production catalog wiring (S6/S14): the hydrated registry carries only
  the fixture provider, so the child's own tool loop could never dispatch
  :agent/spawn (nested spawn), :agent/status, or :agent/cancel — a nested
  spawn would fail closed as :provider/not-found. This step registers the
  three kernel-owned agent providers (spawn closed over the child's link
  parent) and resolves subagent-tool-catalog against the registry, so the
  production path consumes the single-sourced catalog instead of leaving
  it test-only. The resolved map is attached as :subagent/tool-catalog."
  [db child-session]
  (let [hydrate @(requiring-resolve 'evoclj.runtime.hydrate/hydrate)
        pin (if (map? child-session) child-session {:session/id child-session})
        executor (hydrate db pin)
        child-id (types/session-id (or (:session/id pin) (:session/id child-session)))
        parent-id (try (work-store/get-parent-session-id db child-id) (catch Exception _ nil))
        reg (get-in executor [:dispatch :registry])]
    (when (and (some? reg) (instance? clojure.lang.Atom reg))
      (registry/register! reg (agent-spawn-provider db parent-id))
      (registry/register! reg (agent-status-provider db))
      (registry/register! reg (agent-cancel-provider db))
      (let [resolved (registry/resolve-tool-catalog reg subagent-tool-catalog)]
        (assoc executor :subagent/tool-catalog resolved)))))

(defn audit-child-task
  "Audit the executed `task` against the spawn-time digest bind carried by
  the child Work `child-work-id`: returns {:spawn/digest <sha256-or-nil>
  :executed/digest <sha256> :match? bool}. A nil spawn bind (older Works
  minted before payload persistence, or a nil spawn task) matches nothing
  and reports :match? false with the executed digest for the record — the
  audit never throws, so ad-hoc runs of a different task than the spawn
  spec stay executable while the divergence is observable."
  [db child-work-id executed-task]
  (let [w (work-store/fetch-work (sqlite/db-spec db) child-work-id)
        spawn-digest (:work/payload-ref w)
        executed-digest (task-digest executed-task)]
    {:spawn/digest spawn-digest
     :executed/digest executed-digest
     :match? (boolean (and (string? spawn-digest) (= spawn-digest executed-digest)))}))

(defn- resolve-child-work-id!
  "Resolve the single execution Work for child session `child-id`.
  When `work-id` is supplied it must exist and belong to `child-id`
  (:store/work-not-found / :store/work-invalid). When nil, the child's
  latest Work is used (nil for a never-spawned bare session, whose run
  mints its own :session/run Work). More than one Work on the child is
  :store/work-invalid :reason :multiple-child-works — one spawn mints
  exactly one Work, so two rows mean a second spawn wrote where only a
  run should read."
  [db child-id work-id]
  (if (some? work-id)
    (let [w (work-store/fetch-work (sqlite/db-spec db) work-id)]
      (when-not w
        (throw (err/error :store/work-not-found "child work not found" {:work/id work-id})))
      (when (not= child-id (:work/session-id w))
        (throw (err/error :store/work-invalid "child work belongs to another session"
                          {:reason :child-work-mismatch
                           :work/id (:work/id w)
                           :work/session-id (:work/session-id w)
                           :child/session-id child-id})))
      (:work/id w))
    (let [works (work-store/list-works db child-id)]
      (when (> (count works) 1)
        (throw (err/error :store/work-invalid "child session has more than one Work"
                          {:reason :multiple-child-works
                           :child/session-id child-id
                           :work/ids (mapv :work/id works)})))
      (some-> (last works) :work/id))))

(defn- enforce-child-deadline!
  "Fail closed when the child Work's :work/deadline has passed: drive the
  row terminal (CAS — :failed from :queued, :timed-out from :running or
  :waiting) and throw :subagent/deadline-exceeded. Nil deadline is a no-op."
  [db child-work-id]
  (when child-work-id
    (let [w (work-store/fetch-work (sqlite/db-spec db) child-work-id)
          deadline (:work/deadline w)]
      (when (and deadline (work-store/deadline-passed? deadline (java.util.Date.)))
        (let [state (:work/state w)]
          (try
            (cond
              (= :queued state) (work-store/fail-work! (sqlite/db-spec db) child-work-id {:error/type :subagent/deadline-exceeded})
              (contains? #{:running :waiting} state) (work-store/timeout-work! (sqlite/db-spec db) child-work-id)
              :else nil)
            (catch Exception _ nil)))
        (throw (err/error :subagent/deadline-exceeded "child Work deadline has passed"
                          {:work/id child-work-id :work/deadline deadline}))))))

(defn run-subagent!
  "Synchronously execute a child subagent session.

  `db`                 — sqlite spec, path, or SessionStore handle (must be migrated).
  `parent-session-id`  — UUID of the parent session (for validation / audit; may be nil).
  `child-session-id`   — UUID of the child session to run (must exist; the
  session row is immutable identity; the child Work owns lifecycle and there
  is no Session lifecycle requirement (W2).
  `task`               — EDN-safe task input (e.g. {:text \"hello\"}) fed as the entry node's payload.
  `work-id`            — (5-arity) the child Work to drive. Must exist and
  belong to the child; nil resolves the child's single Work as the 4-arity does.

  Fetches the child session row, builds a fresh child executor via the
  scheduler's phenotype machinery (same genome/resolution as parent,
  child subject, new isolated SCI runtime), and runs scheduler/run-session!
  with `task` and the resolved child Work id. Returns the scheduler result
  map {:status :completed|:failed|:budget-exhausted ...} with
  :task/audit {:spawn/digest _ :executed/digest _ :match? _} comparing the
  executed task against the spawn-time digest bind (the audit reports,
  never rejects — running a different task than the spawn spec stays
  executable while the divergence is observable).

  Child intents go through the broker with the child's persisted derived leases
  from the capabilities table (P1 DB truth); DB miss means deny, no synthetic lease.
  The child has its own event chain (per-session seq 1..M) independent from
  the parent; the parent event records cause/task metadata, while
  Work.parent_work_id is the topology edge (S2).

  W2: Work's running is execution; a future is only an internal await.
  run-subagent! drives the child Work (queued -> running -> succeeded/failed)
  via CAS and awaits the scheduler's internal future synchronously — no bare
  Future is leaked. For async callers see run-subagent-async! (Work stays
  the truth; the future is only an await handle) and await-child!.

  Throws :subagent/not-found when the child session does not exist,
  :store/work-not-found when an explicit work-id is missing,
  :store/work-invalid when it belongs to another session or the child
  carries more than one Work, :subagent/deadline-exceeded when the child
  Work's deadline has passed."
  ([db parent-session-id child-session-id task]
   (run-subagent! db parent-session-id child-session-id task nil))
  ([db parent-session-id child-session-id task work-id]
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
    (let [child-work-id (resolve-child-work-id! db child-id work-id)]
      (enforce-child-deadline! db child-work-id)
      (let [executor (build-child-executor db child)
            run-session! (or (resolve 'evoclj.runtime.scheduler/run-session!)
                             @(requiring-resolve 'evoclj.runtime.scheduler/run-session!))
            ;; W2: reuse the Work the spawn created — never mint a second one.
            ;; The child's sole execution identity is the :subagent/run Work
            ;; spawn-subagent! persisted (queued); scheduler dispatches it to
            ;; :running. Only when the child was never spawned (bare run,
            ;; work-id nil) does the scheduler create its own :session/run
            ;; Work.
            ;; internal future await: Work's running is execution, future is only await
            result @(future (run-session! executor child-id task child-work-id))
            audit (when child-work-id (audit-child-task db child-work-id task))
            ;; S5 auto-delivery: the runner closes the loop — a settled
            ;; child terminal is delivered to the parent chain without a
            ;; model round-trip. Best-effort evidence under :delivery;
            ;; delivery never fails the run itself.
            delivery (when (and parent-id child-work-id)
                       (auto-deliver-child-terminal! db parent-id child-id child-work-id))]
        (cond-> result
          audit (assoc :task/audit audit)
          child-work-id (assoc :work/id child-work-id)
          delivery (assoc :delivery delivery)))))))

;; ---------------------------------------------------------------------------
;; Async run + parent wait/wakeup join + queued-Work replay (W2)
;; ---------------------------------------------------------------------------

(def ^:const await-poll-ms
  "Poll interval for await-child! Work-state wakeups."
  50)

(defn run-subagent-async!
  "Asynchronously execute a child subagent session: resolves the child Work
  (explicit `work-id` or the child's single Work, same contract as
  run-subagent!) and runs run-subagent! inside a future. Returns
  {:future <the await handle> :child/session-id _ :child/work-id _}.
  W2: the Work row stays the execution truth — the future is only an
  internal await handle, never an observable lifecycle. Join with
  await-child! (durable Work poll) or deref the future directly."
  ([db parent-session-id child-session-id task]
   (run-subagent-async! db parent-session-id child-session-id task nil))
  ([db parent-session-id child-session-id task work-id]
  (when (nil? db)
    (throw (ex-info "run-subagent-async! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [child-id (types/session-id child-session-id)
        child-work-id (resolve-child-work-id! db child-id work-id)
        f (future (run-subagent! db parent-session-id child-id task child-work-id))]
    {:future f :child/session-id child-id :child/work-id child-work-id})))

(defn await-child!
  "Parent wait/wakeup join on a child session: poll the child's latest Work
  until it reaches a terminal state (:succeeded :failed :cancelled
  :timed-out) — the terminal row is the wakeup — and return that Work map.
  `timeout-ms` bounds the wait; expiry throws :subagent/await-timeout
  carrying the last observed :work/state (nil when the child has no Work
  yet). Never blocks on a future: the DB row is the truth, so a crashed
  runner still wakes the waiter via recovery or replay."
  [db child-session-id timeout-ms]
  (let [child-id (types/session-id child-session-id)
        deadline (+ (System/currentTimeMillis) (or timeout-ms 0))]
    (loop []
      (let [w (some-> (last (try (work-store/list-works db child-id)
                                 (catch Exception _ nil)))
                      (#(try (work-store/fetch-work (sqlite/db-spec db) (:work/id %))
                             (catch Exception _ %))))]
        (cond
          (and w (contains? #{:succeeded :failed :cancelled :timed-out} (:work/state w))) w
          (>= (System/currentTimeMillis) deadline)
          (throw (err/error :subagent/await-timeout "timed out waiting for child Work to settle"
                            {:child/session-id child-id
                             :work/state (:work/state w)
                             :work/id (:work/id w)
                             :timeout-ms timeout-ms}))
          :else (do (Thread/sleep await-poll-ms) (recur)))))))

(defn child-task-for-work
  "Recover the spawn-time task for a queued child Work `work-id` from the
  parent's :subagent/spawned event metadata (:child/spec :task) — the
  durable record written at spawn, so a queued Work is replayable after a
  crash without the original caller. Throws :store/work-not-found when the
  Work is missing, :subagent/task-not-found when no spawn event names this
  child session."
  [db work-id]
  (let [spec (sqlite/db-spec db)
        w (work-store/fetch-work spec work-id)]
    (when-not w
      (throw (err/error :store/work-not-found "no work with this id" {:work/id work-id})))
    (let [child-id (:work/session-id w)
          parent-id (try (work-store/get-parent-session-id db child-id) (catch Exception _ nil))
          task (when parent-id
                 (some (fn [ev]
                         (when (and (= :subagent/spawned (:event/type ev))
                                    (= child-id (get-in ev [:metadata :child/session-id])))
                           (get-in ev [:metadata :child/spec :task])))
                       (try (event/events-for-session db parent-id) (catch Exception _ nil))))]
      (if (nil? task)
        (throw (err/error :subagent/task-not-found "no spawn-time task recorded for this child Work"
                          {:work/id (:work/id w) :child/session-id child-id}))
        task))))

(defn poll-queued-children!
  "Durable executor poll loop: sweep expired deadlines, then redeliver queued children.
  First work-store/sweep-expired-deadlines! drives every non-terminal Work whose
  :work/deadline has passed to its timeout terminal (:queued -> :failed,
  :running/:waiting -> :timed-out, best-effort — a sweep failure never stops
  the poll). Then every remaining :queued :subagent/run Work is replayed:
  each one's spawn-time task is recovered via child-task-for-work and run with
  `run-fn` — (fn [db parent-id child-id task work-id]), defaulting to the
  synchronous run-subagent!. Queued orphans left by a crash stay :queued
  (recover-works! never settles them), so a later poll replays them to a
  terminal state. The waiting half of the join is await-child!: a parent
  polls the child's Work row until it reaches a terminal state — the terminal
  row is the wakeup, so a crashed runner still wakes the waiter via recovery
  or this replay. Returns a vector of {:work/id _ :status _} per replayed
  Work, or {:work/id _ :error/type _} when the task is unrecoverable or the
  run throws. Never throws for a single bad row — the loop continues."
  ([db] (poll-queued-children! db nil))
  ([db run-fn]
  (let [run-fn (or run-fn (fn [db parent-id child-id task work-id]
                            (run-subagent! db parent-id child-id task work-id)))]
    (try (work-store/sweep-expired-deadlines! (sqlite/db-spec db)) (catch Exception _ nil))
    (let [queued (try (work-store/fetch-works-by-state (sqlite/db-spec db) :queued)
                      (catch Exception _ []))]
    (into []
           (comp (filter #(= :subagent/run (:work/type %)))
                 (map (fn [w]
                        (let [wid (:work/id w)
                              child-id (:work/session-id w)
                              parent-id (try (work-store/get-parent-session-id db child-id)
                                             (catch Exception _ nil))]
                          (try
                            (let [task (child-task-for-work db wid)
                                  res (run-fn db parent-id child-id task wid)]
                              {:work/id wid :status (:status res)})
                            (catch clojure.lang.ExceptionInfo e
                              {:work/id wid :error/type (:error/type (ex-data e))})
                            (catch Throwable t
                              {:work/id wid :error/type :subagent/replay-failed
                               :error/message (ex-message t)}))))))
           queued)))))

(defn cancel-subagent!
  "Cancel a single child subagent session `child-session-id` spawned from
  `parent-session-id`. Delegates to evoclj.runtime.subagent-cancel/cancel-subagent!."
  [db parent-session-id child-session-id reason]
  (cancel/cancel-subagent! db parent-session-id child-session-id reason))

(defn cancel-subagent-tree!
  "Cascade-cancel the entire subtree rooted at `root-session-id`.
  Delegates to evoclj.runtime.subagent-cancel/cancel-subagent-tree!."
  [db root-session-id reason]
  (cancel/cancel-subagent-tree! db root-session-id reason))

(defn cancel-non-terminal-children!
  "Structured-concurrency enforcement: cancel every live child of
  `session-id`. Delegates to evoclj.runtime.subagent-cancel/cancel-non-terminal-children!."
  ([db session-id] (cancel/cancel-non-terminal-children! db session-id :parent-cancel cancel-subagent!))
  ([db session-id reason] (cancel/cancel-non-terminal-children! db session-id reason cancel-subagent!)))
;; ---------------------------------------------------------------------------
;; S5 — result delivery to parent chain
;; ---------------------------------------------------------------------------

(defn- sha256-cas-ref?
  [s]
  (and (string? s) (boolean (re-matches #"^sha256:[0-9a-f]{64}$" s))))

(defn- delivered-result-event
  "The existing :subagent/result event on `parent-id` that already names
  `terminal-event-id` in its metadata, or nil. Delivery is idempotent:
  re-delivering the same child terminal returns the recorded event
  instead of appending a duplicate."
  [db parent-id terminal-event-id]
  (some (fn [ev]
          (when (and (= :subagent/result (:event/type ev))
                     (= terminal-event-id (get-in ev [:metadata :terminal/event-id])))
            ev))
        (try (event/events-for-session db parent-id) (catch Exception _ nil))))

(defn- check-delivery-terminal!
  "Provenance gate shared by both delivery paths: `terminal-event-id`
  must name the child's LATEST event and it must carry `expected-type`
  (:session/completed or :session/failed). Returns the terminal event."
  [db child-id terminal-event-id expected-type]
  (let [terminal (try (event/get-event-by-id db terminal-event-id) (catch Exception _ nil))]
    (when-not terminal
      (throw (err/error :store/event-invalid "delivery terminal event not found"
                        {:terminal/event-id terminal-event-id :child/session-id child-id})))
    (when-not (and (= child-id (:session/id terminal))
                   (= expected-type (:event/type terminal)))
      (throw (err/error :store/event-invalid "delivery terminal is not the child's expected terminal event"
                        {:terminal/event-id terminal-event-id
                         :child/session-id child-id
                         :event/session-id (:session/id terminal)
                         :event/type (:event/type terminal)
                         :expected/type expected-type})))
    (let [latest-id (some-> (last (try (event/events-for-session db child-id)
                                       (catch Exception _ nil)))
                            :event/id)]
      (when-not (= terminal-event-id latest-id)
        (throw (err/error :store/event-invalid "delivery terminal is not the child's latest event (stale link)"
                          {:terminal/event-id terminal-event-id
                           :latest/event-id latest-id
                           :child/session-id child-id}))))
    terminal))

(defn- append-result-event-tx!
  "Append the :subagent/result event to `parent-id` inside ONE BEGIN
  IMMEDIATE transaction that FIRST re-checks the child Work is still in
  `expected-state` (the provenance CAS — a concurrent cancel/settle that
  moved the row aborts delivery instead of recording a lie). The causal
  link points at the explicit `terminal-event-id`, never 'the latest'.
  Returns the appended event."
  [db parent child-work-id terminal-event-id expected-state metadata]
  (let [parent-id (:session/id parent)
        spec (sqlite/db-spec db)]
    (sqlite/with-write-tx [conn spec]
      (let [row (first (sqlite/query-raw! conn "SELECT state FROM works WHERE id = ?" [(str child-work-id)]))
            state (:state row)]
        (when-not (= ({:succeeded "succeeded" :failed "failed"} expected-state) state)
          (throw (err/error (if (= :succeeded expected-state) :subagent/not-completed :subagent/not-failed)
                            "child work left its terminal state inside the delivery transaction"
                            {:work/id child-work-id :work/state state}))))
      (let [prev-id (event/latest-event-id-on-conn conn parent-id)]
        (when-not prev-id
          (throw (err/error :store/event-invalid "parent session has no events"
                            {:session/id parent-id})))
        (let [req {:session/id parent-id
                   :generation/id (:generation/id parent)
                   :phenotype/id (:phenotype/id parent)
                   :event/type :subagent/result
                   :prev/event-id prev-id
                   :causal-links #{{:from terminal-event-id :type :subagent/result}}
                   :payload-ref nil
                   :metadata metadata}]
          (es/validate-append-request req)
          (event/append-event-on-conn! conn req))))))

(defn deliver-result-for-works!
  "Canonical Work-handle success delivery: link child Work `child-work-id`
  (must be :succeeded) to its parent under an explicit terminal event.

  `parent-session-id` names the receiving session; `parent-work-id` names
  the exact parent Work (nil for legacy parents that own no Work — the
  link-equality check is then skipped, child provenance still enforced).
  `terminal-event-id` must be the child's latest event and a
  :session/completed; `cas-ref` must be sha256:<64 hex> AND equal the
  child Work's :work/payload-ref whenever the row carries one (the
  scheduler persists the output CAS ref on success — a caller-supplied
  ref that differs is :store/cas-mismatch, never recorded).

  The event carries {:child/session-id _ :child/work-id _
  :terminal/event-id _ :result/cas-ref _ :result/status :succeeded} and a
  causal link from the terminal event. Commit is one BEGIN IMMEDIATE
  transaction (provenance re-check + append). Idempotent: re-delivering
  the same terminal returns the recorded event.

  Typed errors: :store/session-invalid (nil db), :store/work-not-found,
  :subagent/not-found (child session missing), :store/session-not-found
  (parent missing), :subagent/not-completed, :store/work-invalid (child
  not attributed to the parent work), :store/cas-invalid,
  :store/cas-mismatch, :store/event-invalid (unknown, foreign, stale, or
  non-terminal event)."
  [db parent-session-id parent-work-id child-work-id terminal-event-id cas-ref]
  (when (nil? db)
    (throw (ex-info "deliver-result-for-works! requires a db/store handle" {:error/type :store/session-invalid})))
  (when-not (sha256-cas-ref? cas-ref)
    (throw (ex-info (str "invalid cas-ref: " cas-ref)
                    {:error/type :store/cas-invalid
                     :cas-ref cas-ref})))
  (let [spec (sqlite/db-spec db)
        child-work (work-store/fetch-work spec child-work-id)]
    (when-not child-work
      (throw (err/error :store/work-not-found "child work not found" {:work/id child-work-id})))
    (when-not (= :succeeded (:work/state child-work))
      (throw (err/error :subagent/not-completed (str "child not completed: work=" (:work/state child-work))
                        {:work/id child-work-id
                         :work/state (:work/state child-work)})))
    (let [parent-work (when parent-work-id (work-store/fetch-work spec parent-work-id))]
      (when (and parent-work-id (not parent-work))
        (throw (err/error :store/work-not-found "parent work not found" {:work/id parent-work-id})))
      (when (and parent-work
                 (:work/parent-work-id child-work)
                 (not= (:work/id parent-work) (:work/parent-work-id child-work)))
        (throw (err/error :store/work-invalid "child work is not attributed to the parent work"
                          {:reason :work-link-mismatch
                           :work/id child-work-id
                           :parent/work-id parent-work-id
                           :work/parent-work-id (:work/parent-work-id child-work)})))
      (when (and parent-work
                 (not= (:work/session-id parent-work)
                        (try (types/session-id parent-session-id) (catch Exception _ parent-session-id))))
        (throw (err/error :store/work-invalid "parent work belongs to another session"
                          {:reason :parent-work-mismatch
                           :parent/work-id parent-work-id
                           :parent/session-id parent-session-id})))
      (let [child-id (:work/session-id child-work)
            parent-id (try (types/session-id parent-session-id) (catch Exception _ parent-session-id))]
        (when-not (session/get-session db child-id)
          (throw (ex-info (str "child session not found: " child-id)
                          {:error/type :subagent/not-found
                           :session/id child-id})))
        (let [parent (session/get-session db parent-id)]
          (when-not parent
            (throw (ex-info (str "parent session not found: " parent-id)
                            {:error/type :store/session-not-found
                             :session/id parent-id})))
          (check-delivery-terminal! db child-id terminal-event-id :session/completed)
          (let [bound (:work/payload-ref child-work)]
            (when (and (string? bound) (not= bound cas-ref))
              (throw (err/error :store/cas-mismatch "supplied cas-ref differs from the child Work payload_ref"
                                {:work/id child-work-id
                                 :work/payload-ref bound
                                 :result/cas-ref cas-ref}))))
          (let [delivery (->Delivery terminal-event-id parent-work-id
                                    {:from terminal-event-id :type :subagent/result}
                                    cas-ref)]
            (if-let [existing (delivered-result-event db parent-id (:child-terminal delivery))]
              existing
              (append-result-event-tx! db parent child-work-id (:child-terminal delivery) :succeeded
                                       {:child/session-id child-id
                                        :child/work-id child-work-id
                                        :terminal/event-id (:child-terminal delivery)
                                        :result/cas-ref (:payload-ref delivery)
                                        :result/status :succeeded}))))))))

(defn deliver-failure-for-works!
  "Canonical Work-handle failure delivery: link child Work `child-work-id`
  (must be :failed) to its parent under an explicit terminal event. The
  recorded :error is DERIVED from the canonical rows — the child terminal
  :session/failed event's metadata (:error/artifact-ref, :error/type,
  :status) — never trusted from the caller. `opts` may carry
  :error/fallback, used only when the terminal event records no error
  detail (else {:error/type :subagent/child-failed}). Same atomicity,
  idempotency, and typed-error contract as deliver-result-for-works!
  (with :subagent/not-failed for a non-failed child)."
  ([db parent-session-id parent-work-id child-work-id terminal-event-id]
   (deliver-failure-for-works! db parent-session-id parent-work-id child-work-id terminal-event-id nil))
  ([db parent-session-id parent-work-id child-work-id terminal-event-id opts]
   (when (nil? db)
     (throw (ex-info "deliver-failure-for-works! requires a db/store handle" {:error/type :store/session-invalid})))
   (let [spec (sqlite/db-spec db)
         child-work (work-store/fetch-work spec child-work-id)]
     (when-not child-work
       (throw (err/error :store/work-not-found "child work not found" {:work/id child-work-id})))
     (when-not (= :failed (:work/state child-work))
       (throw (err/error :subagent/not-failed (str "child not failed: work=" (:work/state child-work))
                         {:work/id child-work-id
                          :work/state (:work/state child-work)})))
     (let [parent-work (when parent-work-id (work-store/fetch-work spec parent-work-id))]
       (when (and parent-work-id (not parent-work))
         (throw (err/error :store/work-not-found "parent work not found" {:work/id parent-work-id})))
       (when (and parent-work
                  (:work/parent-work-id child-work)
                  (not= (:work/id parent-work) (:work/parent-work-id child-work)))
         (throw (err/error :store/work-invalid "child work is not attributed to the parent work"
                           {:reason :work-link-mismatch
                            :work/id child-work-id
                            :parent/work-id parent-work-id
                            :work/parent-work-id (:work/parent-work-id child-work)})))
       (let [child-id (:work/session-id child-work)
             parent-id (try (types/session-id parent-session-id) (catch Exception _ parent-session-id))]
         (when-not (session/get-session db child-id)
           (throw (ex-info (str "child session not found: " child-id)
                           {:error/type :subagent/not-found
                            :session/id child-id})))
         (let [parent (session/get-session db parent-id)]
           (when-not parent
             (throw (ex-info (str "parent session not found: " parent-id)
                             {:error/type :store/session-not-found
                              :session/id parent-id})))
           (let [terminal (check-delivery-terminal! db child-id terminal-event-id :session/failed)
                 derived (select-keys (:metadata terminal) [:error/artifact-ref :error/type :status])
                 fallback (:error/fallback opts)
                 error (cond (seq derived) (cond-> derived
                                             (and fallback (not= fallback derived))
                                             (assoc :error/caller fallback))
                             (some? fallback) fallback
                             :else {:error/type :subagent/child-failed})]
            (let [delivery (->Delivery terminal-event-id parent-work-id
                                      {:from terminal-event-id :type :subagent/result}
                                      nil)]
              (if-let [existing (delivered-result-event db parent-id (:child-terminal delivery))]
                existing
                (append-result-event-tx! db parent child-work-id (:child-terminal delivery) :failed
                                         {:child/session-id child-id
                                          :child/work-id child-work-id
                                          :terminal/event-id (:child-terminal delivery)
                                          :result/status :failed
                                          :error error}))))))))))

(defn deliver-result!
  "Deliver a successful child subagent result to its parent's causal chain
  (legacy session-id entry point — resolves the child's single Work, the
  attributed (or latest) parent Work, and the child's latest event, then
  delegates to deliver-result-for-works!). Prefer the Work-handle
  canonical path for new callers.

  E1: appends a :subagent/result event to the parent's chain with
  `:prev/event-id = parent's latest` and
  `:causal-links #{ {:from <child-terminal-event-id> :type :subagent/result} }`
  so cross-session causality is explicit in the graph, not overloaded
  onto prev. The event also carries :child/work-id and
  :terminal/event-id.

  The supplied `cas-ref` must equal the child Work's :work/payload-ref
  whenever the row carries one (:store/cas-mismatch otherwise).

  Typed errors: :store/session-invalid when db nil, :store/session-not-found
  when parent missing, :subagent/not-found when child missing,
  :subagent/not-completed when child is not :succeeded, :store/cas-invalid
  when cas-ref is not sha256:<64 hex>, :store/cas-mismatch on provenance
  drift, :store/work-not-found / :store/work-invalid on handle problems,
  :store/event-invalid on terminal problems."
  [db parent-session-id child-session-id cas-ref]
  (when (nil? db)
    (throw (ex-info "deliver-result! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        child-id (types/session-id child-session-id)]
    (when-not (sha256-cas-ref? cas-ref)
      (throw (ex-info (str "invalid cas-ref: " cas-ref)
                      {:error/type :store/cas-invalid
                       :cas-ref cas-ref})))
    (when-not (session/get-session db child-id)
      (throw (ex-info (str "child session not found: " child-id)
                      {:error/type :subagent/not-found
                       :session/id child-id})))
    (let [child-work-id (resolve-child-work-id! db child-id nil)
          child-work (when child-work-id (work-store/fetch-work (sqlite/db-spec db) child-work-id))]
      (when-not (and child-work (= :succeeded (:work/state child-work)))
        (throw (ex-info (str "child not completed: " child-id " work=" (:work/state child-work))
                        {:error/type :subagent/not-completed
                         :session/id child-id
                         :work/state (:work/state child-work)})))
      (when-not (session/get-session db parent-id)
        (throw (ex-info (str "parent session not found: " parent-id)
                        {:error/type :store/session-not-found
                         :session/id parent-id})))
      (let [parent-work-id (or (:work/parent-work-id child-work)
                               (some-> (last (work-store/list-works db parent-id)) :work/id))
            terminal-id (some-> (last (try (event/events-for-session db child-id)
                                           (catch Exception _ nil)))
                                :event/id)]
        (when-not terminal-id
          (throw (err/error :store/event-invalid "child session has no terminal event"
                            {:session/id child-id})))
        (deliver-result-for-works! db parent-id parent-work-id child-work-id terminal-id cas-ref)))))

(defn deliver-failure!
  "Deliver a failed child subagent result to its parent's causal chain
  (legacy session-id entry point — resolves handles, then delegates to
  deliver-failure-for-works!). The recorded :error is derived from the
  canonical child terminal event; the supplied `error` is kept only as a
  fallback when the terminal records none.

  E1: appends :subagent/result with prev = parent latest and
  causal-links #{ {:from <child-terminal> :type :subagent/result} }.
  Typed errors mirror deliver-result! but with :subagent/not-failed when child not failed."
  [db parent-session-id child-session-id error]
  (when (nil? db)
    (throw (ex-info "deliver-failure! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [parent-id (types/session-id parent-session-id)
        child-id (types/session-id child-session-id)]
    (when-not (session/get-session db child-id)
      (throw (ex-info (str "child session not found: " child-id)
                      {:error/type :subagent/not-found
                       :session/id child-id})))
    (let [child-work-id (resolve-child-work-id! db child-id nil)
          child-work (when child-work-id (work-store/fetch-work (sqlite/db-spec db) child-work-id))]
      (when-not (and child-work (= :failed (:work/state child-work)))
        (throw (ex-info (str "child not failed: " child-id " work=" (:work/state child-work))
                        {:error/type :subagent/not-failed
                         :session/id child-id
                         :work/state (:work/state child-work)})))
      (when-not (session/get-session db parent-id)
        (throw (ex-info (str "parent session not found: " parent-id)
                        {:error/type :store/session-not-found
                         :session/id parent-id})))
      (let [parent-work-id (or (:work/parent-work-id child-work)
                               (some-> (last (work-store/list-works db parent-id)) :work/id))
            terminal-id (some-> (last (try (event/events-for-session db child-id)
                                           (catch Exception _ nil)))
                                :event/id)]
        (when-not terminal-id
          (throw (err/error :store/event-invalid "child session has no terminal event"
                            {:session/id child-id})))
        (deliver-failure-for-works! db parent-id parent-work-id child-work-id terminal-id
                                    (when (some? error) {:error/fallback error}))))))

(defn- auto-deliver-child-terminal!
  "Auto-deliver a settled child's terminal to its parent (structured
  result handoff — the runner, not the model, closes the loop). Reads
  the child Work's own :work/parent-work-id for the parent handle and
  the child's latest event for the terminal; success delivers with the
  Work's persisted payload ref as the CAS ref, failure derives the error
  from the canonical terminal event. Never throws: returns
  {:delivered true :event/id _} or {:delivered false :reason _
  [:work/state _] [:error/type _]} for the run result's :delivery."
  [db parent-id child-id child-work-id]
  (try
    (let [child-work (work-store/fetch-work (sqlite/db-spec db) child-work-id)]
      (cond
        (nil? child-work)
        {:delivered false :reason :work-not-found :work/id child-work-id}
        ;; A nil :work/parent-work-id (legacy tool-path spawns against a
        ;; workless parent) still delivers — the canonical path skips only
        ;; the link-equality check, child provenance still enforced.
        :else
        (let [terminal-id (some-> (last (try (event/events-for-session db child-id)
                                             (catch Exception _ nil)))
                                  :event/id)]
          (if-not terminal-id
            {:delivered false :reason :no-terminal-event}
            (case (:work/state child-work)
              :succeeded
              (if-let [cas (:work/payload-ref child-work)]
                {:delivered true
                 :event/id (:event/id (deliver-result-for-works! db parent-id (:work/parent-work-id child-work)
                                                                 child-work-id terminal-id cas))}
                {:delivered false :reason :no-payload-ref :work/id child-work-id})
              :failed
              {:delivered true
               :event/id (:event/id (deliver-failure-for-works! db parent-id (:work/parent-work-id child-work)
                                                                 child-work-id terminal-id))}
              {:delivered false :reason :not-terminal :work/state (:work/state child-work)})))))
    (catch clojure.lang.ExceptionInfo e
      {:delivered false :reason :delivery-error :error/type (:error/type (ex-data e))})
    (catch Throwable t
      {:delivered false :reason :delivery-error :error/type :subagent/delivery-failed})))

;; ---------------------------------------------------------------------------
;; Agent tool surface (S6) — single-sourced from evoclj.tool.specs
;; ---------------------------------------------------------------------------
;; The Malli arg/output schemas, the canonical C-Tool descriptors, and the
;; wire catalog entries are defined ONCE in tool.specs (INV-05). Everything
;; below aliases those definitions — no descriptor map is duplicated here.
;; Providers describe via the canonical maps (identical values) and
;; validate args against the canonical :input-schema.
(def AgentSpawnArgsSchema
  "Alias of tool.specs/AgentSpawnArgsSchema (single source)."
  tool.specs/AgentSpawnArgsSchema)

(def AgentSpawnOutputSchema
  "Alias of tool.specs/AgentSpawnOutputSchema (single source)."
  tool.specs/AgentSpawnOutputSchema)

(def AgentStatusArgsSchema
  "Alias of tool.specs/AgentStatusArgsSchema (single source)."
  tool.specs/AgentStatusArgsSchema)

(def AgentStatusOutputSchema
  "Alias of tool.specs/AgentStatusOutputSchema (single source)."
  tool.specs/AgentStatusOutputSchema)

(def AgentCancelArgsSchema
  "Alias of tool.specs/AgentCancelArgsSchema (single source)."
  tool.specs/AgentCancelArgsSchema)

(def AgentCancelOutputSchema
  "Alias of tool.specs/AgentCancelOutputSchema (single source)."
  tool.specs/AgentCancelOutputSchema)

(def agent-spawn-tool-descriptor
  "Alias of the canonical tool.specs/agent-spawn-tool (:effect :write —
  spawn persists session row + event + link + exactly one queued child Work)."
  tool.specs/agent-spawn-tool)

(def agent-status-tool-descriptor
  "Alias of the canonical tool.specs/agent-status-tool (read-only over
  the Work lifecycle)."
  tool.specs/agent-status-tool)

(def agent-cancel-tool-descriptor
  "Alias of the canonical tool.specs/agent-cancel-tool (atomic durable
  Work CAS plus cascade revoke)."
  tool.specs/agent-cancel-tool)

(def subagent-tool-catalog
  "The tool catalog the scheduler's tool loop consumes for subagents:
  the three S6 wire tools, single-sourced from tool.specs (wire form
  {:name :description :parameters :tool}). build-child-executor registers
  the matching providers and resolves this catalog against the child
  executor's registry (S14 pin→provider enforcement), so the production
  path consumes the catalog instead of leaving it test-only."
  [tool.specs/agent-spawn-wire-tool tool.specs/agent-status-wire-tool tool.specs/agent-cancel-wire-tool])

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
  the canonical resource {:kind :tool :id :agent/spawn}. Authorization is
  exact-lease: the broker allows only a lease whose principal equals the
  requesting session and whose resource is exactly {:kind :tool
  :id :agent/spawn} (I2, Global Constraint 9).
  execute-request! calls spawn-subagent! with the task, attenuating the
  BROKER's leases (carried on the authorized request by the pipeline —
  never empty-by-construction) into child leases, and returns
  {:child/session-id <uuid> :child/work-id <uuid> :child/capabilities [...]}
  (EDN-safe). The tool path keeps the latest-Work parent fallback (model
  args carry no Work); the Intent path requires an explicit :parent/work-id.
  Depth/budget caps are enforced by spawn-subagent! itself."
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
            ;; child-spec carries the task text plus the :capabilities narrowing
            ;; request (Grant meet in spawn-subagent!, not audit metadata)
            child-spec (cond-> {:task task}
                         (:capabilities args) (assoc :capabilities (:capabilities args)))
            ;; Narrow the broker's leases into the child (P1 durable meet
            ;; derive). The pipeline carries the authorizing leases on the
            ;; authorized request; without them the child is unleasable and
            ;; every child intent denies — fail closed, never mint ambient
            ;; authority.
            parent-leases (or (:leases authorized-request) [])
            res (spawn-subagent! db parent-id child-spec parent-leases)]
         {:child/session-id (:child/session-id res)
          :child/work-id (:child/work-id res)
          :child/capabilities (:child/capabilities res)})))))
(defn- resolve-status-target
  "Resolve an :agent/status target from `args`: :work-id (first-class Work
  handle — the Work must exist, resolved to its owning session) or
  :session-id. Returns {:session/id _ :work/id-or-nil _}. Throws
  :provider/request-invalid when neither handle is present,
  :store/work-not-found when the Work handle names no row."
  [db args]
  (let [work-str (:work-id args)]
    (if (some? work-str)
      (let [wid (try (UUID/fromString (str work-str)) (catch Exception _ work-str))
            w (try (work-store/fetch-work (sqlite/db-spec db) wid) (catch Exception _ nil))]
        (when-not w
          (throw (err/error :store/work-not-found "status Work handle names no row"
                            {:work/id work-str})))
        {:session/id (:work/session-id w) :work/id (:work/id w)})
      (let [sid-str (:session-id args)]
        (when-not sid-str
          (throw (err/error :provider/request-invalid
                            "agent/status requires :session-id or :work-id"
                            {:value (err/sanitize args)})))
        {:session/id (try (types/session-id sid-str) (catch Exception _ sid-str))
         :work/id nil}))))

(defn agent-status-provider
  "Build the kernel-owned :agent/status provider (component).

  `db` — sqlite handle. normalize validates args and binds the requesting
  session (the intent's :session/id) as :requester/session-id. execute
  resolves the target from :session-id or :work-id (first-class Work
  handle → owning session) and returns the session map's public fields +
  child/depth info when available, but ONLY for descendants of the
  requester: any other target (self, parent, sibling, unrelated) fails
  closed with :capability/scope-denied. A missing session still reports
  {:found false} (the pre-existing contract)."
  [db]
  (reify proto/Provider
    (describe [_] agent-status-tool-descriptor)
    (normalize-request [_ intent]
      (let [args (tool-args intent)
            _ (validate-args! agent-status-tool-descriptor args)]
        {:tool/id :agent/status
         :resource {:kind :tool :id :agent/status}
         :args args
         :requester/session-id (:session/id intent)}))
    (execute-request! [_ authorized-request]
      (let [requester (:requester/session-id authorized-request)]
        (when-not requester
          (throw (err/error :provider/request-invalid
                            "agent/status requires the requesting session id (intent :session/id)"
                            {:value (err/sanitize authorized-request)})))
        (let [target (resolve-status-target db (:args authorized-request))
              sid (:session/id target)
              sess (try (session/get-session db sid) (catch Exception _ nil))]
          (if-not sess
            (cond-> {:found false :reason :session-not-found :session/id sid}
              (:work/id target) (assoc :work/id (:work/id target)))
            (let [descendants (try (set (work-store/list-descendants db requester))
                                   (catch Exception _ #{}))]
              (when-not (contains? descendants sid)
                (throw (err/error :capability/scope-denied
                                  "agent/status is descendant-scoped: the target is not a descendant of the requesting session"
                                  {:requester/session-id requester
                                   :target/session-id sid})))
              (cond-> {:found true
                       :session/id (:session/id sess)
                       :state (some-> (last (work-store/list-works (sqlite/db-spec db) (:session/id sess))) :work/state)
                       :phenotype/id (:phenotype/id sess)
                       :depth (try (subagent-depth db sid) (catch Exception _ nil))
                       :children (try (work-store/child-session-ids db sid) (catch Exception _ []))}
                (:work/id target) (assoc :work/id (:work/id target))))))))))

(defn- resolve-cancel-target
  "Resolve an :agent/cancel target from `args`: :work-id (first-class Work
  handle — the Work must exist, resolved to its owning session) or
  :session-id. Returns {:session/id _ :work/id-or-nil _}. Throws
  :provider/request-invalid when neither handle is present,
  :store/work-not-found when the Work handle names no row."
  [db args]
  (let [work-str (:work-id args)]
    (if (some? work-str)
      (let [wid (try (UUID/fromString (str work-str)) (catch Exception _ work-str))
            w (try (work-store/fetch-work (sqlite/db-spec db) wid) (catch Exception _ nil))]
        (when-not w
          (throw (err/error :store/work-not-found "cancel Work handle names no row"
                            {:work/id work-str})))
        {:session/id (:work/session-id w) :work/id (:work/id w)})
      (let [sid-str (:session-id args)]
        (when-not sid-str
          (throw (err/error :provider/request-invalid
                            "agent/cancel requires :session-id or :work-id"
                            {:value (err/sanitize args)})))
        {:session/id (try (types/session-id sid-str) (catch Exception _ sid-str))
         :work/id nil}))))

(defn agent-cancel-provider
  "Build the kernel-owned :agent/cancel provider (component).

  `db` — sqlite handle. normalize validates args and binds the requesting
  session (the intent's :session/id) as :requester/session-id. execute
  resolves the target from :session-id or :work-id (first-class Work
  handle → owning session) and cancels the subtree via cancel-subagent!
  (atomic revoke + Work CAS + events), but ONLY for descendants of the
  requester: any other target (self, parent, sibling, unrelated) fails
  closed with :capability/scope-denied — the same ancestor scope the
  :intent/subagent-cancel path enforces. :reason is optional
  (default :user-request)."
  [db]
  (reify proto/Provider
    (describe [_] agent-cancel-tool-descriptor)
    (normalize-request [_ intent]
      (let [args (tool-args intent)
            _ (validate-args! agent-cancel-tool-descriptor args)]
        {:tool/id :agent/cancel
         :resource {:kind :tool :id :agent/cancel}
         :args args
         :requester/session-id (:session/id intent)}))
    (execute-request! [_ authorized-request]
      (let [requester (:requester/session-id authorized-request)]
        (when-not requester
          (throw (err/error :provider/request-invalid
                            "agent/cancel requires the requesting session id (intent :session/id)"
                            {:value (err/sanitize authorized-request)})))
        (let [target (resolve-cancel-target db (:args authorized-request))
              sid (:session/id target)
              descendants (try (set (work-store/list-descendants db requester))
                               (catch Exception _ #{}))]
          (when-not (contains? descendants sid)
            (throw (err/error :capability/scope-denied
                              "agent/cancel is descendant-scoped: the target is not a descendant of the requesting session"
                              {:requester/session-id requester
                               :target/session-id sid})))
          (let [reason (or (:reason (:args authorized-request)) :user-request)
                res (cancel-subagent! db requester sid reason)]
            (cond-> {:cancelled (:cancelled res)
                     :already-cancelled? (:already-cancelled? res)
                     :session/id sid}
              (:work/id target) (assoc :work/id (:work/id target)))))))))
