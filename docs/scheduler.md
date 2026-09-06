# Scheduler Concurrency Semantics (component)

**Scope:** `evoclj.runtime.scheduler/run-session!`, the executor map it
consumes, and the store components it writes through
(`evoclj.store.event/append-event!`,
`evoclj.store.session/transition-session!`, `evoclj.store.cas`).
This document describes what is **serialized** and what is
**concurrent** in the v0 single-session scheduler, and what the component stress test (`test/evoclj/runtime/scheduler_stress_test.clj`)
proves about it. The doc is the contract: it matches the code, and
the stress test verifies the claims below under real concurrency.

## 1. The v0 model: deterministic single-session FIFO

`run-session!` executes **one** session against the phenotype
topology the executor carries. Within one call:

- node visits are strictly sequential — the walk starts at the
  topology's `:entry` and follows `:next` edges one node at a time;
- each step's events are **fully persisted before the scheduler
  advances** to the next node (component Step 3) — `:node/started`,
  `:node/completed`, and every intent-effect event for that node are
  appended to the store first;
- the session's causal log is a single **linear chain**: every event's
  `:prev/event-id` is its immediate predecessor (`:event/seq = prev-seq + 1`,
  same session), anchored on a `:session/created` root the host appended at
  creation time; cross-session causality travels separately in `:causal-links`
  (E1 split);
- the topology's `:limits {:max-steps N}` bounds the walk;
  `:loop` iteration counters travel in the scheduler's per-session
  `:loop-state` (session-local data, never a SCI global var).

The scheduler spawns **no threads**. It is a pure per-session driver;
concurrency (or its absence) is decided by the host that calls it.

## 2. What is serialized (within a session)

| Concern | Mechanism |
| Event persistence | `append-event!` runs each append in one `BEGIN IMMEDIATE` transaction: seq allocation (`max(seq)+1`), prev validation (strict immediate predecessor), `prev-hash` linkage, v2 hash computation, row insert — atomic, never interleaved |
| Node execution | FIFO; one node completes before the next is stepped |
| Session state | compare-and-set `transition-session!` hops: `:created → :resolving → :running → :waiting → :completed` (or `:failed` / `:budget-exhausted`) |
| SCI runtime use | one session at a time — a SCI runtime is not thread-safe: one live session per runtime (the stress test builds exactly this shape) |
| Intent effect transaction | per intent: `:intent/proposed` → broker dispatch (one call) → `:intent/authorized` + `:provider/call-started` + `:provider/call-completed`, or `:intent/denied` / `:intent/failed` — persisted before the session continues |
| Loop state | per-session `:loop-state` map, built fresh by every `run-session!` call |

## 3. What is concurrent (across sessions)

The scheduler does not serialize different sessions with each other.
A host may run N sessions **in parallel** — one `run-session!` per
thread — and the following shared components handle the contention:

| Shared component | Concurrency behavior |
| --- | --- |
| SQLite store | `append-event!` takes SQLite's write lock up front (`BEGIN IMMEDIATE`) with `busy_timeout = 10000`; contended appends **wait** instead of failing with `SQLITE_BUSY`. Sequence allocation is per-session, so parallel appends can never collide or interleave inside one transaction |
| CAS | content-addressed: identical bytes map to one artifact id; writes are atomic (temp file + rename) and idempotent; concurrent writers of the same payload converge |
| Provider registry / broker context | shared host components; the fixture provider's execution counter and the broker's usage map are atoms |
| Reads | `events-for-session`, `get-session`, `verify-event-chain` are reads and run concurrently with appends |

The one hard rule the host must respect: **a concurrently running
session must not share a SCI runtime with another running session**.
Each parallel session gets its own isolated SCI runtime (its own Phenotype
instance); the store is shared. The stress test builds exactly this
shape — N per-session executors over ONE shared sqlite db, CAS, and
registry.

## 4. Isolation guarantees (what the stress test proves)

For N sessions × M events run concurrently:

1. **Hash chain valid** — `verify-event-chain` re-derives every
   stored event's hash from its own canonical header and checks the
   `prev-hash` linkage per session; it must pass for every session.
2. **No cross-session leakage** — every event row carries its own
   session id and the session's pinned generation/phenotype identity
   (Global Constraint 20); the store enforces that every non-root
   `:prev/event-id` is the **immediate predecessor in the same
   session** (`:event/seq = prev-seq + 1`; `:store/prev-not-immediate` /
   `:store/prev-session-mismatch` otherwise); per-session
   `:event/seq` is exactly `1..M` with no gaps or duplicates; session
   pins (genome/resolution/phenotype) are immutable after insert
   (Global Constraint 2).
3. **No lost events** — each append is atomic (all-or-nothing inside
   the `BEGIN IMMEDIATE` transaction) and waits on contention, so the
   per-session event count and the global total are exact, and the
   shared provider executes exactly once per requested tool call.

## 5. The component stress test

`test/evoclj/runtime/scheduler_stress_test.clj` runs N sessions (8 by
default) concurrently behind a `CountDownLatch` barrier over **one**
shared store, each session on its own executor running a pure chain
of `tool-count` `:fixture/echo` tool nodes into `:emit`. Every session
persists the same exact M = 5 + 6×tool-count events (root, started,
6 per tool node, 2 for the emit node, completed), with its own task
text. After all sessions finish it asserts, for every session:
verified hash chain with exactly M events, the exact expected
event-type sequence, per-session seq `1..M`, no cross-session prev
references, no foreign event rows, pinned session rows, outputs
containing only that session's own text, the provider execution count
(N × tool-count), and the global event total (N × M). A second test
runs the whole scenario twice and asserts identical structural
fingerprints (determinism).

## 6. Host guidance

- Run sessions in parallel **only** with per-session Phenotype
  instances (isolated SCI runtimes); never two live sessions on one
  SCI runtime.
- One store (sqlite + CAS) may serve any number of concurrent
  sessions; the store serializes writers at the transaction level.
- `run-session!` only starts sessions in `:created`; a session that
  is already running or terminal is rejected with
  `:scheduler/session-invalid`.
- Recovery of a session interrupted mid-run is the store recovery
  layer's job (component), not the scheduler's.
## 7. Async durable work — the `works` lifecycle (W1/W2)

**Components:** `evoclj.store.work` (schema, 7-state SM, CAS, deadline sweeper), `evoclj.store.recovery` (orphan classification and recovery), `evoclj.runtime.work` (vocabulary + SM verification), `evoclj.runtime.subagent` (executor poll loop + wait/wakeup join), `evoclj.environment.registry` (`refresh-async!`), `evoclj.mcp.adapter` (Tasks `continue`). Wolfram [W-20..W-27] (`docs/formal/async-model.md`).

> **Retired (ExtraModules repair):** the heritage `commands` six-state compat track (`store/command.clj`, `find-orphaned-commands`/`recover-commands!`) is removed — Work is the only lifecycle (INV-12). The `commands` table remains in old databases as inert history (migration `018-work.sql` backfilled it into `works`). The per-state command history lives in git, not here.

Async work eliminates bare `future`. Every piece of work that outlives its call site is reified as a row in `works`, tracked by the closed 7-state machine, and resumable after a crash:

```
queued | running | waiting | succeeded | failed | timed-out | cancelled
```

Stored as lowercase `TEXT` (`timed_out` in SQLite, mapped to `:timed-out` in code). Transitions are CAS-guarded (`WHERE state IN (...)` — concurrent drivers cannot both move the same row); terminals are sinks. Wolfram checks [W-20..W-24] cover the SM (`runtime/work.clj` verification + `work_property_test.clj` 100-round walks).

| From | To | Helper in `store/work.clj` |
| --- | --- | --- |
| `:queued` | `:running` | `dispatch-work!` |
| `:running` | `:waiting` | `wait-work!` (paused for input — subagent child, external signal) |
| `:running\|:waiting` | `:succeeded` | `succeed-work!` |
| `:queued\|:running\|:waiting` | `:failed` | `fail-work!` |
| `:queued\|:running\|:waiting` | `:cancelled` | `cancel-work!` (explicit intent, not a timeout) |
| `:running\|:waiting` | `:timed-out` | `timeout-work!` (after deadline; see the sweeper below) |

### 7.1 Recovery of orphans — `store/recovery.clj`

After a restart any row still in `queued`, `running`, or `waiting` has no in-process worker driving it. `store/recovery.clj` is the recovery scan:

* `find-orphaned-works` — read-only classification; terminals are never reported.
* `recover-works!` — **report, not fabricate**: `:queued` orphans stay `:queued` (no write) so redelivery is possible; `:running`/`:waiting` orphans move to `:failed` with `{:error/type :recovery/orphaned}` via CAS — NEVER synthesized `:succeeded`.

The report shape is `{:orphaned-works [...] :recovered-queued [...] :recovered-running [...]}`. Re-running on already-terminal rows is a no-op.

### 7.2 Deadline sweeper — `sweep-expired-deadlines!`

`store/work.clj` `sweep-expired-deadlines!` drives every non-terminal Work whose `:work/deadline` has passed to its timeout terminal via CAS: `:queued -> :failed`, `:running`/`:waiting -> :timed-out` (each carrying `{:error/type :work/deadline-exceeded}` in the report). Rows without a deadline and terminal rows are untouched; a CAS race on one row is skipped, never thrown; re-running is a no-op. Tests inject a fixed `now`; production passes none.

### 7.3 Executor poll loop — sweep, redeliver, wake up

`runtime/subagent.clj` owns the durable join. `poll-queued-children!` is the loop: first sweep expired deadlines (best-effort preamble, never stops the poll), then redeliver every remaining `:queued` `:subagent/run` Work by recovering its spawn-time task and running it to a terminal state. `await-child!` is the waiting half: a parent polls the child's Work row until it reaches a terminal state — the terminal row IS the wakeup, so a crashed runner still wakes the waiter via recovery or replay. Neither path blocks on a future: the DB row is the truth.

### 7.4 Host wiring — `refresh-async!` and MCP Tasks `continue`

```text
refresh-async! (environment/registry.clj)
  synthesizes an :environment/refresh Work (id, type, state :queued)
  -> when a durable :store is wired, create-work! + queued->running->succeeded/failed;
  -> retains the work map under :work-queue / :last-work for auditability;
  -> no future handle is leaked as the return value (W1: no :last-refresh-future)

MCP Tasks continuation (mcp/adapter.clj)
  2026 path (Adapter2026/continue): persists an :mcp/continue Work and
             returns {:status :continuing, :work/id id, :work work}
  2025 fallback (Adapter2025/continue): degrades to the same Work audit
             (does NOT throw :mcp/not-supported), returns {:status :queued, :work/id id}
```

Both paths persist `:mcp/continue` / `:environment/refresh` Works best-effort (a store failure leaves the audit in-band, never throws) so recovery and cancellation apply uniformly. Tests assert no leaked future handle remains after `refresh-async!` and that both 2026 and 2025 adapters produce a Work row when a store is present.

## 8. Subagents — Work-supervised child executions

**Components:** `evoclj.runtime.subagent` (spawn / run / cancel / result + tool surface), `evoclj.store.work` (7-state lifecycle + `parent_work_id` graph), `evoclj.store.session` (immutable identity rows + `subagent_links` session mirror), `evoclj.store.recovery` (Work-only orphan reporting), `evoclj.capability.mint` (`derive-lease!` attenuation), `evoclj.intent.schema` / `evoclj.intent.dispatch` (typed intents). Wolfram [W-16..W-19] (subagent SM), [W-08..W-11] (attenuation / downward-closed), [W-25..W-27] (event chain).

> **Lifecycle note (W1/W2):** `Work` (`works` table, 7-state SM
> `queued|running|waiting|succeeded|failed|cancelled|timed-out`) is the sole
> durable lifecycle for subagents (INV-12). A session row is immutable
> identity — it is inserted once as `:created` and never transitions for a
> subagent; completion, failure, and cancellation truth all live on the
> child Work row, driven by compare-and-set. The `transition-session!`
> hops in section 2 describe the scheduler's session mirror, not the
> subagent lifecycle.

A subagent is not a thread. It is an independent **session** — own `session/id`, own phenotype (SCI runtime), own single-session FIFO scheduler — that runs through the same broker and store as its parent and is supervised via the Work graph and the lease lattice. Parent and child share no mutable state except the `subagent_links` edge, the parent->child Work edge, and the derived lease chain.

* **Same genome/resolution, new session + new child Principal.** Spawn derives a child execution (Phenotype instance) from the parent's genome/resolution, so the Principal is `{:principal/type :session :session/id child-id}` (I2 single field) — siblings on the same genome are different Principals (exact tagged-value equality, [W-01]).
* **Derived leases via `capability/mint.clj` `derive-lease!`.** The child's capability set is an **attenuation** of the parent's: `actions child subset actions parent`, `maxCalls child <= maxCalls parent`, `issued child >= issued parent`, `expires child <= expires parent`, with `:cap/attenuated-from` chain retained for audit. An expanded action set or longer window is rejected (`[W-08..W-11]` narrow derivation + downward-closed: the parent's authority is a superset of every reachable child's). The mutation path never mints a fresh lease for a child — it always derives.
* **Independent scheduler lane.** The child's intents all pass the broker with the child's Principal and derived leases; provider execution is per-session. Each session's event chain is positionally `1..M` (`[W-25]`) with strict immediate-prev only (`[W-26]`) and sha256 hash chain verified per session (`[W-27]`).
* **One spawn mints exactly one child Work.** `spawn-subagent!` returns `{:child/session-id, :child/work-id, ...}`: the session id is the identity handle, the Work id (`:subagent/run`, starting `:queued`) is the durable execution handle the run, status, cancel, and replay paths resolve. There is no created-session lifecycle requirement — `run-subagent!` drives whatever live child Work the handle names.

### 8.2 Spawn graph — `parent_work_id` (durable) + `subagent_links` (session mirror)

Parent links are not on the `sessions` row — they live in a dedicated table so one parent can have many children and ancestry is queryable without parsing metadata:

```sql
CREATE TABLE IF NOT EXISTS subagent_links (
  child_session_id  TEXT PRIMARY KEY,                            -- one parent per child
  parent_session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
  created_at        TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS subagent_links_parent_idx ON subagent_links(parent_session_id);
```

`ensure-subagent-link-table!` is idempotent — ad-hoc in-memory DBs opened by tests get the table without running the full migration chain.

Queries (`runtime/subagent.clj` and `store/session.clj` share the shape):

| Helper | Meaning |
| --- | --- |
| `get-parent-session-id(child)` | single hop upward |
| `child-session-ids(parent)` | direct children ordered by `created_at` |
| `subagent-depth(session)` | walks `get-parent-session-id` upward to count depth (root = 0) |
| `list-descendants(root)` | **BFS closure** over `child-session-ids`, not including root; iteratively expands a queue; each newly discovered node's children are queued — returns full transitive descendant set in BFS order; cycle-free by `PRIMARY KEY` + parent-exists-before-child insertion order |

The durable spawn graph is `works.parent_work_id`: every child Work points at the exact parent Work the spawn was attributed to (`:parent/work-id` on the Intent path, the parent's latest Work on the `:agent/spawn` tool path). Navigation (`get-parent-work-id`, `child-work-ids`, `work-depth`, `work-fanout`, `work-descendants`) lives in `store/work.clj`. `subagent_links` is the session-level mirror kept for compat; cancel targets are the union of the link BFS and the Work BFS, so neither graph can strand a descendant.

`cancel-subagent-tree!` computes its revocation set over both graphs and `cancel-subagent!` on a mid-tree node revokes its whole subtree via the same closure.

### 8.3 Spawn — `:intent/subagent-spawn` via `spawn-subagent!`

```text
intent/subagent-spawn
  -> intent/schema.clj validates PayloadSubagentSpawnSchema (Malli)
  -> intent/dispatch.clj -> runtime/subagent.clj spawn-subagent! (db, parent-id, child-spec, parent-leases)
       * checks depth <= max-subagent-depth (5) and spawns-per-parent <= 10 before insertion
       * derives child execution (Phenotype instance) from parent genome/resolution; child Principal is {:principal/type :session :session/id child-id} (I2)
       * derives child leases as narrowings via capability/mint.clj derive-lease!
       * inserts child session row (immutable identity, :created) + subagent_links row + records leases in leases-by-session
       * appends :subagent/spawned event to the parent chain with prev -> parent's latest event id (GC-20 causal link)
       * creates exactly ONE child Work (:subagent/run, :queued) carrying the spawn-time task digest as :work/payload-ref and the spawn deadline as :work/deadline — the durable handle the run, status, cancel, and replay paths resolve
```

Malli payload (`intent/schema.clj` `PayloadSubagentSpawnSchema`): `{:session/id uuid parent, :parent/work-id uuid (required — a spawn is caused by exactly one parent Work), :child/spec map task + overrides, :child/capabilities [map] optional narrowed set}`. Absent capabilities, the child receives an attenuation of the parent's full lease set (perm-model section 2). The `:agent/spawn` model-tool path keeps the latest-Work fallback (model args carry no Work) and — as an `:effect :write` tool — demands `:metadata {:idempotency/key ...}` on the tool-call path.

### 8.4 Child runtime — `run-subagent!` drives the child Work

```text
run-subagent! (child-id, task [, work-id])
  -> resolve the child Work (explicit work-id, else the child's single Work; >1 is :store/work-invalid)
  -> enforce the child Work deadline (fail-closed :subagent/deadline-exceeded, terminal CAS first)
  -> build-child-executor (own SCI namespace, own compiler program, own CAS dir, own Phenotype instance;
     registers the :agent/spawn + :agent/status + :agent/cancel providers and resolves
     subagent-tool-catalog against the child registry, so nested spawn/status/cancel dispatch)
  -> run-session! (child-id, task, child-work-id) -- same scheduler path as any session;
     the child Work moves queued -> running -> succeeded|failed via CAS
  -> audit the executed task against the spawn-time digest bind (reports, never rejects)
  -> auto-deliver the settled child terminal to the parent chain (best-effort evidence under :delivery)
  -> child intents all pass broker with child leases + child Principal
```

* There is no created-session requirement: the session row stays `:created` (immutable identity) while the child Work carries the lifecycle. Awaiting uses the Work handle (`await-child!` polls the Work to a terminal; `run-subagent-async!` keeps the future as an await handle only — Work stays the truth).
* No shared SCI binding, no shared lease atom, no shared event cursor. Each session's `seq` is `1..M` locally; parent and child chains interleave only via the `:subagent/spawned` prev link and `:causal-links` edges.
* Progress events are fanned out through `mcp/manager` so the host can observe `waiting` vs `running` accurately.
* Hash chain: each session's `verify-event-chain` is independent; tampering changes the header digest and is detected per session.
* Queued-Work replay: `poll-queued-children!` picks up child Works still `:queued` (e.g. after a crash before the run) and drives them — the spawn-time digest makes the replay auditable.

### 8.5 Cancel and cascade — `cancel-subtree-tx!` (revoke + Work CAS + events, one tx)

Typed intent `intent/subagent-cancel` (`{:target/session-id uuid, :target/work-id uuid (cross-checked against the owning session), :reason #{:user-request :parent-cancel :timeout ...}}`) and the `:agent/cancel` model tool (descendant-scoped, session- or Work-addressable) both drive one path:

* **Lease revocation (fail-closed).** `cancel-subagent!` / `cancel-subagent-tree!` revoke every lease recorded for each target in the global `subagent-lease-registry` (in-memory, `capability/mint.clj` `revoke-lease!`, `create-lease-registry`) and in the persistent `capabilities` table (`store/capability-store` `revoke-capability!` when present), plus `leases-by-session` tombstones. The registry is partition-safe — revoked leases are tombstoned even if unseen. The next broker call on that child with the revoked lease yields `:capability/denied`.
* **Work CAS (the lifecycle truth).** `cancel-subtree-tx!` moves every target Work `queued|running|waiting -> cancelled` with `WHERE state IN (...)` compare-and-set in ONE `BEGIN IMMEDIATE` transaction together with the revoke and the cancel events — DB-first, memory tombstones after commit. No session row is transitioned (rows stay `:created`); the per-target `:session/cancelled` event and the `:subagent/cancelled` edge on each immediate parent chain are annotations sequenced inside the same tx. Cancel is idempotent — an already `:cancelled` Work is a no-op (returns `already-cancelled?`), and other terminal Work states (`succeeded`, `failed`, `timed-out`) are left as-is.
* **Transitive cascade.** `cancel-subagent-tree!` with root `R` computes the BFS closure over the link graph AND the Work graph and revokes + cancels every descendant in one call; `cancel-subagent!` on a mid-tree node likewise cancels its subtree, so a child cannot outlive its parent's revocation. The scheduler's terminal step also cancels non-terminal children (`cancel-non-terminal-children!`), joining structured concurrency with `await-child!`.

### 8.6 Result delivery — `:subagent/result` with CAS ref

```text
child Work terminal (succeeded|failed|cancelled|timed-out)
  -> runtime/subagent.clj deliver-result-for-works! / deliver-failure-for-works!
     (legacy deliver-result! / deliver-failure! resolve session handles and delegate)
       * re-checks the child Work terminal inside one BEGIN IMMEDIATE tx (provenance CAS)
       * the supplied CAS ref must equal the child Work's payload_ref (:store/cas-mismatch otherwise)
       * failure :error is derived from the canonical :session/failed terminal event (caller value is fallback only)
       * appends :subagent/result event to the PARENT chain with payload
         {:child/session-id uuid, :child/work-id uuid, :terminal/event-id id,
          :result/cas-ref sha256: or :error, :result/status kw}
         and prev -> child's terminal event, with a `:causal-links #{ {:from <child-terminal-id> :type :subagent/result} }` edge
       * replay of the same terminal is an idempotent no-op (returns the recorded event)
  -> run-subagent! auto-delivers the settled child terminal (evidence under :delivery, never fails the run)
  -> recovery: orphaned children (parent Work terminal while child Work still
     live :queued|:running|:waiting) are surfaced by
     store/recovery.clj find-orphaned-subagents — Work-only, via the
     parent_work_id graph — and recover-orphaned-subagents! cancels them
     DB-first (never replays an orphan whose parent is terminal),
     never fabricated as completed (S5)
```

The `:subagent/result` intent type is the parent-side handle for awaiting a child's artifact; raw store rows carry the artifact's `sha256:` CAS reference so the parent can fetch the bytes without duplicating them.

### 8.7 Limits — depth and budget

```clojure
(def ^:const max-subagent-depth 5)       ;; runtime/subagent.clj: ancestor chain length
(def ^:const max-spawns-per-parent 10)   ;; direct children per parent (maps to :tool/budget {:max-calls 10})
```

Checked in `check-depth-and-budget!` before insertion. Depth is per-chain (`depth child = depth parent + 1`, root depth 0). Budget is per-parent branching factor — a parent may spawn at most `max-spawns-per-parent` direct children. Violations throw typed errors (`:subagent/depth-exceeded` or `:subagent/budget-exceeded`) on the spawner; the child never starts. These caps are independent of the per-session `max-steps` / `max-tool-rounds` that drive the `budgetExhausted` terminal via `run-session!`.

### 8.8 Tool surface — `:agent/spawn`, `:agent/status`, `:agent/cancel` (single-sourced)

The model-facing facade is the canonical `tool.specs` triple (S6, `activate_skill`-style) — the Malli schemas, the C-Tool maps, and the wire entries are defined once in `tool.specs`; `runtime/subagent.clj` aliases them (no duplicate maps) and consumes `subagent-tool-catalog` in `build-child-executor` (registers the three providers, S14-resolves the catalog):

* `:agent/spawn` — `agent-spawn-provider` (broker-executable), args `{:task, :capabilities}` validated against the canonical input schema, output `{:child/session-id, :child/work-id, :child/capabilities}`; `:effect :write` (persists session row + event + link + child Work), so the tool-call path demands `:metadata {:idempotency/key ...}`.
* `:agent/status` — `agent-status-provider`, args `{:session-id}` or first-class `{:work-id}`, output `{:session/id, :work/id, :state, :children [...], :depth, ...}` where `:state` is the child Work state; descendant-scoped (`:capability/scope-denied` otherwise).
* `:agent/cancel` — `agent-cancel-provider`, args `{:session-id}` or `{:work-id}` plus `:reason`; cancels the subtree via `cancel-subagent!`; descendant-scoped like status.

All three descriptors declare `:tool/audience #{:model}`. The broker path is `tool -> :agent/spawn -> agent-spawn-provider -> spawn-subagent!` (same Grant-meet lease derivation, link insertion, and single-Work minting as the intent path).

