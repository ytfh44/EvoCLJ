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
  `:cause/event-id` is the previous event's `:event/id`, anchored on a
  `:session/created` root the host appended at creation time;
- the topology's `:limits {:max-steps N}` bounds the walk;
  `:loop` iteration counters travel in the scheduler's per-session
  `:loop-state` (session-local data, never a SCI global var).

The scheduler spawns **no threads**. It is a pure per-session driver;
concurrency (or its absence) is decided by the host that calls it.

## 2. What is serialized (within a session)

| Concern | Mechanism |
| --- | --- |
| Node execution | FIFO; one node completes before the next is stepped |
| Event persistence | `append-event!` runs each append in one `BEGIN IMMEDIATE` transaction: seq allocation (`max(seq)+1`), cause validation, `prev-hash` linkage, hash computation, row insert — atomic, never interleaved |
| Session state | compare-and-set `transition-session!` hops: `:created → :resolving → :running → :waiting → :completed` (or `:failed` / `:budget-exhausted`) |
| SCI runtime use | one session at a time — `evoclj.sci.execute` documents a SCI runtime as **not thread-safe** ("it belongs to one Phenotype/session") |
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
session must not share a SCI runtime with another running session**
(`evoclj.sci.execute`: "A runtime is not thread-safe"). Each parallel
session gets its own Phenotype instance (its own isolated SCI
runtime); the store is shared. The stress test builds exactly this
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
   `:cause/event-id` references an **earlier event in the same
   session** (`:store/cause-session-mismatch` otherwise); per-session
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
event-type sequence, per-session seq `1..M`, no cross-session cause
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
## 7. Async commands — durable outbox and recoverable execution

**Components:** `evoclj.store.command` (schema, SM, outbox), `evoclj.store.recovery` (orphan classification and recovery), `evoclj.environment.registry` (`refresh-async!`), `evoclj.mcp.adapter` (Tasks `continue`). Wolfram [W-20..W-27] (`docs/formal/async-model.md`).

Async commands eliminate bare `future`. Every piece of work that outlives its call site is reified as a row in `commands`, tracked by a six-state machine, and resumable after a crash. The write path is an **outbox**: the command row and its `:command/submitted` announcement are committed in one `BEGIN IMMEDIATE` transaction, so they co-live or co-die.

### 7.1 `commands` table

DDL lives in `resources/migrations/012-commands.sql` and is enforced by `store/command.clj` Malli `CommandSchema`:

```sql
CREATE TABLE IF NOT EXISTS commands (
  id                TEXT PRIMARY KEY,
  type              TEXT NOT NULL,
  state             TEXT NOT NULL CHECK (state IN ('queued','running','succeeded','failed','timed_out','cancelled')),
  idempotency_key   TEXT NOT NULL UNIQUE,
  payload_ref       TEXT NOT NULL,  -- sha256: reference to CAS bytes
  owner_session_id  TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT,
  parent_cmd_id     TEXT REFERENCES commands(id) ON DELETE SET NULL,
  continuation_edn  TEXT,
  deadline          TEXT,
  created_at        TEXT NOT NULL
);
```

* `state` CHECK is the DB mirror of `CommandState` (`[:enum :queued :running :succeeded :failed :timed-out :cancelled]`) — illegal states are rejected even if Malli is bypassed.
* `idempotency_key UNIQUE` is the at-most-once fence at the storage layer.
* `payload_ref` is `sha256:` content-addressed (same discipline as `store/cas` and genome artifacts); bytes live in CAS, the row stores the reference.
* `owner_session_id` FK pins the command to the session that submitted it.
* `parent_cmd_id` self-FK chains continuations (nullable, `ON DELETE SET NULL`).

### 7.2 State machine

```
queued | running | succeeded | failed | timedOut | cancelled
```

Stored as lowercase `TEXT` (`timed_out` in SQLite, mapped to `:timed-out` in code via `state->db` / `db->state`). Six is the complete set — no other string passes the `CHECK`.

| From | To | Helper in `store/command.clj` | CAS guard |
| --- | --- | --- | --- |
| `:queued` | `:running` | `dispatch-command!` | `WHERE state = 'queued'` — concurrent dispatchers cannot both move the same row |
| `:queued` | `:failed` | `fail-command!` | `WHERE state IN ('queued','running')` |
| `:queued` | `:cancelled` | `cancel-command!` | `WHERE state IN ('queued','running')` |
| `:running` | `:succeeded` | `succeed-command!` | `WHERE state = 'running'`; writes `:command/completed` + CAS result pattern mirrors `store/promotion_outbox` |
| `:running` | `:failed` | `fail-command!` | `WHERE state = 'running'` (typed error) |
| `:running` | `:timedOut` | `timeout-command!` | `WHERE state = 'running'`; gated by `deadline-passed?` on `:cmd/deadline` vs now; `make-interrupt-fn` is non-capturable |
| `:running` | `:cancelled` | `cancel-command!` | `WHERE state IN ('queued','running')` (explicit cancel, not a timeout) |

No other edges exist; in particular there is no `running -> queued` (re-queue requires a fresh row with a new key). Wolfram checks [W-20] `edgesLegalQ`, [W-21] `acyclicQ`, [W-22] `fourTerminalsSinkQ` (`succeeded, failed, timedOut, cancelled` are sinks), [W-23] `queuedToSucceededPathQ`, [W-24] `queuedToTimedOutPathQ` — all pass (`async-model.md` section 1.3).

### 7.3 Same-transaction outbox — `create-command-with-event!`

`store/command.clj` `create-command-with-event!` is the single outbox writer:

```text
BEGIN IMMEDIATE                           -- with-command-tx
  1. INSERT INTO commands (...)           -- command row
  2. INSERT event :command/submitted      -- same connection, same TX
     -- seq = max(seq)+1 for owner session, cause validated, prev-hash linked,
     -- hash = sha256(canonical-header) where canonical-header is
     --        "id|session|type|cause|seq|created-at" (store/event canonical-header)
COMMIT  -- or ROLLBACK on any failure
```

Helpers on the same connection: `with-command-tx` (TX macro, rollback on throw), `insert-event-in-tx!` (raw JDBC event insert including `cause` session check and `edn-safe-metadata?` guard), `canonical-header` / `event-hash` (deterministic header serialization shared with `store/event`). The outbox copies the `store/promotion_outbox` single-DB atomic pattern and the FK-owner discipline.

Failpoint contract (A2): inject a failure after step 1 before step 2 — on retry neither row nor event exists; after a clean commit both exist and `cause` points at the parent. `create-command!` (simple non-outbox path) and `duplicate-key?` detection still run on the same table so bare and outbox inserts share the `UNIQUE` fence.

### 7.4 Idempotency

`idempotency_key` is the GC deduplication key. DB `UNIQUE` enforces at-most-once even if code forgets. Re-submitting the same logical command returns the existing row without executing a second time. The creation helpers `create-command!` and `create-command-with-event!` both surface `duplicate-key?` so callers can distinguish "inserted" from "already existed".

### 7.5 Recovery of orphans — `store/recovery.clj`

After a restart any row still in `queued` or `running` has no in-process worker driving it. `store/recovery.clj` is the recovery scan:

* `find-orphaned-commands` — read-only classification: `SELECT ... WHERE state IN ('queued','running')` ordered by `created_at`; terminals are never reported.
* `recover-commands!` — **report, not fabricate** (preserves the "never fabricate completion" invariant from `store/recovery` for sessions):

```clojure
;; queued orphan  -> left :queued (no write) so redelivery is possible;
;;                  resubmit of the same idempotency_key hits UNIQUE and re-queues exactly once
;; running orphan -> fail-command! with {:error/type :recovery/orphaned}
;;                  row moves :running -> :failed; NEVER synthesizes :succeeded
```

The report shape is `{:orphaned-commands [...] :recovered-queued [...] :recovered-running [...]}`. The host decides whether to re-submit a queued key; the running case is crash residue, not a real execution failure, and is surfaced via `:recovery/orphaned`.

### 7.6 Dispatch, timeout, and cancel

* **Dispatch** — `dispatch-command!` compare-and-sets `queued -> running` and then drives the work through the existing broker/provider path (same execution path the scheduler uses for intents). Completion writes a CAS artifact reference plus a `:command/completed` event atomically with the state change (A3).
* **Timeout** — `:cmd/deadline` powers `timedOut`. `store/command.clj` `deadline-passed?` compares deadline vs now; `timeout-command!` moves `running -> timedOut` and appends `:command/timed-out` on the owner's chain. The SCI interrupt it cooperates with is `sci/limits` `make-interrupt-fn` (non-capturable).
* **Cancel** — `cancel-command!` moves `queued|running -> cancelled` (explicit intent, not a timeout) and appends `:command/cancelled`. All three helpers are CAS-guarded — a mismatched `state` throws with `{:expected ... :state actual}`.

### 7.7 Host wiring — `refresh-async!` and MCP Tasks `continue`

```text
refresh-async! (environment/registry.clj)
  before A6: (future (refresh! ...))           -- leaked future, no audit trail
  now:        synthesizes an :environment/refresh command (id, type, idempotency-key,
              sha256 payload-ref, owner via resolve-refresh-owner, created-at)
              -> when a durable :store is wired, create-command! + dispatch-command!
                 queued->running->succeeded/failed inside the future;
              -> retains the command map under :command-queue / :last-command so
                 no-DB tests can assert auditability without a DB;
              -> stores the raw future under :last-refresh-future but returns the
                 command map (not the future) — the command is the observable result
              -> no future handle is leaked as the return value

MCP Tasks continuation (mcp/adapter.clj)
  2026 path (Adapter2026/continue): persists a command with parent_cmd_id +
             continuation_edn and returns {:status :continuing, :command-id id, :command cmd}
  2025 fallback (Adapter2025/continue): degrades to the same command queue
             (does NOT throw :mcp/not-supported), returns {:status :queued, :command-id id}
```

Both paths go through `store/command.clj` (`make-mcp-continue-cmd` + `create-command!`) so recovery and cancellation apply uniformly. Tests assert no leaked future handle remains after `refresh-async!` and that both 2026 and 2025 adapters produce a command row when a store is present.

## 8. Subagents — nested supervised sessions

**Components:** `evoclj.runtime.subagent` (spawn / run / cancel / result + tool surface), `evoclj.store.session` (state + `try-cancel-session!` + `subagent_links` graph), `evoclj.store.recovery` (orphan reporting), `evoclj.capability.mint` (`derive-lease!` attenuation), `evoclj.intent.schema` / `evoclj.intent.dispatch` (typed intents). Wolfram [W-16..W-19] (subagent SM), [W-08..W-11] (attenuation / downward-closed), [W-25..W-27] (event chain).

A subagent is not a thread. It is an independent **session** — own `session/id`, own phenotype (SCI runtime), own single-session FIFO scheduler — that runs through the same broker and store as its parent and is supervised via the link graph and the lease lattice. Parent and child share no mutable state except the `subagent_links` edge and the derived lease chain.

### 8.1 What a subagent session is

* **Same genome/resolution, new session + new phenotype subject.** Spawn derives a child phenotype from the parent's genome/resolution (P3 dual-anchor `{:session/id :phenotype/id}`), so the subject is `{:session/id child-id :phenotype/id child-phenotype}` — siblings on the same genome are different subjects (`subject-matches?` isolation, [W-01]).
* **Derived leases via `capability/mint.clj` `derive-lease!`.** The child's capability set is an **attenuation** of the parent's: `actions child subset actions parent`, `maxCalls child <= maxCalls parent`, `issued child >= issued parent`, `expires child <= expires parent`, with `:cap/attenuated-from` chain retained for audit. An expanded action set or longer window is rejected (`[W-08..W-11]` narrow derivation + downward-closed: the parent's authority is a superset of every reachable child's). The mutation path never mints a fresh lease for a child — it always derives.
* **Independent scheduler lane.** The child's intents all pass the broker with the child's subject and derived leases; provider execution is per-session. Each session's event chain is positionally `1..M` (`[W-25]`) with earlier-cause only (`[W-26]`) and sha256 hash chain verified per session (`[W-27]`).

### 8.2 `subagent_links` graph — `store/session.clj` + `runtime/subagent.clj`

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

`cancel-subagent-tree!` computes its revocation set as `list-descendants(root)` and `cancel-subagent!` on a mid-tree node revokes its whole subtree via the same BFS.

### 8.3 Spawn — `:intent/subagent-spawn` via `spawn-subagent!`

```text
intent/subagent-spawn
  -> intent/schema.clj validates PayloadSubagentSpawnSchema (Malli)
  -> intent/dispatch.clj -> runtime/subagent.clj spawn-subagent! (db, parent-id, child-spec, parent-leases)
       * checks depth <= max-subagent-depth (5) and spawns-per-parent <= 10 before insertion
       * derives child phenotype from parent (P3 dual-anchor)
       * derives child leases as narrowings via capability/mint.clj derive-lease!
       * inserts child session row (state :created) + subagent_links row + records leases in leases-by-session
       * appends :subagent/spawned event to the parent chain with cause -> parent's last event (GC-20 causal link)
       * child is Runnable: created -> resolving on first scheduler tick
```

Malli payload (`intent/schema.clj` `PayloadSubagentSpawnSchema`): `{:session/id uuid parent, :child/spec map task + overrides, :child/capabilities [map] optional narrowed set}`. Absent capabilities, the child receives an attenuation of the parent's full lease set (perm-model section 2).

### 8.4 Child runtime — `run-subagent!`

```text
run-subagent! (child-id, task)
  -> build-child-executor (own SCI namespace, own compiler program, own CAS dir, own Phenotype instance)
  -> run-session! (child-id, task)   -- same scheduler path as any session
  -> child intents all pass broker with child leases + child subject
```

* No shared SCI binding, no shared lease atom, no shared event cursor. Each session's `seq` is `1..M` locally; parent and child chains interleave only via the `:subagent/spawned` causal `cause`.
* Progress events are fanned out through `mcp/manager` so the host can observe `waiting` vs `running` accurately.
* Hash chain: each session's `verify-event-chain` is independent; tampering changes the header digest and is detected per session.

### 8.5 Cancel and cascade — `revoke-leases-for-session!` + `try-cancel-session!`

Typed intent `intent/subagent-cancel` (`{:session/id target, :reason #{:user-request :parent-cancel :timeout ...}}`) drives two layers:

* **Lease revocation (fail-closed).** `runtime/subagent.clj` `revoke-leases-for-session!` (and bulk `cancel-subagent!` / `cancel-subagent-tree!`) revokes every lease recorded for each target in the global `subagent-lease-registry` (in-memory, `capability/mint.clj` `revoke-lease!`, `create-lease-registry`) and in the persistent `capabilities` table (`store/capability-store` `revoke-capability!` when present), plus `leases-by-session` tombstones. The registry is partition-safe — revoked leases are tombstoned even if unseen. The next broker call on that child with the revoked lease yields `:capability/denied`.
* **Session state via `store/session.clj` `try-cancel-session!`.** Cancel uses **direct SQL** (`UPDATE sessions SET state = 'cancelled' WHERE state IN (...)`) rather than `transition-session!`, so it can move **` :created -> :cancelled`** directly (the normal `transition-session!` edge set does not allow `:created -> :cancelled` for sessions, but a subagent that was spawned and then cancelled before ever resolving must still land in a terminal). `try-cancel-session!` is idempotent — an already `:cancelled` row is a no-op (returns `already-cancelled?`), and other terminal rows (`completed`, `failed`, `budget-exhausted`) are left as-is.
* **Transitive cascade.** `cancel-subagent-tree!` with root `R` computes `list-descendants(R)` as the BFS closure and revokes + cancels every descendant in one call; `cancel-subagent!` on a mid-tree node likewise cancels its subtree, so a child cannot outlive its parent's revocation. Events ` :session/cancelled` (on each child) and `:subagent/cancelled` (on each child's immediate parent) are appended best-effort with `cause` pointing at the latest event of the respective chain.

### 8.6 Result delivery — `:subagent/result` with CAS ref

```text
child terminal (completed|failed|budget-exhausted|cancelled)
  -> runtime/subagent.clj deliver-result! or deliver-failure!
       * validates sha256: CAS ref shape
       * checks child is in a terminal state (non-terminal delivery is rejected)
       * appends :subagent/result event to the PARENT chain with payload
         {:child/session-id uuid, :child/state kw, :result/cas-ref sha256: or :error}
         and cause -> child's terminal event
       * atomic with the link update (same outbox discipline as A2)
  -> recovery: orphaned children (parent completed while child still
     non-terminal :created|:resolving|:running|:waiting) are surfaced by
     store/recovery.clj find-orphaned-subagents via subagent_links join
     where parent state IN (completed,failed,cancelled,budget-exhausted)
     and child state IN (created,resolving,running,waiting) — reported,
     never fabricated as completed (S5)
```

The `:subagent/result` intent type is the parent-side handle for awaiting a child's artifact; raw store rows carry the artifact's `sha256:` CAS reference so the parent can fetch the bytes without duplicating them.

### 8.7 Limits — depth and budget

```clojure
(def ^:const max-subagent-depth 5)       ;; runtime/subagent.clj: ancestor chain length
(def ^:const max-spawns-per-parent 10)   ;; direct children per parent (maps to :tool/budget {:max-calls 10})
```

Checked in `check-depth-and-budget!` before insertion. Depth is per-chain (`depth child = depth parent + 1`, root depth 0). Budget is per-parent branching factor — a parent may spawn at most `max-spawns-per-parent` direct children. Violations throw typed errors (`:subagent/depth-exceeded` or `:subagent/budget-exceeded`) on the spawner; the child never starts. These caps are independent of the per-session `max-steps` / `max-tool-rounds` that drive the `budgetExhausted` terminal via `run-session!`.

### 8.8 Tool surface — `:agent/spawn` and `:agent/status`

The model-facing facade over spawn/status is the canonical `tool.specs` pair (S6, `activate_skill`-style):

* `:agent/spawn` — `runtime/subagent.clj` `agent-spawn-provider` (broker-executable), args `{:task, :capabilities}` validated against `AgentSpawnArgsSchema`, output `{:child/session-id, :child/capabilities}`; catalog entry `agent-spawn-tool-descriptor` -> `tool.specs/agent-spawn-tool`.
* `:agent/status` — `agent-status-provider`, args `{:session-id}`, output `{:state, :children [...], :depth, ...}`; descriptor `agent-status-tool-descriptor` -> `tool.specs/agent-status-tool`.

Both descriptors declare `:tool/audience #{:model}` and are wired as `subagent-tool-catalog` for provider registry registration. The broker path is `tool -> :agent/spawn -> agent-spawn-provider -> spawn-subagent!` (same lease derivation and link insertion as the intent path).

