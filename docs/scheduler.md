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
