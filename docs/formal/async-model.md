# Async Command Model — AsyncCommand State Machine + Event Chain (Wolfram-Verified)

> **Source:** `local://evoclj-implementation-dag.md` §1.5–§1.6 (AsyncCommand 6-state SM and append-only EventChain, [W-20..W-27]), §2 rows A1–A6, §3–§4 (DAG topology + waves). Precedent: `store/promotion_outbox` (same-DB atomic outbox) and `store/event` hash chain.
> **Status:** V1 solidification — documents the outbox + idempotency + recovery discipline that A1–A6 realized. No new semantics after S6.
> **Scope:** `commands` table, the six-state SM, the `create-command! ↔ :command/submitted` same-transaction outbox, dispatch through the provider/broker, deadline & cancellation, recovery of orphaned `queued`/`running` commands, and the append-only event chain with positional `seq` and sha256 hash chain. Realized in `store/command.clj`, `store/recovery.clj`, `store/event.clj`, `store/migrate.clj` (migrations 012/013), `environment/registry.clj` and `mcp/adapter.clj` (A6 futures elimination).
> **Siblings:** `perm-model.md` (leases), `subagent-model.md` (nested sessions).

---

## 1. AsyncCommand — a durable, re-dispatchable piece of work

Async commands exist to eliminate bare `future`. Every piece of work that outlives the call site (refresh, MCP `continue`, custom commands) is reified as a row in `commands`, tracked by a six-state machine, and resumable after a crash via the recovery scan. An `idempotency_key` makes re-submission safe — the same logical command submitted twice executes once.

### 1.1 Malli schema (normative — `store/command.clj`)

```clojure
(def CommandState
  [:enum :queued :running :succeeded :failed :timed-out :cancelled])

(def CommandSchema
  [:map {:closed true}
   [:cmd/id                uuid?]
   [:cmd/type              keyword?]                  ; e.g. :tool/invoke
   [:cmd/state             CommandState]
   [:cmd/idempotency-key   [:and string? [:fn seq]]]  ; non-empty, DB UNIQUE
   [:cmd/payload-ref       [:and string? [:re #"^sha256:[0-9a-f]{64}$"]]]
   [:cmd/owner-session-id  uuid?]                      ; FK → sessions
   [:cmd/created-at        [:fn inst?]]
   [:cmd/parent-cmd-id     {:optional true} [:maybe uuid?]]
   [:cmd/continuation-edn  {:optional true} :any]
   [:cmd/deadline          {:optional true} [:maybe [:fn inst?]]]])
```

* `:cmd/payload-ref` is `sha256:` content-addressed (GC-21) — the bytes live in the CAS directory, the row stores the reference. Same discipline as genome artifacts.
* `:cmd/idempotency-key` is the GC deduplication key. DB `UNIQUE` enforces at-most-once at the storage layer even if the code forgets.
* `:cmd/deadline` powers the `timed-out` terminal (A4).
* `:cmd/parent-cmd-id` self-FK for causal chaining (nullable, `ON DELETE SET NULL`).

### 1.2 DDL — `resources/migrations/012-commands.sql` (A1)

```sql
CREATE TABLE IF NOT EXISTS commands (
  id                TEXT PRIMARY KEY,                    -- uuid string
  type              TEXT NOT NULL,
  state             TEXT NOT NULL
                    CHECK (state IN ('queued','running',
                                     'succeeded','failed','timed_out','cancelled')),
  idempotency_key   TEXT NOT NULL UNIQUE,
  payload_ref       TEXT NOT NULL,                       -- sha256: reference
  owner_session_id  TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT,
  parent_cmd_id     TEXT REFERENCES commands(id) ON DELETE SET NULL,
  continuation_edn  TEXT,                                -- optional EDN
  deadline          TEXT,                                -- optional ISO-8601 UTC
  created_at        TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS commands_owner_session_idx ON commands(owner_session_id);
CREATE INDEX IF NOT EXISTS commands_idempotency_key_idx ON commands(idempotency_key);
CREATE INDEX IF NOT EXISTS commands_state_idx ON commands(state);
CREATE INDEX IF NOT EXISTS commands_parent_cmd_idx ON commands(parent_cmd_id)
  WHERE parent_cmd_id IS NOT NULL;
```

The `state` CHECK is the DB mirror of `CommandState` — illegal states are rejected even if the code bypasses Malli. SQLite foreign keys are per-connection (`PRAGMA foreign_keys = ON`) and callers route through `store/sqlite` helpers (same discipline as `promotion_outbox`).

### 1.3 State machine (six states)

```
queued | running | succeeded | failed | timedOut | cancelled
```

The machine is stored as lowercase `TEXT` (`timed_out` in DB, mapped to `:timed-out` in code via `state->db` / `db->state`). Six is the complete set — no other string passes the `CHECK`.

#### Transitions (directed edges)

```
queued  → { running, failed, cancelled }
running → { succeeded, failed, timedOut, cancelled }
```

* `queued → running` is dispatched via `dispatch-command!` (compare-and-set on `state = queued`).
* `running → succeeded` via `succeed-command!` writes `:command/completed` event atomically with the CAS result (A3 — mirrors `promotion_outbox` discipline).
* `queued|running → failed` via `fail-command!` (typed error).
* `running → timedOut` via `timeout-command!` (A4 — after `deadline-passed?` on the deadline vs now).
* `queued|running → cancelled` via `cancel-command!` (A4 — explicit cancel intent, not a timeout).

No other edges exist. In particular there is no `running → queued` (no re-queue by mutation — a fresh row with a new key is required).

#### Wolfram checks [W-20..W-24]

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-20] | `edgesLegalQ` | Every transition endpoint is inside the six-state set | pass |
| [W-21] | `acyclicQ` | The directed graph is acyclic (finite progress — no loop of states) | pass |
| [W-22] | `fourTerminalsSinkQ` | The four terminals `succeeded, failed, timedOut, cancelled` have no outgoing edges | pass |
| [W-23] | `queuedToSucceededPathQ` | A path `queued → … → succeeded` exists (the system can complete) | pass |
| [W-24] | `queuedToTimedOutPathQ` | A path `queued → running → timedOut` exists (timeout is reachable) | pass |

`acyclic` is strict despite the two-step `queued → running → timedOut` path — no state can be revisited, so every command makes finite progress toward a terminal.

The helpers `dispatch-command!`, `succeed-command!`, `fail-command!`, `timeout-command!`, `cancel-command!` are allCAS (compare state, throw on mismatch) so concurrent dispatchers cannot both move the same queued row to running.

---

## 2. Outbox pattern — command + `:command/submitted` atomically (A2)

### 2.1 Why an outbox

A command that is inserted but whose announcement event is lost is invisible — it will never be dispatched. Conversely announcing an event for a command that rolled back is a ghost. The `store/promotion_outbox` precedent solved the same problem for promotion proposals by inserting the row and its event in a single `BEGIN IMMEDIATE` transaction.

### 2.2 How `create-command-with-event!` works

```text
BEGIN IMMEDIATE
  1. INSERT INTO commands (…)  -- the command row
  2. INSERT event (:command/submitted) with :cause → parent event
     -- same connection, same transaction; event seq is max(seq)+1
     -- event hash = sha256(canonical-header) where canonical-header is
     --              "id|session|type|cause|seq|created-at"
  3. COMMIT  (or ROLLBACK on any failure — command and event co-live or co-die)
```

Helpers inside `store/command.clj`:

* `with-command-tx` — the transaction macro (rolls back on throw).
* `insert-event-in-tx!` — raw JDBC event insert on the same connection, including `cause` validation (`root-event?` check) and `edn-safe-metadata?` guard.
* `canonical-header` / `event-hash` — deterministic header serialization used by both command-outbox events and `store/event`.

The normative test is the failpoint crash test (A2): inject a failure after step 1 but before step 2 — on retry, neither row nor event exists; after a clean commit, both exist and `cause` points at the parent.

### 2.3 Single writer and idempotency

The durable outbox has a single writer (`create-command-with-event!`) and a single deduplication key (`idempotency_key UNIQUE`). Re-submitting the same logical command returns the existing row without executing a second time. The creation helpers `create-command!` (simple non-outbox path) and `duplicate-key?` detection still run on the same table so bare `INSERT` and outbox `INSERT` share the `UNIQUE` fence.

### 2.4 A6 — eliminating bare `future` and the MCP dual rail

```text
refresh-async! (environment/registry.clj) was: (future (refresh! …))  // leaked
           now: submits a command via submit-command! into the outbox
               // pollable via store/command, recoverable after crash

MCP Tasks 'continue' (mcp/adapter.clj)
  2026 path:落地 into a command continuation (parent_cmd_id + continuation_edn)
  2025 fallback:降级 to the command queue (does NOT throw :mcp/not-supported)
```

Both paths go through the command table, so recovery and cancellation apply uniformly. The `registry.clj` test asserts no leaked `future` handle remains after `refresh-async!`; `mcp/adapter` has contract tests for both 2026 and 2025 behavior.

---

## 3. Recovery — orphaned `queued`/`running` after a crash (A5)

### 3.1 What is an orphan

After a restart, any row still in `queued` or `running` has no in-process worker driving it. `running` rows are particularly dangerous — they claim work is in progress that may have completed after the crash but never recorded.

### 3.2 `store/recovery` discipline (DAG A5)

```clojure
(find-orphaned-commands store)  ; queries commands WHERE state IN ('queued','running')
(recover-commands! store)        ; reports orphans, does NOT fabricate completion
```

* `find-orphaned-commands` returns the set of `queued`/`running` rows (via `sqlite/query`).
* `recover-commands!` **reports** orphans (returns them with `:recovery/orphaned` typed error) — it does **not** synthesize a transition to `succeeded` or `failed`. This preserves the "never fabricate completion" invariant established by `store/recovery` for `running` sessions.
* Re-dispatch is the caller's choice: the recovery scan tells the host which keys are orphaned, and the host re-submits the idempotency key if desired — re-submission hits the `UNIQUE` path and re-queues exactly once.

The same discipline applies to subagent orphans (subagent-model §2.4) — `find-orphaned-subagents` surfaces children whose parent completed while they still ran.

### 3.3 Two-test contract (A5)

A5 ships with two tests on the recovery row:

* Orphan `queued` is reported, re-submission with the same `idempotency_key` is deduplicated.
* Orphan `running` is reported, **not** auto-completed; a subsequent `fail-command!` / `timeout-command!` from the recovering host moves it to a terminal normally.

---

## 4. Event chain — append-only `seq` + sha256 hash chain [W-25..W-27]

The same chain that the permission and subagent models ride. `store/event` owns append, sequence allocation, and verification; `store/command` reuses its header canonicalization.

### 4.1 `seq` continuity [W-25] — the model discovery

```text
Invariant: events_for_session ordered by seq satisfy  events[i].seq = i+1
           (positionally continuous 1..M, not merely "multiset is {1..M}")
```

`append-event!` allocates `max(seq)+1` per session inside `with-append-tx` (a `BEGIN IMMEDIATE` sibling of the command outbox). The earlier phrasing "values are `{1..M}` as a set" was shown by Wolfram to accept `{1,3,2}` — three events whose set equals `{1,2,3}` yet the third event claims seq 2 out of order. The positional form rejects that. The current `store/event` test locks the invariant as `events[i].seq = i+1`.

### 4.2 Causal `cause` [W-26]

```
cause ∈ { nil (only for root :session/created), otherwise an event id
          that is strictly earlier in the same session }
```

`cause` may only reference an earlier event of the same session; forward references and cross-session jumps are rejected at append time. The only root event type is `:session/created` (cause is absent).

### 4.3 sha256 hash chain [W-27]

```text
header_i = "id | session_id | type | cause | seq | created_at"   ; canonical-header
hash_i   = sha256( header_i )
chain_i  = sha256( header_{i-1} || hash_i )  (conceptually — stored per-event as :header/hash)
verify-event-chain(session_id)  →  checks header hash per event and that seq
                                   chaining has no gap; tampering changes the
                                   header digest and is detected
```

`store/event canonical-header` is the exact string used by both the event table and the command outbox's `insert-event-in-tx!`. The same `hash/text-digest` function is shared, so a mismatch would break verification. `tamper detection` is proved by a test that mutates a stored header and asserts verification fails.

### 4.4 Wolfram checks [W-25..W-27]

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-25] | `seqPositionalContinuousQ` | `seq` is positionally `1..M` (the fix — not set equality) | pass (after rephrase) |
| [W-26] | `causeStrictlyEarlierQ` | `cause` must reference an earlier event of the same session | pass |
| [W-27] | `sha256HashChainQ` | Full chain verifies; tampering is detected via digest mismatch | pass |

[W-25] is the only one of the 26 whose first phrasing failed — after rephrasing to the positional form it passed, and an assertion test now guards the fix. The other two passed on the first pass.

---

## 5. Wolfram verification summary (26 checks, all pass)

The async model contributes 8 of the 26 checks (5 SM + 3 chain). Full suite:

```
[W-01..W-15]  CapabilityLease       (perm-model.md)     15 checks
[W-16..W-19]  SubAgentSession       (subagent-model.md)  4 checks
[W-20] edgesLegalQ                   pass
[W-21] acyclicQ                      pass
[W-22] fourTerminalsSinkQ            pass
[W-23] queuedToSucceededPathQ        pass
[W-24] queuedToTimedOutPathQ         pass
[W-25] seqPositionalContinuousQ      pass (after positional rephrase — the discovery)
[W-26] causeStrictlyEarlierQ         pass
[W-27] sha256HashChainQ             pass
Total: 26/26 pass
```

The five SM checks and two of the chain checks passed first-pass; [W-25] passed after the positional correction documented in `perm-model.md` §4.

---

## 6. DAG verification (for this model's position)

Condensed from `local://evoclj-implementation-dag.md` §3–§4 — same graph that secures the sibling models:

* 21 nodes, 25 edges; **acyclic = true**.
* Sources `{P1, A1, S1}` — **A1** is this chain's source (`CommandSchema` + `commands` DDL) and can start alongside P1 and S1 (disjoint files: `store/command.clj` vs `capability/schema.clj` vs `intent/schema.clj`).
* Edges touching this model: `A1→A2`, `A2→A3`, `A2→A5`, `A2→A6`, `A3→A4`, `A5→A6`, plus the V1 convergence `A5→V1` and the final tail `A6→V2`.
* 7 waves (Kahn, Wolfram). Async participation:

| Wave | Async work |
|------|------------|
| W1 | **A1** — `CommandSchema` + `commands` table (migration 012) with six-state CHECK |
| W2 | **A2** — `create-command-with-event!` same-transaction outbox (mirrors `promotion_outbox`) |
| W3 | **A3** — `dispatch-command!` / `succeed-command!` / `fail-command!`; **A5** — `find-orphaned-commands` + `recover-commands!` (no fabrication) |
| W4 | **A4** — `timeout-command!` / `cancel-command!` + deadline helpers; **A6** — `refresh-async!` outbox + MCP Tasks dual rail |
| W6 | V1 — this solidification (async half) |
| W7 | V2 — doc closure (scheduler.md follow-up) |

* **Critical path** is `P1 → P2 → P3 → S2 → S3 → S6 → V2` — the async chain (A-series) is **off** the critical path. Its widest wave is W3 with three async commits (A3+A5 alongside P3+P5+P7), so it consumes no extra wall-clock.

---

## 7. References

* Design source: `local://evoclj-implementation-dag.md` §1.5–§1.6, §2 rows A1–A6, §3 edges, §4 waves/critical path, §1.7 (seq discovery).
* Implementation: `src/evoclj/store/command.clj` (schema + SM transitions + outbox `WITH` transaction), `resources/migrations/012-commands.sql` (six-state CHECK + UNIQUE fence + FKs), `src/evoclj/store/recovery.clj` (`find-orphaned-commands` / `recover-commands!` — A5), `src/evoclj/store/event.clj` (append, seq, hash, verification — [W-25..W-27]), `src/evoclj/environment/registry.clj` + `src/evoclj/mcp/adapter.clj` (A6 futures elimination).
* Tests: `test/evoclj/store/command_test.clj`, `test/evoclj/store/recovery_test.clj`, `test/evoclj/store/event_test.clj`, plus environment/MCP adapter tests that assert no leaked `future`.
* Hash discipline shared with sibling files; this file adds no hash-shaped bare tokens and passes `scripts/verify-doc-hashes.clj` exit 0 without exemptions. The `sha256:` strings shown in the DDL/schema snippets are matched by the script's `sha256:` stripping rule (E2) and are therefore not scanned as refs.
