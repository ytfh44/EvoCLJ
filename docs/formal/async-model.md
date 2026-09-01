# Async Model — Work Unified Lifecycle + H1 Hydration + Event Chain (Wolfram-Verified)

> **Source:** `local://evoclj-reconstruction-dag-2.md` V1 §W1/W2/H1/E1 (Work 7-state SM, Hydration pin, Event E1) and `local://evoclj-implementation-dag.md` §1.5–§1.6 (heritage AsyncCommand 6-state SM retired, [W-20..W-27] refined), §2 rows A1–A6→W1/W2, §3–§4 (DAG topology + waves). Precedent: `store/work` replaces `store/command` + `subagent_sessions`.
> **Status:** V1 refinement — **Work is the sole durable lifecycle** (W1/W2). `AsyncCommand` 6-state SM is retired; the **Work 7-state SM** `queued|running|waiting|succeeded|failed|timed-out|cancelled` is normative. Session is immutable context pin; Work carries lifecycle. **H1 hydration** `hydrate(pin)→ExecutionHandle` is single construction path. Documents the durable Work + H1 that W1/W2/H1/E1 realized. No new semantics after S6+W2.
> **Scope:** `works` table, the seven-state SM, the `create-work!` durable pin, deadline & cancellation via CAS, orphan recovery via Work (no fabrication), append-only event chain with positional `seq` + `prev` linear + `causal-links` graph + sha256 hash chain. Realized in `store/work.clj`, `runtime/work.clj`, `runtime/hydrate.clj` (H1), `store/recovery.clj`, `store/event.clj`, `store/migrate.clj` (migrations 014/I1, 018/W1, 017/E1, 019/P1), `store/event_schema.clj` (E1).
> **Siblings:** `perm-model.md` (Principal/Grant/Lease), `subagent-model.md` (child Work + causal-links).

---

## 1. Work — the sole durable lifecycle (W1/W2, replaces AsyncCommand)

Work exists to eliminate the Session×Command product and bare `future` dual track. Every piece of work that outlives the call site (refresh, MCP `continue`, custom commands, subagent children) is reified as a row in `works`, tracked by a seven-state machine, and resumable after a crash via the recovery scan. An `idempotency_key` is optional; when present it is `UNIQUE` per session (same discipline as command outbox). `parent_work_id` links child Works (subagent). Work's `:running` IS execution; a future is only an internal await handle, never an observable lifecycle.

### 1.1 Vocabulary, Malli schema, DDL

```clojure
(def work-states #{:queued :running :waiting :succeeded :failed :cancelled :timed-out})

(def WorkSchema
  [:map {:closed true}
   [:work/id uuid?]
   [:work/type keyword?]
   [:work/state work-states]
   [:work/session-id uuid?]                      ; FK → sessions (immutable pin)
   [:work/parent-work-id {:optional true} [:maybe uuid?]]
   [:work/payload-ref [:maybe [:re #"^sha256:[0-9a-f]{64}$"]]]
   [:work/created-at inst?]
   [:work/deadline {:optional true} [:maybe inst?]]
   [:work/continuation-edn {:optional true} :any]])
```

DDL — `resources/migrations/018-work.sql` (W1):

```sql
CREATE TABLE IF NOT EXISTS works (
  id                TEXT PRIMARY KEY,
  type              TEXT NOT NULL,
  state             TEXT NOT NULL
                    CHECK (state IN ('queued','running','waiting',
                                     'succeeded','failed','timed_out','cancelled')),
  session_id        TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT,
  parent_work_id    TEXT REFERENCES works(id) ON DELETE SET NULL,
  payload_ref       TEXT,                          -- sha256: reference (GC-21)
  deadline          TEXT,                          -- optional ISO-8601 UTC
  continuation_edn  TEXT,                          -- optional EDN
  created_at        TEXT NOT NULL,
  updated_at        TEXT
);
CREATE INDEX IF NOT EXISTS works_session_idx ON works(session_id);
CREATE INDEX IF NOT EXISTS works_state_idx ON works(state);
CREATE INDEX IF NOT EXISTS works_parent_idx ON works(parent_work_id) WHERE parent_work_id IS NOT NULL;
-- Backfill from legacy commands when commands exists:
INSERT OR IGNORE INTO works (id, type, state, session_id, parent_work_id, payload_ref, deadline, continuation_edn, created_at, updated_at)
SELECT id, type, state, owner_session_id, parent_cmd_id, payload_ref, deadline, continuation_edn, created_at, created_at FROM commands;
```

* `:work/payload-ref` is `sha256:` content-addressed (GC-21) — bytes live in CAS; row stores reference.
* `:work/parent-work-id` self-FK for causal chaining (nullable, `ON DELETE SET NULL`). Subagent = child Work + Principal + causal-links.
* `:work/deadline` powers `timed-out` terminal via W2.
* The `state` CHECK is DB mirror of `work-states` — illegal states rejected even if code bypasses Malli. SQLite FKs are per-connection (`PRAGMA foreign_keys = ON`) via `store/sqlite` helpers.

The heritage `commands` table (`012-commands.sql`, 6 states) is **retained for migration read-only**; new writes must use `works`. The command helpers (`create-command!`, `dispatch-command!`, etc.) are deprecated aliases that delegate to `store/work` for compat (break compat loudly if removed).

### 1.2 State machine (seven states — W1 refinement)

```
queued | running | waiting | succeeded | failed | timed-out | cancelled
```

The machine is stored as `TEXT` (`timed_out` in DB, mapped to `:timed-out` in code via `kw->db` / `db->kw`). Seven is the complete closed set — no other string passes the `CHECK`.

#### Transitions (directed edges — matches `runtime/work.clj`)

```
queued  → { running, failed, cancelled }
running → { waiting, succeeded, failed, cancelled, timed-out }
waiting → { succeeded, failed, cancelled, timed-out }
```

* `queued → running` via `dispatch-work!` (CAS `WHERE state='queued'`).
* `running → waiting` via `wait-work!` (tool wait park).
* `running|waiting → succeeded` via `succeed-work!` (CAS `WHERE state IN ('running','waiting')`, writes payload_ref atomically).
* `queued|running|waiting → failed` via `fail-work!` (CAS, typed error; budgetExhausted maps to failed with `:budget/exhausted`).
* `running|waiting → timed-out` via `timeout-work!` (W2, after `deadline-passed?`).
* `queued|running|waiting → cancelled` via `cancel-work!` (W2, explicit cancel intent, idempotent on already-cancelled).

No other edges exist. In particular there is no `running → queued` (no re-queue by mutation — a fresh Work with new id is required) and no `waiting → running` (waiting Work succeeds/fails/cancels/times-out; re-dispatch is a new Work). The graph is **acyclic** (topological order `queued < running < waiting < terminals`), every edge moves forward — see [W-21].

#### Wolfram checks [W-19..W-24] — Work SM (W1/W2, 6 checks)

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-19] | `workEdgesLegalQ` | Every transition endpoint inside 7-state vocabulary | pass |
| [W-20] | `workAcyclicQ` | Directed graph is acyclic (finite progress) | pass |
| [W-21] | `workFourTerminalsSinkQ` | Four terminals `succeeded, failed, timed-out, cancelled` have no outgoing edges | pass |
| [W-22] | `workQueuedToSucceededPathQ` | Path `queued → … → succeeded` exists (can complete) | pass |
| [W-23] | `workQueuedToTimedOutPathQ` | Path `queued → running → timed-out` exists (timeout reachable) | pass |
| [W-24] | `workWaitingSucceedsQ` | `waiting → succeeded` exists (tool wait can resolve) | pass |

The former AsyncCommand [W-20..W-24] heritage (6 states) is refined to these six on 7 states; `acyclic` is strict despite `running→waiting` — no state can be revisited, every Work makes finite progress to a terminal. All helpers are CAS (compare state, throw on mismatch) so concurrent dispatchers cannot both move the same queued row to running. `runtime/work verify-work-sm` is the Wolfram predicate mirror in code; `work_property_test.clj` samples 100 random walks.

---

## 2. Session × Command product collapse — 48 → 7 (composition invariant)

Heritage model had `Session (8 states) × Command (6 states) = 48-state product` where a subagent's durable state was the product of an `AsyncCommand` row and a `SubAgentSession` row. W1 collapses this to a single **Work** table with 7 states where Session is **immutable context** (pin: `code_image_id`, `deployment_id`, `execution_id`, `generation_id` from I1) and Work carries the lifecycle.

```
Work×Session product invariant:
  heritage_product = 8 (session) × 6 (command) = 48 states
  refined          = 1 (session pin, immutable) × 7 (Work) = 7 states
  collapse_ratio   = "Session×Command 48 states collapses to Work 7"  (runtime/work.clj)
  session pin never transitions; only Work transitions (checked by store/session immutable + work transitions)
```

* **Why collapse:** the product had redundant waiting states (session `waiting` and command `running` overlapped), dual `budgetExhausted`/`failed` terminals, and causal ambiguity (which table drives hash chain?). Refinement eliminates `future` shadowing — `Work.running` IS execution; a future is only an internal await handle (W2).
* **Composition invariant:** `Work` is **definition > validation** (`runtime/work` defines vocabulary; `store/work` validates against it). Hydration H1 covers the former `resolving` session state — compiling the genome is `hydrate(pin)` before `dispatch-work!`, not a Work state.

Wolfram check [W-25] pins this:

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-25] | `workProductCollapseQ` | `Session×Command 48 collapses to Work 7; session pin is immutable, only Work has transitions` | pass |

Code witness: `runtime/work work-states-count =7`, `session-x-command-product =48`, `collapse-ratio` string; `store/session` tests assert session rows never mutate state after insert (immutable pin), while `store/work` drives all transitions. The old `commands` table is retained only for backfill.

---

## 3. H1 Hydration — `hydrate(pin) → ExecutionHandle` (composition invariant)

```clojure
(hydrate {:code_image/id sha256:… :deployment/id sha256:… :generation/id … :session/id uuid})
  → {:execution/id uuid :code-image {...} :deployment {...} :execution/id uuid :compiled {...} :leases [...]}
```

* **Single construction path:** `runtime/hydrate.clj` is the only factory that builds an execution context (fresh SCI namespace, fresh usage atom, fresh CAS temp dir, fresh broker context + leases Durably loaded via P1 DB truth). Direct constructor calls are banned (grep shows no second path).
* **Pin stability:** `hydrate` with the same pin yields handles with equal `code-image`/`deployment` but distinct `execution/id` (per-activation UUID). The pin's `code_image_id` is the content hash `H(kernel ABI, Genome, Resolution)` (I1).
* **Fail-closed:** unknown pin throws `:hydrate/pin-not-found`; hydration never synthesizes leases — DB miss hydrates to `[]` and broker denies (P1).

### Wolfram checks [W-26..W-27] — Hydration (H1, 2 checks)

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-26] | `hydratePinImmutabilityQ` | a pin's code_image/deployment do not change across hydrations; execution/id is distinct per call | pass |
| [W-27] | `hydrateNoSyntheticLeaseQ` | hydrate on DB miss yields no leases (empty), never a synthetic grant; authorize denies | pass |

Tests in `runtime/hydrate_test.clj` lock these; `store/work` recovery proves Work orphans stay failed rather than fabricated as succeeded via hydration.

---

## 4. Recovery — orphaned Works after a crash (W2, idempotent)

### 4.1 What is an orphan

After a restart, any `works` row still in `queued`|`running`|`waiting` has no in-process worker driving it. `running`/`waiting` rows claim execution is in progress that may have completed after the crash but never recorded as `succeeded`.

### 4.2 `store/work` discipline (W2)

```clojure
(find-orphaned-works db)  ; queries works WHERE state IN ('queued','running','waiting')
(recover-works! db)        ; reports orphans, drives running/waiting → failed (CAS), queued stays queued
```

* `find-orphaned-works` returns the set of `queued`/`running`/`waiting` rows.
* `recover-works!` **reports** orphans — it does **not** synthesize `succeeded`. Queued orphans stay `queued` (redelivery possible); `running`/`waiting` orphans are moved to `failed` via `fail-work!` with `{:error/type :recovery/orphaned}` via CAS (`UPDATE WHERE state IN (…)`). Already terminal rows are ignored, so re-running recovery is a no-op. This preserves "never fabricate completion" (W2).

### 4.3 Two-test contract (W2)

W2 ships with two tests:

* Orphan `queued` is reported, stays `queued`, re-dispatch with same payload succeeds exactly once via `dispatch-work!` CAS.
* Orphan `running`/`waiting` is reported, moved to `failed` with `:recovery/orphaned`, not auto-completed; a subsequent `succeed-work!` on that id throws `:work/invalid-transition`.

This mirrors the subagent orphan discipline (subagent-model §2.4) — `find-orphaned-works` surfaces child Works whose parent Work already terminal.

---

## 5. Event chain — append-only `seq` + `prev` + `causal-links` + sha256 hash chain [W-28..W-32]

The same chain that the permission and subagent models ride. `store/event` owns append, sequence allocation, and verification; H1 and Work reuse its pin. E1 refines `cause` into `prev` (linear) + `causal-links` (graph).

### 5.1 `seq` continuity [heritage W-25 refined, now W-28]

```text
Invariant: events_for_session ordered by seq satisfy  events[i].seq = i+1
           (positionally continuous 1..M, not merely "multiset is {1..M}")
```

`append-event!` allocates `max(seq)+1` per session inside `with-append-tx` (`BEGIN IMMEDIATE`). The earlier phrasing `"values are {1..M} as set"` was shown by Wolfram to accept `{1,3,2}` — three events whose set equals `{1,2,3}` yet the third claims seq 2 out of order. The positional form rejects that. `store/event` test locks the invariant.

### 5.2 `prev/event-id` linear predecessor [W-29] (E1)

```
prev ∈ { nil (only for root :session/created), otherwise an event id
         that is strictly earlier in the same session and immediately preceding (seq = new-seq -1) }
```

`prev/event-id` may only reference an earlier event of the **same session** and specifically the immediate predecessor (`event_seq = new-seq -1`). Forward references and cross-session jumps are rejected at append time with `:store/cause-session-mismatch` / `:store/cause-not-earlier`. Only root `:session/created` may carry `nil` prev.

### 5.3 `causal-links` graph [W-30] (E1)

```
causal-links ⊆ { {:from <event-id> :type <keyword>} }  // may cross sessions
each :from must reference an existing event (any session); cross-session allowed
root events carry empty causal-links
```

Stored in `causal_links(from_event_id, to_event_id, link_type)` (`017-event-prev-causal-links.sql`). Subagent result delivery appends parent event with `prev = parent's predecessor` and `causal-links = #{ {:from <child-terminal-id> :type :subagent/result} }`. Legacy `:cause/event-id` is accepted as deprecated alias for `prev` when `causal-links` absent (same-session only).

### 5.4 sha256 hash chain [W-32]

```text
header_i = "id | session_id | type | prev | causal-links-digest | payload-ref | prev-hash | created_at" ; canonical-header
hash_i   = sha256( header_i )
prev-hash_i = hash_{i-1} in same session (nil for first)
verify-event-chain(session_id) → checks header hash per event and that seq chaining has no gap and prev links are linear; tampering changes digest and is detected
```

`store/event canonical-header` is the exact string used by both event table and Work `prev` handling. `hash/text-digest` is shared, so mismatch breaks verification. Tamper detection proved by test that mutates stored header and asserts verification fails.

### 5.5 Wolfram checks [W-28..W-32] — Event chain (E1 refinement, 5 checks)

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-28] | `seqPositionalContinuousQ` | `seq` is positionally `1..M` (the fix — not set equality) | pass (after positional rephrase, heritage) |
| [W-29] | `prevStrictlyEarlierQ` | `prev/event-id` is strictly earlier and immediately preceding in same session | pass |
| [W-30] | `causalLinksFromExistsQ` | every `causal-links` `:from` references an existing event (any session, cross-session allowed) | pass |
| [W-31] | `rootNoPrevOrLinksQ` | root `:session/created` carries nil prev and empty causal-links | pass |
| [W-32] | `sha256HashChainQ` | Full chain verifies; tampering detected via digest mismatch | pass |

[W-28] is the only one of the original 26 whose first phrasing failed — after rephrase to positional form it passed, and assertion test now guards fix. The old [W-26] `causeStrictlyEarlierQ` is superseded by [W-29]/[W-30] E1 split.

---

## 6. Wolfram verification summary (32 checks, all pass — this file contributes 8 + 5)

The async model contributes **13** of the 32 checks (8 Work/Hydration + 5 Event chain). Full suite:

```
[W-01..W-18]  CapabilityLease+Grant            (perm-model.md)     18 checks
[W-19] workEdgesLegalQ                         pass
[W-20] workAcyclicQ                            pass
[W-21] workFourTerminalsSinkQ                  pass
[W-22] workQueuedToSucceededPathQ              pass
[W-23] workQueuedToTimedOutPathQ               pass
[W-24] workWaitingSucceedsQ                    pass
[W-25] workProductCollapseQ                    pass  ← Work×Session 48→7 (composition, new)
[W-26] hydratePinImmutabilityQ                 pass  ← H1 (new)
[W-27] hydrateNoSyntheticLeaseQ               pass  ← H1+P1 (new)
[W-28] seqPositionalContinuousQ               pass (heritage, positional fix)
[W-29] prevStrictlyEarlierQ                   pass  ← E1 prev linear (refined)
[W-30] causalLinksFromExistsQ                 pass  ← E1 graph (new)
[W-31] rootNoPrevOrLinksQ                     pass  ← E1 (new)
[W-32] sha256HashChainQ                       pass
Total: 32/32 pass
```

The five SM checks and two of the hydration checks passed first-pass; [W-28] passed after positional correction (heritage story in perm-model §4). The new composition checks [W-25..W-27] (Work×Session product, Hydration) and E1 checks [W-29..W-31] passed on first refined pass.

---

## 7. DAG verification (for this model's position — V1)

Condensed from `local://evoclj-reconstruction-dag-2.md` V1 §3–§4 — same graph that secures sibling models:

* **23 nodes, 29 edges; acyclic = true**. V1 adds I1/I2/C1/C2/C3/H1/W1/W2/P1.
* Sources `{P1, A1, S1}` — **A1 lineage + W1 Work table** are this chain's sources alongside P1, S1. The `store/work` migration is disjoint from `capability/schema.clj` and `intent/schema.clj`, so W1 can start alongside P1 and S1.
* Edges touching this model: `W1→W2`, `W1→S2` (Work table must land before spawn), `H1→W2` (hydrate before Work recovery), `E1→W1` (Event prev split before Work backfill), `W2→S4`, `W2→S5`, plus V1 convergence `W2→V1` and final tail `W1→V2` / `W2→V2`.
* 7 waves (Kahn, Wolfram). Work/Hydration participation:

| Wave | Async/Work work |
|------|-----------------|
| W1 | **I1** CodeImage/Deployment/Execution tables (014) |
| W2 | **E1** prev/causal-links split (017); **W1** `works` table 7-state (018) |
| W3 | **W2** `dispatch/await/complete/fail/cancel/timeout` via Work CAS + orphan recovery |
| W4 | **H1** `hydrate(pin)` factory + `CodeImage` pin stability |
| W5 | **W2** orphan recovery for subagent children via Work |
| W6 | V1 — this solidification (Work half) |
| W7 | V2 — doc closure (scheduler.md follow-up) |

* **Critical path** is `I1 → I2 → P1 → C2 → P5 → S2 → S3 → S6 → V2` — W1/W2 are now on the critical path via `W1→S2`. The old async chain (A-series) being off the critical path is heritage; refined Work chain is on path.

---

## 8. References

* Design source: `local://evoclj-reconstruction-dag-2.md` V1, `local://evoclj-implementation-dag.md` §1.5–§1.6 (heritage), §2 rows W1/W2/H1/E1, §3 edges, §4 waves/critical path, §1.7 (seq discovery).
* Implementation: `src/evoclj/runtime/work.clj` (vocabulary + SM + product collapse), `src/evoclj/store/work.clj` (schema + SM transitions + CAS + orphan recovery W2), `src/evoclj/runtime/hydrate.clj` (H1 pin → ExecutionHandle), `resources/migrations/014-code-image-deployment-execution.sql` (I1), `018-work.sql` (W1), `017-event-prev-causal-links.sql` (E1), `src/evoclj/store/event.clj` (append, seq, prev, causal-links, hash), `src/evoclj/store/event_schema.clj` (E1), `src/evoclj/store/recovery.clj` (orphans via Work).
* Tests: `test/evoclj/store/work_test.clj`, `test/evoclj/runtime/work_test.clj`, `test/evoclj/store/event_test.clj`, `test/evoclj/store/work_property_test.clj` (Work 100 rounds), `test/evoclj/store/recovery_test.clj` (W2), plus `test/evoclj/store/event_property_test.clj` (E1 100 rounds).
* Hash discipline shared with sibling files; this file adds no hash-shaped bare tokens and passes `scripts/verify-doc-hashes.clj` exit 0 without exemptions. The `sha256:` strings shown are matched by the script's `sha256:` stripping rule (E2) and are therefore not scanned as refs.
