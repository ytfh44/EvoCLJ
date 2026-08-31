# Subagent Model — SubAgentSession State Machine (Wolfram-Verified)

> **Source:** `local://evoclj-implementation-dag.md` §1.4 (SubAgentSession SM, [W-16..W-19]), §2 rows S1–S6, §3–§4 (DAG topology + waves). Context: `local://wave1-context.md` (GC-20 causality, broker closed registry).
> **Status:** V1 solidification — documents the 8-state machine that S2–S6 realized. The machine lives in `store/session.clj` (state column + transitions) and is exercised through `runtime/subagent.clj` (spawn / cancel / result) and `intent/schema.clj` / `intent/dispatch.clj` (typed intents).
> **Scope:** Session lifecycle, spawn/cancel/result flows, the `subagent_links` graph, and cascade revocation via the global lease registry extension (P5/S4). Sibling under `docs/formal/perm-model.md` (permissions) and `docs/formal/async-model.md` (commands + event chain).

---

## 1. SubAgentSession — the supervised nested session

A subagent is not a thread. It is an independent **session** (own `session/id`, own SCI runtime, own single-session FIFO scheduler) that runs through the same broker and is supervised by its parent session. Parent and child share no mutable state except the parent→child link and the derived lease chain.

### 1.1 States (eight)

```
created | resolving | running | waiting | completed | failed | budgetExhausted | cancelled
```

Definitions (normative, matches `store/session.clj`):

* `created`       — row inserted, not yet resolved to a phenotype (has `session/id` and `task` spec).
* `resolving`     — provider/topology resolution in progress (compiler path).
* `running`       — SCI runtime is executing intents through the broker.
* `waiting`       — blocked on a tool/model subcall (progress events may arrive via the fan-out path, but the session itself is parked).
* `completed`     — terminal: task finished with a CAS artifact reference.
* `failed`        — terminal: unrecoverable error (effect lattice rejection, tool failure, etc.).
* `budgetExhausted` — terminal: budget (steps / tool rounds) exhausted.
* `cancelled`     — terminal: cancelled by parent or by the cancel intent (S4).

Four of the eight are terminal (`completed`, `failed`, `budgetExhausted`, `cancelled`) — they have no outgoing edges.

### 1.2 Transitions (directed edges)

```
created   → { resolving, failed, cancelled }
resolving → { running, failed, cancelled }
running   → { waiting, completed, failed, budgetExhausted, cancelled }
waiting   → { running, failed, cancelled }
```

No edges leave a terminal state. `waiting ↔ running` is the only loop (tool waits re-enter `running` when the result returns). Every other progress is forward.

### 1.3 Wolfram checks [W-16..W-19]

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-16] | `edgesLegalQ` | Every transition endpoint is inside the 8-state set | pass |
| [W-17] | `allReachableFromCreatedQ` | Every one of the 8 states is reachable from `created` (no dead state) | pass |
| [W-18] | `cancelledTerminalQ` | `cancelled` has no outgoing edges | pass |
| [W-19] | `completedTerminalQ` | `completed` has no outgoing edges | pass |

`budgetExhausted` and `failed` are likewise terminal (implied by the edge table, though the two named terminal checks name `completed` and `cancelled` explicitly in the DAG). The Wolfram graph for this SM is acyclic except for the waiting↔running loop, which was modeled as the two directed edges `running→waiting` and `waiting→running` — the loop is intentional and bounded (the budget caps its iterations).

### 1.4 Session table encoding

`store/session.clj` maps the SM onto the `sessions` table (`state` TEXT). The valid state set is enforced both in code (Malli enum) and in queries; illegal states are rejected on write. The `state` column is updated only via the transition helpers (`fail-session!`, `budget-exhaust!`, `run-session!`, etc.) — no ad-hoc `UPDATE` bypasses the edge table. Tests in `runtime/subagent_*_test` drive each edge and assert `state` values.

---

## 2. Spawn / Cancel / Result flows

### 2.1 Spawn (S2)

```
intent/subagent-spawn
  → dispatch.clj validates IntentSchema (PayloadSubagentSpawnSchema)
  → subagent/spawn-subagent! (db, parent-id, child-spec, parent-leases)
       * checks depth ≤ max-subagent-depth (5) and spawns-per-parent ≤ 10
       * derives a child phenotype from the parent (P3 dual-anchor)
       * derives child leases as narrowings of parent leases (perm-model §2)
       * inserts child session row (state created) + subagent_links row
       * appends :subagent/spawned event with cause → parent (GC-20 causal link)
       * records child leases in leases-by-session for later cascade
  → child is Runnable (created→resolving on first scheduler tick)
```

Malli payload (`intent/schema.clj`):

```clojure
(PayloadSubagentSpawnSchema
  [:map {:closed false}
   [:session/id   uuid?]          ; parent
   [:child/spec   :map]            ; task + optional overrides
   [:child/capabilities [:vector [:map {:closed false}]]]])
```

The parent may optionally supply narrowed capabilities; absent, the child receives an attenuation of the parent's full lease set (downward-closed, perm-model [W-11]).

### 2.2 Child runtime (S3)

```
run-subagent! (child-id, task)
  → build-child-executor (own SCI namespace, own compiler program, own CAS dir)
  → run-session! (child-id, task)   ; same scheduler path as any session
  → child intents all pass broker with child leases + child subject
```

* Isolation: no shared SCI binding, no shared lease atom, no shared event cursor. Each session's `seq` is `1..M` locally (async-model §1.6, [W-25] positional).
* Progress events from a child are fanned out through the same `mcp/manager` progress→event-store path as the parent (M17), so cancellation can observe `waiting` vs `running` accurately.
* Hash chain: each session's event chain is independent and verified by `store/event verify-event-chain` (sha256 header digest). Parent and child chains interleave only via the `:subagent/spawned` causal `cause` pointer.

### 2.3 Cancel and cascade (S4, depends on P5)

```
intent/subagent-cancel  { :session/id target, :reason ∈ {:user-request :parent-cancel :timeout} }
  → dispatch.clj → subagent/cancel-subagent! or cancel-subagent-tree!
       * resolves the target set: single child or whole BFS descendant tree
         (via subagent_links — §3)
       * for each target: non-capturable interrupt via sci/limits make-interrupt-fn
       * revoke-leases! for each target's recorded leases (perm-model §3)
         — the global lease registry is partition-safe: revoked leases are
         tombstoned even if unseen
       * CAS-recursive revoke: revoking a mid-tree node revokes its subtree too
       * appends :session/cancelled or :subagent/cancelled event (cause → parent)
       * state edge: running|waiting|resolving → cancelled (or created → cancelled)
       * next intent on that child → :capability/denied (broker sees revoked lease)
```

Key property: **cancellation is transitive**. `cancel-subagent-tree!` with root `R` computes `list-descendants(R)` as the BFS closure over `subagent_links` and revokes every descendant's leases in one call. A child cannot "outlive" its parent's revocation — its next broker call is denied.

The test `subagent_cancel_test.clj` proves: parent revoked ⇒ child and grandchild leases all revoked ⇒ their next intents are `:capability/denied`.

### 2.4 Result delivery (S5)

```
child terminal (completed|failed|budgetExhausted|cancelled)
  → deliver-result! or deliver-failure!
       * validates CAS reference shape (sha256:…)
       * checks child is in a terminal state (non-terminal delivery is rejected)
       * appends :subagent/result event to the PARENT chain with
         payload { :child/session-id, :child/state, :result/cas-ref or :error }
         and cause → child's terminal event
       * raw atomic via the same outbox discipline as command outbox (A2):
         event write and link update are transaction-local
  → recovery: orphaned children (parent completed while child still running)
    are surfaced by recovery/scan-recovery-state (S5) and reported, never
    fabricated as completed
```

The `:subagent/result` intent type is the parent-side handle for awaiting a child's artifact; `store/recovery` also exposes `find-orphaned-subagents` so the startup integrity scan can report stranded children.

The `tool.specs` canonical pair `:agent/spawn` / `:agent/status` (S6) is the model-facing façade over this: `activate_skill`-style tool names that the broker dispatches to `agent-spawn-provider` / `agent-status-provider` in `runtime/subagent.clj`.

---

## 3. The `subagent_links` graph

Parent links are not stored on the `sessions` row — they live in a dedicated helper table so one parent can have many children and ancestry is queryable without parsing metadata.

### 3.1 DDL (applied via `store/session.clj` and reinforced in `store/recovery.clj`)

```sql
CREATE TABLE IF NOT EXISTS subagent_links (
  child_session_id  TEXT PRIMARY KEY,
  parent_session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
  created_at        TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS subagent_links_parent_idx
  ON subagent_links(parent_session_id);
```

* `child_session_id` PRIMARY KEY — a child has exactly one parent.
* `ON DELETE CASCADE` — deleting a session cascades away its links (used by maintenance, never by normal flow).
* `parent_idx` — fast enumeration of direct children (`child-session-ids`).
* The `store/session.clj` helper `ensure-subagent-link-table!` is idempotent, so tests that open ad-hoc in-memory DBs get the table without running the full migration chain.

### 3.2 Queries over the graph

* `get-parent-session-id(child)` — single hop upward.
* `child-session-ids(parent)` — single level downward, ordered by `created_at`.
* `subagent-depth(session)` — walks upward via `get-parent-session-id` to count depth (0 for a root).
* `list-descendants(root)` — **BFS closure** over `child-session-ids`, not including `root` itself. Iteratively expands a queue; each newly discovered node's children are queued. Returns the full transitive descendant set in BFS order. This is the function that `cancel-subagent-tree!` uses to compute the revocation set. Cycle-free by construction (the sessions table inserts a child only after its parent exists, and `child_session_id` PK forbids a second parent).

---

## 4. Limits — depth and budget

```clojure
(def ^:const max-subagent-depth 5)        ; ancestor chain length
(def ^:const max-spawns-per-parent 10)    ; direct children per parent
```

Stored as provider-side checks in `check-depth-and-budget!` (called from `spawn-subagent!` before insertion). Depth is per-chain (grandchild depth = parent depth + 1, rooted at the outermost parent at depth 0). Budget is per-parent branching factor — a parent can spawn at most `max-spawns-per-parent` direct children. Violations throw a typed error (`:subagent/depth-exceeded` or `:subagent/budget-exceeded`) — the spawner, not the child, fails.

These caps are independent of the per-session `max-steps` / `max-tool-rounds` budgets that govern `run-session!` (exhaustion drives the `budgetExhausted` terminal). S6 also wires the broker tool catalog limits through `agent-spawn-tool` descriptors.

---

## 5. Causal chain and GC-20

Every subagent event carries `cause` (the parent's event that caused the spawn or the child's terminal event that the parent consumes). `append-event!` persists `cause` as a FK-like reference (must be an event of the declaring session or of the linked-counterpart session, verified at read time by `verify-event-chain`). This is the GC-20 invariant — "every effect is causally linked to the parent" — and it is also how `recovery` reconstructs orphan relationships without fabricating completion.

---

## 6. Wolfram verification summary (26 checks, all pass)

The subagent model contributes 4 of the 26 checks ([W-16..W-19]):

```
[W-16] edgesLegalQ                pass   (8 states, edges inside set)
[W-17] allReachableFromCreatedQ   pass   (no dead state)
[W-18] cancelledTerminalQ         pass   (no egress from cancelled)
[W-19] completedTerminalQ         pass   (no egress from completed)
```

Together with perm-model [W-01..W-15] (15), async-model [W-20..W-24] (5) and event chain [W-25..W-27] (3), the full Wolfram suite is **26/26 pass**. The subagent SM was true on the first pass; no rephrasing was needed.

---

## 7. DAG verification (for this model's position)

Condensed from `local://evoclj-implementation-dag.md` §3–§4 (same Wolfram graph that secures perm-model and async-model):

* 21 nodes, 25 edges; **acyclic = true**.
* Sources `{P1, A1, S1}` — S1 (intent type schema) is this chain's source and can start alongside P1 and A1.
* Sinks `{P7, V2}` — P7 is an optional storage leaf; the subagent chain sinks at S6 then V2.
* Edges touching this model: `S1→S2`, `P3→S2` (gate: dual-anchor must land before spawn), `S2→S3`, `S2→S4`, `S2→S5`, `S3→S6`, `S5→S6`, `P5→S4` (revoke generalization must land before cascade), `A5→V1`, `S4→V1`, `P5→V1`, `S6→V2`.
* 7 waves; this model spans W1 (S1), W4 (S2), W5 (S3+S4+S5), W6 (S6), W7 (V2 doc).

| Wave | Subagent work |
|------|---------------|
| W1 | **S1** — `:intent/subagent-spawn|:intent/subagent-result|:intent/subagent-cancel` type + payload schemas in `intent/schema.clj` |
| W4 | **S2** — `spawn-session!` (lease-derivation + `subagent_links` + `:subagent/spawned` causal event) |
| W5 | **S3** — child SCI runtime / `run-subagent!`; **S4** — `cancel-subagent!` + BFS cascade + lease revocation; **S5** — `deliver-result!` + orphan recovery |
| W6 | **S6** — `:agent/spawn` + `:agent/status` broker surface (`tool.specs` + `runtime/subagent.clj` providers) + depth/budget caps |
| W7 | V2 doc closure |

* **Critical path** is `P1 → P2 → P3 → S2 → S3 → S6 → V2` — S2 cannot start before P3 (subject aliasing must be settled), so the permission→subagent chain dominates wall-clock. The async chain (A-series) remains off the critical path.

---

## 8. References

* Design source: `local://evoclj-implementation-dag.md` §1.4, §2 rows S1–S6, §3 edges, §4 waves/critical path.
* Implementation: `src/evoclj/intent/schema.clj` (S1 intent types), `src/evoclj/runtime/subagent.clj` (spawn / run / cancel / result / tool surface), `src/evoclj/store/session.clj` (state + `subagent_links` DDL), `src/evoclj/store/recovery.clj` (orphan reporting), `src/evoclj/intent/dispatch.clj` (§subagent-spawn branch), `src/evoclj/tool/specs.clj` (canonical `:agent/spawn|:agent/status` specs).
* Tests: `test/evoclj/intent/subagent_intent_test.clj`, `test/evoclj/runtime/subagent_spawn_test.clj`, `test/evoclj/runtime/subagent_run_test.clj`, `test/evoclj/runtime/subagent_cancel_test.clj`, `test/evoclj/runtime/subagent_result_test.clj`, `test/evoclj/runtime/subagent_tool_test.clj`.
* Wolfram & hash discipline shared with `perm-model.md` and `async-model.md`; this file adds no hash-shaped bare tokens and passes `scripts/verify-doc-hashes.clj` exit 0 without exemptions.
