# Subagent Model — SubAgent as Child Work with Causal-Links (Wolfram-Verified)

> **Source:** `local://evoclj-reconstruction-dag-2.md` V1 §subagent refinement (Work replaces Session×Command, E1 causal-links, H1 hydration) and `local://evoclj-implementation-dag.md` §1.4 (SubAgentSession SM, [W-16..W-19] retired), §2 rows S1–S6, §3–§4 (DAG topology + waves). Context: `local://wave1-context.md` (GC-20 causality, broker closed registry).
> **Status:** V1 refinement — **subagent is a child Work** (not a child Session row alone). Events use **E1 causal refinement**: `prev/event-id` linear same-session predecessor + `causal-links #{ {:from :type} }` cross-session graph. Hydration (H1) guarantees `hydrate(pin) → ExecutionHandle`. Documents the Work-based subagent that W1/W2 + E1 + H1 realized.
> **Scope:** Work child lifecycle, spawn/cancel/result flows via `store/work` + `runtime/subagent`, the `subagent_links` helper and new `works.parent_work_id` link, cascade revocation via DB-truth lease registry (P1), and H1 hydration pin.
> **Sibling:** `perm-model.md` (Principal/Grant/Lease), `async-model.md` (Work 7-state SM + H1).

---

## 1. SubAgentSession is retired — Work child model (W1/W2 refinement)

A subagent is not a thread and not a separate `sessions` row lifecycle alone. It is a **child Work** (`works` table, 7-state SM) pinned to its parent's session context and carrying its own **Principal** and **Work id**. Parent and child share no mutable state except the `works.parent_work_id` link and the causal-links edge. The old 8-state `SubAgentSession` SM (`created|resolving|running|waiting|completed|failed|budgetExhausted|cancelled`) is **retained only as heritage**; the durable lifecycle is now `Work` (§1.1). Session rows are **immutable context pins** (I1+I2): `sessions.code_image_id/deployment_id/execution_id` pin the code; Work carries the mutable lifecycle.

### 1.1 Work states for subagent children (seven, replaces eight)

```
queued | running | waiting | succeeded | failed | cancelled | timed-out
```

For subagents, `queued` is the inserted child Work, `running` is the SCI runtime executing, `waiting` is blocked on a tool/model subcall, and `succeeded`/`failed`/`cancelled`/`timed-out` are terminal (same as any Work — async-model §1.3). The heritage mapping is `created→queued`, `resolving→queued`, `running→running`, `waiting→waiting`, `completed→succeeded`, `failed→failed`, `budgetExhausted→failed` (via `fail-work!` with budget error), `cancelled→cancelled`. Hydration H1 covers `resolving` → the compiled `CodeImage` is loaded at `hydrate` time, not as a Work state.

Four of the seven are terminal (`succeeded`, `failed`, `cancelled`, `timed-out`) — no outgoing edges (same as async-model [W-22]).

### 1.2 Transitions for child Work (directed edges — matches `runtime/work.clj`)

```
queued  → { running, failed, cancelled }
running → { waiting, succeeded, failed, cancelled, timed-out }
waiting → { succeeded, failed, cancelled, timed-out }
```

No edges leave a terminal state. `waiting ↔ running` is the only loop (tool waits re-enter `running` when result returns, modeled as `running→waiting` and `waiting→running` via `waiting`→`succeeded` + new Work? In unified Work, `waiting→running` is `running→waiting` then `succeed` or `waiting` again — the SM itself is acyclic per [W-21]; tool polling is via new Works, not SM cycles). Every other progress is forward.

### 1.3 Wolfram checks heritage vs refinement

The old [W-16..W-19] session SM is **retired** and replaced by the Work SM checks [W-21..W-24] (see async-model). The subagent-specific new checks are E1 causal refinement [W-27..W-32] (§5).

### 1.4 Work table encoding for subagents

`store/work.clj` maps Work onto the `works` table (`state TEXT` with CHECK covering 7 states). `works.parent_work_id TEXT REFERENCES works(id) ON DELETE SET NULL` links a child Work to its parent Work (the subagent relation). The legacy `subagent_links(child_session_id, parent_session_id)` table is **retained as helper** for session-graph queries but new subagents use `parent_work_id`; both are kept in sync by `runtime/subagent.clj` for migration. Valid state set enforced in code (Malli enum) and DB CHECK; illegal states rejected on write. State column updated only via CAS helpers (`dispatch-work!`, `wait-work!`, `succeed-work!`, `fail-work!`, `cancel-work!`, `timeout-work!`) — no ad-hoc UPDATE bypasses the edge table. Tests in `runtime/subagent_*_test` and `store/work` drive each edge via Work.

---

## 2. Spawn / Cancel / Result flows (Work-based, P1 DB truth)

### 2.1 Spawn (S2 refined via W1 + P1 + H1)

```
intent/subagent-spawn
  → dispatch.clj validates IntentSchema (PayloadSubagentSpawnSchema)
  → subagent/spawn-subagent! (db, parent-work-id, child-spec, parent-principal)
       * checks depth ≤ max-subagent-depth (5) and spawns-per-parent ≤ 10 via parent_work_id count
       * derives child Principal from parent Principal (I2 single-field, not dual-anchor)
       * derives child leases as narrowings of parent leases (perm-model §2, Grant attenuates)
       * inserts child Work row (state queued) + subagent_links row (compat) + parent_work_id link
       * appends :subagent/spawned event with prev → parent's latest event and causal-links #{} (GC-20 linear)
       * records child leases durably via capability_store (P1 DB truth) + in-mem cache
  → child Work is dispatchable (queued → running on scheduler)
  → hydrate(pin) loads CodeImage/Deployment for child ExecutionHandle (H1)
```

Malli payload (`intent/schema.clj`):

```clojure
(PayloadSubagentSpawnSchema
  [:map {:closed false}
   [:session/id   uuid?]          ; parent session (pin)
   [:parent/work-id uuid?]        ; parent Work
   [:child/spec   :map]
   [:child/capabilities [:vector [:map {:closed false}]]]])
```

The parent may supply narrowed capabilities; absent, child receives attenuation of parent's full lease set (downward-closed, perm-model [W-13]/[W-14] Grant meet).

### 2.2 Child runtime (S3, W1 + H1)

```
run-subagent! (child-work-id, task)
  → hydrate(pin) → ExecutionHandle { code-image, deployment, execution-id, leases, compiled }
  → dispatch-work! (child-work-id)  ; queued → running (CAS)
  → run Work payload via SCI (own namespace, own CAS dir)
  → child intents all pass broker with child Principal + derived leases
```

* Isolation: no shared SCI binding, no shared lease atom. Each Work's execution is isolated via H1 `ExecutionHandle`. Each event's `seq` is `1..M` per session locally (async-model §1.6, [W-25] positional) and `prev/event-id` is the linear predecessor in same session; cross-session causality is `causal-links`.
* Progress events from a child are appended to the **child's session chain** with linear `prev`; parent observes via `causal-links` edge, not by inheriting child's `seq`.
* Hash chain: each session's event chain is independent and verified by `store/event verify-event-chain`. Parent and child interleave only via `causal-links` edge of type `:subagent/result`.

### 2.3 Cancel and cascade (S4, P1 DB truth + Work)

```
intent/subagent-cancel  { :session/id target, :work/id target-work, :reason ∈ {:user-request :parent-cancel :timeout} }
  → dispatch.clj → subagent/cancel-subagent! or cancel-subagent-tree!
       * resolves target set: single child Work or whole BFS descendant tree
         via works.parent_work_id + subagent_links (both, for compat)
       * for each target: CAS cancel-work! (queued|running|waiting → cancelled)
       * revoke-leases! for each target's leases (perm-model §3, P1 DB truth: UPDATE WHERE revoked=0 then cache tombstone)
       * appends :session/cancelled or :subagent/cancelled event with prev → target's latest and causal-links → parent
       * next intent on that child → :capability/denied (broker sees revoked Principal's lease)
```

Key property: **cancellation is transitive**. `cancel-subagent-tree!` with root Work `R` computes `list-descendants(R)` as BFS closure over `works.parent_work_id` (and `subagent_links`) and CAS-cancels every descendant + revokes leases idempotently via P1 DB truth. A child cannot outlive its parent's revocation — its next broker call is denied.

The test `subagent_cancel_test.clj` proves: parent revoked ⇒ child and grandchild leases all revoked (DB rows `revoked=1`) ⇒ their next intents are `:capability/denied`. Hydration of a cancelled Work returns a handle whose leases fail closed.

### 2.4 Result delivery (S5, E1 causal-links)

```
child Work terminal (succeeded|failed|cancelled|timed-out)
  → deliver-result! or deliver-failure! (db, parent-work-id, child-work-id, child-terminal-event-id, result/cas-ref or error)
       * validates CAS reference shape (sha256:…)
       * checks child Work is in terminal state (non-terminal delivery rejected)
       * appends :subagent/result event to PARENT session chain with
         prev/event-id = parent's immediate predecessor (same-session linear)
         causal-links  = #{ {:from <child-terminal-event-id> :type :subagent/result} }  (cross-session edge)
         payload { :child/work-id, :child/state, :result/cas-ref or :error }
       * atomically: event append + Work link update in BEGIN IMMEDIATE (same discipline as command outbox)
  → recovery: orphaned child Works (parent terminal while child still running/waiting)
    are surfaced by store/work find-orphaned-works (W2) and reported, never fabricated as succeeded
```

The `:subagent/result` intent type is the parent-side handle for awaiting a child's artifact. Delivery validates that the `from` event exists (any session) and that `prev` is same-session linear. Legacy `:cause/event-id` alias is accepted as `prev` when `causal-links` is absent (same-session only, deprecated).

The `tool.specs` canonical pair `:agent/spawn` / `:agent/status` (S6) is the model-facing façade: `activate_skill`-style tool names dispatched to `agent-spawn-provider` / `agent-status-provider` in `runtime/subagent.clj`, now operating on Works.

---

## 3. The `subagent_links` graph (retained) + `works.parent_work_id` (canonical)

Parent links live in two places for migration: the dedicated helper table `subagent_links` and the canonical self-FK `works.parent_work_id`. New code writes both; read paths prefer `parent_work_id` but fall back to the helper.

### 3.1 DDL

```sql
-- helper retained (store/session.clj + recovery.clj + migration backfill)
CREATE TABLE IF NOT EXISTS subagent_links (
  child_session_id  TEXT PRIMARY KEY,
  parent_session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
  created_at        TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS subagent_links_parent_idx ON subagent_links(parent_session_id);

-- canonical (018-work.sql)
-- works.parent_work_id TEXT REFERENCES works(id) ON DELETE SET NULL
CREATE INDEX IF NOT EXISTS works_parent_idx ON works(parent_work_id) WHERE parent_work_id IS NOT NULL;
```

* `child_session_id` PRIMARY KEY — a child has exactly one parent (session graph).
* `works.parent_work_id` is the Work graph — a child Work has exactly one parent Work.
* `ON DELETE CASCADE` / `SET NULL` — deleting a session/work cascades helper links but never fabricates delivery.
* `store/session.clj` helper `ensure-subagent-link-table!` is idempotent, so in-mem DB tests get the table without full migration chain.

### 3.2 Queries over the graph

* `get-parent-session-id(child)` — single hop upward via `subagent_links`.
* `get-parent-work-id(child-work)` — single hop upward via `works.parent_work_id`.
* `child-session-ids(parent)` / `child-work-ids(parent-work)` — single level downward, ordered by `created_at`.
* `subagent-depth(session)` — walks upward via `get-parent-session-id` to count depth (0 for root).
* `list-descendants(root)` — **BFS closure** over `child-work-ids` (canonical) union `child-session-ids` (compat), not including `root` itself. Cycle-free by construction (child inserted only after parent exists, `parent_work_id` FK + `child_session_id` PK forbid second parent). This is the function that `cancel-subagent-tree!` uses to compute the revocation set.

---

## 4. Limits — depth and budget (W2)

```clojure
(def ^:const max-subagent-depth 5)        ; ancestor chain length via works.parent_work_id
(def ^:const max-spawns-per-parent 10)    ; direct children per parent Work
```

Stored as provider-side checks in `check-depth-and-budget!` (called from `spawn-subagent!` before insertion). Depth is per-chain (grandchild depth = parent depth +1, rooted at outermost parent Work at depth 0). Budget is per-parent branching factor. Violations throw typed error (`:subagent/depth-exceeded` or `:subagent/budget-exceeded`) — the spawner, not the child, fails.

These caps are independent of the per-Work `deadline` that drives `timed-out` via `timeout-work!` (W2). S6 also wires the broker tool catalog limits through `agent-spawn-tool` descriptors.

---

## 5. Causal chain — E1 refinement (replaces GC-20 cause same-session)

Every subagent effect is captured via **E1 split**:

* `prev/event-id` — linear predecessor in the **same session** (nil only for root `:session/created`). Validated at append: must be same session and immediately preceding (`event_seq = new-seq -1`).
* `causal-links` — **cross-session graph** `${:from :type}` where `:from` may be any prior event (any session) and `:to` is implicitly the appended event. Validated at append: `from` must exist, `:type` must be keyword. Cross-session is allowed.

`append-event!` persists `prev` as `prev_event_id` column and each causal link as a row in `causal_links(from_event_id, to_event_id, link_type)` (migration `017-event-prev-causal-links.sql`). This is how `recovery` reconstructs orphan relationships without fabricating completion and how `verify-event-chain` checks both the linear hash chain and the semantic graph. Legacy `:cause/event-id` is accepted as deprecated alias for `prev` when `:causal-links` absent (same-session only).

### 5.1 Wolfram checks for this model [W-27..W-32] — E1 causal refinement (new, 6 checks)

| Check | Predicate | Meaning | Result |
|-------|-----------|---------|--------|
| [W-27] | `prevStrictlyEarlierQ` | `prev/event-id` (when present) is strictly earlier in same session (`event_seq` < new-seq) | pass |
| [W-28] | `causalLinkFromExistsQ` | every `causal-links` `:from` references an existing event (any session) | pass |
| [W-29] | `rootNoPrevOrLinksQ` | root `:session/created` carries `nil` prev and empty causal-links | pass |
| [W-30] | `crossSessionCausalLinksAllowedQ` | causal-links MAY cross sessions (e.g. child terminal → parent result) while `prev` MUST NOT | pass |
| [W-31] | `subagentResultCarriesCausalLinkQ` | `:subagent/result` parent event carries `causal-links #{ {:from <child-terminal> :type :subagent/result} }` and linear prev | pass |
| [W-32] | `hashChainIncludesPrevQ` | `verify-event-chain` checks header hash per event including `prev/event-id` and `causal-links` digest; tampering changes digest and is detected | pass |

These six replace the former same-session `causeStrictlyEarlierQ` ([W-26] heritage) with the E1 split vocabulary. The old session SM checks [W-16..W-19] are retired — subagent lifecycle is now Work SM (async-model [W-19..W-24]).

### 5.2 Invariants that moved

* **Same-session cause** → **prev linear same-session + causal-links cross-session**: old invariant `cause must be same session` is removed; new invariants are [W-27] (prev same-session) and [W-30] (causal-links cross-session allowed).
* **Dual-anchor** → **Principal single field**: old `dual-anchor subject` is removed from subagent derive; new principal derive is I2 single-field equality.
* **Session budgetExhausted** → **Work failed via H1**: budget exhaustion now fails the child Work via `fail-work!` with typed error, not a distinct session terminal.

---

## 6. H1 Hydration pin — CodeImage/Deployment/Execution (I1)

A session's pin `{:code_image/id :deployment/id :execution/id :generation/id}` is immutable (I1 splits Phenotype into CodeImage/Deployment/Execution). `runtime/hydrate hydrate(pin) → ExecutionHandle { code-image, deployment, execution-id, compiled, leases }` loads the compiled code image, bindings, authority (leases from DB P1), and returns a fresh handle. Subsidiary works reuse the parent's pin unless they explicitly carry a new deployment (subagent inherits parent pin via `derive-and-publish!` at spawn time but may be re-pinned on promotion).

Hydration invariants (async-model [W-25..W-26]):

* `hydrate` with a valid pin always yields an `ExecutionHandle` whose `code-image.id` equals the pin's `code_image/id` (closed, no fallback to synthetic leases).
* `hydrate` with an unknown pin throws `:hydrate/pin-not-found` (fail-closed, P1 DB truth — no synthetic lease).
* `hydrate` never mutates the DB; publication belongs to the downstream `Projector→Bundle` transaction (INV-06).

---

## 7. Wolfram verification summary (32 checks, all pass — this file contributes 6)

The subagent model contributes **6** of the 32 checks ([W-27..W-32] E1 refinement):

```
[W-27] prevStrictlyEarlierQ            pass
[W-28] causalLinkFromExistsQ           pass
[W-29] rootNoPrevOrLinksQ              pass
[W-30] crossSessionCausalLinksAllowedQ pass
[W-31] subagentResultCarriesCausalLinkQ pass
[W-32] hashChainIncludesPrevQ          pass
```

Together with perm-model [W-01..W-18] (18) and async-model [W-19..W-26] (8), the full Wolfram suite is **32/32 pass**. The old subagent SM [W-16..W-19] is retired (heritage docs note the mapping to Work). The E1 refinement was true on the first refined pass; no rephrasing needed.

---

## 8. DAG verification (for this model's position — V1)

Condensed from `local://evoclj-reconstruction-dag-2.md` V1 §3–§4 (same Wolfram graph that secures perm-model and async-model):

* **23 nodes, 29 edges; acyclic = true**. V1 adds I1/I2/C1/C2/C3/H1/W1/W2/P1 (8 nodes) to prior 21.
* Sources `{P1, A1, S1}` — S1 (intent type schema) is this chain's source and can start alongside P1 and A1.
* Sinks `{P7, V2}` — P7 optional leaf; subagent chain sinks at S6 then V2.
* Edges touching this model: `I2→S2` (Principal single field must land before spawn), `C2→S2` (Grant lattice before attenuation), `H1→S2` (hydrate before child runtime), `E1→S2` (E1 prev/causal-links before spawn), `S1→S2`, `P1→S4` (DB truth before cascade), `S2→S3`, `S2→S4`, `S2→S5`, `S3→S6`, `S5→S6`, `P5→V1`, `S4→V1`, `P5→V1`, `S6→V2`.
* 7 waves; this model spans W2 (E1), W4 (S2+H1), W5 (S3+S4+S5), W6 (S6), W7 (V2 doc).

| Wave | Subagent work |
|------|---------------|
| W2 | **E1** — `prev/causal-links` split, migrations 017 |
| W4 | **S2** — `spawn-subagent!` via Work child + Principal + H1 hydrate + P1 DB leases + `subagent_links` compat; **H1** — `hydrate(pin)` factory |
| W5 | **S3** — child SCI via ExecutionHandle; **S4** — `cancel-subagent!` + BFS cascade via works.parent_work_id + P1 revoke; **S5** — `deliver-result!` with E1 causal-links + orphan recovery via Work |
| W6 | **S6** — `:agent/spawn|:agent/status` broker surface via Work; **V1** doc refinement |
| W7 | V2 doc closure |

* **Critical path** is `I1 → I2 → P1 → C2 → P5 → S2 → S3 → S6 → V2` — S2 cannot start before I2 (Principal) and C2 (Grant). The Work chain (W1/W2) is now on the critical path via `W1→S2`.

---

## 9. References

* Design source: `local://evoclj-reconstruction-dag-2.md` V1 (E1/H1/W1/W2/P1), `local://evoclj-implementation-dag.md` §1.4, §2 rows S1–S6, §3 edges, §4 waves/critical path.
* Implementation: `src/evoclj/intent/schema.clj` (S1 intent types with parent Work), `src/evoclj/runtime/subagent.clj` (spawn/run/cancel/result via Work), `src/evoclj/runtime/hydrate.clj` (H1 pin → ExecutionHandle), `src/evoclj/store/work.clj` (Work table), `src/evoclj/store/event.clj` + `event_schema.clj` (E1 prev/causal-links), `src/evoclj/store/session.clj` (immutable pin + `subagent_links` compat), `src/evoclj/store/recovery.clj` (orphans via Work).
* Tests: `test/evoclj/intent/subagent_intent_test.clj`, `test/evoclj/runtime/subagent_spawn_test.clj`, `test/evoclj/runtime/subagent_run_test.clj`, `test/evoclj/runtime/subagent_cancel_test.clj`, `test/evoclj/runtime/subagent_result_test.clj`, `test/evoclj/runtime/subagent_tool_test.clj`, `test/evoclj/runtime/hydrate_test.clj`, `test/evoclj/store/event_test.clj` (E1), `test/evoclj/store/work_property_test.clj` (W2 100 rounds).
* Wolfram & hash discipline shared with `perm-model.md` and `async-model.md`; this file contains no hash-shaped bare tokens and passes `scripts/verify-doc-hashes.clj` exit 0 without exemptions. The `sha256:` literals shown are stripped by rule E2.
