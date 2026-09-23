# Semantic Verification Report

**Scope:** formal model + real-code verification of the ten core semantic
claims of EvoCLJ, per subsystem. Every check drives the REAL production
namespaces (no mocks); the models are stated explicitly and the invariant
is enumerated exhaustively where the domain is finite.

**How to re-run:**

```bash
for f in scripts/verify-semantics/verify*_*.clj; do clojure -M "$f"; done
```

**Result:** 10/10 suites, 173 assertions, 0 failures (2026-09-24).

---

## 1. Atomic CURRENT compare-and-set — `verify1_cas.clj`

**Model.** A promotion transaction is `{cond: current == expected, act:
current = id}`. Under BEGIN IMMEDIATE the whole transaction is one atomic
step; at statement granularity the cond and act interleave.

**Enumerated.** All interleavings of two sibling promotions:

- atomic model, both orders: exactly one winner, exactly one CURRENT, the
  loser is `:stale` — safe.
- statement-level model: **4 of 6 interleavings are broken** (two CURRENTs
  or a lost pointer). This is the formal justification for BEGIN IMMEDIATE:
  the CAS must be atomic, and the code relies on SQLite's write lock for
  that atomicity (documented in `promotion/current.clj`).

**Real code.** On a migrated store, `cas-current! G42→G43a` returns `:ok`,
the stale sibling `cas-current! G42→G43b` returns `:stale`, and exactly one
CURRENT row exists pointing at the winner. The concurrency suite
(`test/evoclj/adversarial/concurrency_test.clj`) additionally pins the
races with latches + the `:failpoint` seam.

**Finding (doc/API contract — FIXED).** `sqlite/with-db`'s docstring said it binds a "connection"; the binding is actually the java.jdbc **spec-with-connection map** (`jdbc/with-db-connection` semantics — the live `java.sql.Connection` lives under the map's `:connection` key), as `candidate.clj`/`session.clj` docstrings already recorded. Code and comments disagreed — and the disagreement ran both ways (one docstring said "connection", two said "spec-with-connection map"). Audited every `with-db` call site: no latent misuse exists — java.jdbc high-level fns receive the spec-with-connection map, and the raw-JDBC primitives (`cas-current!`, `read-current`) receive an explicit `(jdbc/get-connection spec)` (as `promote.clj`/`rollback.clj` do). Both docstrings (`with-db`, `enable-foreign-keys!`) were rewritten to state the exact contract; the code needed no change.

## 2. Event hash-chain tamper-evidence — `verify2_hashchain.clj`

**Model.** `hash_i = H(header_i ∥ hash_{i-1})`. Inductive property:
tampering row *k* without recomputing *k+1..n* is always detected — at *k*
(own `:event/hash-mismatch`) or at *k+1* (`:prev-hash` link).

**Real code.** A 10-event chain built via `append-event!` verifies clean;
tampering **every** position (type rewrite after dropping the append-only
trigger, simulating a direct store write) is detected by
`verify-event-chain`. Note: the DB layer itself enforces append-only via
triggers (`events_no_update`/`events_no_delete`) — the hash chain is the
second, position-independent line of defense that detects *already-written*
tampering.

## 3. Deterministic canary allocation — `verify3_canary.clj`

**Model.** `bucket = sha256(key)[0:16] mod 10000 / 10000`; under the SHA-256
uniformity assumption `bucket ~ U[0,1)`, so `P(canary) = allocation` and the
count over n keys ~ `Binomial(n, p)`.

**Real code.** 10,000 deterministic keys through `routing-bucket`:

| p   | observed | μ (np) | σ        | z    |
|-----|----------|--------|----------|------|
| 0.10| 1003     | 1000   | 30.0     | 0.10 |
| 0.25| 2504     | 2500   | 43.3     | 0.09 |
| 0.50| 4969     | 5000   | 50.0     | -0.62|

Bucket mean 0.4998, variance 0.08354 (U[0,1): 0.5, 0.08333). Pure function
(same key → same bucket), ladder monotonic (canary(10%) ⊆ canary(25%)).

## 4. Lexicographic eligibility (GC 14) — `verify4_eligibility.clj`

**Model.** `eligible? = AND(no hard violation, utility-delta ≥ min-delta,
every cost ratio ≤ max-cost-regression, complexity guard if declared)` —
short-circuiting, no weighted compensation.

**Real code.** Exhaustive enumeration over
{hard pass/fail} × {5 utility deltas} × {5 cost ratios} against
`compare/eligibility`:

- hard violation ⇒ ineligible for **every** utility/cost combination (25
  combos) — utility cannot compensate (GC 14);
- utility < min-delta ⇒ ineligible (15 combos);
- cost ratio > ceiling ⇒ ineligible even at utility +0.20;
- the decision is a **step function at min-delta**:
  `[-0.1 false] [0.0 false] [0.049 false] [0.05 true] [0.2 true]` —
  exact threshold behavior, no noise tolerance below the floor.

## 5. Frozen evidence packs — `verify5_evidence.clj`

**Model.** `pack = f(episodes with last_event ≤ cutoff)`, f pure and
deterministic; the selection predicate is monotone in the cutoff, so
episodes arriving after the cutoff are outside f's input set and the pack
(id + content) is invariant to them.

**Real code.** 5 episodes before cutoff 10 (3 completed, 2 failed) + 1 late
episode (last_event 12) + a second late arrival: the pack excludes the late
episodes, `:summary` reports `{:successes 3 :failures 2}`, rebuilding yields
the identical content-addressed id, and raising the cutoff to 20 admits the
late episode.

## 6. Content addressing determinism (GC 1, 6) — `verify6_content_addressing.clj`

**Model.** `ID = sha256(sorted index of (path, digest) lines)`; file digest
over LF-normalized UTF-8 bytes.

**Real code.** Same seed tree loaded twice → identical ID; a CRLF-rewritten
copy → **identical** ID (line-ending normalization); a single content change
that stays valid EDN (`:max-steps 64` → `65`) → different ID; shuffled entry
order → identical tree-digest; and `genome/id` equals the tree-digest of its
own entries exactly (the ID is the canonical tree digest).

## 7. Work state machine closure/legality — `verify7_work_states.clj`

**Model.** Directed graph over the seven Work states defined by
`evoclj.runtime.work/work-transitions`; terminal states are sinks and the graph
is acyclic.

**Real code.** The verifier calls the production Work predicates for closure,
acyclicity, terminal sinks, and reachability, then checks every ordered state
pair against the production transition table.

## 8. Session pin identity (GC 2, 20) — `verify8_pin_identity.clj`

**Model.** A pin is `{session/id, code/id, deployment/id, execution/id}` and
admission is a conjunction of existence and agreement constraints over the
three I1 identity tables:

    executions.deployment_id = pin.deployment/id
    executions.code_image_id = pin.code/id          (agreement)
    deployments.code_image_id = pin.code/id         (agreement)
    code_images.id           = pin.code/id          (agreement)
    every named row EXISTS                          (existence)

    accept <=> all conjuncts hold

**Enumerated.** The domain is finite (four ids, three tables, one row each),
so the script drives the two failure modes that matter — a MISSING row and a
DISAGREEING row — plus the consistent case.

**Real code.** Real SQLite + real `store.migrate`: `store.identity/record-*!`
write each row once and are idempotent under replay; the readers round-trip
the rows; `store.session/create-session!` persists the identity triple on the
sessions row and `row->session` reads it back; `runtime.hydrate/verify-pin!`
admits the consistent pin and rejects (with `:hydrate/pin-mismatch`) both a
pin whose rows disagree and a pin naming an unregistered execution. A session
whose identity was never registered cannot be executed — the pin check is
fail-closed, not fail-open.

## 9. One hard-gate rule (GC 14) — `verify9_hard_gate_single_src.clj`

**Model.** The G6 hard gate (`evoclj.eval.core`) and the eligibility decision
(`evoclj.eval.compare`) answer the same question — does this summary violate
the profile's cost/complexity thresholds? Two implementations could disagree;
the claim is that the gate derives its reasons from
`compare/thresholds-for` + `compare/guard-reason` over the SAME
`metrics/cost-regressions` / `metrics/complexity-regressions` records, so the
answers are equal by construction.

**Enumerated (finite domain).** Cost over the max; complexity over the max;
both over the max (cost is checked first); neither over the max; and
complexity left informational when the profile omits the guard — for each,
the eligibility reasons must equal the reasons recomputed independently
through `guard-reason`, item for item.

**Real code.** `compare/eligibility` returns exactly the reasons
`compare/guard-reason` produces (and, for the sample floor, exactly what
`statistics/promotion-checks` produces); `thresholds-for` merges the profile
declaration over the canonical defaults; a sample below `:min-pairs` and one
above `:max-candidate-failure-rate` are both ineligible. Structural half:
`src/evoclj/eval/core.clj` contains no `metrics/ratio` call and no numeric
cost threshold — it names the rule, not a second implementation of it.

## 10. State-table write ownership — `verify10_ownership.clj`

**Model.** Every mutable state table has exactly ONE writer module (its
owner). Ownership is a structural claim about the source, so it is checked
by reading the source: collect every SQL write naming table T from every
production namespace and assert the set of files that do so is exactly the
allowed set.

**Enumerated.** Six state tables — `works`, `capabilities`, `generations`,
`candidates`, `kernel_state`, `episodic_memory` — each checked twice: the
writer set equals the allowed set, and it is non-empty (a state table nobody
writes would be dead schema).

**Real code.** The scan is AST-based: each file is read as Clojure forms, so
docstring prose that mentions a statement is never mistaken for a write. A
write is a string or table keyword passed to an SQL-executing call
(`sqlite/insert-raw!`, `sqlite/exec!`, `jdbc/execute!`, `jdbc/insert!`,
`jdbc/update!`, and the promotion namespaces' private `raw-update!` /
`raw-insert!`) whose text matches `INSERT [OR …] INTO T` / `UPDATE T` /
`DELETE FROM T`. The two documented exceptions are `kernel_state` (the
singleton definition in `store/current-store` plus the CURRENT
compare-and-set in `promotion/current` — Global Constraint 15 makes the
promotion transaction the pointer authority) and `episodic_memory` (the
store owner plus the memory provider's duplicated statement, which the
provider-handle routing removes).

---

## Summary of findings

| # | Finding | Severity | Action |
| --- | --------- | ---------- | -------- |
| 1 | `with-db`/`enable-foreign-keys!` docstrings contradicted the real binding (spec-with-connection map, not a raw Connection) | Low (doc) | **Fixed**: docstrings rewritten to the exact contract; all call sites audited — no latent misuse, no code change needed |
| 2 | Statement-level CAS is broken in 4/6 interleavings | Informational | Confirms the necessity of BEGIN IMMEDIATE; already relied upon, now formally justified |
| 3 | Canary uniformity confirmed empirically (z ≤ 0.62 over 10k keys) | Informational | Binomial model holds; allocation percentages are accurate |
| 4–7 | All invariants hold under exhaustive/real-code verification | — | No changes required |
| 8 | The identity triple was written by the CLI but never read back by the pin check (a session could run against an unregistered identity) | High (correctness) | **Fixed**: the writers landed (C2.1) and `hydrate/verify-pin!` now fails closed on a missing or disagreeing row (C2.2); `verify8` pins the behavior |
| 9 | The G6 gate and the eligibility decision could drift apart (two implementations of the same threshold rule) | Medium (integrity) | **Fixed**: the gate consumes `compare/thresholds-for` + `compare/guard-reason` (C1.1); `verify9` pins the equality and the absence of a second ratio implementation |
| 10 | The CLI (and the harness) carried their own copies of state-table writes, reads, and decoders | Medium (integrity) | **Fixed**: the store layer owns the write statements and the row decoders (C4.1–C4.5); `verify10` pins the writer set per state table |

The ten core semantic claims of the system — atomic CAS with a single
winner, tamper-evident causal chains, deterministic proportional routing,
hard-constraint-dominant lexicographic comparison, frozen evidence
boundaries, canonical content identity, a closed state machine, a
fail-closed session pin identity, a single hard-gate rule, and one writer
per state table — are verified against the production code.
