# MCP Gap Closure Report — 2026-08-20

> **STATUS: SUPERSEDED — historical record, NOT the current closure truth.**
>
> This report documents the **2026-08-20** MCP gap-closure pass (6 steps,
> 26 gaps A1–G3, `mcp:2.0.0`). It is retained for historical context only and
> is **no longer authoritative**. The authoritative, current closure and
> repair state for the MCP/Skills effort is the live repair ledger
> [`docs/codebase/REPAIR-PLAN.md`](../../codebase/REPAIR-PLAN.md) together
> with the invariant checklist [`docs/invariants.md`](../../invariants.md).
> The MCP closure-repair phase that supersedes this report landed at commit
> `c19e3d9` (see the live work-group ledger's M20 entry).

6 steps, 26 gaps (A1–G3), `mcp:2.0.0` pinned. All gates pass.

## Summary

| Step | Focus | Gaps closed | Gate |
|------|-------|-------------|------|
| 1 | CallContract / DescriptorGeneration | A4, E2½, E3½ | pass |
| 2 | Schema layering & canonical pipeline | A1, A2, A3 | pass |
| 3 | Host-owned McpManager | D1–D6, F2, B2 remainder | pass |
| 4 | Error/effect algebra + wire bytes | C1, C2, C3, F1 | pass |
| 5 | CanonicalResource second-layer scope | B1, B3 | pass |
| 6 | Versioned ProtocolAdapter | G1, G2, G3, F3 | pass |

26/26 closed. 3 were partial before (A1/A3/B2) — now complete.

## Steps

### 1 — Freeze descriptor generation
Freeze `D_normalize=D_authorize=D_execute=D_validate`. Move refresh before normalize, forbid `reset!` after call-started. Add `generation` + `freshness(:required/:best-effort/:pinned)` with `:best-effort` stale audit flag and `:required` fail-closed `:provider/freshness-required`.

### 2 — Split schema layers, canonicalize
Descriptor → `:provider/*` vs `:mcp/*` + `:mcp/*-json`. Pipeline: `args → value->canonical (keyword→string) → Malli(:provider) → networknt(:mcp 2020-12) → serialize`. Late `edn->json-compatible` removed. `$ref` external deny, depth/node/time/regex budgets. Validate `structuredContent` via `:mcp/output-schema` then envelope via `:provider/output-schema`.

### 3 — Host-owned manager
`src/evoclj/mcp/manager.clj` Integrant `:mcp/manager` owns pools. `ConnectionKey=[version conn-id transport-identity cred-fingerprint]` (`:auth/ref` hash, never secret). Pool ops single `swap!`, single-flight promise for first open, `ready→broken→reconnecting→ready(gen++)` healing, refcount per owner (acquire at construction, release at dispose). `halt!` closes all. Supersedes global atoms.

### 4 — Typed error/effect algebra
Delete regex heuristics; `stable :error/type → known SDK class → cause chain`. `isError=true` returns `{:mcp/tool-status :error ...}` not throw — treated as `:ok` model-visible, no retry. Effect journal `proposed→authorized→call-started(gen+idempotency)→committed/rejected/ambiguous`; ambiguous never blind-retried. `raw-size-bytes` from serialized wire bytes.

### 5 — CanonicalResource object scope
`src/evoclj/mcp/canonical.clj` shapes `{:kind :filesystem/path ...}` etc. `normalize-request` parses canonical resource (path normalized, `../` collapsed). `broker` enforces `may invoke tool? ∧ may access resource?`. `:present→:removed`, `:newly-discovered→:discovered-ungranted` — `:removed` fails closed.

### 6 — Versioned ProtocolAdapter
`src/evoclj/mcp/adapter.clj` protocol `(discover/wire-request/on-notification/cache-policy/continue)` with `MCP-2025-11` (sync client + initialize, sessionful) and `MCP-2026-07` (stateless `_meta`, cacheable `tools/list` ttl/scope, subscriptions, 2020-12 budgets, MRTR/Tasks continuation). Kernel above adapter. `call-tool-streaming` renamed/honest.

## Files changed (HEAD~6..HEAD)

```
src/evoclj/mcp/adapter.clj          — new ProtocolAdapter + impls
src/evoclj/mcp/manager.clj          — new host-owned manager
src/evoclj/mcp/canonical.clj        — canonical shapes + wiring fixes
src/evoclj/mcp/json_schema.clj      — budgets, validator hardening
src/evoclj/provider/mcp_bridge.clj  — freeze generation, manager injection
src/evoclj/capability/broker.clj    — second-layer scope
test/evoclj/mcp/adapter_test.clj    — adapter equivalence + cache/continuation
.tmp/mcp-gap-fix-flow.json          — flow definition (not shipped)
```

Commits (historical):

The six abbreviated SHAs that originally accompanied these entries were
rewritten away and **no longer resolve** to any commit in this repository,
so they are intentionally omitted to keep this documentation hash-traceable
under INV-08. See the SUPERSEDED notice above for the current authoritative
pointer; the original subjects are retained as historical context:

* fix(mcp): correct manager status transitions syntax
* fix(mcp): freeze descriptor generation
* fix(mcp): enforce second-layer scope and stabilize adapter wiring
* feat: host-owned MCP manager with pooled single-flight connections
* feat: enforce JSON Schema budgets and harden validator limits
* feat: add versioned protocol adapter with cache and continuation support

## Closure map A1–G3

| Gap | Status | Step |
|-----|--------|------|
| A1 JSON Schema sound | closed | 2 |
| A2 keyword/string drift | closed | 2 |
| A3 outputSchema mix | closed | 2 |
| A4 descriptor TOCTOU | closed | 1 |
| B1 tool-only lease | closed | 5 |
| B2 secret in error data | closed | 3 |
| B3 stale tool surface | closed | 5 (+1,3) |
| C1 wrapper misclassified | closed | 4 |
| C2 isError as failure | closed | 4 |
| C3 idempotency global | closed | 4 |
| D1 refcount per call | closed | 3 |
| D2 conn-id not bound | closed | 3 |
| D3 stale-read lost update | closed | 3 |
| D4 double-open race | closed | 3 |
| D5 dead-but-open | closed | 3 |
| D6 global ownerless pool | closed | 3 |
| E1 toolsChanged not wired | closed | 3 |
| E2 refresh only invalidate | closed | 1+3 |
| E3 refresh failure swallowed | closed | 1 |
| E4 no removal transition | closed | 5 |
| F1 raw-size-bytes chars | closed | 4 |
| F2 managed counters volatile | closed | 3 |
| F3 streaming not streaming | closed | 6 |
| G1 sessionful hardwired | closed | 6 |
| G2 no cache/subscription | closed | 6 |
| G3 no MRTR/Tasks | closed | 6 |

## Verification

`git log --oneline -6` all present. Gates s1–s6 `verdict:pass` (0 failures, keyword-drift/oneOf checks, 50× concurrent acquire no lost update, double-open single-flight, isError model-visible, scope-denied on `/etc/shadow`, adapter equivalence). `mcp:2.0.0` pinned throughout.

## Residual

* Global-lock ceiling tagged `# ponytail:` — future shard if contended.
* `C2` breaking: callers check `:mcp/tool-status :error` not `catch :mcp/tool-error`.
* `B2` breaking: transport config now `{:auth/ref ...}` — subtree redaction for `:env/:headers`.
