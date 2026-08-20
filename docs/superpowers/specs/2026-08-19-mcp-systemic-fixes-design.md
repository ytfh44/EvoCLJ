# EvoCLJ MCP Subsystem — Systemic Fix Design

Date: 2026-08-19
Scope: Close the open control-plane / feedback-plane / lifecycle gaps in the
MCP provider bridge (Layers 1–5 already closed the data-plane and the
legacy-compatible 2025-11-25 client surface).

Constraint carried forward: **keep the MCP Java SDK pinned at 2.0.0 (legacy
era)**. No upgrade to 2026-07-28 stateless MCP. Behaviour must remain
compatible with legacy servers (stdio + Streamable HTTP + SSE fallback).

## 0. Current state (ground truth after Layers 1–5)

Reading the actual files (`mcp_bridge.clj`, `client.clj`, `registry.clj`,
`protocol.clj`, `error.clj`, `dispatch.clj`):

- **Registry does NOT feed a stale descriptor into dispatch.** `register!`
  caches `:descriptor` only to validate at registration time. The dispatcher
  (`dispatch-registered!`) re-derives the descriptor live via
  `(proto/describe provider)` at dispatch time, which reads the provider's
  `descriptor-atom`. So issue #1 (three-world split-brain) is **largely already
  closed** by the live re-describe. We keep that invariant and only document it.
- **The remote JSON Schema is silently discarded (issue #2, confirmed).**
  `java.util.Map` (what the 2.0 SDK returns for `inputSchema`/`outputSchema`)
  returns `false` for Clojure `map?`, so `list-tools` falls through to `{}`, and
  `json-schema->malli` of `{}` → `:any`. After a refresh, validation becomes
  `:any` = fail-open.
- **`result->edn` validates the wrong object (issue #3).** It derives the
  result value from `:mcp/content` (text blocks), but `:output-schema`
  describes `CallToolResult.structuredContent`. The two channels are not joined.
- **`content-block->edn` is missing `:audio`/`:resource-link` (issue #4).**
  `client.clj` decodes them, but the bridge drops them to `(:content/raw
  block)` which is `nil` for those types.
- **Dead-but-open connections are not healed (issue #5); pool refcount counts
  invocations not owners (issue #6); cross-server contamination bypasses the
  check (issue #7); pool is JVM-global (issue #8); `shutdown-pool!` is never
  called on halt (issue #9).**
- **Notifications are not wired into EvoCLJ (issue #10); `refresh-provider!`
  only resets a timestamp (issue #11); removed tools leave stale providers
  (issue #12); all non-tool errors collapse to `:provider/transient-error`
  (issue #13); idempotency/`wall-ms` never cross the boundary (issues #14/#15);
  audit lives in Clojure metadata, not durable data (issue #17); `:env`
  secrets in transport-config are not redacted (issue #18).**

Severity tiers used below: **P0** = correctness/safety regression or fail-open;
**P1** = lifecycle/resource correctness; **P2** = feedback/observability.

## 1. Fix clusters (execution order)

### Layer A — Output & content correctness (P0)
Addresses #3, #4, #17.

1. **Dual-channel result envelope.** `result->edn` returns an explicit map
   `{:value <envelope> :audit <map>}` where `<envelope>` is:
   - `{:mcp/structured-content <map>}`, when `:mcp/structured-content` is
     present (the canonical validated output), plus
   - `{:mcp/model-content <blocks>}` (the human/content blocks, sandboxed).
   Audit is an **explicit top-level key**, not Clojure metadata (#17).
2. **Output schema validates the right object.** When a remote `outputSchema`
   is present, `execute-request!` validates `:mcp/structured-content` against
   it (fail-closed). The host-declared `:output-schema` continues to validate
   the whole envelope. Which schema wins is decided by presence of a remote
   `outputSchema` (see Layer B converter output).
3. **`content-block->edn` gains `:audio` and `:resource-link`** returning
   sandboxed placeholders (`{:mcp/content-type :audio :mcp/sandboxed true
   :mime-type ...}`, same shape as `:image`) instead of `nil`.

### Layer B — Schema & error safety (P0)
Addresses #2, #13.

4. **JSON Schema normalization helper** `normalize-json-schema` that converts
   a `java.util.Map<String,Object>` (string keys, possibly nested) into a
   plain Clojure map with **string keys** recursively. Then `json-schema->malli`
   reads string keys (`"type"`, `"properties"`, `"required"`, `"items"`).
   This alone makes the existing converter actually see the schema.
5. **Fail-closed, not fail-open.** Cases the converter cannot *prove*
   equivalent to a Malli primitive (enum/const/$ref/oneOf/anyOf/allOf/
   additionalProperties/pattern/min-max/null/format-dependentSchemas) keep the
   **raw JSON Schema object** as the descriptor value under a new key
   `:output-schema-json` / `:input-schema-json`, and validation runs a small
   native JSON-Schema validator (`json-schema/validate`) rather than degrading
   to `:any`. A true `:any` is only emitted for an explicitly empty/absent
   schema. No unsupported schema silently becomes `:any`.
6. **Error classification in the bridge (fixes #13).** `execute-request!`
   catches transport/protocol errors (via `classify-mcp-error` from client.clj,
   or its own check) and emits `:provider/transient-error` **only** for those;
   tool/business/authorization/protocol errors become `:provider/execution-failed`
   (or a typed `:provider/business-error`), so the dispatcher does NOT retry
   permanent failures. Idempotency hints from the remote tool annotation are
   surfaced but never auto-trusted for retry (host policy decides).

### Layer C — Connection pool & lifecycle (P1)
Addresses #5, #6, #7, #8, #9.

7. **Self-healing write-back (#5).** After a successful `call-tool` on a
   non-shared provider, write the (possibly reopened) managed record back into
   `client-atom`. On a transport failure, mark the managed record unhealthy
   so the next `ensure-open` actually replaces it (close + reopen), instead of
   retrying the same dead client.
8. **Refcount per owner, not per call (#6).** Move acquire/release to provider
   lifecycle: `mcp-provider` acquires a pool slot when constructed and releases
   on a new `close`/`dispose` method (extend `Provider` protocol with an
   optional `dispose!`). The invocation path no longer mutates the refcount.
9. **Contamination-safe pool key (#7).** Pool keyed by
   `[connection-id normalized-transport-identity]`; `pool-acquire!` validates
   the expected transport identity atomically, so Provider B cannot ride
   Provider A's connection.
10. **Host-owned manager (#8/#9).** Extract `evoclj.mcp/manager` holding
    `connection-pool`, `provider-refresh-fns`, and a notification router,
    constructed as an Integrant component. Providers hold a reference to the
    manager (passed in opts). `shutdown!` on the manager closes all pooled
    connections; wire it into the host/system halt (`kernel.system` /
    `runtime.system`) so MCP OS/network resources are released on halt.

### Layer D — Feedback, refresh & secrets (P2)
Addresses #10, #11, #12, #18.

11. **Notifications into EvoCLJ (#10).** `mcp-provider` passes
    `tools-change-consumer`/`progress-consumer` to `mcp-client/open!`.
    `tools/list_changed` invalidates the provider descriptor (re-list on next
    execute, see #12). `progress` appends an `:mcp/progress` event to the
    runtime event store with `:intent/id`/`:session/id` (passed via provider
    opts or a dynamic var).
12. **Refresh semantics (#11).** Split into `invalidate-schema!` (clears
    `:mcp/last-refreshed`) and `refresh-schema-now!` (does the actual
    `list-all-tools` + descriptor update synchronously, with tool-addition and
    tool-removal handling). `refresh-provider!` → `refresh-schema-now!`.
13. **Tool deletion (#12).** On re-list, a previously-present tool that is now
    absent is marked `:mcp/status :removed` in the descriptor (or
    unregistered from the registry if this provider owns exactly that tool); a
    subsequent call fails closed with `:provider/tool-removed`. Newly-present
    tools are recorded as `:mcp/status :discovered-ungranted` (never auto-granted).
14. **Env secret redaction (#18).** `kernel.error/sanitize` treats any value
    under a `:env` key (and well-known `:authorization*`/`*token*`/`*secret*`
    string keys) as sensitive by default, redacting it before it crosses a
    serialization boundary. No resolved secret is placed in transport-config
    EDN.

### Explicitly deferred (documented, not in this pass)
- **#1** already closed by live re-describe (documented, no code change).
- **#14/#15** (idempotency key / wall-ms across the boundary): 2026 `_meta`
  is not available on the legacy SDK; we preserve the idempotency key in the
  canonical resource for EvoCLJ's own ledger but do not transmit it. Full
  cross-boundary enforcement needs the stateless protocol upgrade, which is out
  of scope.
- **#16** (resource-level capability, e.g. per-path filesystem authority):
  a separate provider-design feature; left as a future extension with a clear
  hook (`normalize-request` may already emit a finer `:resource`).

## 2. Testing strategy
- Layer A/B: unit tests for `result->edn` (structuredContent path),
  `content-block->edn` (audio/resource-link), `json-schema->malli` (java.util.Map
  with string keys, unsupported-construct → JSON-Schema validator, never `:any`),
  and bridge error classification (tool-error vs transient vs execution-failed).
- Layer C/D: pool refcount (per-owner), contamination rejection,
  `shutdown!` closes children, notification routing (event store append +
  descriptor invalidation), `refresh-schema-now!` (add/remove), env redaction.
- The existing real-server integration test (`mcp-dispatch-test` against
  sequential-thinking) must stay green across all layers; it exercises the
  full broker→provider→client→server path.

## 3. Rollout
Implemented as serial layers A → B → C → D. Each layer is implemented by a
coder subagent, reviewed by a second subagent, then committed by the
coordinator only if the review is clean and the test suite stays green.
