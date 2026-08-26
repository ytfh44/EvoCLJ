# EvoCLJ Shared Invariant Checklist

This file is the common review baseline for every contributor and
adversarial reviewer working on the MCP/Skills closure repair effort. It
has two parts:

1. **Inherited constraints (GC-01 – GC-24)** — the Global Constraints of
   the implementation plan, restated as one-line reminders.
2. **Repair invariants (INV-01 – INV-09)** — additional invariants added
   by the repair plan, each grounded in a concrete observed incident,
   with its violation consequence and the machine-checkable guard that
   enforces it.

**Normative source:** part 1 is a *pointer*, not a replacement — the
authoritative wording of GC-01 – GC-24 is
`docs/implementation-plan.md` (the "Global Constraints" section,
lines 9–34). Where this file and the implementation plan disagree, the
implementation plan wins. Part 2 derives from
`docs/codebase/REPAIR-PLAN.md` (section "新增不变量", items 1–9) and the
confirmed findings recorded in `docs/codebase/BASELINE-TRIAGE.md` and the
repair ledger's progress log.

All `file:line` references below were valid at the time of writing; they
are evidence anchors, not stability promises — when a cited location
moves, update the citation together with the code change.

---

## Part 1 — Inherited Global Constraints (GC-01 – GC-24)

Normative source: `docs/implementation-plan.md:9-34`. Numbering below is
one-to-one with that list. Modality note: MUST/SHOULD strength follows the
original text; this table flattens SHOULD items (GC-13, GC-21) to
declarative form.

| # | One-line summary |
| --- | --- |
| GC-01 | A Genome is immutable and content-addressed. |
| GC-02 | Every session stays pinned to exactly one Genome ID and one Resolution ID for its whole lifetime. |
| GC-03 | A live Phenotype must not modify its own Genome in place. |
| GC-04 | Evolution produces successor candidates only through structured, deterministic mutations. |
| GC-05 | Every mutation identifies parent Genome, evidence, hypothesis, risk class, operations, and expected effect. |
| GC-06 | Mutation application is deterministic: same parent bytes + same mutation value ⇒ same candidate hash. |
| GC-07 | Evolvable SCI code runs without ambient JVM, filesystem, process, network, secret, or database authority. |
| GC-08 | All external effects cross the kernel-owned Intent/Capability Broker. |
| GC-09 | Adding a visible action/tool must not itself grant resource authority. |
| GC-10 | Episodic memory writes remain distinct from procedural Genome changes. |
| GC-11 | Candidate evaluation uses an informationally isolated selection set unavailable to Executor, Diagnostician, and Mutator. |
| GC-12 | A candidate must not modify the evaluator that judges that same candidate. |
| GC-13 | Parent and candidate are evaluated as a paired comparison on the same case set and environment fixture. |
| GC-14 | Hard safety/integrity/policy constraints dominate utility/cost metrics and are never collapsed into a compensating weighted score. |
| GC-15 | Promotion is an atomic compare-and-set against the parent generation/current pointer. |
| GC-16 | Rejected mutations remain durable, queryable negative evidence. |
| GC-17 | Every promoted generation retains complete lineage: parent, mutation, evidence, evaluation, decision, deployment state. |
| GC-18 | Rollback restores future generation selection only; it never claims to reverse already-committed external effects. |
| GC-19 | Kernel source, authority root, audit root, evaluator-isolation root, and promotion root are not agent-mutable. |
| GC-20 | Every externally visible effect is attributable to session-id, phenotype-id, node-id, intent-id, authorization decision, and outcome. |
| GC-21 | Large immutable payloads are stored by content hash; SQLite rows hold references, not duplicated bodies. |
| GC-22 | Public module boundaries exchange validated Clojure data only — no raw Java objects, lazy seqs, futures, or open resources across Genome/SCI/Intent/Event boundaries. |
| GC-23 | Candidate evaluation workspaces, SCI contexts, session namespaces, and mutable temp state stay isolated from the current production generation. |
| GC-24 | YAGNI for v1: no model-weight training, arbitrary JVM eval, arbitrary native codegen, persistent schema self-migration, automatic capability enlargement, or simultaneous evaluator/candidate co-evolution. |

---

## Part 2 — Repair Invariants (INV-01 – INV-09)

Each entry states: the invariant, the **Motivation** (real incident with
`file:line` evidence), an example **Violation consequence**, and the
**Enforcement** (which test, script, or namespace mechanically checks it,
plus the repair work item that owns closing any remaining gap).

### INV-01 — Diagnostic representation ≠ pool identity / execution input

A value used for human-facing display/redaction must never simultaneously
serve as a connection-pool identity key or as the configuration handed to
an executing component. Each role gets its own derivation.

- **Motivation.** `evoclj.mcp.manager`'s transport normalizer (the
  pre-M2 `normalize-transport`, renamed `redact-transport` at
  `src/evoclj/mcp/manager.clj:39`) replaces `:env`/`:headers` values
  with the literal `"[REDACTED]"`. That single redacting function was
  then reused for three incompatible roles: (a) it produced the config
  actually passed to `mcp-client/open!`
  (`src/evoclj/provider/mcp_bridge.clj:346,347`;
  `src/evoclj/mcp/source.clj:110,188,190`) — i.e. the *execution input*;
  (b) `transport-identity`/`connection-key`
  (`src/evoclj/mcp/manager.clj:93,112`) derived the *pool identity*
  from the redacted form, so two transports differing only in
  env/header content collapsed onto one connection key; (c) the same
  output was embedded into error payloads
  (`src/evoclj/provider/mcp_bridge.clj:395,402`;
  `src/evoclj/mcp/source.clj:245,252`) as the *diagnostic
  representation*.
- **Violation consequence.** Two MCP servers whose configs differ only in
  credentials share one pooled client (wrong server silently reused), or a
  connection is opened against redacted placeholder values instead of the
  real environment; error reports meanwhile look correct, so nothing in
  the logs explains the misrouting.
- **Enforcement.** Landed by WO-M2 (split display-redact vs
  identity-fingerprint; `open!` receives the real config): targeted
  tests in `evoclj.mcp.manager-identity-test` /
  `evoclj.provider.mcp-bridge-test` assert that configs differing only
  in `:env`/`:headers` yield *different* connection keys while
  diagnostic payloads stay redacted, and a grep audit shows no
  production call site feeds `redact-transport` output into `open!`.
  NOTE the M2 key-format change: `connection-key` remains
  `[type cid ti cf]`, but `ti` now carries per-field stable sha256
  fingerprints of the secret fields (not whole-value "[REDACTED]"
  placeholders) and `cf` is a stable sha256 digest of `:auth/ref` —
  safe without migration because the pool is purely in-memory state.
  Adversarial reviewers attack this tuple with at least one
  differ-only-in-secret counterexample (PROTOCOL-B step 4).

### INV-02 — Existence checks must throw on failure

When code verifies that a referenced entity exists, a negative result
must raise a typed error. An existence check that computes a verdict and
then discards it is a no-op masquerading as validation.

- **Motivation.** `validate-bundle-exists`
  (`src/evoclj/store/binding.clj:221-248`) looks the bundle up in the
  registry (lines 232–242) and in CAS (lines 243–247) but every
  not-found branch evaluates to `nil`, and the function ends by returning
  the input bundle unchanged (line 248). A binding can therefore be
  published/restored referencing a bundle that exists nowhere.
- **Violation consequence.** A restore path accepts a binding whose
  revision was garbage-collected; activation later fails far from the
  cause with an opaque missing-artifact error instead of a typed
  validation failure at the boundary.
- **Enforcement.** To be closed by WO-B2 (substantive throw +
  static `requiring-resolve` resolution). Mechanical check: focused tests
  in `evoclj.store.binding-test` must prove that activating/restoring a
  binding with a nonexistent bundle id throws a typed error
  (`:store/binding-invalid` family) on every path (registry-only, cas-only,
  neither). General rule for reviewers: any helper named `validate-*` or
  `check-*` that can return normally after a failed lookup is a finding.

### INV-03 — Limits are enforced before reads

Size/count/depth limits must be applied *before* bulk data is read,
materialized, or written into stores — never after the fact on data that
is already fully loaded.

- **Motivation.** `snapshot-tree!` (`src/evoclj/fs/snapshot.clj:215-248`)
  originally read every file byte-for-byte and wrote each one into CAS
  (the capture loop, lines 235–238) *before* `check-limits!` ran, so an
  over-limit tree paid full I/O and permanently polluted the CAS store
  before failing with `:fs/snapshot-limit-exceeded`. WO-S4 moved the limit
  check to a read-only PREFLIGHT: `check-limits!` now runs at line 234
  (limit predicates at lines 70–95) against attribute metadata gathered
  without reading content by `preflight-entries!` (lines 97–130).
- **Violation consequence.** A hostile or accidentally huge skill
  directory exhausts disk/CAS quota even though the snapshot "fails";
  repeated attempts amplify the waste. The same late-check pattern, if
  copied onto streaming readers, turns configured limits into
  post-mortem diagnostics instead of guardrails.
- **Enforcement.** GUARDED (landed by WO-S4, e2e #9): `evoclj.fs.snapshot-test`
  drives the production `snapshot-tree!` and asserts an over-limit tree
  is rejected fail-closed with `:fs/snapshot-limit-exceeded` carrying its
  `:limit`/`:actual`, with ZERO new CAS artifacts (reject before read)
  for each of `:max-files`/`:max-depth`/`:max-file-bytes`/
  `:max-total-bytes`, plus an exactly-at-boundary admission test and a
  concurrent capture-independence check. Reviewers reject any
  implementation that counts entries only after a full walk/read.

### INV-04 — Materialization fails closed

Reading immutable content back (from CAS or any pinned store) must either
return the exact pinned content or throw. Silent substitution of cached,
default, or "convenient" content is forbidden.

- **Motivation.** The skill materializer
  (`src/evoclj/skill/surface.clj:60-88`) catches *every* exception while
  loading SKILL.md from CAS and falls back to the snapshot-time
  `body-cache` (lines 74–87, comment at line 85 admits the fallback
  exists for test convenience); a nil CAS likewise returns the cache
  (line 87). Corrupt or missing trees are thus invisible: the reader sees
  stale-but-plausible content.
- **Violation consequence.** A corrupted CAS tree silently serves an old
  skill body; evolution then evaluates behavior that no longer matches
  the pinned revision, and audits cannot detect the divergence because no
  error was ever raised.
- **Enforcement.** To be closed by WO-S1/S2 (descriptor `:cas-tree-file`,
  assembler placeholder segments fail-closed, `cas-fn` banned — see
  INV-09). Mechanical check: surface/materializer tests must show that
  unreadable CAS content throws a typed error rather than returning the
  cache; adversarial mutation removes the tree from CAS and expects the
  materialize call to fail loudly.

### INV-05 — Single implementation principle

One mechanism, one implementation. Reaching into another module's
internals via reflective resolution (`ns-resolve`/`requiring-resolve`),
or maintaining parallel hand-copied variants of the same dispatch, is a
drift defect waiting to happen.

- **Motivation.** (a) `evoclj.mcp.source` duplicates bridge logic by
  reflectively dereferencing `json-schema->malli` and `result->edn` out
  of `evoclj.provider.mcp-bridge` (`src/evoclj/mcp/source.clj:55,194`)
  instead of importing a shared codec; (b)
  `evoclj.context.materializer/fetch-via-cas`
  (`src/evoclj/context/materializer.clj:38-75`) dispatches on six
  different shapes of the `cas` argument (fn, `:root` map, plain maps,
  string/File/Path), two of them via `requiring-resolve`
  (lines 48, 68); (c) `evoclj.store.binding` weaves a web of
  `requiring-resolve` calls into environment/context/mount internals
  (`src/evoclj/store/binding.clj:234,292,300-329`). Precedent for the
  cost: the BT1 baseline regression — `build-diagnostician` had a
  protocol pass-through branch (`satisfies?` at
  `src/evoclj/kernel/system.clj:375` in the fixed tree) while the
  parallel builder `build-mutator` did not (see
  `docs/codebase/BASELINE-TRIAGE.md`, BT1), so every record-valued
  mutator config threw `:evolution/system-invalid` and ~160 tests went
  red; the fix (`src/evoclj/kernel/system.clj:394-401`) had to re-establish
  the ordering discipline the twin builder already had.
- **Violation consequence.** Fixing a bug in one copy leaves the other
  broken (BT1); reflective reach-ins bypass compile-time dependency
  checks, so renaming or reloading a namespace breaks production only at
  runtime, invisibly to static analysis.
- **Enforcement.** To be closed by WO-M11 (single `mcp.codec` implementation,
  delete `ns-resolve`, fail-closed source validation) and WO-B2 (static
  resolution proven acyclic). Mechanical check: a repo grep must find no
  `ns-resolve`/`requiring-resolve` outside an explicit allowlist of
  namespaces, and M11 acceptance tests must pin that MCP transport/result
  conversion has exactly one implementation used by bridge, source, and
  CLI paths alike.

### INV-06 — `snapshot!` is side-effect free

A LiveSource `snapshot!` captures state and returns a value. It must not
publish bundles, mutate registries, advance counters, or otherwise change
the world; publication belongs to the downstream
Source→Revision→Projector→Bundle transaction.

- **Motivation.** `SkillSource`'s `snapshot!`
  (`src/evoclj/skill/adapter.clj:166-196`) calls `derive-and-publish!`
  inside the capture loop (line 176), publishing SurfaceBundles into the
  registry as a side effect of taking a snapshot, and stamps wall-clock
  time (`System/currentTimeMillis`, line 195) into the returned value —
  capture and publication are fused.
- **Violation consequence.** Merely *observing* the environment (e.g. a
  health check or an eval runner capturing a snapshot mid-comparison)
  mutates the registry and can reorder publishes; retries duplicate
  publications, and GC-13's paired comparison can be perturbed by the
  capture itself.
- **Enforcement.** To be closed by WO-E2 (single-transaction pipeline,
  `snapshot!` purification; e2e #11). Mechanical check: E2 acceptance
  tests assert that calling `snapshot!` twice yields equal payloads and
  zero registry mutations between calls; the existing payload-determinism
  pattern in `test/evoclj/mcp/source_test.clj:28-45` is the template.
  Reviewers reject any `snapshot!` whose return depends on mutable state
  beyond the captured source.

### INV-07 — Authorization tuple: Subject × Tool × Resource × ResourceAction

Every authorization decision is made over the full four-part tuple. An
action dimension that is collapsed, defaulted, or implied — rather than
explicitly granted — violates GC-09/GC-14 discipline at the granularity
level.

- **Motivation.** The current policy layer maps *every* v0 intent type to
  the single hard-coded action `:invoke`
  (`src/evoclj/capability/policy.clj:18-24`), so resource-level actions
  (read vs write vs delete on the same resource) cannot be distinguished
  by grants; the lease carries `:actions` (`src/evoclj/capability/lease.clj:165`)
  but nothing below the tool level ever varies it. WO-M14 records the
  corresponding broker gap (dual authorization to generalize into a
  resource-kind registry).
- **Violation consequence.** Granting a subject permission to invoke a
  tool implicitly authorizes every effect class that tool can produce
  (e.g. a memory tool's read and destructive write), making least-
  privilege grants unexpressible and audit reasoning coarser than the
  event log suggests (undermines GC-20 attribution quality).
- **Enforcement.** Closed by WO-M14 (resource action enters policy;
  broker dual authorization generalized into a resource-kind registry).
  Mechanical check: `evoclj.capability.broker-resource-action-registry-test`
  (new, M14) drives `evoclj.capability.broker/authorize` through the
  production path and asserts the six required behaviors: the
  ResourceAction is a first-class tuple component (a `:filesystem`
  request carrying `:action :read` is allowed only when the lease grants
  `:read`, and `:read` vs `:write` are distinct); the broker dispatches
  uniformly per registered kind via `default-resource-kind-registry`
  (`src/evoclj/capability/broker.clj:95`), so the former hard-coded
  `:filesystem/path` dual branch is gone (a custom registry with a
  request-only target authorizes without a tool grant, while the
  default still requires it); an unregistered resource kind is denied
  fail-closed with `:capability/unknown-resource-kind`; an action absent
  from the lease's `:actions` is denied with `:capability/action-denied`;
  and `evoclj.capability.policy-property-test` / `evoclj.capability.broker-test`
  remain green (deny on any unmatched component, monotone in the lease
  set). No default action synthesis: a `:request` target uses the
  resource's own `:action` (or the intent action only as explicit
  fallback in `resolve-target-action`,
  `src/evoclj/capability/broker.clj:112`), never an implicit `:invoke`.

### INV-08 — Documentation hashes must resolve

Every abbreviated/full commit SHA cited in repository documentation must
resolve to a real commit. Documentation that cites unverifiable history
is unauditable.

- **Motivation.** Before T5 there was no mechanical check, and docs had
  accumulated stale references: the first authoritative scan of the real
  `docs/` tree produced an invalid-reference inventory (recorded in the
  repair ledger's T5 entry as D1 input — eight occurrences at that time).
  The concrete incident (now closed by WO-D1): the MCP gap-closure report's
  "Commits" section
  (`docs/superpowers/specs/2026-08-20-mcp-gap-closure-report.md`) cited six
  abbreviated SHAs — previously at `:53-58` — that no longer resolve to any
  commit in this repository; the history they described was rewritten away,
  and nothing could detect that until the scan existed. WO-D1 has since
  marked that report SUPERSEDED with a resolvable pointer and removed the
  unresolvable SHAs, so it can no longer be mistaken for the current closure
  truth.
  `scripts/verify-doc-hashes.clj` now scans every markdown file and
  verifies each candidate with `git rev-parse --verify <sha>^{commit}`.
- **Violation consequence.** A reviewer tracing "which commit introduced
  this guarantee" hits a dead end; worse, a plausible-looking fabricated
  hash lends false authority to a claim and survives review because
  nothing checks it.
- **Enforcement.** Active guard (already landed, T5): run
  `clojure -M scripts/verify-doc-hashes.clj` — exit 0 required for any
  docs change; the script contract (lexical rules E1–E4, exit codes,
  WARN-visible exemptions) is pinned by
  `test/evoclj/support/doc-hashes-test.clj`. WO-D1 closed the closure-report
  share of the existing backlog (6 of 8) and made the report hash-traceable
  via a resolvable SUPERSEDED pointer; the 2 remaining invalid refs are
  ledger agent-instance-ids (non-commit refs, tracked as D1-followup). New
  documentation must not increase the invalid-reference count; pure
  `file:line` citations are unaffected by this rule.

### INV-09 — Tests must traverse production paths

Tests exercise the real production components end to end. Test-only
injection hooks inside production code, shape assertions impersonating
behavior tests, and test suites that replicate logic the production path
lacks are all banned.

- **Motivation.** (a) `discover-tools`
  (`src/evoclj/mcp/source.clj:91-105`) branches on a `:discover-fn` stub
  carried by the production Source record (fields at lines 268,
  309-328) so most tests never construct a real client path; (b) the
  context materializer keeps an fn-shaped CAS resolver branch "for test
  convenience" (`src/evoclj/context/materializer.clj:41-44`), which
  `test/evoclj/skill/adapter_test.clj:299` exploits with a `:cas cas-fn`;
  (c) the T4 concurrency kit's first round was rejected because its
  `raced` gate asserted nothing behavioral (repair ledger, T4 entry);
  (d) BT2's snakeyaml loss — upgrading `com.networknt/json-schema-validator`
  dropped `org.yaml/snakeyaml` as it was only a transitive dependency,
  crashing full loads (`deps.edn:13-14` now pins both directly) — was
  caught *only* by the full-suite gate, because targeted per-namespace
  tests never load the SDK validator stack: narrow green does not imply
  the production path works (repair ledger, BT2 entry).
- **Violation consequence.** Suites stay green while the shipped path is
  broken (a–c), or a defect escapes every targeted suite and only a
  whole-system run stumbles onto it (d) — late, expensive, and hard to
  bisect.
- **Enforcement.** Standing protocol guard, applied to every work item:
  PROTOCOL-B steps 2–3 — mutation check (new tests must fail on the
  pre-fix baseline) and bypass scan (grep for injected fns replacing WO-
  designated production components, shape-only assertions, replicated
  logic), plus at least three executed counterexamples (step 4). Work
  items that remove a hook (S1 bans `cas-fn`; M20 wires McpSource into
  production) close the corresponding instance. Regression floor: the
  full suite remains the final gate for every batch (baseline-failure set
  must shrink monotonically).

---

## Part 3 — Maintenance rules

1. **No invariant without a guard.** Adding INV-N (or amending an entry)
   requires, in the same change, a machine-checkable enforcement path: a
   named test namespace/test file, a script command, or a pinned
   protocol step — written into the Enforcement field before merge.
   Entries whose guard is still pending must name the owning work item
   and are converted to a landed test path when that item closes.
2. **Evidence stays fresh.** When a code change moves or eliminates a
   `file:line` anchor cited here, the same change updates the citation.
   Deleted incidents are condensed, not erased — the motivation must
   always point at something verifiable.
3. **Reviewers adjudicate against this checklist.** Per PROTOCOL-B step
   5, every adversarial review marks each applicable invariant
   ok/violation/n-a with a reason; a REJECT finding may cite an invariant
   number directly.
4. **Precedence.** GC-01–GC-24 remain normatively defined in
   `docs/implementation-plan.md`; the one-line summaries above are
   navigational. INV-01–INV-09 extend (never override) the Global
   Constraints. Discrepancies are bugs to report, matching the
   maintenance notes in `docs/README.md`.
5. **Index sync.** Any rename or addition of a top-level document must be
   reflected in the `docs/README.md` document map in the same change.
