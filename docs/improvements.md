# EvoCLJ Improvements Plan

**Status: COMPLETE (2026-08-16)** — all 26 tasks landed as individual commits (see git log from 058baf0); full suite green (990 tests / 8378 assertions).

Source: brainstorm session (T1 闭环验证 / T2 信任审计 / T3 操作体验 / T4 研究纵深)
plus the unticked items of `docs/roadmap.md` (E1–E5, V2, V5, R4, O4, O5, S3).
Relation: builds on `docs/foundations.md` — the F1–F7 foundations exist as tested
leaf modules; Phase A gives each foundation its first real consumer.

## TDD/Commit Discipline

- Every task is one commit. Commit messages: `feat: <imperative>`, `fix: ...`,
  `test: ...`, `chore: ...`, `docs: ...` (see git log for style).
- Strict TDD per task: failing test first, focused run (`clojure -M:test -n <ns-regex>`),
  implement, focused green, then FULL suite (`clojure -M:test`) green.
- Touch ONLY the files listed in the task. Do not modify `deps.edn` unless listed.
- Windows host, Git Bash; LF line endings only (`.gitattributes` enforces).
- The 24 Global Constraints in `docs/implementation-plan.md` bind all tasks.
- Docs are the contract: code that contradicts docs is a bug — fix the code.
- The reviewer/committer is a separate agent; implementers never commit.

---

## Phase A — Foundation adoption (wire F1–F7 to their first consumers)

### component — Redaction on the event write path (F7)

- **Purpose**: `evoclj.store.event/append-event!` accepts optional redaction specs
  and applies `evoclj.security.redact/redact-event` to the event payload BEFORE
  hashing/append. No specs → behavior unchanged (backward compatible).
- **Files**: `src/evoclj/store/event.clj`, `src/evoclj/security/redact.clj` (read-only),
  `test/evoclj/store/event_test.clj`.
- **Steps**: read `redact.clj` (validate-specs!, redact-event) and `append-event!`;
  add optional `:redaction-specs` argument (or config map entry) to the append path;
  redaction must be idempotent and applied before the hash-chain link is computed.
- **Acceptance**: events appended with specs are redacted in payload but the
  hash chain stays valid (`verify-event-chain` passes); default path byte-identical
  to before; invalid specs throw `:security/redact-invalid`.
- **Commit**: `feat(store): redact events on the write path`

### component — Metric records during evaluation (F2)

- **Purpose**: `evoclj.eval.core/evaluate-candidate!` records Metric records
  (see `evoclj.metrics.core`: `{:metric/id :metric/name :metric/scope
  :metric/scope-id :metric/value :metric/unit :metric/at}`) for G0–G6 phase
  durations and gate outcomes, via `collect-metric!` (injectable atom collector).
- **Files**: `src/evoclj/eval/core.clj`, `src/evoclj/metrics/core.clj` (read-only),
  `test/evoclj/eval/core_test.clj`.
- **Steps**: collector is an optional argument (default nil = no-op); record at
  least: per-phase wall-ms, per-gate `:pass/:fail`, total eval ms.
- **Acceptance**: with a test collector, expected records appear (scope :candidate,
  scope-id = candidate id); without collector, zero overhead/no-op; existing tests
  unchanged.
- **Commit**: `feat(eval): record evaluation phase metrics`

### component — Parallel candidate batch evaluation (F4)

- **Purpose**: batch evaluation of N candidates through `evoclj.eval.workers/run-batch!`
  with `side-task-runner`; same isolation semantics as `run-side!` (temp stores),
  per-task timeout, per-task error isolation, structured batch results.
- **Files**: `src/evoclj/eval/runner.clj`, `src/evoclj/eval/core.clj`,
  `src/evoclj/eval/workers.clj` (read-only), `test/evoclj/eval/runner_test.clj`.
- **Steps**: add `evaluate-batch!` (candidates → batch results) using run-batch!;
  keep single-candidate path untouched; concurrency capped (default 4).
- **Acceptance**: batch of 3+ candidates with a mix of pass/fail isolates errors
  per candidate; timeout aborts only the stalled task; results structured
  (per-candidate status + eval refs).
- **Commit**: `feat(eval): parallel candidate batch evaluation`

### component — Validated config in CLI startup (F5)

- **Purpose**: `evoclj.cli.session/build-config` delegates to
  `evoclj.config/load-config` (validated merge over EDN/map, `resolve-profile`,
  `config-value`), preserving the existing `:overrides` deep-merge injection for
  tests/hosts; `EVOCLJ_*` env overrides honored; `validate-config!` failures
  surface as typed `:config/invalid`.
- **Files**: `src/evoclj/cli/session.clj`, `src/evoclj/config.clj` (read-only),
  `test/evoclj/cli/cli_test.clj`.
- **Acceptance**: CLI boots with valid config; env override wins over file;
  invalid config exits with `:config/invalid` (not a stack trace); host
  overrides (cases/fixtures/mutator injection) still work — existing tests pass.
- **Commit**: `feat(cli): wire validated config into session startup`

### component — Behavior profiles into the diagnostician (F1)

- **Purpose**: the diagnostician's input context gains a compact BehaviorProfile
  summary (`evoclj.analytics.behavior/profile-events` + `fingerprint`) computed
  from the candidate's evidence events, when available.
- **Files**: `src/evoclj/evolution/diagnose.clj` or `llm_diagnostician.clj`,
  `src/evoclj/analytics/behavior.clj` (read-only), `test/evoclj/evolution/diagnose_test.clj`.
- **Steps**: build context map; add `:context/behavior-profile` (summary EDN: intent
  counts, tool-call sequence, failures, wall-ms, fingerprint) only when evidence
  events exist; deterministic.
- **Acceptance**: with evidence, context contains the profile summary + stable
  `sha256:` fingerprint; without evidence, key absent; existing diagnose tests pass.
- **Commit**: `feat(evolution): feed behavior profiles to the diagnostician`

### component — Judge verdicts as enrichments (F3)

- **Purpose**: per-case judge verdicts persist as enrichment records
  (`evoclj.store.enrichment/put-enrichment!`; entity-kind `:evaluation`,
  entity-id = evaluation id, kind `:judge-verdict`, versioned, payload in CAS).
- **Files**: `src/evoclj/eval/judge.clj` or `src/evoclj/eval/core.clj`,
  `src/evoclj/store/enrichment.clj` (read-only), `test/evoclj/eval/judge_test.clj`.
- **Steps**: pure verdict→enrichment mapping + store adapter; enrichment failure
  must NOT fail the evaluation (isolated, recorded).
- **Acceptance**: verdict round-trips via `latest-enrichment`/`payload`; version
  counter increments per verdict batch; evaluation succeeds even if store write fails.
- **Commit**: `feat(eval): persist judge verdicts as enrichments`

### component — Regression-detection trigger rule (F6, alert only)

- **Purpose**: a data-driven rule (`evoclj.runtime.trigger`: `match-metric-rule`,
  `evaluate`, `register-action!`, `run-actions!`) that fires when a promoted
  child's paired utility drops below parent by a threshold within a window —
  action `:monitor/alert-regression` which ONLY appends an audit event; NO state
  mutation (auto-rollback is component).
- **Files**: `src/evoclj/runtime/trigger.clj` (read-only), new
  `src/evoclj/runtime/regression.clj`, `test/evoclj/runtime/regression_test.clj`.
- **Steps**: rule data (metric name, comparator, threshold, window); wire action
  via register-action!; action appends event through the store event API.
- **Acceptance**: synthetic metric series triggers exactly when threshold crossed
  within window; action audited in event log; rule data contains no fns.
- **Commit**: `feat(runtime): regression-detection trigger rule`

---

## Roadmap backlog — Evolution depth (E1–E5)

### component — Evidence/history retrieval tools for the mutator

- **Purpose** (roadmap E1): expose `:evolution/evidence` and `:evolution/history`
  as broker tools so the LLM mutator retrieves context via the tool-calling loop
  instead of prompt-rendered context.
- **Files**: `src/evoclj/evolution/llm_mutator.clj`, `src/evoclj/capability/*`
  (tool definitions/lease), `src/evoclj/runtime/nodes/tool.clj` (read-only),
  `test/evoclj/evolution/llm_mutator_test.clj`.
- **Steps**: two read-only tools: evidence (by candidate/evidence id → evidence
  pack fields) and history (rejection history window, default 50/max 500); both
  subject-bound through the broker; mutator's tool catalog includes them.
- **Acceptance**: tool call through broker returns scoped data; out-of-scope
  subject denied with the standard deny codes; tools read-only (no mutation).
- **Commit**: `feat(evolution): evidence/history broker tools for the mutator`

### component — Hypothesis ranking (roadmap E2)

- **Purpose**: diagnosis hypotheses carry `:confidence`; the kernel re-validates
  order (descending confidence) before adoption; ties broken deterministically;
  malformed confidence (non-numeric, outside [0,1]) → typed `:evolution/...` error.
- **Files**: `src/evoclj/evolution/diagnose.clj`, `src/evoclj/evolution/diagnosis_schema.clj`,
  `test/evoclj/evolution/diagnose_test.clj`.
- **Acceptance**: unordered input adopted in validated order; stable across runs;
  bad confidence rejected with typed error.
- **Commit**: `feat(evolution): rank and re-validate diagnosis hypotheses`

### component — Candidate diff report (roadmap E3)

- **Purpose**: CLI shows file-level diff of parent vs candidate.
- **Files**: `src/evoclj/cli/evolution.clj` (or `eval_inspect.clj`),
  `src/evoclj/genome/patch.clj` (read-only), `test/evoclj/cli/evolution_test.clj`.
- **Steps**: `candidate inspect --diff <candidate-id>` → per-file diff (line hunks
  via the existing patch machinery / rewrite-clj), EDN default + `--pretty` renderer.
- **Acceptance**: diff of two known genomes shows exactly the changed files/lines;
  unrelated files absent; empty diff for identical genomes.
- **Commit**: `feat(cli): candidate diff report`

### component — Mutation-budget adaptation (roadmap E4)

- **Purpose**: budget profile adapts from rejection history (per mutation-class
  success rate via F2 aggregation): allowance shrinks after consecutive
  rejections, grows after successes, with floor/ceiling caps.
- **Files**: `src/evoclj/evolution/budget.clj`, `test/evoclj/evolution/budget_test.clj`.
- **Steps**: pure `adapt-budget` (history → profile); deterministic; wired where
  the budget is checked in the propose loop; caps prevent zero/explosion.
- **Acceptance**: scripted histories produce the expected adapted profiles within
  caps; unchanged history → unchanged profile.
- **Commit**: `feat(evolution): adapt mutation budgets from rejection history`

### component — Evidence-pack usage enrichment (roadmap E5)

- **Purpose**: evidence-pack summaries include model usage/cost when present in
  the model-call channel (token counts, cost estimate).
- **Files**: `src/evoclj/evolution/evidence.clj`, `src/evoclj/evolution/evidence_schema.clj`,
  `test/evoclj/evolution/evidence_test.clj`.
- **Steps**: optional `:usage` fields in the schema; populated from the call
  channel when available; ABSENT when unknown (never zero — honest accounting).
- **Acceptance**: pack with usage data round-trips; pack without usage omits the
  key; schema rejects non-numeric usage.
- **Commit**: `feat(evolution): include model usage in evidence packs`

---

## Roadmap backlog — Eval, runtime, ops (V2, V5, R4, O4, S3)

### component — Judge score aggregation

- **Purpose** (roadmap V2): per-case judge verdicts feed a utility summary
  (win/loss/equiv counts, per-category breakdown) joined into the paired outcome.
- **Files**: `src/evoclj/eval/judge.clj`, `src/evoclj/eval/paired.clj` (read-only),
  `src/evoclj/eval/statistics.clj` (read-only), `test/evoclj/eval/judge_test.clj`.
- **Acceptance**: verdict list → summary record; counts correct; categories stable.
- **Commit**: `feat(eval): aggregate judge verdicts into utility summaries`

### component — Judge configuration

- **Purpose** (roadmap V5): config exposes `:judge {:temperature :system-prompt
  :max-tokens}` with defaults; validated; passed into the judge's model call.
- **Files**: `src/evoclj/config.clj`, `src/evoclj/eval/judge.clj`,
  `src/evoclj/provider/model_registry.clj` (read-only), `test/evoclj/eval/judge_test.clj`.
- **Acceptance**: defaults apply; overrides flow into the call; invalid values
  rejected by `validate-config!`; fixture judge path unaffected.
- **Commit**: `feat(eval): expose judge model configuration`

### component — Scheduler concurrency semantics + stress test

- **Purpose** (roadmap R4): document single-session scheduler semantics (what is
  serialized, what is concurrent) and add a stress test proving no corruption.
- **Files**: `docs/scheduler.md` (new), `test/evoclj/runtime/scheduler_stress_test.clj`,
  `src/evoclj/runtime/scheduler.clj` (read-only).
- **Steps**: read scheduler + episode; document semantics; stress: N sessions ×
  M events interleaved → hash chain valid, no cross-session leakage, no lost events.
- **Acceptance**: doc matches code; stress test green and deterministic.
- **Commit**: `docs(runtime): scheduler concurrency semantics + stress test`

### component — Full-cycle harness + performance baseline

- **Purpose** (roadmap O4): a harness that runs evolve → eval → promote on seed
  genomes with the configured model (fixture/mock fallback, no network needed),
  collecting F2 metrics and wall timings per phase; results written as structured
  EDN; `docs/performance-baseline.md` updated with measured numbers (or, if no
  real endpoint is reachable, mock timings + an explicit "no endpoint" note).
- **Files**: `scripts/full-cycle.clj` (new), `docs/performance-baseline.md`,
  `test/evoclj/perf/full_cycle_test.clj`.
- **Acceptance**: harness runs headless with fixture provider; output EDN contains
  per-phase timings; baseline doc updated honestly.
- **Commit**: `feat(ops): full-cycle timing harness + performance baseline`

### component — Lease refinement denial tests

- **Purpose** (roadmap S3): per-model and per-tool lease denial cases.
- **Files**: `test/evoclj/capability/lease_test.clj` (or broker_test), tests only.
- **Acceptance**: model A allowed / model B denied; tool X allowed / tool Y
  denied; window expiry; call-budget edge exactly at max; each case asserts the
  stable deny code.
- **Commit**: `test(capability): per-model/per-tool lease denial cases`

---

## Phase C — Trust deepening

### component — F6 action registry authority audit + ACL

- **Purpose** (T2b): `register-action!` requires an action descriptor
  (`:action/id`, `:action/allowlist`, `:action/subject-scope`); `run-actions!`
  enforces the ACL — unknown/unauthorized action ids become `:error` entries and
  are NEVER executed; every executed action appends an audit event to the store.
- **Files**: `src/evoclj/runtime/trigger.clj`, `test/evoclj/runtime/trigger_test.clj`.
- **Acceptance**: unauthorized action → error entry, no execution, audited;
  authorized action runs and is audited; descriptor missing → `:trigger/invalid`.
- **Commit**: `feat(runtime): action registry ACL and audit trail`

### component — Auto-rollback action

- **Purpose** (T1b): the `:promotion/auto-rollback` action invokes the promotion
  rollback API (CAS-safe) when the regression rule fires; guarded — rollback only
  after a minimum observation count (no knee-jerk on first data point).
- **Files**: `src/evoclj/runtime/regression.clj`, `src/evoclj/promotion/rollback.clj`
  (read-only), `src/evoclj/promotion/monitor.clj` (read-only),
  `test/evoclj/runtime/regression_test.clj`.
- **Acceptance**: synthetic series → rollback exactly at the guard threshold;
  first-data-point case does NOT roll back; rollback path via the public API.
- **Commit**: `feat(runtime): auto-rollback on confirmed regression`

### component — Failure-driven case evolution

- **Purpose** (T1b): a confirmed regression auto-creates a new evaluation case
  from the failing input into the hidden dataset (append-only, provenance-linked
  to the triggering evidence).
- **Files**: `src/evoclj/eval/dataset.clj` (read-only), `src/evoclj/runtime/regression.clj`,
  `src/evoclj/store/enrichment.clj` (read-only), `test/evoclj/runtime/regression_test.clj`.
- **Acceptance**: regression → case appended with cause ref to the evidence pack;
  duplicate regression does not duplicate the case; dataset stays append-only.
- **Commit**: `feat(runtime): failure-driven evaluation case evolution`

### component — Seed trust anchors

- **Purpose** (T2c): a trust-anchor file (map of seed genome id → expected
  `sha256:`) ships in `resources/` (and is overridable via config); `load-genome`
  verifies anchored genomes against it; mismatch → typed error, load refused.
- **Files**: `resources/trust-anchors.edn` (new), `src/evoclj/genome/load.clj`,
  `src/evoclj/config.clj` (read-only), `test/evoclj/genome/load_test.clj`.
- **Acceptance**: pristine seed loads; tampered seed (any byte) refuses with the
  typed error; unanchored genomes unaffected (backward compatible).
- **Commit**: `feat(security): verify seed genomes against trust anchors`

---

## Phase D — Operations

### component — Demo profile with built-in mutator

- **Purpose** (T3a): a `:demo` config profile injects a built-in heuristic
  mutator (non-LLM: template/function-swap mutations over the seed genome) via the
  existing host injection path; `docs/quickstart.md` documents the 30-minute demo
  (fresh state dir → evolved candidate → promote).
- **Files**: `src/evoclj/config.clj` (demo profile), new
  `src/evoclj/evolution/demo_mutator.clj`, `docs/quickstart.md`,
  `test/evoclj/evolution/demo_mutator_test.clj`.
- **Acceptance**: fresh state dir + demo profile → propose/eval/promote run headless
  to a promoted candidate; demo mutator produces valid genomes (compiler topology
  passes); quickstart steps reproducible.
- **Commit**: `feat(cli): demo profile with built-in mutator`

### component — Lineage CLI with candidate diffs + provenance (roadmap O5)

- **Purpose**: `lineage` shows per-generation: genome ids, file-level diff
  summary vs parent (stats + changed files), promotion reason, and evidence
  provenance refs (evidence pack + CAS refs). EDN default + `--pretty`.
- **Files**: `src/evoclj/cli/promotion.clj`, `src/evoclj/promotion/lineage.clj`
  (read-only), `test/evoclj/cli/promotion_test.clj`.
- **Acceptance**: known lineage renders correct per-generation entries; diff
  stats accurate; provenance refs resolve to stored evidence.
- **Commit**: `feat(cli): lineage with candidate diffs and provenance`

---

## Phase E — Research depth (commit-sized)

### component — Property-based core invariant suites

- **Purpose** (T4d): test.check suites over core invariants.
- **Files**: `deps.edn` (`:test` extra-deps gains `org.clojure/test.check`),
  `test/evoclj/genome/hash_property_test.clj`,
  `test/evoclj/capability/policy_property_test.clj`,
  `test/evoclj/store/cas_property_test.clj`.
- **Properties**: hash determinism, entry-order independence, single-byte-change
  sensitivity; `policy/decide` first-allow-wins + deterministic order; CAS
  single-winner under concurrent interleavings (mirroring verify1's model).
- **Acceptance**: all properties green over a meaningful sample count; existing
  suite unaffected.
- **Commit**: `test: add property-based core invariant suites`

### component — Dual-parent crossover (host opt-in)

- **Purpose** (T4a): a `crossover` mutation op producing a valid child genome
  from two parents via topology-aware recombination (split the topology at a node,
  take the node's subtree from the other parent, re-resolve dependencies);
  output must satisfy compiler topology validity; pure; NOT in the default
  mutation distribution (host opt-in).
- **Files**: `src/evoclj/evolution/mutation.clj`, `src/evoclj/genome/patch*.clj`
  (read-only), `src/evoclj/compiler/topology.clj` (read-only),
  `test/evoclj/evolution/mutation_test.clj`.
- **Acceptance**: valid crossovers produce topology-valid children; invalid
  parent combinations rejected with typed error; determinism for identical inputs.
- **Commit**: `feat(evolution): dual-parent crossover mutation`

### component — Judge calibration harness

- **Purpose** (T4c): a calibration fixture of known-equivalent/known-different
  pairs + a harness that runs the judge (fixture judge in CI) and reports
  agreement statistics (pure `agreement-stats` fn); doc note in eval docs.
- **Files**: `test/fixtures/evals/calibration.edn` (new), `src/evoclj/eval/judge.clj`,
  `test/evoclj/eval/judge_calibration_test.clj`.
- **Acceptance**: harness reports exact agreement on the fixture; stats fn pure
  and tested; fixture pairs cover equiv/non-equiv/shared-edge.
- **Commit**: `feat(eval): judge calibration harness`

---

## Future Work (deferred — needs design and/or live-model data)

- **T4b population/Pareto/speciation**: single-lineage CURRENT model → population
  - Pareto archive + novelty search. Model-level change; needs its own design doc.
- **T1c meta-evolution**: mutate the mutator/judge prompts, case weights, and
  promotion policy as evolvable objects. Needs Phase B data and its own design.
- **B1 real-model marathon timings**: full loop against a live LLM endpoint
  (LM Studio etc.) for realistic `performance-baseline.md` numbers. Blocked on
  endpoint availability; the component harness is the vehicle for it.
