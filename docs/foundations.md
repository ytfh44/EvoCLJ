# EvoCLJ Common Foundations

**Status: FOUNDATION LAYING IN PROGRESS.** This document records the
step-by-step analysis that derives the shared substrates ("common
foundations") beneath the brainstormed feature backlog, and the module
design that establishes each foundation in the codebase. Each
foundation lands as its own tested module and its own commit.

## 1. Why this document

Two brainstorm rounds produced ~40 feature ideas across evolution,
evaluation, runtime, ops, and security. Before implementing any of
them, the shared infrastructure they all lean on must exist — a
foundation that only one feature needs is a feature, not a foundation.
This document is the audit trail for the derivation:

1. inventory the features;
2. extract each feature's enabling capability;
3. cluster the capabilities into shared foundations;
4. map each foundation onto what the codebase already has;
5. design each foundation as a module (contract, tests, commit).

## 2. Feature inventory (both brainstorm rounds)

### Round 1
Hall-of-fame regression; crossover (dual-parent recombination);
mutation-budget bandit; failure-driven case evolution; plateau
detection; Pareto promotion; confidence-interval comparison; judge
drift detection; case weighting; species-level shared memory;
long-horizon plan node; session suspend/resume; dual-phenotype
negotiation; dashboard export; model routing; cost hard budgets;
CI-style pipeline; deterministic replay debugger; policy dual-control;
patch lint gate; seed supply-chain trust; SCI DoS stress suite.

### Round 2
Speciation (niche protection); novelty search; behavioral fingerprint
as first-class data; mutation lab notebook; self-documenting genomes;
curriculum evaluation (difficulty + early exit); dual-judge
disagreement escalation; judge calibration; worst-case resource
profiling; intent-level token bucket; memory TTL/compaction;
deterministic tool-result caching; failure taxonomy; evolution weather
report; pipeline YAML; distributed eval workers; HTTP API shell; event
log redaction; behavioral anomaly detection; canary auto-rollback; SCI
static pre-screening.

## 3. Capability extraction and clustering

Per-feature enabling capability (what infrastructure the feature
cannot exist without):

| Feature | Enabling capability |
| --- | --- |
| Speciation, novelty search, anomaly detection | structured behavior profile derived from event logs + stable fingerprint |
| Curriculum difficulty, worst-case profiling, weather report, lab notebook, replay debugger, judge drift, failure taxonomy | event-sequence reducers: per-case/per-session aggregations, failure classification, resource stats |
| Confidence-interval comparison | bootstrap confidence intervals over observed samples |
| Plateau detection | trend test over a value series |
| Mutation-budget bandit | per-class success-rate statistics |
| Judge calibration, Pareto promotion, cost aggregation | unified metric records (name/scope/value/unit/at) + aggregation |
| Case weighting, curriculum metadata, self-documenting genomes, lab notebook storage, hall-of-fame flags, failure-driven case evolution, memory TTL | versioned derived metadata attached to immutable entities (genome/case/candidate/generation) |
| Distributed eval, curriculum early exit, worst-case isolation | concurrent isolated evaluation with per-task budget, timeout, early-exit, structured results |
| Model routing, cost budgets, judge config (V5), pipeline YAML, retention policy, policy dual-control | validated declarative configuration with profiles + gated policy-proposal records |
| Anomaly alarms, auto-rollback, budget stop, drift re-eval, plateau-triggered exploration, notifications | data-driven rules evaluated over the append-only event stream, dispatching kernel-side actions |
| Event redaction, seed trust, patch lint, SCI pre-screen | write-path hygiene: redaction specs, static patch lint, trust anchors |

Clustering yields **seven foundations**:

| # | Foundation | Module | Serves |
| --- | --- | --- | --- |
| F1 | Behavior profile layer | `evoclj.analytics.behavior` | fingerprint/speciation/novelty, anomaly detection, curriculum difficulty, worst-case profiles, failure taxonomy, lab notebook, weather report, replay, judge drift |
| F2 | Metrics & inference core | `evoclj.metrics.core`, `evoclj.metrics.inference` | CI comparison, plateau detection, bandit budgets, judge calibration, Pareto, cost aggregation |
| F3 | Enrichment store | `evoclj.store.enrichment` (+ migration 004) | case weighting, curriculum metadata, self-doc, lab notebook persistence, hall-of-fame, failure-driven cases, TTL metadata |
| F4 | Eval worker pool | `evoclj.eval.workers` | distributed eval, curriculum early exit, worst-case isolation |
| F5 | Config & policy surface | `evoclj.config` | model routing, budgets, judge config, pipelines, retention, dual-control proposals |
| F6 | Event triggers | `evoclj.runtime.trigger` | anomaly alarms, auto-rollback, budget stop, drift re-eval, plateau exploration |
| F7 | Trust & hygiene | `evoclj.security.redact`, `evoclj.security.patch-lint` | event redaction, patch lint, (later) seed trust anchors, SCI pre-screen |

## 4. Mapping onto existing code

What the codebase already provides, per foundation:

- **F1**: append-only hash-chained event log (`evoclj.store.event`) and
  the public Event contract (`evoclj.store.event-schema`) — the raw
  substrate. MISSING: any reducer that turns event sequences into
  structured behavior profiles or fingerprints.
- **F2**: `evoclj.eval.statistics` is DESCRIPTIVE ONLY by contract
  (mean/median/wins/losses, explicitly no p-values/CI). MISSING: a
  metric-record vocabulary shared across eval/evolution/runtime, and
  inferential primitives (bootstrap CI, trend test) in a separate
  namespace that states its own assumptions.
- **F3**: content-addressed CAS + SQLite with append-only discipline
  and migrations (001-init, 002-memory, 003-routing). Entities
  (genomes, candidates, cases) are immutable by design. MISSING: a
  generic, versioned, append-only annotation layer keyed by
  entity-kind + entity-id whose payloads live in the CAS.
- **F4**: `evoclj.eval.runner/run-side!` already evaluates one side in
  fully isolated throwaway stores (Global Constraints 11/12/23).
  MISSING: concurrency, per-task timeout, early exit, structured
  batch results.
- **F5**: config is currently scattered across CLI flags and evaluator
  maps. MISSING: one validated config contract with defaults, profile
  merge, and a gated policy-proposal record type.
- **F6**: events are append-only with a hash chain; `recovery` and
  `events` CLIs read them. MISSING: a data-driven rule evaluator over
  event/metric data plus a kernel-side action registry.
- **F7**: mutation ops are structured (`:file`, `:op`, validated by
  `evoclj.evolution.mutation-schema`); event append validates EDN-safe
  metadata but no redaction layer exists. MISSING: write-path
  redaction specs and a static patch lint.

## 5. Foundation designs (module contracts)

Each foundation is a small, single-purpose module with a typed error
contract, Malli schemas, EDN-safe data (Global Constraint 22), and a
test namespace under `test/evoclj/<area>/`. Foundations are
composable: F6 rules consume F2 metric records; F4 wraps the existing
run-side! harness; F3 stores any derived artifact (F1 profiles, F2
metrics, generated docs).

### F1 — `evoclj.analytics.behavior` (behavior profile + fingerprint)

- `profile-events` — events → BehaviorProfile (intent counts by type,
  tool-call sequence, failure records, status, wall-ms, resource
  counters).
- `fingerprint` — profile → `sha256:<64 hex>` (deterministic, via
  `evoclj.genome.hash`); the identity key for speciation/novelty.
- `summarize-failures` — events → canonical failure taxonomy records.
- `tool-usage-stats` — per-tool call counts (anomaly-detection
  substrate).
- Error: `:analytics/events-invalid`.

### F2 — `evoclj.metrics.core` + `evoclj.metrics.inference`

- `evoclj.metrics.core`: Metric record
  `{:metric/id :metric/name :metric/scope :metric/scope-id
  :metric/value :metric/unit :metric/at}`; `record-metric` (pure),
  `collect-metric!` (atom collector, kernel-side), `aggregate`,
  `quantiles`.
- `evoclj.metrics.inference`: `bootstrap-ci` (deterministic-seeded
  resampling CI, documented as a sample estimate, never a probability
  claim); `trend-test` (linear slope + monotonic streak — plateau
  detection substrate).
- Errors: `:metrics/invalid`, `:metrics/inference-invalid`.

### F3 — `evoclj.store.enrichment` (+ migration 004)

- Migration 004: append-only `enrichments` table
  (entity_kind/entity_id/kind/version, payload_ref → CAS,
  cause_ref, created_at) with no-update/no-delete triggers mirroring
  the events table; bump `evoclj.store.migrate/latest-version` to 3.
- `put-enrichment!` — payload → CAS, row references it (Constraint
  21); version counter per (entity-kind, entity-id, kind).
- `enrichments` / `latest-enrichment` / `payload`.
- Error: `:enrichment/store-invalid`, `:enrichment/invalid`.

### F4 — `evoclj.eval.workers`

- `run-batch!` — generic over a task-runner fn: bounded concurrency,
  per-task timeout, early-exit predicate, per-task error isolation,
  structured batch result + stats.
- `side-task-runner` — adapter from the G5 evaluator to run-side!.
- Error: `:eval/workers-invalid`.

### F5 — `evoclj.config`

- `default-config`, `load-config` (validated merge over EDN/map),
  `resolve-profile`, `config-value`.
- Gated policy surface: `PolicyProposalSchema`
  (`:proposed → :approved | :rejected`), `propose-policy`,
  `transition-policy!` (CAS-style, pure validation).
- Errors: `:config/invalid`, `:config/profile-not-found`.

### F6 — `evoclj.runtime.trigger`

- Data-driven rules (threshold/comparator/window — no fns in rule
  data); `match-event-rule`, `match-metric-rule`, `evaluate`.
- Kernel-side action registry: `register-action!`, `run-actions!`
  (audited results, per-action error isolation).
- Errors: `:trigger/invalid`, `:trigger/action-not-found`.

### F7 — `evoclj.security.redact` + `evoclj.security.patch-lint`

- `redact` / `redact-event` — pattern and key-path redaction specs
  over EDN-safe values; idempotent.
- `lint-patch` / `lint-patch!` — static pre-application lint of
  Mutation IR ops against protected-path prefixes and allowed asset
  classes; findings are data (`:fatal` / `:warn`); `lint-patch!`
  throws on `:fatal`.
- Errors: `:security/redact-invalid`, `:security/patch-lint-invalid`,
  `:security/patch-lint-fatal`.

## 6. Commit plan

1. `docs: add common-foundations analysis (brainstorm → substrate)`
2. `feat(analytics): add behavior-profile foundation (F1)`
3. `feat(metrics): add metric records + inference foundation (F2)`
4. `feat(store): add enrichment-store foundation (F3)`
5. `feat(eval): add isolated worker-pool foundation (F4)`
6. `feat(config): add config & policy-surface foundation (F5)`
7. `feat(runtime): add event-trigger foundation (F6)`
8. `feat(security): add trust & hygiene foundation (F7)`

## 7. What the features build on top (future)

Once the foundations exist, the backlog features are thin layers:
speciation = F1 fingerprint + F3 enrichment over generations; bandit
budget = F2 per-class stats feeding the mutator; auto-rollback = F6
rule over F2 monitor metrics; redaction-first event append = F7 spec
hooked into the store write path. The foundations are the reusable
contract; the features are compositions of them.
