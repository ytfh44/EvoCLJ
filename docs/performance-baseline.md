# EvoCLJ Performance Baseline

Task 12.2 — benchmark fixtures and regression ceilings. This document is
written from the ACTUAL measurements of `test/evoclj/perf/runtime_benchmark_test.clj`
(three consecutive runs on the recording host, 2026-06). It records the
baseline environment, fixture sizes, the measured numbers, and the broad
pathological ceilings the benchmark asserts.

All timings are wall-clock via `System/nanoTime`, reported as best/mean
over repeated samples where applicable (the benchmark prints every
measurement on stdout — `[perf]` lines).

## 1. Baseline environment (recording host)

| Item | Value |
| --- | --- |
| Host | LAPTOP-J1T5JL0O (Windows 11, NT 10.0 build 26200), Git Bash (MINGW64) shell |
| JVM | OpenJDK 26.0.1 (Zulu26.30+11-CA), 64-bit Server VM |
| Clojure | org.clojure/clojure 1.12.5 (CLI 1.12.5.1664, deps.clj) |
| Test runner | cognitect.test-runner v0.5.1 (`:test` alias, `clojure -M:test`) |
| Working dir | `D:/PROJECTS/EvoCLJ` (repo root; benchmark runs from here) |

## 2. Fixture sizes

**Seed genome bundle** (`genomes/seed`, the ONLY read-only input the
benchmark touches):

| File | Size (bytes) |
| --- | --- |
| `manifest.edn` | 1 427 |
| `topology.edn` | 1 516 |
| `models.edn` | 532 |
| `memory.edn` | 312 |
| `evolution.edn` | 541 |
| `programs/route.clj` | 2 430 |
| **Total** | **6 758 (6 files)** |

**Benchmark-created fixtures** (temp dirs only, cleaned up after every
test): CAS small artifacts are 1 KiB each (200 puts), CAS large artifacts
are 1 MiB each (10 puts); the candidate-evaluation bundle is a
2-file-program genome written to a temp dir (manifest/topology/models/
memory/evolution + `programs/route.clj`).

## 3. Measured numbers (three runs)

| # | Measurement | Run 1 | Run 2 | Run 3 |
| --- | --- | --- | --- | --- |
| 1 | Genome load + hash (seed bundle, best of 3) | 10.7 ms | 9.1 ms | 10.1 ms |
| 2 | Compile seed genome (best of 3) | 11.3 ms | 11.4 ms | 10.8 ms |
| 3 | SCI invocation, per invocation (500 route-program calls) | 0.596 ms | 0.625 ms | 0.571 ms |
| 4 | Broker authorize, per call (1 000 calls) | 0.555 ms | 0.556 ms | 0.525 ms |
| 5 | Append-event throughput (200 chained events) | 73.3 ev/s | 77.4 ev/s | 75.9 ev/s |
| 6 | CAS small, 200 × 1 KiB puts | 2 785 ms | 2 801 ms | 2 607 ms |
| 7 | CAS large, 10 × 1 MiB puts | 1 046 ms | 1 303 ms | 1 123 ms |
| 8 | Seed end-to-end task (build + run, excl. model network) | 844 ms | 899 ms | 775 ms |
| 9 | Candidate evaluation orchestration (full G0–G6 pipeline) | 1 426 ms | 1 391 ms | 1 512 ms |

Derived per-run figures for #3/#4/#8:

- SCI: 500 invocations in 285–313 ms total.
- Broker: 1 000 authorizations in 525–556 ms total.
- Seed e2e split: executor build 335–396 ms, `scheduler/run-session!` run
  440–503 ms.

## 4. Regression ceilings (asserted by the benchmark)

Ceilings are deliberately 18–900× the recorded baseline — broad enough to
only trip on pathological regressions, never on normal host variance. The
two floors/ceilings named in Task 12.2 (`append-event > 50 events/s`,
`seed e2e < 60 s`, `genome load+hash < 10 s`) are used verbatim.

| # | Assertion in the benchmark | Recorded | Ceiling |
| --- | --- | --- | --- |
| 1 | Genome load+hash completes | ~10 ms | < 10 000 ms |
| 2 | Compile completes | ~11 ms | < 10 000 ms |
| 3 | Mean SCI invocation | ~0.6 ms | < 20 ms |
| 4 | Mean broker authorize | ~0.55 ms | < 10 ms |
| 5 | Append-event throughput | ~75 ev/s | **> 50 events/s** |
| 6 | 200 × 1 KiB CAS puts | ~2.7 s | < 30 000 ms |
| 7 | 10 × 1 MiB CAS puts | ~1.1 s | < 30 000 ms |
| 8 | Seed end-to-end task latency | ~0.85 s | < 60 000 ms |
| 9 | Candidate evaluation orchestration | ~1.4 s | < 60 000 ms |

**Margin note on #5:** the recorded append-event throughput (~75 events/s,
dominated by one fsync'd SQLite transaction per event on this host) sits
1.5× above the 50 events/s floor. 50 events/s is the Task 12.2 normative
example floor and is kept verbatim; the 1.5× margin means a busy host with
a ~35% slowdown would approach the floor — if this ever flakes on CI, raise
the floor's margin by asserting on the best-of-3 run or lowering it to
25 events/s. All other ceilings have ≥ 18× headroom.

## 5. Running and excluding the benchmark

The benchmark namespace is `evoclj.perf.runtime-benchmark-test`. Every
deftest carries `^:perf` metadata; the namespace is fully self-contained
(only its own temp dirs plus the read-only `genomes/seed` bundle) and is
independent of the correctness suite — timing assertions can never affect
the 601 correctness tests.

```text
# run ONLY the benchmark
clojure -M:test -n evoclj.perf.runtime-benchmark-test

# run only the ^:perf tests in the whole suite
clojure -M:test -i :perf

# EXCLUDE the perf tests from a correctness run
clojure -M:test -e :perf

# run everything (benchmark included — the default, and it must pass)
clojure -M:test
```

The benchmark runs (and passes) as part of the default full suite. The
`-e :perf` exclusion exists for hosts where wall-clock jitter is
unacceptable in CI.

## 6. What each measurement exercises (source of truth)

| # | Code path | Production module |
| --- | --- | --- |
| 1 | `load/load-genome` on `genomes/seed` (load + content addressing, Global Constraint 6 determinism asserted) | `evoclj.genome.load` |
| 2 | `core/compile-genome` (Resolution + topology/program compilation) | `evoclj.compiler.core` |
| 3 | `execute/invoke!` on the real route program (isolated SCI runtime) | `evoclj.sci.execute` |
| 4 | `broker/authorize` pure decision with a granted lease | `evoclj.capability.broker` |
| 5 | `event/append-event!` chained causal events in one pinned session | `evoclj.store.event` |
| 6/7 | `cas/put-bytes!` (+ `get-bytes` round-trip) for 1 KiB / 1 MiB payloads | `evoclj.store.cas` |
| 8 | Real seed end-to-end: load → compile → instantiate → pinned session → `scheduler/run-session!` → broker dispatch → event store (no model network) | `evoclj.runtime.scheduler` + broker + store |
| 9 | `eval-core/evaluate-candidate!` full G0–G6 phase order on a minimal valid evaluator | `evoclj.eval.core` |

## 7. Full-cycle harness (Task O4)

`scripts/full-cycle.clj` (ns `evoclj.perf.full-cycle-harness`) runs the
whole evolve → eval → promote loop through the same public subsystem
APIs the `cycle` CLI command walks — `propose-candidates!`,
`evaluate-candidate!` (with the Task A2 F2 collector), and the atomic
`promote!` CAS — timing every phase with `System/nanoTime` and
collecting F2 metric records (`evoclj.metrics.core`, closed
MetricSchema) into one structured EDN report.

**The harness seed genome is `test/fixtures/evolution-e2e/route-a`**
(G1), the same deterministic full-cycle fixture the cli cycle tests
use, with the hidden selection set
(`test/fixtures/evolution-e2e/selection`, sel-a/sel-b) and the
recorded replay case. It is NOT `genomes/seed`: the seed route never
fails (every input finishes), so it can never produce the failure
evidence the deterministic pattern Diagnostician needs to fire a
`:task/success` hypothesis; route-a fails `:echo-b` by construction,
producing the Evolution set (1 success + 2 failures) that fires the
mutation and a real promotion (route-b behavior).

### 7.1 NO MODEL ENDPOINT (explicit)

The host system is built with the real models.dev catalog (3 s fetch
timeout; cached under the state dir; an unreachable source degrades
to `:catalog/unavailable` without failing startup). A model endpoint
is reported reachable ONLY when the kernel-owned model registry has a
live provider (an API key). **On the recording host no API keys are
configured** (`:model/configured []`, `:model/endpoint? false`, catalog
`:catalog/fresh`), so the harness falls back to the fixture providers:
**the numbers below are FIXTURE-mode timings, NOT real-model cycle
timings.** Roadmap O4's real-model timings are deferred until a host
with a configured model endpoint runs the harness; the report's
`:provider` section always states this honestly.

### 7.2 Measured numbers (single headless run, recording host, 2026-08-16)

| Phase | Wall time | What it measures |
| --- | --- | --- |
| EVOLVE | 274 ms | evidence pack + deterministic Diagnostician + 1 bounded mutation, `propose-candidates!` |
| EVAL | 2 242 ms | full G0–G6 evaluation of the candidate (per-gate breakdown below) |
| PROMOTE | 98 ms | candidate CAS body + atomic `promote!` CAS moving CURRENT |
| **CYCLE total** | **2 620 ms** | evolve + eval + promote, one headless run |

Per-gate eval breakdown (the Task A2 `:eval/<phase>-ms` records):

| Gate | Wall time |
| --- | --- |
| G0 parse | 31 ms |
| G1 schema/ABI | 10 ms |
| G2 static policy | 18 ms |
| G3 deterministic suites | 23 ms |
| G4 replay | 68 ms |
| G5 paired hidden selection | 2 003 ms (dominant: two fresh isolated stores + SQLite/CAS per side) |
| G6 cost/complexity | 10 ms |
| eval total | 2 237 ms |

Outcome: 1 candidate evolved, 1 eligible evaluation, 1 promotion
(`:cycle/promoted? 1.0`); the report's `:f2/metrics` vector holds 21
schema-valid records. Seed genome id
`sha256:ec7134443cff4ea57c44b6d3fbcbd17ed3e5c1aa9e78ae91e0046a062d6ffeb5`.

### 7.3 Running the harness

```text
# full run, report as EDN on stdout (fresh temp state dir, cleaned up)
clojure -M scripts/full-cycle.clj

# write the report to a file (kept for the baseline record)
clojure -M scripts/full-cycle.clj --out reports/full-cycle-report.edn

# pin the state dir (inspect the db/cas afterwards)
clojure -M scripts/full-cycle.clj --state-dir /tmp/fullcycle-state --out /tmp/fullcycle-report.edn

# the harness test (drives the same code in-process)
clojure -M:test -n evoclj.perf.full-cycle-test
```

Run from the repo root (`D:/PROJECTS/EvoCLJ`). The harness is
headless and deterministic: no network is needed (the catalog fetch
times out to `:catalog/unavailable` when offline and the fixture
providers keep working), and the full suite runs the test as part of
`clojure -M:test`.
