# EvoCLJ Documentation

EvoCLJ is a JVM-Clojure self-evolving agent runtime: immutable,
content-addressed **Genomes** compile into isolated **Phenotypes**;
phenotypes execute tasks through typed **Intents** that cross a
kernel-owned capability broker; evolution proposes structured
successor mutations, evaluates them in isolation, and promotes the
winners through an atomic compare-and-set.

## Document map

| Document | What it is | Read when... |
| --- | --- | --- |
| [`implementation-plan.md`](implementation-plan.md) | The normative plan: Milestones 1-9, global constraints, per-task acceptance criteria, top-level source/test maps | You need the authoritative contract for any subsystem (Genome, Compiler, SCI, Broker, Store, Executor, Evolution, Eval, Promotion) |
| [`models-integration.md`](models-integration.md) | Real LLM support: models.dev catalog, dialect layer, OpenAI/Anthropic adapters, :llm node + tool loop, LLM-driven evolution, `evoclj cycle`, LM Studio deployment notes | You are wiring, using, or debugging real models |
| [`roadmap.md`](roadmap.md) | Feature roadmap across five directions (evolution, evaluation, runtime, ops, security) with completion state | You want the current feature status and the future backlog |
| [`semantic-verification.md`](semantic-verification.md) | Formal verification of the seven core semantic claims (no mocks, real namespaces, re-runnable scripts) | You are changing core invariants or auditing safety |
| [`performance-baseline.md`](performance-baseline.md) | Measured benchmark baselines and regression ceilings (Task 12.2) | You are optimizing or changing hot paths |

## Suggested reading order

1. **Start here** — this index.
2. [`implementation-plan.md`](implementation-plan.md) — the architecture and constraints (the Global Constraints list is the safety contract).
3. [`models-integration.md`](models-integration.md) — how the runtime actually talks to language models.
4. [`roadmap.md`](roadmap.md) — what exists and what is next.
5. [`semantic-verification.md`](semantic-verification.md) and [`performance-baseline.md`](performance-baseline.md) — when you touch core logic or hot paths.

## Quick orientation

```text
src/evoclj/
  kernel/     host lifecycle + error contract
  genome/     immutable bundles, hashing, patches
  compiler/   validation, Resolution, topology/programs
  sci/        restricted evolvable Clojure execution
  intent/     typed effect requests
  capability/ leases, policy, authorization
  provider/   model/tool/memory adapters
  store/      SQLite, CAS, append-only events
  runtime/    sessions, scheduler, nodes, episodes, usage
  evolution/  evidence, diagnosis, mutation, candidates
  eval/       gates, paired runners, metrics, comparison, judge
  promotion/  candidate state, CAS activation, rollback
  cli/        operator entry points (run, evolve, cycle, eval,
              eval-inspect, promote, cost, recovery, events, ...)
```

## Command cheat-sheet

```bash
clojure -M:test                  # full test suite
clojure -M -m evoclj.cli.main model list
clojure -M -m evoclj.cli.main run --session <uuid>
clojure -M -m evoclj.cli.main cycle --generation current --no-promote
clojure -M -m evoclj.cli.main cost --generation current
clojure -M -m evoclj.cli.main recovery
clojure -M -m evoclj.cli.main events --session <uuid> --tree
clojure -M -m evoclj.cli.main eval-inspect <evaluation-id>
```

## Maintenance notes

- Every document is plain Markdown; keep the README index in sync when
  adding a document.
- The implementation plan is normative; feature docs describe the
  realized system on top of it — discrepancies are bugs to report,
  not doc edits to paper over.
