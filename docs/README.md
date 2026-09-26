# EvoCLJ Documentation

EvoCLJ is a JVM-Clojure self-evolving agent runtime: immutable,
content-addressed **Genomes** compile into isolated program images
(**Phenotypes** — program identity only, never execution semantics);
Phenotype instances execute tasks through typed **Intents** that cross a
kernel-owned capability broker; evolution proposes structured
successor mutations, evaluates them in isolation, and promotes the
candidates that pass evaluation under the active profile through
an atomic compare-and-set (evidence of passing experiment E under
profile P — never a global improvement proof).

## Document map

| Document | What it is | Read when... |
| --- | --- | --- |
| [`implementation-plan.md`](implementation-plan.md) | The normative plan: Milestones 1-12, global constraints, per-task acceptance criteria, top-level source/test maps | You need the authoritative contract for any subsystem (Genome, Compiler, SCI, Broker, Store, Executor, Evolution, Eval, Promotion) |
| [`models-integration.md`](models-integration.md) | Real LLM support: models.dev catalog, dialect layer, OpenAI/Anthropic adapters, :llm node + tool loop, LLM-driven evolution, `evoclj cycle`, LM Studio deployment notes | You are wiring, using, or debugging real models |
| [`roadmap.md`](roadmap.md) | Feature roadmap across five directions (evolution, evaluation, runtime, ops, security) with completion state | You want the current feature status and the future backlog |
| [`semantic-verification.md`](semantic-verification.md) | Formal verification of the ten core semantic claims (no mocks, real namespaces, re-runnable scripts) | You are changing core invariants or auditing safety |
| [`performance-baseline.md`](performance-baseline.md) | Measured benchmark baselines and regression ceilings (component) | You are optimizing or changing hot paths |
| [`invariants.md`](invariants.md) | Shared invariant checklist: inherited Global Constraints GC-01–GC-24 plus repair invariants INV-01–INV-13, each with incident evidence, violation consequences, and its guarding test/script. Also the authoritative MCP/Skills closure state (the 2026-08-20 gap-closure report was superseded and has since been retired) | You are implementing or adversarially reviewing a repair work item (checklist step 5) |
| [`canonical-conventions.md`](canonical-conventions.md) | The canonical-EDN map: `evoclj.genome.hash` is the sole owner, plus every deliberate variant (candidate-store, dag, patch-edn, resolution), the one known sharp edge in `compiler.core`, and the decision table for whether a copy may delegate | You are touching anything that hashes EDN content — identity bytes, pool keys, merge-plan or case-body digests |

## Suggested reading order

1. **Start here** — this index.
2. [`implementation-plan.md`](implementation-plan.md) — the architecture and constraints (the Global Constraints list is the safety contract).
3. [`models-integration.md`](models-integration.md) — how the runtime actually talks to language models.
4. [`roadmap.md`](roadmap.md) — what exists and what is next.
5. [`semantic-verification.md`](semantic-verification.md) and [`performance-baseline.md`](performance-baseline.md) — when you touch core logic or hot paths.
6. [`canonical-conventions.md`](canonical-conventions.md) — before you touch anything that hashes EDN content.

## Quick orientation

```text
src/evoclj/
  kernel/     host lifecycle (Integrant), error contract, dynamic types
  config.clj  system.edn loading + env overrides
  control.clj system entry point
  genome/     immutable bundles, hashing, patches, paths
  compiler/   validation, Resolution, topology/programs
  sci/        restricted evolvable Clojure execution
  intent/     typed effect requests, generic dispatch
  capability/ leases, policy, authorization, budgets, registry
  provider/   model/tool/memory adapters (openai, anthropic, fixture)
  store/      SQLite, CAS, append-only events, work/session/binding
  runtime/    sessions, scheduler, nodes, episodes, subagents, usage
  evolution/  evidence, diagnosis, mutation, candidates
  eval/       gates, paired runners, metrics, comparison, judge
  promotion/  candidate state, CAS activation, rollback, monitor
  context/    offers, materializer, policy, compression
  environment/ registry, sources, revisions, bundles, surfaces
  binding.clj call binding (a call's resolved target set)
  node/       node descriptors and executors
  mcp/        MCP sources, client, manager, codec, transport
  skill/      skill adapter, vendor, surfaces
  mount/      filesystem + CAS mount backends
  fs/         walk, snapshot, CAS trees
  security/   static recheck for the promotion gate
  analytics/  metrics rollups
  metrics/    metric records and report shapes
  tool/       generic tool protocol helpers
  support/    test-support namespaces (fixtures, concurrency)
  cli/        operator entry points (run, evolve, cycle, eval,
             eval-inspect, promote, rollback, cost, recovery, events,
             context, source, skill, model, mcp, deploy, ...)
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
