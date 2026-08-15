# EvoCLJ Feature Roadmap (goal: 5 directions x 3-10 features each)

Status: active goal — continue brainstorming, dispatch subagents per feature,
verify each with real-token (local LM Studio) runs, and commit. Tick features
as they land. Each direction must reach 3-10 COMPLETED features.

## Direction 1: Evolution Loop Depth (evolution/)
- [x] LLM Diagnostician + LLM Mutator + host wiring (committed 4086cfd/4972398/b7bb858)
- [ ] E1: LLM Mutator tool-call retrieval — expose :evolution/evidence and
      :evolution/history as broker tools so the model retrieves via the
      tool-calling loop instead of prompt-rendered context
- [ ] E2: hypothesis ranking — Diagnostician sorts hypotheses by confidence,
      kernel re-validates order before adoption
- [ ] E3: candidate diff report — CLI shows file-level diff parent vs candidate
- [ ] E4: mutation-budget adaptation — budget profile tuned from rejection history
- [ ] E5: evidence-pack enrichment — model usage/cost enters pack summaries

## Direction 2: Evaluation Depth (eval/)
- [x] V1: LLM-as-judge equivalence — :equivalence/llm-judge keyword registered,
      real model decides semantic output equivalence (currently byte-identical
      or injected fns only)
- [ ] V2: judge score aggregation — judge verdicts per case feed utility summary
- [ ] V3: eval inspection CLI — per-evaluation detail query (gates, sides, usage)
- [ ] V4: hard-case library — hidden selection cases shipped as fixtures
- [ ] V5: judge config — temperature/system-prompt/max-tokens exposed in config

## Direction 3: Runtime & Memory (runtime/, intent/)
- [x] R1: episodic memory wiring — memory/read + memory/write nodes against a
      kernel-owned memory store (currently intent types without a provider)
- [ ] R2: per-session memory isolation — session-scoped memory keys
- [ ] R3: llm-node retry/backoff — transient model errors retried with policy
- [ ] R4: concurrency documentation — scheduler single-session semantics
      documented + stress test

## Direction 4: Ops & Observability (cli/, store/)
- [ ] O1: session detail CLI — event-tree query for one session
- [ ] O2: cost report CLI — model usage/cost aggregated by generation
- [ ] O3: recovery scan CLI — expose store recovery scan results
- [ ] O4: LLM performance baseline — update docs/performance-baseline.md with
      real-model cycle timings
- [ ] O5: lineage CLI polish — promotion lineage with candidate diffs

## Direction 5: Security & Adversarial (sci/, capability/, adversarial/)
- [x] S1: prompt-injection suite — adversarial inputs to :llm nodes must not
      change tool behavior; assert tool-call boundaries hold
- [ ] S2: SCI sandbox adversarial expansion — new escape attempts rejected
- [ ] S3: lease refinement tests — per-model and per-tool lease denial cases
- [ ] S4: model-output schema hardening — strict output validation tests for
      judge/diagnosis/mutation shapes

## Progress
- Round 1: V1 (LLM-as-judge) in progress.