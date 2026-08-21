# Real LLM Provider Integration (post-v0 extension 1)

This document describes the real-model capability added to EvoCLJ after
the v0 release gate: the models.dev catalog service, the dialect layer,
the OpenAI-compatible and Anthropic provider adapters, and their wiring
through the broker, leases, the :llm node, and the CLI.


## Contents

- [What was added](#what-was-added)
- [The catalog](#the-catalog)
- [The dialect layer (OpenAI-compatible dialects)](#the-dialect-layer-openai-compatible-dialects)
- [Providers](#providers)
- [Authorization and execution](#authorization-and-execution)
- [API keys](#api-keys)
- [CLI](#cli)
- [Tests](#tests)
- [Configuration (resources/system.edn)](#configuration-resourcessystemedn)
- [LLM-driven evolution](#llm-driven-evolution)
  - [Enabling it in system.edn](#enabling-it-in-systemedn)
  - [The :model-call injection contract](#the-model-call-injection-contract)
  - [The kernel-computes-:expect/hash security property](#the-kernel-computes-expecthash-security-property)
  - [Fail-loud error contract](#fail-loud-error-contract)
  - [Driving a cycle through the CLI](#driving-a-cycle-through-the-cli)
- [`evoclj cycle` — one command walks the whole loop](#evoclj-cycle--one-command-walks-the-whole-loop)
- [Real end-to-end loop on a local server (verified with LM Studio)](#real-end-to-end-loop-on-a-local-server-verified-with-lm-studio)
  - [Provider catalog and resolution](#provider-catalog-and-resolution)
  - [Registry timeouts for slow local models](#registry-timeouts-for-slow-local-models)
  - [JSON stability: use the server's json_schema mode](#json-stability-use-the-servers-json_schema-mode)
  - [Model-output robustness the adapters already handle](#model-output-robustness-the-adapters-already-handle)
  - [Attribution](#attribution)

## What was added

| Component | File | Role |
| --- | --- | --- |
| models.dev catalog | src/evoclj/provider/modelsdev.clj | fetch/validate/cache/index https://models.dev/api.json at every startup |
| dialect layer | src/evoclj/provider/dialect.clj | pure-EDN request/response transforms for OpenAI-compatible dialects |
| OpenAI-compatible adapter | src/evoclj/provider/openai.clj | one Provider per endpoint, built on com.openai:openai-java (base-url override) |
| Anthropic adapter | src/evoclj/provider/anthropic.clj | one Provider per endpoint, built on com.anthropic:anthropic-java |
| model registry | src/evoclj/provider/model_registry.clj | model-id -> provider instance, one adapter per (provider, style, base-url) |
| dispatch | src/evoclj/intent/dispatch.clj | :intent/model-call branch (model registry lookup -> normalize -> authorize -> execute) |
| leases | src/evoclj/capability/lease.clj | :kind :model resource matching (exact id or provider/* prefix) |
| policy | src/evoclj/capability/policy.clj | :intent/model-call requests the :invoke action |
| :llm node | src/evoclj/runtime/nodes/llm.clj | emits :intent/model-call; resolves the models.edn alias via the compiled Resolution |
| CLI | src/evoclj/cli/model.clj, main.clj, session.clj | model list / model inspect / --model run lease |
| host wiring | src/evoclj/kernel/system.clj, resources/system.edn | :modelsdev/catalog and :model/registry Integrant components |
| dependencies | deps.edn | com.openai/openai-java 4.50.0, com.anthropic/anthropic-java 2.54.0, cheshire 5.13.0 |

## The catalog

- Fetched from https://models.dev/api.json (185 providers, ~6300 models
  as of the recording date) at EVERY startup — never embedded, per the
  operator requirement.
- Cached atomically under <state-dir>/catalog (api.json + meta.edn);
  offline startups fall back to the cache (:catalog/cached); with
  neither network nor cache the catalog is :catalog/unavailable and
  model resolution fails closed (fixture providers keep working).
- Each model is classified by API style, the JVM answer to Vercel AI
  SDK's npm labels: "@ai-sdk/openai-compatible" / "@ai-sdk/openai" ->
  :openai-compatible, "@ai-sdk/anthropic" -> :anthropic. A built-in
  override table maps providers whose primary SDK label is proprietary
  but which expose OpenAI-compatible endpoints in practice (azure,
  mistral, xai, groq, togetherai, cerebras, fireworks-ai, deepinfra,
  perplexity, google/gemini, nvidia, lmstudio, ollama, ...) — Azure
  OpenAI is treated exactly as the OpenAI-compatible dialect it is.
- Base URLs resolve from operator config > the catalog :api field >
  the well-known table. Models with a known style but no base URL are
  :needs-config (operator supplies :catalog/base-urls); models with no
  OpenAI-compatible or Anthropic-compatible endpoint are listed
  :unsupported honestly.

Coverage at recording time: 6288 models — 4786 supported
(:openai-compatible 4968, :anthropic 71), 253 needs-config,
1249 unsupported.

## The dialect layer (OpenAI-compatible dialects)

Because OpenAI-compatible APIs have many dialects, the adapter never
assumes the OpenAI shape:

- Interleaved reasoning: DeepSeek returns reasoning in the
  reasoning_content field of the assistant message. The catalog
  marks such models (:interleaved {:field "reasoning_content"} or
  boolean true); the dialect layer extracts that field into
  :model/output :reasoning — never merged into :text.
- reasoning effort: {:reasoning {:mode :effort :level "high"}}
  becomes the reasoning_effort request param; toggle modes map to a
  per-dialect param (:reasoning-toggle-param) and are rejected with
  a typed error when the dialect has none.
- Server-side search: :web-search-options mode emits the native
  web_search_options body (openai-java WebSearchOptions); the
  :web-search-tool mode appends a tools-based web_search declaration.
- Vendor fields: any dialect :extra-params merge into the request
  body via additionalProperties (openai-java putAdditionalProperty),
  serialized snake_case.
- Everything is pure EDN (evoclj.provider.dialect): fully unit
  tested offline without the SDK.

## Providers

Both adapters implement the v0 Provider protocol (describe /
normalize-request / execute-request!). Constructor secrets (API keys,
base URLs) are closed over; only validated EDN crosses the boundary.
execute-request! reads the raw HTTP response through the SDK
raw-response API and converts it to EDN before returning. Results:

    {:model/output {:text "..." :reasoning "..."}   ; reasoning when dialect
     :usage {:model-input-tokens n :model-output-tokens n
             :model-cost-units <usd estimate>}}     ; from catalog pricing

Errors: HTTP 429/5xx and IO timeouts are :provider/transient-error
(retryable because the descriptor declares :retry {:safe? true} —
a model call has no side effect beyond cost); 4xx and malformed
responses are :provider/model-error. The output-schema validation in
the dispatcher rejects malformed provider output (never model-visible
data).

## Authorization and execution

- The :llm node emits a validated :intent/model-call with the
  RESOLVED full models.dev id (the compiled Resolution maps the
  models.edn alias to provider-model, e.g. deepseek/deepseek-v4-flash).
- The broker dispatches :intent/model-call through the model registry:
  unknown model -> :provider/not-found; no API key -> 
  :provider/not-configured (reason :api-key-missing); then
  normalize-request -> canonical {:kind :model :id <full-id>} resource
  -> broker authorize -> execute.
- Leases: {:resource {:kind :model :id "deepseek/deepseek-v4-flash"}}
  grants exactly one model; {:id "deepseek/*"} grants a provider
  prefix. Global Constraint 9 holds: listing a model never grants it.

## API keys

Each catalog provider declares its expected environment variable
(:model/api-key-env, e.g. DEEPSEEK_API_KEY, ANTHROPIC_API_KEY). Keys
resolve from the :registry/api-keys config override first, then the
environment. Models without a key are registered :api-key-missing and
fail closed with an informative error.

## CLI

    evoclj model list                      ; supported models + status counts
    evoclj model list --style :anthropic   ; filter by style/status/provider
    evoclj model inspect deepseek/deepseek-v4-flash
    evoclj run --genome current --task t.edn --model deepseek/* ...

## Tests

- evoclj.provider.modelsdev-test — fetch/cache/fallback/classification
- evoclj.provider.dialect-test — request/response transforms
- evoclj.provider.openai-test / anthropic-test — adapter e2e against a
  local fake HTTP endpoint (wire-dialect assertions included)
- evoclj.provider.model-integration-test — registry, leases, dispatch
- evoclj.runtime.llm-e2e-test — a full :llm-node Genome runs through
  the real pipeline against the fake endpoint

All model tests are offline: local HttpServer fixtures only.

## Configuration (resources/system.edn)

    :modelsdev/catalog
    {:url "https://models.dev/api.json"
     :cache-dir "catalog"          ; resolved against the state dir
     :ttl-hours 24
     :timeout-ms 30000
     :base-urls {}                 ; operator endpoint overrides
     :style-overrides {}           ; per-provider style overrides
     :dialect-overrides {}}        ; per-provider dialect overrides

    :model/registry
    {:catalog #ig/ref :modelsdev/catalog
     :registry/api-keys {}}        ; provider-id -> api key (or env)

Environment overrides: EVOCLJ_CATALOG_URL, EVOCLJ_CATALOG_CACHE_DIR.

## LLM-driven evolution

The deterministic pattern Diagnostician (component) and the no-op
default Mutator are the shipped defaults, but both evolution adapters
can be switched to LLM-driven ones by configuring a `{:type :llm ...}`
map in `resources/system.edn`. LLM evolution is strictly OPT-IN — the
shipped `:diagnostician` pattern map and `:mutator :none` stay
unchanged until an operator enables it.

### Enabling it in system.edn

Within the `:evolution/system` block, inject the model registry and
the broker dispatch context, grant a model lease, and switch the two
adapters to their `:llm` forms:

    :evolution/system
    {:store {...}
     :diagnostician {:task/success-threshold 1.0 ...}   ; shipped default
     :mutator :none                                      ; shipped default
     ...
     ;; --- enable LLM-driven evolution (uncomment to turn on) ---
     ;; :model/registry #ig/ref :model/registry
     ;; :dispatch #ig/ref :capability/broker
     ;; :model-lease {:kind :model :id "lmstudio/*"}
     ;; :diagnostician {:type :llm
     ;;                 :model/id "lmstudio/qwen3.6-35b-a3b-uncensored-hauhaucs-aggressive"
     ;;                 :max-hypotheses 3
     ;;                 :confidence-band :medium}
     ;; :mutator {:type :llm
     ;;           :model/id "lmstudio/qwen3.6-35b-a3b-uncensored-hauhaucs-aggressive"
     ;;           :max-mutations 3}}

The three wiring keys are OPTIONAL and only consulted when an `:llm`
adapter is present:

- `:model/registry` — the kernel-owned model registry (`#ig/ref
  :model/registry` or an injected atom). REQUIRED once an `:llm`
  adapter is configured.
- `:dispatch` — the `:capability/broker` value (or a compatible
  broker context). REQUIRED once an `:llm` adapter is configured.
- `:model-lease` — an optional capability lease granting the model
  resource (`:kind :model :id "<provider>/*"`). Without one the
  broker denies every model call.

If an `:llm` adapter is configured but `:model/registry` or
`:dispatch` is missing, the host FAILS CLOSED with
`:evolution/system-invalid` (reason `:llm-needs-model-registry` /
`:llm-needs-dispatch`) — it never silently falls back to the pattern
adapter or the no-op mutator.

### The :model-call injection contract

The host builds ONE `:model-call` closure (in
`evoclj.kernel.system/build-model-call`) and wires it into both LLM
adapters. Each call constructs a single attributable
`:intent/model-call` and dispatches it through a LOCAL broker context
(the host broker context is never mutated — only the model registry and
lease are injected locally):

    (fn [model-id messages options])   ; -> the broker dispatch result
        {:result/status :ok
         :value {:model/output {:text "..."}
                 :usage {...}}}
        ;; or THROWS ExceptionInfo with a stable :error/type

Attribution is kernel-deterministic (Global Constraint 20): a fixed
`session-id` over `evoclj/evolution/session`, a content-addressed
`phenotype-id` (`sha256:...`, from `evoclj/evolution`), the
`:node/evolution` node, and `cause/event-id 0`. The adapters never
call a provider directly (Global Constraint 8) — every external effect
crosses the broker. Error data is EDN-safe and sanitized (Global
Constraint 22).

### The kernel-computes-:expect/hash security property

The LLM Mutator proposes mutation ops WITHOUT `:expect/hash` (a
language model cannot compute digests). The adapter — not the model —
attaches each op's `:expect/hash` from the parent Genome's `:files`
digest, using the same `sha256:...` convention the patch runtime
verifies against. A model can never name a preimage it does not know,
so stale patches are impossible. The persisted candidate record's
mutation `:expect/hash` therefore always equals the parent file's
digest, even when the model JSON carried none.

### Fail-loud error contract

Model failures never silently degrade the evolution loop. The
adapter's `:model-call` throws `:evolution/model-call-failed` on a
non-`:ok` dispatch; the LLM Diagnostician/Mutator translate that into
`:diagnosis/llm-failed` / `:mutation/llm-failed`. A model response
that cannot be parsed, or whose hypotheses/mutations are entirely
unusable, throws `:diagnosis/llm-response-invalid` /
`:mutation/llm-response-invalid` rather than returning an empty
result (the LLM-NOISE TOLERANCE POLICY).

### Driving a cycle through the CLI

Use the CLI to run a full evolution cycle end to end:

    evoclj evolve --generation current

With the `:llm` adapters wired, `evolve` calls
`evolution.core/propose-candidates!`, which freezes the evidence pack,
runs the LLM Diagnostician and Mutator through the broker, and persists
candidate records — all against the injected model lease. (See
`test/evoclj/kernel/evolution_llm_wiring_test.clj` for a fully
offline fake-endpoint integration test of the wiring.)
## `evoclj cycle` — one command walks the whole loop

`evoclj cycle` is the single orchestrated path from evidence to
promotion, so an operator no longer runs `evolve`, then `eval` per
candidate, then `promote` per evaluation by hand. It composes the same
public subsystem entry points the individual commands use and stays a
driver (Global Constraint 15 — the only pointer that ever moves is the
atomic CURRENT compare-and-set inside `promotion.promote/promote!`; it
never auto-schedules or daemonizes):

    evoclj cycle [--generation <id|current>] [--profile <profile-id>]
                 [--max-candidates <n>] [--evolve] [--no-promote]

Phases:

1. **Resolve the generation** (default `current`, via
   `evoclj.cli.session/current-generation-info`).
2. **EVOLVE** — run exactly what `evolve` runs
   (`evolution.core/propose-candidates!` with the standard
   evidence-selector and `--max-candidates`, default 3). It runs only
   when `--evolve` is given OR no `:evaluation-pending` candidate exists
   for that generation, so a rerun evaluates pre-existing candidates
   without proposing a duplicate cycle.
3. **EVAL** — for every `:evaluation-pending` candidate of the
   generation, run the `eval` pipeline
   (`evoclj.cli.session/build-evaluator` + `eval.core/evaluate-candidate!`
   under `--profile`, default `:default-v1`) and collect each Evaluation.
4. **PROMOTE** — for every evaluation whose eligibility verdict is
   exactly `:eligible? true`, run the `promote` pipeline as
   `evoclj.cli.promotion/promote!` does (operator session, the compiled
   Resolution id of the candidate bundle, `expected-parent-generation`
   = the candidate parent generation). `--no-promote` skips the pointer
   move and reports what WOULD happen.

A failed eval or failed promote for ONE candidate never aborts the
cycle — the failure is collected into the report as per-candidate
evidence. A broken system map or a genuinely missing candidate id stays
a hard typed `:cli/*` error. The command returns a plain EDN-safe
report (Global Constraint 22):

    {:generation/id <stable-id>
     :phases {:evolve {:run? bool :candidates [<candidate-shape> ...]}
              :eval [{:candidate/id <uuid> :evaluation/id <uuid>
                      :eligibility {:eligible? bool :reasons [...]}}
                     | {:candidate/id <uuid> :error {...}} ...]
              :promote [{:candidate/id <uuid> :status <kw> :outcome <result>
                         :error {...}} ...]}}

For the promotion to pass `promote!` Database Invariant 7 integrity
re-hash, the candidate Genome canonical body must be present in the
CAS. `evoclj cycle` performs that host bookkeeping itself (the same
storage a standalone `promote` assumes the operator already did) before
promoting each passing candidate.

## Real end-to-end loop on a local server (verified with LM Studio)

The full real-model loop — evidence, LLM diagnosis, LLM mutation, candidate, real-model evaluation, CAS promotion — has been verified end to end against a local LM Studio server (gemma-4-e2b, one cycle in about a minute; repeated runs produced candidates, real cost figures, and {:status :promoted} CAS switches). Deployment guidance learned from those runs:

### Provider catalog and resolution

The :provider-catalog of BOTH :evolution/system and :eval/system must resolve the genome's model alias to the real model id, otherwise the compiled topology names a fixture model:

    {:reasoning/high {:provider :lmstudio
                      :provider-model "lmstudio/gemma-4-e2b-uncensored-hauhaucs-aggressive"
                      :adapter-version "1"}}

The runtime model dispatch itself is driven by the model registry (:model/registry plus a model lease), independent of the compile-time catalog — a mismatch between them surfaces as :provider/not-found :reason :unknown-model at dispatch time.

### Registry timeouts for slow local models

Local models are slow: the SDK default is 60s per call. Set :registry/timeout-ms on the :model/registry component (e.g. 300000) so evolution/evaluation calls are not killed mid-generation.

### JSON stability: use the server's json_schema mode

Small local models drift out of strict-JSON output. LM Studio does NOT support response_format json_object (400 error) but DOES support json_schema. Pin the model's dialect extra-params so every diagnosis/mutation response is forced into valid JSON:

    :model/dialect {:interleaved :none
                    :reasoning-options []
                    :server-side-search :off
                    :extra-params
                    {:response_format
                     {:type "json_schema"
                      :json_schema
                      {:name "diagnosis_or_mutations"
                       :schema {:type "object"
                                :properties
                                {:hypotheses {:type "array"}
                                 :mutations {:type "array"}}
                                :required []}}}}}

### Model-output robustness the adapters already handle

- Models emit counterevidence as string arrays like ["None"] — the LLM Diagnostician drops non-ref entries and keeps the hypothesis.
- Models emit keywords with leading colons in EDN values ("::type", ":steps"), which would pr-str to ILLEGAL EDN (::steps) and compile-fail the candidate — the LLM Mutator sanitizes every op's EDN payload to plain keywords.
- The evaluation stage carries real model usage into the G6 cost guardrail: with :measure/cost absent, the :cost summary section is derived from the aggregated :model-cost-units of both sides.

### Attribution

All evolution/evaluation model calls are attributed to deterministic kernel ids (see the :model-call injection contract above) and the eval runner attributes its model usage to the side session, so every cost figure is auditable (Global Constraint 20).
