# Canonical EDN conventions — the owner and its deliberate variants

EvoCLJ hashes logical content in several places (Genome trees, Resolution
IDs, program/deployment IDs, pool keys, merge plans, candidate file bytes,
regression case bodies). All of them rest on one idea: **normalize a value
so that equal logical content produces identical bytes, then hash those
bytes** (Global Constraint 6). This document records which namespaces own
that normalization, which ones deliberately differ, and why — so a future
reader does not "unify" a variant that is load-bearing.

Normative owner: `evoclj.genome.hash` (INV-05). Its docstring says it
directly: *"This is THE convention; do not introduce a second copy."*

---

## The owner

| namespace | function | output |
| --- | --- | --- |
| `evoclj.genome.hash` | `canonical` | normalized **value** (maps → `sorted-map-by` pr-str key order; sets → `sorted-set-by` pr-str element order; vectors/seqs → eager `mapv`; scalars unchanged) |
| `evoclj.genome.hash` | `text-digest` | `"sha256:<64 hex>"` of UTF-8 bytes, **CRLF/CR normalized to LF** |
| `evoclj.genome.hash` | `digest` | `text-digest` of `pr-str` of `canonical` — the convenience composition nearly every caller wants |

Callers must use `digest` unless they specifically need the intermediate
value or the byte-level variant.

---

## Deliberate variants (do NOT unify)

### `evoclj.store.candidate_store` — VerifiedDigest-aware

Carries an extra `existence/verified-digest?` branch that the owner has no
reason to know about. The owner's docstring calls this out by name. This
is a *superset* of the owner's behaviour, not a divergence in ordering.

### `evoclj.evolution.dag` — provenance-stripping

`canonicalize` (`src/evoclj/evolution/dag.clj:23-34`) differs from the
owner in two ways, **both required by its contract**:

1. **It strips `provenance-keys`** (17 keys: `:mutation/id`, `:candidate/id`,
   `:hypothesis/id`, `:evidence/id`, `:created-at`, `:provenance`, `:source`,
   `:source/id`, `:parent/source`, `:branch/id`, `:edge/id`, `:metadata`,
   `:reason`, `:risk`, …). `canonical-merge-plan`'s documented contract is
   *"provenance-free and stable across map/set/input ordering"* — two merge
   plans that differ only in evidence/provenance must digest **identically**,
   because they describe the same merge. The owner must never do this: silently
   dropping keys is exactly the behaviour a general canonicalizer must not have.
2. **It orders by raw UTF-8 `str` bytes** (`bytes-key`), not `pr-str`. This
   is a genuinely different total order, and it is part of the merge-plan
   digest's frozen definition (`parent-edge-key`, `merge-plan-digest`).

Evidence this is not accidental drift: `merge-plan-digest` and
`parent-edge-key` are persisted dedupe keys for merge plans, and the
namespace docstring states the provenance-free guarantee as the contract.

### `evoclj.genome.patch_edn` — canonical EDN **text**

`canonical-str` (`src/evoclj/genome/patch_edn.clj:45-63`) is not a value
normalizer at all — it is a **writer**. It produces EDN *source text*
(hand-rolled `{...}`, `#{...}`, `[...]`, `(...)` with single-space joins)
rather than a normalized value, because its output is written into Genome
files whose bytes are then content-addressed by `hash/text-digest`
(`canonical-edn-text`, `evolution/mutation.clj:579`). It differs from `pr-str` in
whitespace and in preserving vector-vs-list print shape.

This is why `patch-edn` cannot delegate to `hash/canonical`: the owner
returns a *value*; this must return *bytes for a file*.

### `evoclj.compiler.resolution` — validated, fail-closed normalizer

`canonical-edn` (`src/evoclj/compiler/resolution.clj:89-115`) is
**behaviourally different from the owner on purpose**:

| input | `resolution/canonical-edn` | `genome.hash/canonical` |
| --- | --- | --- |
| map with mixed-type keys (keyword + string) | sorts via `canonical-compare` — works | `sorted-map-by` default `compare` → **`ClassCastException`** |
| non-EDN value (function, lazy seq, host object) | **throws `:resolution/invalid`** (Global Constraint 22) | passes through silently |
| set | `sorted-set-by canonical-compare` | `sorted-set-by` pr-str compare |
| vector | `mapv` | `mapv` |

The fail-closed rejection is the point: Resolution consumes
**externally-supplied** model config and provider catalog data, so it must
refuse anything that is not pure EDN. The owner is a low-level primitive
called on already-validated data and is allowed to be permissive.

**Do not merge these.** Doing so would either (a) delete the GC-22 gate, or
(b) change `ResolutionId` bytes. Both are unacceptable — see the next
section for the measured evidence.

---

## `evoclj.compiler.core` — a known sharp edge (open item)

`compiler/core.clj`'s private `canonical-edn-value` (`:162-177`) is a
**third** normalizer, used by `code-id`, `deployment-id`, and
`runtime-image-id`. It differs from the owner in exactly the two ways
above (no fail-closed gate; `sorted-map` default ordering) and it sorts
sets as `vec`, matching neither the owner nor Resolution.

Measured behaviour on inputs that reach it in production:

| input | owner | `compiler.core` |
| --- | --- | --- |
| plain `{:kernel 1 :genome 1 …}` (the ABI) | same bytes | same bytes |
| default runtime descriptor (`:adapter/builds` = string→string map) | same bytes | same bytes |
| catalog entry containing a set | `#{"a" "b"}` | `["a" "b"]` — **different bytes** |
| `bindings` with a mixed-type-key map | works | **`ClassCastException`, untagged** |

The first two rows are why this has never caused a visible failure: the
values actually passed today are maps of keyword→scalar and the
descriptor is `string→string`, so the three implementations agree on all
**currently reachable** inputs. The last two rows are reachable in
principle — `deployment-id` is called with caller-supplied `bindings` and
`leases` from `runtime/phenotype.clj:274`, and a `ClassCastException` is
neither one of this codebase's typed errors nor a fail-closed signal.

**Status: deferred, not unified.** Unifying here would change
`ProgramImageId` / `DeploymentId` / `ResolutionId` bytes (all persisted
identity) and — since `compiler/core.clj:184-185` documents *"Byte-identical
formula, unchanged"* as an explicit compatibility commitment — requires a
deliberate identity-migration decision plus a decision on whether
`compilation` should gain the GC-22 gate (compilation performs no IO and
currently rejects non-EDN only indirectly). Neither decision is in scope
for a mechanical cleanup. The realistic fix, when someone takes it, is to
**add the missing `vector?`-equivalent normalization and a typed error at
the `deployment-id` boundary** rather than to swap in the owner.

---

## `evoclj.mcp.manager` — fixed (was a real defect)

`manager`'s private `canonical-edn` was **not** a deliberate variant — it
was a stale copy that had lost the `vector?` branch entirely, so a
vector's *contents* were never normalized. Two logically identical
transport configs whose secret sat inside a vector therefore produced
**different pool keys**, violating the identity guarantee its own
docstring states (*"identical configs -> identical identities"*, INV-01).

Fixed by delegating to `hash/digest`. Guarded by
`guard-vector-contents-canonicalize-before-hashing` and
`guard-set-contents-canonicalize-before-hashing` in
`test/evoclj/mcp/manager_identity_test.clj`.

The lesson this file exists to preserve: **the difference between a
deliberate variant and a stale copy is a test.** A copy with no test
asserting *why* it differs is a copy that will silently rot.

---

## Quick decision table

| namespace | shape | may it delegate to `hash/*`? |
| --- | --- | --- |
| `genome.hash` | owner | — |
| `store.candidate_store` | superset (VerifiedDigest) | no — extra branch is load-bearing |
| `evolution.dag` | name-order + provenance strip | no — contract requires both |
| `genome.patch_edn` | text writer | no — different output type |
| `compiler.resolution` | validated + closed | no — GC-22 gate would be lost |
| `compiler.core` | stale-ish, unreachable-in-practice | **open item**, needs a decision |
| `mcp.manager` | was stale, now delegates | yes — done |
| `runtime.regression` | was stale, now delegates | yes — done |
