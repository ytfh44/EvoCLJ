# Permission Model — CapabilityLease Boundary (Wolfram-Verified)

> **Source:** `local://evoclj-implementation-dag.md` §1.1–§1.3, §1.7 (Wolfram object-boundary models, 26 checks) and §3–§4 (DAG topology, waves, critical path).
> **Status:** V1 solidification — models frozen after S6. No new semantics; documents the checked invariants that the implementation already satisfies.
> **Scope:** The single issuance surface, the sealed handle, the dual-anchor subject, attenuation, revocation, and the mount authorization intersection. Files that realize it: `src/evoclj/capability/schema.clj`, `src/evoclj/capability/mint.clj`, `src/evoclj/capability/lease.clj`, `src/evoclj/capability/broker.clj`, `src/evoclj/capability/policy.clj`, `src/evoclj/mount/filesystem.clj`, `resources/migrations/013-capabilities.sql`.

---

## 1. CapabilityLease — the permission token

### 1.1 Fields

```
CapabilityLease = {
  capId        : UUID                          // stable handle id
  subject      : { session/id, phenotype/id }  // dual-anchor — §1.1 subject
  resource     : { kind, id }                  // kind ∈ closed vocabulary (§1.1)
  actions      : set ⊆ {invoke,read,list,stat,write,create,delete}
  constraints  : map                           // provider-defined, closed-top is open
  issued-at    : inst
  expires-at   : inst
}
invariants:  subject complete, kind closed, actions non-empty, window positive.
```

The on-wire projection is a closed EDN map (seven keys, no extension) validated by `CapabilityLeaseSchema`. The in-process handle is a sealed `deftype` — `assoc`/`without` throw, identity is `identical?` on a file-private secret (mirrors the broker registry S5/S6 sealing).

### 1.2 Malli schema (normative — `capability/schema.clj`)

```clojure
(def SubjectSchema
  [:map {:closed true}
   [:session/id   [:or uuid? [:string {:min 1}]]]
   [:phenotype/id PhenotypeIdSchema]])   ; sha256: content hash

(def CapabilityLeaseSchema
  [:and
   [:map {:closed true}
    [:cap/id      uuid?]
    [:subject     SubjectSchema]
    [:resource    [:map {:closed false}]]
    [:actions     [:and [:set [:enum :invoke :read :list :stat :write :create :delete]]
                        [:fn seq]]]       ; non-empty
    [:constraints [:map {:closed false}]]
    [:issued-at   inst?]
    [:expires-at  inst?]]
   [:fn positive-window?]])              ; issued-at before expires-at
```

`positive-window?` is the single predicate that enforces `[W-04]` / `[W-07]`. `make-lease` calls `validate-lease` on construction — no lease exists unvalidated. `lease->map` is the GC-20 event-log projection; `lease?` checks the private secret.

### 1.3 Wolfram checks [W-01..W-07] — boundary table

| Check | Wolfram predicate | Meaning | Result |
|-------|-------------------|---------|--------|
| [W-01] | `validSubjectQ` | subject must carry both `session/id` and `phenotype/id` (dual-anchor) | pass |
| [W-02] | `resourceKindQ` | `resource.kind` ∈ `{tool, model, memory, filesystem, filesystem/path}` | pass |
| [W-03] | `actionsNonEmptyQ` | actions non-empty and ⊆ allowed set | pass |
| [W-04] | `positiveWindowQ` | `issued-at` before `expires-at` (positive window) | pass |
| [W-05] | `rejectMissingPhenotype` | missing phenotype → rejected (`validate-lease` throws) | pass |
| [W-06] | `rejectIllegalAction` | action outside allowlist → rejected | pass |
| [W-07] | `rejectZeroWindow` | zero window → rejected | pass |

All seven were true on the first modeling pass. The model initially described `[W-01]` as "subject present"; Wolfram forced the stronger dual-anchor reading — a single session id is insufficient because sibling sessions sharing a genome would otherwise alias.

### 1.4 Subject dual-anchor — why two keys

The `subject` is `{ :session/id, :phenotype/id }` where `:phenotype/id` is a `sha256:` content hash of the deployed genome. Two sessions running the same genome are **different subjects** — `subject-matches?` requires both keys equal. This is the isolation criterion that the P3 change proved with the "sibling genome" test (shares genome, still distinct subject). Any single-key subject would allow a child to inherit a lease minted for its parent without explicit derivation, violating the downward-closed attenuation chain.

### 1.5 Broker decision — where leases matter

`broker.clj` is the single decision point (no second path):

```text
authorize(subject, resource, action, now, leases, usage)
  → leases are partitioned into non-revoked vs revoked (via the lease registry)
  → policy/decide on non-revoked set
  → if no non-revoked lease allows the action but a revoked lease would have,
    the result is :deny with reason :capability/revoked (fail-closed)
  → per-kind action sets replace the former :invoke collapse (P6)
```

`policy.clj` holds the per-kind action tables; unknown actions are denied without consulting revocation (no false attribution).

---

## 2. Derivation / Attenuation — narrowing only

### 2.1 Rule

```
clear(parent, child) =
  actionsᶜ ⊆ actionsᵖ
  ∧ maxCallsᶜ ≤ maxCallsᵖ          ; when :cap/max-calls is present
  ∧ issuedᶜ  ≥ issuedᵖ
  ∧ expiresᶜ ≤ expiresᵖ
```

### 2.2 Checks [W-08..W-11]

| Check | Meaning | Result |
|-------|---------|--------|
| [W-08] | Narrowing derivation (subset actions, smaller count, shorter window) → allowed | pass |
| [W-09] | Expanding the action set → rejected | pass |
| [W-10] | Extending expiry → rejected | pass |
| [W-11] | Downward closure: a parent's grant ⊇ every reachable child (transitive attenuation chain) | pass |

`derive-lease!` (`capability/mint.clj` and `lease.clj`) enforces the rule before sealing the child; it also records `:cap/attenuated-from` on the child so the chain is auditable via storage. A child cannot be derived from a revoked parent — `derive-lease!` checks `lease-revoked?` first and throws `cannot derive from a revoked parent lease`.

### 2.3 Storage chain

The `capabilities` table (`013-capabilities.sql`) persists the chain fields:

* `subject_session_id` TEXT FK → sessions (dual-anchor half)
* `subject_phenotype_id` TEXT (second anchor half, no FK — phenotype is a content hash)
* `resource_kind` CHECK IN closed set — mirrors `[W-02]`
* `actions` TEXT JSON array — CHECK length > 2 and not `[]` — mirrors `[W-03]`
* `issued_at` / `expires_at` TEXT ISO-8601 — CHECK `expires_at > issued_at` — mirrors `[W-04]`
* `revoked` INTEGER 0/1 — fail-closed flag
* `id` TEXT PK, indexes on `subject_session_id`, `resource_kind`, `revoked`

The DB constraint mirrors the Malli schema so a bypass of the code still fails. The sole writer is `mint-lease!` (P2/P7 single issuance surface) — `grep` shows no second mint path; the capability tests assert this.

---

## 3. Revocation + EffectiveAccess

### 3.1 Revocation

`revoke-lease!` / `revoke-leases!` are idempotent and generic across kinds (P5 generalized the former filesystem-only path). Internally they set `{:lease … :revoked? true}` in the lease registry atom (and tombstone an unseen id). Idempotent: revoking twice is the same as once. Fail-closed: any later `authorize` that would have been allowed by that lease is denied with `:capability/revoked` (broker §1.5 partition logic).

DB parity: the `capabilities.revoked` column mirrors the atom; `revoke-lease!` updates both so a restart does not lose revocation. Tests per kind assert "after revoke, same lease and action → denied with exact typed error".

### 3.2 EffectiveAccess

```
EffectiveAccess = SurfaceAccessMax ∩ Lease
```

Authorization is the intersection of two independent gates — both must pass. A read-only surface denies `write` even when the lease allows `write`; conversely a lease missing `write` denies `write` even when the surface would allow it. `mount/filesystem` was the first surface to enforce the intersection; P5 and the `capabilities` table generalize it to tool / model / memory.

### 3.3 Checks [W-12..W-15]

| Check | Meaning | Result |
|-------|---------|--------|
| [W-12] | Before revoke: authorization passes | pass |
| [W-13] | After revoke: same lease, same action → denied (fail-closed) | pass |
| [W-14] | `EffectiveAccess = Surface ∩ Lease`: read-only surface denies `write` | pass |
| [W-15] | Surface and lease must both allow | pass |

---

## 4. Model discovery — seq continuity fix (Wolfram finding)

While modeling `EventChain` (§1.6 in the DAG), Wolfram exposed that the original phrasing of [W-25] — "seq values are the multiset `{1..M}`" — was insufficient. A payload `{1,3,2}` passes a sorted-set equality check yet violates positional continuity: the third event claims to be second. The correct invariant (now enforced by `store/event.clj` and locked by an assertion test) is:

> **seq must be positionally continuous `1..M`**, i.e. `events[i].seq = i+1` for the session-ordered list.

The earlier `store/event append-event!` allocated `max(seq)+1` so it was constructively continuous, but the invariant was not asserted — a future rewrite could have broken it silently. This was the only one of the 26 checks whose first formulation failed; after rephrasing [W-25] to the positional form it passed. All other 25 passed on the first pass.

---

## 5. Per-kind action sets (P6) — from the former `:invoke` collapse

Before P6 every capability folded to `:invoke`. After P6 each `resource.kind` has an explicit action vocabulary:

* `tool`        → `#{:invoke}`
* `model`       → `#{:invoke}`
* `memory`      → `#{:read :write}`
* `filesystem`  → `#{:read :list :stat :write :create :delete}`
* `filesystem/path` → same as filesystem (path-scoped narrowing)

The broker decision (`broker.clj`) now dispatches on kind; an intent that carries the wrong action for its resource is denied before lease lookup (no lease is consulted). An explicit `:intent` fallback remains for intents that do not name a resource.

---

## 6. Wolfram verification summary (26 checks, all pass)

The permission model contributes 15 of the 26 checks ([W-01..W-15]). Full suite outcome:

```
[W-01] validSubjectQ                  pass
[W-02] resourceKindQ                  pass
[W-03] actionsNonEmptyQ               pass
[W-04] positiveWindowQ                pass
[W-05] rejectMissingPhenotype         pass
[W-06] rejectIllegalAction            pass
[W-07] rejectZeroWindow               pass
[W-08] narrowDerivation               pass
[W-09] rejectExpandActions            pass
[W-10] rejectExtendExpiry             pass
[W-11] downwardClosed                 pass
[W-12] preRevokeAllows                pass
[W-13] postRevokeDenies (fail-closed) pass
[W-14] effectiveAccessIntersectionRO  pass
[W-15] effectiveAccessBothAllow       pass
[W-16..W-19]  SubAgentSession  (see subagent-model.md)   4 checks, all pass
[W-20..W-24]  AsyncCommand     (see async-model.md)      5 checks, all pass
[W-25..W-27]  EventChain       (see async-model.md)      3 checks, all pass
—
Total: 26/26 pass (first pass 25/26; [W-25] rephrased to positional form, then pass)
```

Verification was done in Wolfram Language over the object-boundary predicates (`valid*Q`) and the DAG graph (acyclicity, topological order, waves, critical path — see §7). The checks are reproduced as assertions in the capability test suites (`capability/*-test`, `mount/filesystem-test`, `store/capability-test`).

---

## 7. DAG verification (for this model's position)

From `local://evoclj-implementation-dag.md` §3–§4 (Wolfram graph checks, summarized here because V1 must carry them):

* Graph: 21 nodes, 25 directed edges; **acyclic = true** (25 edges, no loop).
* Sources (no predecessors, may start first): `{P1, A1, S1}`.
* Sinks (no successors): `{P7, V2}` — P7 is an optional leaf and does not gate anything.
* One legal topological order: `S1 P1 P2 P7 P5 P3 S2 S5 S4 S3 S6 P6 P4 A1 A2 A5 V1 A6 V2 A3 A4` (Wolfram-computed; any legal order respects the edges above).
* 7 waves (Kahn, Wolfram-computed):

| Wave | Commits | Note |
|------|---------|------|
| W1 | P1, A1, S1 | three chain entries, disjoint files, parallel |
| W2 | A2, P2 |  |
| W3 | A3, A5, P3, P5, P7 | widest wave (five) |
| W4 | A4, A6, P4, P6, S2 | S2 gates on P3 |
| W5 | S3, S4, S5 | subagent body |
| W6 | S6, V1 | tool surface + this solidification |
| W7 | V2 | doc closure |

* **Critical path** (longest chain, 6 edges / 7 commits): `P1 → P2 → P3 → S2 → S3 → S6 → V2`. The async chain (A-series) is off the critical path — it runs alongside the permission→subagent chain without adding depth. 7 waves equals the critical-path length, hence no redundant waiting.

This model sits on the critical path at P1..P3 and P5/P7 (storage). V1 itself is W6 — it solidifies what P-series proved.

---

## 8. References

* Design source: `local://evoclj-implementation-dag.md` §1.1–§1.3, §1.7, §2 (21 commits), §3 (DAG edges), §4 (waves + critical path).
* Context before S6: `local://wave1-context.md` (GC-01..24, INV-01..09, wave-1 leaves P1/A1/S1 discipline).
* Implementation: `src/evoclj/capability/schema.clj` (lease shape + sealing), `mint.clj` (single writer + revoke), `lease.clj` (derive + revoke generalization), `broker.clj` (decision), `policy.clj` (per-kind actions), `mount/filesystem.clj` (surface ∩ lease), `resources/migrations/013-capabilities.sql` (DB mirror).
* Tests: `test/evoclj/capability/*-test`, `test/evoclj/mount/filesystem-test`, `test/evoclj/store/capability-test`.
* Hash discipline: `scripts/verify-doc-hashes.clj` / `test/evoclj/support/doc_hashes_test.clj` (this file is scanned recursively under `docs/`; it carries no hex-shaped bare tokens, so it passes exit 0 without exemptions).
