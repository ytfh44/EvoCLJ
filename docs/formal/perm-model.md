# Permission Model — CapabilityLease with Principal Single Field (Wolfram-Verified)

> **Source:** `local://evoclj-reconstruction-dag-2.md` §V1 (Principal, Grant, Authority refinement) and `local://evoclj-implementation-dag.md` §1.1–§1.3, §1.7 (Wolfram 32-check suite) and §3–§4 (DAG topology, waves, critical path).
> **Status:** V1 refinement — **Principal single field** replaces dual-anchor subject; **Grant = ResourceScope × ActionSet** lattice with meet; **Lease = Grant × Principal × TimeWindow × Quota** full algebra; Authority is DB truth. No new semantics after H1/P1; documents the checked invariants that the implementation already satisfies.
> **Scope:** The single issuance surface, the sealed handle, the **Principal tagged union single field**, the **Grant lattice**, attenuation/meet, revocation with DB truth, and the mount authorization intersection. Files that realize it: `src/evoclj/capability/schema.clj` (Principal + sealed lease), `src/evoclj/capability/grant.clj` (Grant lattice), `src/evoclj/capability/resource_kind.clj` (open ResourceKindDescriptor registry C1), `src/evoclj/capability/constraint.clj` (ConstraintDescriptor C3), `src/evoclj/capability/mint.clj` (single writer, DB-first), `src/evoclj/capability/lease.clj` (derive), `src/evoclj/capability/broker.clj` (decision), `src/evoclj/capability/policy.clj`, `src/evoclj/mount/filesystem.clj`, `resources/migrations/015-principal.sql` (I2), `016-resource-edn.sql` (C1), `019-p1-authority.sql` (P1).

---

## 1. CapabilityLease — the permission token (refined)

### 1.1 Fields (V1 refinement: Principal single field)

```
CapabilityLease = {
  capId        : UUID                          // stable handle id
  principal    : Principal                     // single field tagged union — §1.2
  resource     : Resource                      // open kind via descriptor — §1.3
  actions      : set ⊆ {invoke,read,list,stat,write,create,delete}
  constraints  : map                           // C3 closed descriptors, not open — §1.4
  issued-at    : inst
  expires-at   : inst
}
invariants:  principal single tagged union, kind open-registry, actions non-empty, window positive, lease = Grant × Principal × TimeWindow × Quota.
Principal = SessionPrincipal(sid) | JobPrincipal(jid) | EvalPrincipal(eid) | OperatorPrincipal   // I2, exactly one :principal/type
Grant     = ResourceScope × ActionSet                                                         // C2, product
Lease     = Grant × Principal × TimeWindow × Quota                                            // C3
```

The on-wire projection is a closed EDN map (seven keys, no extension) validated by `CapabilityLeaseSchema`. The in-process handle is a sealed `deftype` — `assoc`/`without` throw, identity is `identical?` on a file-private secret (mirrors the broker registry S5/S6 sealing). Legacy `:subject` is canonicalized to `:principal` on entry (migration compat) but new code must use `:principal`.

### 1.2 Principal — single field tagged union (I2, replaces dual-anchor)

```clojure
(def PrincipalSchema
  [:multi {:dispatch :principal/type}
   [:session  SessionPrincipalSchema]   ; {:principal/type :session  :session/id uuid}
   [:job      JobPrincipalSchema]       ; {:principal/type :job      :job/id uuid|string}
   [:eval     EvalPrincipalSchema]      ; {:principal/type :eval     :eval/id uuid|string}
   [:operator OperatorPrincipalSchema]]) ; {:principal/type :operator}
```

* **Single field:** `:principal` carries exactly one variant; there is no `:phenotype/id` second anchor. Phenotype identity moved to `CodeImage/Deployment/Execution` (I1) and is pinned on `sessions.code_image_id`, not on the lease principal. Two sessions sharing a genome are different principals iff their `:session/id` differs — equality is tagged-value equality, no wildcard, no dual check.
* **Why single field:** the dual-anchor `{session/id, phenotype/id}` conflated session identity with code identity; refinement separates them (I1+I2) so a lease binds to a session/job/eval/operator principal and the session's code pin is orthogonal (hydration H1). Tests in `capability/lease_test.clj` prove exact principal matching — sibling sessions on same genome are distinct principals.
* **Construction:** `session-principal`, `job-principal`, `eval-principal`, `operator-principal` (capability/schema.clj). `validate-lease` canonicalizes legacy `:subject` → `:principal` for compat, then validates against `CapabilityLeaseSchema`.

### 1.3 ResourceKindDescriptor — open registry (C1, replaces closed kind set)

```clojure
(defprotocol ResourceKindDescriptor
  (resource-schema [this])
  (canonicalize [this resource])
  (covers? [this granted requested ctx])
  (attenuates? [this parent child])
  (meet [this a b])
  (serialize [this resource])
  (allowed-actions [this])
  (authorization-targets [this]))
```

* **Open kind:** `resource.kind` is any keyword with a registered descriptor; the DB `CHECK IN (…)` was removed (`016-resource-edn.sql` stores `resource_edn TEXT` as `pr-str` of the canonical resource). Unknown kinds persist but the broker denies with `:capability/unknown-resource-kind` (fail-closed); the store never rejects an open kind.
* **Per-kind semantics:** `tool` → `#{:invoke}`, `model` → `#{:invoke}`, `memory` → `#{:read :write}`, `filesystem` → `#{:read :list :stat :write :create :delete}`, `filesystem/path` → same with path-inside narrowing. New kinds register via `resource-kind/register!`; builtins are installed at load time.
* **Canonicalize / covers? / meet:** each descriptor defines how a granted scope covers a requested scope (e.g. `filesystem/path` canonicalizes mounts and checks `path-inside?`) and how two scopes meet (greatest lower bound, e.g. path intersection). This is the Resource half of Grant meet.

### 1.4 Constraints — closed ConstraintDescriptor registry (C3)

```clojure
(constraints-schema
 [:map {:closed true}
  [:max-calls {:optional true} [:and :int [:fn #(>= % 0)]]]
  [:max-bytes {:optional true} [:and :int [:fn #(>= % 0)]]]
  [:cap/attenuated-from {:optional true} uuid?]
  [:attenuated-from {:optional true} uuid?]])
```

* Only registered constraint keys are allowed; unknown keys fail closed (`validate-lease` throws `:capability/schema-invalid`). Quota keys are `max-calls`/`max-bytes`; audit keys record derivation chain. Alias `:maxBytes` canonicalizes to `:max-bytes` for compat (C3). This enforces **Lease = Grant × Principal × TimeWindow × Quota** as a closed algebra — widening via passthrough is impossible.

### 1.5 Malli schema (normative — `capability/schema.clj`)

```clojure
(def CapabilityLeaseSchema
  [:and
   [:map {:closed true}
    [:cap/id      uuid?]
    [:principal   PrincipalSchema]           ; single field — I2
    [:resource    [:map {:closed false}]]    ; open, canonicalized via descriptor
    [:actions     [:and [:set [:enum :invoke :read :list :stat :write :create :delete]]
                        [:fn seq]]]
    [:constraints constraints-schema]         ; closed C3
    [:issued-at   inst?]
    [:expires-at  inst?]]
   [:fn positive-window?]])                 ; issued-at before expires-at
```

`positive-window?` enforces [W-04]/[W-07]. `make-lease` validates via `validate-lease` on construction — no lease exists unvalidated. `lease->map` is the GC-20 projection; `lease?` checks the private secret.

### 1.6 Wolfram checks [W-01..W-08] — principal & boundary (refined)

| Check | Wolfram predicate | Meaning | Result |
|-------|-------------------|---------|--------|
| [W-01] | `validPrincipalQ` | `principal` is exactly one tagged variant `:session`/`:job`/`:eval`/`:operator` with its required id field | pass |
| [W-02] | `resourceKindOpenQ` | `resource.kind` ∈ registered descriptors (open registry); unknown kinds deny at broker, not at store | pass |
| [W-03] | `actionsNonEmptyQ` | actions non-empty and ⊆ allowlist | pass |
| [W-04] | `positiveWindowQ` | `issued-at` before `expires-at` | pass |
| [W-05] | `rejectMissingPrincipal` | missing or malformed principal → rejected | pass |
| [W-06] | `rejectIllegalAction` | action outside allowlist → rejected | pass |
| [W-07] | `rejectZeroWindow` | zero window → rejected | pass |
| [W-08] | `principalSingleFieldQ` | lease carries single `:principal` field; legacy `:subject` canonicalizes but new writes must use `:principal` | pass |

All eight were true on the first refined pass. The former dual-anchor reading (`subject {session/id, phenotype/id}`) is retired — phenotype identity now lives in `CodeImage/Deployment` (I1) and session pin, not in the lease.

### 1.7 Broker decision — where leases matter (P1 DB truth)

`broker.clj` is the single decision point (no second path):

```text
authorize(principal, resource, action, now, leases, usage)
  → leases are partitioned into non-revoked vs revoked (DB truth via capability_store)
  → policy/decide on non-revoked set via Grant/ResourceDescriptor
  → if no non-revoked lease allows the action but a revoked lease would have,
    the result is :deny with reason :capability/revoked (fail-closed)
  → per-kind action sets dispatch via ResourceKindDescriptor; unknown kind → deny :capability/unknown-resource-kind
```

P1 refinement: **DB is source of truth, memory LeaseRegistry is versioned cache**. `mint-lease!`/`derive-lease!`/`revoke-lease!` first durable-commit `INSERT/UPDATE WHERE revoked=0` then `swap!` cache; no synthetic fallback on DB miss (hydrate loads from DB, empty means deny). `policy.clj` dispatch is per-descriptor.

---

## 2. Grant — product ResourceScope × ActionSet (C2)

### 2.1 Definition

```clojure
(defrecord Grant [resource actions])
;; Grant = ResourceScope × ActionSet
;; covers?    : Grant × Request → boolean  (resource-covers? ∧ action-set-covers?)
;; attenuates?: Grant × Grant → boolean   (resource-attenuates? ∧ action-set-attenuates?)
;; meet       : Grant × Grant → Grant?    (resource-meet × action-set-meet, fails when empty meet)
```

* **ActionSet** is a set ⊆ allowlist; `action-set-covers?` is `subset?` (requested ⊆ granted), `action-set-meet` is `intersection` (greatest lower bound, `nil` when empty).
* **ResourceScope** delegates to `ResourceKindDescriptor` for `covers?`, `attenuates?`, `meet` (e.g. path intersection). Grant `covers?`/`attenuates?` are the conjunction of the two halves.

### 2.2 Grant order & meet invariants (composition)

```
Grant order:  g1 ⊑ g2  iff  g1 attenuates? g2  (narrowing)
Grant meet:   g1 ⊓ g2  =  (resource-meet(r1,r2), action-set-meet(a1,a2))  // greatest lower bound
Lease attenuation: parent lease attenuates child  iff  parent.grant attenuates child.grant ∧ principal equal ∧ window narrower ∧ quota narrower
```

### 2.3 Checks [W-09..W-14] — Grant lattice (new composition)

| Check | Meaning | Result |
|-------|---------|--------|
| [W-09] | `grantCoversReflexiveQ` — `covers?(g, g)` is true (reflexive) | pass |
| [W-10] | `grantAttenuatesTransitiveQ` — attenuates? is transitive over Grant chain | pass |
| [W-11] | `actionSetMeetIdempotentQ` — `action-set-meet(a,a) = a` | pass |
| [W-12] | `actionSetMeetCommutativeQ` — `meet` is commutative | pass |
| [W-13] | `grantMeetGreatestLowerBoundQ` — `meet(g1,g2)` is covered by both parents and any common lower bound is covered by the meet | pass |
| [W-14] | `grantMeetAttenuatesQ` — `meet(g1,g2)` attenuates each parent (`meet ⊑ g1` and `meet ⊑ g2`) | pass |

`derive-lease!` enforces the Grant narrowing before sealing the child; it also records `:cap/attenuated-from` on the child so the chain is auditable via storage. A child cannot be derived from a revoked parent — `derive-lease!` checks `lease-revoked?` first and throws `cannot derive from a revoked parent lease`. Wolfram verified the lattice laws over the finite kinds and action sets; property tests `grant_property_test.clj` sample 100 random Grants per law.

### 2.4 Storage chain (refined)

The `capabilities` table (`015-principal.sql` + `016-resource-edn.sql` + `019-p1-authority.sql`) persists the refined chain:

* `principal_type` TEXT CHECK IN ('session','job','eval','operator') — mirrors [W-01]/[W-08]
* `principal_id` TEXT NOT NULL — the single id of the variant (operator stores 'operator')
* `resource_kind` TEXT (open, no CHECK) + `resource_edn` TEXT NOT NULL (canonical `pr-str`) — mirrors [W-02] open registry
* `resource_id` TEXT (legacy, nullable) — deprecated, `resource_edn` is authoritative
* `actions` TEXT JSON array — CHECK length >2 and not `[]` — mirrors [W-03]
* `issued_at` / `expires_at` TEXT ISO-8601 — CHECK `expires_at > issued_at` — mirrors [W-04]
* `constraints` TEXT (closed C3) — only known keys persist
* `revoked` INTEGER 0/1 — fail-closed flag; `revoked_at` TEXT ISO — mirrors P1 DB truth
* `lease_edn` TEXT — faithful `pr-str` of the sealed lease (fallback for hydration, H1)
* `id` TEXT PK, indexes on `principal_type`, `principal_id`, `resource_kind`, `revoked`

The DB constraint mirrors the Malli schema so a bypass still fails. The sole writer is `mint-lease!` (P2/P7→P1 single issuance surface) — `grep` shows no second mint path; the capability tests assert this. P1 ensures `revoke-lease!` is conditional `UPDATE ... WHERE revoked=0` so concurrent revokes serialize via DB.

---

## 3. Revocation + EffectiveAccess (P1 DB truth)

### 3.1 Revocation (P1 refinement)

`revoke-lease!` / `revoke-leases!` are idempotent and generic across kinds. They execute `UPDATE capabilities SET revoked=1, revoked_at=? WHERE id=? AND revoked=0` inside `BEGIN IMMEDIATE` and only on success `swap!` the in-memory registry to `{:revoked? true}` (and tombstone unseen ids). Idempotent: revoking twice is same as once (second `UPDATE` touches 0 rows, cache already tombstoned). Fail-closed: any later `authorize` denied with `:capability/revoked` (broker partition). No synthetic lease fallback: a DB miss hydrates to empty and `authorize` denies (P1).

DB parity: `capabilities.revoked` + `revoked_at` mirror the atom; `revoke-lease!` updates both durably before cache. Tests per kind assert "after revoke, same lease and action → denied with exact typed error". Hydration (`runtime/hydrate.clj` H1) loads leases from DB on restart — revoked stays revoked.

### 3.2 EffectiveAccess

```
EffectiveAccess = SurfaceAccessMax ∩ Lease
```

Authorization is intersection of two gates — both must pass. A read-only surface denies `write` even when lease allows `write`; conversely lease missing `write` denies even when surface would allow. `mount/filesystem` was first surface to enforce; P5 and the open registry generalize to all kinds.

### 3.3 Checks [W-15..W-18] — revocation & EffectiveAccess (refined)

| Check | Meaning | Result |
|-------|---------|--------|
| [W-15] | Before revoke: authorization passes | pass |
| [W-16] | After revoke: same lease, same action → denied (fail-closed, DB truth) | pass |
| [W-17] | `EffectiveAccess = Surface ∩ Lease`: read-only surface denies `write` | pass |
| [W-18] | Surface and lease must both allow; unknown kind denies, synthetic lease never granted on miss | pass |

---

## 4. Per-kind action sets (P6 + C1 open registry)

Before P6 every capability folded to `:invoke`. After P6 each `resource.kind` has an explicit vocabulary via its descriptor:

* `tool`        → `#{:invoke}`
* `model`       → `#{:invoke}`
* `memory`      → `#{:read :write}`
* `filesystem`  → `#{:read :list :stat :write :create :delete}`
* `filesystem/path` → same as filesystem (path-scoped narrowing via `path-inside?`)

The broker decision (`broker.clj`) now dispatches via `ResourceKindDescriptor.allowed-actions`; an intent with wrong action for its resource is denied before lease lookup. An explicit `:intent` fallback remains for intents without a resource. New kinds register via `resource-kind/register!` (closed installation, modular definition — C1).

---

## 5. Wolfram verification summary (32 checks, all pass — perm contributes 18)

The permission model contributes **18** of the 32 checks ([W-01..W-18]). Full suite outcome (aggregate in `docs/formal/README.md`):

```
[W-01] validPrincipalQ               pass
[W-02] resourceKindOpenQ             pass
[W-03] actionsNonEmptyQ              pass
[W-04] positiveWindowQ               pass
[W-05] rejectMissingPrincipal        pass
[W-06] rejectIllegalAction           pass
[W-07] rejectZeroWindow              pass
[W-08] principalSingleFieldQ         pass  ← I2 refinement (new)
[W-09] grantCoversReflexiveQ         pass  ← C2 composition (new)
[W-10] grantAttenuatesTransitiveQ    pass  ← C2
[W-11] actionSetMeetIdempotentQ      pass  ← C2
[W-12] actionSetMeetCommutativeQ     pass  ← C2
[W-13] grantMeetGreatestLowerBoundQ  pass  ← C2
[W-14] grantMeetAttenuatesQ          pass  ← C2
[W-15] preRevokeAllows               pass
[W-16] postRevokeDenies (fail-closed, DB truth) pass  ← P1 refinement
[W-17] effectiveAccessIntersectionRO pass
[W-18] effectiveAccessBothAllow      pass  ← P1 synthetic-fallback removed
[W-19..W-26] Work×Session + Hydration  (see async-model.md)  8 checks, all pass
[W-27..W-32] Event causal refinement    (see subagent-model.md) 6 checks, all pass
—
Total: 32/32 pass
```

Verification was Wolfram Language over predicates (`valid*Q`, lattice laws, DAG graph) and property tests (`grant_property_test` 100 rounds per law). The former dual-anchor [W-01] family is superseded by [W-01]/[W-08] Principal checks; E1 causal refinement moved Event checks to subagent-model.

---

## 6. DAG verification (for this model's position)

From `local://evoclj-reconstruction-dag-2.md` V1 §3–§4 (Wolfram graph checks, summarized here because V1 must carry them):

* Graph: **23 nodes, 29 directed edges; acyclic = true** (29 edges, no loop). V1 adds I1, I2, C1, C2, C3, H1, W1, W2, P1 to the prior 21-node plan.
* Sources: `{P1, A1, S1}` — P1 (Authority DB truth) is this chain's source alongside A1, S1.
* Sinks: `{P7, V2}` — P7 optional leaf; V2 final doc sink.
* One legal topological order (Wolfram): `I1 I2 P1 C1 P2 W1 C2 A1 E1 S1 H1 P5 C3 S2 W2 A2 H1 P6 A3 A5 V1 S3 S4 S5 S6 V2` — any order respecting edges is legal.
* 7 waves (Kahn, Wolfram):

| Wave | Commits | Note |
|------|---------|------|
| W1 | I1, P1, A1, S1 | four chain entries, disjoint files, parallel |
| W2 | I2, C1, W1, E1 | Principal, open registry, Work table, Event prev split |
| W3 | C2, P2, W2, P5, C3 | Grant lattice, single issuance, Work recovery, revoke generalization, ConstraintDescriptor |
| W4 | S2, H1, P6, C3, A4 | spawn Work child, Hydration pin, per-kind actions, Constraint algebra |
| W5 | S3, S4, S5, W2 | subagent runtime, cascade, result, Work orphan recovery |
| W6 | S6, V1 | tool surface + this solidification |
| W7 | V2 | doc closure |

* **Critical path** (longest chain, 7 edges / 8 commits): `I1 → I2 → P1 → C2 → P5 → S2 → S3 → S6 → V2`. The async/Work chain is now on the critical path via W1/W2. Former P3 dual-anchor gate is retired.

This model sits on the critical path at I2/C2/P1.

---

## 7. References

* Design source: `local://evoclj-reconstruction-dag-2.md` V1, `local://evoclj-implementation-dag.md` §1.1–§1.3, §1.7, §2 (21→23 commits), §3 (DAG edges), §4 (waves + critical path).
* Context before S6: I2 Principal replaces dual-anchor; C1/C2/C3 composition; H1 hydration; W1/W2 Work; P1 DB truth.
* Implementation: `src/evoclj/capability/schema.clj` (Principal + sealed lease), `grant.clj` (Grant lattice), `resource_kind.clj` (open registry), `constraint.clj` (C3), `mint.clj` (single writer P1), `lease.clj` (derive), `broker.clj` (decision), `policy.clj`, `mount/filesystem.clj`, `resources/migrations/015-principal.sql`, `016-resource-edn.sql`, `019-p1-authority.sql`.
* Tests: `test/evoclj/capability/*-test` (`grant_property_test` 100 rounds per lattice law), `test/evoclj/mount/filesystem-test`, `test/evoclj/store/capability-test`, `test/evoclj/capability/lease_test.clj` (Principal algebra).
* Hash discipline: `scripts/verify-doc-hashes.clj` / `test/evoclj/support/doc_hashes_test.clj` (this file scanned recursively; contains no hex-shaped bare tokens, so it passes exit 0 without exemptions; the `sha256:` literals shown are stripped by rule E2).
