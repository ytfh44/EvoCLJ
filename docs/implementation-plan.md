# EvoCLJ Self-Evolving Agent Implementation Plan

**Goal:** Build a JVM-Clojure self-evolving agent runtime in which immutable, content-addressed Genomes are compiled into isolated Phenotypes; Phenotypes may produce structured successor mutations but may not mutate themselves; all external effects cross a kernel-owned capability broker; every successor must pass isolated evaluation before atomic promotion.

**Architecture:** The trusted kernel owns Genome loading/compilation, SCI execution boundaries, capability enforcement, event/audit persistence, evaluation isolation, and promotion. A Genome is mostly EDN plus bounded SCI Clojure programs and text assets. Every live session is pinned to one `(Genome, Resolution)` pair. Evolution creates new candidate Genomes by deterministic structured patches; candidates run in isolated workspaces and can only become current through a compare-and-set promotion transaction.

**Tech Stack:** JVM Clojure; EDN; Integrant for host-system lifecycle; Malli for runtime schemas; Babashka SCI for restricted evolvable programs; rewrite-clj for source-preserving structural patches; SQLite JDBC for durable metadata/event state; filesystem content-addressed storage for large immutable artifacts; `clojure.test` plus property/integration/adversarial test layers.

## Global Constraints

1. `Genome` MUST be immutable and content-addressed.
2. Every session MUST be pinned to one Genome ID and one Resolution ID for its whole lifetime.
3. A live Phenotype MUST NOT modify its own Genome in place.
4. Evolution MUST produce successor candidates through structured, deterministic mutations.
5. Every mutation MUST identify its parent Genome, evidence, hypothesis, risk class, operations, and expected effect.
6. Mutation application MUST be deterministic: same parent bytes plus same mutation value yields the same candidate hash.
7. Evolvable SCI code MUST execute without ambient JVM, filesystem, process, network, secret, or database authority.
8. All external effects MUST cross the kernel-owned Intent/Capability Broker.
9. Adding a visible action/tool MUST NOT itself grant resource authority.
10. Episodic memory writes MUST remain distinct from procedural Genome changes.
11. Candidate evaluation MUST use an informationally isolated selection set unavailable to Executor, Diagnostician, and Mutator.
12. A candidate MUST NOT modify the evaluator that judges that same candidate.
13. Parent and candidate SHOULD be evaluated as a paired comparison on the same case set and environment fixture.
14. Hard safety, integrity, and policy constraints MUST dominate utility/cost metrics; they MUST NOT be collapsed into a compensating weighted score.
15. Promotion MUST be an atomic compare-and-set against the parent generation/current pointer.
16. Rejected mutations MUST remain durable, queryable negative evidence.
17. Every promoted generation MUST retain complete lineage: parent, mutation, evidence, evaluation, decision, and deployment state.
18. Rollback MUST restore future generation selection only; it MUST NOT claim to reverse already-committed external effects.
19. Kernel source, authority root, audit root, evaluator-isolation root, and promotion root MUST NOT be agent-mutable.
20. Every externally visible effect MUST be attributable to `session-id`, `phenotype-id`, `node-id`, `intent-id`, authorization decision, and outcome.
21. All large immutable payloads MUST be stored by content hash; SQLite rows SHOULD store references rather than duplicated payload bodies.
22. All public module boundaries MUST use validated Clojure data; raw Java objects, lazy sequences, futures, and open resources MUST NOT cross Genome/SCI/Intent/Event boundaries.
23. Candidate evaluation workspaces, SCI contexts, session namespaces, and mutable temporary state MUST be isolated from the current production generation.
24. The first implementation MUST prefer YAGNI: no model-weight training, arbitrary JVM `eval`, arbitrary generated native code, persistent schema self-migration, automatic capability enlargement, or simultaneous evaluator/candidate co-evolution.

---

## 0. Dependency Order and Delivery Definition

The required dependency chain is:

```text
Genome
  ↓
Compiler
  ↓
SCI Runtime
  ↓
Intent / Capability Broker
  ↓
Event Store
  ↓
Executor
  ↓
Evolution
  ↓
Evaluator
  ↓
Promotion
```

A later milestone may use only stable interfaces produced by earlier milestones. Do not shortcut a later subsystem by reaching into an earlier subsystem's internals.

The first complete release is done only when this scenario passes end to end:

1. Load immutable seed Genome `G1`.
2. Compile `G1` into a `CompiledGenome`.
3. Instantiate a Phenotype with a restricted SCI router.
4. Execute a deterministic fixture task through typed Intents and a capability-scoped tool.
5. Persist its episode and all effect/audit events.
6. Build an evidence pack from several episodes.
7. Produce one bounded mutation that changes only an evolvable asset.
8. Materialize candidate `G2` deterministically.
9. Evaluate `G1` and `G2` on hidden selection fixtures in isolated sessions.
10. Reject `G2` if any hard gate fails; otherwise promote only if the configured lexicographic comparison passes.
11. Atomically change `CURRENT` from `G1` to `G2` only if `CURRENT == G1` at commit time.
12. Start a new session pinned to `G2`; an existing `G1` session remains pinned to `G1`.
13. Query lineage and reconstruct exactly why `G2` was or was not promoted.

### Top-level source map

```text
src/evoclj/
├── kernel/        ; host lifecycle and invariant helpers
├── genome/        ; immutable Genome values, hashing, loading, patches
├── compiler/      ; validation, Resolution, topology/program compilation
├── sci/           ; restricted evolvable Clojure execution
├── intent/        ; typed effect requests
├── capability/    ; leases, policy, authorization
├── provider/      ; model/tool/filesystem/process adapters
├── store/         ; SQLite, CAS, append-only events
├── runtime/       ; sessions, scheduler, node execution, episodes
├── evolution/     ; evidence, diagnosis, mutation, candidate search
├── eval/          ; gates, paired runners, metrics, comparison
├── promotion/     ; candidate state, CAS activation, canary, rollback
└── cli/           ; operator entry points
```

### Test map

```text
test/evoclj/
├── genome/
├── compiler/
├── sci/
├── intent/
├── capability/
├── store/
├── runtime/
├── evolution/
├── eval/
├── promotion/
└── adversarial/
```

Use deterministic fixture directories under `test/fixtures/`; do not put hidden selection fixtures in a path mounted into candidate execution workspaces.

---

# Milestone 1 — Genome

**Milestone outcome:** A Genome can be loaded from disk, validated as pure serializable data/assets, canonicalized, hashed, inspected, and rejected for forbidden paths or non-deterministic structure. No execution exists yet.

## Task 1.1 — Establish domain values, error contract, and project skeleton

**Files:**

- Create: `src/evoclj/kernel/error.clj`
- Create: `src/evoclj/genome/types.clj`
- Create: `test/evoclj/kernel/error_test.clj`
- Create: `test/evoclj/genome/types_test.clj`
- Create/Modify: `deps.edn`

**Interfaces:**

- Produces: `(evoclj.kernel.error/error type message data)` returning `ExceptionInfo` with stable `:error/type`.
- Produces: `(evoclj.kernel.error/error-data throwable)` returning serializable error data.
- Produces predicates/value constructors for IDs: `genome-id?`, `resolution-id?`, `session-id?`, `intent-id?`, `artifact-id?`.
- All later modules depend on the error namespace and ID conventions.

- [ ] **Step 1: Write failing tests for typed errors and IDs.**

```clojure
(deftest typed-error-is-data-readable
  (let [e (err/error :genome/invalid "bad genome" {:path "manifest.edn"})]
    (is (= :genome/invalid (:error/type (ex-data e))))
    (is (= "manifest.edn" (:path (ex-data e))))))

(deftest genome-id-format
  (is (types/genome-id? "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
  (is (not (types/genome-id? "G42"))))
```

- [ ] **Step 2: Run only these tests and confirm failure because namespaces/functions do not exist.**

```bash
clojure -M:test -n evoclj.kernel.error-test -n evoclj.genome.types-test
```

- [ ] **Step 3: Implement the smallest error and ID helpers.** Do not introduce records yet; IDs remain strings/UUIDs in validated maps so they stay easy to persist and print.

- [ ] **Step 4: Add a serialization test.** `error-data` must not contain a Throwable object, Java class instance, lazy sequence, or function.

- [ ] **Step 5: Run the focused tests, then the full test command.**

- [ ] **Step 6: Commit.**

```bash
git add deps.edn src/evoclj/kernel/error.clj src/evoclj/genome/types.clj test/evoclj/kernel/error_test.clj test/evoclj/genome/types_test.clj
git commit -m "feat: establish EvoCLJ domain error and id contracts"
```

**Acceptance:** Later code can distinguish machine-readable failures by `:error/type` without parsing exception strings.

---

## Task 1.2 — Define Malli schemas for Genome manifest and module descriptors

**Files:**

- Create: `src/evoclj/genome/schema.clj`
- Create: `test/evoclj/genome/schema_test.clj`
- Create fixtures: `test/fixtures/genomes/minimal-valid/manifest.edn`
- Create fixtures: `test/fixtures/genomes/invalid-manifest/manifest.edn`

**Interfaces:**

- Produces: `GenomeManifestSchema`, `ModuleDescriptorSchema`, `CapabilityRequestSchema`.
- Produces: `(validate-manifest x)` → validated manifest or throws `:genome/schema-invalid` with serializable Malli explanation.

Required v1 manifest shape:

```clojure
{:genome/format 1
 :agent/id :main
 :agent/entry :graph/main
 :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
 :modules {:topology "topology.edn"
           :models "models.edn"
           :memory "memory.edn"
           :evolution "evolution.edn"}
 :capabilities/requested #{:model/call}
 :evolution {:max-risk :behavioral
             :mutable #{:parameters :prompts :skills :programs}}
 :metadata {:name "seed-agent"
            :description "minimal fixture"}}
```

- [ ] **Step 1: Write tests for one valid manifest and specific invalid cases:** missing `:genome/format`, wrong ABI value type, absolute module path, unknown risk keyword.
- [ ] **Step 2: Run tests and confirm failure.**
- [ ] **Step 3: Implement schemas with closed maps at trust boundaries.** Reject unknown top-level keys unless explicitly placed inside `:metadata`.
- [ ] **Step 4: Make `validate-manifest` return the input unchanged on success.** Validation must not silently coerce dangerous values.
- [ ] **Step 5: Add a test that the explanation data can be `pr-str`/`edn/read-string` round-tripped.**
- [ ] **Step 6: Run tests and commit.**

```bash
git add src/evoclj/genome/schema.clj test/evoclj/genome/schema_test.clj test/fixtures/genomes
git commit -m "feat: define immutable genome manifest schemas"
```

**Acceptance:** A manifest is a pure EDN contract; invalid or ambiguous structure fails before any file or program is compiled.

---

## Task 1.3 — Canonical path validation and deterministic Genome hashing

**Files:**

- Create: `src/evoclj/genome/path.clj`
- Create: `src/evoclj/genome/hash.clj`
- Create: `test/evoclj/genome/path_test.clj`
- Create: `test/evoclj/genome/hash_test.clj`

**Interfaces:**

- `(normalize-relative-path s)` → canonical slash-separated relative path.
- `(allowed-genome-path? path)` → boolean.
- `(file-digest bytes)` → `sha256:<hex>`.
- `(tree-digest [{:path p :digest d} ...])` → Genome ID.

Canonical hashing rules are normative:

```text
1. UTF-8 bytes for text assets.
2. Normalize CRLF/CR to LF before hashing text files.
3. Never include mtime, inode, owner, or host absolute path.
4. Reject symlinks, absolute paths, `.` and `..` path components.
5. Sort tree entries by normalized path using bytewise lexical order.
6. Hash each index line as: path + NUL + digest + LF.
7. Genome ID = SHA-256 of the concatenated index bytes.
```

- [ ] **Step 1: Write path traversal tests:** `../x`, `/tmp/x`, `a/../../b`, backslash-normalized Windows traversal, and symlink fixture all fail.
- [ ] **Step 2: Write a golden hash test for a two-file synthetic tree.** Hard-code the expected SHA-256 so accidental canonicalization changes break the test.
- [ ] **Step 3: Implement normalization and hashing.**
- [ ] **Step 4: Add property tests:** shuffling input entry order must not change `tree-digest`; changing one byte must change the digest.
- [ ] **Step 5: Run tests twice on the same fixture to ensure determinism.**
- [ ] **Step 6: Commit.**

**Acceptance:** Genome identity depends only on canonical logical content.

---

## Task 1.4 — Load an immutable Genome bundle from disk

**Files:**

- Create: `src/evoclj/genome/load.clj`
- Create: `test/evoclj/genome/load_test.clj`
- Add fixtures: `test/fixtures/genomes/minimal-valid/{topology.edn,models.edn,memory.edn,evolution.edn}`

**Interfaces:**

```clojure
(load-genome root-path)
;; => {:genome/id "sha256:..."
;;     :genome/root <Path>
;;     :manifest {...}
;;     :files {"manifest.edn" {:digest ... :bytes ... :kind :edn}
;;             ...}}
```

`load-genome` MUST NOT execute Clojure code. `.clj` files are bytes/text only at this stage.

- [ ] **Step 1: Write a test that loads the minimal fixture and returns a stable `:genome/id`.**
- [ ] **Step 2: Write failure tests for missing manifest, undeclared required module, symlink, unreadable file, duplicate-normalized path, and path traversal.**
- [ ] **Step 3: Implement directory walk without following links.**
- [ ] **Step 4: Parse only declared EDN files using `clojure.edn/read-string`; never `read-string` from `clojure.core`.**
- [ ] **Step 5: Validate manifest before trusting its module paths.**
- [ ] **Step 6: Ensure all loaded bytes are immutable values in memory and no open stream escapes the function.**
- [ ] **Step 7: Run tests and commit.**

**Milestone 1 exit test:**

```bash
clojure -M:test -r ".*evoclj\.genome\..*"
```

Expected: all Genome tests pass; no SCI, provider, network, or database namespace is needed.

---

# Milestone 2 — Compiler

**Milestone outcome:** A validated Genome is transformed into a pure `CompiledGenome` containing resolved module data, validated topology IR, parsed program descriptors, capability requests, and a reproducible `Resolution`. Compilation performs no external effects beyond explicit provider metadata resolution supplied as data.

## Task 2.1 — Define Resolution and provider alias resolution

**Files:**

- Create: `src/evoclj/compiler/resolution.clj`
- Create: `test/evoclj/compiler/resolution_test.clj`
- Create: `test/fixtures/resolution/provider-catalog.edn`

**Interfaces:**

```clojure
(resolve-models models-config provider-catalog)
;; => {:resolution/id "sha256:..."
;;     :models {:planner {:alias :reasoning/high
;;                        :provider :fixture
;;                        :provider-model "fixture-model-v1"
;;                        :adapter-version "1"}}}
```

Resolution is pure data. Secrets never appear in it.

- [ ] **Step 1: Write tests for deterministic alias resolution and missing alias failure.**
- [ ] **Step 2: Write a test proving two provider catalogs that resolve to different concrete model IDs produce different Resolution IDs even with the same Genome.**
- [ ] **Step 3: Implement canonical Resolution hashing using the same deterministic EDN normalization rules used elsewhere.**
- [ ] **Step 4: Reject secret-looking keys (`:api-key`, `:token`, `:password`, `:secret`) in resolved data.**
- [ ] **Step 5: Run and commit.**

**Acceptance:** The same Genome can be instantiated under explicitly different resolved environments without pretending they are the same Phenotype.

---

## Task 2.2 — Define and validate topology IR

**Files:**

- Create: `src/evoclj/compiler/topology.clj`
- Create: `test/evoclj/compiler/topology_test.clj`
- Add fixture: `test/fixtures/genomes/minimal-valid/topology.edn`

**Interfaces:**

Supported v0 node types:

```text
:llm
:sci
:tool
:route
:loop
:emit
:memory/read
:memory/write
```

Topology value:

```clojure
{:graph/id :graph/main
 :entry :node/planner
 :nodes
 {:node/planner {:node/type :llm :model :planner :next :node/router}
  :node/router  {:node/type :sci :program :program/route}
  :node/finish  {:node/type :emit}}
 :limits {:max-steps 64}}
```

- [ ] **Step 1: Write tests for unknown node type, missing entry node, dangling `:next`, duplicate IDs after merge, and illegal raw cycle.**
- [ ] **Step 2: Define the rule that arbitrary graph cycles are rejected; only explicit `:loop` nodes may iterate.**
- [ ] **Step 3: Implement topology validation and compile adjacency into a normalized map/vector representation.**
- [ ] **Step 4: Add a test that node ordering in the EDN source does not affect compiled topology equality.**
- [ ] **Step 5: Run and commit.**

**Acceptance:** The runtime never has to discover malformed graph structure while already executing a task.

---

## Task 2.3 — Discover and statically validate evolvable SCI programs

**Files:**

- Create: `src/evoclj/compiler/program.clj`
- Create: `test/evoclj/compiler/program_test.clj`
- Add fixture: `test/fixtures/genomes/minimal-valid/programs/route.clj`

**Interfaces:**

Genome programs are declared as descriptors, not inferred from arbitrary source files:

```clojure
{:program/id :program/route
 :file "programs/route.clj"
 :entry 'agent.route/run
 :input-schema :schema/route-input
 :output-schema :schema/intent-or-route}
```

`compile-program-descriptor` validates file existence, path, entry symbol, and source readability. It does not execute the program.

- [ ] **Step 1: Test a valid descriptor and invalid descriptor variants.**
- [ ] **Step 2: Reject `load-file`, `eval`, `require` of undeclared host namespaces, Java class literals, and reader-eval forms at compile policy inspection time where detectable.** The runtime sandbox remains the final enforcement layer.
- [ ] **Step 3: Parse source using a Clojure/rewrite-clj reader suitable for structure inspection without host evaluation.**
- [ ] **Step 4: Return a serializable `ProgramDescriptor` containing source digest, entry symbol, and declared schemas.**
- [ ] **Step 5: Run and commit.**

---

## Task 2.4 — Produce the `CompiledGenome` and Phenotype identity

**Files:**

- Create: `src/evoclj/compiler/core.clj`
- Create: `test/evoclj/compiler/core_test.clj`

**Interfaces:**

```clojure
(compile-genome loaded-genome provider-catalog)
;; => {:compiled/genome-id ...
;;     :compiled/resolution-id ...
;;     :compiled/phenotype-id ...
;;     :manifest ...
;;     :topology ...
;;     :programs ...
;;     :requested-capabilities ...
;;     :resolution ...}
```

Phenotype identity must include kernel ABI, Genome ID, and Resolution ID:

```text
phenotype-id = SHA256(kernel-abi || genome-id || resolution-id)
```

- [ ] **Step 1: Write a full fixture compile test.**
- [ ] **Step 2: Write a test that changing only the Resolution changes Phenotype ID but not Genome ID.**
- [ ] **Step 3: Write a test that compile output can round-trip through EDN except source bytes, which remain artifact references/digests.**
- [ ] **Step 4: Implement `compile-genome` as orchestration only; keep validation logic in the preceding focused modules.**
- [ ] **Step 5: Run all Milestone 1–2 tests and commit.**

**Milestone 2 exit test:** Compile the seed fixture 100 times in one process and assert identical semantic output and IDs.

---

# Milestone 3 — SCI Runtime

**Milestone outcome:** A `CompiledGenome` program can run inside an explicitly restricted SCI context, accept only validated EDN input, return only fully realized validated EDN output, and be interrupted by deterministic resource limits. It still cannot perform external effects.

## Task 3.1 — Build a closed SCI context with an explicit allow surface

**Files:**

- Create: `src/evoclj/sci/context.clj`
- Create: `src/evoclj/sci/expose.clj`
- Create: `test/evoclj/sci/context_test.clj`

**Interfaces:**

```clojure
(make-context {:programs ... :api-namespaces ... :limits ...})
(run-form ctx source entry input)
```

Default host exposure MUST exclude filesystem, environment, Java interop, process execution, dynamic loading, and arbitrary host vars.

- [ ] **Step 1: Write passing tests for arithmetic, maps/vectors, pure functions, and the explicitly exposed `evo.api.intent` constructor namespace.**
- [ ] **Step 2: Write adversarial failing tests for `System/getenv`, `java.io.File`, `Runtime/getRuntime`, `ProcessBuilder`, `slurp`, `spit`, `load-file`, host `eval`, and undeclared `require`.**
- [ ] **Step 3: Implement a context with explicit namespaces/classes/symbol policies. Do not use `:allow :all`.**
- [ ] **Step 4: Verify definitions inside the SCI context do not mutate host Clojure Vars.**
- [ ] **Step 5: Run and commit.**

**Acceptance:** The SCI layer is useful for pure decision logic but useless as an ambient shell.

---

## Task 3.2 — Enforce the EDN-safe boundary and eager realization

**Files:**

- Create: `src/evoclj/sci/boundary.clj`
- Create: `test/evoclj/sci/boundary_test.clj`

**Interfaces:**

```clojure
(edn-safe? x)
(materialize-edn x)
(validate-program-input schema x)
(validate-program-output schema x)
```

- [ ] **Step 1: Test accepted values:** nil, booleans, numbers, strings, keywords, symbols, vectors, lists, maps, sets, nested combinations.
- [ ] **Step 2: Test rejected values:** Java `File`, InputStream, function, atom, promise, future, lazy seq that has not been realized, SCI var object, arbitrary record not explicitly registered.
- [ ] **Step 3: Implement recursive eager materialization with explicit maximum depth and maximum collection size.**
- [ ] **Step 4: Ensure infinite/lazy sequences cannot escape by realizing them under the runtime limit rather than returning the lazy value.**
- [ ] **Step 5: Run and commit.**

---

## Task 3.3 — Add SCI execution limits and interruption

**Files:**

- Create: `src/evoclj/sci/limits.clj`
- Create: `src/evoclj/sci/execute.clj`
- Create: `test/evoclj/sci/limits_test.clj`

**Interfaces:**

```clojure
(execute-program sci-runtime program-descriptor input
                 {:wall-ms 100
                  :max-steps 10000
                  :max-output-nodes 10000})
;; => {:status :ok :value ... :usage {...}}
;; or {:status :error :error {...}}
```

- [ ] **Step 1: Write a test for an intentionally infinite `loop/recur` fixture.** Expected result: typed `:sci/limit-exceeded`, no stuck test process.
- [ ] **Step 2: Write a test for excessive output materialization.**
- [ ] **Step 3: Implement wall-clock cancellation plus SCI `:interrupt-fn` checks.**
- [ ] **Step 4: Convert internal exceptions to stable serializable error data at the boundary.**
- [ ] **Step 5: Repeat the infinite-loop test 100 times and check no leaked worker threads remain.**
- [ ] **Step 6: Run and commit.**

---

## Task 3.4 — Execute a Genome-declared SCI entry point

**Files:**

- Modify: `src/evoclj/sci/execute.clj`
- Create: `test/evoclj/sci/program_execution_test.clj`

**Interfaces:**

```clojure
(load-program! runtime compiled-program)
(invoke! runtime :program/route input)
```

The load operation mutates only the isolated SCI context owned by a Phenotype instance, never host Vars and never the source Genome.

- [ ] **Step 1: Test the seed `route.clj` program end to end from compiled descriptor to EDN output.**
- [ ] **Step 2: Test two independent runtime contexts loaded from the same Genome: redefining a SCI var in one test context must not affect the other.**
- [ ] **Step 3: Test that source bytes remain unchanged before/after execution by comparing the Genome digest.**
- [ ] **Step 4: Implement program loading and symbol lookup only inside the context.**
- [ ] **Step 5: Run and commit.**

**Milestone 3 exit test:** the seed routing program can compute a decision from EDN input, while every attempted host side effect is denied or impossible.

---

# Milestone 4 — Intent and Capability Broker

**Milestone outcome:** Evolvable code can request actions only by emitting typed Intents. The trusted broker normalizes the real resource target, checks an explicit capability lease and budget/policy constraints, invokes a registered provider adapter, and returns a typed Result. Tool visibility and authorization are separate concepts.

## Task 4.1 — Define the Intent ABI and canonical normalization

**Files:**

- Create: `src/evoclj/intent/schema.clj`
- Create: `src/evoclj/intent/core.clj`
- Create: `test/evoclj/intent/core_test.clj`

**Interfaces:**

Supported v0 Intent types:

```text
:intent/model-call
:intent/tool-call
:intent/memory-read
:intent/memory-write
:intent/finish
:intent/fail
```

Base shape:

```clojure
{:intent/id #uuid "..."
 :intent/type :intent/tool-call
 :session/id #uuid "..."
 :phenotype/id "sha256:..."
 :node/id :node/tool
 :cause/event-id 17
 :payload {:tool/id :fixture/echo
           :args {:text "hello"}}
 :budget {:wall-ms 1000}
 :metadata {}}
```

- [ ] **Step 1: Write schema tests for every v0 intent type.**
- [ ] **Step 2: Write rejection tests for missing attribution fields, unknown intent type, Java object in payload, and negative budget.**
- [ ] **Step 3: Implement `(normalize-intent x)` so semantically equivalent map ordering yields equal normalized data but the function never invents authorization.**
- [ ] **Step 4: Implement helper constructors in `evoclj.intent.core` that SCI may call indirectly through an exposed API namespace.**
- [ ] **Step 5: Run and commit.**

**Acceptance:** A runtime action is always represented by a validated immutable value before any provider code runs.

---

## Task 4.2 — Define capability resources and lease semantics

**Files:**

- Create: `src/evoclj/capability/schema.clj`
- Create: `src/evoclj/capability/lease.clj`
- Create: `test/evoclj/capability/lease_test.clj`

**Interfaces:**

```clojure
{:cap/id #uuid "..."
 :subject {:phenotype/id "sha256:..."}
 :resource {:kind :tool :id :fixture/echo}
 :actions #{:invoke}
 :constraints {:max-calls 10}
 :issued-at #inst "..."
 :expires-at #inst "..."}
```

Functions:

```clojure
(valid-at? lease instant)
(subject-matches? lease subject)
(resource-covers? lease normalized-resource action)
```

- [ ] **Step 1: Test exact subject matching. A capability for `P1` must not authorize `P2`, even if both share the same Genome.**
- [ ] **Step 2: Test expiry boundaries and action mismatch.**
- [ ] **Step 3: Test filesystem-style resource scoping using canonical resolved paths, not user-supplied strings.**
- [ ] **Step 4: Implement leases as plain immutable maps validated by Malli.**
- [ ] **Step 5: Run and commit.**

**Acceptance:** A capability is a bounded host-owned grant, not a string name visible to the model.

---

## Task 4.3 — Introduce provider/tool descriptors and real-resource normalization

**Files:**

- Create: `src/evoclj/provider/protocol.clj`
- Create: `src/evoclj/provider/registry.clj`
- Create: `src/evoclj/provider/fixture.clj`
- Create: `test/evoclj/provider/registry_test.clj`

**Interfaces:**

Provider protocol:

```clojure
(defprotocol Provider
  (describe [provider])
  (normalize-request [provider intent])
  (execute-request! [provider authorized-request]))
```

Descriptor example:

```clojure
{:tool/id :fixture/echo
 :effect :pure
 :input-schema [:map [:text :string]]
 :output-schema [:map [:text :string]]
 :required-action :invoke
 :retry {:safe? true}}
```

- [ ] **Step 1: Write tests proving registration rejects duplicate tool IDs and malformed descriptors.**
- [ ] **Step 2: Write a fixture provider whose `normalize-request` turns a user-facing request into a canonical resource descriptor.**
- [ ] **Step 3: Ensure normalization happens before authorization. Add a traversal-style fixture where raw `"a/../secret"` resolves to a protected path and must be checked as the protected canonical path.**
- [ ] **Step 4: Ensure secrets/config remain closed over by provider instances and do not appear in descriptors or results.**
- [ ] **Step 5: Run and commit.**

---

## Task 4.4 — Implement authorization as a pure broker decision

**Files:**

- Create: `src/evoclj/capability/policy.clj`
- Create: `src/evoclj/capability/broker.clj`
- Create: `test/evoclj/capability/broker_test.clj`

**Interfaces:**

```clojure
(authorize {:intent intent
            :normalized-request request
            :leases leases
            :usage usage
            :now now})
;; => {:decision :allow :lease-id ...}
;; or {:decision :deny :reason :capability/missing}
```

`authorize` is pure. The effectful wrapper comes later.

- [ ] **Step 1: Test allow with exact capability and deny with no lease.**
- [ ] **Step 2: Test that merely registering or exposing a new tool does not authorize it.**
- [ ] **Step 3: Test expired lease, wrong phenotype, wrong action, exceeded max-call constraint, and normalized resource outside allowed scope.**
- [ ] **Step 4: Implement deterministic decision reason codes.**
- [ ] **Step 5: Add property test: removing leases from an authorization input can never turn a prior deny into allow.**
- [ ] **Step 6: Run and commit.**

**Acceptance:** Authorization can be tested without invoking a real provider.

---

## Task 4.5 — Execute authorized Intents with idempotency and typed results

**Files:**

- Create: `src/evoclj/intent/dispatch.clj`
- Create: `test/evoclj/intent/dispatch_test.clj`

**Interfaces:**

```clojure
(dispatch! broker-context intent)
;; => {:result/status :ok
;;     :intent/id ...
;;     :value ...
;;     :authorization {...}
;;     :usage {...}}
```

For non-pure writes, request descriptors must include an idempotency key before execution. Automatic retries are allowed only if provider descriptor declares `:retry {:safe? true}`.

- [ ] **Step 1: Test allowed fixture echo.**
- [ ] **Step 2: Test denied request never increments fixture provider's execution counter.**
- [ ] **Step 3: Test a simulated transient error retries a pure/idempotent fixture but not a non-idempotent fixture.**
- [ ] **Step 4: Test output schema validation; malformed provider output is `:provider/output-invalid`, not accepted as model-visible data.**
- [ ] **Step 5: Implement dispatcher order exactly: validate intent → lookup provider → normalize resource → authorize → execute once/retry per policy → validate output → return typed result.**
- [ ] **Step 6: Run and commit.**

**Milestone 4 exit test:** A SCI program can construct an Intent value, but only the host broker can turn it into a fixture effect; visible-but-ungranted tools are consistently denied.

---

# Milestone 5 — Event Store and Artifact Store

**Milestone outcome:** Every session transition, Intent, authorization, provider effect, result, episode, candidate, evaluation, and promotion decision can be represented durably. Large payloads are immutable CAS artifacts. The event log is append-only and reconstructable after process restart.

## Task 5.1 — Create SQLite schema and migration runner

**Files:**

- Create: `resources/migrations/001-init.sql`
- Create: `src/evoclj/store/sqlite.clj`
- Create: `src/evoclj/store/migrate.clj`
- Create: `test/evoclj/store/migrate_test.clj`

**Initial tables:**

```text
meta
generations
candidates
mutations
sessions
events
artifacts
model_calls
tool_calls
episodes
eval_runs
eval_cases
eval_results
capability_leases
promotions
```

- [ ] **Step 1: Write a test that creates a fresh temporary database and applies all migrations once.**
- [ ] **Step 2: Apply migrations a second time and verify no duplicate/schema damage.**
- [ ] **Step 3: Assert required unique constraints:** Genome/generation identifiers, one event sequence per session, one current-pointer row, unique artifact hash.
- [ ] **Step 4: Add foreign keys for lineage relationships where they do not make append-only recovery impossible.**
- [ ] **Step 5: Implement migration transaction and schema-version check.**
- [ ] **Step 6: Run and commit.**

**Acceptance:** A blank installation can deterministically establish durable state without manual SQL.

---

## Task 5.2 — Implement filesystem content-addressed storage

**Files:**

- Create: `src/evoclj/store/cas.clj`
- Create: `test/evoclj/store/cas_test.clj`

**Interfaces:**

```clojure
(put-bytes! cas bytes {:media-type "application/edn"})
;; => {:artifact/id "sha256:..." :size 123 :media-type ...}

(get-bytes cas artifact-id)
(exists? cas artifact-id)
```

Physical layout:

```text
cas/sha256/ab/abcdef.../body
cas/sha256/ab/abcdef.../meta.edn
```

- [ ] **Step 1: Test same bytes written twice yield the same ID and one logical artifact.**
- [ ] **Step 2: Test atomic write via temp file + fsync/rename semantics appropriate to the host filesystem.**
- [ ] **Step 3: Test corrupted body is detected by re-hashing on read when verification mode is enabled.**
- [ ] **Step 4: Test metadata cannot overwrite body identity.**
- [ ] **Step 5: Implement and commit.**

---

## Task 5.3 — Implement append-only events with causal references

**Files:**

- Create: `src/evoclj/store/event.clj`
- Create: `src/evoclj/store/event_schema.clj`
- Create: `test/evoclj/store/event_test.clj`

**Interfaces:**

```clojure
(append-event! store
  {:session/id ...
   :generation/id ...
   :phenotype/id ...
   :event/type :intent/authorized
   :cause/event-id 41
   :payload-ref "sha256:..."
   :metadata {}})
;; => event with monotonically increasing :event/seq for that session
```

- [ ] **Step 1: Test per-session monotonic sequence allocation inside a transaction.**
- [ ] **Step 2: Test cause reference must point to an earlier event in the same session unless event type is a root event.**
- [ ] **Step 3: Test no update/delete API exists in the event namespace.**
- [ ] **Step 4: Implement event append and query by session/sequence/type.**
- [ ] **Step 5: Add a hash-chain field over canonical event headers for tamper-evidence. Test modifying a copied historical row causes `verify-event-chain` to fail.**
- [ ] **Step 6: Run and commit.**

**Acceptance:** The store records not just what happened, but the causal chain by which an effect was requested and authorized.

---

## Task 5.4 — Persist session pinning and lifecycle transitions

**Files:**

- Create: `src/evoclj/store/session.clj`
- Create: `test/evoclj/store/session_test.clj`

**State machine:**

```text
:created → :resolving → :running ↔ :waiting → :completed
                         ├──────────────→ :failed
                         ├──────────────→ :cancelled
                         └──────────────→ :budget-exhausted
```

**Interfaces:**

```clojure
(create-session! store {:genome/id ... :resolution/id ... :phenotype/id ...})
(transition-session! store session-id expected-state new-state data)
(get-session store session-id)
```

- [ ] **Step 1: Test a session records immutable Genome/Resolution/Phenotype IDs at creation.**
- [ ] **Step 2: Test illegal state transition fails with `:session/invalid-transition`.**
- [ ] **Step 3: Test no update operation can change pinned IDs.**
- [ ] **Step 4: Implement compare-and-set state transition in SQL so concurrent workers cannot both transition from the same state silently.**
- [ ] **Step 5: Run and commit.**

---

## Task 5.5 — Implement restart recovery and integrity checks

**Files:**

- Create: `src/evoclj/store/recovery.clj`
- Create: `test/evoclj/store/recovery_test.clj`

**Interfaces:**

```clojure
(scan-recovery-state store cas)
;; => {:orphaned-sessions [...]
;;     :missing-artifacts [...]
;;     :invalid-event-chains [...]
;;     :stale-candidates [...]}
```

- [ ] **Step 1: Simulate process death after session entered `:running` but before a terminal event. Recovery must classify it, not pretend completion.**
- [ ] **Step 2: Simulate missing CAS payload referenced by an event. Integrity scan must fail loudly.**
- [ ] **Step 3: Simulate a prepared but uncommitted candidate; recovery may mark it stale but must not promote it.**
- [ ] **Step 4: Implement startup integrity scan with configurable strict mode; production default is fail-closed on current-generation corruption.**
- [ ] **Step 5: Run all store tests and commit.**

**Milestone 5 exit test:** Start runtime, append a realistic session/intent/result trace, terminate process, reopen DB/CAS, verify event chain and reconstruct pinned session identity without in-memory state.

---

# Milestone 6 — Executor

**Milestone outcome:** A compiled Genome becomes a live Phenotype capable of executing a task through its topology. Node handlers produce Intents; the broker performs effects; every transition is persisted; the session remains pinned to its original generation. The seed Genome can complete a deterministic end-to-end fixture task.

## Task 6.1 — Define Phenotype construction and lifecycle

**Files:**

- Create: `src/evoclj/runtime/phenotype.clj`
- Create: `src/evoclj/runtime/system.clj`
- Create: `test/evoclj/runtime/phenotype_test.clj`

**Interfaces:**

```clojure
(instantiate compiled-genome runtime-deps)
;; => {:phenotype/id ...
;;     :compiled compiled-genome
;;     :sci-runtime ...
;;     :providers ...
;;     :capabilities ...}

(halt! phenotype)
```

- [ ] **Step 1: Test two Phenotypes from one Genome have isolated SCI mutable context while sharing immutable compiled data.**
- [ ] **Step 2: Test `halt!` is idempotent.**
- [ ] **Step 3: Test construction does not open resources not declared in runtime dependencies.**
- [ ] **Step 4: Use Integrant only for stable host components; do not turn Genome graph nodes into global Integrant components.**
- [ ] **Step 5: Run and commit.**

---

## Task 6.2 — Implement node handler protocol and pure transitions

**Files:**

- Create: `src/evoclj/runtime/node.clj`
- Create: `src/evoclj/runtime/nodes/emit.clj`
- Create: `src/evoclj/runtime/nodes/sci.clj`
- Create: `src/evoclj/runtime/nodes/tool.clj`
- Create: `test/evoclj/runtime/node_test.clj`

**Interfaces:**

```clojure
(defprotocol NodeHandler
  (step [handler runtime-state node input-event]))

;; Result is data:
{:transition/status :continue
 :outputs [...]
 :intents [...]
 :next [:node/x]}
```

- [ ] **Step 1: Write tests for `:emit`, `:sci`, and `:tool` handlers.**
- [ ] **Step 2: Ensure node handlers themselves do not call providers; `:tool` emits an Intent only.**
- [ ] **Step 3: Validate every handler result against one shared transition schema.**
- [ ] **Step 4: Implement registry from node type keyword to trusted handler constructor.**
- [ ] **Step 5: Run and commit.**

---

## Task 6.3 — Build deterministic scheduler and step budget

**Files:**

- Create: `src/evoclj/runtime/scheduler.clj`
- Create: `test/evoclj/runtime/scheduler_test.clj`

**Interfaces:**

```clojure
(run-session! executor session-id task-input)
;; => {:status :completed :output-ref ... :episode/id ...}
```

For v0, scheduler is single-session deterministic FIFO. Add concurrency only after semantics are stable.

- [ ] **Step 1: Write a fixture graph `SCI route → tool → emit` and assert exact logical event order.**
- [ ] **Step 2: Test `:limits {:max-steps 3}` halts an overlong graph as `:budget-exhausted`.**
- [ ] **Step 3: Ensure scheduler appends node-start/node-result/intent/result events before advancing.**
- [ ] **Step 4: Ensure an unhandled node failure transitions session to `:failed` and preserves the error artifact.**
- [ ] **Step 5: Implement and commit.**

---

## Task 6.4 — Add explicit bounded loop semantics

**Files:**

- Create: `src/evoclj/runtime/nodes/loop.clj`
- Modify: `src/evoclj/compiler/topology.clj`
- Create: `test/evoclj/runtime/loop_test.clj`

**Interfaces:**

```clojure
{:node/type :loop
 :body :node/body
 :until :program/done?
 :max-iterations 8
 :next :node/finish}
```

- [ ] **Step 1: Test a loop that terminates after three iterations.**
- [ ] **Step 2: Test a loop whose predicate never succeeds terminates at `:max-iterations` with typed budget outcome.**
- [ ] **Step 3: Test compiler still rejects ordinary graph cycles outside explicit loop nodes.**
- [ ] **Step 4: Implement loop state as session-local data, not a SCI global var.**
- [ ] **Step 5: Run and commit.**

---

## Task 6.5 — Materialize Episode records from completed sessions

**Files:**

- Create: `src/evoclj/runtime/episode.clj`
- Create: `test/evoclj/runtime/episode_test.clj`

**Interfaces:**

```clojure
(materialize-episode! store session-id)
;; => {:episode/id ...
;;     :genome/id ...
;;     :resolution/id ...
;;     :task-ref ...
;;     :trace-range [first-event last-event]
;;     :outcome {:status ... :score ...}
;;     :usage {...}}
```

- [ ] **Step 1: Test completed and failed sessions both become episodes; failures are evidence, not discarded traces.**
- [ ] **Step 2: Test Episode references the full trace range and artifacts rather than copying every payload.**
- [ ] **Step 3: Test Episode generation ID equals the session's pinned generation, even if current generation pointer changes before materialization.**
- [ ] **Step 4: Implement and commit.**

---

## Task 6.6 — Seed Genome end-to-end execution fixture

**Files:**

- Create: `genomes/seed/manifest.edn`
- Create: `genomes/seed/topology.edn`
- Create: `genomes/seed/models.edn`
- Create: `genomes/seed/memory.edn`
- Create: `genomes/seed/evolution.edn`
- Create: `genomes/seed/programs/route.clj`
- Create: `test/evoclj/runtime/e2e_seed_test.clj`

**Fixture behavior:** Given `{:op :echo :text "abc"}`, the SCI router emits a typed fixture-tool Intent; the broker authorizes it with an explicitly supplied capability; the fixture provider returns `{:text "abc"}`; the emit node completes the session.

- [ ] **Step 1: Write the E2E test before finalizing fixture files.**
- [ ] **Step 2: Run it and confirm failure at the first missing integration.**
- [ ] **Step 3: Add only the glue required for `load → compile → instantiate → session → scheduler → broker → event store → episode`.**
- [ ] **Step 4: Assert exact invariants: Genome hash unchanged, session pin unchanged, one authorization event, one provider result, one completed Episode.**
- [ ] **Step 5: Restart the store and verify the episode/trace remains queryable.**
- [ ] **Step 6: Commit.**

**Milestone 6 exit test:** The system now performs useful work, but has no evolution privileges yet.

---

# Milestone 7 — Evolution

**Milestone outcome:** Episodes can be converted into bounded evidence packs; an isolated Diagnostician can emit structured hypotheses; a Mutator can emit a finite Mutation IR; the kernel can deterministically apply those mutations to a parent Genome and persist one or more immutable Candidates. Evolution still cannot decide promotion.

## Task 7.1 — Build evidence selection and frozen evidence packs

**Files:**

- Create: `src/evoclj/evolution/evidence.clj`
- Create: `src/evoclj/evolution/evidence_schema.clj`
- Create: `test/evoclj/evolution/evidence_test.clj`

**Interfaces:**

```clojure
(build-evidence-pack store
  {:generation/id G42
   :cutoff-event-id 9001
   :selector {:recent 40
              :include-successes 10
              :include-failures 10
              :include-high-cost 5}})
;; => {:evidence/id "sha256:..."
;;     :generation/id G42
;;     :cutoff-event-id 9001
;;     :episodes [...artifact refs...]
;;     :summary {...}}
```

Evidence cutoff is immutable. Episodes created after the cutoff MUST NOT silently enter a running evolution job.

- [ ] **Step 1: Write a test with successes and failures; assert both are represented.** This prevents the optimizer from learning only from failures and destroying already-correct behavior.
- [ ] **Step 2: Write a test where a new episode arrives after the pack is created; the pack hash/content must remain unchanged.**
- [ ] **Step 3: Write deterministic sampling/ranking tests. If randomness is needed, seed and persist it in the pack.**
- [ ] **Step 4: Store large trace excerpts as artifact refs with compact metadata; preserve original episode provenance.**
- [ ] **Step 5: Implement and commit.**

**Acceptance:** An evolution run has a reproducible evidence boundary.

---

## Task 7.2 — Define Diagnostician contract and structured hypotheses

**Files:**

- Create: `src/evoclj/evolution/diagnose.clj`
- Create: `src/evoclj/evolution/diagnosis_schema.clj`
- Create: `test/evoclj/evolution/diagnose_test.clj`

**Interfaces:**

```clojure
{:diagnosis/id ...
 :evidence/id ...
 :hypotheses
 [{:hypothesis/id ...
   :pattern :premature-tool-mutation
   :claim "..."
   :support [{:episode/id ... :event-ids [...]}]
   :counterevidence [{:episode/id ...}]
   :target {:kind :skill :id :debugging}
   :expected-effect {:metric :task/success :direction :increase}
   :confidence-band :medium}]}
```

No free-form diagnosis may directly alter the Genome.

- [ ] **Step 1: Write schema tests requiring support, target, and expected effect.**
- [ ] **Step 2: Test that unsupported hypotheses with zero evidence references are rejected.**
- [ ] **Step 3: Implement a `Diagnostician` protocol so the first test adapter is deterministic, while a future LLM adapter can conform to the same contract.**

```clojure
(defprotocol Diagnostician
  (diagnose [d evidence-pack]))
```

- [ ] **Step 4: Ensure the adapter receives Evolution-set evidence only; no Selection/Audit fixture handle is present in its constructor.**
- [ ] **Step 5: Persist diagnosis artifacts and provenance.**
- [ ] **Step 6: Run and commit.**

---

## Task 7.3 — Define the Mutation IR and patch preconditions

**Files:**

- Create: `src/evoclj/evolution/mutation_schema.clj`
- Create: `src/evoclj/evolution/mutation.clj`
- Create: `test/evoclj/evolution/mutation_test.clj`

**Interfaces:**

Required Mutation shape:

```clojure
{:mutation/id #uuid "..."
 :parent/genome-id "sha256:..."
 :hypothesis/id #uuid "..."
 :evidence/id "sha256:..."
 :risk :behavioral
 :ops
 [{:op :set-edn
   :file "skills/debugging.edn"
   :path [:workflow :before-edit]
   :expect/hash "sha256:..."
   :value [:reproduce :localize]}]
 :expected-effect
 {:primary-metric :task/success
  :direction :increase}}
```

Initial operation set:

```text
:set-edn
:delete-edn
:insert-text
:replace-text
:delete-text
:replace-form
:insert-form
:delete-form
:add-node
:remove-node
:add-edge
:remove-edge
:update-node
```

- [ ] **Step 1: Write schema tests for all op variants and shared preconditions.**
- [ ] **Step 2: Require an expected preimage digest/path selector for any destructive/replace operation. This prevents stale patches from silently applying to a different parent.**
- [ ] **Step 3: Reject operations targeting kernel files, evaluation roots, protected Genome paths, undeclared mutable classes, or capability-root data.**
- [ ] **Step 4: Implement only data validation in this task; application comes next.**
- [ ] **Step 5: Run and commit.**

---

## Task 7.4 — Apply EDN/text/Clojure-form mutations deterministically

**Files:**

- Create: `src/evoclj/genome/patch.clj`
- Create: `src/evoclj/genome/patch_edn.clj`
- Create: `src/evoclj/genome/patch_text.clj`
- Create: `src/evoclj/genome/patch_clj.clj`
- Create: `test/evoclj/genome/patch_test.clj`

**Interfaces:**

```clojure
(apply-mutation parent-loaded-genome mutation output-dir)
;; => newly loaded immutable Genome with new :genome/id
```

- [ ] **Step 1: EDN patch test:** set a nested value and verify exact resulting canonical EDN data.
- [ ] **Step 2: Stale-preimage test:** wrong `:expect/hash` fails without creating a candidate directory marked valid.
- [ ] **Step 3: Text patch test:** replacement must match an explicitly bounded source range/hash, not an unconstrained global string replace.
- [ ] **Step 4: Clojure form patch test using rewrite-clj:** replace a target var/form while preserving unrelated comments and whitespace.
- [ ] **Step 5: Test deterministic application twice into separate temporary directories; resulting Genome IDs and logical file contents must match exactly.**
- [ ] **Step 6: Test patch application cannot follow symlinks or escape the candidate staging root.**
- [ ] **Step 7: Implement staging write → validate/load/hash → atomic candidate directory finalize.**
- [ ] **Step 8: Run and commit.**

**Acceptance:** The Mutator never writes arbitrary files; the kernel applies a finite declarative patch language.

---

## Task 7.5 — Enforce mutation budgets and risk classes

**Files:**

- Create: `src/evoclj/evolution/budget.clj`
- Create: `test/evoclj/evolution/budget_test.clj`

**Initial risk classes:**

```text
R0 :parameter
R1 :behavioral      ; prompt/skill/text rules
R2 :program         ; SCI forms
R3 :topology
R4 :meta            ; evolution policy; not enabled in v0 release
```

Default v0 budget profile:

```clojure
{:parameter {:max-ops 8}
 :behavioral {:max-files 2 :max-added-bytes 8192 :max-deleted-bytes 8192}
 :program {:max-files 2 :max-top-level-forms 3}
 :topology {:max-new-nodes 2 :max-removed-nodes 1 :max-edge-changes 4}}
```

- [ ] **Step 1: Write a mutation-cost calculator test for each op class.**
- [ ] **Step 2: Test aggregate limits across multiple ops/files.**
- [ ] **Step 3: Test an R1 mutation containing an R3 graph operation is rejected as under-declared risk.**
- [ ] **Step 4: Explicitly reject R4 in v0 with `:evolution/risk-not-enabled`.**
- [ ] **Step 5: Implement and commit.**

---

## Task 7.6 — Create Candidate records without activation rights

**Files:**

- Create: `src/evoclj/evolution/candidate.clj`
- Create: `test/evoclj/evolution/candidate_test.clj`

**Candidate state machine:**

```text
:proposed → :materialized → :evaluation-pending → :evaluated
                                          └────→ :invalid
```

Later Promotion may transition an evaluated candidate to `:canary`, `:promoted`, `:rejected`, or `:stale`.

- [ ] **Step 1: Test candidate creation records parent generation, parent Genome ID, mutation ID, evidence ID, candidate Genome ID, and risk.**
- [ ] **Step 2: Test a Candidate API has no function that changes current generation.**
- [ ] **Step 3: Test duplicate deterministic materialization of the same parent+mutation may refer to the same Genome content but remains auditable as the same or deduplicated candidate according to one explicit uniqueness rule. Choose uniqueness by `(parent-genome-id, mutation-hash)`.**
- [ ] **Step 4: Implement candidate persistence and commit.**

---

## Task 7.7 — Retain rejected mutation evidence and prevent immediate oscillation

**Files:**

- Create: `src/evoclj/evolution/history.clj`
- Create: `test/evoclj/evolution/history_test.clj`

**Interfaces:**

```clojure
(recent-mutation-history store generation-lineage {:limit 50})
;; => accepted/rejected mutation summaries with metric deltas/reasons
```

- [ ] **Step 1: Persist rejection reason and metric deltas once evaluator results exist; until then history reports pending.**
- [ ] **Step 2: Add a similarity fingerprint based on targeted files/op types/normalized selectors, not LLM semantic judgment alone.**
- [ ] **Step 3: Test exact repeat of a recently rejected mutation is flagged to the Mutator as negative evidence.**
- [ ] **Step 4: Do not automatically ban similar future mutations; expose history as evidence and leave final proposal logic separate.**
- [ ] **Step 5: Run and commit.**

---

## Task 7.8 — Orchestrate one evolution proposal cycle

**Files:**

- Create: `src/evoclj/evolution/core.clj`
- Create: `test/evoclj/evolution/core_test.clj`

**Interfaces:**

```clojure
(propose-candidates! evolution-system
  {:generation/id current
   :evidence-selector ...
   :max-candidates 3})
;; => [candidate-record ...]
```

- [ ] **Step 1: Use deterministic fake Diagnostician and Mutator adapters to write the orchestration test.**
- [ ] **Step 2: Assert exact phase order: freeze evidence → diagnose → load negative history → propose mutation → validate risk/budget → apply patch → compile candidate → persist Candidate.**
- [ ] **Step 3: Assert any failure before materialization cannot affect current Genome directory or current-generation pointer.**
- [ ] **Step 4: Limit v0 to max three candidates per cycle.**
- [ ] **Step 5: Run and commit.**

**Milestone 7 exit test:** Given fixture Episodes, EvoCLJ produces a deterministic immutable `G2` Candidate from `G1`, records why it exists, but has no mechanism yet to call it “better.”

---

# Milestone 8 — Evaluator

**Milestone outcome:** Candidates are judged by a kernel-owned evaluation pipeline separated from the mutation context. Gates run from static validity through historical replay to hidden paired selection. Results preserve hard constraints, utility, cost, and complexity separately. Evaluation produces eligibility facts, not activation.

## Task 8.1 — Define evaluation profiles and physically separated datasets

**Files:**

- Create: `src/evoclj/eval/profile.clj`
- Create: `src/evoclj/eval/dataset.clj`
- Create: `test/evoclj/eval/dataset_test.clj`
- Create: `evals/evolution/README.md`
- Create: `evals/selection/README.md`
- Create: `evals/audit/README.md`

**Interfaces:**

```clojure
{:eval/profile-id :default-v1
 :evolution-set {:source ...}
 :selection-set {:source ... :visibility :kernel-only}
 :audit-set {:source ... :visibility :operator-only}
 :repetitions 1
 :promotion {...}}
```

- [ ] **Step 1: Write a mount/access test proving candidate workspace construction does not contain the Selection or Audit directory.**
- [ ] **Step 2: Ensure Evolution adapters receive only artifact refs explicitly copied into their evidence pack.**
- [ ] **Step 3: Ensure selection case bodies are loaded only inside evaluator code after candidate materialization.**
- [ ] **Step 4: Define audit set as absent from ordinary automated evolution execution entirely.**
- [ ] **Step 5: Implement dataset loaders and commit.**

**Acceptance:** Selection isolation is an architectural boundary, not a system prompt instruction.

---

## Task 8.2 — Implement Gates G0–G3: parse, schema, static policy, deterministic tests

**Files:**

- Create: `src/evoclj/eval/gates.clj`
- Create: `src/evoclj/eval/static.clj`
- Create: `test/evoclj/eval/gates_test.clj`

**Gate result schema:**

```clojure
{:gate/id :G2-static-policy
 :status :pass|:fail|:error
 :hard? true
 :details-ref ...
 :duration-ms ...}
```

- [ ] **Step 1: G0 calls Genome load/Compiler parse path against the candidate from scratch, not cached Mutator claims.**
- [ ] **Step 2: G1 revalidates all schemas and ABI compatibility.**
- [ ] **Step 3: G2 checks protected paths, requested capability expansion, forbidden program surfaces, invalid topology, and evaluator mutation attempts.**
- [ ] **Step 4: G3 invokes registered deterministic unit/property suites in a fresh candidate workspace.**
- [ ] **Step 5: Test short-circuit behavior: a hard G2 failure prevents later effectful gates.**
- [ ] **Step 6: Persist every gate result and commit.**

---

## Task 8.3 — Implement G4 historical replay with representative cases

**Files:**

- Create: `src/evoclj/eval/replay.clj`
- Create: `test/evoclj/eval/replay_test.clj`

**Interfaces:**

```clojure
(run-replay! evaluator candidate replay-case-ids)
;; => per-case outcomes + aggregate regressions
```

- [ ] **Step 1: Build replay cases from stored Episodes with fixtureable external providers.** Replays must not blindly repeat real external writes.
- [ ] **Step 2: Define provider replay modes: `:fixture`, `:recorded-read`, `:forbid-write`.**
- [ ] **Step 3: Test a candidate that fixes one failure but breaks a known success; replay must surface both.**
- [ ] **Step 4: Allow output equivalence predicates rather than byte-identical output where the task contract permits variation.**
- [ ] **Step 5: Mark a configurable subset of replay cases `:critical`; any critical regression is a hard failure.**
- [ ] **Step 6: Run and commit.**

---

## Task 8.4 — Implement isolated paired Selection runner G5

**Files:**

- Create: `src/evoclj/eval/paired.clj`
- Create: `src/evoclj/eval/runner.clj`
- Create: `test/evoclj/eval/paired_test.clj`

**Interfaces:**

```clojure
(run-paired-selection! evaluator
  {:parent-generation G42
   :candidate-id C17
   :case-set selection-set
   :repetitions 3})
;; => {:parent {...} :candidate {...} :pairs [...]}
```

- [ ] **Step 1: For each case/repetition, derive one persisted random seed/fixture version and use it for both parent and candidate where the provider supports determinism.**
- [ ] **Step 2: Alternate execution order (`parent/candidate`, then `candidate/parent`) to reduce temporal/provider bias.**
- [ ] **Step 3: Ensure parent is re-evaluated now; do not compare a fresh candidate to a stale historical parent score.**
- [ ] **Step 4: Use fresh Phenotypes and fresh session namespaces for every side of every pair.**
- [ ] **Step 5: Verify the Mutator receives only post-evaluation aggregate/approved diagnostics, never hidden case prompts, expected outputs, or verifier internals.**
- [ ] **Step 6: Persist case-level results with hidden bodies stored under evaluator-only artifact ACL/path.**
- [ ] **Step 7: Run and commit.**

**Acceptance:** Performance claims are paired and contemporaneous.

---

## Task 8.5 — Preserve hard, utility, cost, and complexity metrics separately

**Files:**

- Create: `src/evoclj/eval/metrics.clj`
- Create: `src/evoclj/eval/compare.clj`
- Create: `test/evoclj/eval/compare_test.clj`

**Evaluation summary:**

```clojure
{:hard {:safety {:parent 1.0 :candidate 1.0 :violations []}
        :integrity {:parent :pass :candidate :pass}}
 :utility {:task/success {:parent 0.72 :candidate 0.79}}
 :cost {:tokens/task {:parent 1200 :candidate 1260}
        :latency-ms {:parent 1500 :candidate 1580}}
 :complexity {:genome-bytes {:parent 18000 :candidate 18600}
              :graph-nodes {:parent 4 :candidate 4}}}
```

Comparison is lexicographic, not a weighted scalar.

- [ ] **Step 1: Write comparison tests where higher utility cannot compensate for a hard safety violation.**
- [ ] **Step 2: Write guardrail tests where utility improves but cost exceeds configured maximum regression, making the candidate ineligible.**
- [ ] **Step 3: Write min-delta test: tiny/noisy improvement below threshold is not enough.**
- [ ] **Step 4: Implement `(eligibility evaluation-summary profile)` returning data with explicit reasons.**
- [ ] **Step 5: Do not call Promotion code from this namespace.**
- [ ] **Step 6: Run and commit.**

---

## Task 8.6 — Add repeated rollout statistics without pretending certainty

**Files:**

- Create: `src/evoclj/eval/statistics.clj`
- Create: `test/evoclj/eval/statistics_test.clj`

**Interfaces:**

```clojure
(summarize-paired-deltas pairs)
;; => {:n ... :mean-delta ... :median-delta ... :wins ... :losses ... :ties ...}
```

- [ ] **Step 1: Test deterministic cases with one repetition.**
- [ ] **Step 2: Test stochastic fixture cases with multiple paired repetitions.**
- [ ] **Step 3: Store raw paired observations; summaries must be recomputable.**
- [ ] **Step 4: Avoid claiming a formal probability/calibration not justified by the sample. Expose descriptive statistics first.**
- [ ] **Step 5: Let profile require a minimum number of pairs and maximum allowed failure variance for high-risk mutations.**
- [ ] **Step 6: Run and commit.**

---

## Task 8.7 — End-to-end candidate evaluation orchestration

**Files:**

- Create: `src/evoclj/eval/core.clj`
- Create: `test/evoclj/eval/core_test.clj`

**Interfaces:**

```clojure
(evaluate-candidate! evaluator candidate-id profile-id)
;; => immutable Evaluation record with :eligibility
```

Phase order:

```text
G0 parse
→ G1 schema/ABI
→ G2 static policy
→ G3 deterministic tests
→ G4 replay
→ G5 paired hidden selection
→ G6 cost/complexity guardrails
→ eligibility summary
```

Canary remains Promotion's responsibility.

- [ ] **Step 1: Write orchestration tests for one passing candidate and one candidate failing at G2.**
- [ ] **Step 2: Ensure a failed hard gate records later gates as `:not-run`, not implicit passes.**
- [ ] **Step 3: Ensure evaluation is immutable after finalization; a rerun creates a new Evaluation ID.**
- [ ] **Step 4: Update Candidate state from `:evaluation-pending` to `:evaluated` transactionally after report persistence.**
- [ ] **Step 5: Run and commit.**

**Milestone 8 exit test:** A Candidate can now be declared `eligible? true/false` with a complete independent evidence trail, but CURRENT still cannot change.

---

# Milestone 9 — Promotion, Canary, Rollback, and Lineage

**Milestone outcome:** Only the trusted Promotion subsystem can change which generation new sessions receive. Activation is atomic and parent-checked. Canary rollout is explicit. Rollback changes future selection without erasing history or claiming to undo external effects. Operators can reconstruct the complete evolutionary lineage.

## Task 9.1 — Model generation and promotion states

**Files:**

- Create: `src/evoclj/promotion/schema.clj`
- Create: `src/evoclj/promotion/state.clj`
- Create: `test/evoclj/promotion/state_test.clj`

**Generation states:**

```text
:seed
:active
:superseded
:rolled-back
```

**Candidate terminal/deployment states:**

```text
:rejected
:stale
:canary
:promoted
:canary-failed
```

- [ ] **Step 1: Test only an evaluated, eligible candidate may enter `:canary` or direct promotion.**
- [ ] **Step 2: Test an ineligible candidate can only become `:rejected`.**
- [ ] **Step 3: Test state transition tables are pure and closed; unknown states fail.**
- [ ] **Step 4: Implement state machine separately from SQL mutation.**
- [ ] **Step 5: Run and commit.**

---

## Task 9.2 — Implement atomic CURRENT compare-and-set promotion

**Files:**

- Create: `src/evoclj/promotion/current.clj`
- Create: `src/evoclj/promotion/promote.clj`
- Create: `test/evoclj/promotion/promote_test.clj`

**Interfaces:**

```clojure
(promote! promotion-system
  {:candidate-id C17
   :evaluation-id E91
   :expected-parent-generation G42})
;; => {:status :promoted :from G42 :to G43}
;; or {:status :stale :current G43a :expected G42}
```

Promotion transaction order:

```text
read candidate/evaluation
verify immutable eligibility
read CURRENT
compare CURRENT == candidate.parent
insert promotion decision
mark old active → superseded
mark new generation → active
CAS CURRENT pointer
append promotion event
COMMIT
```

- [ ] **Step 1: Write happy-path test.**
- [ ] **Step 2: Write two-candidate concurrency test: C1 and C2 share parent G42; once C1 promotes to G43, C2 must become stale rather than overwriting CURRENT.**
- [ ] **Step 3: Write transaction-failure injection tests at several points; after rollback there must still be exactly one active CURRENT generation.**
- [ ] **Step 4: Ensure promotion code never re-computes evaluator judgment from model text; it consumes finalized immutable eligibility data.**
- [ ] **Step 5: Implement and commit.**

**Acceptance:** Promotion is a database state transition, not “copy candidate files over production files.”

---

## Task 9.3 — Route new sessions by canary allocation without migrating old sessions

**Files:**

- Create: `src/evoclj/promotion/canary.clj`
- Modify: `src/evoclj/store/session.clj`
- Create: `test/evoclj/promotion/canary_test.clj`

**Interfaces:**

```clojure
(select-generation-for-new-session deployment-state stable-routing-key)
;; deterministic choice from configured allocation
```

Canary ladder default:

```text
10% → 25% → 50% → 100%
```

- [ ] **Step 1: Test deterministic routing by stable hash of session-routing key, not mutable global random state.**
- [ ] **Step 2: Test a session selected for G42 remains pinned to G42 after allocation changes.**
- [ ] **Step 3: Test G43 receives only the declared canary percentage over a sufficiently large deterministic key fixture.**
- [ ] **Step 4: Persist allocation version with each session decision so routing can be audited later.**
- [ ] **Step 5: Run and commit.**

---

## Task 9.4 — Evaluate online canary guardrails and automatic stop

**Files:**

- Create: `src/evoclj/promotion/monitor.clj`
- Create: `test/evoclj/promotion/monitor_test.clj`

**Online guardrails:**

```text
hard policy violation
unexpected provider denial surge
session failure rate
cost/task
latency/task
operator escalation rate
```

- [ ] **Step 1: Write a test where one hard safety violation immediately stops new sessions from entering the candidate.**
- [ ] **Step 2: Write a soft threshold test using a minimum sample count before acting on noisy failure-rate changes.**
- [ ] **Step 3: Stopping canary changes routing for future sessions only; record what happens to already-running candidate sessions according to profile (`:finish` or `:cancel`).**
- [ ] **Step 4: Persist stop reason and observed metrics as promotion evidence.**
- [ ] **Step 5: Run and commit.**

---

## Task 9.5 — Implement rollback semantics explicitly

**Files:**

- Create: `src/evoclj/promotion/rollback.clj`
- Create: `test/evoclj/promotion/rollback_test.clj`

**Interfaces:**

```clojure
(rollback! promotion-system
  {:from-generation G43
   :to-generation G42
   :reason :canary-regression})
```

- [ ] **Step 1: Test rollback changes only the generation chosen for future sessions.**
- [ ] **Step 2: Test all G43 events, episodes, external-effect receipts, and promotion records remain queryable.**
- [ ] **Step 3: Test rollback refuses a target whose Genome/artifacts fail integrity verification.**
- [ ] **Step 4: Test rollback does not invoke compensating external actions automatically. Any compensation must be a separately authorized operator/agent task.**
- [ ] **Step 5: Implement and commit.**

---

## Task 9.6 — Build lineage reconstruction

**Files:**

- Create: `src/evoclj/promotion/lineage.clj`
- Create: `test/evoclj/promotion/lineage_test.clj`

**Interfaces:**

```clojure
(lineage store generation-id)
;; => {:generation ...
;;     :parent ...
;;     :mutation ...
;;     :evidence ...
;;     :evaluation ...
;;     :promotion ...
;;     :children [...]}
```

- [ ] **Step 1: Construct fixture lineage `G1 → G2 rejected`, `G1 → G3 promoted`, `G3 → G4 promoted → rollback to G3`.**
- [ ] **Step 2: Test lineage reports rejected branches, not only winners.**
- [ ] **Step 3: Test every edge has the mutation/evaluation/promotion evidence needed to explain it.**
- [ ] **Step 4: Add integrity verification over referenced artifacts while reconstructing in strict mode.**
- [ ] **Step 5: Run and commit.**

---

## Task 9.7 — Complete end-to-end evolutionary promotion test

**Files:**

- Create: `test/evoclj/promotion/e2e_evolution_test.clj`
- Add fixture Genomes/eval cases under `test/fixtures/evolution-e2e/`

**Test scenario:**

```text
G1 route program chooses tool A for every request.
Evolution-set episodes show requests of class B fail with A.
Deterministic Diagnostician proposes hypothesis H.
Deterministic Mutator produces Δ changing the SCI router.
Candidate G2 uses B-tool only for B requests.
Replay proves old A requests still pass.
Hidden Selection contains both A and B cases.
G2 beats G1 above min-delta with no hard/cost regression.
Promotion CAS changes CURRENT from G1 to G2.
An already-running G1 session remains on G1.
A new session receives G2.
Lineage query explains G1 + evidence + Δ + evaluation → G2.
```

- [ ] **Step 1: Write the entire scenario as one black-box test using only public subsystem interfaces.**
- [ ] **Step 2: Ensure hidden Selection fixture is not reachable from Diagnostician/Mutator test objects.**
- [ ] **Step 3: Assert all expected events and artifacts exist.**
- [ ] **Step 4: Assert one failed/stale promotion branch can be introduced without corrupting the winning branch.**
- [ ] **Step 5: Restart the process/store and re-run lineage/current checks.**
- [ ] **Step 6: Commit.**

**Milestone 9 exit test:** EvoCLJ has a full self-evolution loop with a hard causal firewall between proposal and acceptance.

---

# Milestone 10 — Operator CLI and System Assembly

This milestone is not a new conceptual dependency in the requested chain; it assembles the completed subsystems into operable commands without bypassing their APIs.

## Task 10.1 — Wire stable host components with Integrant

**Files:**

- Create: `src/evoclj/kernel/system.clj`
- Create: `resources/system.edn`
- Create: `test/evoclj/kernel/system_test.clj`

**Integrant-owned host components:**

```text
:store/sqlite
:store/cas
:provider/registry
:capability/broker
:runtime/executor
:evolution/system
:eval/system
:promotion/system
```

Genome graph nodes are NOT Integrant components.

- [ ] **Step 1: Write system init/halt test with temporary DB/CAS.**
- [ ] **Step 2: Test `halt!` twice is safe.**
- [ ] **Step 3: Test reinitializing the host system reconstructs durable state from stores rather than retaining stale in-memory generation/session objects.**
- [ ] **Step 4: Keep constructors dependency-injected so unit tests can use fixture providers/stores.**
- [ ] **Step 5: Run and commit.**

---

## Task 10.2 — Implement CLI read/execute commands

**Files:**

- Create: `src/evoclj/cli/main.clj`
- Create: `src/evoclj/cli/genome.clj`
- Create: `src/evoclj/cli/session.clj`
- Create: `src/evoclj/cli/evolution.clj`
- Create: `src/evoclj/cli/promotion.clj`
- Create: `test/evoclj/cli/cli_test.clj`

**Required commands:**

```text
evoclj genome validate <path>
evoclj genome inspect <id-or-path>
evoclj genome diff <left> <right>

evoclj run --genome <id|current> --task <edn-file>
evoclj replay --session <uuid>

evoclj evolve --generation <id|current>
evoclj candidate list
evoclj candidate inspect <id>

evoclj eval <candidate-id> --profile <profile-id>
evoclj promote <candidate-id> --evaluation <id>
evoclj rollback --to <generation-id> --reason <keyword>

evoclj lineage <generation-id>
evoclj events --session <uuid>
evoclj capability inspect --session <uuid>
```

- [ ] **Step 1: Test read-only commands against fixture stores.**
- [ ] **Step 2: Test mutating commands call public Promotion/Evolution APIs and cannot directly update SQL current-pointer rows.**
- [ ] **Step 3: Emit machine-readable EDN by default with an optional concise human renderer.**
- [ ] **Step 4: Exit non-zero for typed failures and include `:error/type`.**
- [ ] **Step 5: Run and commit.**

---

# Milestone 11 — Adversarial and Integrity Release Gate

No v0 release should be called complete until the following tests execute in CI. They directly test the architectural claims rather than only happy-path behavior.

## Task 11.1 — Self-authority escalation suite

**Files:**

- Create: `test/evoclj/adversarial/authority_test.clj`
- Add malicious Genome fixtures under `test/fixtures/adversarial/authority/`

Cases:

```text
SCI attempts System/getenv.
SCI attempts java.io.File.
SCI attempts Runtime.exec/ProcessBuilder.
SCI tries slurp/spit/load-file/eval.
Genome adds a network tool descriptor without a network capability.
Genome requests broader filesystem scope than the host grant.
Child/future extension attempts to reuse another Phenotype's capability ID.
```

- [ ] **Step 1: Make every case fail closed before any protected effect occurs.**
- [ ] **Step 2: Assert denial is visible in audit events with normalized resource and reason.**
- [ ] **Step 3: Assert denial does not automatically grant a fallback broader tool.**

---

## Task 11.2 — Evaluation leakage and tampering suite

**Files:**

- Create: `test/evoclj/adversarial/eval_isolation_test.clj`

Cases:

```text
Mutator tries to read evals/selection.
Mutation targets evaluator configuration.
Candidate output guesses hidden test paths.
Candidate tries to modify replay fixture files.
Candidate tries to delete failing deterministic tests.
Verifier adapter accidentally receives Diagnostician private context.
```

- [ ] **Step 1: Check filesystem/mount/capability isolation, not merely prompts.**
- [ ] **Step 2: Assert any candidate mutation touching protected evaluator paths fails G2.**
- [ ] **Step 3: Add a constructor-level test proving evaluator-only dependencies cannot be obtained from evolution-system dependencies.**

---

## Task 11.3 — Concurrency and stale promotion suite

**Files:**

- Create: `test/evoclj/adversarial/concurrency_test.clj`

Cases:

```text
Two candidates evaluate concurrently from one parent.
Two workers call promote concurrently.
Rollback races with promotion.
Session creation races with CURRENT change.
```

Normative outcomes:

```text
Exactly one CURRENT generation after every transaction.
At most one sibling candidate wins a parent CAS.
Every created session records exactly one immutable generation.
No session's pinned generation changes after creation.
```

- [ ] **Step 1: Run the race tests repeatedly, not once.**
- [ ] **Step 2: Add deterministic barriers/latches in test adapters so the race windows are deliberate and reproducible.**
- [ ] **Step 3: Verify SQLite transaction behavior under the chosen journal/locking mode.**

---

## Task 11.4 — Crash/fault injection suite

**Files:**

- Create: `test/evoclj/adversarial/crash_recovery_test.clj`

Inject failure after:

```text
CAS artifact temp write
artifact rename before DB insert
session state transition
provider effect before result event
candidate materialization
final Evaluation persistence
promotion decision insert before CURRENT CAS
CURRENT CAS before outer transaction commit
```

- [ ] **Step 1: For each injection point, document the expected recoverable state in the test name/assertions.**
- [ ] **Step 2: Ensure recovery never invents a successful effect. Ambiguous non-idempotent external effects must be marked ambiguous/manual-review, not silently retried.**
- [ ] **Step 3: Ensure no partially written Genome is accepted as content-addressed valid content.**

---

## Task 11.5 — Mutation determinism and sandbox escape suite

**Files:**

- Create: `test/evoclj/adversarial/mutation_test.clj`

Cases:

```text
Patch path traversal.
Symlink inside candidate staging tree.
Wrong preimage hash.
Ambiguous text range.
rewrite-clj selector matches zero forms.
rewrite-clj selector matches multiple forms when uniqueness required.
Same parent+mutation applied 100 times.
SCI infinite loop after mutation.
SCI emits huge lazy/infinite data.
```

- [ ] **Step 1: All malformed patches fail before candidate registration.**
- [ ] **Step 2: All valid repeated applications yield one candidate Genome hash.**
- [ ] **Step 3: Runtime exhaustion is contained to candidate/session and cannot block evaluator/kernel threads indefinitely.**

---

# Milestone 12 — Observability and Performance Baseline

Performance optimization happens only after correctness, but v0 needs measurement so later evolution cannot quietly purchase tiny quality gains with explosive cost.

## Task 12.1 — Standard usage accounting

**Files:**

- Create: `src/evoclj/runtime/usage.clj`
- Create: `test/evoclj/runtime/usage_test.clj`

Track at minimum:

```text
wall-ms
model-input-tokens
model-output-tokens
model-cost-units/provider-reported-cost
provider-calls
tool-calls
network-bytes when known
SCI steps/interruption checks
artifact bytes written
```

- [ ] **Step 1: Usage accumulates monotonically within a session.**
- [ ] **Step 2: Child/provider usage is attributed to the originating Intent/session/node.**
- [ ] **Step 3: Evaluation can aggregate usage per case and per successful task.**

---

## Task 12.2 — Establish benchmark fixtures and regression ceilings

**Files:**

- Create: `test/evoclj/perf/runtime_benchmark_test.clj`
- Create: `docs/performance-baseline.md`

Measure rather than prematurely optimize:

```text
Genome load/hash time
compile time
SCI invocation overhead
broker authorization overhead
append-event throughput
CAS small/large artifact throughput
seed end-to-end task latency excluding model network
candidate evaluation orchestration overhead
```

- [ ] **Step 1: Record baseline environment and fixture sizes.**
- [ ] **Step 2: Set broad regression ceilings only for pathological changes, not microbenchmark vanity.**
- [ ] **Step 3: Keep correctness tests independent from timing-sensitive assertions where possible.**

---

# Detailed Public Data Contracts

The following names should remain stable after Milestone 6 unless a deliberate ABI change is made.

## `LoadedGenome`

```clojure
{:genome/id GenomeId
 :genome/root string-or-path-metadata
 :manifest map?
 :files
 {string?
  {:artifact/id ArtifactId
   :digest GenomeId
   :kind #{:edn :text :clj :binary}
   :size int?}}}
```

No live InputStreams or mutable path handles are part of the persisted/portable representation.

## `CompiledGenome`

```clojure
{:compiled/genome-id GenomeId
 :compiled/resolution-id ResolutionId
 :compiled/phenotype-id PhenotypeId
 :abi {:kernel int? :genome int? :intent int? :tool int?}
 :manifest map?
 :topology map?
 :programs {keyword? map?}
 :requested-capabilities set?
 :resolution map?}
```

## `Session`

```clojure
{:session/id uuid?
 :generation/id uuid-or-stable-id
 :genome/id GenomeId
 :resolution/id ResolutionId
 :phenotype/id PhenotypeId
 :state keyword?
 :created-at inst?
 :routing {:deployment-version string?
           :bucket int?}}
```

Pinned identity fields are immutable after insert.

## `Intent`

```clojure
{:intent/id uuid?
 :intent/type keyword?
 :session/id uuid?
 :phenotype/id PhenotypeId
 :node/id keyword?
 :cause/event-id int?
 :payload map?
 :budget map?
 :metadata map?}
```

## `CapabilityLease`

```clojure
{:cap/id uuid?
 :subject {:phenotype/id PhenotypeId}
 :resource map?
 :actions set?
 :constraints map?
 :issued-at inst?
 :expires-at inst?}
```

## `Event`

```clojure
{:event/id int?
 :event/seq int?
 :session/id uuid?
 :generation/id stable-id?
 :phenotype/id PhenotypeId
 :event/type keyword?
 :cause/event-id int-or-nil?
 :payload-ref ArtifactId-or-nil
 :prev-hash string-or-nil?
 :event-hash string?
 :created-at inst?
 :metadata map?}
```

## `Episode`

```clojure
{:episode/id uuid?
 :session/id uuid?
 :generation/id stable-id?
 :genome/id GenomeId
 :resolution/id ResolutionId
 :task-ref ArtifactId
 :trace {:first-event int? :last-event int?}
 :outcome map?
 :usage map?}
```

## `Mutation`

```clojure
{:mutation/id uuid?
 :parent/genome-id GenomeId
 :hypothesis/id uuid?
 :evidence/id ArtifactId
 :risk #{:parameter :behavioral :program :topology :meta}
 :ops vector?
 :expected-effect map?}
```

## `Candidate`

```clojure
{:candidate/id uuid?
 :parent/generation-id stable-id?
 :parent/genome-id GenomeId
 :candidate/genome-id GenomeId
 :mutation/id uuid?
 :evidence/id ArtifactId
 :risk keyword?
 :state keyword?
 :created-at inst?}
```

## `Evaluation`

```clojure
{:evaluation/id uuid?
 :candidate/id uuid?
 :parent/generation-id stable-id?
 :profile/id keyword?
 :gates vector?
 :paired-results-ref ArtifactId-or-nil
 :summary {:hard map? :utility map? :cost map? :complexity map?}
 :eligibility {:eligible? boolean? :reasons vector?}
 :created-at inst?}
```

## `Promotion`

```clojure
{:promotion/id uuid?
 :candidate/id uuid?
 :evaluation/id uuid?
 :from-generation stable-id?
 :to-generation stable-id?
 :decision keyword?
 :reason map?
 :created-at inst?}
```

---

# Event Taxonomy

Use namespaced keywords. Do not encode semantics into free-form message text.

Minimum event types:

```text
:session/created
:session/started
:session/waiting
:session/resumed
:session/completed
:session/failed
:session/cancelled
:session/budget-exhausted

:node/started
:node/completed
:node/failed

:sci/invoked
:sci/completed
:sci/failed
:sci/limit-exceeded

:intent/proposed
:intent/normalized
:intent/authorized
:intent/denied
:intent/completed
:intent/failed

:provider/call-started
:provider/call-completed
:provider/call-ambiguous

:memory/read
:memory/write

:evolution/evidence-frozen
:evolution/diagnosis-created
:evolution/mutation-proposed
:evolution/candidate-materialized
:evolution/candidate-invalid

:eval/started
:eval/gate-passed
:eval/gate-failed
:eval/completed

:promotion/canary-started
:promotion/canary-stopped
:promotion/promoted
:promotion/stale
:promotion/rejected
:promotion/rollback
```

Every effect-bearing event should include an artifact reference to normalized request/result details rather than dumping arbitrary model/tool output into event metadata.

---

# Database Invariants

These should be represented as SQL constraints where practical and verified again by application tests.

1. `artifacts.hash` unique.
2. A session's `(genome_id, resolution_id, phenotype_id)` never changes.
3. `(session_id, event_seq)` unique and monotonically allocated.
4. A finalized Evaluation is immutable; reruns create new IDs.
5. A Promotion references exactly one finalized Evaluation.
6. `CURRENT` has exactly one row/value.
7. An active generation's Genome must exist in CAS/Genome store and pass integrity check at activation time.
8. Candidate parent generation and parent Genome must agree with the stored generation record.
9. A promoted candidate cannot later be rewritten to reference a different mutation/evaluation.
10. Append-only event semantics are enforced by exposing no ordinary update/delete application API; operator retention tooling, if added later, must be a separate privileged subsystem.

---

# Transaction Boundaries

## Intent effect transaction

Do not wrap a remote side effect and SQLite commit in one fictitious ACID transaction. Instead use an explicit effect protocol:

```text
persist intent proposed
persist normalized request
persist authorization decision
persist provider-call-started with idempotency key
perform external effect
persist completed result OR ambiguous outcome
```

If the process dies after an irreversible external effect but before recording success, recovery marks the call ambiguous unless the provider supports idempotent lookup/reconciliation. It MUST NOT blindly retry a non-idempotent action.

## Candidate materialization transaction

```text
create staging directory
copy/link immutable parent files safely
apply declarative patch
load + validate + hash candidate
put candidate tree/artifacts into immutable storage
insert candidate row
rename/finalize staging reference
```

Failure before candidate row means no valid Candidate exists. Orphan staging directories may be garbage-collected.

## Evaluation finalization transaction

```text
all gate/case artifacts already durable
insert final summary
mark Evaluation finalized
update Candidate → evaluated
commit
```

## Promotion transaction

As specified in Task 9.2; it must be the only code path that changes CURRENT.

---

# Security Boundary Checklist

Before release, manually inspect code for each item:

- SCI receives no provider object, JDBC connection, filesystem root object, secret map, or capability store.
- Mutation adapters receive no Selection/Audit dataset loader.
- Provider secrets are constructor-private and redacted before artifact/event persistence.
- Resource normalization occurs before capability matching.
- Capability comparison uses canonical resource identity, not model-provided display names.
- Generated tools, if introduced later, still emit/enter broker-mediated Intents rather than obtaining host objects.
- Kernel modules are outside all Genome mutable roots.
- Candidate workspaces are separate from current-generation immutable content and evaluator hidden data.
- `clojure.core/eval`, unrestricted `read-string`, shell execution, and unrestricted Java reflection are absent from Genome execution paths.
- No “confirm?” UI, prompt rule, or tool visibility check is treated as the underlying authorization boundary.

---

# TDD/Commit Discipline

For every task above:

1. Add the smallest failing test demonstrating the contract.
2. Run that focused test and observe the intended failure.
3. Implement the minimum behavior required.
4. Run the focused test.
5. Run all tests for the current milestone and its dependencies.
6. Commit one reviewable behavior change.
7. Do not combine an architectural refactor with an unrelated feature in the same commit.

Recommended commit progression:

```text
feat: establish EvoCLJ domain error and id contracts
feat: define immutable genome manifest schemas
feat: add canonical genome hashing
feat: load immutable genome bundles
feat: resolve runtime model aliases
feat: compile topology IR
feat: validate evolvable program descriptors
feat: compile genomes into phenotype descriptors
feat: add restricted SCI contexts
feat: enforce SCI data boundary
feat: bound SCI execution
feat: define intent ABI
feat: add capability lease model
feat: register normalized providers
feat: authorize broker requests
feat: dispatch capability scoped intents
feat: initialize durable runtime store
feat: add content addressed artifacts
feat: append causal audit events
feat: persist pinned sessions
feat: recover runtime integrity after restart
feat: instantiate isolated phenotypes
feat: execute graph node transitions
feat: schedule bounded sessions
feat: add explicit loop semantics
feat: materialize episodes
feat: run seed genome end to end
feat: freeze evolution evidence packs
feat: add structured diagnosis contract
feat: define mutation IR
feat: apply deterministic genome patches
feat: enforce mutation budgets
feat: persist immutable candidates
feat: retain rejected mutation history
feat: orchestrate candidate proposals
feat: separate evaluation datasets
feat: add static evaluation gates
feat: replay historical behavior
feat: run paired hidden selection
feat: compare hard utility and cost metrics
feat: finalize candidate evaluations
feat: model promotion states
feat: atomically promote current generation
feat: route canary sessions
feat: stop regressing canaries
feat: implement explicit rollback
feat: reconstruct evolutionary lineage
feat: verify end to end self evolution
```

---

# Recommended Implementation Batches

These batches are review boundaries, not a license to skip per-task commits.

## Batch A — Immutable substrate

Tasks 1.1–2.4.

**Review questions:**

- Is every Genome/Resolution identity deterministic?
- Can any loader path execute source?
- Are paths canonical and symlink-safe?
- Is CompiledGenome pure serializable data?

**Demo:** `genome validate` equivalent internal call loads and compiles seed Genome repeatedly to the same IDs.

## Batch B — Safe computation/effects

Tasks 3.1–4.5.

**Review questions:**

- Can SCI obtain ambient authority?
- Is normalized resource checked before authorization?
- Does tool visibility differ from permission?
- Can a denied request cause any provider effect?

**Demo:** malicious SCI access attempts fail; fixture Intent succeeds only with a matching lease.

## Batch C — Durable execution

Tasks 5.1–6.6.

**Review questions:**

- Are all effects attributable and recoverable?
- Are session pins immutable?
- Does restart preserve truth without preserving stale live objects?
- Does the seed task execute strictly through Intent/Broker?

**Demo:** seed task completes; process restarts; episode/trace remains valid.

## Batch D — Evolution without power to self-approve

Tasks 7.1–7.8.

**Review questions:**

- Is evidence frozen?
- Are mutations declarative, bounded, deterministic, and preconditioned?
- Can a candidate touch Kernel/evaluator roots?
- Can evolution alter CURRENT?

**Demo:** Episodes create immutable G2 candidate while G1 stays current.

## Batch E — Independent judgment

Tasks 8.1–8.7.

**Review questions:**

- Is Selection physically unavailable to proposal actors?
- Is parent re-evaluated contemporaneously?
- Are hard constraints lexically dominant?
- Can evaluation results be rewritten?

**Demo:** one bad candidate fails G2; one good candidate becomes eligible after paired hidden evaluation.

## Batch F — Governed activation

Tasks 9.1–9.7 plus adversarial release gate.

**Review questions:**

- Is CURRENT changed in exactly one subsystem?
- Does sibling promotion race stale correctly?
- Does rollback preserve external-effect history?
- Can lineage answer “why is this generation running?”

**Demo:** full G1→G2 evolution and promotion, concurrent stale candidate, restart, lineage reconstruction.

---

# Definition of Done by Milestone

| Milestone | Required observable proof |
| --- | --- |
| Genome | Same logical tree → same hash; path/symlink attacks rejected |
| Compiler | Seed Genome → stable pure CompiledGenome/Phenotype ID |
| SCI Runtime | Pure router runs; host/JVM escape attempts fail; infinite loop terminates |
| Intent/Broker | Visible ungranted tool denied without effect; granted fixture tool succeeds |
| Event Store | Full trace survives restart; tamper/missing artifact detected |
| Executor | Seed task completes through graph/Intent/Broker with pinned session |
| Evolution | Episodes deterministically produce immutable bounded Candidate |
| Evaluator | Candidate and parent receive isolated paired evaluation; hard gate dominates |
| Promotion | Eligible candidate atomically becomes current; stale sibling cannot overwrite |
| Canary/Rollback | New-session routing changes without mutating old sessions/history |
| Release | Adversarial, race, crash, and mutation determinism suites pass |

---

# Explicit v0 Non-Goals

Do not expand scope during this plan to include:

- LLM weight fine-tuning or RL training.
- Arbitrary generated JVM/native code.
- Arbitrary shell access from SCI.
- Multi-host/distributed consensus.
- Networked P2P Genome exchange.
- Persistent database schema self-evolution.
- Automatic capability enlargement by the agent.
- Simultaneous candidate and evaluator co-evolution.
- Genetic crossover/population algorithms beyond at most three sibling candidates.
- Autonomous Kernel source updates.
- Automatic compensation/undo of real-world side effects.
- General cyclic actor graphs; use explicit bounded loop nodes.
- High-scale multi-agent spawning before single-session semantics are stable.
- A scalar “AGI fitness score.”

---

# Post-v0 Extension Order

Only after the full release gate passes:

1. Add real model-provider adapters behind the established `Provider` boundary.
2. Add real filesystem/read-only repository tools with canonical resource scopes.
3. Add semantic-memory retrieval policy as an evolvable Genome module while keeping memory storage/kernel schema fixed.
4. Add bounded topology mutation beyond the seed graph.
5. Add model-routing evolution and explicit compute-cost objectives.
6. Add child-agent/spawn Intent with the invariant `child authority ⊆ parent authority` and hierarchical budget transfer.
7. Add R4 slow/meta evolution for mutation strategy under a separate meta-evaluation profile.
8. Add delayed evaluator evolution with an audit corpus and causal firewall; never let `V'` judge the same candidate that produced `V'`.
9. Consider WASM/Datalog/other executable Genome assets only if they preserve the same data, capability, provenance, and evaluation contracts.

---

# Final Release Acceptance Scenario

A release candidate is acceptable only if an operator can perform and verify this sequence from a clean checkout and empty state directory:

```text
1. Initialize SQLite/CAS.
2. Validate and import seed Genome G1.
3. Resolve provider aliases to R1 and compile phenotype identity P1.
4. Set G1 as seed/current through the trusted bootstrap path.
5. Execute a set of Evolution-set fixture tasks under G1.
6. Inspect persisted Episodes and causal event chains.
7. Start one evolution cycle with a fixed evidence cutoff.
8. Observe a structured Diagnosis and bounded Mutation Δ.
9. Materialize immutable candidate G2; verify G1 bytes/hash unchanged.
10. Run G0–G4 without exposing hidden Selection data to evolution actors.
11. Run paired G1/G2 hidden Selection under fresh isolated Phenotypes.
12. Produce immutable Evaluation E with hard/utility/cost/complexity sections.
13. If eligible, begin canary or promote using expected parent G1.
14. In parallel, attempt sibling candidate promotion from G1 and observe stale rejection.
15. Verify a pre-existing G1 session remains pinned to G1.
16. Verify new routing selects G2 according to deployment state.
17. Trigger a synthetic canary regression and stop/rollback future G2 routing.
18. Confirm historical G2 external-effect/audit records remain untouched.
19. Restart the entire JVM/application.
20. Verify CURRENT, generations, candidate/evaluation history, CAS integrity, and event chains.
21. Run `lineage` and reconstruct parent + evidence + mutation + evaluation + decision for every branch.
22. Run the adversarial suite and observe zero unauthorized fixture side effects.
```

At that point, the project has demonstrated the claim it is built around: **the agent may evolve the program that proposes future behavior, but only the fixed kernel can decide whether that proposed successor is valid, authorized, evaluated, and active.**

---

# Repo Conventions (orchestrator addendum)

These rules are binding for every implementation agent working in this repository:

1. `deps.edn` at the repo root is PRE-PROVISIONED with the complete declared stack: clojure 1.12.5, malli 0.20.1, integrant 1.0.1, sci 0.15.58, rewrite-clj 1.2.55, sqlite-jdbc 3.49.1.0, java.jdbc 0.7.12, and the cognitect test-runner under the `:test` alias. Do NOT remove or change dependency entries; add a new dependency only if your task section explicitly requires one.
2. Test commands: full suite `clojure -M:test`; focused `clojure -M:test -n <namespace>` (the pinned cognitect test-runner v0.5.1 has no --focus; -n selects a namespace, -r takes a regex pattern for groups of namespaces).
3. Implementation agents never run `git commit`. A separate reviewer agent verifies the work and commits it with the exact message listed in "Recommended commit progression" for the task.
4. Repo root on this host is `D:/PROJECTS/EvoCLJ` (Git Bash path `/d/PROJECTS/EvoCLJ`). Windows host: never use CRLF line endings; keep all sources UTF-8 with LF.
5. The plan's Global Constraints, Data Contracts, Event Taxonomy, and Transaction Boundaries are normative; when a task's text and a contract conflict, the Global Constraints win and the deviation must be reported.
6. Only touch the files listed in your task's Files section.
