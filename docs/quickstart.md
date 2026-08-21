# EvoCLJ Quickstart — the 30-minute demo (component)

This is the hands-on demo of the self-evolving loop. With the built-in
`:demo` config profile, EvoCLJ runs the **whole evolution cycle
headless** — no language model, no API keys, no config files: a fresh
state dir is enough.

The `:demo` profile injects, through the CLI's existing host injection
path (the `:overrides` seam in `evoclj.cli.session/build-config`):

- a **built-in heuristic mutator** (`evoclj.evolution.demo-mutator`) —
  deterministic template/function-swap mutations over the seed genome's
  own mutable program file (`programs/route.clj`); and
- the demo's **hidden selection cases and fixture providers** the
  evaluator needs to judge the candidate.

The demo runs one `cycle` command that walks evolve → eval → promote
through the public subsystem APIs and atomically moves `CURRENT` to the
promoted candidate (Global Constraint 15 — the pointer moves only inside
`promotion.promote/promote!`'s compare-and-set transaction).

## Requirements

- JDK 17+ and the Clojure CLI (`clojure`) on the `PATH`.
- Repo root as the working directory (the demo uses the real
  `genomes/seed` bundle).

## Step 0 — provision a fresh state dir (one-time, ~1 minute)

There is no `provision` command on purpose (v0 hosts provision state
dirs exactly like the tests do). Run this once from the repo root to
turn `genomes/seed` into `generation-1` of a brand-new state dir:

```bash
clojure -M -e "
(require '[clojure.java.io :as io]
         '[clojure.java.jdbc :as jdbc]
         '[clojure.string :as str]
         '[evoclj.cli.session :as session]
         '[evoclj.compiler.core :as compiler]
         '[evoclj.genome.load :as load]
         '[evoclj.genome.path :as gpath]
         '[evoclj.store.cas :as cas]
         '[evoclj.store.migrate :as migrate]
         '[evoclj.store.sqlite :as sqlite])
(import '(java.nio.file Files Paths LinkOption FileVisitOption)
        '(java.nio.file.attribute FileAttribute)
        '(java.nio.charset StandardCharsets))
(let [state-dir \"demo-state\"
      db-path (str state-dir \"/db/evoclj.db\")
      dash (fn [id] (str/replace id \":\" \"-\"))
      _ (Files/createDirectories (Paths/get (str state-dir \"/db\")
                                            (make-array String 0))
                                 (make-array FileAttribute 0))
      loaded (assoc (load/load-genome \"genomes/seed\")
                    :programs [session/route-descriptor])
      compiled (compiler/compile-genome loaded session/provider-catalog)
      genome-id (:compiled/genome-id compiled)
      body (apply str (map (fn [[p {:keys [digest]}]]
                             (str p \"\\u0000\" digest \"\\n\"))
                           (sort-by (fn [[p _]] p)
                                    gpath/bytewise-compare (:files loaded))))]
  (migrate/migrate! (sqlite/spec db-path))
  (jdbc/insert! (sqlite/spec db-path) :generations
                {:id \"generation-1\"
                 :genome_id genome-id
                 :resolution_id (:compiled/resolution-id compiled)
                 :parent_id nil
                 :state \"active\"
                 :current 1
                 :created_at \"2025-01-01T00:00:00Z\"})
  (cas/put-bytes! (cas/->cas (str state-dir \"/cas\"))
                  (.getBytes body StandardCharsets/UTF_8)
                  {})
  (let [from (Paths/get \"genomes/seed\" (make-array String 0))
        to (Paths/get (str state-dir \"/genomes/\" (dash genome-id))
                      (make-array String 0))]
    (with-open [stream (Files/walk from (make-array FileVisitOption 0))]
      (doseq [p (iterator-seq (.iterator stream))]
        (let [target (.resolve to (.relativize from p))]
          (when (Files/isDirectory p (make-array LinkOption 0))
            (Files/createDirectories target (make-array FileAttribute 0)))
          (when (Files/isRegularFile p (make-array LinkOption 0))
            (Files/createDirectories (.getParent target)
                                     (make-array FileAttribute 0))
            (Files/copy p target (make-array java.nio.file.CopyOption 0)))))))
  (println \"demo state provisioned at\" state-dir \"genome\" genome-id))
"
```

This provisions: the SQLite store (`demo-state/db/evoclj.db`) with the
`generation-1` row pinned to the seed genome's content address
(`current = 1`), the seed's canonical body in the CAS, and the seed
bundle under `demo-state/genomes/<id-as-dash>`.

## Step 1 — run the whole loop headless (~2–5 minutes)

```bash
EVOCLJ_STATE_DIR=./demo-state EVOCLJ_PROFILE=demo \
  clojure -M -m evoclj.cli.main cycle
```

(`cycle` is one operator command that runs evolve → eval → promote.
The config profile comes from the `EVOCLJ_PROFILE` env var — the CLI
itself never guesses which profile to use.)

In one invocation the demo:

1. **evolves** — freezes the (empty) evolution-set evidence, runs the
   deterministic pattern Diagnostician, then asks the demo mutator for
   candidates. The mutator proposes three deterministic template swaps
   of the seed's routing `case` (a `:routing/echo-b` improvement plus
   two semantically-neutral fallback swaps), all with the
   kernel-computed `:expect/hash` preimage digests. Each proposal is
   validated, applied, and compiled — the compiler topology gate
   passes for every candidate (only the mutable route program changes;
   `topology.edn` is never touched).
2. **evaluates** — every candidate runs the full component pipeline
   against the demo's hidden selection cases (`:sel/demo-echo`,
   `:sel/demo-echo-b`) with the demo's fixture providers
   (`:fixture/echo`, `:fixture/echo-b`). The seed parent passes only
   `:echo`; the `:routing/echo-b` candidate passes both, so its
   utility delta clears the profile threshold and it is **eligible**.
   The two fallback-swap candidates do not beat the parent and are
   recorded as ineligible — real negative evidence.
3. **promotes** — the eligible candidate's Genome becomes
   `generation-2` and `CURRENT` moves atomically
   (compare-and-set inside `promote/promote!`).

The command prints a structured EDN report:

```clojure
{:generation/id "generation-1"
 :phases
 {:evolve {:run? true
           :candidates [{:candidate/id #uuid "…"
                         :parent/generation-id "generation-1"
                         :parent/genome-id "sha256:…"
                         :candidate/genome-id "sha256:…"
                         :mutation/id #uuid "…"
                         :evidence/id "sha256:…"
                         :risk :program
                         :state :evaluation-pending} …]}
  :eval [{:candidate/id #uuid "…"
          :evaluation/id #uuid "…"
          :eligibility {:eligible? true :reasons []}} …]
  :promote [{:candidate/id #uuid "…"
             :status :promoted
             :outcome {:status :promoted
                       :from "generation-1"
                       :to "generation-2"}}]}}
```

## Step 2 — inspect what happened (optional, read-only)

```bash
EVOCLJ_STATE_DIR=./demo-state clojure -M -m evoclj.cli.main candidate list
EVOCLJ_STATE_DIR=./demo-state clojure -M -m evoclj.cli.main candidate inspect <candidate-id> --diff
EVOCLJ_STATE_DIR=./demo-state clojure -M -m evoclj.cli.main lineage generation-1
EVOCLJ_STATE_DIR=./demo-state clojure -M -m evoclj.cli.main lineage generation-2
```

`candidate inspect <id> --diff` shows the per-file line diff of the
candidate vs its parent — for the promoted candidate it is exactly the
routing `case` swap in `programs/route.clj`. `lineage` reconstructs the
generation history with the promotion decision and evaluation evidence.

## Step 3 — re-run (idempotent, optional)

Running `cycle` again proposes the same deterministic mutations (same
parent bytes + same mutation value → same candidate hashes); the
candidate uniqueness rule dedupes them to the same auditable rows, so
the demo is reproducible run after run.

## What the demo proves

- **Deterministic structured evolution** (Global Constraints 4, 6): a
  non-LLM heuristic mutator proposes bounded, hash-preimage-guarded
  patches; identical inputs always produce identical candidates.
- **Compiler topology validation** gates every candidate before it is
  ever persisted.
- **Evaluation is paired and hidden** (Global Constraints 11, 13): the
  parent and the candidate run the same hidden selection cases; a
  candidate is promoted only when it honestly beats the parent
  (hard gates first, then utility).
- **Atomic promotion** (Global Constraint 15): `CURRENT` moves only
  through the CAS compare-and-set; a re-run can never clobber it.

## Troubleshooting

- **`:config/profile-not-found`** — the `:demo` profile is built-in:
  it needs no config file, but the profile must be *selected*
  (`EVOCLJ_PROFILE=demo`). Without it the CLI ships the v0 no-op
  mutator and `evolve` proposes nothing.
- **`no CURRENT generation`** — the state dir was not provisioned
  (Step 0). `recovery` (`evoclj recovery`) is the read-only integrity
  report.
- **Windows / Git Bash** — paths above use forward slashes and work in
  Git Bash; the state dir lives wherever `EVOCLJ_STATE_DIR` points.
- The demo is offline: no model endpoint, no API keys. The `cycle`
  report's `:promote` entry is the only place `CURRENT` moves.
