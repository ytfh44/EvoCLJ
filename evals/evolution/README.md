# Evolution dataset

**Source keyword:** `:evals/evolution` · **Visibility:** evolution loop (Diagnostician, Mutator adapters)

This directory is the **Evolution dataset** root — the evidence the
evolution loop reads to propose candidate mutations. It is physically
separated from the Selection and Audit datasets (Global Constraint 11):
candidate evaluation uses an informationally isolated selection set
unavailable to Executor, Diagnostician, and Mutator, and this separation
is enforced by directory boundaries, not by instruction.

## Contents

- `README.md` — this manifest.
- `*.edn` — case files; each holds one case map `{:case/id <keyword> ...}`.

## Boundary contract

- Evolution adapters receive only **artifact refs** (`{:case/id k
  :artifact-ref "sha256:..."}`) copied into their evidence pack —
  never case bodies and never a dataset loader handle
  (`evoclj.eval.dataset/evolution-case-refs`, `evolution-input`).
- Candidate workspaces mount **only** this dataset
  (`evoclj.eval.dataset/build-candidate-workspace!`); the Selection and
  Audit directories are never staged into a workspace.
- Case bodies in this root are evolution evidence. The Selection
  dataset's bodies are reachable only via the evaluator-only
  `selection-loader`; the Audit dataset is operator-only
  (`audit-cases`) and absent from ordinary automated evolution.
