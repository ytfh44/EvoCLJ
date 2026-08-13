# Selection dataset

**Source keyword:** `:evals/selection` · **Visibility:** `:kernel-only` (evaluator kernel)

This directory is the **Selection dataset** root — the paired-comparison
case set used to judge candidates. It is physically separated from the
Evolution and Audit datasets (Global Constraint 11): candidate
evaluation uses an informationally isolated selection set unavailable to
Executor, Diagnostician, and Mutator, and this separation is enforced by
directory boundaries, not by instruction.

## Contents

- `README.md` — this manifest.
- `*.edn` — case files; each holds one case map `{:case/id <keyword> ...}`.

## Boundary contract

- Selection case **bodies are loaded only by evaluator code**, after
  candidate materialization, through the loader returned by
  `evoclj.eval.dataset/selection-loader`. That loader is the only API
  surface that reveals these bodies, and it is never part of the
  evolution boundary: the `evoclj.evolution.*` namespaces have no
  dependency on `evoclj.eval.dataset`, and `evolution-input` /
  `evolution-case-refs` never carry selection bodies, refs, or loader
  handles.
- This dataset is **never mounted into a candidate workspace**.
- Expected outputs and verifier internals for these cases must never
  reach the Mutator; the Mutator receives only post-evaluation
  aggregate diagnostics.
