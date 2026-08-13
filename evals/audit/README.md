# Audit dataset

**Source keyword:** `:evals/audit` · **Visibility:** `:operator-only`

This directory is the **Audit dataset** root — operator-run audit cases.
It is physically separated from the Evolution and Selection datasets
(Global Constraint 11) and is **absent from ordinary automated evolution
execution entirely**: it never appears in the evolution input, in
evolution artifact refs, or in any candidate workspace.

## Contents

- `README.md` — this manifest.
- `*.edn` — case files; each holds one case map `{:case/id <keyword> ...}`.

## Boundary contract

- The audit set is reachable only through the explicit operator-only
  accessor `evoclj.eval.dataset/audit-cases`.
- Automated evolution pipelines never mount, read, or reference this
  dataset. Its absence from evolution execution is enforced
  architecturally (`evolution-input` is pure data with no fn values;
  `build-candidate-workspace!` stages only the Evolution dataset).
