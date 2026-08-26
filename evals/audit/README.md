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

## Empty dataset behavior (fail closed)

This dataset is currently **empty** (no `*.edn` case file is present
beyond this manifest). An operator audit run against an empty audit
dataset must NOT silently report nothing: the operator-only accessor
`evoclj.eval.dataset/audit-cases` fails closed with the explicit typed
`:dataset/empty` marker (carrying the `:dataset/source` and `:case-count
0`) rather than yielding an empty case set. The marker is observable and
serializable (Global Constraint 22). Seed this dataset with at least one
case file before running an operator audit.

## Boundary contract

- The audit set is reachable only through the explicit operator-only
  accessor `evoclj.eval.dataset/audit-cases`.
- Automated evolution pipelines never mount, read, or reference this
  dataset. Its absence from evolution execution is enforced
  architecturally (`evolution-input` is pure data with no fn values;
  `build-candidate-workspace!` stages only the Evolution dataset).
