# Evolution dataset — Task 9.7 fixture

The Evolution dataset root for the end-to-end evolutionary promotion
test. In this e2e the evolution EVIDENCE is the generation's store
episodes (frozen by `evoclj.evolution.evidence/build-evidence-pack`
from the executor's SQLite/CAS), so this root carries no case files —
it exists so the physical dataset separation contract
(`evoclj.eval.dataset` dataset-roots: evolution/selection/audit are
THREE DISTINCT roots) and the candidate-workspace staging
(`dataset/build-candidate-workspace!` mounts ONLY this dataset) have a
real evolution root to stage from.

The Selection dataset (test/fixtures/evolution-e2e/selection) is never
mounted here and never staged into a candidate workspace (Global
Constraints 11, 23).
