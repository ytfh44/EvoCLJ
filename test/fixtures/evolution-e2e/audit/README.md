# Audit dataset — Task 9.7 fixture

The Audit dataset root for the end-to-end evolutionary promotion
test. The audit set is OPERATOR-only and absent from ordinary
automated evolution execution entirely: it never appears in evolution
input, in evolution refs, or in a candidate workspace (Global
Constraint 11 — the audit root exists so the three datasets stay
physically separated and the profile's `:audit-set :visibility
:operator-only` declaration resolves).
