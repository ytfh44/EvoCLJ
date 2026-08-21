-- 003-routing.sql — session routing audit columns (component)
--
-- Design decision (component Step 4): every NEW session persists the
-- allocation version and the stable routing bucket that decided its
-- generation, so routing can be audited later. These columns are
-- ADDITIVE: existing rows keep NULL (they were created before routing
-- was persisted — the pre-Task-9.3 deviation documented in
-- evoclj.store.session) and no existing column is rewritten.
--
-- The migration runner applies this file once and records it in the
-- meta table's applied_migrations set; re-running migrate! is a no-op.
-- ALTER TABLE ... ADD COLUMN is not reversible, which is exactly what
-- an append-only schema history wants: the columns exist from this
-- version onward.

ALTER TABLE sessions ADD COLUMN routing_deployment_version TEXT;
ALTER TABLE sessions ADD COLUMN routing_bucket INTEGER;

-- Backs audit queries over a deployment version's decisions.
CREATE INDEX sessions_routing_idx
  ON sessions (routing_deployment_version, routing_bucket);
