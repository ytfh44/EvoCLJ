-- 020-subagent-links.sql — promote subagent_links DDL to migration
-- Design decisions (clean):
--   * subagent_links is the session-level child->parent helper used by
--     evoclj.store.session/list-descendants and the cancel cascade that
--     unions subagent_links with the Work-graph (works.parent_work_id).
--     The Work graph is the durable spawn truth (W2); subagent_links is
--     the compat mirror kept for fast BFS over sessions.
--   * Columns: child_session_id PRIMARY KEY, parent_session_id FK to
--     sessions(id) ON DELETE CASCADE, created_at ISO-8601.
--   * Migration makes the CREATE TABLE previously inlined in
--     evoclj.runtime.subagent/ensure-subagent-link-table! non-runtime;
--     that helper is deleted. Read paths (get-parent-session-id,
--     child-session-ids, cancel cascade) now hit a table that the
--     migration runner creates once at bootstrap.
--   * Idempotency: CREATE TABLE IF NOT EXISTS, CREATE INDEX IF NOT EXISTS.
--   * Single transaction via the runner.

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS subagent_links (
  child_session_id  TEXT PRIMARY KEY,
  parent_session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
  created_at        TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS subagent_links_parent_idx
  ON subagent_links(parent_session_id);