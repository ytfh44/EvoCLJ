-- 020-subagent-links.sql — historical topology DDL retained for upgrades
-- Design decisions (clean cutover):
--   * Existing databases keep this table so migration history remains valid.
--   * Runtime topology is stored only in works.parent_work_id; runtime code never
--     reads or writes subagent_links.
--   * Columns are preserved unchanged for upgrade compatibility.
--   * This migration replaces the former inline CREATE TABLE helper; it is
--     intentionally schema-only and has no active read path.
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