-- 018-work.sql — W1: Work unified lifecycle (queued/running/waiting/succeeded/failed/cancelled/timed-out)
-- Design decisions (W1, break compat, clean):
--   * Work is the single durable lifecycle (7 states). Session becomes immutable context (pin).
--   * Table `works` replaces `commands` + part of `subagent_sessions` (subagent = child Work + Principal + causal-links).
--   * Columns: id TEXT PK uuid, type TEXT NOT NULL, state TEXT CHECK 7, session_id TEXT FK, parent_work_id TEXT self-FK, payload_ref TEXT, deadline TEXT, continuation_edn TEXT, created_at/updated_at TEXT NOT NULL.
--   * State strings in DB: 'queued','running','waiting','succeeded','failed','cancelled','timed_out' (DB stores snake_case timed_out, code maps both hyphen forms).
--   * Migration: create works IF NOT EXISTS, then backfill from commands when commands exists (command queued->work queued etc, timed_out->timed-out). subagent_links rows are NOT auto-migrated — they remain as helper but future subagents use parent_work_id.
--   * Idempotency: CREATE TABLE IF NOT EXISTS, INSERT OR IGNORE for backfill, indexes IF NOT EXISTS.
--   * Single transaction via runner.

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS works (
  id                TEXT PRIMARY KEY,
  type              TEXT NOT NULL,
  state             TEXT NOT NULL CHECK (state IN ('queued','running','waiting','succeeded','failed','timed_out','cancelled')),
  session_id        TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT,
  parent_work_id    TEXT REFERENCES works(id) ON DELETE SET NULL,
  payload_ref       TEXT,
  deadline          TEXT,
  continuation_edn  TEXT,
  created_at        TEXT NOT NULL,
  updated_at        TEXT
);

CREATE INDEX IF NOT EXISTS works_session_idx ON works(session_id);
CREATE INDEX IF NOT EXISTS works_state_idx ON works(state);
CREATE INDEX IF NOT EXISTS works_parent_idx ON works(parent_work_id) WHERE parent_work_id IS NOT NULL;

-- Backfill from legacy commands table (commands exists from 012).
INSERT OR IGNORE INTO works (id, type, state, session_id, parent_work_id, payload_ref, deadline, continuation_edn, created_at, updated_at)
SELECT id, type, state, owner_session_id, parent_cmd_id, payload_ref, deadline, continuation_edn, created_at, created_at
FROM commands;
