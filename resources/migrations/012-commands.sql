-- 012-commands.sql — Fleet A1: durable async command outbox (DAG A1)
-- Introduces the `commands` table for the async command system.
--
-- Design decisions (component):
--   * id TEXT PRIMARY KEY — uuid string (stable id, mirrors sessions.id).
--   * type TEXT NOT NULL — namespaced keyword string, e.g. ":tool/invoke".
--   * state TEXT NOT NULL CHECK(state IN (...)) — six AsyncCommand states
--     [W-20..W-24]; DB stores snake_case `timed_out`, schema uses :timed-out.
--   * idempotency_key TEXT NOT NULL UNIQUE — GC de-duplication key (A2/A3);
--     UNIQUE enforces at-most-once per key at the storage layer.
--   * payload_ref TEXT NOT NULL — sha256: CAS reference (GC-21); content lives
--     in the filesystem CAS, row stores the reference only.
--   * owner_session_id TEXT NOT NULL REFERENCES sessions(id) — follows the
--     promotion_outbox FK style (TEXT REFERENCES sessions(id) ON DELETE RESTRICT).
--     SQLite FKs are per-connection (PRAGMA foreign_keys=ON); callers must route
--     through evoclj.store.sqlite/with-db.
--   * parent_cmd_id TEXT REFERENCES commands(id) — optional causal parent;
--     self-referential FK, nullable.
--   * continuation_edn TEXT — optional EDN continuation payload.
--   * deadline TEXT — optional ISO-8601 UTC instant for timed-out transitions.
--   * created_at TEXT NOT NULL — ISO-8601 UTC.
--   * Indexes on owner_session_id and idempotency_key mirror existing FK/index
--     patterns (sessions_generation_idx, promotion_outbox indexes).
--   * Migration is single-transaction via the runner's apply-files! (BEGIN IMMEDIATE).

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS commands (
  id                TEXT PRIMARY KEY,
  type              TEXT NOT NULL,
  state             TEXT NOT NULL CHECK (state IN ('queued','running','succeeded','failed','timed_out','cancelled')),
  idempotency_key   TEXT NOT NULL UNIQUE,
  payload_ref       TEXT NOT NULL,
  owner_session_id  TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT,
  parent_cmd_id     TEXT REFERENCES commands(id) ON DELETE SET NULL,
  continuation_edn  TEXT,
  deadline          TEXT,
  created_at        TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS commands_owner_session_idx ON commands(owner_session_id);
CREATE INDEX IF NOT EXISTS commands_idempotency_key_idx ON commands(idempotency_key);
CREATE INDEX IF NOT EXISTS commands_state_idx ON commands(state);
CREATE INDEX IF NOT EXISTS commands_parent_cmd_idx ON commands(parent_cmd_id) WHERE parent_cmd_id IS NOT NULL;
