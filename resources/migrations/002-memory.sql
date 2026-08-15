-- 002-memory.sql — episodic memory table (feature R1).
--
-- Episodic memory is a kernel-owned effect: the :memory/kv provider
-- (evoclj.provider.memory) reads and writes rows here through the
-- Intent/Capability Broker (Global Constraint 8). Content is stored as
-- pr-str EDN (Global Constraint 22 — plain data only). Each row is
-- scoped to a single session (per-session memory isolation, feature
-- R2), keyed by (session_id, memory_key); a write UPSERTS on that
-- pair (INSERT OR REPLACE), so a repeat write to the same key within a
-- session overwrites the earlier content.

CREATE TABLE episodic_memory (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id TEXT    NOT NULL,
  memory_key TEXT    NOT NULL,
  content    TEXT    NOT NULL,   -- pr-str EDN (Global Constraint 22)
  created_at TEXT    NOT NULL,   -- ISO-8601 (UTC)
  UNIQUE (session_id, memory_key)
);

CREATE INDEX episodic_memory_session_idx ON episodic_memory (session_id);
