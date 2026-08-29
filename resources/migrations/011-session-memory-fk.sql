-- 011-session-memory-fk.sql — Fleet horizontal: FK existence for sessions and episodic_memory (DAG P5/F horizontal)
-- Closes gaps analogous to 009 but for horizontal surfaces: sessions (genome/resolution/phenotype) and episodic_memory.
--
-- Gaps closed:
--   * sessions.genome_id had no FK — now REFERENCES genomes(id) ON DELETE RESTRICT
--   * sessions.resolution_id had no FK — now REFERENCES artifacts(hash) ON DELETE RESTRICT
--   * sessions.phenotype_id had no FK — now REFERENCES artifacts(hash) ON DELETE RESTRICT
--   * episodic_memory.session_id had no FK — now REFERENCES sessions(id) ON DELETE CASCADE
--
-- Defense in depth: app layer via VerifiedDigest (evoclj.store.existence) refuses to create
-- a proof unless CAS contains the artifact; this migration enforces the same at rest.
-- SQLite cannot ADD FOREIGN KEY via ALTER TABLE; tables are rebuilt via 12-step procedure.

PRAGMA foreign_keys = OFF;

-- Backfill placeholder artifacts/genomes for existing sessions that have no FK target yet.
-- This keeps the migration from failing on pre-existing test data that used fake sha strings
-- without requiring a CAS body. New rows after migration must have real artifacts (FK).
INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
  SELECT DISTINCT genome_id, 'application/octet-stream', 0, datetime('now') FROM sessions WHERE genome_id NOT IN (SELECT hash FROM artifacts);
INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
  SELECT DISTINCT resolution_id, 'application/octet-stream', 0, datetime('now') FROM sessions WHERE resolution_id NOT IN (SELECT hash FROM artifacts);
INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
  SELECT DISTINCT phenotype_id, 'application/octet-stream', 0, datetime('now') FROM sessions WHERE phenotype_id NOT IN (SELECT hash FROM artifacts);
INSERT OR IGNORE INTO genomes (id, created_at)
  SELECT DISTINCT genome_id, datetime('now') FROM sessions WHERE genome_id NOT IN (SELECT id FROM genomes);
INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
  SELECT DISTINCT genome_id, 'application/octet-stream', 0, datetime('now') FROM generations WHERE genome_id NOT IN (SELECT hash FROM artifacts);
INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
  SELECT DISTINCT resolution_id, 'application/octet-stream', 0, datetime('now') FROM generations WHERE resolution_id NOT IN (SELECT hash FROM artifacts);
INSERT OR IGNORE INTO genomes (id, created_at)
  SELECT DISTINCT genome_id, datetime('now') FROM generations WHERE genome_id NOT IN (SELECT id FROM genomes);


CREATE TABLE IF NOT EXISTS artifacts (
  hash       TEXT PRIMARY KEY,
  media_type TEXT NOT NULL,
  size       INTEGER NOT NULL CHECK (size >= 0),
  created_at TEXT NOT NULL
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS genomes (
  id         TEXT PRIMARY KEY REFERENCES artifacts(hash) ON DELETE RESTRICT,
  created_at TEXT NOT NULL
) WITHOUT ROWID;

-- Rebuild sessions with FKs
CREATE TABLE sessions_new (
  id            TEXT PRIMARY KEY,
  generation_id TEXT NOT NULL REFERENCES generations (id) ON DELETE RESTRICT,
  genome_id     TEXT NOT NULL REFERENCES genomes(id) ON DELETE RESTRICT,
  resolution_id TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  phenotype_id  TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  state         TEXT NOT NULL DEFAULT 'created',
  created_at    TEXT NOT NULL,
  updated_at    TEXT,
  routing_deployment_version TEXT,
  routing_bucket INTEGER
);

INSERT INTO sessions_new (id, generation_id, genome_id, resolution_id, phenotype_id, state, created_at, updated_at, routing_deployment_version, routing_bucket)
  SELECT id, generation_id, genome_id, resolution_id, phenotype_id, state, created_at, updated_at, routing_deployment_version, routing_bucket FROM sessions;

DROP TABLE sessions;
ALTER TABLE sessions_new RENAME TO sessions;

CREATE INDEX sessions_generation_idx ON sessions (generation_id);
CREATE INDEX sessions_routing_idx ON sessions (routing_deployment_version, routing_bucket);

-- Rebuild episodic_memory with FK to sessions
CREATE TABLE episodic_memory_new (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id TEXT    NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
  memory_key TEXT    NOT NULL,
  content    TEXT    NOT NULL,
  created_at TEXT    NOT NULL,
  UNIQUE (session_id, memory_key)
);

INSERT INTO episodic_memory_new (id, session_id, memory_key, content, created_at)
  SELECT id, session_id, memory_key, content, created_at FROM episodic_memory;

DROP TABLE episodic_memory;
ALTER TABLE episodic_memory_new RENAME TO episodic_memory;

CREATE INDEX episodic_memory_session_idx ON episodic_memory (session_id);

PRAGMA foreign_keys = ON;