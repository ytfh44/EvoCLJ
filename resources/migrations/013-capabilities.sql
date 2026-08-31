-- 013-capabilities.sql — P7: store-backed leases table (DAG P7)
-- Introduces the `capabilities` table for durable CapabilityLease persistence.
--
-- Design decisions (component):
--   * id TEXT PRIMARY KEY — capId (UUID string, mirrors capability_leases.id).
--   * subject_session_id TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT — dual-anchor subject, FK to sessions.
--     SQLite FKs are per-connection (PRAGMA foreign_keys=ON); callers must route
--     through evoclj.store.sqlite/with-db (same discipline as commands.owner_session_id).
--   * subject_phenotype_id TEXT NOT NULL — second half of dual-anchor subject (TEXT, no FK to artifacts; phenotype is a content hash/CAS ref).
--   * resource_kind TEXT NOT NULL CHECK(resource_kind IN ('tool','model','memory','filesystem','filesystem/path')) — closed kind vocabulary [W-02].
--   * resource_id TEXT NOT NULL — provider-defined resource identifier.
--   * actions TEXT NOT NULL (JSON array string) — non-empty check; canonical JSON array.
--   * constraints TEXT (JSON) — optional JSON object string, nullable.
--   * issued_at TEXT NOT NULL, expires_at TEXT NOT NULL CHECK(expires_at > issued_at) — positive window [W-04].
--   * revoked INTEGER NOT NULL DEFAULT 0 CHECK(revoked IN (0,1)) — boolean store (SQLite has no BOOLEAN).
--   * created_at TEXT NOT NULL — insertion timestamp ISO-8601 UTC.
--   * CHECK(length(actions) > 2) enforces non-empty JSON array (at least '["x"]'); complements resource_kind closed set.
--   * Indexes on subject_session_id, resource_kind, revoked mirror the assignment's required indexes.
--   * Migration is single-transaction via the runner's apply-files! (BEGIN IMMEDIATE).

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS capabilities (
  id                     TEXT PRIMARY KEY,
  subject_session_id     TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT,
  subject_phenotype_id   TEXT NOT NULL,
  resource_kind          TEXT NOT NULL CHECK(resource_kind IN ('tool','model','memory','filesystem','filesystem/path')),
  resource_id            TEXT NOT NULL,
  actions                TEXT NOT NULL CHECK(length(actions) > 2 AND actions != '[]'),
  constraints            TEXT,
  issued_at              TEXT NOT NULL,
  expires_at             TEXT NOT NULL CHECK(expires_at > issued_at),
  revoked                INTEGER NOT NULL DEFAULT 0 CHECK(revoked IN (0, 1)),
  created_at             TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS capabilities_subject_session_idx ON capabilities(subject_session_id);
CREATE INDEX IF NOT EXISTS capabilities_resource_kind_idx ON capabilities(resource_kind);
CREATE INDEX IF NOT EXISTS capabilities_revoked_idx ON capabilities(revoked);
