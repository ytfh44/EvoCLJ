-- 019-p1-authority.sql — P1 single-source Authority
-- DB is source of truth, memory LeaseRegistry is versioned cache.
-- Derive/revoke must DURABLE commit (INSERT/UPDATE WHERE revoked=0) before swap! cache.
-- Design (P1, break compat, clean):
--   * capabilities.lease_edn TEXT — faithful EDN of the sealed lease (pr-str), authoritative resource identity fallback.
--   * capabilities.revoked_at TEXT — ISO-8601 UTC timestamp of revocation (nullable; SET on revoke, mirrors revoked flag).
--   * No synthetic fallback: hydrate loads from DB, empty means deny.
--   * Revocation is conditional: UPDATE ... WHERE revoked=0 so draining commits are serialized.
-- Migration is table-recreate (SQLite has no DROP CONSTRAINT) mirroring 015/016, plus idempotent column add for already-migrated DBs.
-- Single-transaction via the runner's apply-files! (BEGIN IMMEDIATE).

PRAGMA foreign_keys = OFF;

CREATE TABLE IF NOT EXISTS capabilities_new (
  id                     TEXT PRIMARY KEY,
  principal_type         TEXT NOT NULL CHECK(principal_type IN ('session','job','eval','operator')),
  principal_id           TEXT NOT NULL,
  subject_session_id     TEXT,
  subject_phenotype_id   TEXT,
  resource_kind          TEXT NOT NULL,
  resource_edn           TEXT NOT NULL,
  resource_id            TEXT,
  actions                TEXT NOT NULL CHECK(length(actions) > 2 AND actions != '[]'),
  constraints            TEXT,
  issued_at              TEXT NOT NULL,
  expires_at             TEXT NOT NULL CHECK(expires_at > issued_at),
  revoked                INTEGER NOT NULL DEFAULT 0 CHECK(revoked IN (0, 1)),
  revoked_at             TEXT,
  lease_edn              TEXT,
  created_at             TEXT NOT NULL
);

-- Copy existing data into new table; lease_edn faithful copy is backfilled as pr-str of the row's logical lease when absent.
INSERT OR IGNORE INTO capabilities_new
  (id, principal_type, principal_id, subject_session_id, subject_phenotype_id,
   resource_kind, resource_edn, resource_id, actions, constraints,
   issued_at, expires_at, revoked, revoked_at, lease_edn, created_at)
SELECT id, principal_type, principal_id, subject_session_id, subject_phenotype_id,
       resource_kind, resource_edn, resource_id, actions, constraints,
       issued_at, expires_at, revoked, NULL, NULL, created_at
FROM capabilities
WHERE NOT EXISTS (SELECT 1 FROM capabilities_new WHERE capabilities_new.id = capabilities.id);

DROP TABLE IF EXISTS capabilities_old;
ALTER TABLE capabilities RENAME TO capabilities_old;
ALTER TABLE capabilities_new RENAME TO capabilities;
DROP TABLE IF EXISTS capabilities_old;

CREATE INDEX IF NOT EXISTS capabilities_principal_type_idx ON capabilities(principal_type);
CREATE INDEX IF NOT EXISTS capabilities_principal_id_idx ON capabilities(principal_id);
CREATE INDEX IF NOT EXISTS capabilities_subject_session_idx ON capabilities(subject_session_id);
CREATE INDEX IF NOT EXISTS capabilities_resource_kind_idx ON capabilities(resource_kind);
CREATE INDEX IF NOT EXISTS capabilities_revoked_idx ON capabilities(revoked);
CREATE INDEX IF NOT EXISTS capabilities_revoked_at_idx ON capabilities(revoked_at);

PRAGMA foreign_keys = ON;
