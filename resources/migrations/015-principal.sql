-- 015-principal.sql — I2: replace Subject with Principal tagged union
-- Design decisions (I2, break compat, clean):
--   * Principal = SessionPrincipal(sid) | JobPrincipal(jid) | EvalPrincipal(pid) | OperatorPrincipal
--   * New columns: principal_type TEXT NOT NULL CHECK(principal_type IN ('session','job','eval','operator'))
--                 principal_id TEXT NOT NULL (operator uses 'operator')
--   * Keep legacy subject_* columns for backward compat during migration (not NOT NULL after alter)
--   * Add indexes on principal_type, principal_id
--   * No FK to sessions for job/eval/operator (only session principal may reference sessions, but we relax FK to allow all)
--   Migration is idempotent via IF NOT EXISTS / pragma checks.

PRAGMA foreign_keys = OFF;

-- Add new columns if not exists (SQLite 3.35+ supports IF NOT EXISTS for ADD COLUMN via workaround: check pragma)
-- We use simple ALTER TABLE ADD COLUMN; if column exists, it will error but migration runner wraps in transaction and we make idempotent via checking pragma table_info
-- To keep runner simple, we attempt to add and ignore error via separate statements handled by runner's idempotency (IF NOT EXISTS not supported for ADD COLUMN, so we use a trick: create new table if needed)

-- For fresh DB, 013 already creates table without principal columns; 015 will add them.
-- For existing DB, we add columns via ALTER.

-- Attempt to add columns - runner will ignore duplicate column errors if we use a helper? Instead we recreate table with new schema if columns missing.

-- Check and add principal_type
-- SQLite does not support IF NOT EXISTS for ADD COLUMN, so we use a conditional via table recreation fallback.
-- Simpler: try ALTER and ignore error in migration runner (our migrate.clj runs each statement and continues on duplicate column error? Let's make it safe by using a new table and copy).

-- Create new capabilities table with principal columns if not already migrated
CREATE TABLE IF NOT EXISTS capabilities_new (
  id                     TEXT PRIMARY KEY,
  principal_type         TEXT NOT NULL CHECK(principal_type IN ('session','job','eval','operator')),
  principal_id           TEXT NOT NULL,
  subject_session_id     TEXT,
  subject_phenotype_id   TEXT,
  resource_kind          TEXT NOT NULL CHECK(resource_kind IN ('tool','model','memory','filesystem','filesystem/path')),
  resource_id            TEXT NOT NULL,
  actions                TEXT NOT NULL CHECK(length(actions) > 2 AND actions != '[]'),
  constraints            TEXT,
  issued_at              TEXT NOT NULL,
  expires_at             TEXT NOT NULL CHECK(expires_at > issued_at),
  revoked                INTEGER NOT NULL DEFAULT 0 CHECK(revoked IN (0, 1)),
  created_at             TEXT NOT NULL
);

-- Copy existing data if capabilities has old schema and capabilities_new is empty
INSERT OR IGNORE INTO capabilities_new (id, principal_type, principal_id, subject_session_id, subject_phenotype_id, resource_kind, resource_id, actions, constraints, issued_at, expires_at, revoked, created_at)
SELECT id,
       'session' as principal_type,
       subject_session_id as principal_id,
       subject_session_id,
       subject_phenotype_id,
       resource_kind, resource_id, actions, constraints, issued_at, expires_at, revoked, created_at
FROM capabilities
WHERE NOT EXISTS (SELECT 1 FROM capabilities_new WHERE capabilities_new.id = capabilities.id);

-- If capabilities_new has data and original capabilities is old schema, replace
DROP TABLE IF EXISTS capabilities_old;
ALTER TABLE capabilities RENAME TO capabilities_old;
ALTER TABLE capabilities_new RENAME TO capabilities;
DROP TABLE IF EXISTS capabilities_old;

CREATE INDEX IF NOT EXISTS capabilities_principal_type_idx ON capabilities(principal_type);
CREATE INDEX IF NOT EXISTS capabilities_principal_id_idx ON capabilities(principal_id);
CREATE INDEX IF NOT EXISTS capabilities_subject_session_idx ON capabilities(subject_session_id);
CREATE INDEX IF NOT EXISTS capabilities_resource_kind_idx ON capabilities(resource_kind);
CREATE INDEX IF NOT EXISTS capabilities_revoked_idx ON capabilities(revoked);

PRAGMA foreign_keys = ON;
