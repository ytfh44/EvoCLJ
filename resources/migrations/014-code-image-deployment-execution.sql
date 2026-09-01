-- 014-code-image-deployment-execution.sql — I1: split Phenotype into CodeImage/Deployment/Execution
-- Design decisions (I1, one-time break compat):
--   * CodeImageId  = H(kernel ABI, Genome, Resolution) — pure code identity, sha256
--   * DeploymentId = H(CodeImage, bindings, authority) — bound deployment, sha256
--   * ExecutionId  = UUID per activation — distinct execution identity
--   * PhenotypeId legacy alias is removed at API level; store retains phenotype_id for
--     backwards compat during transition but adds canonical code_image_id.
--   * Creates tables code_images, deployments, executions.
--   * Adds code_image_id, deployment_id, execution_id to sessions/events and backfills.

PRAGMA foreign_keys = OFF;

-- code_images — normalized CodeImage identity (pure code)
CREATE TABLE IF NOT EXISTS code_images (
  id            TEXT PRIMARY KEY,
  abi           TEXT NOT NULL,
  genome_id     TEXT NOT NULL,
  resolution_id TEXT NOT NULL,
  created_at    TEXT NOT NULL
) WITHOUT ROWID;

-- deployments — bound deployment (CodeImage + bindings + authority)
CREATE TABLE IF NOT EXISTS deployments (
  id            TEXT PRIMARY KEY,
  code_image_id TEXT NOT NULL REFERENCES code_images (id),
  bindings      TEXT,
  authority     TEXT,
  created_at    TEXT NOT NULL
) WITHOUT ROWID;

CREATE INDEX IF NOT EXISTS deployments_code_image_idx ON deployments (code_image_id);

-- executions — per-activation execution (UUID, distinct per instantiate)
CREATE TABLE IF NOT EXISTS executions (
  id            TEXT PRIMARY KEY,
  deployment_id TEXT NOT NULL REFERENCES deployments (id),
  code_image_id TEXT NOT NULL REFERENCES code_images (id),
  created_at    TEXT NOT NULL
) WITHOUT ROWID;

CREATE INDEX IF NOT EXISTS executions_deployment_idx ON executions (deployment_id);
CREATE INDEX IF NOT EXISTS executions_code_image_idx ON executions (code_image_id);

-- sessions — add I1 columns, backfill from phenotype_id, keep phenotype_id for compat
ALTER TABLE sessions ADD COLUMN code_image_id TEXT;
ALTER TABLE sessions ADD COLUMN deployment_id TEXT;
ALTER TABLE sessions ADD COLUMN execution_id TEXT;

UPDATE sessions SET code_image_id = phenotype_id WHERE code_image_id IS NULL;

-- events — add I1 columns, backfill
ALTER TABLE events ADD COLUMN code_image_id TEXT;
ALTER TABLE events ADD COLUMN deployment_id TEXT;
ALTER TABLE events ADD COLUMN execution_id TEXT;

UPDATE events SET code_image_id = phenotype_id WHERE code_image_id IS NULL;

CREATE INDEX IF NOT EXISTS sessions_code_image_idx ON sessions (code_image_id);
CREATE INDEX IF NOT EXISTS sessions_deployment_idx ON sessions (deployment_id);
CREATE INDEX IF NOT EXISTS sessions_execution_idx ON sessions (execution_id);

CREATE INDEX IF NOT EXISTS events_code_image_idx ON events (code_image_id);
CREATE INDEX IF NOT EXISTS events_deployment_idx ON events (deployment_id);
CREATE INDEX IF NOT EXISTS events_execution_idx ON events (execution_id);

PRAGMA foreign_keys = ON;
