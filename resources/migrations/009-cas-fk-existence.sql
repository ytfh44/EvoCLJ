-- 009-cas-fk-existence.sql — Fleet P5/F: CAS FK / existence proof gap (DAG P5/F)
-- Fixes FK omissions in 001-init.sql and enforces existence proofs at the DB layer.
--
-- Gaps closed:
--   * generations.genome_id had no FK — now REFERENCES genomes(id).
--     genomes is the content-addressed Genome registry (id = sha256:<64 hex>),
--     itself FK to artifacts(hash) so a genome cannot be registered without
--     its CAS artifact existing (the existence proof at rest).
--   * candidates.genome_id / evidence_id had no FK — now genome_id
--     REFERENCES genomes(id) and evidence_id REFERENCES artifacts(hash).
--   * candidates.payload_ref had no column and no FK — added as
--     TEXT REFERENCES artifacts(hash) (nullable for backward compat;
--     future code should populate it via VerifiedDigest).
--   * artifacts.hash is the CAS definition (Database Invariant 1).
--
-- Defense in depth: the app layer (evoclj.store.existence/VerifiedDigest)
-- refuses to create a proof unless CAS contains the artifact; this
-- migration enforces the same invariant at rest so a raw SQL INSERT with
-- a bogus hash still fails with a foreign-key violation even if the
-- app boundary is bypassed.
--
-- SQLite cannot ADD FOREIGN KEY via ALTER TABLE; tables are rebuilt via
-- the 12-step procedure (create new, copy, drop old, rename, reindex).
-- The rebuild is done inside the migration transaction so a failure
-- rolls back cleanly. No placeholder fabrication: a genome_id that has
-- no artifacts row is a FK violation, not a size-0 artifact (CAS size
-- must be the real content size, not 0). Tests and application code
-- must insert real CAS artifacts before the FK-dependent rows (via
-- evoclj.store.cas/put-bytes! and explicit artifacts/genomes rows).

PRAGMA foreign_keys = OFF;

-- ---------------------------------------------------------------------------
-- 0. Ensure base tables exist (idempotent for fresh DB vs upgrade)
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS artifacts (
  hash       TEXT PRIMARY KEY,
  media_type TEXT NOT NULL,
  size       INTEGER NOT NULL CHECK (size >= 0),
  created_at TEXT NOT NULL
) WITHOUT ROWID;

-- genomes — content-addressed Genome registry, the FK target for generations/candidates genome_id.
-- id is the sha256:<64 hex> GenomeId (same shape as artifact hash). It FKs to artifacts(hash)
-- so a genome row cannot exist without its CAS artifact (existence proof at rest).
CREATE TABLE IF NOT EXISTS genomes (
  id         TEXT PRIMARY KEY REFERENCES artifacts(hash) ON DELETE RESTRICT,
  created_at TEXT NOT NULL
) WITHOUT ROWID;

-- Drop dependent triggers that reference generations (from 007) before rebuild
DROP TRIGGER IF EXISTS kernel_state_sync_current_after_insert;
DROP TRIGGER IF EXISTS kernel_state_sync_current_after_update;
DROP TRIGGER IF EXISTS kernel_state_no_delete;
-- Also drop candidate mismatch triggers that will be lost anyway (will be recreated)
DROP VIEW IF EXISTS candidates_normalized;
DROP TRIGGER IF EXISTS candidates_no_mismatch_insert;
DROP TRIGGER IF EXISTS candidates_no_mismatch_update;
DROP TRIGGER IF EXISTS mutations_immutable_fields;
-- Drop legacy placeholder triggers if they exist (pre-fix 009 had them)
DROP TRIGGER IF EXISTS generations_ensure_fk_before_insert;
DROP TRIGGER IF EXISTS generations_ensure_fk_before_update;
DROP TRIGGER IF EXISTS candidates_ensure_fk_before_insert;
DROP TRIGGER IF EXISTS candidates_ensure_fk_before_update;
DROP TRIGGER IF EXISTS candidates_payload_fk_insert;
DROP TRIGGER IF EXISTS candidates_payload_fk_update;

-- ---------------------------------------------------------------------------
-- 1. Rebuild generations with FK to genomes(id)
-- ---------------------------------------------------------------------------

CREATE TABLE generations_new (
  id            TEXT PRIMARY KEY,
  genome_id     TEXT NOT NULL REFERENCES genomes(id) ON DELETE RESTRICT,
  resolution_id TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  parent_id     TEXT REFERENCES generations_new(id),
  state         TEXT NOT NULL DEFAULT 'active'
                CHECK (state IN ('active', 'retired', 'rolled-back')),
  current       INTEGER NOT NULL DEFAULT 0 CHECK (current IN (0, 1)),
  created_at    TEXT NOT NULL
);

INSERT INTO generations_new (id, genome_id, resolution_id, parent_id, state, current, created_at)
  SELECT id, genome_id, resolution_id, parent_id, state, current, created_at FROM generations;

DROP TABLE generations;
ALTER TABLE generations_new RENAME TO generations;

CREATE UNIQUE INDEX generations_current_unique ON generations (current) WHERE current = 1;
CREATE UNIQUE INDEX generations_id_genome_unique ON generations (id, genome_id);
CREATE INDEX generations_parent_idx ON generations (parent_id);

-- Recreate kernel_state triggers (from 007) now that generations exists again
CREATE TRIGGER IF NOT EXISTS kernel_state_no_delete BEFORE DELETE ON kernel_state
BEGIN
  SELECT RAISE(ABORT, 'kernel_state is a singleton — deletion forbidden');
END;

CREATE TRIGGER IF NOT EXISTS kernel_state_sync_current_after_insert AFTER INSERT ON kernel_state
BEGIN
  UPDATE generations SET current = IIF(id = NEW.current_generation, 1, 0);
END;

CREATE TRIGGER IF NOT EXISTS kernel_state_sync_current_after_update AFTER UPDATE OF current_generation ON kernel_state
BEGIN
  UPDATE generations SET current = IIF(id = NEW.current_generation, 1, 0);
END;

-- ---------------------------------------------------------------------------
-- 2. Rebuild candidates with FKs: genome_id -> genomes, evidence_id -> artifacts,
--    payload_ref -> artifacts, plus composite FK to generations (Invariant 8)
-- ---------------------------------------------------------------------------

CREATE TABLE candidates_new (
  id                   TEXT PRIMARY KEY,
  parent_generation_id TEXT NOT NULL,
  parent_genome_id     TEXT NOT NULL,
  genome_id            TEXT NOT NULL REFERENCES genomes(id) ON DELETE RESTRICT,
  mutation_id          TEXT NOT NULL REFERENCES mutations(id) ON DELETE RESTRICT,
  evidence_id          TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  payload_ref          TEXT REFERENCES artifacts(hash) ON DELETE RESTRICT,
  risk                 TEXT NOT NULL,
  state                TEXT NOT NULL DEFAULT 'materialized'
                       CHECK (state IN ('materialized', 'evaluating',
                                        'eligible', 'promoted',
                                        'rejected', 'stale')),
  created_at           TEXT NOT NULL,
  FOREIGN KEY (parent_generation_id, parent_genome_id)
      REFERENCES generations (id, genome_id)
);

INSERT INTO candidates_new (id, parent_generation_id, parent_genome_id, genome_id, mutation_id, evidence_id, payload_ref, risk, state, created_at)
  SELECT id, parent_generation_id, parent_genome_id, genome_id, mutation_id, evidence_id, NULL, risk, state, created_at FROM candidates;

DROP TABLE candidates;
ALTER TABLE candidates_new RENAME TO candidates;

CREATE INDEX candidates_parent_generation_idx ON candidates (parent_generation_id);
CREATE INDEX candidates_mutation_idx ON candidates (mutation_id);
CREATE INDEX candidates_payload_ref_idx ON candidates (payload_ref) WHERE payload_ref IS NOT NULL;

-- Recreate S3 triggers from 008 (lost on DROP TABLE candidates/mutations)
CREATE TRIGGER IF NOT EXISTS candidates_no_mismatch_insert
BEFORE INSERT ON candidates
FOR EACH ROW
BEGIN
  SELECT RAISE(ABORT, 'candidates parent_genome_id must equal mutations parent_genome_id')
    WHERE NEW.parent_genome_id IS NOT (SELECT parent_genome_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates evidence_id must equal mutations evidence_id')
    WHERE NEW.evidence_id IS NOT (SELECT evidence_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates risk must equal mutations risk')
    WHERE NEW.risk IS NOT (SELECT risk FROM mutations WHERE id = NEW.mutation_id);
END;

CREATE TRIGGER IF NOT EXISTS candidates_no_mismatch_update
BEFORE UPDATE OF parent_genome_id, evidence_id, risk, mutation_id ON candidates
FOR EACH ROW
BEGIN
  SELECT RAISE(ABORT, 'candidates parent_genome_id must equal mutations parent_genome_id')
    WHERE NEW.parent_genome_id IS NOT (SELECT parent_genome_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates evidence_id must equal mutations evidence_id')
    WHERE NEW.evidence_id IS NOT (SELECT evidence_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates risk must equal mutations risk')
    WHERE NEW.risk IS NOT (SELECT risk FROM mutations WHERE id = NEW.mutation_id);
END;

CREATE VIEW IF NOT EXISTS candidates_normalized AS
SELECT
  c.id                    AS id,
  c.parent_generation_id  AS parent_generation_id,
  m.parent_genome_id      AS parent_genome_id,
  c.genome_id             AS genome_id,
  c.mutation_id           AS mutation_id,
  m.evidence_id           AS evidence_id,
  m.risk                  AS risk,
  c.state                 AS state,
  c.created_at            AS created_at
FROM candidates c
JOIN mutations m ON c.mutation_id = m.id;

CREATE TRIGGER IF NOT EXISTS mutations_immutable_fields
BEFORE UPDATE OF parent_genome_id, evidence_id, risk ON mutations
FOR EACH ROW
BEGIN
  SELECT RAISE(ABORT, 'mutations parent_genome_id is immutable')
    WHERE NEW.parent_genome_id IS NOT OLD.parent_genome_id;
  SELECT RAISE(ABORT, 'mutations evidence_id is immutable')
    WHERE NEW.evidence_id IS NOT OLD.evidence_id;
  SELECT RAISE(ABORT, 'mutations risk is immutable')
    WHERE NEW.risk IS NOT OLD.risk;
END;

-- Declarative FKs only — no auto-create placeholder triggers.
-- Every genome_id/evidence_id/payload_ref must have a real artifacts/genomes
-- row inserted explicitly (via CAS) before the FK-dependent row, otherwise
-- the INSERT fails with a foreign-key violation.

-- Re-enable FK enforcement for subsequent statements / connections
PRAGMA foreign_keys = ON;
