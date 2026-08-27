-- 008-normalize-candidate.sql — S3 Candidate normalization (Fleet S3, DAG S3)
-- Candidate duplicates Mutation fields (parent_genome_id, evidence_id, risk).
-- Normalize: mutation is definition; candidate stores only FK references.
-- Keep DB columns for backward compat but make mismatch unrepresentable via
-- triggers and provide a normalized view that derives via JOIN.

PRAGMA foreign_keys = ON;

-- Normalized view: candidate derived fields come from mutations (definition > validation)
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

-- Enforce mismatch unrepresentable: candidate columns must equal mutation columns.
-- Keeps physical columns for backward compat but aborts on divergence.
-- Use SELECT RAISE(...) WHERE ... to avoid CASE END confusing the migration splitter.
-- NULL-safe comparison via IS NOT (covers NULL vs non-NULL mismatches).
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

-- S3 fix: enforce mutation immutability — reject any UPDATE to
-- parent_genome_id, evidence_id, risk, or the entire row. Uses
-- SELECT RAISE(ABORT ...) with NULL-safe IS NOT comparisons.
CREATE TRIGGER IF NOT EXISTS mutations_no_update
BEFORE UPDATE ON mutations
FOR EACH ROW
BEGIN
  SELECT RAISE(ABORT, 'mutations are immutable');
END;

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
