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
CREATE TRIGGER IF NOT EXISTS candidates_no_mismatch_insert
BEFORE INSERT ON candidates
FOR EACH ROW
BEGIN
  SELECT RAISE(ABORT, 'candidates parent_genome_id must equal mutations parent_genome_id')
    WHERE (SELECT parent_genome_id FROM mutations WHERE id = NEW.mutation_id) IS NOT NULL
      AND NEW.parent_genome_id != (SELECT parent_genome_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates evidence_id must equal mutations evidence_id')
    WHERE (SELECT evidence_id FROM mutations WHERE id = NEW.mutation_id) IS NOT NULL
      AND NEW.evidence_id != (SELECT evidence_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates risk must equal mutations risk')
    WHERE (SELECT risk FROM mutations WHERE id = NEW.mutation_id) IS NOT NULL
      AND NEW.risk != (SELECT risk FROM mutations WHERE id = NEW.mutation_id);
END;

CREATE TRIGGER IF NOT EXISTS candidates_no_mismatch_update
BEFORE UPDATE OF parent_genome_id, evidence_id, risk, mutation_id ON candidates
FOR EACH ROW
BEGIN
  SELECT RAISE(ABORT, 'candidates parent_genome_id must equal mutations parent_genome_id')
    WHERE (SELECT parent_genome_id FROM mutations WHERE id = NEW.mutation_id) IS NOT NULL
      AND NEW.parent_genome_id != (SELECT parent_genome_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates evidence_id must equal mutations evidence_id')
    WHERE (SELECT evidence_id FROM mutations WHERE id = NEW.mutation_id) IS NOT NULL
      AND NEW.evidence_id != (SELECT evidence_id FROM mutations WHERE id = NEW.mutation_id);
  SELECT RAISE(ABORT, 'candidates risk must equal mutations risk')
    WHERE (SELECT risk FROM mutations WHERE id = NEW.mutation_id) IS NOT NULL
      AND NEW.risk != (SELECT risk FROM mutations WHERE id = NEW.mutation_id);
END;
