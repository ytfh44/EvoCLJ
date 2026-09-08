-- 022-evolution-dag.sql — immutable speculative-evolution parent edges
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS generation_parent_edges (
  child_generation_id TEXT NOT NULL REFERENCES generations(id) ON DELETE RESTRICT,
  parent_generation_id TEXT NOT NULL REFERENCES generations(id) ON DELETE RESTRICT,
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  role TEXT NOT NULL DEFAULT 'parent',
  merge_plan_digest TEXT,
  mutation_hash TEXT,
  created_at TEXT NOT NULL,
  PRIMARY KEY (child_generation_id, ordinal),
  UNIQUE (child_generation_id, parent_generation_id),
  CHECK (child_generation_id <> parent_generation_id)
);
CREATE INDEX IF NOT EXISTS generation_parent_edges_parent_idx
  ON generation_parent_edges(parent_generation_id, ordinal, child_generation_id);
CREATE TRIGGER IF NOT EXISTS generation_parent_edges_no_cycle
BEFORE INSERT ON generation_parent_edges BEGIN
  SELECT RAISE(ABORT, 'generation parent edge creates a cycle')
  WHERE EXISTS (
    WITH RECURSIVE ancestors(id) AS (
      SELECT NEW.parent_generation_id
      UNION
      SELECT e.parent_generation_id
      FROM generation_parent_edges e JOIN ancestors a ON e.child_generation_id = a.id)
    SELECT 1 FROM ancestors WHERE id = NEW.child_generation_id);
END;
CREATE TRIGGER IF NOT EXISTS generation_parent_edges_no_update
BEFORE UPDATE ON generation_parent_edges BEGIN
  SELECT RAISE(ABORT, 'generation parent edges are immutable');
END;
CREATE TRIGGER IF NOT EXISTS generation_parent_edges_no_delete
BEFORE DELETE ON generation_parent_edges BEGIN
  SELECT RAISE(ABORT, 'generation parent edges are immutable');
END;

CREATE TABLE IF NOT EXISTS candidate_parent_edges (
  candidate_id TEXT NOT NULL REFERENCES candidates(id) ON DELETE RESTRICT,
  parent_generation_id TEXT REFERENCES generations(id) ON DELETE RESTRICT,
  parent_candidate_id TEXT REFERENCES candidates(id) ON DELETE RESTRICT,
  parent_genome_id TEXT NOT NULL,
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  role TEXT NOT NULL DEFAULT 'parent',
  merge_plan_digest TEXT,
  mutation_hash TEXT,
  provenance TEXT NOT NULL DEFAULT '{}',
  created_at TEXT NOT NULL,
  PRIMARY KEY (candidate_id, ordinal),
  UNIQUE (candidate_id, parent_generation_id, parent_candidate_id, parent_genome_id),
  FOREIGN KEY (parent_generation_id, parent_genome_id)
    REFERENCES generations(id, genome_id) ON DELETE RESTRICT,
  CHECK (candidate_id <> parent_candidate_id),
  CHECK (parent_generation_id IS NOT NULL OR parent_candidate_id IS NOT NULL)
);
CREATE INDEX IF NOT EXISTS candidate_parent_edges_generation_idx
  ON candidate_parent_edges(parent_generation_id, ordinal, candidate_id);
CREATE INDEX IF NOT EXISTS candidate_parent_edges_candidate_idx
  ON candidate_parent_edges(parent_candidate_id, ordinal, candidate_id);
CREATE TRIGGER IF NOT EXISTS candidate_parent_edges_no_cycle
BEFORE INSERT ON candidate_parent_edges BEGIN
  SELECT RAISE(ABORT, 'candidate parent edge creates a cycle')
  WHERE NEW.parent_candidate_id IS NOT NULL
    AND EXISTS (
      WITH RECURSIVE ancestors(id) AS (
        SELECT NEW.parent_candidate_id
        UNION
        SELECT e.parent_candidate_id
        FROM candidate_parent_edges e JOIN ancestors a ON e.candidate_id = a.id
        WHERE e.parent_candidate_id IS NOT NULL)
      SELECT 1 FROM ancestors WHERE id = NEW.candidate_id);
END;
CREATE TRIGGER IF NOT EXISTS candidate_parent_edges_no_update
BEFORE UPDATE ON candidate_parent_edges BEGIN
  SELECT RAISE(ABORT, 'candidate parent edges are immutable');
END;
CREATE TRIGGER IF NOT EXISTS candidate_parent_edges_no_delete
BEFORE DELETE ON candidate_parent_edges BEGIN
  SELECT RAISE(ABORT, 'candidate parent edges are immutable');
END;

-- Backfill the compatibility parent_id into ordinal-zero edge rows. Existing
-- generations retain parent_id as the legacy primary-parent API.
INSERT OR IGNORE INTO generation_parent_edges
  (child_generation_id, parent_generation_id, ordinal, role, created_at)
SELECT id, parent_id, 0, 'parent', created_at
FROM generations
WHERE parent_id IS NOT NULL;

INSERT OR IGNORE INTO candidate_parent_edges
  (candidate_id, parent_generation_id, parent_genome_id, ordinal, role, created_at)
SELECT id, parent_generation_id, parent_genome_id, 0, 'parent', created_at
FROM candidates;
