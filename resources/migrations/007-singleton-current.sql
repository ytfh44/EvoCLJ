-- 007-singleton-current.sql — CURRENT as singleton reference (Fleet S1, DAG S1)
-- Reifies "exactly one CURRENT" from protocol to schema.
-- Before: generations.current INTEGER 0/1 + partial unique index (at most one).
-- After: kernel_state singleton row (id=1, FK to generations) is the definition.
-- Migration is additive and backward-compatible: generations.current is kept
-- as a derived column via triggers until fleet S1b removes it.

PRAGMA foreign_keys = ON;

-- Singleton table: exactly one row, id always 1, never deletable by definition
CREATE TABLE kernel_state (
  id                  INTEGER PRIMARY KEY CHECK (id = 1),
  current_generation  TEXT NOT NULL REFERENCES generations(id),
  updated_at          TEXT NOT NULL
) WITHOUT ROWID;

-- Seed from existing CURRENT if any
INSERT INTO kernel_state (id, current_generation, updated_at)
  SELECT 1, id, created_at FROM generations WHERE current = 1
  LIMIT 1;

-- If no CURRENT existed (empty DB), leave kernel_state empty — seed will populate it
-- via application code; the CHECK(id=1) plus FK ensures at most one row.

-- Prevent deletion — definition-level "exactly one" after seed
CREATE TRIGGER kernel_state_no_delete BEFORE DELETE ON kernel_state
BEGIN
  SELECT RAISE(ABORT, 'kernel_state is a singleton — deletion forbidden');
END;

-- Keep generations.current derived for backward compat (transitional)
-- Any move of CURRENT must go through kernel_state; the trigger syncs the predicate column
CREATE TRIGGER kernel_state_sync_current_after_insert AFTER INSERT ON kernel_state
BEGIN
  UPDATE generations SET current = IIF(id = NEW.current_generation, 1, 0);
END;

CREATE TRIGGER kernel_state_sync_current_after_update AFTER UPDATE OF current_generation ON kernel_state
BEGIN
  UPDATE generations SET current = IIF(id = NEW.current_generation, 1, 0);
END;
