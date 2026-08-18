-- 005-deploy.sql — deployment decision log (S1-3).
--
-- Records explicit deploy/rollback decisions for a generation without
-- mutating the generations CURRENT pointer (Global Constraint 15 stays
-- in promotion.promote). The deploy CLI writes here; rollback can also
-- record a decision if needed later.

CREATE TABLE deployment_decisions (
  id            TEXT PRIMARY KEY,       -- decision id (uuid string)
  generation_id TEXT NOT NULL,          -- the deployed generation
  decision      TEXT NOT NULL CHECK (decision IN ('deployed', 'rolled-back')),
  reason        TEXT,                   -- optional operator / canary reason
  created_at    TEXT NOT NULL           -- ISO-8601 (UTC)
);

CREATE INDEX deployment_decisions_generation_idx
  ON deployment_decisions (generation_id);
