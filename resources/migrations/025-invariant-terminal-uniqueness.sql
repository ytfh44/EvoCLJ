-- 025-invariant-terminal-uniqueness.sql — serialize one terminal decision/disable per proposal
PRAGMA foreign_keys = ON;

CREATE UNIQUE INDEX IF NOT EXISTS invariant_decisions_one_terminal
  ON invariant_decisions(proposal_id);
CREATE UNIQUE INDEX IF NOT EXISTS invariant_disables_one_per_proposal
  ON invariant_disables(proposal_id);
