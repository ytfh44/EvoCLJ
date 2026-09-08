-- 023-self-generated-invariants.sql — evidence-first generated invariants
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS invariant_proposals (
  id TEXT PRIMARY KEY,
  proposer TEXT NOT NULL,
  reviewer TEXT,
  scope TEXT NOT NULL CHECK (scope IN ('session','generation','candidate','effect','tool','model','runtime')),
  risk TEXT NOT NULL CHECK (risk IN ('low','medium','high','critical')),
  version INTEGER NOT NULL CHECK (version > 0),
  registry_revision TEXT NOT NULL,
  predicate_digest TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  predicate_json TEXT NOT NULL,
  proposal_digest TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  evidence_refs TEXT NOT NULL,
  replay_refs TEXT NOT NULL,
  adversarial_refs TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('proposed','rejected','approved','active','disabled','quarantined')),
  created_at TEXT NOT NULL
);
CREATE UNIQUE INDEX IF NOT EXISTS invariant_proposals_version_digest ON invariant_proposals(version, predicate_digest);
CREATE INDEX IF NOT EXISTS invariant_proposals_status_idx ON invariant_proposals(status, created_at);

CREATE TABLE IF NOT EXISTS invariant_runs (
  id TEXT PRIMARY KEY,
  proposal_id TEXT NOT NULL REFERENCES invariant_proposals(id) ON DELETE RESTRICT,
  kind TEXT NOT NULL CHECK (kind IN ('counterexample','replay','adversarial')),
  result_ref TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  run_digest TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  gate TEXT,
  model_policy TEXT,
  deterministic INTEGER NOT NULL CHECK (deterministic IN (0,1)),
  fresh_model INTEGER NOT NULL CHECK (fresh_model IN (0,1)),
  passed INTEGER NOT NULL CHECK (passed IN (0,1)),
  run_json TEXT NOT NULL,
  created_at TEXT NOT NULL,
  UNIQUE (proposal_id, run_digest)
);
CREATE INDEX IF NOT EXISTS invariant_runs_proposal_idx ON invariant_runs(proposal_id, kind, created_at);

CREATE TABLE IF NOT EXISTS invariant_decisions (
  id TEXT PRIMARY KEY,
  proposal_id TEXT NOT NULL REFERENCES invariant_proposals(id) ON DELETE RESTRICT,
  decision TEXT NOT NULL CHECK (decision IN ('approved','rejected')),
  reviewer TEXT NOT NULL,
  decision_digest TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  activation_qualified INTEGER NOT NULL CHECK (activation_qualified IN (0,1)),
  reason TEXT,
  created_at TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS invariant_decisions_proposal_idx ON invariant_decisions(proposal_id, created_at);

CREATE TABLE IF NOT EXISTS invariant_activations (
  id TEXT PRIMARY KEY,
  proposal_id TEXT NOT NULL REFERENCES invariant_proposals(id) ON DELETE RESTRICT,
  decision_id TEXT NOT NULL REFERENCES invariant_decisions(id) ON DELETE RESTRICT,
  version INTEGER NOT NULL CHECK (version > 0),
  predicate_digest TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  activation_digest TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  registry_revision TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('active','disabled','quarantined')),
  committed_at TEXT NOT NULL,
  UNIQUE (version, predicate_digest),
  UNIQUE (proposal_id, version)
);
CREATE INDEX IF NOT EXISTS invariant_activations_status_idx ON invariant_activations(status, committed_at);

CREATE TABLE IF NOT EXISTS invariant_events (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  activation_id TEXT NOT NULL REFERENCES invariant_activations(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL CHECK (event_type IN ('activated','disabled','quarantined')),
  payload_ref TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  created_at TEXT NOT NULL
);
CREATE UNIQUE INDEX IF NOT EXISTS invariant_events_activation_type ON invariant_events(activation_id, event_type);

CREATE TABLE IF NOT EXISTS invariant_outbox (
  id TEXT PRIMARY KEY,
  activation_id TEXT NOT NULL UNIQUE REFERENCES invariant_activations(id) ON DELETE RESTRICT,
  event_id INTEGER NOT NULL UNIQUE REFERENCES invariant_events(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL,
  dispatched INTEGER NOT NULL DEFAULT 0 CHECK (dispatched IN (0,1)),
  created_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS invariant_disables (
  id TEXT PRIMARY KEY,
  proposal_id TEXT NOT NULL REFERENCES invariant_proposals(id) ON DELETE RESTRICT,
  reviewer TEXT NOT NULL,
  reason TEXT NOT NULL,
  disable_digest TEXT NOT NULL REFERENCES artifacts(hash) ON DELETE RESTRICT,
  created_at TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS invariant_disables_proposal_idx ON invariant_disables(proposal_id, created_at);

CREATE TRIGGER IF NOT EXISTS invariant_proposals_no_update
BEFORE UPDATE ON invariant_proposals BEGIN SELECT RAISE(ABORT, 'invariant proposals are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_proposals_no_delete
BEFORE DELETE ON invariant_proposals BEGIN SELECT RAISE(ABORT, 'invariant proposals are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_runs_no_update
BEFORE UPDATE ON invariant_runs BEGIN SELECT RAISE(ABORT, 'invariant runs are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_runs_no_delete
BEFORE DELETE ON invariant_runs BEGIN SELECT RAISE(ABORT, 'invariant runs are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_decisions_no_update
BEFORE UPDATE ON invariant_decisions BEGIN SELECT RAISE(ABORT, 'invariant decisions are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_decisions_no_delete
BEFORE DELETE ON invariant_decisions BEGIN SELECT RAISE(ABORT, 'invariant decisions are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_activations_no_update
BEFORE UPDATE ON invariant_activations BEGIN SELECT RAISE(ABORT, 'invariant activations are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_activations_no_delete
BEFORE DELETE ON invariant_activations BEGIN SELECT RAISE(ABORT, 'invariant activations are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_events_no_update
BEFORE UPDATE ON invariant_events BEGIN SELECT RAISE(ABORT, 'invariant events are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_events_no_delete
BEFORE DELETE ON invariant_events BEGIN SELECT RAISE(ABORT, 'invariant events are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_disables_no_update
BEFORE UPDATE ON invariant_disables BEGIN SELECT RAISE(ABORT, 'invariant disables are immutable'); END;
CREATE TRIGGER IF NOT EXISTS invariant_disables_no_delete
BEFORE DELETE ON invariant_disables BEGIN SELECT RAISE(ABORT, 'invariant disables are immutable'); END;
