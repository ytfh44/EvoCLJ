-- 021-capability-budgets.sql — finite capability budget market
-- JSON stores canonical non-negative integer dimensions. BEGIN IMMEDIATE in the
-- store serializes sibling reservations and prevents oversubscription.
CREATE TABLE IF NOT EXISTS capability_budgets (
  id             TEXT PRIMARY KEY,
  lease_id       TEXT,
  parent_id      TEXT REFERENCES capability_budgets(id) ON DELETE CASCADE,
  root_id        TEXT NOT NULL,
  budget_json    TEXT NOT NULL,
  reserved_json  TEXT NOT NULL DEFAULT '{}',
  consumed_json  TEXT NOT NULL DEFAULT '{}',
  state          TEXT NOT NULL CHECK(state IN ('active','revoked','closed')),
  expires_at     TEXT,
  created_at     TEXT NOT NULL,
  updated_at     TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS capability_budgets_parent_idx ON capability_budgets(parent_id);
CREATE INDEX IF NOT EXISTS capability_budgets_root_idx ON capability_budgets(root_id);
CREATE INDEX IF NOT EXISTS capability_budgets_lease_idx ON capability_budgets(lease_id);

CREATE TABLE IF NOT EXISTS capability_budget_reservations (
  id             TEXT PRIMARY KEY,
  budget_id      TEXT NOT NULL REFERENCES capability_budgets(id) ON DELETE CASCADE,
  intent_id      TEXT,
  attempt        INTEGER NOT NULL,
  idempotency_key TEXT NOT NULL UNIQUE,
  amount_json    TEXT NOT NULL,
  status         TEXT NOT NULL CHECK(status IN ('reserved','settled','released')),
  created_at     TEXT NOT NULL,
  settled_json   TEXT NOT NULL DEFAULT '{}',
  settled_at     TEXT
);
CREATE INDEX IF NOT EXISTS capability_budget_reservations_budget_idx
  ON capability_budget_reservations(budget_id);

CREATE TABLE IF NOT EXISTS capability_budget_ledger (
  id             TEXT PRIMARY KEY,
  budget_id      TEXT NOT NULL REFERENCES capability_budgets(id) ON DELETE CASCADE,
  reservation_id TEXT,
  event_type     TEXT NOT NULL CHECK(event_type IN ('allocate','reserve','settle','release','reallocate','revoke','recover')),
  amount_json    TEXT NOT NULL,
  idempotency_key TEXT NOT NULL UNIQUE,
  created_at     TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS capability_budget_ledger_budget_idx
  ON capability_budget_ledger(budget_id);
