-- 024-invariant-disable-outbox.sql — allow one outbox row per activation event
PRAGMA foreign_keys = ON;

ALTER TABLE invariant_outbox RENAME TO invariant_outbox_023;
CREATE TABLE invariant_outbox (
  id TEXT PRIMARY KEY,
  activation_id TEXT NOT NULL REFERENCES invariant_activations(id) ON DELETE RESTRICT,
  event_id INTEGER NOT NULL UNIQUE REFERENCES invariant_events(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL CHECK (event_type IN ('activated','disabled','quarantined')),
  dispatched INTEGER NOT NULL DEFAULT 0 CHECK (dispatched IN (0,1)),
  created_at TEXT NOT NULL,
  UNIQUE (activation_id, event_type)
);
INSERT INTO invariant_outbox (id, activation_id, event_id, event_type, dispatched, created_at)
SELECT id, activation_id, event_id, event_type, dispatched, created_at FROM invariant_outbox_023;
DROP TABLE invariant_outbox_023;
