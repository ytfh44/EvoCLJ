-- 017-event-prev-causal-links.sql — E1: split Event.cause into prev (local linear) + causal_links (cross-session graph)
-- Design decisions (E1, break compat, clean):
--   * Event = { event/id, session/id, seq, prev-event-id (local), causal-links: #{Edge{from,to,type}}, payload-ref, hash }
--   * prev-event-id = linear predecessor in SAME session (seq = new-seq -1, nil for root). Validated at append.
--   * causal_links = cross-session semantic causality (e.g. child terminal -> parent result). May reference any session.
--   * Breaking change: old `cause_event_id` column is deprecated but kept for backward reads. New writes populate
--     `prev_event_id`; old rows are migrated: cause → prev when same-session, otherwise cause → causal_links edge.
--   * New table `causal_links(from_event_id, to_event_id, link_type, created_at)` with composite PK.
--   * Migration is single-transaction via the runner. Idempotency: ALTER ADD COLUMN is one-shot; causal_links IF NOT EXISTS.
--
-- Acceptance (E1):
--   * same-session seq linear via prev, cross-session via causal_links, subagent result via causal_links, store tests 0F.

PRAGMA foreign_keys = OFF;

-- 1. Add prev_event_id column (linear predecessor, same session only)
ALTER TABLE events ADD COLUMN prev_event_id INTEGER REFERENCES events(id);

-- 2. New causal_links table (graph edges, may cross session)
CREATE TABLE IF NOT EXISTS causal_links (
  from_event_id INTEGER NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  to_event_id   INTEGER NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  link_type     TEXT NOT NULL,
  created_at    TEXT NOT NULL,
  PRIMARY KEY (from_event_id, to_event_id, link_type)
) WITHOUT ROWID;

CREATE INDEX IF NOT EXISTS causal_links_from_idx ON causal_links(from_event_id);
CREATE INDEX IF NOT EXISTS causal_links_to_idx ON causal_links(to_event_id);
CREATE INDEX IF NOT EXISTS causal_links_type_idx ON causal_links(link_type);

-- Index for prev lookups
CREATE INDEX IF NOT EXISTS events_prev_idx ON events(prev_event_id);
-- Keep existing cause index if any; add if missing
CREATE INDEX IF NOT EXISTS events_cause_idx ON events(cause_event_id);

-- 3. Backfill prev_event_id from legacy cause_event_id where same-session
--    (All legacy causes were required to be same-session by :store/cause-session-mismatch,
--    so this backfills every non-nil cause.)
UPDATE events
SET prev_event_id = cause_event_id
WHERE cause_event_id IS NOT NULL
  AND prev_event_id IS NULL
  AND EXISTS (
    SELECT 1 FROM events AS c
    WHERE c.id = events.cause_event_id
      AND c.session_id = events.session_id
  );

-- 4. For any legacy cause that was cross-session (should be 0 rows on a valid DB),
--    materialize it as a causal_links edge and wire prev to the local predecessor
--    (previous seq in same session) so the linear chain stays valid.
INSERT OR IGNORE INTO causal_links (from_event_id, to_event_id, link_type, created_at)
SELECT cause_event_id, id, 'cause', created_at
FROM events
WHERE cause_event_id IS NOT NULL
  AND NOT EXISTS (
    SELECT 1 FROM events AS c
    WHERE c.id = events.cause_event_id
      AND c.session_id = events.session_id
  );

-- For those cross-session rows, set prev to local predecessor if we haven't already
UPDATE events
SET prev_event_id = (
  SELECT p.id FROM events AS p
  WHERE p.session_id = events.session_id
    AND p.event_seq = events.event_seq - 1
)
WHERE cause_event_id IS NOT NULL
  AND prev_event_id IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM events AS c
    WHERE c.id = events.cause_event_id
      AND c.session_id = events.session_id
  )
  AND EXISTS (
    SELECT 1 FROM events AS p2
    WHERE p2.session_id = events.session_id
      AND p2.event_seq = events.event_seq - 1
  );

-- Root events keep prev NULL (already the case for cause NULL)

PRAGMA foreign_keys = ON;
