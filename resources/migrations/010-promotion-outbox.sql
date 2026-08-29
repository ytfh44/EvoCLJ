-- 010-promotion-outbox.sql — Fleet P4: promotion outbox (DAG P4)
-- Closes promotion gap — promotion must atomically move CURRENT and append event in same transaction (outbox pattern).
--
-- Before (gap): evoclj.promotion.promote/promote! performed CAS CURRENT inside BEGIN IMMEDIATE
-- then appended :promotion/* event AFTER commit via a separate connection
-- (evoclj.store.event/append-event! owns its own BEGIN IMMEDIATE). A crash or
-- failure between COMMIT and event append leaves promotion without event (or
-- stale marking without event) — atomicity gap. The post-commit event also
-- means a failed event leaves a committed promotion with no audit trail.
--
-- After (outbox): promotion transaction wraps BOTH the generations CURRENT
-- pointer move (cas-current! via kernel_state or generations predicate) AND
-- the event append (INSERT INTO events) in the SAME jdbc transaction/connection.
-- The event row is inserted on the same raw Connection before COMMIT, so either
-- both commit or both rollback (outbox pattern). An outbox table provides a
-- durable FK anchor from promotion → event and enables auditable dispatch;
-- the FK ensures no promotion can exist without its event in the same atomic commit.
--
-- Design decisions:
--  * promotion_outbox is additive: existing promotion rows keep NULL linkage.
--  * FK enforcement: promotion_outbox.promotion_id → promotions(id) and
--    promotion_outbox.event_id → events(id) with RESTRICT so a bogus promotion
--    or event cannot be linked. SQLite FKs are per-connection (PRAGMA foreign_keys=ON)
--    — the promotion transaction enables it.
--  * promotion_id is nullable to allow stale-path outbox rows (stale has no
--    promotions row — the outcome is carried by candidate state + event).
--  * UNIQUE(event_id) and UNIQUE(promotion_id) WHERE NOT NULL enforce 1:1 linkage.
--  * events triggers (events_no_update/delete) remain — transactional insert is
--    the only allowed write, still append-only.
--  * Migration is single-transaction (runner's apply-files! wraps each file).

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS promotion_outbox (
  id            TEXT PRIMARY KEY,  -- outbox id (uuid string)
  promotion_id  TEXT REFERENCES promotions(id) ON DELETE CASCADE,
  session_id    TEXT NOT NULL REFERENCES sessions(id) ON DELETE RESTRICT,
  event_id      INTEGER NOT NULL REFERENCES events(id) ON DELETE RESTRICT,
  event_type    TEXT NOT NULL,
  event_seq     INTEGER NOT NULL,
  created_at    TEXT NOT NULL,
  dispatched    INTEGER NOT NULL DEFAULT 0 CHECK (dispatched IN (0, 1)),
  UNIQUE (event_id),
  UNIQUE (session_id, event_seq)
);

-- 1:1 when promotion exists; stale rows have NULL promotion_id
CREATE UNIQUE INDEX IF NOT EXISTS promotion_outbox_promotion_unique
  ON promotion_outbox (promotion_id) WHERE promotion_id IS NOT NULL;

CREATE INDEX IF NOT EXISTS promotion_outbox_session_idx ON promotion_outbox (session_id);
CREATE INDEX IF NOT EXISTS promotion_outbox_promotion_idx ON promotion_outbox (promotion_id) WHERE promotion_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS promotion_outbox_event_idx ON promotion_outbox (event_id);
CREATE INDEX IF NOT EXISTS promotion_outbox_dispatched_idx ON promotion_outbox (dispatched) WHERE dispatched = 0;
