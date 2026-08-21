-- 006-session-bindings.sql — durable session bindings (dynamic environment).
--
-- A session binding pins a logical skill/context to an exact immutable
-- bundle revision for the lifetime of the session (or until an explicit
-- reload). Bindings are runtime environment, NOT compile-time identity:
-- they never touch PhenotypeID or Resolution (stored in generations/
-- sessions) and installing an unused Skill must not change PhenotypeID.
--
-- The registry's current revision (the catalog projection) moves on
-- refresh; an active binding stays at revision A until an explicit
-- reload moves it to B. Only reload writes session_bindings and is
-- auditable via the event log; refresh is catalog-only.
--
-- Lookup is via CAS (revision_id -> artifact/tree), never via the
-- current catalog, so a binding survives source removal or a process
-- restart: the immutable tree A still exists in CAS even when the
-- catalog no longer offers it.
--
-- Sibling surfaces (Context, Directory, Tools) in one bundle are
-- co-versioned (same revision_id) and published atomically; restoring
-- a binding restores all siblings at the same revision.

CREATE TABLE session_bindings (
  id             TEXT PRIMARY KEY,       -- binding instance id (uuid string)
  session_id     TEXT NOT NULL REFERENCES sessions (id),
  binding_type   TEXT NOT NULL,         -- e.g. 'skill', 'context', 'directory', 'tools'
  logical_id     TEXT NOT NULL,         -- EDN pr-str of the logical vector, e.g. "[:skill \"debugging\"]"
  revision_id    TEXT NOT NULL,         -- content identity "sha256:<64 hex>" (CAS key)
  bundle_id      TEXT NOT NULL,         -- bundle that was activated
  state          TEXT NOT NULL CHECK (state IN ('active', 'inactive')),
  activated_at   TEXT NOT NULL,         -- ISO-8601 (UTC)
  deactivated_at TEXT,                  -- ISO-8601 (UTC), null while active
  metadata_edn   TEXT                   -- optional EDN map (surfaces, source provenance)
);

-- Lookup active bindings for a session.
CREATE INDEX session_bindings_session_idx
  ON session_bindings (session_id);

-- Lookup by session + logical binding.
CREATE INDEX session_bindings_session_logical_idx
  ON session_bindings (session_id, logical_id);

-- At most one active binding per (session, logical_id).
CREATE UNIQUE INDEX session_bindings_active_unique
  ON session_bindings (session_id, logical_id) WHERE state = 'active';

-- Backs bundle/revision audits.
CREATE INDEX session_bindings_bundle_idx
  ON session_bindings (bundle_id);

CREATE INDEX session_bindings_revision_idx
  ON session_bindings (revision_id);
