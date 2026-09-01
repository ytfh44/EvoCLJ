-- 016-resource-edn.sql — C1: ResourceKindDescriptor migration
-- Migrate capabilities resource identification from the closed
-- resource_kind CHECK + resource_id pair to a faithful `resource_edn`
-- column, and drop the hardcoded kind allowlist (definition now lives in
-- the ResourceKindDescriptor registry, evoclj.capability.resource-kind).
--
-- Design (C1, break compat, clean):
--   * resource_kind  TEXT NOT NULL            — KIND keyword name (no CHECK; open to
--                                               newly registered descriptors). Kept for
--                                               queryability/indexing on the kind axis.
--   * resource_edn   TEXT NOT NULL            — faithful full resource map (EDN), the
--                                               authoritative resource identity.
--   * resource_id    TEXT                     — legacy scalar id, kept nullable for
--                                               backward-compat reads; new writes use
--                                               resource_edn. Old rows backfilled.
--   * Backfill generates a faithful EDN map from the legacy pair:
--       {:kind :<kind> :id "<resource_id>"}  — the <kind>-resource placeholder
--                                               (legacy filesystem/path rows) is
--                                               materialized to its literal id.
--   * The resource_kind IN (...) CHECK is removed: adding a kind is now a
--     single-file Descriptor registration, never a schema change.
-- Migration is a table-recreate (SQLite has no DROP CONSTRAINT), mirroring
-- 015. Single-transaction via the runner.

PRAGMA foreign_keys = OFF;

CREATE TABLE IF NOT EXISTS capabilities_new (
  id                     TEXT PRIMARY KEY,
  principal_type         TEXT NOT NULL CHECK(principal_type IN ('session','job','eval','operator')),
  principal_id           TEXT NOT NULL,
  subject_session_id     TEXT,
  subject_phenotype_id   TEXT,
  resource_kind          TEXT NOT NULL,
  resource_edn           TEXT NOT NULL,
  resource_id            TEXT,
  actions                TEXT NOT NULL CHECK(length(actions) > 2 AND actions != '[]'),
  constraints            TEXT,
  issued_at              TEXT NOT NULL,
  expires_at             TEXT NOT NULL CHECK(expires_at > issued_at),
  revoked                INTEGER NOT NULL DEFAULT 0 CHECK(revoked IN (0, 1)),
  created_at             TEXT NOT NULL
);

-- Backfill: faithful EDN map; kind keywordized, id kept as the legacy string.
-- Replace '"' with '\"' so paths with quotes stay valid EDN.
INSERT OR IGNORE INTO capabilities_new
  (id, principal_type, principal_id, subject_session_id, subject_phenotype_id,
   resource_kind, resource_edn, resource_id, actions, constraints,
   issued_at, expires_at, revoked, created_at)
SELECT id, principal_type, principal_id, subject_session_id, subject_phenotype_id,
       resource_kind,
       '{:kind :' || resource_kind || ' :id "' ||
         replace(replace(resource_id, '\', '\\'), '"', '\"') || '"}',
       resource_id, actions, constraints, issued_at, expires_at, revoked, created_at
FROM capabilities
WHERE NOT EXISTS (SELECT 1 FROM capabilities_new WHERE capabilities_new.id = capabilities.id);

-- Swap tables (identical to 015 pattern)
DROP TABLE IF EXISTS capabilities_old;
ALTER TABLE capabilities RENAME TO capabilities_old;
ALTER TABLE capabilities_new RENAME TO capabilities;
DROP TABLE IF EXISTS capabilities_old;

CREATE INDEX IF NOT EXISTS capabilities_principal_type_idx ON capabilities(principal_type);
CREATE INDEX IF NOT EXISTS capabilities_principal_id_idx ON capabilities(principal_id);
CREATE INDEX IF NOT EXISTS capabilities_subject_session_idx ON capabilities(subject_session_id);
CREATE INDEX IF NOT EXISTS capabilities_resource_kind_idx ON capabilities(resource_kind);
CREATE INDEX IF NOT EXISTS capabilities_revoked_idx ON capabilities(revoked);

PRAGMA foreign_keys = ON;