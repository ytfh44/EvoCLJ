-- 004-enrichment.sql — enrichment store (Foundation F3).
--
-- Enrichment is a versioned, append-only DERIVED-metadata layer attached
-- to immutable content-addressed entities (genomes, candidates, cases).
-- Derived metadata — case weights, curriculum difficulty, generated docs,
-- hall-of-fame flags, lab-notebook narratives — must be attached WITHOUT
-- mutating the entity. This table is that attachment: nothing here rewrites
-- an existing row, and the presence of a derived fact on an entity is itself
-- a new fact (an append to a versioned log), so the layer is append-only.
--
-- Design decisions:
--   * id TEXT PRIMARY KEY — a uuid string, exactly as 001-init.sql treats
--     ids (Global Constraint 21, Task 5.1 conventions).
--   * entity_kind / entity_id — the content-addressed or stable inner key
--     of the immutable entity an enrichment attaches to. Because entities
--     are immutable, this pair is a write-only identity: enrichment never
--     updates it.
--   * kind TEXT — the enrichment's derived-metadata class (e.g. :case/weight
--     :curriculum/difficulty :docs/generated :hof/flag :notebook/narrative).
--   * version INTEGER — an app-transactionally allocated monotonic counter
--     per (entity_kind, entity_id, kind); UNIQUE over the triple enforces
--     immutability at the database level: the same version of the same kind
--     can never be written twice.
--   * payload_ref TEXT — content address ("sha256:<hex>") of the EDN body
--     in the filesystem CAS. SQLite rows REFERENCE the CAS and never
--     duplicate bodies (Global Constraint 21). Like every other content-hash
--     reference in this schema it deliberately carries NO foreign key: the
--     CAS is filesystem-backed and its artifacts may arrive out of band.
--   * cause_ref TEXT — optional provenance: the cause Event id / artifact id
--     / stable id that produced this enrichment (a lab-notebook narrative
--     cites the run that generated it).
--   * created_at TEXT — ISO-8601 (UTC), Task 5.1 conventions.
--
-- The append-only invariant is enforced at the database level by triggers
-- mirroring events_no_update / events_no_delete in 001-init.sql: a stray
-- UPDATE/DELETE on enrichments fails loudly instead of silently corrupting
-- the derived-metadata log.

CREATE TABLE enrichments (
  id          TEXT PRIMARY KEY,       -- enrichment id (uuid string)
  entity_kind TEXT NOT NULL,          -- e.g. :genome | :candidate | :case
  entity_id   TEXT NOT NULL,          -- content address or stable entity id
  kind        TEXT NOT NULL,          -- derived-metadata class
  version     INTEGER NOT NULL,       -- per-(entity_kind, entity_id, kind)
  payload_ref TEXT NOT NULL,          -- content address of the EDN body in the CAS
  cause_ref   TEXT,                   -- optional provenance reference
  created_at  TEXT NOT NULL,          -- ISO-8601 (UTC)
  UNIQUE (entity_kind, entity_id, kind, version)  -- immutability of each version
);

-- Backs lookups over one derived-metadata class of one entity.
CREATE INDEX enrichments_lookup_idx
  ON enrichments (entity_kind, entity_id, kind);

CREATE TRIGGER enrichments_no_update BEFORE UPDATE ON enrichments
BEGIN
  SELECT RAISE(ABORT, 'enrichments are append-only');
END;

CREATE TRIGGER enrichments_no_delete BEFORE DELETE ON enrichments
BEGIN
  SELECT RAISE(ABORT, 'enrichments are append-only');
END;
