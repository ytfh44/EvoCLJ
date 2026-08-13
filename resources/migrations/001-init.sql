-- 001-init.sql — EvoCLJ initial schema (Milestone 5, Task 5.1)
--
-- Design decisions (normative, Task 5.1):
--   * Ids are stored as TEXT: uuid strings, stable generation ids, and
--     content hashes ("sha256:<hex>").
--   * Queryable metadata payloads are stored as TEXT EDN (pr-str format).
--     Large immutable payloads are referenced by content hash and never
--     duplicated in rows (Global Constraint 21); SQLite rows store
--     references, the bodies live in the filesystem CAS (Task 5.2).
--   * Timestamps are TEXT ISO-8601 (UTC).
--   * Lineage foreign keys are added only where the referenced row is
--     guaranteed to pre-exist by the plan's transaction orders AND where
--     enforcing the link cannot break append-only recovery (Task 5.1
--     Step 4). Content-hash references to the CAS (genome_id, evidence_id,
--     payload_ref, task_ref, ...) deliberately carry NO foreign key: the
--     CAS is filesystem-backed and its rows are written by later tasks,
--     so an FK there would block the append path.
--   * The CURRENT pointer is a marker column + partial unique index (one
--     of the two options Task 5.1 sanctions; see generations below).
--
-- NOTE: `PRAGMA foreign_keys = ON` is a per-connection setting and is NOT
-- persisted. Every runtime connection MUST enable it (evoclj.store.sqlite
-- does). The pragma here applies only to the migration connection.

PRAGMA foreign_keys = ON;

-- ---------------------------------------------------------------------------
-- meta — kernel key/value store; records schema_version and
-- applied_migrations, later kernel-owned metadata (Global Constraint 19).
-- ---------------------------------------------------------------------------
CREATE TABLE meta (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
) WITHOUT ROWID;

-- ---------------------------------------------------------------------------
-- artifacts — content-addressable artifact registry (Database Invariant 1:
-- artifacts.hash unique). The body lives in the filesystem CAS; this row
-- is the durable index entry.
-- ---------------------------------------------------------------------------
CREATE TABLE artifacts (
  hash       TEXT PRIMARY KEY,          -- "sha256:<hex>" content address
  media_type TEXT NOT NULL,
  size       INTEGER NOT NULL CHECK (size >= 0),
  created_at TEXT NOT NULL
) WITHOUT ROWID;

-- ---------------------------------------------------------------------------
-- generations — promoted Genome/Resolution pairs, append-only lineage.
-- Every row keeps its parent link; state transitions are CAS updates that
-- never rewrite identity or lineage columns.
-- ---------------------------------------------------------------------------
CREATE TABLE generations (
  id            TEXT PRIMARY KEY,       -- generation id (stable id)
  genome_id     TEXT NOT NULL,          -- content-addressed GenomeId
  resolution_id TEXT NOT NULL,          -- compiled ResolutionId
  parent_id     TEXT REFERENCES generations (id),
  state         TEXT NOT NULL DEFAULT 'active'
                CHECK (state IN ('active', 'retired', 'rolled-back')),
  current       INTEGER NOT NULL DEFAULT 0 CHECK (current IN (0, 1)),
  created_at    TEXT NOT NULL
);

-- Database Invariant 6 (CURRENT is exactly one row): a partial unique
-- index over current = 1 enforces AT MOST ONE current row at the database
-- level. "Exactly one" is guaranteed by the promotion CAS (Task 9.x): the
-- seed generation is activated with current = 1 and every later promotion
-- clears the parent and sets the child inside a single transaction, so a
-- second current = 1 row can never be created.
CREATE UNIQUE INDEX generations_current_unique
  ON generations (current) WHERE current = 1;

-- Backs the composite candidate lineage foreign key, so a candidate's
-- (parent_generation_id, parent_genome_id) must agree with the stored
-- generation record (Database Invariant 8).
CREATE UNIQUE INDEX generations_id_genome_unique
  ON generations (id, genome_id);

CREATE INDEX generations_parent_idx ON generations (parent_id);

-- ---------------------------------------------------------------------------
-- mutations — structured, deterministic evolution proposals (Global
-- Constraints 4-6). Rejected mutations stay durable negative evidence
-- (Global Constraint 16), so this table is append-only.
-- ---------------------------------------------------------------------------
CREATE TABLE mutations (
  id               TEXT PRIMARY KEY,    -- mutation id (uuid)
  parent_genome_id TEXT NOT NULL,       -- content-addressed GenomeId
  hypothesis_id    TEXT NOT NULL,       -- uuid
  evidence_id      TEXT NOT NULL,       -- ArtifactId of the frozen evidence pack
  risk             TEXT NOT NULL,       -- :parameter | :behavioral | :program | :topology | :meta
  ops              TEXT NOT NULL,       -- EDN vector of structured ops
  expected_effect  TEXT NOT NULL,       -- EDN map
  created_at       TEXT NOT NULL
);

-- ---------------------------------------------------------------------------
-- candidates — materialized successor Genomes. parent_generation_id and
-- mutation_id are lineage FKs: the parent generation and the mutation are
-- persisted before a candidate is materialized (Task 6.2 phase order), so
-- enforcing them cannot break recovery. Invariant 8 is enforced by the
-- composite FK below.
-- ---------------------------------------------------------------------------
CREATE TABLE candidates (
  id                   TEXT PRIMARY KEY, -- candidate id (uuid)
  parent_generation_id TEXT NOT NULL,
  parent_genome_id     TEXT NOT NULL,    -- content-addressed GenomeId
  genome_id            TEXT NOT NULL,    -- candidate GenomeId (content-addressed)
  mutation_id          TEXT NOT NULL,
  evidence_id          TEXT NOT NULL,    -- ArtifactId of the frozen evidence pack
  risk                 TEXT NOT NULL,
  state                TEXT NOT NULL DEFAULT 'materialized'
                       CHECK (state IN ('materialized', 'evaluating',
                                        'eligible', 'promoted',
                                        'rejected', 'stale')),
  created_at           TEXT NOT NULL,
  FOREIGN KEY (parent_generation_id, parent_genome_id)
      REFERENCES generations (id, genome_id),   -- Database Invariant 8
  FOREIGN KEY (mutation_id) REFERENCES mutations (id)
);

CREATE INDEX candidates_parent_generation_idx ON candidates (parent_generation_id);
CREATE INDEX candidates_mutation_idx ON candidates (mutation_id);

-- ---------------------------------------------------------------------------
-- sessions — every session is pinned to one Generation and one
-- (genome_id, resolution_id, phenotype_id) for its whole lifetime
-- (Global Constraint 2, Database Invariant 2). The pinned columns are
-- immutable after insert; Task 5.4's compare-and-set transition updates
-- only :state.
-- ---------------------------------------------------------------------------
CREATE TABLE sessions (
  id            TEXT PRIMARY KEY,       -- session id (uuid)
  generation_id TEXT NOT NULL REFERENCES generations (id),
  genome_id     TEXT NOT NULL,          -- pinned — never updated
  resolution_id TEXT NOT NULL,          -- pinned — never updated
  phenotype_id  TEXT NOT NULL,          -- pinned — never updated
  state         TEXT NOT NULL DEFAULT 'created',
  created_at    TEXT NOT NULL,
  updated_at    TEXT
);

CREATE INDEX sessions_generation_idx ON sessions (generation_id);

-- ---------------------------------------------------------------------------
-- events — append-only causal event log. (session_id, event_seq) is unique
-- and monotonically allocated per session (Database Invariant 3); event_seq
-- > 0 rejects sequence-0 rows that would collide with a "not yet started"
-- sentinel. The triggers below make Invariant 10 true at the database
-- level: a stray UPDATE/DELETE fails loudly instead of silently corrupting
-- the log.
-- ---------------------------------------------------------------------------
CREATE TABLE events (
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id     TEXT NOT NULL REFERENCES sessions (id),
  event_seq      INTEGER NOT NULL CHECK (event_seq > 0),
  generation_id  TEXT NOT NULL REFERENCES generations (id),
  phenotype_id   TEXT NOT NULL,
  event_type     TEXT NOT NULL,         -- namespaced keyword, e.g. ":intent/authorized"
  cause_event_id INTEGER REFERENCES events (id),
  payload_ref    TEXT,                  -- ArtifactId or NULL (Global Constraint 21)
  payload        TEXT,                  -- small EDN metadata
  prev_hash      TEXT,                  -- hash-chain link
  event_hash     TEXT NOT NULL,         -- causal content hash
  created_at     TEXT NOT NULL,
  UNIQUE (session_id, event_seq)        -- Database Invariant 3
);

CREATE INDEX events_session_idx ON events (session_id);
CREATE INDEX events_generation_idx ON events (generation_id);

CREATE TRIGGER events_no_update BEFORE UPDATE ON events
BEGIN
  SELECT RAISE(ABORT, 'events are append-only');
END;

CREATE TRIGGER events_no_delete BEFORE DELETE ON events
BEGIN
  SELECT RAISE(ABORT, 'events are append-only');
END;

-- ---------------------------------------------------------------------------
-- model_calls / tool_calls — per-call effect records anchored to their
-- :provider/call-started event (the transaction protocol persists the
-- call-started event before performing the external effect, so the FK
-- anchor always pre-exists). request_ref/response_ref point at normalized
-- request/result artifacts instead of duplicating payloads.
-- ---------------------------------------------------------------------------
CREATE TABLE model_calls (
  id            TEXT PRIMARY KEY,       -- uuid
  session_id    TEXT NOT NULL REFERENCES sessions (id),
  event_id      INTEGER NOT NULL REFERENCES events (id),
  model         TEXT NOT NULL,
  request_ref   TEXT,                   -- ArtifactId of normalized request
  response_ref  TEXT,                   -- ArtifactId of normalized response
  input_tokens  INTEGER,
  output_tokens INTEGER,
  total_cost    REAL,
  outcome       TEXT NOT NULL CHECK (outcome IN ('completed', 'failed', 'ambiguous')),
  created_at    TEXT NOT NULL
);

CREATE INDEX model_calls_session_idx ON model_calls (session_id);

CREATE TABLE tool_calls (
  id           TEXT PRIMARY KEY,        -- uuid
  session_id   TEXT NOT NULL REFERENCES sessions (id),
  event_id     INTEGER NOT NULL REFERENCES events (id),
  tool_id      TEXT NOT NULL,
  intent_id    TEXT NOT NULL,           -- uuid
  request_ref  TEXT,
  response_ref TEXT,
  outcome      TEXT NOT NULL CHECK (outcome IN ('completed', 'failed', 'ambiguous')),
  created_at   TEXT NOT NULL
);

CREATE INDEX tool_calls_session_idx ON tool_calls (session_id);

-- ---------------------------------------------------------------------------
-- episodes — episodic memory records, distinct from procedural Genome
-- changes (Global Constraint 10). task_ref is the episode's task artifact;
-- first/last event ids bound the causal trace.
-- ---------------------------------------------------------------------------
CREATE TABLE episodes (
  id             TEXT PRIMARY KEY,      -- episode id (uuid)
  session_id     TEXT NOT NULL REFERENCES sessions (id),
  generation_id  TEXT NOT NULL REFERENCES generations (id),
  genome_id      TEXT NOT NULL,
  resolution_id  TEXT NOT NULL,
  task_ref       TEXT NOT NULL,         -- ArtifactId
  first_event_id INTEGER REFERENCES events (id),
  last_event_id  INTEGER REFERENCES events (id),
  outcome        TEXT NOT NULL,         -- EDN map
  usage          TEXT,                  -- EDN map
  created_at     TEXT NOT NULL
);

CREATE INDEX episodes_session_idx ON episodes (session_id);

-- ---------------------------------------------------------------------------
-- eval_runs / eval_cases / eval_results — candidate evaluation. A run is
-- finalized exactly once; reruns create new IDs so a finalized evaluation
-- is immutable (Database Invariant 4). Case selection is informationally
-- isolated (Global Constraints 11, 13).
-- ---------------------------------------------------------------------------
CREATE TABLE eval_runs (
  id                   TEXT PRIMARY KEY, -- evaluation id (uuid)
  candidate_id         TEXT NOT NULL REFERENCES candidates (id),
  parent_generation_id TEXT NOT NULL REFERENCES generations (id),
  profile_id           TEXT NOT NULL,
  gates                TEXT NOT NULL,   -- EDN vector
  paired_results_ref   TEXT,            -- ArtifactId or NULL
  summary              TEXT NOT NULL,   -- EDN map {:hard ... :utility ... :cost ...}
  eligibility          TEXT NOT NULL,   -- EDN map {:eligible? bool :reasons [...]}
  status               TEXT NOT NULL DEFAULT 'running'
                       CHECK (status IN ('running', 'finalized')),
  created_at           TEXT NOT NULL
);

CREATE INDEX eval_runs_candidate_idx ON eval_runs (candidate_id);

CREATE TABLE eval_cases (
  id          TEXT PRIMARY KEY,         -- case id (uuid)
  eval_run_id TEXT NOT NULL REFERENCES eval_runs (id),
  case_ref    TEXT NOT NULL,            -- ArtifactId of the hidden selection case
  created_at  TEXT NOT NULL
);

CREATE INDEX eval_cases_run_idx ON eval_cases (eval_run_id);

CREATE TABLE eval_results (
  id          TEXT PRIMARY KEY,         -- uuid
  eval_run_id TEXT NOT NULL REFERENCES eval_runs (id),
  case_id     TEXT NOT NULL REFERENCES eval_cases (id),
  gate        TEXT NOT NULL,
  passed      INTEGER NOT NULL CHECK (passed IN (0, 1)),
  metric      TEXT,                     -- EDN map
  detail      TEXT,                     -- EDN
  created_at  TEXT NOT NULL
);

CREATE INDEX eval_results_run_idx ON eval_results (eval_run_id);

-- ---------------------------------------------------------------------------
-- capability_leases — bounded HOST-OWNED grants (Task 4.2). Leases are
-- granted to a phenotype subject and are independent of session rows, so
-- they carry no session FK.
-- ---------------------------------------------------------------------------
CREATE TABLE capability_leases (
  id          TEXT PRIMARY KEY,         -- cap id (uuid)
  subject     TEXT NOT NULL,            -- EDN {:phenotype/id ...}
  resource    TEXT NOT NULL,            -- EDN {:kind ... ...}
  actions     TEXT NOT NULL,            -- EDN set
  constraints TEXT NOT NULL,            -- EDN map
  issued_at   TEXT NOT NULL,
  expires_at  TEXT NOT NULL,
  revoked_at  TEXT,
  created_at  TEXT NOT NULL
);

-- ---------------------------------------------------------------------------
-- promotions — atomic CAS promotion decisions (Global Constraint 15).
-- evaluation_id references the finalized eval_run the decision is based on
-- (Database Invariant 5); from/to generation ids bound the pointer move.
-- ---------------------------------------------------------------------------
CREATE TABLE promotions (
  id                 TEXT PRIMARY KEY,  -- promotion id (uuid)
  candidate_id       TEXT NOT NULL REFERENCES candidates (id),
  evaluation_id      TEXT NOT NULL REFERENCES eval_runs (id),   -- Invariant 5
  from_generation_id TEXT NOT NULL REFERENCES generations (id),
  to_generation_id   TEXT NOT NULL REFERENCES generations (id),
  decision           TEXT NOT NULL CHECK (decision IN ('promoted', 'rejected',
                                                       'stale', 'rolled-back')),
  reason             TEXT NOT NULL,     -- EDN map
  created_at         TEXT NOT NULL
);

CREATE INDEX promotions_candidate_idx ON promotions (candidate_id);
