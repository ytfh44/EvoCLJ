(ns evoclj.store.artifact
  "Idempotent registration of content-addressed identity rows.

  CAS owns bytes; this small catalog boundary owns the SQLite rows that
  make those identities available to foreign-keyed domain records. Callers
  must put real CAS content first when an artifact has a body."
  (:require [evoclj.store.sqlite :as sqlite]))

(defn ensure-artifact!
  "Register `artifact-id` once and return it.

  The operation is intentionally idempotent: identity rows are shared by
  generations, sessions, candidates, evaluations, and event payloads."
  [db artifact-id media-type size]
  (sqlite/exec! db
                ["INSERT OR IGNORE INTO artifacts
                  (hash, media_type, size, created_at)
                  VALUES (?, ?, ?, datetime('now'))"
                 artifact-id media-type size])
  artifact-id)

(defn ensure-genome!
  "Register a Genome identity after its artifact row exists and return it."
  [db genome-id]
  (sqlite/exec! db
                ["INSERT OR IGNORE INTO genomes (id, created_at)
                  VALUES (?, datetime('now'))"
                 genome-id])
  genome-id)
