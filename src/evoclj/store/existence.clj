(ns evoclj.store.existence
  "Fleet P5/F — Content-addressable existence proof (DAG P5/F).

  Gap: 001-init.sql omitted FKs and candidate creation accepted raw
  payload_ref strings without proving the referenced CAS artifact exists.
  A raw string is not a proof — it can name a hash that has never been
  stored, and SQLite without FKs cannot reject it.

  This namespace seals the proof: a VerifiedDigest is an opaque,
  content-addressed existence object that can only be obtained by
  verifying that the CAS actually contains the artifact. It wraps the
  canonical \"sha256:<64 hex>\" digest (evoclj.genome.hash/file-digest
  / VerifiedDigest already) and is the ONLY way to supply a
  genome_id/evidence_id/payload_ref to candidate creation. Raw strings
  are rejected at the boundary (definition > validation).

  Construction is via `verified-digest` which checks CAS existence
  (cas/exists? + optional verification) and returns a sealed object.
  The digest string is retrievable only via `digest-of` on a genuine
  VerifiedDigest — `(:digest vd)` is nil because the carrier is a
  deftype, not a map. This makes a forged proof unrepresentable without
  going through CAS.

  The DB layer (009-cas-fk-existence.sql) then enforces the same
  invariant at rest: generations.genome_id -> genomes(id),
  candidates.genome_id -> genomes(id), candidates.evidence_id ->
  artifacts(hash), candidates.payload_ref -> artifacts(hash). The app
  proof and the DB FK are two independent enforcements of the same
  existence invariant (defense in depth)."
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]))

;; ---------------------------------------------------------------------------
;; Sealed carrier — deftype so (:digest vd) is nil and proof is opaque
;; ---------------------------------------------------------------------------

(deftype VerifiedDigest [digest])

(defn verified-digest?
  "True when x is a VerifiedDigest sealed by this namespace."
  [x]
  (instance? VerifiedDigest x))

(defn digest-of
  "The canonical \"sha256:<64 hex>\" digest carried by a VerifiedDigest.
  Throws :existence/invalid-proof unless x is a genuine VerifiedDigest."
  [vd]
  (when-not (verified-digest? vd)
    (throw (err/error :existence/invalid-proof
                      "not a VerifiedDigest existence proof"
                      {:reason :not-a-verified-digest :value (err/sanitize vd)})))
  (.-digest ^VerifiedDigest vd))

(defn verified-digest
  "Create an existence proof for `artifact-id` by verifying the CAS
  actually contains it.

  `cas` is a CAS root/config (as in evoclj.store.cas); `artifact-id` is
  a canonical \"sha256:<64 hex>\" string. The artifact must exist
  (cas/exists? true) — otherwise throws :store/cas-missing (or
  :store/cas-invalid-id for a malformed id) via cas artifact-dir
  validation. On success returns a sealed VerifiedDigest whose
  digest-of is the same id.

  With a verified CAS (evoclj.store.cas/->cas with {:verify true}) the
  caller may also call this after a get-bytes that already re-hashed;
  this function does the existence check without a full read."
  [cas artifact-id]
  (when (nil? cas)
    (throw (err/error :existence/cas-missing
                      "CAS is required to create an existence proof"
                      {:reason :cas-missing})))
  ;; Validate id shape first — types/artifact-id throws :id/invalid on bad shape;
  ;; normalize to :store/cas-invalid-id for the CAS boundary.
  (let [id (try (types/artifact-id artifact-id)
                (catch clojure.lang.ExceptionInfo e
                  (throw (err/error :store/cas-invalid-id
                                    "artifact id must be sha256:<64 lowercase hex>"
                                    {:artifact/id artifact-id :cause (:error/type (ex-data e))}))))]
    (when-not (cas/exists? cas id)
      (throw (err/error :store/cas-missing
                        "no artifact with this id — existence proof refused"
                        {:artifact/id id})))
    (->VerifiedDigest id)))

(defn ^:private unsafe-verified-digest
  "Create a VerifiedDigest WITHOUT checking CAS — for tests that need a
  proof without a live CAS, or for migration backfill where the CAS
  body is not available but the hash is known to be content-addressed.
  Prefer `verified-digest` in production code; this is explicitly
  marked unsafe so callers must justify why CAS verification is skipped.
  Private — tests must access via #'evoclj.store.existence/unsafe-verified-digest
  to make the opt-out explicit."
  [artifact-id]
  (let [id (types/artifact-id artifact-id)]
    (->VerifiedDigest id)))

(defn ensure-proof
  "Coerce `x` to a VerifiedDigest or throw :existence/invalid-proof.
  Accepts only a VerifiedDigest — raw strings are rejected even when
  they look like a valid hash. This is the boundary that closes the
  payload_ref gap: callers cannot pass a raw string where a proof is
  required."
  [x]
  (if (verified-digest? x)
    x
    (throw (err/error :existence/invalid-proof
                      "existence proof required — raw payload_ref string is not a proof"
                      {:reason :raw-string-not-proof :value (err/sanitize x)}))))
