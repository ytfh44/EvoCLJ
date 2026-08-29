(ns evoclj.store.candidate-store
  "Fleet R — narrow opaque handle for candidate/mutation rows.

  Only this namespace may do jdbc on the candidates/mutations tables
  (Fleet R: make illegal authority unrepresentable — definition >
  validation). Business namespaces (e.g. evoclj.evolution.candidate)
  must receive a CandidateStore, not a raw {:sqlite db :cas cas} map.

  The handle is opaque via deftype — it does NOT expose :db or :sqlite
  via keyword access; (:db handle) is nil. No db-of escape is provided.

  S3 Normalization (Fleet S3, DAG S3 — definition > validation):
    Candidate duplicates Mutation fields (parent_genome_id, evidence_id,
    risk). Mutation is the definition; candidates store only
    mutation_id + genome_id + parent_generation_id + state. The
    derived fields are obtained via JOIN mutations at read time
    (candidates_normalized view) and the physical columns are kept
    for backward compat but enforced to equal the mutation row via
    DB triggers (008-normalize-candidate.sql) and store-level
    derivation on write. See row->candidate and materialize!.

  P5/F (DAG P5/F — CAS FK / existence proof): genome/evidence/payload
    references are existence proofs (VerifiedDigest) at the app boundary
    and FOREIGN KEYs at rest (009-cas-fk-existence.sql). Raw payload_ref
    strings are not proofs and are rejected where a proof is required
    (existence/ensure-proof)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.evolution.candidate-states :as cstates]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.existence :as existence])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; Opaque handle — deftype so (:db handle) is nil
;; ---------------------------------------------------------------------------

(deftype CandidateStore [db])

(defn make-candidate-store
  "Constructor for the narrow CandidateStore handle. `db` is a SQLite
  path string or java.jdbc spec. The handle is opaque — it does not
  expose :db or :sqlite via keyword access."
  [db]
  (when (nil? db)
    (throw (err/error :candidate/store-invalid
                      "CandidateStore requires a non-nil db"
                      {:reason :sqlite-missing})))
  (->CandidateStore db))

;; ---------------------------------------------------------------------------
;; Shared helpers (single source — only this ns does jdbc on candidates/mutations)
;; ---------------------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (err/error :candidate/invalid
                                       "timestamp must be an inst, Instant, or ISO-8601 string"
                                       {:timestamp ts})))]
    (.format timestamp-fmt inst)))

(defn- set-busy-timeout!
  [db ms]
  (let [^java.sql.Connection conn (:connection db)]
    (with-open [stmt (.createStatement conn)]
      (.execute stmt (str "PRAGMA busy_timeout = " ms)))))

;; Single-source DB mapping — delegates to candidate-states (definition > validation)
(def ^:private db-state->state cstates/db-state->kw)
(def ^:private state->db-state cstates/kw->db-state)

(defn- canonical
  [x]
  (cond
    (existence/verified-digest? x) (existence/digest-of x)
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn mutation-hash
  [mutation]
  (hash/text-digest (pr-str (canonical (dissoc mutation :mutation/id)))))

(defn- row->mutation
  [{:keys [id parent_genome_id hypothesis_id evidence_id risk ops
           expected_effect]}]
  {:mutation/id (UUID/fromString id)
   :parent/genome-id parent_genome_id
   :hypothesis/id (UUID/fromString hypothesis_id)
   :evidence/id evidence_id
   :risk (keyword risk)
   :ops (edn/read-string ops)
   :expected-effect (edn/read-string expected_effect)})

(defn- row->candidate
  "Construct Candidate map from a row. When the row is the result of a
  JOIN with mutations (candidates_normalized view or explicit JOIN),
  derived fields (parent_genome_id, evidence_id, risk) are taken from
  the mutation columns (m_parent_genome_id etc.) — definition > validation.
  Falls back to the physical candidates columns for backward compat
  reads (e.g. legacy SELECT *)."
  [row]
  (let [state (get db-state->state (:state row))]
    (when-not state
      (throw (err/error :candidate/invalid
                        "candidates row carries an unknown state"
                        {:candidate/id (:id row) :state (:state row)})))
    ;; S3: derived fields come from JOIN when present; physical columns are
    ;; kept for backward compat but must equal the JOIN values (enforced by
    ;; DB triggers). Prefer the JOIN-derived values.
    {:candidate/id (UUID/fromString (:id row))
     :parent/generation-id (:parent_generation_id row)
     :parent/genome-id (or (:m_parent_genome_id row)
                           (:parent_genome_id row))
     :candidate/genome-id (:genome_id row)
     :mutation/id (UUID/fromString (:mutation_id row))
     :evidence/id (or (:m_evidence_id row)
                      (:evidence_id row))
     :risk (keyword (or (:m_risk row)
                        (:risk row)))
     :state state
     :created-at (Date/from (Instant/parse (:created_at row)))}))

(defn- proof->digest
  "Require a VerifiedDigest existence proof (Fleet P5/F) and return its
  canonical sha256:<64 hex> digest. Raw strings are rejected — they are
  not proofs (definition > validation). The caller must supply a
  VerifiedDigest sealed by evoclj.store.existence (verified-digest or
  the private unsafe-verified-digest for tests via var indirection)."
  [x]
  (existence/digest-of (existence/ensure-proof x)))

(defn- payload-ref-of
  "Extract payload_ref digest from candidate or mutation if present as proof/string."
  [candidate mutation]
  (when-let [v (or (:candidate/payload-ref candidate)
                   (:payload-ref candidate)
                   (:payload-ref mutation))]
    (proof->digest v)))

(defn- insert-mutation-row!
  [conn mutation ts]
  (jdbc/execute! conn
                 ["INSERT OR IGNORE INTO mutations
                   (id, parent_genome_id, hypothesis_id, evidence_id,
                    risk, ops, expected_effect, created_at)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                  (str (:mutation/id mutation))
                  (proof->digest (:parent/genome-id mutation))
                  (str (:hypothesis/id mutation))
                  (proof->digest (:evidence/id mutation))
                  (name (:risk mutation))
                  (pr-str (:ops mutation))
                  (pr-str (:expected-effect mutation))
                  ts])
  (let [row (first (jdbc/query conn
                               ["SELECT parent_genome_id FROM mutations WHERE id = ?"
                                (str (:mutation/id mutation))]))]
    (when (and row (not= (proof->digest (:parent/genome-id mutation)) (:parent_genome_id row)))
      (throw (err/error :candidate/mutation-mismatch
                        "an existing mutation row with this id belongs to a different parent genome"
                        {:mutation/id (:mutation/id mutation)
                         :row/parent-genome-id (:parent_genome_id row)})))))

(defn- find-by-dedupe-key
  [conn parent-genome-id mh]
  (let [rows (jdbc/query conn
                         ["SELECT id, parent_genome_id, hypothesis_id, evidence_id,
                                  risk, ops, expected_effect
                          FROM mutations WHERE parent_genome_id = ?"
                          parent-genome-id])
        matching-ids (into #{}
                           (keep (fn [row]
                                   (when (= mh (mutation-hash (row->mutation row)))
                                     (:id row))))
                           rows)]
    (when (seq matching-ids)
      (let [placeholders (apply str (interpose ", " (repeat (count matching-ids) "?")))
            sql (str "SELECT c.*, m.parent_genome_id AS m_parent_genome_id, m.evidence_id AS m_evidence_id, m.risk AS m_risk
                      FROM candidates c JOIN mutations m ON c.mutation_id = m.id
                      WHERE c.parent_genome_id = ? AND c.mutation_id IN (" placeholders ")
                      ORDER BY c.created_at ASC, c.id ASC")
            params (into [parent-genome-id] matching-ids)]
        (first (jdbc/query conn (into [sql] params)))))))

;; ---------------------------------------------------------------------------
;; Narrow operations — the ONLY jdbc on candidates/mutations
;; ---------------------------------------------------------------------------

(defn materialize!
  "Materialize candidate via CandidateStore (internal Fleet R impl).
  See evoclj.evolution.candidate/materialize-candidate! for contract.

  P5/F Existence proof (DAG P5/F): genome_id/evidence_id/payload_ref MUST be
  VerifiedDigest proofs (sealed by evoclj.store.existence). Raw strings are
  rejected at this boundary (existence/ensure-proof); the DB FK (009) is the
  second enforcement at rest.

  S3 Normalization: parent_genome_id, evidence_id, risk are DERIVED
  from the mutation (definition > validation). The candidate map's
  duplicate fields are ignored; the mutation's values are written to
  the physical columns (kept for backward compat) and also serve as
  the source for row->candidate via JOIN. DB triggers enforce equality
  so a mismatch is unrepresentable even via raw SQL."
  [^CandidateStore store candidate mutation]
  (when-not (instance? CandidateStore store)
    (throw (err/error :candidate/store-invalid
                      "materialize! requires a CandidateStore"
                      {:reason :not-a-candidate-store})))
  (let [db (.-db ^CandidateStore store)
        mh (mutation-hash mutation)
        ts (canonical-timestamp (:created-at candidate))]
    (sqlite/with-db [conn db]
      (set-busy-timeout! conn 10000)
      (insert-mutation-row! conn mutation ts)
      (if-let [row (find-by-dedupe-key conn (proof->digest (:parent/genome-id mutation)) mh)]
        ;; find-by-dedupe-key now returns a JOIN-derived row; normalize via row->candidate
        (row->candidate row)
        (do
          ;; S3: derive duplicate fields from mutation, not candidate
          (let [payload-ref (payload-ref-of candidate mutation)
              parent-genome-id (proof->digest (:parent/genome-id mutation))
              cand-genome-id (proof->digest (:candidate/genome-id candidate))
              evid-id (proof->digest (:evidence/id mutation))]
          (jdbc/insert! conn :candidates
                        (cond-> {:id (str (:candidate/id candidate))
                                 :parent_generation_id (:parent/generation-id candidate)
                                 :parent_genome_id parent-genome-id
                                 :genome_id cand-genome-id
                                 :mutation_id (str (:mutation/id mutation))
                                 :evidence_id evid-id
                                 :risk (name (:risk mutation))
                                 :state "materialized"
                                 :created_at ts}
                          payload-ref (assoc :payload_ref payload-ref))))
          (row->candidate
           (first (jdbc/query conn
                              ["SELECT c.*, m.parent_genome_id AS m_parent_genome_id, m.evidence_id AS m_evidence_id, m.risk AS m_risk
                                FROM candidates c JOIN mutations m ON c.mutation_id = m.id
                                WHERE c.id = ?"
                               (str (:candidate/id candidate))]))))))))

(defn transition!
  "CAS state transition via CandidateStore. Returns updated candidate.
  S3: returns JOIN-derived candidate."
  [^CandidateStore store candidate-id expected-state new-state]
  (when-not (instance? CandidateStore store)
    (throw (err/error :candidate/store-invalid
                      "transition! requires a CandidateStore"
                      {:reason :not-a-candidate-store})))
  ;; Fleet S2 — reject non-persisted targets before the DB CHECK (NULL would fail).
  (when-not (state->db-state new-state)
    (throw (err/error :candidate/invalid-transition
                      "target state has no DB mapping (not persistable in 5.1)"
                      {:candidate/id (types/session-id candidate-id)
                       :expected-state expected-state
                       :new-state new-state})))
  (let [cid (types/session-id candidate-id)
        key (str cid)
        db (.-db ^CandidateStore store)]
    (sqlite/with-db [conn db]
      (set-busy-timeout! conn 10000)
      (let [count (first (jdbc/execute! conn
                                        ["UPDATE candidates
                                          SET state = ?
                                          WHERE id = ? AND state = ?"
                                         (state->db-state new-state)
                                         key
                                         (state->db-state expected-state)]))]
        (when-not (= 1 count)
          (let [row (first (jdbc/query conn
                                       ["SELECT state FROM candidates WHERE id = ?"
                                        key]))]
            (if row
              (throw (err/error :candidate/invalid-transition
                                "candidate is not in the expected state"
                                {:candidate/id cid
                                 :expected-state expected-state
                                 :new-state new-state
                                 :actual-state (db-state->state (:state row))}))
              (throw (err/error :candidate/not-found
                                "no candidate with this id"
                                {:candidate/id cid})))))
        (some-> (first (jdbc/query conn
                                   ["SELECT c.*, m.parent_genome_id AS m_parent_genome_id, m.evidence_id AS m_evidence_id, m.risk AS m_risk
                                     FROM candidates c JOIN mutations m ON c.mutation_id = m.id
                                     WHERE c.id = ?" key]))
                row->candidate)))))

(defn find-candidate
  "Find candidate by id via CandidateStore, or nil.
  S3: derived fields are obtained via JOIN mutations (definition > validation)."
  [^CandidateStore store candidate-id]
  (when-not (instance? CandidateStore store)
    (throw (err/error :candidate/store-invalid
                      "find-candidate requires a CandidateStore"
                      {:reason :not-a-candidate-store})))
  (some-> (first (sqlite/query (.-db ^CandidateStore store)
                               ["SELECT c.*, m.parent_genome_id AS m_parent_genome_id, m.evidence_id AS m_evidence_id, m.risk AS m_risk
                                 FROM candidates c JOIN mutations m ON c.mutation_id = m.id
                                 WHERE c.id = ?"
                                (str (types/session-id candidate-id))]))
          row->candidate))

(defn find-candidates-by-parent
  "Find all candidates for parent genome via CandidateStore.
  S3: derived fields via JOIN."
  [^CandidateStore store parent-genome-id]
  (when-not (instance? CandidateStore store)
    (throw (err/error :candidate/store-invalid
                      "find-candidates-by-parent requires a CandidateStore"
                      {:reason :not-a-candidate-store})))
  (->> (sqlite/query (.-db ^CandidateStore store)
                     ["SELECT c.*, m.parent_genome_id AS m_parent_genome_id, m.evidence_id AS m_evidence_id, m.risk AS m_risk
                       FROM candidates c JOIN mutations m ON c.mutation_id = m.id
                       WHERE c.parent_genome_id = ?
                       ORDER BY c.created_at ASC, c.id ASC"
                      parent-genome-id])
       (mapv row->candidate)))