(ns evoclj.store.recovery
  "Startup recovery and integrity scans (Task 5.5).

  `scan-recovery-state` is the normative read-only scan. It NEVER
  writes: no event is appended, no session state is rewritten, no
  candidate is promoted. Recovery classifies crash residue so the
  runtime can act on it; it does not pretend completion.

      (scan-recovery-state store cas)
      ;; => {:orphaned-sessions   [...]
      ;;     :missing-artifacts   [...]
      ;;     :invalid-event-chains [...]
      ;;     :stale-candidates    [...]}

  Category semantics (Task 5.5 Steps 1-3):

  * :orphaned-sessions — sessions whose persisted :state is non-terminal
    AND whose event log contains no terminal session event
    (:session/completed, :session/failed, :session/cancelled,
    :session/budget-exhausted): the process died mid-flight. Each entry
    carries :session/id, :state, and the last :event/seq recorded
    (nil for a session with no events). The scan classifies them as
    orphaned and does NOT pretend completion — no terminal event is
    fabricated and the row state is untouched (Step 1). A session whose
    log already holds a terminal event is finished regardless of its row
    state and is never reported.

  * :missing-artifacts — events carrying a :payload-ref whose CAS
    artifact is absent. Rows reference payloads by content hash (Global
    Constraint 21); a reference that does not resolve is reported loudly
    with the offending session/seq/type (Step 2). Existence is checked
    with cas/exists? (the CAS itself fails loudly on a verifying read if
    a present body is later consumed and corrupted).

  * :invalid-event-chains — sessions whose hash chain fails
    evoclj.store.event/verify-event-chain (Task 5.3 Step 5): a tampered
    or corrupted historical row. Each entry is the verify failure
    (reason, event seq, expected/actual hashes) plus :session/id.

  * :stale-candidates — candidate rows still in a prepared state
    (:materialized, :evaluating, :eligible) when the process died. They
    are reported with their original :state; recovery MUST NOT promote
    them — promotion is exclusively the Task 9 compare-and-set path and
    this scan performs no writes at all (Step 3).

  `startup-integrity-scan` (Step 4) wraps the recovery scan with a
  configurable strict mode. The PRODUCTION DEFAULT is strict
  (fail-closed): when the scan finds corruption — an unresolved payload
  reference, an invalid event chain, or a CURRENT generation whose
  Genome artifact is absent or fails content verification (Database
  Invariant 7) — strict mode throws :store/integrity-failure carrying
  the full report, so the runtime refuses to start. Orphaned sessions
  and stale candidates are NOT corruption: they are the expected residue
  of a crash and never block startup. A missing/ambiguous CURRENT row
  when generations exist violates Database Invariant 6 and is a hard
  finding too; an empty store (:none) has nothing to protect.

  Known boundary: the current-generation check verifies that the
  generation's genome_id resolves to an intact CAS artifact (existence
  plus a re-hashing read). Tree-level Genome loading and manifest
  verification belong to the Genome loader (Milestone 6+); at this
  milestone the store enforces the durable half of Invariant 7."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

(def terminal-session-event-types
  "Event types that close a session's lifecycle. A session whose log
  contains one of these is finished; a session with a non-terminal row
  state and none of these is orphaned (Task 5.5 Step 1)."
  #{:session/completed :session/failed :session/cancelled
    :session/budget-exhausted})

(def prepared-candidate-states
  "Candidate states that mean materialization or evaluation was in
  flight when the process died: :materialized, :evaluating, :eligible
  (not yet promoted, rejected, or stale). These are reported as stale;
  recovery never promotes them."
  #{:materialized :evaluating :eligible})

(def ^:private terminal-event-sql
  "SQL IN-list literal matching the four terminal session event types as
  stored (evoclj.store.event/type->db renders them without a leading
  colon)."
  "('session/completed','session/failed','session/cancelled','session/budget-exhausted')")

;; --- helpers ----------------------------------------------------------------

(defn- cas-root
  "The storage root of a cas argument (config map or bare root path)."
  [cas]
  (if (map? cas) (:root cas) cas))

(defn- verifying-cas
  "A cas config with read verification enabled, for content integrity
  reads (every body is re-hashed and compared to its id)."
  [cas]
  (cas/->cas (cas-root cas) {:verify true}))

;; --- the four normative categories ------------------------------------------

(defn- orphaned-sessions
  "Sessions whose row state is non-terminal AND whose event log holds no
  terminal session event: {:session/id uuid, :state kw,
  :last-event-seq int-or-nil}."
  [store]
  (let [rows (sqlite/query store ["SELECT id, state FROM sessions"])
        terminal-ids (set (map :session_id
                               (sqlite/query store
                                             [(str "SELECT DISTINCT session_id FROM events
                                                    WHERE event_type IN " terminal-event-sql)])))
        last-seqs (into {}
                        (map (juxt :session_id :last_event_seq))
                        (sqlite/query store
                                      ["SELECT session_id, MAX(event_seq) AS last_event_seq
                                        FROM events GROUP BY session_id"]))]
    (into []
          (keep (fn [row]
                  (let [sid (:id row)
                        state (keyword (:state row))]
                    (when (and (not (contains? session/terminal-states state))
                               (not (contains? terminal-ids sid)))
                      {:session/id (UUID/fromString sid)
                       :state state
                       :last-event-seq (get last-seqs sid)}))))
          rows)))

(defn- missing-artifacts
  "Events whose :payload-ref content address does not resolve in the
  CAS: {:session/id uuid, :event/seq int, :event/type kw,
  :payload-ref string}."
  [store cas]
  (into []
        (keep (fn [row]
                (let [ref (:payload_ref row)]
                  (when (and ref (not (cas/exists? cas ref)))
                    {:session/id (UUID/fromString (:session_id row))
                     :event/seq (:event_seq row)
                     :event/type (keyword (:event_type row))
                     :payload-ref ref}))))
        (sqlite/query store
                      ["SELECT session_id, event_seq, event_type, payload_ref
                        FROM events WHERE payload_ref IS NOT NULL"])))

(defn- invalid-event-chains
  "Sessions whose event hash chain fails verification; each entry is the
  verify-event-chain failure map plus :session/id."
  [store]
  (into []
        (keep (fn [row]
                (let [sid (UUID/fromString (:id row))
                      v (event/verify-event-chain store sid)]
                  (when-not (:valid? v)
                    (assoc v :session/id sid)))))
        (sqlite/query store ["SELECT id FROM sessions"])))

(defn- row->candidate
  "A candidates row as the public Candidate contract map."
  [row]
  {:candidate/id (UUID/fromString (:id row))
   :parent/generation-id (:parent_generation_id row)
   :parent/genome-id (:parent_genome_id row)
   :candidate/genome-id (:genome_id row)
   :mutation/id (UUID/fromString (:mutation_id row))
   :evidence/id (:evidence_id row)
   :risk (keyword (:risk row))
   :state (keyword (:state row))
   :created-at (Date/from (Instant/parse (:created_at row)))})

(defn- stale-candidates
  "Candidate rows still in a prepared state: public Candidate maps with
  their original :state. Reported, never promoted."
  [store]
  (mapv row->candidate
        (sqlite/query store
                      ["SELECT * FROM candidates
                        WHERE state IN ('materialized','evaluating','eligible')"])))

;; --- CURRENT generation integrity (Database Invariants 6 and 7) -------------

(defn- current-generation
  "Verify the CURRENT pointer (Invariant 6: exactly one current row) and
  the integrity of the current generation's Genome in the CAS
  (Invariant 7: an active generation's Genome must exist and pass
  integrity).

  Returns {:status :ok :generation/id ... :genome/id ...} when exactly
  one current generation exists and its genome_id resolves to an intact
  CAS artifact (a verifying read re-hashes the body); {:status :none}
  for an empty store; {:status :missing-current} when generations exist
  but no current row does; {:status :ambiguous} for more than one
  current row (unreachable through the partial unique index); and
  {:status :missing} / {:status :corrupt} when the genome artifact is
  absent or its bytes do not re-hash to the id."
  [store cas]
  (let [gen-count (count (sqlite/query store ["SELECT id FROM generations"]))
        rows (sqlite/query store ["SELECT id, genome_id FROM generations WHERE current = 1"])]
    (cond
      (zero? gen-count) {:status :none}
      (empty? rows) {:status :missing-current}
      (> (count rows) 1) {:status :ambiguous :generation/ids (mapv :id rows)}
      :else
      (let [row (first rows)
            gen-id (:id row)
            genome-id (:genome_id row)]
        (try
          (if-not (cas/exists? cas genome-id)
            {:status :missing :generation/id gen-id :genome/id genome-id}
            (do (cas/get-bytes (verifying-cas cas) genome-id)
                {:status :ok :generation/id gen-id :genome/id genome-id}))
          (catch clojure.lang.ExceptionInfo e
            {:status :corrupt :generation/id gen-id :genome/id genome-id
             :reason (:error/type (ex-data e))}))))))

;; --- public API -------------------------------------------------------------

(defn scan-recovery-state
  "The normative recovery scan (Task 5.5 interface). Read-only: it
  classifies crash residue and reports corruption; it never appends,
  rewrites, or promotes anything.

  Returns {:orphaned-sessions [...] :missing-artifacts [...]
  :invalid-event-chains [...] :stale-candidates [...]}."
  [store cas]
  {:orphaned-sessions (orphaned-sessions store)
   :missing-artifacts (missing-artifacts store cas)
   :invalid-event-chains (invalid-event-chains store)
   :stale-candidates (stale-candidates store)})

(defn- hard-findings
  "The corruption findings strict mode fails closed on: unresolved
  payload references, invalid event chains, and a broken CURRENT
  generation. Orphaned sessions and stale candidates are recoverable
  crash residue and never count."
  [report]
  (concat (:missing-artifacts report)
          (:invalid-event-chains report)
          (when-let [cg (:current-generation report)]
            (when (contains? #{:missing :corrupt :missing-current :ambiguous}
                             (:status cg))
              [cg]))))

(defn startup-integrity-scan
  "Startup integrity scan with configurable strict mode (Task 5.5
  Step 4). The production default is strict (fail-closed).

  Runs scan-recovery-state, verifies the CURRENT generation (Database
  Invariants 6 and 7), and — in strict mode (the default, {:strict?
  false} to disable) — throws :store/integrity-failure carrying the
  full report (the four normative categories plus :current-generation)
  when any hard finding exists: a missing payload artifact, an invalid
  event chain, or a CURRENT generation whose genome artifact is
  absent/corrupt (or a missing/ambiguous CURRENT). Orphaned sessions
  and stale candidates never block startup.

  Returns the report augmented with :current-generation
  {:status :ok|:none|:missing|:corrupt|:missing-current|:ambiguous ...}
  and :ok? (true when there are no hard findings)."
  [store cas & [opts]]
  (let [{:keys [strict?] :or {strict? true}} opts
        report (scan-recovery-state store cas)
        current (current-generation store cas)
        report (assoc report :current-generation current)
        ok? (empty? (hard-findings report))]
    (if (and strict? (not ok?))
      (throw (err/error :store/integrity-failure
                        "startup integrity scan found corruption; refusing to start"
                        report))
      (assoc report :ok? ok?))))
