(ns evoclj.store.recovery
  "Startup recovery and integrity scans (component).

  `scan-recovery-state` is the normative read-only scan. It NEVER
  writes: no event is appended, no Session identity row is rewritten, no
  candidate is promoted. Recovery classifies crash residue so the runtime
  can act on it; it does not pretend completion.

      (scan-recovery-state store cas)
      ;; => {:missing-artifacts   [...]
      ;;     :invalid-event-chains [...]
      ;;     :stale-candidates    [...]}
  Category semantics (component Steps 1-3):


  * :missing-artifacts — events carrying a :payload-ref whose CAS
    artifact is absent. Rows reference payloads by content hash (Global
    Constraint 21); a reference that does not resolve is reported loudly
    with the offending session/seq/type (Step 2). Existence is checked
    with cas/exists? (the CAS itself fails loudly on a verifying read if
    a present body is later consumed and corrupted).

  * :invalid-event-chains — sessions whose hash chain fails
    evoclj.store.event/verify-event-chain (component Step 5): a tampered
    or corrupted historical row. Each entry is the verify failure
    (reason, event seq, expected/actual hashes) plus :session/id.

  * :stale-candidates — candidate rows still in a prepared state
    (:materialized, :evaluating, :eligible) when the process died. They
    are reported with their original :state; recovery MUST NOT promote
    them — promotion is exclusively the component compare-and-set path and
    this scan performs no writes at all (Step 3).

  W2 Work recovery (sole durable lifecycle) follows the same discipline — report, not
  fabricate completion. `find-orphaned-works` classifies Works left
  in :queued (submitted, never dispatched), :running or :waiting (dispatched,
  never settled) as crash residue. `recover-works!` acts on it:
  :queued orphans stay :queued for redelivery, and :running/:waiting orphans are
  marked :failed with {:error/type :recovery/orphaned} via CAS. Recovery NEVER
  fabricates :succeeded and is idempotent — re-running on already-terminal rows
  is a no-op. The heritage `commands` compat track (`store/command.clj`,
  `find-orphaned-commands`/`recover-commands!`) was removed in the ExtraModules
  repair: Work is the only lifecycle; the `commands` table remains in old
  databases as inert history (migration 018 backfilled it into `works`).

  Known boundary: the current-generation check verifies that the
  generation's genome_id resolves to an intact CAS artifact (existence
  plus a re-hashing read). Tree-level Genome loading and manifest
  milestone the store enforces the durable half of Invariant 7."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.evolution.invariant :as invariant]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.invariant :as invariant-store]
            [evoclj.store.work :as work-store]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.util Date UUID)))


(def prepared-candidate-states
  "Candidate states that mean materialization or evaluation was in
  flight when the process died: :materialized, :evaluating, :eligible
  (not yet promoted, rejected, or stale). These are reported as stale;
  recovery never promotes them."
  #{:materialized :evaluating :eligible})


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
(defn- persisted-session-id
  "Decode a UUID session id while preserving opaque legacy TEXT ids."
  [raw-id]
  (try (UUID/fromString raw-id)
       (catch IllegalArgumentException _ raw-id)))

;; --- the three normative categories ------------------------------------------


(defn- missing-artifacts
  "Events whose :payload-ref content address does not resolve in the
  CAS: {:session/id uuid-or-string, :event/seq int, :event/type kw,
  :payload-ref string}."
  [store cas]
  (into []
        (keep (fn [row]
                (let [ref (:payload_ref row)]
                  (when (and ref (not (cas/exists? cas ref)))
                    {:session/id (persisted-session-id (:session_id row))
                     :event/seq (:event_seq row)
                     :event/type (keyword (:event_type row))
                     :payload-ref ref}))))
        (sqlite/query store
                      ["SELECT session_id, event_seq, event_type, payload_ref
                        FROM events WHERE payload_ref IS NOT NULL"])))

(defn- invalid-event-chains
  "Sessions whose event hash chain fails verification; each entry is the
  verify-event-chain failure map plus :session/id.

  The sessions table stores IDs as TEXT for compatibility with pre-UUID
  stores and fixtures. The event verifier's public Session contract is
  UUID-only, so opaque historical IDs are not fed to it: they are
  outside the strict UUID chain-verification domain, but must not make a
  read-only startup scan throw."
  [store]
  (into []
        (keep (fn [row]
                (let [sid (persisted-session-id (:id row))]
                  (when (instance? UUID sid)
                    (let [v (event/verify-event-chain store sid)]
                      (when-not (:valid? v)
                        (assoc v :session/id sid)))))))
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
            genome-id (:genome_id row)
            bootstrap? (and (= "generation-1" gen-id)
                            (re-matches #"^sha256:7{64}$" (str genome-id)))]
        (try
          (if bootstrap?
            {:status :none :generation/id gen-id :genome/id genome-id :bootstrap? true}
            (if-not (cas/exists? cas genome-id)
              {:status :missing :generation/id gen-id :genome/id genome-id}
              (do (cas/get-bytes (verifying-cas cas) genome-id)
                  {:status :ok :generation/id gen-id :genome/id genome-id})))
          (catch clojure.lang.ExceptionInfo e
            {:status :corrupt :generation/id gen-id :genome/id genome-id
             :reason (:error/type (ex-data e))}))))))

(defn- terminal-activation-findings
  "Return hard findings for terminal activation rows whose terminal evidence is incomplete."
  [store]
  (into []
        (keep (fn [row]
                (let [status (:status row)
                      aid (:id row)
                      disable-rows (sqlite/query store
                                                  ["SELECT * FROM invariant_disables WHERE proposal_id = ?"
                                                   (:proposal_id row)])
                      disabled-events (sqlite/query store
                                                     ["SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'disabled'"
                                                      aid])
                      disabled-outbox (sqlite/query store
                                                     ["SELECT * FROM invariant_outbox WHERE activation_id = ? AND event_type = 'disabled'"
                                                      aid])
                      quarantine-events (sqlite/query store
                                          ["SELECT * FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined'"
                                           aid])
                      quarantine-outbox (sqlite/query store
                                          ["SELECT * FROM invariant_outbox WHERE activation_id = ? AND event_type = 'quarantined'"
                                           aid])
                      outbox-valid? (fn [events outbox event-type payload-ref]
                                      (and (= 1 (count events))
                                           (= 1 (count outbox))
                                           (= (:id (first events)) (:event_id (first outbox)))
                                           (= event-type (:event_type (first outbox)))
                                           (some? payload-ref)
                                           (= payload-ref (:payload_ref (first events)))))
                      missing (case status
                                "disabled"
                                (vec (concat
                                      (when (not= 1 (count disable-rows)) [:disable-decision])
                                      (when-not (and (= 1 (count disable-rows))
                                                     (outbox-valid? disabled-events disabled-outbox "disabled"
                                                                    (:disable_digest (first disable-rows))))
                                        [:disabled-event-outbox])
                                      (when (or (seq quarantine-events) (seq quarantine-outbox)) [:quarantine-conflict])))
                                "quarantined"
                                (vec (concat
                                      (when (seq disable-rows) [:disable-conflict])
                                      (when-not (outbox-valid? quarantine-events quarantine-outbox "quarantined"
                                                              (:activation_digest row))
                                        [:quarantined-event-outbox])
                                      (when (or (seq disabled-events) (seq disabled-outbox)) [:disabled-conflict])))
                                [:unknown-terminal-status])]
                  (when (seq missing)
                    {:table :invariant_activations
                     :activation/id aid
                     :status :terminal-evidence-missing
                     :terminal/status (keyword status)
                     :missing missing}))))
        (sqlite/query store
                       ["SELECT * FROM invariant_activations WHERE status IN ('disabled','quarantined')"])))

;; --- generated-invariant integrity -----------------------------------------

(defn- invariant-integrity
  "Read-only invariant scan with full durable row/CAS authority binding."
  [store cas]
  (let [cas-input cas
        cas (verifying-cas cas-input)
        authority-store {:sqlite store :cas (cas-root cas-input)}
        refs (mapcat (fn [[table column kind]]
                       (mapcat (fn [row]
                                 (let [ref (get row (keyword column))]
                                   (cond
                                     (not (and (string? ref) (re-matches #"^sha256:[0-9a-f]{64}$" ref)))
                                     [{:table table :column column :status :malformed-ref :value (err/sanitize ref) :kind kind}]
                                     (not (cas/exists? cas ref))
                                     [{:table table :column column :artifact/id ref :kind kind :status :missing}]
                                     :else
                                     (try
                                       (cas/get-bytes cas ref)
                                       nil
                                       (catch Exception e
                                         [{:table table :column column :artifact/id ref :kind kind
                                           :status :corrupt :error (err/error-data e)}])))))
                               (sqlite/query store [(str "SELECT " column " FROM " table)])))
                     [["invariant_proposals" "predicate_digest" :predicate]
                      ["invariant_proposals" "proposal_digest" :proposal]
                      ["invariant_runs" "result_ref" :result]
                      ["invariant_runs" "run_digest" :run]
                      ["invariant_decisions" "decision_digest" :decision]
                      ["invariant_disables" "disable_digest" :disable]
                      ["invariant_activations" "activation_digest" :activation]])
        malformed (into [] (keep (fn [row]
                                   (try
                                     (let [p (edn/read-string (:predicate_json row))]
                                       (invariant/validate-predicate! p)
                                       nil)
                                     (catch Exception e
                                       {:table :invariant_proposals :proposal/id (:id row)
                                        :status :malformed :error (err/error-data e)}))))
                    (sqlite/query store ["SELECT id,predicate_json FROM invariant_proposals"]))
        malformed-runs (into [] (keep (fn [row]
                                        (try
                                          (invariant/run (edn/read-string (:run_json row)))
                                          nil
                                          (catch Exception e
                                            {:table :invariant_runs :run/id (:id row)
                                             :status :malformed :error (err/error-data e)}))))
                          (sqlite/query store ["SELECT id,run_json FROM invariant_runs"]))
        authority-mismatches (into [] (concat
          (keep (fn [row]
                  (try
                    (invariant-store/get-proposal authority-store (:id row))
                    nil
                    (catch Throwable e
                      {:table :invariant_proposals :proposal/id (:id row)
                       :status :authority-mismatch :error (err/error-data e)})))
                (sqlite/query store ["SELECT id FROM invariant_proposals"]))
          (keep (fn [row]
                  (try
                    (invariant-store/verify-approved-decision! authority-store row)
                    nil
                    (catch Throwable e
                      {:table :invariant_decisions :decision/id (:id row)
                       :status :authority-mismatch :error (err/error-data e)})))
                (sqlite/query store ["SELECT * FROM invariant_decisions WHERE decision = 'approved'"]))
          (keep (fn [row]
                  (try
                    (invariant-store/verify-activation! authority-store row)
                    nil
                    (catch Throwable e
                      {:table :invariant_activations :activation/id (:id row)
                       :status :authority-mismatch :error (err/error-data e)})))
                (sqlite/query store ["SELECT * FROM invariant_activations WHERE status = 'active'"]))
          (keep (fn [row]
                  (try
                    (invariant-store/verify-disable-row! authority-store row)
                    nil
                    (catch Throwable e
                      {:table :invariant_disables :proposal/id (:proposal_id row)
                       :status :authority-mismatch :error (err/error-data e)})))
                (sqlite/query store ["SELECT * FROM invariant_disables"]))))
        partial (mapv #(assoc % :status :partial)
                      (sqlite/query store
                                    ["SELECT a.id AS activation_id
                                      FROM invariant_activations a
                                      LEFT JOIN invariant_events e ON e.activation_id = a.id AND e.event_type = 'activated'
                                      LEFT JOIN invariant_outbox o ON o.activation_id = a.id AND o.event_id = e.id AND o.event_type = 'activated'
                                      WHERE a.status = 'active' AND (e.id IS NULL OR o.id IS NULL)"]))
         quarantined (mapv #(assoc % :status :quarantined)
                           (sqlite/query store
                                         ["SELECT DISTINCT a.id AS activation_id
                                           FROM invariant_activations a
                                           JOIN invariant_events q ON q.activation_id = a.id AND q.event_type = 'quarantined'"]))
         terminal (terminal-activation-findings store)]
    {:dangling-cas-refs (vec (remove #(= :malformed-ref (:status %)) refs))
     :malformed-refs (vec (filter #(= :malformed-ref (:status %)) refs))
     :malformed (vec (concat malformed malformed-runs))
     :authority-mismatches authority-mismatches
     :partial-activations partial
     :quarantined quarantined
     :terminal-evidence-missing terminal}))

(declare ambiguous-provider-effects)
(defn scan-recovery-state
  "The normative recovery scan (component interface). Read-only: it
  classifies crash residue and reports corruption; it never appends,
  rewrites, promotes, or otherwise mutates durable state.

  Returns the historical categories plus :invariant-state."
  [store cas]
  (let [inv (try (invariant-integrity store cas)
                 (catch java.sql.SQLException _
                    {:dangling-cas-refs [] :malformed [] :partial-activations []
                     :quarantined [] :terminal-evidence-missing [] :status :unavailable}))]
    {:missing-artifacts (missing-artifacts store cas)
     :invalid-event-chains (invalid-event-chains store)
     :stale-candidates (stale-candidates store)
     :invariant-state inv
     :generated-invariants inv
     :ambiguous-effects (ambiguous-provider-effects store)}))

(defn- hard-findings
  "The corruption findings strict mode fails closed on: unresolved
  payload references, invalid event chains, and a broken CURRENT
  generation. Stale candidates are recoverable
  crash residue and never count."
  [report]
  (concat (:missing-artifacts report)
          (:invalid-event-chains report)
          (let [inv (:invariant-state report)]
            (concat (when (= :unavailable (:status inv)) [inv])
                    (:dangling-cas-refs inv)
                    (concat (:authority-mismatches inv) (:malformed-refs inv))
                    (:malformed inv)
                    (:partial-activations inv)
                     (:quarantined inv)
                     (:terminal-evidence-missing inv)))
          (when-let [cg (:current-generation report)]
            (when (contains? #{:missing :corrupt :missing-current :ambiguous}
                             (:status cg))
              [cg]))))
(defn startup-integrity-scan
  "Run read-only recovery plus CURRENT and invariant integrity checks.
  Strict mode fails closed on any hard finding; this function never
  auto-activates pending invariant proposals."
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

;; ---------------------------------------------------------------------------
;; W2 Work recovery — idempotent, sole durable lifecycle
;; ---------------------------------------------------------------------------

(def orphan-work-error
  "W2: the recovery marker for a Work left :running/:waiting when the
  process died: crash residue, not a real execution failure. Persisted via
  fail-work! (CAS -> :failed) and carried in the recovery report.
  Succeeded equals execution completed, so running orphans are never
  fabricated as succeeded."
  {:error/type :recovery/orphaned})

(defn find-orphaned-works
  "W2: Works left in a non-terminal in-flight state when the process
  died: :queued, :running, or :waiting. Returns a vector of :work/* maps.
  Terminal states are never orphans. Idempotent classification; never writes."
  [db]
  (work-store/find-orphaned-works db))

(defn- same-provider-call?
  [started other]
  (let [sm (:metadata started)
        om (:metadata other)
        intent-match? (and (:intent/id sm) (:intent/id om)
                           (= (str (:intent/id sm)) (str (:intent/id om))))
        key-match? (and (:idempotency/key sm) (:idempotency/key om)
                        (= (str (:idempotency/key sm))
                           (str (:idempotency/key om))))]
    (and (> (:event/seq other) (:event/seq started))
         (or intent-match? key-match?))))

(defn ambiguous-provider-effects
  "Report provider effects whose started event has no matching result.
  The provider may already have acted, so these effects are ambiguous and
  require manual review; recovery MUST NOT redeliver them blindly."
  [store]
  (reduce
   (fn [acc row]
     (let [sid (persisted-session-id (:id row))]
       (if-not (instance? UUID sid)
         acc
         (let [events (event/events-for-session store sid)
               started (filter #(= :provider/call-started (:event/type %)) events)
               results (filter #(contains? #{:provider/call-completed
                                             :provider/call-ambiguous}
                                             (:event/type %)) events)]
           (into acc
                 (keep (fn [start]
                         (when-not (some #(same-provider-call? start %) results)
                           (let [metadata (:metadata start)]
                             {:session/id sid
                              :event/id (:event/id start)
                              :event/seq (:event/seq start)
                              :event/type :provider/call-started
                              :intent/id (:intent/id metadata)
                              :tool/id (:tool/id metadata)
                              :idempotency/key (:idempotency/key metadata)
                              :outcome :ambiguous
                              :manual-review true})))
                       started))))))
   []
   (sqlite/query store ["SELECT id FROM sessions"])))

(defn record-ambiguous-provider-effects!
  "Durably classify provider effects as ambiguous in the append-only event
  log. The marker is keyed by the started event id and is idempotent under
  concurrent recovery. A marker never authorizes provider re-execution."
  [db effects]
  (let [effects (vec effects)]
    (if (empty? effects)
      []
      (let [pins (into {}
                       (map (fn [effect]
                              [(str (:session/id effect))
                               (session/get-session db (:session/id effect))]))
                       effects)]
        (sqlite/with-write-tx [conn db]
          (mapv (fn [effect]
                  (let [sid (:session/id effect)
                        pin (get pins (str sid))]
                    (when-not (and (instance? UUID sid) pin)
                      (throw (err/error
                              :recovery/ambiguous-effect-unrecordable
                              "cannot record an ambiguous provider effect without a UUID session pin"
                              {:session/id sid
                               :event/id (:event/id effect)})))
                    (let [marker-rows
                          (sqlite/query-raw!
                           conn
                           "SELECT id, payload FROM events WHERE session_id = ? AND event_type = ? ORDER BY event_seq ASC"
                           [(str sid) "provider/call-ambiguous"])
                          existing
                          (some (fn [row]
                                  (let [metadata (try (edn/read-string (:payload row))
                                                      (catch Exception _ {}))]
                                    (when (or (= (str (:provider/call-started-event/id metadata))
                                                 (str (:event/id effect)))
                                              (and (:intent/id metadata)
                                                   (:intent/id effect)
                                                   (= (str (:intent/id metadata))
                                                      (str (:intent/id effect))))
                                              (and (:idempotency/key metadata)
                                                   (:idempotency/key effect)
                                                   (= (str (:idempotency/key metadata))
                                                      (str (:idempotency/key effect)))))
                                      (assoc metadata :event/id (:id row)))))
                                marker-rows)]
                      (or existing
                          (let [tip (first (sqlite/query-raw!
                                            conn
                                            "SELECT id FROM events WHERE session_id = ? ORDER BY event_seq DESC LIMIT 1"
                                            [(str sid)]))]
                            (event/append-event-on-conn!
                             conn
                             {:session/id sid
                              :generation/id (:generation/id pin)
                              :phenotype/id (:phenotype/id pin)
                              :event/type :provider/call-ambiguous
                              :prev/event-id (:id tip)
                              :causal-links #{}
                              :payload-ref nil
                              :metadata {:provider/call-started-event/id (:event/id effect)
                                         :intent/id (:intent/id effect)
                                         :tool/id (:tool/id effect)
                                         :idempotency/key (:idempotency/key effect)
                                         :outcome :ambiguous
                                         :manual-review true
                                         :recovery/reason :provider-result-missing}}))))))
                effects))))))

(defn recover-works!
  "W2: Idempotent Work recovery (report, not fabricate :succeeded).
  :queued orphans stay :queued for redelivery; :running/:waiting orphans
  are marked :failed via CAS (fail-work! with :recovery/orphaned).
  Provider calls whose result event was never persisted are first classified
  :ambiguous and marked with :provider/call-ambiguous, preventing blind
  retry of an effect that may already have happened. Re-running on already
  terminal rows and already-marked provider effects is a no-op."
  [db]
  (let [ambiguous (ambiguous-provider-effects db)
        marked (record-ambiguous-provider-effects! db ambiguous)
        report (work-store/recover-works! db)]
    (assoc report
           :ambiguous-effects ambiguous
           :marked-ambiguous-effects marked)))

(def terminal-work-states
  "Work states that close a Work's lifecycle. A Work in any other state
  (:queued, :running, :waiting) after a crash is residue."
  #{:succeeded :failed :cancelled :timed-out})

(defn find-orphaned-subagents
  "Work-only subagent orphan classification: a child Work stuck
  non-terminal (:queued, :running, or :waiting per find-orphaned-works)
  whose parent Work is already terminal. Session rows carry identity only;
  parent/child links come from the Works themselves (:work/parent-work-id),
  with owning sessions resolved from the Work rows.

  Returns a vector of {:parent/session-id uuid :child/session-id uuid
  :parent/work-id uuid :child/work-id uuid :parent/state kw :child/state
  kw} where states are WORK states (:parent/state is always terminal,
  :child/state never is)."
  [db]
  (let [spec (if (string? db) db db)
        orphans (try (find-orphaned-works spec) (catch Exception _ []))]
    (into [] (keep (fn [w]
                     (let [pwid (:work/parent-work-id w)]
                       (when pwid
                         (let [pw (try (work-store/fetch-work spec pwid)
                                       (catch Exception _ nil))]
                           (when (and pw (contains? terminal-work-states (:work/state pw)))
                             {:parent/session-id (:work/session-id pw)
                              :child/session-id (:work/session-id w)
                              :parent/work-id (:work/id pw)
                              :child/work-id (:work/id w)
                              :parent/state (:work/state pw)
                              :child/state (:work/state w)})))))
                   orphans))))

(defn recover-orphaned-subagents!
  "Act on Work-only orphaned children (find-orphaned-subagents): drive
  each orphaned child Work to :cancelled via CAS (a child whose parent
  already settled terminal must never run — structured concurrency)
  and revoke its owning session's capability rows DB-first (UPDATE WHERE
  revoked = 0). Queued orphans are NOT left for replay here (unlike
  recover-works!): their parent is terminal, so redelivery would run an
  orphan. Already-terminal rows are untouched, so re-running is a no-op.
  Post-crash recovery owns no in-memory leases (the crashed process took
  them), so only durable rows are revoked — live registries re-hydrate
  from the DB. Returns {:orphaned-subagents [...] :recovered
  [{:child/work-id _}] :revoked-capabilities [...]}."
  [db]
  (let [spec (if (string? db) db db)
        orphans (find-orphaned-subagents spec)]
    (reduce (fn [report o]
              (let [cwid (:child/work-id o)
                    csid (:child/session-id o)]
                (try (work-store/cancel-work! spec cwid)
                     (catch Exception _ nil))
                (let [revoked (try
                                (let [rows (sqlite/query spec ["SELECT id FROM capabilities WHERE principal_type = 'session' AND principal_id = ? AND revoked = 0"
                                                             (str csid)])
                                      now (str (java.time.Instant/now))]
                                  (doseq [r rows]
                                    (try (sqlite/exec! spec ["UPDATE capabilities SET revoked = 1, revoked_at = ? WHERE id = ? AND revoked = 0"
                                                             now (:id r)])
                                         (catch Exception _ nil)))
                                  (mapv :id rows))
                                (catch Exception _ []))]
                  (-> report
                      (update :recovered conj {:child/work-id cwid})
                      (update :revoked-capabilities into revoked)))))
            {:orphaned-subagents orphans :recovered [] :revoked-capabilities []}
            orphans)))


(defn recover-generated-invariants!
  "Publish only already durable, fully committed invariant activations.
  Pending proposals and approvals are never activated by recovery."
  [store cas]
  (invariant-store/recover-activations! {:sqlite store :cas cas}))