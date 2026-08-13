(ns evoclj.promotion.rollback
  "Task 9.5 — explicit rollback semantics.

  rollback! is SELECTION-ONLY (Global Constraint 18): it changes ONLY
  which generation is chosen for FUTURE sessions and MUST NOT claim to
  reverse already-committed external effects. It moves the CURRENT
  pointer back through the Task 9.2 CAS machinery
  (evoclj.promotion.current/cas-current! — the ONLY code path that
  changes CURRENT, Global Constraint 15), marks the rolled-back
  generation :rolled-back, reactivates the target :active, and appends
  a :promotion/rollback event carrying the reason. Nothing is deleted:
  G43 events, episodes, external-effect receipts, and promotion
  records stay queryable, because this namespace performs no DELETE
  and no compensation — by construction it requires no dispatch,
  provider, capability, or runtime namespace (Step 4). Any
  compensating external action is a SEPARATELY authorized operator/
  agent task, never this code.

  INTERFACE (normative, Task 9.5):

      (rollback! promotion-system
        {:from-generation G43
         :to-generation G42
         :reason :canary-regression})
      ;; => {:status :rolled-back :from G43 :to G42}
      ;; or {:status :stale :current G43a :expected G43}
      ;;    (from-generation is no longer CURRENT — nothing changed)

  promotion-system is the SAME contract Task 9.2's promote! accepts:
  {:store {:sqlite <db> :cas <CAS root or config>}
   :resolution/id <str>       ; part of the shared contract; rollback
                              ; does not consume it (no generation is
                              ; created)
   :event/session-id <uuid>   ; the operator session anchoring the
                              ; :promotion/rollback event (must exist
                              ; with its :session/created root)
   :failpoint (fn [])}        ; OPTIONAL TEST SEAM: called inside the
                              ; transaction after both state changes,
                              ; immediately before the CAS pointer
                              ; move; a throw rolls back every write

  THE ROLLBACK TRANSACTION (normative order, Task 9.5):

      read CURRENT
      compare CURRENT == from-generation   (else return :stale — no write)
      read target generation row           (else :promotion/generation-not-found)
      reject from == to                    (else :promotion/rollback-invalid)
      target must be :superseded ('retired')  (else :promotion/rollback-target-invalid)
      verify target Genome in CAS, re-hash  (Step 3: else :store/cas-missing
                                             / :store/cas-corrupt, no write)
      validate the :promotion/rollback event anchor (session + root)
      mark from-generation :active → :rolled-back   (CAS-guarded)
      mark target-generation :superseded → :active  (CAS-guarded)
      CAS CURRENT pointer back to the target         (current/cas-current!)
      COMMIT
      append :promotion/rollback event (after commit; see the deviation
      documented below, exactly as promote! documents)

  Serialization: the transaction is opened with BEGIN IMMEDIATE (the
  evoclj.store.event pattern, same as promote!), so SQLite's write
  lock is taken BEFORE the CURRENT read. A concurrent promotion or
  rollback therefore waits for this transaction to commit, then reads
  the moved pointer and reports :stale — the pointer can never be
  overwritten.

  STATE MACHINE DEVIATION (documented, per Repo Convention 5): the
  Task 9.1 closed table in evoclj.promotion.state marks :superseded
  terminal (:superseded → #{}) and reserves :active → :rolled-back
  for the rollback direction. Task 9.5's rollback semantic requires
  the inverse reactivation edge :superseded → :active (\"G42 →
  :active\") — the operator re-activation exception. It is applied at
  the ROW boundary here (state 'retired' → 'active') and nowhere
  else; the pure transition tables in evoclj.promotion.state are
  untouched and still describe the evolution lifecycle for callers
  that validate transitions through them. :rolled-back remains
  terminal in both vocabularies: a rolled-back generation is never
  reactivated by this namespace (the target guard accepts only
  'retired').

  SELECTION-ONLY, NO PROMOTION ROW: the rollback is not a candidate
  decision (the request carries no candidate/evaluation id, and the
  promotions.candidate_id/evaluation_id columns are NOT NULL with
  foreign keys), so no promotions row is written. The Task 5.1
  promotions decision value 'rolled-back' stays RESERVED for host-
  level rollback bookkeeping, exactly as promote! documents for
  'stale'. The rollback is carried by the generation states, the
  CURRENT pointer, and the :promotion/rollback event.

  EVENT ANCHORING (deviation from the letter of the normative order,
  reported): evoclj.store.event/append-event! opens its OWN BEGIN
  IMMEDIATE connection — by design, for per-session sequence
  allocation — so it cannot run inside this namespace's transaction (a
  nested BEGIN fails). The :promotion/rollback event is therefore
  appended via evoclj.store.event AFTER the transaction commits, so it
  always references a COMMITTED rollback. The event is anchored to the
  operator session carried by promotion-system (:event/session-id),
  exactly as promote! anchors its events; the anchor is validated
  INSIDE the transaction so a rollback can never commit without an
  appendable anchor.

  INTEGRITY REFUSAL (Step 3): before ANY write, the target
  generation's Genome must exist in the CAS and re-hash to its content
  id on a VERIFYING read — the same technique promote! uses for
  activation (Database Invariant 7) and the recovery/startup scan
  (evoclj.store.recovery) uses for the CURRENT generation. Refusal is
  a typed error (:store/cas-missing / :store/cas-corrupt from
  evoclj.store.cas) and no state change occurs.

  Typed errors (Global Constraint 22 — plain serializable data):
  :promotion/invalid (request contract violation), :promotion/system-invalid,
  :promotion/generation-not-found (target generation absent),
  :promotion/rollback-invalid (from == to), :promotion/rollback-target-invalid
  (target is not :superseded), :promotion/cas-invalid (no CURRENT
  generation, or the pointer moved underneath the rollback),
  :store/session-not-found, :promotion/event-anchor-missing, plus
  evoclj.store.cas typed errors (:store/cas-missing,
  :store/cas-corrupt) from the Step 3 integrity check."
  (:require [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.current :as current]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.util UUID)))

;; --- boundary validation ------------------------------------------------------

(def RollbackRequestSchema
  "The rollback! input contract (closed): the generation pair the
  CURRENT pointer must move between and the machine-readable reason
  (e.g. :canary-regression, :operator-error)."
  [:map {:closed true}
   [:from-generation string?]
   [:to-generation string?]
   [:reason keyword?]])

(def RollbackSystemSchema
  "The promotion-system contract (the SAME closed schema Task 9.2's
  promote! validates — one system object works for both entry
  points). :cas is required because the rollback target's Genome must
  pass the Step 3 integrity check; :resolution/id is part of the
  shared contract but is NOT consumed by rollback (no generation is
  created); :event/session-id anchors the :promotion/rollback event;
  :failpoint is the optional test seam."
  [:map {:closed true}
   [:store [:map {:closed true}
            [:sqlite any?]
            [:cas any?]]]
   [:resolution/id [:fn types/resolution-id?]]
   [:event/session-id [:fn types/session-id?]]
   [:failpoint {:optional true} fn?]])

(defn- schema-error!
  "Throw :promotion/invalid with a humanized Malli explanation."
  [kind expl]
  (throw (err/error :promotion/invalid
                    (str kind " does not satisfy the rollback contract")
                    {:errors (me/humanize expl)})))

(defn- validate-request!
  [request]
  (when-let [expl (m/explain RollbackRequestSchema request)]
    (schema-error! "rollback! request" expl))
  request)

(defn- validate-system!
  [system]
  (when-let [expl (m/explain RollbackSystemSchema system)]
    (throw (err/error :promotion/system-invalid
                      "promotion-system does not satisfy the rollback contract"
                      {:errors (me/humanize expl)})))
  system)

;; --- the transaction ----------------------------------------------------------

(defn- raw-exec!
  "Execute a no-parameter SQL statement on `conn` (BEGIN IMMEDIATE,
  COMMIT, ROLLBACK, PRAGMA). Raw JDBC is used because java.jdbc
  auto-manages transactions around every statement — and org.xerial's
  setAutoCommit(false) opens its own deferred transaction — so neither
  can coexist with an explicit BEGIN IMMEDIATE (the same rationale as
  evoclj.store.event and evoclj.promotion.promote)."
  [^java.sql.Connection conn sql]
  (with-open [stmt (.createStatement conn)]
    (.execute stmt sql)))

(defn- raw-query
  "Run a parameterized SELECT on `conn`, returning rows as a vector of
  keyword-keyed maps."
  [^java.sql.Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (with-open [rs (.executeQuery stmt)]
      (let [md (.getMetaData rs)
            n (.getColumnCount md)
            labels (mapv #(keyword (.getColumnLabel md (inc %))) (range n))]
        (loop [rows []]
          (if (.next rs)
            (recur (conj rows (zipmap labels
                                      (mapv #(.getObject rs (inc %)) (range n)))))
            rows))))))

(defn- raw-update!
  "Execute a parameterized UPDATE on `conn`; returns the affected-row
  count."
  [^java.sql.Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (.executeUpdate stmt)))

(defmacro ^:private with-rollback-tx
  "Open a connection, enable FK enforcement and a busy timeout, begin
  an IMMEDIATE write transaction, run body, commit, and roll back on
  any failure. BEGIN IMMEDIATE takes SQLite's write lock UP FRONT, so
  a concurrent promotion or rollback serializes on the lock and reads
  the winner's committed pointer instead of racing it (the
  evoclj.store.event / promote! pattern)."
  [[conn-binding db] & body]
  `(with-open [~conn-binding (jdbc/get-connection (sqlite/spec ~db))]
     (raw-exec! ~conn-binding "PRAGMA foreign_keys = ON")
     (raw-exec! ~conn-binding "PRAGMA busy_timeout = 10000")
     (try
       (raw-exec! ~conn-binding "BEGIN IMMEDIATE")
       (let [result# (do ~@body)]
         (raw-exec! ~conn-binding "COMMIT")
         result#)
       (catch Throwable t#
         (try (raw-exec! ~conn-binding "ROLLBACK")
              (catch Throwable _# nil))
         (throw t#)))))

;; --- in-transaction preconditions ----------------------------------------------

(defn- read-target-generation!
  "The generations row for the rollback target `to`, or a typed error
  when absent. The target must be :superseded (row state 'retired'):
  a rollback reactivates the generation the from-generation displaced
  — nothing else may be rolled back to (:rolled-back stays terminal,
  and the CURRENT generation is not a target)."
  [conn to]
  (let [row (first (raw-query conn "SELECT * FROM generations WHERE id = ?" [to]))]
    (when-not row
      (throw (err/error :promotion/generation-not-found
                        "no generation with this id to roll back to"
                        {:generation/id to})))
    (when-not (= "retired" (:state row))
      (throw (err/error :promotion/rollback-target-invalid
                        "only a :superseded generation can be reactivated by rollback"
                        {:generation/id to
                         :state (keyword (:state row))})))
    row))

(defn- verify-target-genome-integrity!
  "Step 3 integrity refusal (Database Invariant 7 technique): the
  rollback target's Genome must exist in the CAS and re-hash to its
  content id on a VERIFYING read (cas/->cas with {:verify true}),
  exactly as promote! verifies a candidate Genome at activation and
  the recovery/startup scan verifies the CURRENT generation. A missing
  body throws :store/cas-missing; a body that no longer matches its id
  throws :store/cas-corrupt. Runs BEFORE any write, so a corrupt
  target refuses the rollback with no state change."
  [cas-config genome-id]
  (let [root (if (map? cas-config) (:root cas-config) cas-config)]
    (cas/get-bytes (cas/->cas root {:verify true}) genome-id))
  nil)

(defn- read-event-anchor!
  "Validate the :promotion/rollback event anchor INSIDE the
  transaction (so a rollback can never commit without an appendable
  anchor): the operator session must exist and already carry at least
  one event (its :session/created root — non-root events must
  reference an earlier event in the same session). Returns the session
  row. Identical to promote!'s anchor check."
  [conn session-id]
  (let [key (str session-id)
        sess (first (raw-query conn
                               "SELECT id, generation_id, phenotype_id FROM sessions WHERE id = ?"
                               [key]))]
    (when-not sess
      (throw (err/error :store/session-not-found
                        "cannot anchor the rollback event to an unknown operator session"
                        {:session/id session-id})))
    (let [newest (first (raw-query conn
                                   "SELECT MAX(id) AS id FROM events WHERE session_id = ?"
                                   [key]))]
      (when (nil? (:id newest))
        (throw (err/error :promotion/event-anchor-missing
                          "the operator session must carry its :session/created root event first"
                          {:session/id session-id})))
      sess)))

;; --- the state changes ---------------------------------------------------------

(defn- mark-rolled-back!
  "The Task 9.1 machine edge :active → :rolled-back at the row
  boundary, CAS-guarded: only the CURRENT, :active from-generation
  may be marked rolled back (the pointer itself is moved afterwards by
  current/cas-current!)."
  [conn from]
  (let [n (raw-update! conn
                       "UPDATE generations SET state = 'rolled-back'
                        WHERE id = ? AND state = 'active' AND current = 1"
                       [from])]
    (when-not (= 1 n)
      (throw (err/error :promotion/cas-invalid
                        "the from-generation is not the CURRENT :active generation"
                        {:generation/id from})))))

(defn- reactivate-generation!
  "The Task 9.5 reactivation edge :superseded → :active at the row
  boundary ('retired' → 'active' in the Task 5.1 vocabulary —
  documented deviation from the Task 9.1 closed table; see the
  namespace docstring). The current=1 flag is NOT touched here: the
  CURRENT pointer is moved exclusively by current/cas-current!."
  [conn to]
  (let [n (raw-update! conn
                       "UPDATE generations SET state = 'active'
                        WHERE id = ? AND state = 'retired'"
                       [to])]
    (when-not (= 1 n)
      (throw (err/error :promotion/rollback-target-invalid
                        "the rollback target is not :superseded anymore"
                        {:generation/id to})))))

(defn- append-rollback-event!
  "Append the :promotion/rollback event through evoclj.store.event
  AFTER the rollback transaction committed (the documented deviation —
  append-event! owns its own BEGIN IMMEDIATE transaction). The event
  is anchored to the operator session: its :generation/id and
  :phenotype/id are the session's pinned values (append-event!
  enforces the match), its :cause is the session's newest event, and
  the metadata carries the move and the reason. A failed append is
  loud — the rollback itself is already committed, and the operator
  must reconcile."
  [db session-key result]
  (let [sess (first (sqlite/query db
                                  ["SELECT generation_id, phenotype_id FROM sessions WHERE id = ?"
                                   session-key]))
        newest (first (sqlite/query db
                                    ["SELECT MAX(id) AS id FROM events WHERE session_id = ?"
                                     session-key]))]
    (when-not sess
      (throw (err/error :store/session-not-found
                        "cannot anchor the rollback event to an unknown operator session"
                        {:session/id session-key})))
    (event/append-event! db
                         {:session/id (types/session-id session-key)
                          :generation/id (:generation_id sess)
                          :phenotype/id (:phenotype_id sess)
                          :event/type :promotion/rollback
                          :cause/event-id (:id newest)
                          :payload-ref nil
                          :metadata {:from (:from result)
                                     :to (:to result)
                                     :reason (:reason result)}})))

;; --- the public entry point ------------------------------------------------------

(defn rollback!
  "Perform the explicit selection-only rollback (Task 9.5). See the
  namespace docstring for the normative transaction order, the
  promotion-system contract, the outcome contract, and the typed
  error vocabulary.

  On success, CURRENT is moved back from `from-generation` to
  `to-generation` via evoclj.promotion.current/cas-current! (the ONLY
  code path that changes CURRENT), `from-generation` becomes
  :rolled-back, `to-generation` becomes :active, and a
  :promotion/rollback event carrying the reason is appended. All G43
  events, episodes, external-effect receipts, and promotion records
  remain queryable — nothing is deleted and no compensating external
  action is invoked (Step 4, by construction).

  Returns {:status :rolled-back :from <from> :to <to>}, or
  {:status :stale :current <actual> :expected <from>} when
  `from-generation` is no longer CURRENT — a no-op with no state
  change and no event."
  [system request]
  (validate-system! system)
  (validate-request! request)
  (let [db (get-in system [:store :sqlite])
        cas-config (get-in system [:store :cas])
        session-key (str (:event/session-id system))
        from (:from-generation request)
        to (:to-generation request)
        reason (:reason request)]
    ;; THE ROLLBACK TRANSACTION (the normative order; a throw rolls
    ;; back every write, and the :stale path writes nothing).
    (let [result (with-rollback-tx [conn db]
                   ;; read CURRENT
                   (let [current-row (current/read-current conn)]
                     (when-not current-row
                       (throw (err/error :promotion/cas-invalid
                                         "no CURRENT generation to roll back from"
                                         {})))
                     (let [current-gen (:id current-row)]
                       ;; compare CURRENT == from-generation — a stale
                       ;; rollback is a no-op (no write, no event)
                       (if (not= current-gen from)
                         {:status :stale
                          :current current-gen
                          :expected from}
                         (do
                           ;; reject a self-rollback before the target check
                           (when (= from to)
                             (throw (err/error :promotion/rollback-invalid
                                               "cannot roll back a generation to itself"
                                               {:generation/id from})))
                           ;; the target must exist and be :superseded
                           (let [target (read-target-generation! conn to)]
                             ;; Step 3: the target Genome must exist in the
                             ;; CAS and re-hash — BEFORE any write
                             (verify-target-genome-integrity! cas-config
                                                              (:genome_id target))
                             ;; the event anchor must exist before we commit
                             (read-event-anchor! conn session-key)
                             ;; mark from-generation :active → :rolled-back
                             (mark-rolled-back! conn from)
                             ;; mark target-generation :superseded → :active
                             (reactivate-generation! conn to)
                             ;; test seam: a throw here must roll back every write
                             (when-let [hook (:failpoint system)]
                               (hook))
                             ;; CAS CURRENT pointer back to the target — the
                             ;; final in-transaction guard
                             (let [cas-result (current/cas-current! conn from to)]
                               (when (= :stale cas-result)
                                 (throw (err/error :promotion/cas-invalid
                                                   "CURRENT moved underneath the rollback"
                                                   {:from-generation from
                                                    :to-generation to})))
                               {:status :rolled-back
                                :from from
                                :to to
                                :reason reason})))))))]
      ;; append the :promotion/rollback event AFTER the commit
      ;; (documented deviation — see the namespace docstring), only for
      ;; an actual rollback; a :stale outcome changes nothing and
      ;; records nothing
      (when (= :rolled-back (:status result))
        (append-rollback-event! db session-key result))
      (dissoc result :reason))))
