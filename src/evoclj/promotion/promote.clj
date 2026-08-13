(ns evoclj.promotion.promote
  "Task 9.2 — the atomic CURRENT compare-and-set promotion.

  promote! is the ONLY code path that moves the generations CURRENT
  pointer (Global Constraint 15; the pointer itself is written
  exclusively by evoclj.promotion.current/cas-current!). Promotion is
  a DATABASE state transition, not \"copy candidate files over
  production files\": the promoted candidate's Genome becomes a new
  generations row, the parent generation is superseded, and the
  pointer moves — all in ONE SQL transaction.

  THE PROMOTION TRANSACTION (normative order, Task 9.2):

      read candidate/evaluation
      verify immutable eligibility
      read CURRENT
      compare CURRENT == candidate.parent
      insert promotion decision
      mark old active → superseded
      mark new generation → active
      CAS CURRENT pointer
      append promotion event
      COMMIT

  Serialization: the transaction is opened with BEGIN IMMEDIATE
  (evoclj.store.event's proven pattern), so SQLite's write lock is
  taken BEFORE any read. A concurrent promotion from the same parent
  therefore waits for the winner to commit, then reads the moved
  pointer and reports :stale — it can never overwrite CURRENT. The
  task's CAS update (`UPDATE generations SET current = 0 WHERE
  current = 1 AND id = <expected>` — affected rows 0 ⇒ :stale) remains
  as the final in-transaction guard.

  ONE WRITE REORDER (FK-driven, within the same transaction): the
  promotions.to_generation_id foreign key requires the target
  generation row to pre-exist, so the new generation row is inserted
  BEFORE the promotion decision row. Atomicity is identical — every
  write still commits or rolls back together; only the row-insertion
  order inside the transaction differs from the plan's step list.

  FINALIZED-ELIGIBILITY ONLY (Task 9.2 Step 4): the decision is the
  evaluation row's stored :eligibility map, consumed verbatim via
  evoclj.promotion.state/deployment-transition (the Task 9.1 gate).
  promote! NEVER re-computes evaluator judgment from model text — an
  eval_runs row that is not status 'finalized' (Database Invariant 4:
  reruns create new IDs) or whose :eligible? is not exactly true is
  rejected. Missing or non-true eligibility fails closed.

  LINEAGE CHECKS (Database Invariants 5, 7, 8): the evaluation must
  belong to the candidate (Invariant 5: a Promotion references exactly
  one finalized Evaluation); the candidate's parent generation must be
  the generation the caller expects (Invariant 8); and the candidate
  Genome must exist in the CAS and pass an integrity re-hash before
  the new generation activates (Invariant 7).

  STATE VOCABULARY DEVIATION (documented, per Repo Convention 5): the
  Task 5.1 generations.state CHECK admits ('active','retired',
  'rolled-back') — it predates the Task 9.1 machine. The 9.1 machine
  states are mapped at the row boundary exactly like
  evoclj.evolution.candidate maps candidates: :superseded ↔ 'retired'
  (the 9.1 :seed state has no row value — the seed generation row is
  born 'active'). Candidate states follow candidate.clj's documented
  mapping: :evaluated ↔ 'eligible', :promoted ↔ 'promoted', :stale ↔
  'stale'.

  PROMOTION EVENT ANCHORING (deviation from the letter of the
  normative order, reported): evoclj.store.event/append-event! opens
  its OWN BEGIN IMMEDIATE connection — by design, for per-session
  sequence allocation — so it cannot run inside this namespace's
  transaction (a nested BEGIN fails). The :promotion/promoted and
  :promotion/stale events are therefore appended via
  evoclj.store.event AFTER this transaction commits: the event always
  references a COMMITTED promotion (never a dangling event), and the
  append-only log is unchanged. The event is anchored to the operator
  session carried by promotion-system (:event/session-id), which must
  pre-exist pinned to the parent generation with its :session/created
  root event — the host's job, exactly as evoclj.evolution.core
  documents for its own event sink. The event's :generation/id is the
  session's pinned (parent) generation; the metadata carries
  :from/:to, and the promotions row carries the full lineage for
  reconstruction (Task 9.6). The anchor is validated INSIDE the
  transaction so a promotion can never commit without an appendable
  event anchor.

  INTERFACE (normative, Task 9.2):

      (promote! promotion-system
        {:candidate-id C17
         :evaluation-id E91
         :expected-parent-generation G42})
      ;; => {:status :promoted :from G42 :to G43}
      ;; or {:status :stale :current G43a :expected G42}

  promotion-system contract:

      {:store         {:sqlite <db> :cas <CAS root or config>}
       :resolution/id <str>      ; the compiled ResolutionId of the
                                 ; candidate Genome (the new generation's
                                 ; resolution; compilation is the host's
                                 ; job — promote! never compiles)
       :event/session-id <uuid>  ; the operator session anchoring the
                                 ; :promotion/* event (must exist with
                                 ; its :session/created root)
       :failpoint (fn [])}       ; OPTIONAL TEST SEAM: called inside the
                                 ; transaction after the new generation
                                 ; row exists, immediately before the
                                 ; CAS pointer move; a throw rolls back
                                 ; every write

  OUTCOMES:

  - :promoted — pointer moved G42 → G43; the parent generation is
    :superseded ('retired'), the candidate is :promoted, a promotions
    row (decision 'promoted') records the lineage, and the
    :promotion/promoted event is appended.
  - :stale — the candidate's parent generation is no longer CURRENT
    (a sibling won the CAS). No promotions row is recorded (there is
    no :to generation for a non-move; the Task 5.1 promotions
    decision 'stale' is reserved for later host-level rejection
    bookkeeping). The losing candidate is marked :stale (Task 9.1:
    the CAS-loser edge), and the :promotion/stale event is appended.

  Typed errors (Global Constraint 22 — plain serializable data):
  :promotion/invalid (request contract violation), :promotion/system-invalid,
  :promotion/candidate-not-found, :promotion/candidate-state-invalid,
  :promotion/evaluation-not-found, :promotion/evaluation-not-finalized,
  :promotion/evaluation-candidate-mismatch, :promotion/ineligible (from
  evoclj.promotion.state, fail-closed), :promotion/parent-mismatch,
  :promotion/cas-invalid, :store/session-not-found,
  :promotion/event-anchor-missing, plus evoclj.store.cas typed errors
  (:store/cas-missing, :store/cas-corrupt) from the Invariant 7 check."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.state :as state]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; --- boundary validation ------------------------------------------------------

(def PromoteRequestSchema
  "The promote! input contract (closed): the candidate and its
  finalized evaluation, plus the caller's assertion of the parent
  generation the CURRENT pointer must currently hold."
  [:map {:closed true}
   [:candidate-id uuid?]
   [:evaluation-id uuid?]
   [:expected-parent-generation string?]])

(def PromotionSystemSchema
  "The promotion-system contract (closed). :cas is required because
  activation must verify the candidate Genome's integrity (Database
  Invariant 7); :resolution/id is the compiled ResolutionId of the
  candidate Genome (compilation is the host's job); :event/session-id
  anchors the :promotion/* event; :failpoint is the optional test
  seam."
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
                    (str kind " does not satisfy the promotion contract")
                    {:errors (me/humanize expl)})))

(defn- validate-request!
  [request]
  (when-let [expl (m/explain PromoteRequestSchema request)]
    (schema-error! "promote! request" expl))
  request)

(defn- validate-system!
  [system]
  (when-let [expl (m/explain PromotionSystemSchema system)]
    (throw (err/error :promotion/system-invalid
                      "promotion-system does not satisfy the promotion contract"
                      {:errors (me/humanize expl)})))
  system)

;; --- the transaction ----------------------------------------------------------

(defn- raw-exec!
  "Execute a no-parameter SQL statement on `conn` (BEGIN IMMEDIATE,
  COMMIT, ROLLBACK, PRAGMA). Raw JDBC is used because java.jdbc
  auto-manages transactions around every statement — and org.xerial's
  setAutoCommit(false) opens its own deferred transaction — so neither
  can coexist with an explicit BEGIN IMMEDIATE that must hold SQLite's
  write lock before the promotion reads (the same rationale as
  evoclj.store.event)."
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

(defmacro ^:private with-promotion-tx
  "Open a connection, enable FK enforcement and a busy timeout, begin
  an IMMEDIATE write transaction, run body, commit, and roll back on
  any failure. BEGIN IMMEDIATE takes SQLite's write lock UP FRONT, so
  a concurrent promotion from the same parent serializes on the lock
  and reads the winner's committed pointer instead of racing it;
  busy_timeout makes a contended BEGIN IMMEDIATE wait instead of
  failing with SQLITE_BUSY (the evoclj.store.event pattern)."
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

;; --- timestamps and ids -------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  "Canonical ISO-8601 UTC string for a timestamp value (a
  java.util.Date, java.time.Instant, or ISO-8601 string); nil means
  now."
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (err/error :promotion/invalid
                                       "timestamp must be an inst, Instant, or ISO-8601 string"
                                       {:timestamp ts})))]
    (.format timestamp-fmt inst)))

(defn- new-generation-id
  "The stable id of the promoted generation, derived deterministically
  from the Genome/Resolution pair it represents (a generation IS a
  compiled Genome/Resolution pair, 001-init.sql): the same pair always
  yields the same id, and different candidates (different Genome ids)
  always yield different ids."
  [genome-id resolution-id]
  (str "generation-"
       (subs (hash/text-digest (str genome-id "\n" resolution-id)) 7 23)))

;; --- in-transaction reads and verification -------------------------------------

(defn- read-candidate-row!
  "The candidates row for `candidate-id`, or a typed error when
  absent. The row must be in state 'eligible' (the Task 9.1 machine's
  :evaluated — the only state with promotion edges; evaluated-only is
  structural)."
  [conn candidate-id]
  (let [key (str candidate-id)
        row (first (raw-query conn
                              "SELECT * FROM candidates WHERE id = ?"
                              [key]))]
    (when-not row
      (throw (err/error :promotion/candidate-not-found
                        "no candidate with this id"
                        {:candidate/id candidate-id})))
    (when-not (= "eligible" (:state row))
      (throw (err/error :promotion/candidate-state-invalid
                        "only an :evaluated candidate can be promoted"
                        {:candidate/id candidate-id
                         :state (keyword (:state row))})))
    row))

(defn- read-evaluation!
  "The FINALIZED eval_runs row for `evaluation-id`, verified against
  Database Invariants 4 and 5: it must exist, be status 'finalized'
  (a finalized Evaluation is immutable; reruns create new IDs), and
  belong to exactly the candidate being promoted. Returns the row."
  [conn candidate-id evaluation-id]
  (let [key (str evaluation-id)
        row (first (raw-query conn
                              "SELECT * FROM eval_runs WHERE id = ?"
                              [key]))]
    (when-not row
      (throw (err/error :promotion/evaluation-not-found
                        "no evaluation with this id"
                        {:evaluation/id evaluation-id})))
    (when-not (= "finalized" (:status row))
      (throw (err/error :promotion/evaluation-not-finalized
                        "promotion requires a FINALIZED evaluation (Invariant 4)"
                        {:evaluation/id evaluation-id
                         :status (:status row)})))
    (when-not (= (str candidate-id) (:candidate_id row))
      (throw (err/error :promotion/evaluation-candidate-mismatch
                        "the evaluation belongs to a different candidate (Invariant 5)"
                        {:evaluation/id evaluation-id
                         :candidate/id candidate-id
                         :evaluation/candidate-id (:candidate_id row)})))
    row))

(defn- verify-genome-integrity!
  "Database Invariant 7: the new generation's Genome must exist in the
  CAS and pass an integrity check at activation time. Reads the
  candidate Genome body through a VERIFYING CAS (re-hash on read), so
  a missing body (:store/cas-missing) or corrupted body
  (:store/cas-corrupt) fails loudly before any write."
  [cas-config genome-id]
  (let [root (if (map? cas-config) (:root cas-config) cas-config)]
    (cas/get-bytes (cas/->cas root {:verify true}) genome-id))
  nil)

(defn- read-event-anchor!
  "Validate the :promotion/* event anchor INSIDE the transaction (so a
  promotion can never commit without an appendable anchor): the
  operator session must exist and already carry at least one event
  (its :session/created root — non-root events must reference an
  earlier event in the same session). Returns the session row."
  [conn session-id]
  (let [key (str session-id)
        sess (first (raw-query conn
                               "SELECT id, generation_id, phenotype_id FROM sessions WHERE id = ?"
                               [key]))]
    (when-not sess
      (throw (err/error :store/session-not-found
                        "cannot anchor the promotion event to an unknown operator session"
                        {:session/id session-id})))
    (let [newest (first (raw-query conn
                                   "SELECT MAX(id) AS id FROM events WHERE session_id = ?"
                                   [key]))]
      (when (nil? (:id newest))
        (throw (err/error :promotion/event-anchor-missing
                          "the operator session must carry its :session/created root event first"
                          {:session/id session-id})))
      sess)))

;; --- the stale path ------------------------------------------------------------

(defn- record-stale!
  "The CAS-loser path (Task 9.1: :evaluated → :stale, the sibling that
  lost the compare-and-set): mark the candidate :stale via a
  compare-and-set on 'eligible'. No promotions row is recorded — a
  non-move has no :to generation — the outcome is carried by the
  candidate state and the :promotion/stale event."
  [conn candidate-row]
  (let [key (:id candidate-row)
        n (raw-update! conn
                       "UPDATE candidates SET state = 'stale'
                        WHERE id = ? AND state = 'eligible'"
                       [key])]
    (when-not (= 1 n)
      (throw (err/error :promotion/candidate-state-invalid
                        "the losing candidate is not :evaluated anymore"
                        {:candidate/id key})))))

;; --- the promoted path -----------------------------------------------------------

(defn- insert-promotion-row!
  "Record the promotion decision (Database Invariant 5): one row
  referencing exactly this candidate and this finalized evaluation,
  naming the generation pair the pointer moved between."
  [conn candidate-row evaluation-row from-gen to-gen reason ts]
  (raw-update! conn
               "INSERT INTO promotions
                  (id, candidate_id, evaluation_id, from_generation_id,
                   to_generation_id, decision, reason, created_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
               [(str (UUID/randomUUID))
                (:id candidate-row)
                (:id evaluation-row)
                from-gen
                to-gen
                "promoted"
                (pr-str reason)
                ts]))

(defn- mark-candidate-promoted!
  "The Task 9.1 machine edge :evaluated → :promoted at the row
  boundary, via compare-and-set on 'eligible'."
  [conn candidate-row]
  (let [n (raw-update! conn
                       "UPDATE candidates SET state = 'promoted'
                        WHERE id = ? AND state = 'eligible'"
                       [(:id candidate-row)])]
    (when-not (= 1 n)
      (throw (err/error :promotion/candidate-state-invalid
                        "candidate is not :evaluated anymore"
                        {:candidate/id (:id candidate-row)})))))

(defn- supersede-generation!
  "The Task 9.1 machine edge :active → :superseded at the row
  boundary ('retired' in the Task 5.1 vocabulary — documented
  deviation). The current=1 flag is NOT touched here: the CURRENT
  pointer is moved exclusively by current/cas-current!."
  [conn generation-id]
  (let [n (raw-update! conn
                       "UPDATE generations SET state = 'retired'
                        WHERE id = ? AND state = 'active'"
                       [generation-id])]
    (when-not (= 1 n)
      (throw (err/error :promotion/cas-invalid
                        "the parent generation is not :active anymore"
                        {:generation/id generation-id})))))

(defn- insert-new-generation!
  "Insert the promoted generation row — the candidate's Genome paired
  with its compiled Resolution — born :active with current = 0 (the
  pointer is set to 1 by current/cas-current! immediately after).
  Lineage: parent_id links to the superseded generation, genome_id is
  the candidate's content address (Global Constraint 17)."
  [conn candidate-row resolution-id parent-gen new-gen ts]
  (raw-update! conn
               "INSERT INTO generations
                  (id, genome_id, resolution_id, parent_id, state, current, created_at)
                VALUES (?, ?, ?, ?, 'active', 0, ?)"
               [new-gen
                (:genome_id candidate-row)
                resolution-id
                parent-gen
                ts]))

(defn- append-promotion-event!
  "Append the :promotion/promoted or :promotion/stale event through
  evoclj.store.event AFTER the promotion transaction committed (the
  documented deviation — append-event! owns its own BEGIN IMMEDIATE
  transaction). The event is anchored to the operator session: its
  :generation/id and :phenotype/id are the session's pinned values
  (append-event! enforces the match), its :cause is the session's
  newest event (its :session/created root), and the metadata carries
  the move. A failed append is loud — the promotion itself is already
  committed, and the operator must reconcile."
  [db session-key result]
  (let [sess (first (sqlite/query db
                                  ["SELECT generation_id, phenotype_id FROM sessions WHERE id = ?"
                                   session-key]))
        newest (first (sqlite/query db
                                    ["SELECT MAX(id) AS id FROM events WHERE session_id = ?"
                                     session-key]))]
    (when-not sess
      (throw (err/error :store/session-not-found
                        "cannot anchor the promotion event to an unknown operator session"
                        {:session/id session-key})))
    (event/append-event! db
                         {:session/id (types/session-id session-key)
                          :generation/id (:generation_id sess)
                          :phenotype/id (:phenotype_id sess)
                          :event/type (if (= :promoted (:status result))
                                        :promotion/promoted
                                        :promotion/stale)
                          :cause/event-id (:id newest)
                          :payload-ref nil
                          :metadata (if (= :promoted (:status result))
                                      {:from (:from result) :to (:to result)}
                                      {:expected (:expected result)
                                       :current (:current result)})})))

;; --- the public entry point --------------------------------------------------------

(defn promote!
  "Perform the atomic CURRENT compare-and-set promotion (Task 9.2).
  See the namespace docstring for the normative transaction order,
  the promotion-system contract, the outcome contract, and the typed
  error vocabulary."
  [system request]
  (validate-system! system)
  (validate-request! request)
  (let [db (get-in system [:store :sqlite])
        cas-config (get-in system [:store :cas])
        resolution-id (:resolution/id system)
        session-key (str (:event/session-id system))
        candidate-id (:candidate-id request)
        evaluation-id (:evaluation-id request)
        expected-parent (:expected-parent-generation request)
        ts (canonical-timestamp nil)]
    ;; THE PROMOTION TRANSACTION (the normative order; the outcome map
    ;; is returned, or a throw rolls back every write).
    (let [result (with-promotion-tx [conn db]
                   (let [candidate (read-candidate-row! conn candidate-id)
                         evaluation (read-evaluation! conn candidate-id evaluation-id)
                         eligibility (edn/read-string (:eligibility evaluation))
                         _ (state/deployment-transition :evaluated eligibility :promoted)
                         ;; the event anchor must exist before we commit anything
                         _ (read-event-anchor! conn session-key)
                         ;; read CURRENT
                         current-row (current/read-current conn)]
                     (when-not current-row
                       (throw (err/error :promotion/cas-invalid
                                         "no CURRENT generation to promote from"
                                         {})))
                     (let [current-gen (:id current-row)
                           candidate-parent (:parent_generation_id candidate)]
                       (cond
                         ;; compare CURRENT == candidate.parent — the CAS-loser
                         ;; test: the candidate's parent is no longer current
                         (not= current-gen candidate-parent)
                         (do (record-stale! conn candidate)
                             {:status :stale
                              :current current-gen
                              :expected expected-parent})

                         ;; the caller's expectation must agree with the
                         ;; candidate's lineage (Database Invariant 8) — a
                         ;; broken caller fails loudly rather than promoting
                         ;; against a different parent
                         (not= expected-parent candidate-parent)
                         (throw (err/error :promotion/parent-mismatch
                                           "expected parent disagrees with the candidate's lineage"
                                           {:candidate/id candidate-id
                                            :expected-parent expected-parent
                                            :candidate/parent candidate-parent
                                            :current current-gen}))

                         ;; promote: full lineage verification, then the writes
                         :else
                         (let [new-gen (new-generation-id (:genome_id candidate)
                                                          resolution-id)
                               reason {:expected-parent expected-parent
                                       :candidate-state :evaluated
                                       :eligibility eligibility
                                       :to-generation new-gen}]
                           ;; Database Invariant 7: the Genome exists and
                           ;; re-hashes before activation
                           (verify-genome-integrity! cas-config (:genome_id candidate))
                           ;; mark new generation → active FIRST: the
                           ;; promotions.to_generation_id FK requires the
                           ;; target row to pre-exist (documented reorder,
                           ;; same transaction)
                           (insert-new-generation! conn candidate resolution-id
                                                   expected-parent new-gen ts)
                           ;; insert promotion decision
                           (insert-promotion-row! conn candidate evaluation
                                                  expected-parent new-gen reason ts)
                           ;; mark old active → superseded
                           (supersede-generation! conn expected-parent)
                           ;; mark the candidate :promoted
                           (mark-candidate-promoted! conn candidate)
                           ;; test seam: a throw here must roll back every write
                           (when-let [hook (:failpoint system)]
                             (hook))
                           ;; CAS CURRENT pointer — the final in-transaction guard
                           (let [cas-result (current/cas-current! conn
                                                                  expected-parent
                                                                  new-gen)]
                             (when (= :stale cas-result)
                               (throw (err/error :promotion/cas-invalid
                                                 "CURRENT moved underneath the promotion"
                                                 {:expected-parent expected-parent
                                                  :to-generation new-gen})))
                             {:status :promoted :from expected-parent :to new-gen}))))))]
      ;; append the promotion event AFTER the commit (documented
      ;; deviation — see the namespace docstring), then return the
      ;; outcome unchanged
      (append-promotion-event! db session-key result)
      result)))
