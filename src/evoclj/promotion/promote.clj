(ns evoclj.promotion.promote
  "component — the atomic CURRENT compare-and-set promotion.

  promote! is the ONLY code path that moves the generations CURRENT
  pointer (Global Constraint 15; the pointer itself is written
  exclusively by evoclj.promotion.current/cas-current!). Promotion is
  a DATABASE state transition, not \"copy candidate files over
  production files\": the promoted candidate's Genome becomes a new
  generations row, the parent generation is superseded, and the
  pointer moves — all in ONE SQL transaction.

  THE PROMOTION TRANSACTION (normative order, component):

      read candidate/evaluation
      verify immutable eligibility
      read CURRENT
      compare CURRENT == candidate.parent
      insert promotion decision
      mark old active → superseded
      mark new generation → active
      CAS CURRENT pointer
      append promotion event (OUTBOX — same transaction)
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

  FINALIZED-ELIGIBILITY ONLY (component Step 4): the decision is the
  evaluation row's stored :eligibility map, consumed verbatim via
  evoclj.promotion.state/deployment-transition (the component gate).
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
  component generations.state CHECK admits ('active','retired',
  'rolled-back') — it predates the component machine. The 9.1 machine
  states are mapped at the row boundary exactly like
  evoclj.evolution.candidate maps candidates: :superseded ↔ 'retired'
  (the 9.1 :seed state has no row value — the seed generation row is
  born 'active'). Candidate states follow candidate.clj's documented
  mapping: :evaluated ↔ 'eligible', :promoted ↔ 'promoted', :stale ↔
  'stale'.

  PROMOTION EVENT ANCHORING (Fleet P4 outbox — atomic, single transaction):
  evoclj.store.event/append-event! previously opened its OWN BEGIN IMMEDIATE
  connection, so it could not run inside this namespace's transaction (nested
  BEGIN fails) — the gap this fleet closes. The :promotion/promoted and
  :promotion/stale events are NOW appended INSIDE the same BEGIN IMMEDIATE
  transaction that moves CURRENT (outbox pattern). A dedicated helper
  insert-event-in-tx! allocates the per-session seq, verifies cause, computes
  the hash chain (sha256 over canonical header via evoclj.genome.hash), and
  INSERTs the event row on the SAME raw Connection before COMMIT. An outbox
  row (promotion_outbox) FK-links the promotion and event (dispatched=0) in
  the same commit. Either both promotion+CURRENT and event+outbox commit, or
  a throw (including :failpoint) rolls back every write — no dagling promotion
  without event, no event without promotion. The event remains anchored to the
  operator session carried by promotion-system (:event/session-id), which must
  pre-exist pinned to the parent generation with its :session/created root
  event — validated INSIDE the transaction so a promotion can never commit
  without an appendable anchor.

  INTERFACE (normative, component):

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
       :activation-handle (ActivationHandle) ; S5 sealed: activation only via handle, not raw fn
   :failpoint (fn [])}       ; OPTIONAL TEST SEAM: called inside the
                                 ; transaction after the new generation
                                 ; row exists AND after the promotion event
                                 ; has been appended (same txn), immediately
                                 ; before COMMIT; a throw rolls back every
                                 ; write including the event/outbox

  OUTCOMES:

  - :promoted — pointer moved G42 → G43; the parent generation is
    :superseded ('retired'), the candidate is :promoted, a promotions
    row (decision 'promoted') records the lineage, and the
    :promotion/promoted event is appended atomically.
  - :stale — the candidate's parent generation is no longer CURRENT
    (a sibling won the CAS). No promotions row is recorded (there is
    no :to generation for a non-move; the component promotions
    decision 'stale' is reserved for later host-level rejection
    bookkeeping). The losing candidate is marked :stale (component:
    the CAS-loser edge), and the :promotion/stale event is appended
    atomically in the same transaction.

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
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.state :as state]
            [evoclj.promotion.activation :as activation]
            [evoclj.security.sci-recheck :as recheck]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
            (java.time Instant)
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
  anchors the :promotion/* event; optional :candidate/root supplies the
  verified bundle so the SCI recheck scans real program sources rather
  than the canonical Genome index body; :failpoint is the optional test
  seam."
  [:map {:closed true}
   [:store [:map {:closed true}
            [:sqlite any?]
            [:cas any?]]]
   [:resolution/id [:fn types/resolution-id?]]
   [:event/session-id [:fn types/session-id?]]
   [:candidate/root {:optional true} string?]
   [:activation-handle {:optional true} [:fn activation/activation-handle?]]
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

(defn- raw-insert!
  "Execute a parameterized INSERT on `conn` inside the promotion
  transaction; nil parameters bind as SQL NULL."
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

;; --- event helpers (outbox pattern — same-connection insert) -----------------

(defn- type->db
  "The full keyword string stored in the event_type column."
  [t]
  (if-let [ns (namespace t)]
    (str ns "/" (name t))
    (name t)))

(defn- canonical-header
  "Deterministic canonical header an event hash is computed over."
  [h]
  (str (:session/id h) "\n"
       (:event/seq h) "\n"
       (type->db (:event/type h)) "\n"
       (or (:prev/event-id h) "") "\n"
       (or (:payload-ref h) "") "\n"
       (or (:prev-hash h) "") "\n"
       (:created-at h)))

(defn- event-hash
  "sha256:<64 hex> over the canonical header."
  [h]
  (hash/text-digest (canonical-header h)))

(defn- edn-safe-metadata?
  "Metadata must round-trip through pr-str / edn read-string."
  [m]
  (try
    (map? (edn/read-string (pr-str m)))
    (catch Exception _ false)))

(defn- insert-event-in-tx!
  "Append one :promotion/* event INSIDE the caller's open promotion
  transaction (same Connection). Allocates per-session seq as
  MAX(event_seq)+1, validates cause, links prev-hash, computes hash,
  and INSERTs the row. Returns {:event/id <int> :event/seq <int>}.
  Throws typed errors on violation — the promotion transaction rolls
  back."
  [conn session-key event-type metadata ts]
  (let [session-id (types/session-id session-key)
        ;; session must exist and carry phenotype/generation (validated earlier via read-event-anchor! but re-read for event fields)
        sess (first (raw-query conn "SELECT generation_id, phenotype_id FROM sessions WHERE id = ?" [session-key]))
        _ (when-not sess
            (throw (err/error :store/session-not-found
                              "cannot anchor the promotion event to an unknown operator session"
                              {:session/id session-id})))
        generation-id (:generation_id sess)
        phenotype-id (:phenotype_id sess)
        ;; newest event is the cause (the :session/created root or prior promotion event)
        newest (first (raw-query conn "SELECT id, event_seq, event_hash FROM events WHERE session_id = ? ORDER BY event_seq DESC LIMIT 1" [session-key]))
        cause-id (:id newest)
        new-seq (if newest (inc (:event_seq newest)) 1)
        prev-hash (:event_hash newest)
        _ (when (nil? cause-id)
            (throw (err/error :promotion/event-anchor-missing
                              "the operator session must carry its :session/created root event first"
                              {:session/id session-id})))
        header {:session/id session-key
                :event/seq new-seq
                :event/type event-type
                :prev/event-id (str cause-id)
                :payload-ref nil
                :prev-hash prev-hash
                :created-at ts}
        ev-hash (event-hash header)
        _ (when-not (edn-safe-metadata? metadata)
            (throw (err/error :store/event-invalid
                              "metadata must be EDN-safe Clojure data"
                              {:event/type event-type})))]
    (raw-insert! conn
                 "INSERT INTO events
                       (session_id, event_seq, generation_id, phenotype_id,
                        event_type, cause_event_id, payload_ref, payload,
                        prev_hash, event_hash, created_at)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 [session-key new-seq generation-id phenotype-id
                  (type->db event-type) cause-id nil (pr-str metadata)
                  prev-hash ev-hash ts])
    ;; retrieve the inserted row's autoincrement id
    (let [row (first (raw-query conn "SELECT id, event_seq FROM events WHERE session_id = ? AND event_seq = ?" [session-key new-seq]))]
      {:event/id (:id row) :event/seq (:event_seq row) :event/hash ev-hash})))

(defn- insert-outbox-in-tx!
  "Insert the promotion_outbox row linking promotion and event in the
  same transaction. promotion-id may be nil for stale path."
  [conn promotion-id session-key event-id event-type event-seq ts]
  (let [outbox-id (str (UUID/randomUUID))]
    (raw-insert! conn
                 "INSERT INTO promotion_outbox
                    (id, promotion_id, session_id, event_id, event_type, event_seq, created_at, dispatched)
                  VALUES (?, ?, ?, ?, ?, ?, ?, 0)"
                 [outbox-id promotion-id session-key event-id (type->db event-type) event-seq ts])
    outbox-id))

;; --- in-transaction reads and verification -------------------------------------

(defn- read-candidate-row!
  "The candidates row for `candidate-id`, or a typed error when
  absent. The row must be in state 'eligible' (the component machine's
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

(defn- executable-program-source
  "Read one Clojure source file without evaluation and omit its ns
  declaration. The compiler already validates the namespace/import
  policy; the promotion red-light gate should inspect executable forms,
  not benign namespace symbols such as `agent.route`."
  [source]
  (binding [*read-eval* false]
    (with-open [reader (clojure.lang.LineNumberingPushbackReader.
                        (java.io.StringReader. source))]
      (loop [forms []]
        (let [form (read {:eof ::eof} reader)]
          (if (= ::eof form)
            (str/join "\n" (map pr-str forms))
            (recur (if (and (seq? form) (= 'ns (first form)))
                     forms
                     (conj forms form)))))))))

(defn- program-sources-from-bundle
  "Load the candidate bundle at activation time, verify that its
  content-addressed Genome id matches the candidate row, and return the
  actual Clojure program sources for the SCI red-light gate."
  [candidate-root genome-id]
  (let [loaded (load/load-genome candidate-root)]
    (when-not (= genome-id (:genome/id loaded))
      (throw (err/error :promotion/genome-mismatch
                        "candidate bundle does not match its Genome id"
                        {:candidate/root candidate-root
                         :candidate/genome-id genome-id
                         :loaded/genome-id (:genome/id loaded)})))
    (->> (:files loaded)
         (filter (fn [[path _]]
                   (str/ends-with? path ".clj")))
         (sort-by first)
         (mapv (fn [[_ {:keys [bytes]}]]
                 (executable-program-source
                  (String. ^bytes (if (bytes? bytes)
                                    bytes
                                    (byte-array bytes))
                           StandardCharsets/UTF_8)))))))

(defn- verify-genome-integrity!
  "Database Invariant 7: the new generation's Genome must exist in the
  CAS and pass an integrity check at activation time. The canonical
  Genome index remains the CAS identity proof; when `candidate-root` is
  supplied, the bundle is loaded and its actual program source files
  are passed to the SCI recheck gate."
  [cas-config genome-id candidate-root]
  (let [root (if (map? cas-config) (:root cas-config) cas-config)
        bytes (cas/get-bytes (cas/->cas root {:verify true}) genome-id)]
    (if candidate-root
      (program-sources-from-bundle candidate-root genome-id)
      ;; Backward-compatible standalone promotion contract: callers that
      ;; provision a source body directly under the Genome id keep the
      ;; existing source-string behavior.
      (String. ^bytes bytes StandardCharsets/UTF_8))))

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

(defn sci-sandbox-gate
  "Pure red-light gate run BEFORE promotion (Task: promote-gate
  heuristic). The candidate's evolvable SCI program source — a single
  string, or a collection of strings for multi-file genomes — is scanned
  by evoclj.security.sci-recheck/recheck-candidate. ANY hit across ANY
  program fails the gate (fail-closed); only when EVERY program is safe
  does the gate pass.

  Returns {:passed? <bool> :violations [{:pattern :match} ...]},
  where :passed? is true exactly when every program rechecks safe
  ((:safe? r) true for all). Pure function: no IO, no randomness,
  deterministic over the source text."
  [program-source]
  (let [programs (cond
                  (nil? program-source) []
                  (string? program-source) [program-source]
                  (sequential? program-source) (mapv str program-source)
                  :else [program-source])
        violations
        (->> programs
             (mapcat (fn [src]
                       (:violations (recheck/recheck-candidate src))))
             (keep identity)
             vec)]
    {:passed? (empty? violations)
     :violations violations}))

(defn- record-stale!
  "The CAS-loser path (component: :evaluated → :stale, the sibling that
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
  [conn promotion-id candidate-row evaluation-row from-gen to-gen reason ts]
  (raw-update! conn
               "INSERT INTO promotions
                  (id, candidate_id, evaluation_id, from_generation_id,
                   to_generation_id, decision, reason, created_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
               [promotion-id
                (:id candidate-row)
                (:id evaluation-row)
                from-gen
                to-gen
                "promoted"
                (pr-str reason)
                ts]))

(defn- mark-candidate-promoted!
  "The component machine edge :evaluated → :promoted at the row
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
  "The component machine edge :active → :superseded at the row
  boundary ('retired' in the component vocabulary — documented
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

;; --- the public entry point --------------------------------------------------------

(defn promote!
  "Perform the atomic CURRENT compare-and-set promotion (component).
  See the namespace docstring for the normative transaction order,
  the promotion-system contract, the outcome contract, and the typed
  error vocabulary."
  [system request]
  (validate-system! system)
  ;; S5: activation requires sealed handle when provided — arbitrary fn never accepted
  (when (contains? system :activation-handle)
    (activation/assert-activation-handle! (:activation-handle system)))
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
    ;; is returned, or a throw rolls back every write INCLUDING the event/outbox).
    (with-promotion-tx [conn db]
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
            (let [result {:status :stale
                          :current current-gen
                          :expected expected-parent}
                  metadata {:expected expected-parent :current current-gen}
                  ev (insert-event-in-tx! conn session-key :promotion/stale metadata ts)
                  _ (insert-outbox-in-tx! conn nil session-key (:event/id ev) :promotion/stale (:event/seq ev) ts)]
              (record-stale! conn candidate)
              ;; test seam: a throw here must roll back every write INCLUDING the event/outbox
              (when-let [hook (:failpoint system)]
                (hook))
              result)

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
                          :to-generation new-gen}
                  promotion-id (str (UUID/randomUUID))]
              ;; Database Invariant 7: the Genome identity is verified
              ;; against CAS; when the host supplies the candidate bundle,
              ;; the SCI gate scans its actual program source files rather
              ;; than the canonical Genome index.
              (let [source (verify-genome-integrity!
                            cas-config
                            (:genome_id candidate)
                            (:candidate/root system))
                    gate (sci-sandbox-gate source)]
                (when-not (:passed? gate)
                  (throw (err/error :promotion/sci-sandbox-failed
                                    "sci sandbox recheck failed"
                                    {:reason "sci sandbox recheck failed"
                                     :violations (:violations gate)}))))
              ;; mark new generation → active FIRST: the
              ;; promotions.to_generation_id FK requires the
              ;; target row to pre-exist (documented reorder,
              ;; same transaction)
              (insert-new-generation! conn candidate resolution-id
                                      expected-parent new-gen ts)
              ;; insert promotion decision
              (insert-promotion-row! conn promotion-id candidate evaluation
                                     expected-parent new-gen reason ts)
              ;; mark old active → superseded
              (supersede-generation! conn expected-parent)
              ;; mark the candidate :promoted
              (mark-candidate-promoted! conn candidate)
              ;; CAS CURRENT pointer — the final in-transaction guard
              (let [cas-result (current/cas-current! conn
                                                     expected-parent
                                                     new-gen)]
                (when (= :stale cas-result)
                  (throw (err/error :promotion/cas-invalid
                                    "CURRENT moved underneath the promotion"
                                    {:expected-parent expected-parent
                                     :to-generation new-gen})))
                ;; append promotion event + outbox ATOMICALLY in same txn
                (let [metadata {:from expected-parent :to new-gen}
                      ev (insert-event-in-tx! conn session-key :promotion/promoted metadata ts)
                      _ (insert-outbox-in-tx! conn promotion-id session-key (:event/id ev) :promotion/promoted (:event/seq ev) ts)
                      result {:status :promoted :from expected-parent :to new-gen}]
                  ;; test seam: a throw here must roll back every write INCLUDING the event/outbox
                  (when-let [hook (:failpoint system)]
                    (hook))
                  result))))))))
)
