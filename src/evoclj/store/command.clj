(ns evoclj.store.command
  "Malli schemas for the async Command contract (A1) + durable outbox
  operations (A2).

  `CommandSchema` validates the durable async command row before it is
  written to the `commands` table (migration 012). The shape mirrors
  GC-21 (payloads live in the CAS, rows carry a sha256: reference) and
  the AsyncCommand state machine [W-20..W-24]:

    queued -> {running,failed,cancelled}
    running -> {succeeded,failed,timed-out,cancelled}

  A1 is schema + pure validation helpers only; A2 adds DB operations:

    * `create-command!` — insert one command row, enforcing
      `idempotency_key UNIQUE` -> :store/duplicate-command.
    * `fetch-command` / `list-commands` — read helpers.
    * `create-command-with-event!` — the outbox transaction: command row
      + `:command/submitted` event row inside a single BEGIN IMMEDIATE
      transaction (copies the promotion_outbox same-tx + FK pattern).
      On any failure both rows are rolled back."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

(def ^:private sha256-re #"^sha256:[0-9a-f]{64}$")

(def ^:private allowed-states
  "The six AsyncCommand states (kebab-case keywords)."
  #{:queued :running :succeeded :failed :timed-out :cancelled})

(def CommandState
  "Malli enum for :cmd/state."
  [:enum :queued :running :succeeded :failed :timed-out :cancelled])

(def CommandSchema
  "Closed Malli map for a durable async command.

  Required keys:
    :cmd/id                uuid?
    :cmd/type              keyword?
    :cmd/state             CommandState
    :cmd/idempotency-key   string? (non-empty, unique per DB constraint)
    :cmd/payload-ref       string? sha256: CAS reference (GC-21)
    :cmd/owner-session-id  uuid?
    :cmd/created-at        inst?

  Optional keys:
    :cmd/parent-cmd-id     uuid? (causal parent command)
    :cmd/continuation-edn  any EDN value (stored as TEXT EDN in DB)
    :cmd/deadline          inst? (expiry for timed-out)"
  [:map {:closed true}
   [:cmd/id uuid?]
   [:cmd/type keyword?]
   [:cmd/state CommandState]
   [:cmd/idempotency-key [:and string? [:fn {:error/message "must be non-empty"} #(seq %)]]]
   [:cmd/payload-ref [:and string? [:re sha256-re]]]
   [:cmd/owner-session-id uuid?]
   [:cmd/created-at [:fn {:error/message "must be an inst"} #(inst? %)]]
   [:cmd/parent-cmd-id {:optional true} [:maybe uuid?]]
   [:cmd/continuation-edn {:optional true} :any]
   [:cmd/deadline {:optional true} [:maybe [:fn {:error/message "must be an inst"} #(inst? %)]]]])

(defn command?
  "True when `x` satisfies CommandSchema."
  [x]
  (m/validate CommandSchema x))

(defn command-state?
  "True when `s` is one of the six allowed command states."
  [s]
  (contains? allowed-states s))

(defn validate-command
  "Validate `cmd` against CommandSchema. Returns `cmd` unchanged, or
  throws :store/command-invalid carrying a humanized Malli explanation."
  [cmd]
  (if-let [expl (m/explain CommandSchema cmd)]
    (throw (err/error :store/command-invalid
                      "command does not satisfy the Command contract"
                      {:errors (me/humanize expl)
                       :explain expl}))
    cmd))

(defn explain-command
  "Return a humanized Malli explanation for `cmd`, or nil when valid."
  [cmd]
  (when-let [expl (m/explain CommandSchema cmd)]
    (me/humanize expl)))

;; ---------------------------------------------------------------------------
;; DB mapping helpers (A2)
;; ---------------------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ^String ts)
               :else (throw (err/error :store/command-invalid
                                       "created-at/deadline must be an inst, Instant, or ISO-8601 string"
                                       {:timestamp ts})))]
    (.format timestamp-fmt inst)))

(defn- state->db
  [s]
  (case s
    :timed-out "timed_out"
    (name s)))

(defn- db->state
  [s]
  (case s
    "timed_out" :timed-out
    (keyword s)))

(defn- type->db
  [t]
  (if-let [ns (namespace t)]
    (str ns "/" (name t))
    (name t)))

(defn- db->type
  [s]
  (keyword s))

(defn- row->command
  [row]
  {:cmd/id (UUID/fromString (:id row))
   :cmd/type (db->type (:type row))
   :cmd/state (db->state (:state row))
   :cmd/idempotency-key (:idempotency_key row)
   :cmd/payload-ref (:payload_ref row)
   :cmd/owner-session-id (UUID/fromString (:owner_session_id row))
   :cmd/parent-cmd-id (some-> (:parent_cmd_id row) (UUID/fromString))
   :cmd/continuation-edn (some-> (:continuation_edn row) edn/read-string)
   :cmd/deadline (some-> (:deadline row) (Instant/parse) (Date/from))
   :cmd/created-at (Date/from (Instant/parse (:created_at row)))})

(defn- normalize-command
  "Accept the Assignment's plain-key map
  {:keys [id type payload-ref idempotency-key owner-session-id parent-cmd-id continuation-edn deadline state created-at]}
  as well as the canonical :cmd/* map, normalizing to :cmd/* keys.
  Also tolerates snake_case variants and :cmd/idempotency_key."
  [cmd]
  (let [m (or cmd {})]
    (-> {}
        ;; :cmd/* passthrough first
        (cond-> (:cmd/id m) (assoc :cmd/id (:cmd/id m))
                (:cmd/type m) (assoc :cmd/type (:cmd/type m))
                (:cmd/state m) (assoc :cmd/state (:cmd/state m))
                (:cmd/idempotency-key m) (assoc :cmd/idempotency-key (:cmd/idempotency-key m))
                (:cmd/payload-ref m) (assoc :cmd/payload-ref (:cmd/payload-ref m))
                (:cmd/owner-session-id m) (assoc :cmd/owner-session-id (:cmd/owner-session-id m))
                (:cmd/parent-cmd-id m) (assoc :cmd/parent-cmd-id (:cmd/parent-cmd-id m))
                (:cmd/continuation-edn m) (assoc :cmd/continuation-edn (:cmd/continuation-edn m))
                (:cmd/deadline m) (assoc :cmd/deadline (:cmd/deadline m))
                (:cmd/created-at m) (assoc :cmd/created-at (:cmd/created-at m))
                ;; snake variant of idempotency key
                (:cmd/idempotency_key m) (assoc :cmd/idempotency-key (:cmd/idempotency_key m)))
        ;; plain keys
        (cond-> (:id m) (assoc :cmd/id (:id m))
                (:type m) (assoc :cmd/type (:type m))
                (:state m) (assoc :cmd/state (:state m))
                (:payload-ref m) (assoc :cmd/payload-ref (:payload-ref m))
                (:payload_ref m) (assoc :cmd/payload-ref (:payload_ref m))
                (:payloadRef m) (assoc :cmd/payload-ref (:payloadRef m))
                (:idempotency-key m) (assoc :cmd/idempotency-key (:idempotency-key m))
                (:idempotency_key m) (assoc :cmd/idempotency-key (:idempotency_key m))
                (:owner-session-id m) (assoc :cmd/owner-session-id (:owner-session-id m))
                (:owner_session_id m) (assoc :cmd/owner-session-id (:owner_session_id m))
                (:parent-cmd-id m) (assoc :cmd/parent-cmd-id (:parent-cmd-id m))
                (:parent_cmd_id m) (assoc :cmd/parent-cmd-id (:parent_cmd_id m))
                (:continuation-edn m) (assoc :cmd/continuation-edn (:continuation-edn m))
                (:continuation_edn m) (assoc :cmd/continuation-edn (:continuation_edn m))
                (:deadline m) (assoc :cmd/deadline (:deadline m))
                (:created-at m) (assoc :cmd/created-at (:created-at m))
                (:created_at m) (assoc :cmd/created-at (:created_at m)))
        ;; defaults
        (cond-> (not (contains? m :cmd/id)) (as-> x (if (contains? x :cmd/id) x x))
                (not (:cmd/id m)) identity)
        )))

(defn- with-defaults
  [cmd]
  (cond-> cmd
    (nil? (:cmd/id cmd)) (assoc :cmd/id (random-uuid))
    (nil? (:cmd/state cmd)) (assoc :cmd/state :queued)
    (nil? (:cmd/created-at cmd)) (assoc :cmd/created-at (Date.))))

(defn- duplicate-key?
  [^Throwable e]
  (when e
    (let [msg (or (.getMessage e) "")
          cause (.getCause e)
          cmsg (when cause (or (.getMessage cause) ""))]
      (or (str/includes? msg "UNIQUE constraint failed")
          (str/includes? cmsg "UNIQUE constraint failed")
          (str/includes? msg "idempotency_key")
          (str/includes? cmsg "idempotency_key")))))

;; ---------------------------------------------------------------------------
;; Public DB ops — simple path (non-outbox)
;; ---------------------------------------------------------------------------

(defn create-command!
  "Insert one command row into `commands`. `db` is a SQLite path string
  or java.jdbc spec. `cmd` may use :cmd/* keys or the assignment's plain
  keys {:keys [id type payload-ref idempotency-key owner-session-id parent-cmd-id continuation-edn deadline]}.

  Returns the canonical :cmd/* map as persisted (via fetch). Throws
  :store/duplicate-command when idempotency_key UNIQUE is violated, and
  :store/command-invalid on schema failure. Foreign-key violations
  (unknown owner_session_id) surface as the SQLite FK error wrapped in
  :store/command-invalid or bubble as ExceptionInfo — callers should
  treat non-duplicate failures as retriable infrastructure errors."
  [db cmd]
  (let [norm (-> cmd normalize-command with-defaults)
        _ (validate-command norm)
        id-str (str (:cmd/id norm))
        type-str (type->db (:cmd/type norm))
        state-str (state->db (:cmd/state norm))
        idem (:cmd/idempotency-key norm)
        pref (:cmd/payload-ref norm)
        owner-str (str (:cmd/owner-session-id norm))
        parent-str (some-> (:cmd/parent-cmd-id norm) str)
        cont-str (some-> (:cmd/continuation-edn norm) pr-str)
        deadline-str (some-> (:cmd/deadline norm) canonical-timestamp)
        created-str (canonical-timestamp (:cmd/created-at norm))]
    (try
      (sqlite/with-db [conn db]
        (jdbc/insert! conn :commands
                      {:id id-str
                       :type type-str
                       :state state-str
                       :idempotency_key idem
                       :payload_ref pref
                       :owner_session_id owner-str
                       :parent_cmd_id parent-str
                       :continuation_edn cont-str
                       :deadline deadline-str
                       :created_at created-str}))
      (catch Exception e
        (if (duplicate-key? e)
          (throw (err/error :store/duplicate-command
                            "idempotency_key already exists"
                            {:idempotency-key idem
                             :cause (.getMessage e)}))
          (throw e))))
    norm))

(defn fetch-command
  "Fetch one command by id (UUID or string). Returns the :cmd/* map or nil."
  [db id]
  (let [key (str (if (uuid? id) id (UUID/fromString (str id))))]
    (some-> (first (sqlite/query db ["SELECT * FROM commands WHERE id = ?" key]))
            row->command)))

(defn list-commands
  "List commands with optional filters:
    {:keys [owner-session-id state]} — both optional.
  `owner-session-id` is a UUID/string; `state` is a :cmd/state keyword."
  ([db] (list-commands db {}))
  ([db {:keys [owner-session-id state]}]
   (let [clauses (cond-> []
                   owner-session-id (conj ["owner_session_id = ?" (str (types/session-id owner-session-id))])
                   state (conj ["state = ?" (state->db state)]))
         sql (if (seq clauses)
               (str "SELECT * FROM commands WHERE " (str/join " AND " (map first clauses)) " ORDER BY created_at ASC, id ASC")
               "SELECT * FROM commands ORDER BY created_at ASC, id ASC")
         params (mapv second clauses)]
     (mapv row->command (sqlite/query db (into [sql] params))))))
;; ---------------------------------------------------------------------------
;; A3 — Dispatch state machine helpers (Wolfram [W-20..W-24])
;; ---------------------------------------------------------------------------

(defn fetch-commands-by-state
  "Fetch all commands in `state` (keyword, e.g. :queued). Returns a vector
  of :cmd/* maps. Throws :store/command-invalid when `state` is not one of
  the six allowed states."
  [db state]
  (when-not (contains? allowed-states state)
    (throw (err/error :store/command-invalid
                      (str "unknown command state: " state)
                      {:state state :allowed allowed-states})))
  (list-commands db {:state state}))

(defn dispatch-command!
  "Transition command `id` from :queued -> :running.
  Atomic UPDATE with state guard; fail-closed when not in :queued.
  Returns the updated :cmd/* map on success, or throws
  :store/invalid-transition when the row is not in :queued (including
  not-found and terminal states)."
  [db id]
  (let [id-str (str id)
        res (sqlite/exec! db ["UPDATE commands SET state = ? WHERE id = ? AND state = ?"
                              "running" id-str "queued"])
        cnt (first res)]
    (if (and cnt (pos? cnt))
      (fetch-command db id-str)
      (let [existing (fetch-command db id-str)
            actual (:cmd/state existing)]
        (throw (err/error :store/invalid-transition
                          (str "cannot dispatch command " id-str " from state " (or actual :not-found))
                          {:id id-str :state actual :expected :queued}))))))

(defn succeed-command!
  "Transition command `id` from :running -> :succeeded.
  `result-cas-ref` is an optional sha256: CAS reference to the result
  artifact; when non-nil it is persisted to `result_ref` if the column
  exists (otherwise the state transition still succeeds).
  Atomic UPDATE with state guard; throws :store/invalid-transition when
  not in :running."
  [db id result-cas-ref]
  (let [id-str (str id)
        has-result (some? result-cas-ref)
        result-str (when has-result (str result-cas-ref))
        attempt
        (fn [sql-params]
          (try
            (sqlite/exec! db sql-params)
            (catch Exception e
              (let [msg (or (.getMessage e) "")
                    cause-msg (some-> (.getCause e) .getMessage)]
                (if (or (str/includes? msg "no such column")
                        (str/includes? (or cause-msg "") "no such column"))
                  ::no-column
                  (throw e))))))
        res (if has-result
              (let [r (attempt ["UPDATE commands SET state = ?, result_ref = ? WHERE id = ? AND state = ?"
                                "succeeded" result-str id-str "running"])]
                (if (= r ::no-column)
                  (sqlite/exec! db ["UPDATE commands SET state = ? WHERE id = ? AND state = ?"
                                    "succeeded" id-str "running"])
                  r))
              (sqlite/exec! db ["UPDATE commands SET state = ? WHERE id = ? AND state = ?"
                                "succeeded" id-str "running"]))
        cnt (first res)]
    (if (and cnt (pos? cnt))
      (fetch-command db id-str)
      (let [existing (fetch-command db id-str)
            actual (:cmd/state existing)]
        (throw (err/error :store/invalid-transition
                          (str "cannot succeed command " id-str " from state " (or actual :not-found))
                          {:id id-str :state actual :expected :running}))))))

(defn fail-command!
  "Transition command `id` from :queued or :running -> :failed.
  `error` is an optional error description (string or ex-data); when
  non-nil and the `error` column exists it is persisted, otherwise only
  the state transition is performed.
  Atomic UPDATE with state guard; throws :store/invalid-transition when
  not in :queued or :running (including terminals and not-found)."
  [db id error]
  (let [id-str (str id)
        err-str (when (some? error) (str error))
        has-err (some? err-str)
        attempt
        (fn [sql-params]
          (try
            (sqlite/exec! db sql-params)
            (catch Exception e
              (let [msg (or (.getMessage e) "")
                    cause-msg (some-> (.getCause e) .getMessage)]
                (if (or (str/includes? msg "no such column")
                        (str/includes? (or cause-msg "") "no such column"))
                  ::no-column
                  (throw e))))))
        res (if has-err
              (let [r (attempt ["UPDATE commands SET state = ?, error = ? WHERE id = ? AND (state = ? OR state = ?)"
                                "failed" err-str id-str "queued" "running"])]
                (if (= r ::no-column)
                  (sqlite/exec! db ["UPDATE commands SET state = ? WHERE id = ? AND (state = ? OR state = ?)"
                                    "failed" id-str "queued" "running"])
                  r))
              (sqlite/exec! db ["UPDATE commands SET state = ? WHERE id = ? AND (state = ? OR state = ?)"
                                "failed" id-str "queued" "running"]))
        cnt (first res)]
    (if (and cnt (pos? cnt))
      (fetch-command db id-str)
      (let [existing (fetch-command db id-str)
            actual (:cmd/state existing)]
        (throw (err/error :store/invalid-transition
                          (str "cannot fail command " id-str " from state " (or actual :not-found))
                          {:id id-str :state actual :expected #{:queued :running}}))))))

;; ---------------------------------------------------------------------------
;; A4 — timeout / cancel transitions + deadline helper (Wolfram [W-20..W-24])
;; ---------------------------------------------------------------------------

(defn- inst->ms
  "Return epoch-millis for an inst (Date, Instant, or ISO-8601 string). Nil -> nil."
  [inst]
  (cond
    (nil? inst) nil
    (instance? Date inst) (.getTime ^Date inst)
    (instance? Instant inst) (.toEpochMilli ^Instant inst)
    (string? inst) (.toEpochMilli (Instant/parse ^String inst))
    (inst? inst) (.getTime ^Date (java.util.Date/from (.toInstant ^Instant (Instant/parse (str inst)))))
    :else (throw (err/error :store/command-invalid "deadline/now must be an inst" {:value inst}))))

(defn deadline-passed?
  "True when `command` has a :cmd/deadline strictly before `now`.
  Both `command` deadline and `now` are insts (Date/Instant/ISO string).
  Returns true when deadline < now, false otherwise (including no deadline).
  Used by explicit timeout callers to decide whether timeout is warranted;
  timeout is explicit (not auto) per A4 spec."
  [command now]
  (if-let [dl (:cmd/deadline command)]
    (let [dl-ms (inst->ms dl)
          now-ms (inst->ms now)]
      (boolean (and dl-ms now-ms (< dl-ms now-ms))))
    false))

(defn timeout-command!
  "Transition command `id` from :running -> :timed-out.
  Atomic UPDATE with state guard; fail-closed when not in :running.
  Returns the updated :cmd/* map on success, or throws
  :store/invalid-transition when the row is not in :running (including
  not-found and terminal states, and :queued per SM queued->{running,failed,cancelled})."
  [db id]
  (let [id-str (str id)
        res (sqlite/exec! db ["UPDATE commands SET state = ? WHERE id = ? AND state = ?"
                              "timed_out" id-str "running"])
        cnt (first res)]
    (if (and cnt (pos? cnt))
      (fetch-command db id-str)
      (let [existing (fetch-command db id-str)
            actual (:cmd/state existing)]
        (throw (err/error :store/invalid-transition
                          (str "cannot timeout command " id-str " from state " (or actual :not-found))
                          {:id id-str :state actual :expected :running}))))))

(defn cancel-command!
  "Transition command `id` from :queued or :running -> :cancelled.
  Atomic UPDATE with state guard; throws :store/invalid-transition when
  not in :queued or :running (including terminals and not-found)."
  [db id]
  (let [id-str (str id)
        res (sqlite/exec! db ["UPDATE commands SET state = ? WHERE id = ? AND (state = ? OR state = ?)"
                              "cancelled" id-str "queued" "running"])
        cnt (first res)]
    (if (and cnt (pos? cnt))
      (fetch-command db id-str)
      (let [existing (fetch-command db id-str)
            actual (:cmd/state existing)]
        (throw (err/error :store/invalid-transition
                          (str "cannot cancel command " id-str " from state " (or actual :not-found))
                          {:id id-str :state actual :expected #{:queued :running}}))))))

;; ---------------------------------------------------------------------------
;; Outbox transaction — command + :command/submitted event atomically
;; ---------------------------------------------------------------------------

(defn- raw-exec!
  [^java.sql.Connection conn sql]
  (with-open [stmt (.createStatement conn)]
    (.execute stmt sql)))

(defn- raw-query
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

(defn- raw-insert!
  [^java.sql.Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (.executeUpdate stmt)))

(defmacro ^:private with-command-tx
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
         (try (raw-exec! ~conn-binding "ROLLBACK") (catch Throwable _# nil))
         (throw t#)))))

;; --- event helpers inside the command outbox (same connection) ---------------

(def ^:private root-event-types #{:session/created})

(defn- root-event? [t] (contains? root-event-types t))

(defn- canonical-header
  [h]
  (str (:session/id h) "\n"
       (:event/seq h) "\n"
       (let [t (:event/type h)]
         (if-let [ns (namespace t)] (str ns "/" (name t)) (name t))) "\n"
       (or (:cause/event-id h) "") "\n"
       (or (:payload-ref h) "") "\n"
       (or (:prev-hash h) "") "\n"
       (:created-at h)))

(defn- event-hash
  [h]
  (hash/text-digest (canonical-header h)))

(defn- edn-safe-metadata?
  [m]
  (try
    (map? (edn/read-string (pr-str m)))
    (catch Exception _ false)))

(defn- insert-event-in-tx!
  "Insert a :command/submitted (or caller-supplied) event inside the
  already-open outbox transaction. Returns the persisted event row as a
  public Event map fragment. Throws typed errors on causality/freshness
  violations — the outer transaction rolls back."
  [conn session-key event]
  (let [ev-type (or (:event/type event) (:event-type event) :command/submitted)
        _ (when-not (keyword? ev-type)
            (throw (err/error :store/event-invalid "event type must be a keyword" {:event/type ev-type})))
        root? (root-event? ev-type)
        ;; session must exist; fetch pinned generation/phenotype for event columns
        sess (first (raw-query conn "SELECT generation_id, phenotype_id FROM sessions WHERE id = ?" [session-key]))
        _ (when-not sess
            (throw (err/error :store/session-not-found "cannot append command event to unknown session" {:session/id session-key})))
        generation-id (:generation_id sess)
        phenotype-id (:phenotype_id sess)
        ;; explicit cause from caller, if any
        explicit-cause (or (:cause/event-id event) (:cause_event_id event) (:cause-event-id event))
        ;; if caller supplied a bad cause shape, treat as invalid early
        _ (when (and explicit-cause (not (integer? explicit-cause)))
            (throw (err/error :store/event-invalid "cause/event-id must be an integer event id when supplied" {:cause/event-id explicit-cause})))
        ;; allocate seq
        new-seq (-> (raw-query conn "SELECT COALESCE(MAX(event_seq), 0) + 1 AS event_seq FROM events WHERE session_id = ?" [session-key])
                    first :event_seq)
        ;; resolve cause: use explicit if given, else latest event id
        resolved-cause
        (cond
          explicit-cause explicit-cause
          root? nil
          :else (:id (first (raw-query conn "SELECT id FROM events WHERE session_id = ? ORDER BY event_seq DESC LIMIT 1" [session-key]))))
        _ (cond
            (and root? resolved-cause)
            (throw (err/error :store/event-invalid "root events carry no cause" {:event/type ev-type :cause/event-id resolved-cause}))
            (and (not root?) (nil? resolved-cause))
            (throw (err/error :store/event-invalid "non-root events must reference an earlier event in the same session" {:event/type ev-type}))
            (not root?)
            (let [cause (first (raw-query conn "SELECT event_seq, session_id FROM events WHERE id = ?" [resolved-cause]))]
              (when-not cause
                (throw (err/error :store/cause-not-found "cause references a nonexistent event" {:event/type ev-type :cause/event-id resolved-cause})))
              (when-not (= session-key (:session_id cause))
                (throw (err/error :store/cause-session-mismatch "cause must reference an event in the same session"
                                  {:event/type ev-type :cause/event-id resolved-cause :session/id session-key :cause/session-id (:session_id cause)})))
              (when-not (< (:event_seq cause) new-seq)
                (throw (err/error :store/cause-not-earlier "cause must reference an earlier event"
                                  {:event/type ev-type :cause/event-id resolved-cause :cause/event-seq (:event_seq cause) :event/seq new-seq}))))
            :else nil)
        prev-hash (-> (raw-query conn "SELECT event_hash FROM events WHERE session_id = ? AND event_seq = ?" [session-key (dec new-seq)])
                      first :event_hash)
        ;; created-at for event
        ts (canonical-timestamp (or (:created-at event) (:created_at event)))
        payload-ref (or (:payload-ref event) (:payload_ref event))
        raw-metadata (or (:metadata event) (:payload event) {})
        ;; command outbox: enrich metadata with command id when the outbox synthesized it
        metadata (if (and (map? raw-metadata) (contains? raw-metadata :command/id))
                   raw-metadata
                   raw-metadata)
        _ (when-not (edn-safe-metadata? metadata)
            (throw (err/error :store/event-invalid "metadata must be EDN-safe Clojure data" {:event/type ev-type})))
        payload (pr-str metadata)
        header {:session/id session-key
                :event/seq new-seq
                :event/type ev-type
                :cause/event-id (some-> resolved-cause str)
                :payload-ref payload-ref
                :prev-hash prev-hash
                :created-at ts}
        ev-hash (event-hash header)]
    (raw-insert! conn
                 "INSERT INTO events (session_id, event_seq, generation_id, phenotype_id, event_type, cause_event_id, payload_ref, payload, prev_hash, event_hash, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 [session-key new-seq generation-id phenotype-id
                  (let [t ev-type] (if-let [ns (namespace t)] (str ns "/" (name t)) (name t)))
                  resolved-cause payload-ref payload prev-hash ev-hash ts])
    (let [row (first (raw-query conn "SELECT * FROM events WHERE session_id = ? AND event_seq = ?" [session-key new-seq]))]
      {:event/id (:id row)
       :event/seq (:event_seq row)
       :session/id (UUID/fromString (:session_id row))
       :generation/id (:generation_id row)
       :phenotype/id (:phenotype_id row)
       :event/type (keyword (:event_type row))
       :cause/event-id (:cause_event_id row)
       :payload-ref (:payload_ref row)
       :prev-hash (:prev_hash row)
       :event-hash (:event_hash row)
       :created-at (Date/from (Instant/parse (:created_at row)))
       :metadata (or (some-> (:payload row) edn/read-string) {})})))

(defn create-command-with-event!
  "The outbox transaction: insert `command` into `commands` and append a
  `:command/submitted` event into `events` inside a single
  BEGIN IMMEDIATE transaction (copies the promotion_outbox pattern).

  `db` is a SQLite path/spec; `command` is a command map (plain or
  :cmd/* keys as in `create-command!`); `event` is an optional event
  override map. When `event` is nil or empty a default
  `:command/submitted` event is synthesized whose :metadata carries
  {:command/id <command-id>} and whose cause is the session's latest
  event. A caller-supplied `event` may override :event/type,
  :cause/event-id, :payload-ref, :metadata, :created-at — bad overrides
  (e.g. a nonexistent cause) throw typed errors and roll back the
  transaction.

  Returns {:command <canonical cmd> :event <persisted event>} on success.
  Throws :store/duplicate-command on idempotency_key collision (rolled
  back), and :store/event-invalid / :store/cause-* etc. on event
  validation failure (also rolled back so no command row survives)."
  ([db command] (create-command-with-event! db command nil))
  ([db command event]
   (let [norm (-> command normalize-command with-defaults)
         _ (validate-command norm)
         id-str (str (:cmd/id norm))
         type-str (type->db (:cmd/type norm))
         state-str (state->db (:cmd/state norm))
         idem (:cmd/idempotency-key norm)
         pref (:cmd/payload-ref norm)
         owner-key (str (:cmd/owner-session-id norm))
         parent-str (some-> (:cmd/parent-cmd-id norm) str)
         cont-str (some-> (:cmd/continuation-edn norm) pr-str)
         deadline-str (some-> (:cmd/deadline norm) canonical-timestamp)
         created-str (canonical-timestamp (:cmd/created-at norm))
         ;; synthesize default :command/submitted metadata if caller gave nothing
         event-override
         (cond
           (nil? event) {:event/type :command/submitted :metadata {:command/id id-str}}
           (empty? event) {:event/type :command/submitted :metadata {:command/id id-str}}
           :else (let [ev event
                       has-cmd (contains? (or (:metadata ev) {}) :command/id)]
                   (cond-> ev
                     (not (:event/type ev)) (assoc :event/type :command/submitted)
                     (not (contains? ev :metadata)) (assoc :metadata {:command/id id-str})
                     (and (contains? ev :metadata) (not has-cmd)) (assoc :metadata (assoc (:metadata ev) :command/id id-str)))))]
     (try
       (with-command-tx [conn db]
         (raw-insert! conn
                      "INSERT INTO commands (id, type, state, idempotency_key, payload_ref, owner_session_id, parent_cmd_id, continuation_edn, deadline, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                      [id-str type-str state-str idem pref owner-key parent-str cont-str deadline-str created-str])
         (let [persisted-event (insert-event-in-tx! conn owner-key event-override)]
           {:command norm :event persisted-event}))
       (catch clojure.lang.ExceptionInfo e
         ;; typed errors from validate-command / insert-event-in-tx! propagate unchanged
         (throw e))
       (catch Exception e
         (if (duplicate-key? e)
           (throw (err/error :store/duplicate-command "idempotency_key already exists" {:idempotency-key idem :cause (.getMessage e)}))
           (throw e)))))))
