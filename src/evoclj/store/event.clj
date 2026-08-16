(ns evoclj.store.event
  "Append-only causal event log (Task 5.3).

  Only append + read/verify queries are exposed: this namespace has NO
  update or delete API by design (Database Invariant 10, Task 5.3
  Step 3). The only write path is append-event!, and the SQL triggers
  in 001-init.sql reject UPDATE/DELETE on the events table outright,
  so the log cannot be silently rewritten through the application
  path.

  Redaction (Task A1, foundation F7): append-event! accepts optional
  redaction specs and applies evoclj.security.redact/redact-event to
  :metadata BEFORE hashing/append; without specs the write path is
  byte-identical.

  Causality: every event belongs to exactly one session and carries
  :cause/event-id referencing an EARLIER event in the SAME session.
  The v0 root set — the events exempt from that rule because they open
  a session's causal chain with a nil cause — is exactly
  #{:session/created} (see root-event-types): :session/created is the
  first event of every session and has no causal parent by definition.
  Every non-root event MUST carry a cause; root events MUST NOT.

  Sequence allocation: append-event! allocates the per-session
  monotonic :event/seq inside a single BEGIN IMMEDIATE transaction, so
  concurrent writers serialize on SQLite's write lock and can never
  interleave or duplicate sequences (Database Invariant 3:
  (session_id, event_seq) unique and monotonically allocated).

  Hash chain (Step 5): each event's :event-hash is sha256 over the
  canonical event header

      session
      seq
      type (full namespaced keyword string)
      cause
      payload-ref
      prev-hash
      created-at

  one field per line in this fixed order, nil rendered as an empty
  line, hashed with the deterministic conventions of
  evoclj.genome.hash (UTF-8 bytes, CRLF/CR normalized to LF,
  \"sha256:<64 hex>\"). :prev-hash links an event to the previous
  event's :event-hash in the same session (nil for the first event).
  verify-event-chain re-derives every stored row's hash from its own
  stored header and checks the prev-hash linkage, so tampering with a
  copied historical row (changing :event/type, :payload-ref,
  :prev-hash, :created-at, ... directly in the database) fails
  verification.

  Public data contract: the Event shape in docs/implementation-plan.md
  (evoclj.store.event-schema/EventSchema). The first argument of every
  function is a SQLite db (a path string or a java.jdbc spec), as in
  evoclj.store.sqlite."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.security.redact :as redact]
            [evoclj.store.event-schema :as es]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

(def root-event-types
  "The v0 root event set: event types that open a session's causal
  chain and are therefore EXEMPT from the cause-reference rule. A root
  event MUST carry a nil :cause/event-id.

  v0 root set: #{:session/created} — the first event of every session,
  with no causal parent by definition. Every other event type in the
  Event Taxonomy causally follows earlier events and must reference
  one."
  #{:session/created})

(defn- root-event? [type]
  (contains? root-event-types type))

(defmacro ^:private with-append-tx
  "Open a connection, enable FK enforcement and a busy timeout, begin
  an IMMEDIATE write transaction, run body, commit, and roll back on
  any failure. BEGIN IMMEDIATE takes SQLite's write lock up front, so
  the per-session seq allocation below is serialized against
  concurrent writers; busy_timeout makes a contended append wait
  instead of failing with SQLITE_BUSY."
  [[conn-binding db] & body]
  `(with-open [~conn-binding (jdbc/get-connection (sqlite/spec ~db))]
     (raw-exec! ~conn-binding "PRAGMA foreign_keys = ON")
     ;; busy_timeout makes a contended BEGIN IMMEDIATE wait instead of
     ;; failing with SQLITE_BUSY
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

(defn- raw-exec!
  "Execute a no-parameter SQL statement on `conn` (BEGIN IMMEDIATE,
  COMMIT, ROLLBACK, PRAGMA). Raw JDBC is used inside the append
  transaction because java.jdbc auto-manages transactions around every
  statement — and org.xerial's setAutoCommit(false) opens its own
  deferred transaction — so neither can coexist with an explicit
  BEGIN IMMEDIATE that must hold SQLite's write lock before the seq
  allocation reads."
  [^java.sql.Connection conn sql]
  (with-open [stmt (.createStatement conn)]
    (.execute stmt sql)))

(defn- raw-query
  "Run a parameterized SELECT on `conn` inside the append transaction,
  returning rows as a vector of keyword-keyed maps (column labels as
  keywords, values as returned by the JDBC driver)."
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
  "Execute a parameterized INSERT on `conn` inside the append
  transaction; nil parameters bind as SQL NULL."
  [^java.sql.Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (.executeUpdate stmt)))

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  "Canonical ISO-8601 UTC string for a :created-at value (a
  java.util.Date, a java.time.Instant, or an ISO-8601 string).
  nil means now. The same string is stored in created_at and hashed,
  so verify-event-chain re-derives hashes losslessly."
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (err/error :store/event-invalid
                                       "created-at must be an inst, Instant, or ISO-8601 string"
                                       {:created-at ts})))]
    (.format timestamp-fmt inst)))

(defn- type->db
  "The full keyword string stored in the event_type column:
  namespace + slash + name (e.g. :session/created is stored as
  session/created). clojure.core/name would drop the namespace, so the
  full string is built explicitly and read back with (keyword s)."
  [t]
  (if-let [ns (namespace t)]
    (str ns "/" (name t))
    (name t)))

(defn- canonical-header
  "The deterministic canonical header an event hash is computed over:
  session, seq, type (the FULL namespaced keyword string, e.g.
  intent/completed — the same string stored in the event_type column,
  so a same-leaf type swap across namespaces changes the hash), cause,
  payload-ref, prev-hash, created-at — one field per line in this
  fixed order, nil rendered as an empty line. None of the fields can
  contain a newline, so the encoding is unambiguous."
  [h]
  (str (:session/id h) "\n"
       (:event/seq h) "\n"
       (type->db (:event/type h)) "\n"
       (or (:cause/event-id h) "") "\n"
       (or (:payload-ref h) "") "\n"
       (or (:prev-hash h) "") "\n"
       (:created-at h)))

(defn- event-hash
  "sha256:<64 hex> over the canonical header, using the deterministic
  text conventions of evoclj.genome.hash (UTF-8, LF line endings)."
  [h]
  (hash/text-digest (canonical-header h)))

(defn- edn-safe-metadata?
  "Metadata must round-trip through pr-str / clojure.edn read-string
  so the payload column stays serializable Clojure data (Global
  Constraint 22): functions, Java objects, and open resources are
  rejected rather than silently stored."
  [m]
  (try
    (map? (edn/read-string (pr-str m)))
    (catch Exception _ false)))

(defn- row->header-map
  "The canonical header fields as stored in a DB row. created-at is
  kept as the exact stored string, so re-derivation at verification
  time is lossless."
  [row]
  {:session/id (:session_id row)
   :event/seq (:event_seq row)
   :event/type (keyword (:event_type row))
   :cause/event-id (:cause_event_id row)
   :payload-ref (:payload_ref row)
   :prev-hash (:prev_hash row)
   :created-at (:created_at row)})

(defn- row->event
  "Convert a DB row into the public Event contract map."
  [row]
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
   :metadata (or (some-> (:payload row) edn/read-string) {})})

;; --- the single write path ---------------------------------------------------

(defn append-event!
  "Append one event to a session's append-only log inside a single
  transaction and return the persisted event (public Event contract).

  Optional third argument `redaction-specs` (Task A1, foundation F7):
  when non-nil, evoclj.security.redact/redact-event is applied to the
  event BEFORE any hash is computed or row inserted, so secrets keyed
  or embedded in :metadata never reach persistent storage. Specs are
  validated by redact-event (invalid specs throw
  :security/redact-invalid before any transaction opens). A nil
  `redaction-specs` — the default, via the two-arity call — leaves the
  write path byte-identical to the pre-F7 behavior.

  Inside the transaction: the session must exist and the event's
  :generation/id must match the session's pinned generation; the
  per-session :event/seq is allocated as max(seq)+1; the cause rule is
  enforced (root events carry a nil cause; every other event must
  reference an EARLIER event in the SAME session); the previous
  event's :event-hash is linked as :prev-hash; the :event-hash is
  computed over the canonical header; and the row is inserted. Any
  failure rolls back the whole append, so a failed append leaves no
  row and consumes no sequence number.

  Typed errors: :security/redact-invalid (invalid redaction specs),
  :store/event-invalid (contract or causality violation),
  :store/session-not-found, :store/cause-not-found,
  :store/cause-session-mismatch, :store/cause-not-earlier."
  ([store event]
   (append-event! store event nil))
  ([store event redaction-specs]
   (es/validate-append-request event)
   (let [event (if (nil? redaction-specs)
                 event
                 (redact/redact-event event redaction-specs))]
     (with-append-tx [conn store]
    (let [session-id (types/session-id (:session/id event))
          session-key (str session-id)
          type (:event/type event)
          cause-id (:cause/event-id event)
          root? (root-event? type)
          sess (first (raw-query conn "SELECT generation_id FROM sessions WHERE id = ?"
                                   [session-key]))
          _ (when-not sess
              (throw (err/error :store/session-not-found
                                "cannot append an event to an unknown session"
                                {:session/id session-id})))
          _ (when-not (= (:generation/id event) (:generation_id sess))
              (throw (err/error :store/event-invalid
                                "event generation must match the session's pinned generation"
                                {:event/type type
                                 :event/generation-id (:generation/id event)
                                 :session/generation-id (:generation_id sess)})))
          new-seq (-> (raw-query conn
                                  "SELECT COALESCE(MAX(event_seq), 0) + 1 AS event_seq
                                   FROM events WHERE session_id = ?"
                                  [session-key])
                      first :event_seq)
          _ (cond
              (and root? cause-id)
              (throw (err/error :store/event-invalid
                                "root events carry no cause reference"
                                {:event/type type :cause/event-id cause-id}))
              (and (not root?) (nil? cause-id))
              (throw (err/error :store/event-invalid
                                "non-root events must reference an earlier event in the same session"
                                {:event/type type}))
              (not root?)
              (let [cause (first (raw-query conn "SELECT event_seq, session_id FROM events WHERE id = ?"
                                             [cause-id]))]
                (when-not cause
                  (throw (err/error :store/cause-not-found
                                    "cause references a nonexistent event"
                                    {:event/type type :cause/event-id cause-id})))
                (when-not (= session-key (:session_id cause))
                  (throw (err/error :store/cause-session-mismatch
                                    "cause must reference an event in the same session"
                                    {:event/type type :cause/event-id cause-id
                                     :session/id session-id
                                     :cause/session-id (:session_id cause)})))
                (when-not (< (:event_seq cause) new-seq)
                  (throw (err/error :store/cause-not-earlier
                                    "cause must reference an earlier event"
                                    {:event/type type :cause/event-id cause-id
                                     :cause/event-seq (:event_seq cause)
                                     :event/seq new-seq}))))
              :else nil)
          prev-hash (-> (raw-query conn
                                    "SELECT event_hash FROM events
                                     WHERE session_id = ? AND event_seq = ?"
                                    [session-key (dec new-seq)])
                        first :event_hash)
          ts (canonical-timestamp (:created-at event))
          header {:session/id session-key
                  :event/seq new-seq
                  :event/type type
                  :cause/event-id cause-id
                  :payload-ref (:payload-ref event)
                  :prev-hash prev-hash
                  :created-at ts}
          ev-hash (event-hash header)
          metadata (or (:metadata event) {})
          _ (when-not (edn-safe-metadata? metadata)
              (throw (err/error :store/event-invalid
                                "metadata must be EDN-safe Clojure data"
                                {:event/type type})))
          payload (pr-str metadata)]
      (raw-insert! conn
                   "INSERT INTO events
                       (session_id, event_seq, generation_id, phenotype_id,
                        event_type, cause_event_id, payload_ref, payload,
                        prev_hash, event_hash, created_at)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                   [session-key new-seq (:generation/id event) (:phenotype/id event)
                    (type->db type) cause-id (:payload-ref event) payload
                    prev-hash ev-hash ts])
      (let [row (first (raw-query conn "SELECT * FROM events
                                        WHERE session_id = ? AND event_seq = ?"
                                   [session-key new-seq]))]
        (es/validate-event (row->event row))
        (row->event row)))))))

;; --- read/verify queries (no update, no delete — by design) -----------------

(defn events-for-session
  "All events of `session-id` in ascending :event/seq order, as a
  vector of public Event maps (never lazy)."
  [store session-id]
  (mapv row->event
        (sqlite/query store
                      ["SELECT * FROM events WHERE session_id = ? ORDER BY event_seq ASC"
                       (str (types/session-id session-id))])))

(defn get-event-by-seq
  "The event at `seq` within `session-id`, or nil when absent."
  [store session-id seq]
  (some-> (first (sqlite/query store
                               ["SELECT * FROM events
                                 WHERE session_id = ? AND event_seq = ?"
                                (str (types/session-id session-id)) seq]))
          row->event))

(defn get-event-by-id
  "The event with the given global :event/id, or nil when absent."
  [store event-id]
  (some-> (first (sqlite/query store ["SELECT * FROM events WHERE id = ?" event-id]))
          row->event))

(defn events-by-type
  "All events of `type` within `session-id`, ascending :event/seq."
  [store session-id type]
  (mapv row->event
        (sqlite/query store
                      ["SELECT * FROM events
                        WHERE session_id = ? AND event_type = ?
                        ORDER BY event_seq ASC"
                       (str (types/session-id session-id)) (type->db type)])))

(defn verify-event-chain
  "Verify the integrity of a session's event chain (Task 5.3 Step 5).

  Reads every event of `session-id` in :event/seq order and, for each
  one: checks that its stored :prev-hash links to the previous event's
  stored :event-hash (nil for the first event), then re-derives its
  :event-hash from the canonical header of its OWN stored row and
  compares it against the stored :event-hash. Any mismatch — including
  a tampered :event/type, :payload-ref, :cause/event-id, :prev-hash,
  or :created-at in a copied historical row — fails verification,
  reporting the offending :event/seq and :reason.

  Returns {:valid? true :events n} for an intact chain (an empty
  session is trivially valid), or {:valid? false :reason k
  :event/seq n ...} with :reason :event/hash-mismatch or
  :event/prev-hash-mismatch."
  [store session-id]
  (let [rows (sqlite/query store
                           ["SELECT * FROM events WHERE session_id = ?
                             ORDER BY event_seq ASC"
                            (str (types/session-id session-id))])]
    (loop [rows rows, prev-hash nil, n 0]
      (if-let [row (first rows)]
        (let [expected (event-hash (row->header-map row))
              stored-hash (:event_hash row)]
          (cond
            (not= prev-hash (:prev_hash row))
            {:valid? false
             :reason :event/prev-hash-mismatch
             :event/seq (:event_seq row)
             :expected-prev prev-hash
             :actual-prev (:prev_hash row)}
            (not= expected stored-hash)
            {:valid? false
             :reason :event/hash-mismatch
             :event/seq (:event_seq row)
             :expected expected
             :actual stored-hash}
            :else
            (recur (rest rows) stored-hash (inc n))))
        {:valid? true :events n}))))
