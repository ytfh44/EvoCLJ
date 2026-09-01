(ns evoclj.store.event
  "Append-only causal event log (E1).

  Only append + read/verify queries are exposed: this namespace has NO
  update or delete API by design (Database Invariant 10). The only write
  path is append-event!, and the SQL triggers in 001-init.sql reject
  UPDATE/DELETE on the events table outright.

  Redaction (F7): append-event! accepts optional redaction specs and
  applies evoclj.security.redact/redact-event to :metadata BEFORE
  hashing/append; without specs the write path is byte-identical.

  E1 causality split (break compat):
  * `:prev/event-id` — linear predecessor inside the SAME session
    (the log's hash chain, seq is contiguous, prev is nil only for
    the v0 root set #{:session/created}). Validated at append: prev
    must reference the immediate predecessor (seq = new-seq - 1) in the
    same session.
  * `:causal-links` — semantic causality graph edges that MAY cross
    sessions, e.g. child terminal -> parent result. Stored in the
    `causal_links` table (from_event, to_event, type). Validated only
    for existence of the `from` event; cross-session is allowed.
    The old overloaded `:cause/event-id` is retained as a deprecated
    alias for `:prev/event-id` when the latter is absent (same-session
    only), so legacy callers keep working until they migrate.

  Sequence allocation: append-event! allocates the per-session
  monotonic :event/seq inside a single BEGIN IMMEDIATE transaction, so
  concurrent writers serialize on SQLite's write lock.

  Hash chain (Step 5, E1): each event's :event-hash is sha256 over the
  canonical header

      session
      seq
      type (full namespaced keyword string)
      prev (or legacy cause)
      payload-ref
      prev-hash
      created-at

  one field per line in this fixed order, nil rendered as an empty
  line, hashed with the deterministic conventions of
  evoclj.genome.hash (UTF-8 bytes, CRLF/CR normalized to LF,
  \"sha256:<64 hex>\"). :prev-hash links an event to the previous
  event's :event-hash in the same session (nil for the first event).

  Public data contract: the Event shape in
  evoclj.store.event-schema/EventSchema. The first argument of every
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
  chain and are therefore EXEMPT from the prev-reference rule. A root
  event MUST carry a nil :prev/event-id (and empty :causal-links).

  v0 root set: #{:session/created} — the first event of every session,
  with no causal parent by definition. Every other event type causally
  follows and must reference its immediate predecessor."
  #{:session/created})

(def subagent-result-event-type
  "S5: the event type appended to a parent session when a child subagent
  delivers its result. Carries {:child/session-id uuid :result/cas-ref sha256 :result/status :succeeded}
  for success or {:child/session-id uuid :result/status :failed :error {...}} for failure.
  Non-root: MUST carry :prev/event-id = parent's latest event and
  :causal-links #{ {:from <child-terminal-id> :type :subagent/result} }."
  :subagent/result)

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

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
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
  [t]
  (if-let [ns (namespace t)]
    (str ns "/" (name t))
    (name t)))

(defn- canonical-header
  "Deterministic header hashed for :event-hash. E1 uses :prev/event-id
  (fallback to legacy :cause/event-id) in the 4th line."
  [h]
  (str (:session/id h) "\n"
       (:event/seq h) "\n"
       (type->db (:event/type h)) "\n"
       (or (:prev/event-id h) (:cause/event-id h) "") "\n"
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

(defn- ensure-causal-links-table!
  "Idempotent DDL for the causal_links table inside a transaction.
  Called at the start of every append so fresh test DBs that were
  created via direct inserts (bypassing migrate!) still work."
  [^java.sql.Connection conn]
  (try
    (raw-exec! conn "CREATE TABLE IF NOT EXISTS causal_links (from_event_id INTEGER NOT NULL REFERENCES events(id) ON DELETE CASCADE, to_event_id INTEGER NOT NULL REFERENCES events(id) ON DELETE CASCADE, link_type TEXT NOT NULL, created_at TEXT NOT NULL, PRIMARY KEY (from_event_id, to_event_id, link_type)) WITHOUT ROWID")
    (catch Exception _ nil))
  (try (raw-exec! conn "CREATE INDEX IF NOT EXISTS causal_links_from_idx ON causal_links(from_event_id)") (catch Exception _ nil))
  (try (raw-exec! conn "CREATE INDEX IF NOT EXISTS causal_links_to_idx ON causal_links(to_event_id)") (catch Exception _ nil))
  (try (raw-exec! conn "CREATE INDEX IF NOT EXISTS causal_links_type_idx ON causal_links(link_type)") (catch Exception _ nil))
  nil)

(defn- fetch-causal-links
  "Fetch causal links for `event-id` on `conn` (inside txn). Returns a set
  of {:from <id> :type <keyword>}."
  [^java.sql.Connection conn event-id]
  (try
    (let [rows (raw-query conn "SELECT from_event_id, link_type FROM causal_links WHERE to_event_id = ?" [event-id])]
      (set (map (fn [r] {:from (:from_event_id r) :type (keyword (:link_type r))}) rows)))
    (catch Exception _ #{})))

(defn- causal-links-for-rows
  "Batch fetch for read paths outside a txn: `store` is a sqlite spec,
  `rows` are event rows. Returns map id -> set."
  [store rows]
  (if (empty? rows)
    {}
    (try
      (let [ids (mapv :id rows)
            placeholders (clojure.string/join "," (repeat (count ids) "?"))
            q (str "SELECT from_event_id, to_event_id, link_type FROM causal_links WHERE to_event_id IN (" placeholders ")")
            link-rows (sqlite/query store (into [q] ids))]
        (reduce (fn [acc r]
                  (update acc (:to_event_id r) (fnil conj #{}) {:from (:from_event_id r) :type (keyword (:link_type r))}))
                {}
                link-rows))
      (catch Exception _ {}))))

(defn- row->header-map
  [row]
  {:session/id (:session_id row)
   :event/seq (:event_seq row)
   :event/type (keyword (:event_type row))
   :prev/event-id (or (:prev_event_id row) (:cause_event_id row))
   :cause/event-id (or (:prev_event_id row) (:cause_event_id row))
   :payload-ref (:payload_ref row)
   :prev-hash (:prev_hash row)
   :created-at (:created_at row)})

(defn- row->event
  "Convert a DB row into the public Event contract map. `links` is the
  causal-links set for this row (already fetched)."
  ([row] (row->event row #{}))
  ([row links]
   (let [prev-id (or (:prev_event_id row) (:cause_event_id row))]
     {:event/id (:id row)
      :event/seq (:event_seq row)
      :session/id (UUID/fromString (:session_id row))
      :generation/id (:generation_id row)
      :phenotype/id (:phenotype_id row)
      :event/type (keyword (:event_type row))
      :prev/event-id prev-id
      :cause/event-id prev-id
      :causal-links (or links #{})
      :payload-ref (:payload_ref row)
      :prev-hash (:prev_hash row)
      :event-hash (:event_hash row)
      :created-at (Date/from (Instant/parse (:created_at row)))
      :metadata (or (some-> (:payload row) edn/read-string) {})})))

;; --- the single write path ---------------------------------------------------

(defn append-event!
  "Append one event to a session's append-only log inside a single
  transaction and return the persisted event (public Event contract).

  E1 contract: `:prev/event-id` is the linear predecessor in the SAME
  session (nil only for root events #{:session/created}, otherwise the
  immediate predecessor seq = new-seq -1). `:causal-links` is a set of
  {:from <event-id> :type <keyword>} that MAY cross sessions.

  Legacy `:cause/event-id` is accepted as a deprecated alias for prev
  when `:prev/event-id` is absent; new code must use `:prev/event-id`.

  Optional third argument `redaction-specs` (F7): when non-nil,
  evoclj.security.redact/redact-event is applied BEFORE any hash is
  computed, so secrets never reach storage.

  Typed errors: :security/redact-invalid, :store/event-invalid,
  :store/session-not-found, :store/cause-not-found (prev not found),
  :store/cause-session-mismatch (prev must be same session, also
  :store/prev-session-mismatch), :store/cause-not-earlier (prev must be
  earlier, also :store/prev-not-earlier), :store/causal-link-not-found."
  ([store event]
   (append-event! store event nil))
  ([store event redaction-specs]
   ;; normalize deprecated alias before schema validation so legacy callers
   ;; that still use :cause/event-id pass validation for :prev/event-id
   (let [event (cond-> event
                 (and (contains? event :cause/event-id)
                      (not (contains? event :prev/event-id)))
                 (assoc :prev/event-id (:cause/event-id event)))
         event (update event :causal-links #(or % #{}))
         event (update event :metadata #(or % {}))
         event (update event :payload-ref #(or % nil))]
     (es/validate-append-request event)
     (let [event (if (nil? redaction-specs)
                   event
                   (redact/redact-event event redaction-specs))]
       (with-append-tx [conn store]
         (ensure-causal-links-table! conn)
         (let [session-id (types/session-id (:session/id event))
               session-key (str session-id)
               type (:event/type event)
               ;; prev is the linear predecessor; accept deprecated alias
               prev-id (or (:prev/event-id event) (:cause/event-id event))
               causal-links (or (:causal-links event) #{})
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
                   (and root? prev-id)
                   (throw (err/error :store/event-invalid
                                     "root events carry no prev reference"
                                     {:event/type type :prev/event-id prev-id}))
                   (and root? (seq causal-links))
                   (throw (err/error :store/event-invalid
                                     "root events carry no causal-links"
                                     {:event/type type :causal-links causal-links}))
                   (and (not root?) (nil? prev-id))
                   (throw (err/error :store/event-invalid
                                     "non-root events must reference the immediate predecessor in the same session"
                                     {:event/type type}))
                   (not root?)
                   (let [prev-row (first (raw-query conn "SELECT event_seq, session_id FROM events WHERE id = ?"
                                                     [prev-id]))]
                     (when-not prev-row
                       (throw (err/error :store/cause-not-found
                                         "prev references a nonexistent event"
                                         {:event/type type :prev/event-id prev-id})))
                     (when-not (= session-key (:session_id prev-row))
                       (throw (err/error :store/cause-session-mismatch
                                         "prev must reference an event in the same session"
                                         {:event/type type :prev/event-id prev-id
                                          :session/id session-id
                                          :cause/session-id (:session_id prev-row)})))
                     (when-not (< (:event_seq prev-row) new-seq)
                       (throw (err/error :store/cause-not-earlier
                                         "prev must reference an earlier event"
                                         {:event/type type :prev/event-id prev-id
                                          :cause/event-seq (:event_seq prev-row)
                                          :event/seq new-seq})))
                     ;; causal-links: each from must exist (any session)
                     (doseq [{:keys [from type]} causal-links]
                       (when-not (contains? #{:from :type} :from)
                         (throw (err/error :store/event-invalid "causal link missing :from" {:link {:from from :type type}})))
                       (let [src (first (raw-query conn "SELECT id FROM events WHERE id = ?" [from]))]
                         (when-not src
                           (throw (err/error :store/causal-link-not-found
                                             "causal link from references a nonexistent event"
                                             {:event/type type :causal/from from})))
                         (when-not (keyword? type)
                           (throw (err/error :store/event-invalid
                                             "causal link :type must be a keyword"
                                             {:link {:from from :type type}}))))))
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
                       :prev/event-id prev-id
                       :cause/event-id prev-id
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
                            event_type, cause_event_id, prev_event_id, payload_ref, payload,
                            prev_hash, event_hash, created_at)
                         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                        [session-key new-seq (:generation/id event) (:phenotype/id event)
                         (type->db type) prev-id prev-id (:payload-ref event) payload
                         prev-hash ev-hash ts])
           (let [row (first (raw-query conn "SELECT * FROM events
                                            WHERE session_id = ? AND event_seq = ?"
                                       [session-key new-seq]))
                 new-id (:id row)]
             (doseq [{:keys [from type]} causal-links]
               (raw-insert! conn
                            "INSERT OR IGNORE INTO causal_links (from_event_id, to_event_id, link_type, created_at) VALUES (?, ?, ?, ?)"
                            [from new-id (type->db type) ts]))
             (let [links (fetch-causal-links conn new-id)
                   ev (row->event row links)]
               (es/validate-event (assoc ev :cause/event-id (:prev/event-id ev)))
               ev))))))))

;; --- read/verify queries (no update, no delete — by design) -----------------

(defn events-for-session
  "All events of `session-id` in ascending :event/seq order, as a
  vector of public Event maps (never lazy). Each event includes
  :prev/event-id (and deprecated :cause/event-id) and :causal-links set."
  [store session-id]
  (let [rows (sqlite/query store
                           ["SELECT * FROM events WHERE session_id = ? ORDER BY event_seq ASC"
                            (str (types/session-id session-id))])
        link-map (causal-links-for-rows store rows)]
    (mapv (fn [r] (row->event r (get link-map (:id r) #{}))) rows)))

(defn get-event-by-seq
  "The event at `seq` within `session-id`, or nil when absent."
  [store session-id seq]
  (some-> (first (sqlite/query store
                               ["SELECT * FROM events
                                 WHERE session_id = ? AND event_seq = ?"
                                (str (types/session-id session-id)) seq]))
          (as-> row
                (let [links (try
                              (let [m (causal-links-for-rows store [row])]
                                (get m (:id row) #{}))
                              (catch Exception _ #{}))]
                  (row->event row links)))))

(defn get-event-by-id
  "The event with the given global :event/id, or nil when absent."
  [store event-id]
  (some-> (first (sqlite/query store ["SELECT * FROM events WHERE id = ?" event-id]))
          (as-> row
                (let [links (try
                              (let [m (causal-links-for-rows store [row])]
                                (get m (:id row) #{}))
                              (catch Exception _ #{}))]
                  (row->event row links)))))

(defn events-by-type
  "All events of `type` within `session-id`, ascending :event/seq."
  [store session-id type]
  (let [rows (sqlite/query store
                           ["SELECT * FROM events
                            WHERE session_id = ? AND event_type = ?
                            ORDER BY event_seq ASC"
                            (str (types/session-id session-id)) (type->db type)])
        link-map (causal-links-for-rows store rows)]
    (mapv (fn [r] (row->event r (get link-map (:id r) #{}))) rows)))

(defn get-causal-links
  "The causal-links set for `event-id` (from -> this event). Returns #{ {:from <id> :type <keyword>} }."
  [store event-id]
  (try
    (let [rows (sqlite/query store ["SELECT from_event_id, link_type FROM causal_links WHERE to_event_id = ?" event-id])]
      (set (map (fn [r] {:from (:from_event_id r) :type (keyword (:link_type r))}) rows)))
    (catch Exception _ #{})))

(defn causal-links-from
  "All edges where `from-event-id` is the source (outgoing links)."
  [store from-event-id]
  (try
    (let [rows (sqlite/query store ["SELECT from_event_id, to_event_id, link_type FROM causal_links WHERE from_event_id = ?" from-event-id])]
      (set (map (fn [r] {:from (:from_event_id r) :to (:to_event_id r) :type (keyword (:link_type r))}) rows)))
    (catch Exception _ #{})))

(defn verify-event-chain
  "Verify the integrity of a session's event chain (component Step 5).

  Reads every event of `session-id` in :event/seq order and, for each
  one: checks that its stored :prev-hash links to the previous event's
  stored :event-hash (nil for the first event), then re-derives its
  :event-hash from the canonical header of its OWN stored row and
  compares it against the stored :event-hash. Any mismatch — including
  a tampered :event/type, :payload-ref, :prev/event-id, :prev-hash,
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
