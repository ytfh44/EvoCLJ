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
    The old overloaded `:cause/event-id` is REMOVED; `:prev/event-id`
    is the sole linear-predecessor key.

  Sequence allocation: append-event! allocates the per-session
  monotonic :event/seq inside a single BEGIN IMMEDIATE transaction, so
  concurrent writers serialize on SQLite's write lock.

  Hash chain (audit item 6, strict-commitment header v2): each
  event's :event-hash is sha256 over the canonical header, one field
  per line in this fixed order (nil rendered as an empty line),
  hashed with the deterministic conventions of evoclj.genome.hash
  (UTF-8 bytes, CRLF/CR normalized to LF, sha256 colon 64 hex):

      session               session id string
      seq                   per-session :event/seq
      type                  full namespaced keyword string
      prev                  :prev/event-id (empty for roots)
      payload-ref           content-address payload reference (empty when nil)
      prev-hash             previous event's :event-hash (empty for seq 1)
      created-at            canonical ISO-8601 instant
      generation-id         session-pinned generation
      phenotype-id          session-pinned code-image identity
      metadata-edn          EXACT stored payload EDN string (pr-str bytes)
      causal-links-edn      pr-str of the sorted from/type edge vector (open-close brackets when empty)

  The v2 header is a strict extension of the legacy 7-line header
  (session..created-at): every field the legacy header committed is
  still committed in the same position. Metadata is committed via the
  exact stored payload string — the write path builds the header from
  the same pr-str bytes it INSERTs, and the verifier reads the payload
  column verbatim — so no EDN round-trip ordering issue can split
  writer and verifier. Causal-links are committed as a sorted vector
  of [from-id type-string] pairs, so set order never leaks into the
  digest.

  Old-row strategy (documented dual acceptance, never silent): rows
  written before this change — and rows written by out-of-band writers
  still emitting the legacy header (the promotion outbox in
  evoclj.promotion.promote) — carry a 7-line legacy hash.
  verify-event-chain tries the v2 header first, then the legacy header
  explicitly, and accepts when EITHER matches the stored :event-hash.
  A tampered v2 row cannot fall through to legacy acceptance (the two
  inputs differ in length, so cross-acceptance would require a sha256
  collision). :prev-hash positional linkage is checked before either
  hash comparison and is unchanged.

  Public data contract: the Event shape in
  evoclj.store.event-schema/EventSchema. The first argument of every
  function is a SQLite db (a path string or a java.jdbc spec), as in
  evoclj.store.sqlite."
  (:require [clojure.edn :as edn]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.sci.boundary :as boundary]
            [evoclj.security.redact :as redact]
            [evoclj.store.event-schema :as es]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
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

(defn- invalid-timestamp
  "Build the typed failure for a :created-at that is not a Date,
  Instant, or ISO-8601 string. The shared coercion + ISO formatting
  lives in evoclj.store.sqlite/canonical-timestamp; only the error type
  stays namespace-local."
  [ts]
  (err/error :store/event-invalid
             "created-at must be an inst, Instant, or ISO-8601 string"
             {:created-at ts}))

(defn- type->db
  [t]
  (if-let [ns (namespace t)]
    (str ns "/" (name t))
    (name t)))

(defn- canonical-causal-links
  "Deterministic EDN encoding of a causal-links set for hash
  commitment. Sorted by [from type] so set iteration order never leaks
  into the digest; each edge encodes as a [from-id type-string] pair
  (plain data, no map key-order sensitivity). #{} encodes as \"[]\"."
  [links]
  (pr-str (mapv (fn [{:keys [from type]}] [from (type->db type)])
                (sort-by (juxt :from :type) (or links [])))))

(defn- canonical-header
  "Deterministic v2 header hashed for :event-hash: the legacy 7 lines
  (session, seq, type, prev, payload-ref, prev-hash, created-at) plus
  generation-id, phenotype-id, the exact stored metadata EDN string,
  and the canonical causal-links encoding — one field per line, nil as
  an empty line. See the ns docstring for the exact field order."
  [h]
  (str (:session/id h) "\n"
       (:event/seq h) "\n"
       (type->db (:event/type h)) "\n"
       (or (:prev/event-id h) "") "\n"
       (or (:payload-ref h) "") "\n"
       (or (:prev-hash h) "") "\n"
       (:created-at h) "\n"
       (or (:generation/id h) "") "\n"
       (or (:phenotype/id h) "") "\n"
       (or (:metadata-edn h) "") "\n"
       (or (:causal-links-edn h) "")))

(defn- canonical-header-legacy
  "The pre-strict-commitment 7-line header (session..created-at).
  Accepted by the verifier ONLY for rows whose stored hash was computed
  before the v2 header existed (or by out-of-band writers still on the
  legacy form) — see the ns docstring old-row strategy."
  [h]
  (str (:session/id h) "\n"
       (:event/seq h) "\n"
       (type->db (:event/type h)) "\n"
       (or (:prev/event-id h) "") "\n"
       (or (:payload-ref h) "") "\n"
       (or (:prev-hash h) "") "\n"
       (:created-at h)))

(defn- event-hash
  [h]
  (hash/text-digest (canonical-header h)))

(defn- legacy-event-hash
  [h]
  (hash/text-digest (canonical-header-legacy h)))

(defn- edn-safe-metadata?
  "True when m is a map of plain EDN-safe data (Global Constraint 22).
  Recursive pre-materialization check via evoclj.sci.boundary/edn-safe?:
  lazy seqs, records, functions and other non-data are rejected WITHOUT
  being realized or serialized."
  [m]
  (and (map? m) (boundary/edn-safe? m)))

(defn- ensure-causal-links-table!
  "Idempotent DDL for the causal_links table inside a transaction.
  Called at the start of every append so fresh test DBs that were
  created via direct inserts (bypassing migrate!) still work."
  [^java.sql.Connection conn]
  (try
    (sqlite/exec-raw! conn "CREATE TABLE IF NOT EXISTS causal_links (from_event_id INTEGER NOT NULL REFERENCES events(id) ON DELETE CASCADE, to_event_id INTEGER NOT NULL REFERENCES events(id) ON DELETE CASCADE, link_type TEXT NOT NULL, created_at TEXT NOT NULL, PRIMARY KEY (from_event_id, to_event_id, link_type)) WITHOUT ROWID")
    (catch Exception _ nil))
  (try (sqlite/exec-raw! conn "CREATE INDEX IF NOT EXISTS causal_links_from_idx ON causal_links(from_event_id)") (catch Exception _ nil))
  (try (sqlite/exec-raw! conn "CREATE INDEX IF NOT EXISTS causal_links_to_idx ON causal_links(to_event_id)") (catch Exception _ nil))
  (try (sqlite/exec-raw! conn "CREATE INDEX IF NOT EXISTS causal_links_type_idx ON causal_links(link_type)") (catch Exception _ nil))
  nil)

(defn- fetch-causal-links
  "Fetch causal links for `event-id` on `conn` (inside txn). Returns a set
  of {:from <id> :type <keyword>}."
  [^java.sql.Connection conn event-id]
  (try
    (let [rows (sqlite/query-raw! conn "SELECT from_event_id, link_type FROM causal_links WHERE to_event_id = ?" [event-id])]
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
  "Header fields for hash verification, rebuilt from a stored row.
  `links` is the causal-links set already fetched for this row. The
  metadata commitment is the payload column verbatim — the exact bytes
  the write path stored — so writer and verifier always agree."
  [row links]
  {:session/id (:session_id row)
   :event/seq (:event_seq row)
   :event/type (keyword (:event_type row))
   :prev/event-id (or (:prev_event_id row) (:cause_event_id row))
   :payload-ref (:payload_ref row)
   :prev-hash (:prev_hash row)
   :created-at (:created_at row)
   :generation/id (:generation_id row)
   :phenotype/id (:phenotype_id row)
   :metadata-edn (or (:payload row) "")
   :causal-links-edn (canonical-causal-links links)})

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
      :causal-links (or links #{})
      :payload-ref (:payload_ref row)
      :prev-hash (:prev_hash row)
      :event-hash (:event_hash row)
      :created-at (Date/from (Instant/parse (:created_at row)))
      :metadata (or (some-> (:payload row) edn/read-string) {})})))

;; --- the single write path ---------------------------------------------------

(defn append-event-on-conn!
  "Append one event using an already-open raw java.sql.Connection `conn`
  that the caller holds inside a BEGIN IMMEDIATE write transaction
  (see evoclj.store.sqlite/with-write-tx). The body is exactly the
  single-write-path logic of append-event! — same validation, same
  per-session seq allocation, same hash commitment, same causal-links
  inserts — minus the transaction boundary, so callers can commit the
  event atomically with companion Work/Capability row updates performed
  on the same connection. `event` must already carry defaults
  (:causal-links #{}, :metadata {}, :payload-ref nil) and any redaction;
  use append-event! when no companion update is needed."
  [conn event]
  (ensure-causal-links-table! conn)
  (let [session-id (types/session-id (:session/id event))
        session-key (str session-id)
        type (:event/type event)
        prev-id (:prev/event-id event)
        causal-links (or (:causal-links event) #{})
        root? (root-event? type)
        sess (first (sqlite/query-raw! conn "SELECT generation_id FROM sessions WHERE id = ?"
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
        new-seq (-> (sqlite/query-raw! conn
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
            (let [prev-row (first (sqlite/query-raw! conn "SELECT event_seq, session_id FROM events WHERE id = ?"
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
              ;; STRICT predecessor (audit item 6a): the supplied prev
              ;; must BE the row at (session, new-seq - 1) — fetched
              ;; directly by position, not inferred from the supplied
              ;; id. An earlier-but-not-immediate prev (a fork) is
              ;; rejected even though it is "earlier": the hash chain's
              ;; prev-hash is always taken from the positional
              ;; predecessor, so any other prev would fork the two
              ;; predecessor concepts.
              (let [immediate-prev (first (sqlite/query-raw! conn "SELECT id, event_seq FROM events WHERE session_id = ? AND event_seq = ?"
                                                      [session-key (dec new-seq)]))]
                (when-not (= (:id immediate-prev) prev-id)
                  (throw (err/error :store/prev-not-immediate
                                    "prev must reference the immediate predecessor (seq = new-seq - 1) in the same session"
                                    {:event/type type :prev/event-id prev-id
                                     :event/seq new-seq
                                     :immediate-prev-event-id (:id immediate-prev)
                                     :immediate-prev-event-seq (:event_seq immediate-prev)}))))
              ;; causal-links: each from must exist (any session)
              (doseq [{:keys [from type]} causal-links]
                (when-not (contains? #{:from :type} :from)
                  (throw (err/error :store/event-invalid "causal link missing :from" {:link {:from from :type type}})))
                (let [src (first (sqlite/query-raw! conn "SELECT id FROM events WHERE id = ?" [from]))]
                  (when-not src
                    (throw (err/error :store/causal-link-not-found
                                      "causal link from references a nonexistent event"
                                      {:event/type type :causal/from from})))
                  (when-not (keyword? type)
                    (throw (err/error :store/event-invalid
                                      "causal link :type must be a keyword"
                                      {:link {:from from :type type}}))))))
            :else nil)
        prev-hash (-> (sqlite/query-raw! conn
                                 "SELECT event_hash FROM events
                                  WHERE session_id = ? AND event_seq = ?"
                                 [session-key (dec new-seq)])
                      first :event_hash)
        ts (sqlite/canonical-timestamp (:created-at event) invalid-timestamp)
        metadata (or (:metadata event) {})
        _ (when-not (edn-safe-metadata? metadata)
            (throw (err/error :store/event-invalid
                              "metadata must be EDN-safe Clojure data"
                              {:event/type type})))
        ;; The committed metadata bytes are EXACTLY the stored payload
        ;; string: header and INSERT share this binding, so writer and
        ;; verifier can never disagree on the committed bytes.
        payload (pr-str metadata)
        header {:session/id session-key
                :event/seq new-seq
                :event/type type
                :prev/event-id prev-id
                :payload-ref (:payload-ref event)
                :prev-hash prev-hash
                :created-at ts
                :generation/id (:generation/id event)
                :phenotype/id (:phenotype/id event)
                :metadata-edn payload
                :causal-links-edn (canonical-causal-links causal-links)}
        ev-hash (event-hash header)]
    (sqlite/insert-raw! conn
                 "INSERT INTO events
                    (session_id, event_seq, generation_id, phenotype_id,
                     event_type, cause_event_id, prev_event_id, payload_ref, payload,
                     prev_hash, event_hash, created_at)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                 [session-key new-seq (:generation/id event) (:phenotype/id event)
                  (type->db type) prev-id prev-id (:payload-ref event) payload
                  prev-hash ev-hash ts])
    (let [row (first (sqlite/query-raw! conn "SELECT * FROM events
                                     WHERE session_id = ? AND event_seq = ?"
                                [session-key new-seq]))
          new-id (:id row)]
      (doseq [{:keys [from type]} causal-links]
        (sqlite/insert-raw! conn
                     "INSERT OR IGNORE INTO causal_links (from_event_id, to_event_id, link_type, created_at) VALUES (?, ?, ?, ?)"
                     [from new-id (type->db type) ts]))
      (let [links (fetch-causal-links conn new-id)
            ev (row->event row links)]
        (es/validate-event ev)
        ev))))

(defn append-event!
  "Append one event to a session's append-only log inside a single
  transaction and return the persisted event (public Event contract).

  E1 contract: `:prev/event-id` is the linear predecessor in the SAME
  session (nil only for root events #{:session/created}, otherwise the
  immediate predecessor seq = new-seq -1). `:causal-links` is a set of
  {:from <event-id> :type <keyword>} that MAY cross sessions.

  The legacy `:cause/event-id` alias is REMOVED; callers must send
  `:prev/event-id` directly.

  Optional third argument `redaction-specs` (F7): when non-nil,
  evoclj.security.redact/redact-event is applied BEFORE any hash is
  computed, so secrets never reach storage.

  Typed errors: :security/redact-invalid, :store/event-invalid,
  :store/session-not-found, :store/cause-not-found (prev not found),
  :store/cause-session-mismatch (prev must be same session, also
  :store/prev-session-mismatch), :store/prev-not-immediate (prev must be
  the immediate predecessor at seq = new-seq - 1),
  :store/causal-link-not-found."
  ([store event]
   (append-event! store event nil))
  ([store event redaction-specs]
   (let [event (update event :causal-links #(or % #{}))
         event (update event :metadata #(or % {}))
         event (update event :payload-ref #(or % nil))]
     (es/validate-append-request event)
     (let [event (if (nil? redaction-specs)
                   event
                   (redact/redact-event event redaction-specs))]
      ;; The append runs inside evoclj.store.sqlite/with-write-tx (one
      ;; connection, FK enforcement + 10s busy timeout, BEGIN IMMEDIATE,
      ;; COMMIT on success / ROLLBACK + rethrow on failure). BEGIN
      ;; IMMEDIATE takes SQLite's write lock up front, so the per-session
      ;; seq allocation below is serialized against concurrent writers;
      ;; busy_timeout makes a contended append wait instead of failing
      ;; with SQLITE_BUSY.
      (sqlite/with-write-tx [conn store]
        (append-event-on-conn! conn event))))))

;; --- read/verify queries (no update, no delete — by design) -----------------

(def ^:private latest-event-id-sql
  "The session event-log tip query — the ONE definition of \"latest event
  id for a session\". ORDER BY event_seq DESC LIMIT 1 is served directly
  by the UNIQUE (session_id, event_seq) index (Database Invariant 3), so
  it needs no rowid/sequence correlation argument."
  "SELECT id FROM events WHERE session_id = ? ORDER BY event_seq DESC LIMIT 1")

(defn latest-event-id-on-conn
  "The id of the last event appended to `session-id`'s log (its tip), or
  nil when the session has no events, read on an already-open raw
  java.sql.Connection the caller holds (e.g. inside a write
  transaction) — the read-side counterpart of append-event-on-conn!.

  Relies on the append path's contiguity invariant: append-event-on-conn!
  allocates event_seq as COALESCE(MAX(event_seq), 0) + 1 inside the SAME
  BEGIN IMMEDIATE transaction as the INSERT, so a session's seqs are
  contiguous and monotonic and the row with the greatest event_seq is
  the last one appended."
  [conn session-id]
  (:id (first (sqlite/query-raw! conn latest-event-id-sql
                                [(str (types/session-id session-id))]))))

(defn latest-event-id
  "The id of the last event appended to `session-id`'s log (its tip), or
  nil when the session has no events. Spec-taking counterpart of
  latest-event-id-on-conn (db is a path string or java.jdbc spec); both
  share the one tip query above."
  [store session-id]
  (:id (first (sqlite/query store
                            [latest-event-id-sql
                             (str (types/session-id session-id))]))))

(defn events-for-session
  "All events of `session-id` in ascending :event/seq order, as a
  vector of public Event maps (never lazy). Each event includes
  :prev/event-id and :causal-links set."
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
  "Verify the integrity of a session's event chain (component Step 5,
  strict-commitment header v2).

  Reads every event of `session-id` in :event/seq order and, for each
  one: checks that its stored :prev-hash links to the previous event's
  stored :event-hash (nil for the first event), then re-derives its
  :event-hash from the canonical header of its OWN stored row and
  compares it against the stored :event-hash. The v2 header is tried
  first, then the legacy 7-line header explicitly (documented dual
  acceptance for pre-change rows and legacy out-of-band writers — see
  the ns docstring); EITHER match accepts the row.

  Precise coverage claim. For a v2 row, verification failing means one
  of these stored fields was altered after append: session id, seq,
  type, prev id, payload-ref, prev-hash, created-at, generation id,
  phenotype id, metadata (payload EDN bytes), or causal-links. For a
  legacy row, only the legacy 7 fields (session..created-at) are
  covered. Positional :prev-hash linkage is always checked. NOT
  covered: the autoincrement row id, the sessions-table pin (checked at
  append time, not by the verifier), and causal_links.created_at
  timestamps. \"Valid\" therefore means \"the full semantic record is
  untampered\" for v2 rows, and \"the linear chain is untampered\" for
  legacy rows — never a blanket cross-table audit truth.

  Returns {:valid? true :events n} for an intact chain (an empty
  session is trivially valid), or {:valid? false :reason k
  :event/seq n ...} with :reason :event/hash-mismatch or
  :event/prev-hash-mismatch."
  [store session-id]
  (let [rows (sqlite/query store
                           ["SELECT * FROM events WHERE session_id = ?
                             ORDER BY event_seq ASC"
                            (str (types/session-id session-id))])
        link-map (causal-links-for-rows store rows)]
    (loop [rows rows, prev-hash nil, n 0]
      (if-let [row (first rows)]
        (let [header (row->header-map row (get link-map (:id row) #{}))
              expected (event-hash header)
              legacy-expected (legacy-event-hash header)
              stored-hash (:event_hash row)]
          (cond
            (not= prev-hash (:prev_hash row))
            {:valid? false
             :reason :event/prev-hash-mismatch
             :event/seq (:event_seq row)
             :expected-prev prev-hash
             :actual-prev (:prev_hash row)}
            (and (not= expected stored-hash) (not= legacy-expected stored-hash))
            {:valid? false
             :reason :event/hash-mismatch
             :event/seq (:event_seq row)
             :expected expected
             :actual stored-hash}
            :else
            (recur (rest rows) stored-hash (inc n))))
        {:valid? true :events n}))))
