(ns evoclj.store.current-store
  "Fleet R+S1 - narrow opaque handle for the CURRENT generation pointer.

  Only this namespace and evoclj.promotion.promote may move CURRENT
  (Fleet R: make illegal authority unrepresentable - definition >
  validation). Business namespaces must receive a CurrentStore, not a
  raw {:sqlite db :cas cas} map. Read-only callers outside a promotion
  transaction should use current-generation via this handle; promotion
  uses the connection-based cas-current! inside its BEGIN IMMEDIATE
  transaction.

  The handle is opaque via deftype - it does NOT expose :db or :sqlite
  via keyword access; (:db handle) is nil. No db-of escape is provided.

  Fleet S1 (DAG S1): CURRENT transitions from predicate (generations.current
  INTEGER 0/1 + partial unique index) to singleton reference (kernel_state
  id=1 WITHOUT ROWID, FK to generations). The predicate column is kept as a
  derived sync for backward compat until S1b; this namespace PREFERS
  kernel_state on every read (JOIN) and on every CAS (UPDATE kernel_state
  where current_generation = expected), falling back to the legacy
  predicate CAS only when kernel_state is empty (empty DB before first
  seed or transitional). init-singleton! seeds the singleton on first
  promotion when empty."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.kernel.error :as err]
            [evoclj.promotion.current :as current]
            [evoclj.store.sqlite :as sqlite]))

(deftype CurrentStore [db])

(defn make-current-store
  "Constructor for the narrow CurrentStore handle. db is a SQLite
  path string or java.jdbc spec. The handle is opaque - it does not
  expose :db or :sqlite via keyword access."
  [db]
  (when (nil? db)
    (throw (err/error :promotion/system-invalid
                      "CurrentStore requires a non-nil db"
                      {:reason :sqlite-missing})))
  (->CurrentStore db))

(defn current-generation
  "The CURRENT generation row as read via CurrentStore (read-only).
  Prefers the singleton kernel_state (JOIN) when present; falls back to
  the legacy generations.current predicate for compat. Returns the row
  map or nil. For callers outside a promotion transaction; the pointer
  itself is only ever changed by cas-current! inside promotion."
  [^CurrentStore store]
  (when-not (instance? CurrentStore store)
    (throw (err/error :promotion/system-invalid
                      "current-generation requires a CurrentStore"
                      {:reason :not-a-current-store})))
  (let [db (.-db ^CurrentStore store)
        via-singleton (try
                        (first (sqlite/query db ["SELECT g.* FROM generations g JOIN kernel_state k ON k.current_generation = g.id WHERE k.id = 1"]))
                        (catch Exception _ nil))]
    (or via-singleton
        (current/current-generation db))))

(defn- read-current
  "Connection-based CURRENT read - prefers kernel_state singleton, falls
  back to generations predicate. Must run on the caller's open transaction
  connection (promotion). Package-private."
  [conn]
  (let [row (try
              (first (jdbc/query conn ["SELECT g.* FROM generations g JOIN kernel_state k ON k.current_generation = g.id WHERE k.id = 1"]))
              (catch Exception _ nil))]
    (or row (current/read-current conn))))

(defn- cas-current!
  "THE CURRENT compare-and-set via CurrentStore connection.
  Prefers the singleton: UPDATE kernel_state SET current_generation = ?
  WHERE id=1 AND current_generation = ?. On success the kernel_state
  trigger syncs generations.current; on miss (empty or stale) falls
  back to the legacy predicate CAS. Exceptions (FK violation, CHECK,
  schema) propagate — zero rows is the only fallback trigger.

  Must be called INSIDE the promotion transaction (BEGIN IMMEDIATE)
  after the new generation row exists. See promotion.current docstring
  for the CAS contract.

  Fleet R: only evoclj.store.current-store and evoclj.promotion.promote
  may call this."
  [conn expected-generation-id new-generation-id]
  (let [cnt (first (jdbc/execute! conn ["UPDATE kernel_state SET current_generation = ?, updated_at = datetime('now') WHERE id = 1 AND current_generation = ?" new-generation-id expected-generation-id]))
        updated (if (vector? cnt) (first cnt) cnt)]
    (if (= 1 updated)
      :ok
      (current/cas-current! conn expected-generation-id new-generation-id))))

(defn init-singleton!
  "Seed kernel_state on first promotion when empty. Inserts (1, generation-id)
  if no row exists; no-op when singleton already present. Call once after
  inserting the seed generation row and before first CAS."
  [^CurrentStore store generation-id]
  (when-not (instance? CurrentStore store)
    (throw (err/error :promotion/system-invalid
                      "init-singleton! requires a CurrentStore"
                      {:reason :not-a-current-store})))
  (sqlite/with-db [conn (.-db ^CurrentStore store)]
    (jdbc/execute! conn ["INSERT OR IGNORE INTO kernel_state (id, current_generation, updated_at) VALUES (1, ?, datetime('now'))" generation-id])))
