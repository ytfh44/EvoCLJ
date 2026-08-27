(ns evoclj.store.current-store
  "Fleet R — narrow opaque handle for the CURRENT generation pointer.

  Only this namespace and evoclj.promotion.promote may move CURRENT
  (Fleet R: make illegal authority unrepresentable — definition >
  validation). Business namespaces must receive a CurrentStore, not a
  raw {:sqlite db :cas cas} map. Read-only callers outside a promotion
  transaction should use current-generation via this handle; promotion
  uses the connection-based cas-current! inside its BEGIN IMMEDIATE
  transaction.

  The handle is opaque via deftype — it does NOT expose :db or :sqlite
  via keyword access; (:db handle) is nil. No db-of escape is provided.

  Schema unchanged; do not add kernel_state here (fleet S1 will)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.promotion.current :as current]))

(deftype CurrentStore [db])

(defn make-current-store
  "Constructor for the narrow CurrentStore handle. `db` is a SQLite
  path string or java.jdbc spec. The handle is opaque — it does not
  expose :db or :sqlite via keyword access."
  [db]
  (when (nil? db)
    (throw (err/error :promotion/system-invalid
                      "CurrentStore requires a non-nil db"
                      {:reason :sqlite-missing})))
  (->CurrentStore db))

(defn current-generation
  "The CURRENT generation row as read via CurrentStore (read-only).
  Returns the row map or nil. For callers outside a promotion
  transaction; the pointer itself is only ever changed by
  cas-current! inside promotion."
  [^CurrentStore store]
  (when-not (instance? CurrentStore store)
    (throw (err/error :promotion/system-invalid
                      "current-generation requires a CurrentStore"
                      {:reason :not-a-current-store})))
  (current/current-generation (.-db ^CurrentStore store)))

(defn- read-current
  "Connection-based CURRENT read — delegates to
  evoclj.promotion.current/read-current. Must run on the caller's open
  transaction connection (promotion). Package-private."
  [conn]
  (current/read-current conn))

(defn- cas-current!
  "THE CURRENT compare-and-set via CurrentStore connection.
  Delegates to evoclj.promotion.current/cas-current! and is the ONLY
  code path (besides promotion) that may change the CURRENT pointer.

  Must be called INSIDE the promotion transaction (BEGIN IMMEDIATE)
  after the new generation row exists. See promotion.current docstring
  for the CAS contract.

  Fleet R: only evoclj.store.current-store and evoclj.promotion.promote
  may call this."
  [conn expected-generation-id new-generation-id]
  (current/cas-current! conn expected-generation-id new-generation-id))
