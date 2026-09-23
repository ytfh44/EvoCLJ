(ns evoclj.store.memory-store
  "Fleet R horizontal — narrow opaque handle for episodic_memory rows.

  Only this namespace may do jdbc on episodic_memory
  (Fleet R: make illegal authority unrepresentable). Business namespaces
  (e.g. evoclj.provider.memory) must receive a MemoryStore, not a raw
  sqlite spec.

  The handle's db is NOT exposed via keyword access — deftype implements
  no ILookup, so (:db handle) is nil. The field itself is a public final
  JVM field (Clojure cannot make deftype fields private) and is reachable
  as (.-db handle), but there is deliberately NO accessor: every caller —
  including evoclj.provider.memory — passes the handle to
  memory-read / read-memory-value / memory-write! / memory-delete!, so the
  handle is the only way in and the statement shapes have one owner.

  FK existence (Fleet P5/F): episodic_memory.session_id
  REFERENCES sessions(id) (011), so a write for an unknown session fails
  with a foreign-key violation at rest, and the app boundary validates
  session existence before writing when a SessionStore is available.

  Outbox note: memory writes are not promoted via CURRENT, so no outbox
  is needed; the FK guarantees referential integrity."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)))

(deftype MemoryStore [db])

(defn make-memory-store
  "Constructor for the narrow MemoryStore handle. db is a SQLite path or spec."
  [db]
  (when (nil? db)
    (throw (err/error :store/memory-invalid
                      "MemoryStore requires a non-nil db"
                      {:reason :sqlite-missing})))
  (->MemoryStore db))

(defn memory-read
  "Read episodic memory content for (session-id, key) via MemoryStore, or nil."
  [^MemoryStore store session-id memory-key]
  (when-not (instance? MemoryStore store)
    (throw (err/error :store/memory-invalid
                      "memory-read requires a MemoryStore"
                      {:reason :not-a-memory-store})))
  (first (sqlite/query (.-db store)
                       ["SELECT content FROM episodic_memory WHERE session_id = ? AND memory_key = ?"
                        (str session-id) (name memory-key)])))

(defn read-memory-value
  "The provider-shaped read of one episodic-memory entry:

      {:memory/key <keyword> :memory/content <decoded EDN|nil>
       :memory/found <boolean>}

  The stored content is EDN text written by memory-write!; this is the
  single decode site (evoclj.provider.memory consumes it)."
  [^MemoryStore store session-id memory-key]
  (let [row (memory-read store session-id memory-key)]
    (if row
      {:memory/key memory-key
       :memory/content (clojure.edn/read-string (:content row))
       :memory/found true}
      {:memory/key memory-key
       :memory/content nil
       :memory/found false})))

(defn memory-write!
  "UPSERT episodic memory via MemoryStore. FK at rest (011) ensures session exists."
  [^MemoryStore store session-id memory-key content]
  (when-not (instance? MemoryStore store)
    (throw (err/error :store/memory-invalid
                      "memory-write! requires a MemoryStore"
                      {:reason :not-a-memory-store})))
  (sqlite/exec! (.-db store)
                ["INSERT OR REPLACE INTO episodic_memory (session_id, memory_key, content, created_at) VALUES (?, ?, ?, ?)"
                 (str session-id) (name memory-key) (pr-str content) (str (Instant/now))]))

(defn memory-delete!
  "Delete memory key for session via MemoryStore."
  [^MemoryStore store session-id memory-key]
  (when-not (instance? MemoryStore store)
    (throw (err/error :store/memory-invalid
                      "memory-delete! requires a MemoryStore"
                      {:reason :not-a-memory-store})))
  (sqlite/exec! (.-db store)
                ["DELETE FROM episodic_memory WHERE session_id = ? AND memory_key = ?"
                 (str session-id) (name memory-key)]))