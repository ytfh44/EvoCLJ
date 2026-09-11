(ns evoclj.store.sqlite
  "SQLite connection helpers (component).

  SQLite enforces foreign keys PER CONNECTION: `PRAGMA foreign_keys` is
  not persisted across connections, so a connection that does not turn it
  on silently ignores every FOREIGN KEY clause in the schema. Every
  connection opened through this namespace enables enforcement, and the
  lineage FKs defined in 001-init.sql only work when callers route their
  connections through `with-db`. The migration runner and the store tests
  do this; future store modules MUST do the same."
  (:require [clojure.java.jdbc :as jdbc])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date)))

(defn spec
  "Coerce a database argument into a java.jdbc db spec.

  A string is treated as a SQLite file path and wrapped in the
  org.xerial driver spec (connection string `jdbc:sqlite:<path>`); a
  map is passed through unchanged so callers can supply their own spec."
  [db]
  (if (string? db)
    {:classname "org.sqlite.JDBC"
     :subprotocol "sqlite"
     :subname db}
    db))

(defn enable-foreign-keys!
  "Enable SQLite foreign-key enforcement on `db` (a path string, a
  java.jdbc spec, or the spec-with-connection map bound by `with-db`).
  java.jdbc high-level functions accept any of these; returns the
  input unchanged."
  [conn]
  (jdbc/execute! conn ["PRAGMA foreign_keys = ON"])
  conn)

(defn set-busy-timeout!
  "Set SQLite's busy timeout to `ms` milliseconds on an already-open
  java.jdbc db spec (`:connection` map, as bound by `with-db`). A
  contended writer then WAITS for SQLite's write lock instead of
  failing with SQLITE_BUSY."
  [db ms]
  (let [^java.sql.Connection conn (:connection db)]
    (with-open [stmt (.createStatement conn)]
      (.execute stmt (str "PRAGMA busy_timeout = " ms)))))

;; --- canonical timestamps ---------------------------------------------------
;;
;; These strings land in `created_at`-style columns and are compared and
;; sorted as text, so the emitted form is a load-bearing invariant: ONE
;; formatter (DateTimeFormatter/ISO_INSTANT) for every store. The
;; per-namespace typed errors stay with the callers via `fail`.

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn canonical-timestamp
  "Canonical ISO-8601 UTC string for a timestamp value (a
  java.util.Date, a java.time.Instant, or an ISO-8601 string); nil means
  now.

  `fail` receives the offending value and MUST return the Throwable to
  throw, so each store keeps its own typed error (callers may match on
  :error/type)."
  [ts fail]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (fail ts)))]
    (.format timestamp-fmt inst)))

(defmacro with-db
  "Run `body` on a single open connection to `db` (a path string or
  java.jdbc spec), with SQLite foreign-key enforcement enabled, and
  close the connection afterwards.

  IMPORTANT CONTRACT: `conn-binding` is bound to the java.jdbc
  spec-with-connection MAP (jdbc/with-db-connection semantics — the
  map carries the live java.sql.Connection under its `:connection`
  key), NOT to a raw Connection. Pass it to java.jdbc high-level
  functions (insert!/query/execute!/update!) — never to raw JDBC
  primitives. Code that needs a raw java.sql.Connection (e.g.
  evoclj.promotion.current/cas-current!) must obtain one explicitly
  with (clojure.java.jdbc/get-connection spec) or (:connection spec)."
  [[conn-binding db] & body]
  `(jdbc/with-db-connection [~conn-binding (evoclj.store.sqlite/spec ~db)]
     (evoclj.store.sqlite/enable-foreign-keys! ~conn-binding)
     ~@body))

(defn exec!
  "Execute sql-params on a fresh connection to `db` (path or spec);
  returns the result of java.jdbc/execute! (a vector of update counts
  for execute! on a plain SQL string)."
  [db sql-params]
  (with-db [conn db]
    (jdbc/execute! conn sql-params)))

(defn query
  "Run sql-params on a fresh connection to `db` (path or spec);
  returns result rows as vectors of maps."
  [db sql-params]
  (with-db [conn db]
    (jdbc/query conn sql-params)))

(defn exec-raw!
  "Execute a no-result SQL statement on an already-open raw
  java.sql.Connection (e.g. inside with-write-tx)."
  [^java.sql.Connection conn sql]
  (with-open [stmt (.createStatement conn)]
    (.execute stmt sql)))

(defn query-raw!
  "Run a parameterized SELECT on an already-open raw java.sql.Connection.
  Returns rows as a vector of keyword-keyed maps (column labels)."
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

(defn insert-raw!
  "Run a parameterized INSERT/UPDATE on an already-open raw
  java.sql.Connection. Returns the update count."
  [^java.sql.Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (.executeUpdate stmt)))

(defmacro with-write-tx
  "Run `body` on a single raw java.sql.Connection to `db` inside one
  BEGIN IMMEDIATE write transaction (busy_timeout 10s, FK enforcement
  on), COMMIT on success and ROLLBACK + rethrow on failure.
  BEGIN IMMEDIATE takes SQLite's write lock up front, so sequence
  allocation and CAS state checks inside serialize against concurrent
  writers. `conn-binding` is a raw Connection: use exec-raw!/query-raw!/
  insert-raw! (or evoclj.store.event/append-event-on-conn!) inside."
  [[conn-binding db] & body]
  `(with-open [~conn-binding (jdbc/get-connection (spec ~db))]
     (exec-raw! ~conn-binding "PRAGMA foreign_keys = ON")
     (exec-raw! ~conn-binding "PRAGMA busy_timeout = 10000")
     (try
       (exec-raw! ~conn-binding "BEGIN IMMEDIATE")
       (let [result# (do ~@body)]
         (exec-raw! ~conn-binding "COMMIT")
         result#)
       (catch Throwable t#
         (try (exec-raw! ~conn-binding "ROLLBACK")
              (catch Throwable _# nil))
         (throw t#)))))
