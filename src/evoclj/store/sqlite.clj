(ns evoclj.store.sqlite
  "SQLite connection helpers (component).

  SQLite enforces foreign keys PER CONNECTION: `PRAGMA foreign_keys` is
  not persisted across connections, so a connection that does not turn it
  on silently ignores every FOREIGN KEY clause in the schema. Every
  connection opened through this namespace enables enforcement, and the
  lineage FKs defined in 001-init.sql only work when callers route their
  connections through `with-db`. The migration runner and the store tests
  do this; future store modules MUST do the same."
  (:require [clojure.java.jdbc :as jdbc]))

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
