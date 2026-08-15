(ns evoclj.store.migrate
  "SQLite migration runner (Task 5.1).

  `migrate!` brings a database up to `latest-version` by applying every
  SQL file under resources/migrations (lexicographic order) inside a
  single transaction, then records the applied set and the schema
  version in the `meta` table. Applying again is a no-op that verifies
  the recorded version and applied set against the classpath.

  The runner NEVER guesses at an untrustworthy database. A database
  whose tables exist without a version record, whose version differs
  from the code's expectation, or whose applied-migration record is
  missing an on-classpath migration fails cleanly with a typed
  :store/schema-mismatch error, leaving the database untouched."
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite]))

(def latest-version
  "The schema version this codebase knows how to migrate to."
  3)

(def ^:private version-key "schema_version")
(def ^:private applied-key "applied_migrations")
(def ^:private migrations-dir "migrations")
(def ^:private migration-pattern #"^\d+.*\.sql$")

(defn migration-files
  "Return the sorted file names of SQL migrations on the classpath."
  []
  (let [dir (or (io/resource migrations-dir)
                (throw (err/error :store/migration-error
                                  "migrations directory not on classpath"
                                  {:path migrations-dir})))]
    (->> dir
         io/file
         file-seq
         (filter #(.isFile ^java.io.File %))
         (map #(.getName ^java.io.File %))
         (filter #(re-matches migration-pattern %))
         (sort))))

(defn- mismatch!
  "Throw a typed :store/schema-mismatch error describing why the
  database cannot be trusted. `expected` is what the code expects,
  `actual` is what the database reports."
  [reason expected actual]
  (throw (err/error :store/schema-mismatch
                    "database schema does not match the code's expectations"
                    {:reason reason :expected expected :actual actual})))

(defn- table-exists? [db table]
  (seq (sqlite/query db
                     ["SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?"
                      table])))

(defn- meta-value
  "The value recorded for `key` in the meta table, or nil when absent."
  [db key]
  (-> (sqlite/query db ["SELECT value FROM meta WHERE key = ?" key])
      first
      :value))

(defn current-version
  "Return the recorded schema version as an int, or 0 when the meta
  table does not exist yet (a blank database)."
  [db]
  (if (table-exists? db "meta")
    (if-let [v (meta-value db version-key)]
      (try
        (Integer/parseInt v)
        (catch NumberFormatException _
          (mismatch! :invalid-version-record (str latest-version) v)))
      0)
    0))

(defn- applied-migrations
  "Return the set of migration file names recorded in the meta table,
  or #{} when nothing is recorded."
  [db]
  (if (table-exists? db "meta")
    (if-let [v (meta-value db applied-key)]
      (set (str/split v #"\s+"))
      #{})
    #{}))

(defn- split-statements
  "Split a SQL script into individual statements for execution.

  org.xerial's Statement.execute runs only the FIRST statement of a
  multi-statement script, so the runner must split on statement
  boundaries itself. The splitter walks the script and only splits on
  `;` outside SQLite string literals ('...' with '' escapes),
  double-quoted identifiers, line comments (--) and block comments
  (/* ... */). Semicolons inside CREATE TRIGGER ... BEGIN ... END
  bodies are not split points (tracked by a BEGIN/END keyword depth),
  and trailing whitespace/comment-only chunks are dropped."
  [sql]
  (let [n (count sql)
        blank? #(every? (fn [c] (Character/isWhitespace c)) %)
        begin? #(re-find #"(?i)\bbegin\s*$" %)
        end?   #(re-find #"(?i)\bend\s*$" %)]
    (loop [i 0, quote nil, line-comment? false, block-comment? false,
           begin-depth 0, start 0, acc []]
      (if (>= i n)
        (let [tail (subs sql start)]
          (if (and (seq tail) (not (blank? tail)))
            (conj acc tail)
            acc))
        (let [c (.charAt sql i)
              nxt (when (< (inc i) n) (.charAt sql (inc i)))
              depth (fn [chunk]
                      (cond
                        (and (zero? begin-depth) (begin? chunk)) 1
                        (and (pos? begin-depth) (end? chunk)) (dec begin-depth)
                        :else begin-depth))]
          (cond
            line-comment?
            (if (= c \newline)
              (recur (inc i) nil false block-comment? begin-depth start acc)
              (recur (inc i) nil true block-comment? begin-depth start acc))

            block-comment?
            (if (and (= c \*) (= nxt \/))
              (recur (+ i 2) nil line-comment? false begin-depth start acc)
              (recur (inc i) nil line-comment? true begin-depth start acc))

            quote
            (cond
              (and (= c quote) (= nxt quote)) ; '' or "" escape inside literal
              (recur (+ i 2) quote line-comment? block-comment? begin-depth start acc)
              (= c quote)
              (recur (inc i) nil line-comment? block-comment? begin-depth start acc)
              :else
              (recur (inc i) quote line-comment? block-comment? begin-depth start acc))

            (and (= c \-) (= nxt \-))
            (recur (+ i 2) nil true block-comment? begin-depth start acc)

            (and (= c \/) (= nxt \*))
            (recur (+ i 2) nil line-comment? true begin-depth start acc)

            (or (= c \') (= c \"))
            (recur (inc i) c line-comment? block-comment? begin-depth start acc)

            (= c \;)
            (let [chunk (subs sql start i)
                  d (depth chunk)]
              (if (pos? d)
                ;; inside a BEGIN ... END body; this ; belongs to it
                (recur (inc i) nil line-comment? block-comment? d start acc)
                (let [stmt chunk]
                  (if (blank? stmt)
                    (recur (inc i) nil line-comment? block-comment? 0 (inc i) acc)
                    (recur (inc i) nil line-comment? block-comment? 0 (inc i)
                           (conj acc stmt))))))

            :else
            (recur (inc i) quote line-comment? block-comment?
                   (depth (subs sql start i)) start acc)))))))

(defn- run-migration!
  "Execute a migration file's statements on the open connection and
  ACCUMULATE its file name into the meta table's applied-migrations set
  (the set is stored space-joined in one meta row, so each migration
  appends its name instead of overwriting the record of earlier ones)."
  [conn file-name sql]
  (doseq [stmt (split-statements sql)]
    (jdbc/execute! conn [stmt]))
  (let [applied (-> (jdbc/query conn ["SELECT value FROM meta WHERE key = ?" applied-key])
                    first
                    :value)
        updated (if (seq applied)
                  (str/join " " (distinct (conj (str/split applied #"\s+") file-name)))
                  file-name)]
    (jdbc/execute! conn
                   ["INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)"
                    applied-key updated])))

(defn- apply-files!
  "Execute `files` (migration file names) inside one transaction and
  record the schema version. Returns :applied."
  [db files]
  (jdbc/with-db-transaction [conn (sqlite/spec db)]
    (sqlite/enable-foreign-keys! conn)
    (doseq [f files]
      (run-migration! conn f (slurp (io/resource (str migrations-dir "/" f)))))
    (jdbc/execute! conn
                   ["INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)"
                    version-key (str latest-version)]))
  :applied)

(defn migrate!
  "Bring `db` (a path string or java.jdbc spec) up to `latest-version`.

  Returns {:status :applied :version n} when migrations ran, or
  {:status :noop :version n} when the database was already current.
  Throws :store/schema-mismatch when the database cannot be trusted:
  tables present without a version record, a version ahead of or
  unknown to this codebase, or a migration file on the classpath that
  the database does not record as applied. A failed migrate! changes
  nothing."
  [db]
  (let [files (migration-files)
        version (current-version db)]
    (cond
      ;; A blank database: no tables, no version record.
      (and (zero? version) (not (table-exists? db "generations")))
      {:status (apply-files! db files) :version latest-version}

      ;; Tables exist but there is no version record: someone created
      ;; schema by hand. Never guess — fail cleanly.
      (zero? version)
      (mismatch! :unversioned-tables
                 {:version latest-version}
                 {:version version
                  :reason "tables present but no schema_version record"})

      ;; Already current: verify the applied-migration record covers
      ;; every migration file on the classpath, then no-op.
      (= version latest-version)
      (let [applied (applied-migrations db)
            missing (remove applied files)]
        (when (seq missing)
          (mismatch! :missing-migration-record
                     (str "applied_migrations covers: "
                          (str/join " " (sort applied)))
                     (str "unrecorded on classpath: "
                          (str/join " " missing))))
        {:status :noop :version latest-version})

      ;; A known older version: apply ONLY the pending migrations (an
      ;; additive upgrade, e.g. a version-1 database gaining
      ;; 003-routing.sql), then record the new version. Pending is empty
      ;; when the files already ran but the version record lags — the
      ;; record is simply brought forward.
      (< version latest-version)
      (let [applied (applied-migrations db)
            pending (remove applied files)]
        (when (seq pending)
          (apply-files! db pending))
        (sqlite/with-db [conn db]
          (jdbc/execute! conn
                         ["INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)"
                          version-key (str latest-version)]))
        {:status (if (seq pending) :applied :noop) :version latest-version})

      ;; Anything else: the database is ahead of, or unknown to, this
      ;; codebase. Fail cleanly rather than half-apply.
      :else
      (mismatch! :version-ahead latest-version version))))
