(ns evoclj.promotion.current
  "component — the CURRENT generation pointer and its compare-and-set.

  This namespace owns the ONLY code path that CHANGES the generations
  CURRENT pointer (Global Constraint 15; the component promotion
  transaction is the only caller). It is deliberately small: reading
  the pointer, and moving it with a compare-and-set. Nothing else in
  the codebase may write the `current` column (evoclj.store.recovery
  reads it; evoclj.promotion.promote moves it exclusively through
  `cas-current!`).

  Database Invariant 6 (CURRENT is exactly one row) is enforced by the
  partial unique index `generations_current_unique` (001-init.sql,
  component) at the database level: at most one row may carry
  current = 1. Exactly-one is guaranteed by this CAS: the seed
  generation is activated with current = 1, and every promotion clears
  the parent and sets the child inside ONE transaction, so a second
  current = 1 row can never be created.

  THE CAS (normative, component):

      UPDATE generations SET current = 0 WHERE current = 1 AND id = ?

  — the affected-rows check decides the race: 1 row means this caller
  cleared the pointer it expected to hold and may now set the child
  (current = 1); 0 rows means the pointer already moved underneath it
  (:stale). The pointer is only ever written inside a BEGIN IMMEDIATE
  transaction (see promote.clj), so a concurrent promotion serializes
  on SQLite's write lock: the loser's transaction starts only after
  the winner committed, reads the moved pointer, and reports :stale
  instead of overwriting it.

  The public functions are CONNECTION-based (they run on the caller's
  open transaction connection, like evoclj.store.event's private raw
  helpers) plus one convenience read over a store. Raw JDBC is used
  for the same reason evoclj.store.event documents: java.jdbc
  auto-manages transactions around every statement and org.xerial's
  setAutoCommit(false) opens its own deferred transaction, so neither
  can coexist with the explicit BEGIN IMMEDIATE that must hold
  SQLite's write lock before the pointer read."
  (:require [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite]))

(defn- raw-query
  "Run a parameterized SELECT on `conn`, returning rows as a vector of
  keyword-keyed maps (column labels as keywords, values as returned by
  the JDBC driver)."
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

(defn- raw-update!
  "Execute a parameterized UPDATE on `conn`; returns the affected-row
  count."
  [^java.sql.Connection conn sql params]
  (with-open [stmt (.prepareStatement conn sql)]
    (doseq [[i v] (map-indexed vector params)]
      (.setObject stmt (inc i) v))
    (.executeUpdate stmt)))

(defn read-current
  "The generations row that currently carries current = 1 (the CURRENT
  pointer, Database Invariant 6), or nil when no generation is current
  yet (an empty store). Connection-based: must run on the caller's
  open transaction connection."
  [conn]
  (first (raw-query conn "SELECT * FROM generations WHERE current = 1" [])))

(defn current-generation
  "The CURRENT generation row as read on a fresh connection to `store`
  (a path string or java.jdbc spec), or nil when no generation is
  current yet. Read-only convenience for callers outside a promotion
  transaction (tests, lineage queries); the pointer itself is only
  ever changed by `cas-current!`."
  [store]
  (first (sqlite/query store ["SELECT * FROM generations WHERE current = 1"])))

(defn cas-current!
  "THE CURRENT compare-and-set (component). Called INSIDE the promotion
  transaction (BEGIN IMMEDIATE), after the new generation row exists:

      1. UPDATE generations SET current = 0
         WHERE current = 1 AND id = <expected-generation-id>
      2. UPDATE generations SET current = 1 WHERE id = <new-generation-id>

  Step 1 is the race decision: 1 affected row means this caller held
  the pointer it expected and cleared it; 0 rows means the pointer
  moved underneath it and the caller must report :stale (the
  promotions table records nothing; CURRENT is untouched). When step 1
  succeeds, step 2 activates the new generation and MUST affect
  exactly one row (the caller inserted it in the same transaction).

  Returns :ok, or :stale when the expected generation is no longer
  current. Throws :promotion/cas-invalid when step 2 does not affect
  exactly one row — a caller bug, since under BEGIN IMMEDIATE the
  pointer cannot move between step 1 and step 2."
  [conn expected-generation-id new-generation-id]
  (let [cleared (raw-update! conn
                             "UPDATE generations SET current = 0
                              WHERE current = 1 AND id = ?"
                             [expected-generation-id])]
    (if (zero? cleared)
      :stale
      (let [activated (raw-update! conn
                                   "UPDATE generations SET current = 1
                                    WHERE id = ?"
                                   [new-generation-id])]
        (when-not (= 1 activated)
          (throw (err/error :promotion/cas-invalid
                            "CAS activated an unknown new generation"
                            {:new-generation-id new-generation-id
                             :activated activated})))
        :ok))))
