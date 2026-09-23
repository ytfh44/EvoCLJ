(ns evoclj.store.generation-store
  "The generations-table WRITE owner (component): every INSERT/UPDATE of
  a generations row goes through this namespace.

  Why: the promotion transaction, the rollback transaction, and the
  CURRENT compare-and-set each carried their own copy of the same
  statements (insert the promoted row, retire the superseded row, flip
  the derived current flag). Copies of a state-machine statement drift —
  a changed predicate in one place silently weakens the CAS in another.
  The statements live here; the promotion namespaces keep the machine
  vocabulary (which edge is legal, which typed error to throw).

  Connection contract: the `-on-conn!` forms take a RAW
  java.sql.Connection and are meant to run INSIDE the caller's BEGIN
  IMMEDIATE transaction (evoclj.store.sqlite/with-write-tx or the
  promotion transaction's own connection) — they never open one. The
  non-suffixed forms are the standalone wrappers.

  Global Constraint 15: the CURRENT pointer is still moved ONLY by
  evoclj.promotion.current/cas-current! (which uses `clear-current-on-conn!`
  / `set-current-on-conn!` for its predicate fallback). The
  generations.current column is DERIVED (the kernel_state triggers keep it
  in sync); these functions are the only writers of it."
  (:require [evoclj.store.sqlite :as sqlite]))

(defn insert-generation-on-conn!
  "INSERT one :active, current = 0 generation row on an EXISTING raw
  connection. `row` carries :id, :genome-id, :resolution-id, :parent-id
  (nullable) and :created-at. Returns the affected row count. The row is
  born :active with current = 0 — the pointer is moved separately by the
  CURRENT compare-and-set."
  [conn {:keys [id genome-id resolution-id parent-id created-at]}]
  (sqlite/insert-raw! conn
                      "INSERT INTO generations
                         (id, genome_id, resolution_id, parent_id, state, current, created_at)
                       VALUES (?, ?, ?, ?, 'active', 0, ?)"
                      [(str id) (str genome-id) (str resolution-id)
                       (some-> parent-id str) (str created-at)]))

(defn insert-generation!
  "Standalone (own-connection) form of `insert-generation-on-conn!`.
  Returns the affected row count."
  [db row]
  (sqlite/with-db [conn db]
    (insert-generation-on-conn! (:connection conn) row)))

(defn retire-generation-on-conn!
  "CAS one generation row :active -> :retired on an EXISTING raw
  connection; returns the affected row count (0 means the row was not
  :active anymore — the caller reports the CAS failure). The derived
  current flag is NOT touched here."
  [conn generation-id]
  (sqlite/insert-raw! conn
                      "UPDATE generations SET state = 'retired'
                        WHERE id = ? AND state = 'active'"
                      [(str generation-id)]))

(defn set-generation-state-on-conn!
  "CAS one generation row's state: `expected-state` -> `new-state` on an
  EXISTING raw connection; returns the affected row count. When
  `require-current?` is true the row must ALSO still carry current = 1
  (the rollback's from-generation guard: only the CURRENT, :active
  generation may be marked rolled back)."
  [conn generation-id expected-state new-state require-current?]
  (sqlite/insert-raw! conn
                      (if require-current?
                        "UPDATE generations SET state = ?
                          WHERE id = ? AND state = ? AND current = 1"
                        "UPDATE generations SET state = ?
                          WHERE id = ? AND state = ?")
                      [(name new-state) (str generation-id) (name expected-state)]))

(defn clear-current-on-conn!
  "The predicate-CAS clear step on an EXISTING raw connection:
  current = 0 for the row that still carries current = 1 and the expected
  id. Returns the affected row count — 1 means this caller held the
  pointer it expected, 0 means the pointer moved underneath it (:stale).
  Called only by evoclj.promotion.current/cas-current!."
  [conn expected-generation-id]
  (sqlite/insert-raw! conn
                      "UPDATE generations SET current = 0
                        WHERE current = 1 AND id = ?"
                      [(str expected-generation-id)]))

(defn set-current-on-conn!
  "The predicate-CAS activate step on an EXISTING raw connection:
  current = 1 for the new generation. Returns the affected row count —
  exactly 1 is required by the caller. Called only by
  evoclj.promotion.current/cas-current!."
  [conn new-generation-id]
  (sqlite/insert-raw! conn
                      "UPDATE generations SET current = 1
                        WHERE id = ?"
                      [(str new-generation-id)]))
