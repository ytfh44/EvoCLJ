(ns evoclj.store.candidate-store
  "Candidate persistence boundary — narrow store handle (Fleet R).
  Only this namespace holds raw SQLite authority for candidates/mutations.
  Callers receive an opaque handle, not {:sqlite db}."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.store.sqlite :as sqlite]))

(defrecord CandidateStore [db])

(defn make-candidate-store
  "Create opaque candidate store handle from db spec/path."
  [db]
  (->CandidateStore db))

(defn db-of [^CandidateStore s] (:db s))

;; Thin wrappers that are the ONLY place touching candidates/mutations tables
(defn query-candidates [^CandidateStore s sql-params]
  (sqlite/query (:db s) sql-params))

(defn insert-candidate! [^CandidateStore s row]
  (sqlite/with-db [conn (:db s)]
    (jdbc/insert! conn :candidates row)))

(defn update-candidate-state! [^CandidateStore s id expected-db-state new-db-state]
  (sqlite/with-db [conn (:db s)]
    (first (jdbc/execute! conn
             ["UPDATE candidates SET state = ? WHERE id = ? AND state = ?"
              new-db-state id expected-db-state]))))

(defn insert-mutation! [^CandidateStore s row]
  (sqlite/with-db [conn (:db s)]
    (jdbc/execute! conn
      ["INSERT OR IGNORE INTO mutations (id, parent_genome_id, hypothesis_id, evidence_id, risk, ops, expected_effect, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
       (:id row) (:parent_genome_id row) (:hypothesis_id row) (:evidence_id row) (:risk row) (:ops row) (:expected_effect row) (:created_at row)])))
