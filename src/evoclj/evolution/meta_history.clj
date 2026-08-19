(ns evoclj.evolution.meta-history
  "Meta-evaluation history tracking (S3-2)."
  (:require [evoclj.store.sqlite :as sqlite]))

(defn- ensure-table!
  [db]
  (sqlite/exec! db ["CREATE TABLE IF NOT EXISTS meta_attempts
                     (id TEXT PRIMARY KEY,
                      generation_id TEXT,
                      fitness REAL,
                      params TEXT,
                      created_at TEXT)"]))

(defn record-meta-attempt!
  "Persist one meta-evolution attempt. Returns the attempt id."
  ([store meta-genome]
   (record-meta-attempt! store meta-genome {}))
  ([store meta-genome _opts]
   (let [db (:sqlite store)
         id (str (java.util.UUID/randomUUID))
         generation-id (or (:meta/generation-id meta-genome) "meta-unknown")
         fitness (double (or (:meta/fitness meta-genome) 0.0))
         params (pr-str (:meta/params meta-genome))
         ts (str (java.time.Instant/now))]
     (ensure-table! db)
     (sqlite/exec! db ["INSERT INTO meta_attempts
                        (id, generation_id, fitness, params, created_at)
                        VALUES (?, ?, ?, ?, ?)"
                       id generation-id fitness params ts])
     id)))

(defn recent-meta-history
  "Return the N most recent meta-attempts as plain maps."
  ([store]
   (recent-meta-history store 50))
  ([store limit]
   (let [db (:sqlite store)
         rows (sqlite/query db ["SELECT id, generation_id, fitness, params, created_at
                                 FROM meta_attempts
                                 ORDER BY created_at DESC
                                 LIMIT ?"
                                (int limit)])]
     (mapv (fn [row]
             {:meta/attempt-id (:id row)
              :meta/generation-id (:generation_id row)
              :meta/fitness (:fitness row)
              :meta/params (try (clojure.edn/read-string (:params row))
                                (catch Throwable _ []))
              :created-at (java.time.Instant/parse (:created_at row))})
           rows))))
