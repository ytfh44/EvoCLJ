(ns evoclj.store.current-store
  "CURRENT pointer store — narrow handle (Fleet R).
  Only this namespace + promotion transaction may move CURRENT.
  No other namespace should UPDATE generations.current."
  (:require [evoclj.store.sqlite :as sqlite]
            [evoclj.promotion.current :as current]))

(defrecord CurrentStore [db])

(defn make-current-store [db] (->CurrentStore db))

(defn read-current
  "Read CURRENT on fresh connection."
  [^CurrentStore s]
  (current/current-generation (:db s)))

(defn read-current-tx
  "Read CURRENT inside caller's transaction connection."
  [conn]
  (current/read-current conn))

(defn cas-current!
  "Compare-and-set CURRENT inside transaction. Only promotion should call this."
  [conn expected-id new-id]
  (current/cas-current! conn expected-id new-id))
