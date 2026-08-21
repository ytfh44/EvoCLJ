(ns evoclj.environment.static
  "StaticSource - immutable single payload LiveSource for tests."
  (:require [evoclj.environment.source :as src]))

(defrecord StaticSource [source-id payload]
  src/LiveSource
  (snapshot! [this]
    {:source/id source-id
     :payload payload
     :captured-at (System/currentTimeMillis)})
  (subscribe! [this _invalidate-fn]
    {:subscription/id (random-uuid) :close! (fn [] nil)})
  (close! [this] nil))

(defn make-static-source
  "Create a StaticSource with source-id and payload."
  [source-id payload]
  (->StaticSource source-id payload))
