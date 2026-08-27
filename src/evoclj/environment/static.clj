(ns evoclj.environment.static
  "StaticSource - immutable single payload LiveSource for tests."
  (:require [evoclj.environment.source :as src]
            [evoclj.environment.surface :as surf]))

(defrecord StaticSource [source-id payload]
  src/LiveSource
  (snapshot! [this]
    {:source/id source-id
     :payload payload
     :captured-at (System/currentTimeMillis)})
  (subscribe! [this _invalidate-fn]
    {:subscription/id (random-uuid) :close! (fn [] nil)})
  (project [this snapshot]
    ;; Pure projector: derive a single ContextSurface bundle from the captured
    ;; snapshot. Pure data transformation only — publishes nothing.
    (let [sid (:source/id snapshot)
          p (:payload snapshot)
          surfaces [(surf/make-context-surface
                      {:id (keyword (name (or sid :static)) "ctx")
                       :descriptor {:name (str sid) :payload p}
                       :materializer (fn ([] p) ([_ _] p) ([_ _ _] p))})]]
      {:logical-id sid
       :source-id sid
       :payload (or p {:source/id sid})
       :surfaces surfaces}))
  (close! [this] nil))

(defn make-static-source
  "Create a StaticSource with source-id and payload."
  [source-id payload]
  (->StaticSource source-id payload))
