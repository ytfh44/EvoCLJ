(ns evoclj.environment.fake
  "FakeSource - mutable atom-backed LiveSource for tests."
  (:require [evoclj.environment.source :as src]
            [evoclj.environment.surface :as surf]))

(defrecord FakeSource [source-id state subs closed?]
  src/LiveSource
  (snapshot! [this]
    (let [{:keys [payload fail? error]} @state]
      (when fail?
        (throw (or error (ex-info "fake snapshot failure" {:error/type :fake/failure}))))
      {:source/id source-id
       :payload payload
       :captured-at (System/currentTimeMillis)}))
  (subscribe! [this invalidate-fn]
    (let [id (random-uuid)
          close-fn (fn [] (swap! subs dissoc id))]
      (swap! subs assoc id invalidate-fn)
      {:subscription/id id :close! close-fn}))
  (project [this snapshot]
    ;; Pure projector: derive a single ContextSurface bundle from the captured
    ;; snapshot. Pure data transformation only — publishes nothing.
    (let [sid (:source/id snapshot)
          payload (:payload snapshot)
          surfaces [(surf/make-context-surface
                      {:id (keyword (name (or sid :fake)) "ctx")
                       :descriptor {:name (str sid) :payload payload}
                       :materializer (fn ([] payload) ([_ _] payload) ([_ _ _] payload))})]]
      {:logical-id sid
       :source-id sid
       :payload (or payload {:source/id sid})
       :surfaces surfaces}))
  (close! [this]
    (reset! closed? true)
    (reset! subs {})))

(defn make-fake-source
  "Create a FakeSource with source-id and initial payload."
  [source-id initial-payload]
  (->FakeSource source-id (atom {:payload initial-payload :fail? false}) (atom {}) (atom false)))

(defn set-payload!
  "Set payload and optionally trigger invalidation."
  ([source payload] (set-payload! source payload false))
  ([source payload trigger?]
   (swap! (:state source) assoc :payload payload)
   (when trigger?
     (doseq [f (vals @(:subs source))]
       (try (f) (catch Exception _ nil))))
   source))

(defn set-failure!
  "Make next snapshot! throw."
  [source error]
  (swap! (:state source) assoc :fail? true :error error)
  source)

(defn clear-failure!
  "Clear failure mode."
  [source]
  (swap! (:state source) assoc :fail? false :error nil)
  source)

(defn trigger-invalidate!
  "Manually trigger all subscribers with raw signal (for testing that
   registry does not forward raw events)."
  [source]
  (doseq [f (vals @(:subs source))]
    (try (f) (catch Exception _ nil))))
