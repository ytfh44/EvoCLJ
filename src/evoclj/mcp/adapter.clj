(ns evoclj.mcp.adapter
  (:require [evoclj.mcp.client :as client]
            [evoclj.kernel.error :as err]))

(defprotocol ProtocolAdapter
  (discover [this ctx] "list+normalize tools")
  (wire-request [this contract] "enrich per-request _meta/headers/session")
  (on-notification [this event] "handle toolsChanged/progress/subscriptions")
  (cache-policy [this] "return {:ttl-ms :cache-scope} or nil")
  (continue [this task] "MRTR/Tasks continuation stub"))

(defrecord Adapter2025 [opts]
  ProtocolAdapter
  (discover [_ ctx] (client/list-all-tools (:client ctx)))
  (wire-request [_ c] (assoc c :adapter/version :mcp-2025-11 :mcp/sessionful true))
  (on-notification [_ e] (when-let [f (:tools-change-consumer opts)] (f e)) e)
  (cache-policy [_] nil)
  (continue [_ _] (throw (err/error :mcp/not-supported "MRTR not supported on 2025 adapter" {}))))

(defrecord Adapter2026 [opts cache subscriptions]
  ProtocolAdapter
  (discover [_ ctx]
    (let [{:keys [ttl-ms]} (cache-policy _)
          now (System/currentTimeMillis)
          cached @cache]
      (if (and cached (< (- now (:ts cached 0)) (or ttl-ms 60000)))
        (:tools cached)
        (let [tools (client/list-all-tools (:client ctx))]
          (reset! cache {:tools tools :ts now}) tools))))
  (wire-request [_ c] (assoc c :adapter/version :mcp-2026-07 :mcp/stateless true :mcp/_meta (merge {:cache-scope :tools/list} (:_meta c))))
  (on-notification [_ e]
    (when (= :tools-changed (:event e)) (reset! cache nil))
    (when-let [f (:listen opts)] (swap! subscriptions conj e)) e)
  (cache-policy [_] {:ttl-ms (or (:ttl-ms opts) 60000) :cache-scope :tools/list})
  (continue [_ task] {:task task :status :continuing :adapter :mcp-2026-07}))

(defn adapter-2025 ([] (->Adapter2025 {})) ([opts] (->Adapter2025 opts)))
(defn adapter-2026 ([] (->Adapter2026 {} (atom nil) (atom []))) ([opts] (->Adapter2026 opts (atom nil) (atom []))))
(def default-adapter (adapter-2025))
