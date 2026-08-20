(ns evoclj.mcp.manager
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as mcp-client]
            [integrant.core :as ig]))

(defn- redact-subtree [m k]
  (if (contains? m k) (assoc m k "[REDACTED]") m))

(defn normalize-transport [cfg]
  (let [cfg (or cfg {})]
    (-> cfg
        (redact-subtree :env)
        (redact-subtree :headers)
        (redact-subtree "env")
        (redact-subtree "headers"))))

(defn credential-fingerprint [cfg]
  (hash (:auth/ref cfg)))

(defn transport-identity [cfg]
  (dissoc (normalize-transport cfg) :auth/ref))

(defn connection-key [cfg]
  (let [cid (:connection/id cfg)
        ti (transport-identity cfg)
        cf (credential-fingerprint cfg)]
    [(:type cfg) cid ti cf]))

(defn create-manager []
  (atom {:pools {} :refresh-registry {}}))

(defn- entry-metrics [entry] (:metrics entry {:call-count 0 :latency-ms nil}))

;; single swap! operations
(defn pool-get [mgr-atom k]
  (get-in @mgr-atom [:pools k]))

(defn acquire [mgr-atom k owner-id]
  (let [res (atom nil)]
    (swap! mgr-atom
           (fn [s]
             (if-let [e (get-in s [:pools k])]
               (do (reset! res e)
                   (update-in s [:pools k :owners] (fnil conj #{}) owner-id))
               (do (reset! res nil) s))))
    @res))

(defn release [mgr-atom k owner-id]
  (swap! mgr-atom
         (fn [s]
           (if-let [e (get-in s [:pools k])]
             (let [owners (disj (or (:owners e) #{}) owner-id)]
               (if (empty? owners)
                 (do (when-let [c (:client e)] (try (mcp-client/close! c) (catch Throwable _ nil)))
                     (update s :pools dissoc k))
                 (assoc-in s [:pools k :owners] owners)))
             s))))

(defn put-ready [mgr-atom k managed]
  (swap! mgr-atom
         (fn [s]
           (let [e (get-in s [:pools k])
                 gen (inc (or (:generation e) 0))]
             (assoc-in s [:pools k]
                       {:state :ready :client managed :owners (or (:owners e) #{})
                        :generation gen :metrics (or (:metrics e) {:call-count 0})
                        :transport-identity (transport-identity (:transport-config managed))
                        :credential-identity (credential-fingerprint (:transport-config managed))
                        :health {:last-ok (System/currentTimeMillis)}})))))

(defn mark-broken [mgr-atom k err-data]
  (swap! mgr-atom update-in [:pools k] merge {:state :broken :health {:last-error err-data}}))

(defn set-metrics [mgr-atom k f]
  (swap! mgr-atom update-in [:pools k :metrics] (fn [m] (f (or m {:call-count 0})))))

;; single-flight: absent -> promise -> connecting -> ready/broken
(defn get-or-open! [mgr-atom k open-fn]
  (let [slot (atom nil)]
    (swap! mgr-atom
           (fn [s]
             (let [e (get-in s [:pools k])]
               (cond
                 (and e (= :ready (:state e))) (do (reset! slot {:hit e}) s)
                 (and e (contains? #{:connecting :reconnecting} (:state e)))
                 (do (reset! slot {:promise (:promise e)}) s)
                 :else
                 (let [p (promise)]
                   (reset! slot {:promise p :new? true})
                   (-> s
                       (assoc-in [:pools k :state] :connecting)
                       (assoc-in [:pools k :promise] p)
                       (assoc-in [:pools k :generation] (inc (or (:generation e) 0)))))))))
    (let [{:keys [hit promise new?]} @slot]
      (cond
        hit (:client hit)
        (and promise (not new?)) @promise
        :else
        (let [p promise]
          (try
            (let [managed (open-fn)
                  client (:client managed)]
              (swap! mgr-atom (fn [s] (-> s
                                          (assoc-in [:pools k :state] :ready)
                                          (assoc-in [:pools k :client] managed)
                                          (assoc-in [:pools k :health] {:last-ok (System/currentTimeMillis)})
                                          (dissoc :promise))))
              (deliver p client)
              client)
            (catch Throwable ex
              (let [ed (err/sanitize ex)]
                (swap! mgr-atom (fn [s] (-> s
                                            (assoc-in [:pools k :state] :broken)
                                            (assoc-in [:pools k :health] {:last-error ed})
                                            (dissoc :promise))))
                (deliver p ex)
                (throw ex)))))))))

(defn mark-removed! [mgr-atom tool-id]
  (when-let [{:keys [descriptor-atom]} (get-in @mgr-atom [:refresh-registry tool-id])]
    (swap! descriptor-atom assoc :mcp/status :removed :mcp/removed-at (System/currentTimeMillis))))

(defn on-tools-changed! [mgr-atom prev-ids curr-ids]
  (doseq [id (clojure.set/difference (set prev-ids) (set curr-ids))]
    (mark-removed! mgr-atom id)))

(defn mark-discovered-ungranted! [mgr-atom tool-id descriptor]
  (swap! mgr-atom assoc-in [:refresh-registry tool-id] {:descriptor-atom (atom (assoc descriptor :mcp/status :discovered-ungranted))}))

(defn tool-status [mgr-atom tool-id]
  (get-in @mgr-atom [:refresh-registry tool-id :descriptor-atom]))

(defn shutdown! [mgr-atom]
  (doseq [[_ e] (:pools @mgr-atom)]
    (when-let [c (:client e)] (try (mcp-client/close! c) (catch Throwable _ nil))))
  (reset! mgr-atom {:pools {} :refresh-registry {}}))

(defmethod ig/init-key :mcp/manager [_ _] (create-manager))
(defmethod ig/halt-key! :mcp/manager [_ mgr] (shutdown! mgr))

;; # ponytail: global-lock ceiling — per-key locking would reduce contention but single atom swap! is sufficient for current scale
