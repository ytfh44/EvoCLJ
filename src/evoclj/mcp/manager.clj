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
  (atom {:pools {}}))

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
(defn get-or-open!
  "Return the managed client record for pool key `k`, opening it via
   `open-fn` (zero-arg; returns a managed record shaped like
   evoclj.mcp.client/open!'s) when no live entry exists.

   UNIFIED RETURN CONTRACT (WO-M1): every path returns the SAME KIND of
   value — the managed record itself, never the raw underlying client,
   never nil:

   - ready hit     -> the managed record stored in the pool entry;
   - first opener  -> open-fn's return value verbatim; it is stored as
                      the entry's :client and delivered to concurrent
                      waiters;
   - concurrent
     waiter        -> blocks on the single-flight promise, then returns
                      that same managed record; if the opener failed the
                      waiter THROWS the opener's Throwable (a Throwable
                      is never returned as a value).

   On open failure the pool entry is left :broken with its :promise
   cleared so a later call starts a fresh attempt (the opener itself
   rethrows); healing/retry policy is owned elsewhere."
  [mgr-atom k open-fn]
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
        (and promise (not new?))
        (let [v @promise]
          (if (instance? Throwable v) (throw v) v))
        :else
        (let [p promise]
          (try
            (let [managed (open-fn)]
              (swap! mgr-atom (fn [s] (-> s
                                          (assoc-in [:pools k :state] :ready)
                                          (assoc-in [:pools k :client] managed)
                                          (assoc-in [:pools k :health] {:last-ok (System/currentTimeMillis)})
                                          (update-in [:pools k] dissoc :promise))))
              (deliver p managed)
              managed)
            (catch Throwable ex
              (let [ed (err/sanitize ex)]
                (swap! mgr-atom (fn [s] (-> s
                                            (assoc-in [:pools k :state] :broken)
                                            (assoc-in [:pools k :health] {:last-error ed})
                                            (update-in [:pools k] dissoc :promise))))
                (deliver p ex)
                (throw ex)))))))))

(defn shutdown! [mgr-atom]
  (doseq [[_ e] (:pools @mgr-atom)]
    (when-let [c (:client e)] (try (mcp-client/close! c) (catch Throwable _ nil))))
  (reset! mgr-atom {:pools {}}))

(defmethod ig/init-key :mcp/manager [_ _] (create-manager))
(defmethod ig/halt-key! :mcp/manager [_ mgr] (shutdown! mgr))

;; # ponytail: global-lock ceiling — per-key locking would reduce contention but single atom swap! is sufficient for current scale
