(ns evoclj.mcp.manager
  "MCP connection-pool manager.

   INV-01 (WO-M2): one transport config feeds THREE independent
   derivations that must never be conflated:

     - EXECUTION INPUT  — callers pass the REAL config to
       evoclj.mcp.client/open!; nothing in this namespace may stand
       between them (the pre-fix normalize-transport did exactly that).
     - POOL IDENTITY    — connection-key derives from transport-identity
       (per-field stable sha256 fingerprints of secret fields) plus
       credential-fingerprint (stable sha256 of :auth/ref).
     - DIAGNOSTIC FORM  — redact-transport replaces whole :env/:headers
       values with \"[REDACTED]\" for error data / audit / descriptor
       serialization boundaries only.

   Fingerprint format follows evoclj.genome.hash: \"sha256:<64 lowercase
   hex>\" over canonical EDN printing (map order normalized before
   hashing). Fingerprints are stable across JVM restarts."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as mcp-client]
            [integrant.core :as ig])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

;; --- diagnostic representation (display/audit ONLY) --------------------------

(defn- redact-subtree [m k]
  (if (contains? m k) (assoc m k "[REDACTED]") m))

(defn redact-transport
  "Redacted DIAGNOSTIC view of a transport config: every :env / :headers
   value (keyword or string key) is replaced by the literal
   \"[REDACTED]\"; all other fields pass through unchanged.

   SCOPE (WO-M2 / INV-01): error data, audit records, and descriptor
   serialization boundaries ONLY. Never feed this output into
   evoclj.mcp.client/open! (execution would lose env vars and auth
   headers) and never derive pool identity from it (configs differing
   only in secret values would collapse onto one key)."
  [cfg]
  (let [cfg (or cfg {})]
    (-> cfg
        (redact-subtree :env)
        (redact-subtree :headers)
        (redact-subtree "env")
        (redact-subtree "headers"))))

;; --- pool identity (stable fingerprints) -------------------------------------

(defn- canonical-edn
  "Order-stable transform of EDN data: maps become sorted maps (keys
   ordered by their printed form), sets become vectors sorted by printed
   element form, seqs become vectors; scalars pass through. Equal
   contents therefore always print identically, regardless of map
   construction order — the ordering guarantee the fingerprints below
   depend on."
  [v]
  (cond
    (map? v) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                   (map (fn [[k x]] [(canonical-edn k) (canonical-edn x)]) v))
    (set? v) (vec (sort-by pr-str (map canonical-edn (seq v))))
    (seq? v) (mapv canonical-edn v)
    :else v))

(defn- stable-digest
  "\"sha256:<64 lowercase hex>\" over the UTF-8 bytes of the canonical
   EDN printing of `v` (evoclj.genome.hash style). Deterministic across
   processes and JVM restarts; not reversible for practical purposes."
  [v]
  (let [ba (.getBytes (pr-str (canonical-edn v)) StandardCharsets/UTF_8)
        ^bytes digest (.digest (MessageDigest/getInstance "SHA-256") ba)]
    (str "sha256:" (apply str (map #(format "%02x" %) digest)))))

(defn credential-fingerprint
  "Stable \"sha256:<hex>\" fingerprint of the config's :auth/ref value.

   NOT a security control: the input is a low-entropy reference string,
   so this is trivially brute-forceable. Used ONLY to separate pooled
   connections whose credential references differ — i.e. connection
   grouping inside the in-memory pool key."
  [cfg]
  (stable-digest (:auth/ref (or cfg {}))))

(defn transport-identity
  "The identity-bearing projection of a transport config for the pool
   key: non-secret fields verbatim, each present secret field
   (:env/:headers, keyword or string key) replaced by its per-field
   stable sha256 fingerprint. :auth/ref is excluded (it lives in the
   cf slot of the key via credential-fingerprint).

   Guarantees (INV-01): identical configs -> identical identities;
   configs differing in ANY secret VALUE -> different identities;
   missing and empty secret maps remain distinguishable."
  [cfg]
  (let [cfg (or cfg {})]
    (reduce (fn [acc k]
              (if (contains? cfg k)
                (assoc acc k (stable-digest (get cfg k)))
                acc))
            (dissoc cfg :auth/ref)
            [:env "env" :headers "headers"])))

(defn connection-key
  "Pool key [type cid ti cf]: transport type, connection id,
   transport-identity, credential-fingerprint.

   SHAPE UNCHANGED from the pre-M2 manager. The KEY FORMAT changed
   (ti now carries per-field sha256 fingerprints instead of whole-value
   \"[REDACTED]\" placeholders; cf is now a sha256 digest instead of a
   Clojure hash) — safe with NO migration because the pool is purely
   in-memory state: entries die with the process and every fresh key
   simply starts a fresh entry."
  [cfg]
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
