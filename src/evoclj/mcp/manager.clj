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
   hashing). Fingerprints are stable across JVM restarts.

   WO-M3 (healing loop): the pool is also a small failure-recovery state
   machine — see get-or-open! for the documented machine. Shared-path
   call sites (mcp-bridge / mcp.source) report transport-family failures
   via mark-broken and successes via mark-ok; get-or-open! refuses to
   hand out :broken entries (it heals them through :reconnecting) and
   refuses to hammer a persistently dead endpoint (:cooldown gate)."
  (:require [clojure.walk :as walk]
            [evoclj.kernel.error :as err]
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

;; --- manager state ------------------------------------------------------------

(def ^:private default-max-reopen-failures
  "Consecutive failed open/reopen attempts after which a key enters
   :cooldown (WO-M3, configurable)."
  3)

(def ^:private default-cooldown-ms
  "How long a :cooldown gate stays closed before the next get-or-open!
   may retry (WO-M3, configurable; tests inject :now-fn instead of
   waiting this out)."
  5000)

(defn create-manager
  "Create a fresh manager atom.

   Optional opts:
     :max-reopen-failures  consecutive open/reopen failures that arm the
                           :cooldown gate (default 3)
     :cooldown-ms          duration of the :cooldown window (default 5000)
     :now-fn               zero-arg millisecond clock; defaults to
                           System/currentTimeMillis (tests inject a
                           controllable clock)"
  ([] (create-manager {}))
  ([{:keys [max-reopen-failures cooldown-ms now-fn]
     :or {max-reopen-failures default-max-reopen-failures
          cooldown-ms default-cooldown-ms}}]
   (atom {:pools {}
          :opts {:max-reopen-failures max-reopen-failures
                 :cooldown-ms cooldown-ms
                 :now-fn (or now-fn System/currentTimeMillis)}})))

(defn- now-ms
  "Manager-configured wall clock read INSIDE a swap! (the fn is plain
   data in :opts, keeping swap! side-effect free)."
  [s]
  ((get-in s [:opts :now-fn])))

(defn pool-snapshot
  "Point-in-time READ-ONLY projection of the pool for tests and
   diagnostics (WO-M3). Per key it exposes only healing bookkeeping —
   {:state :generation :fail-count :health} plus :metrics/:owners/
   :cooldown-until/:has-client? when present. The live managed client
   record and the single-flight :promise are deliberately EXCLUDED so
   assertion messages never print opaque or pending objects. Shape:

     {:pools {k {...}} :opts {:max-reopen-failures n :cooldown-ms n}}"
  [mgr-atom]
  (let [{:keys [pools opts]} @mgr-atom]
    {:pools (into {}
                  (map (fn [[k e]]
                         [k (cond-> {:state (:state e)
                                     :generation (:generation e)
                                     :fail-count (:fail-count e)
                                     :health (:health e)}
                              (:metrics e)        (assoc :metrics (:metrics e))
                              (:owners e)         (assoc :owners (:owners e))
                              (:cooldown-until e) (assoc :cooldown-until (:cooldown-until e))
                              (some? (:client e)) (assoc :has-client? true))]))
                  pools)
     :opts (dissoc opts :now-fn)}))

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
                        :generation gen :fail-count 0
                        :metrics (or (:metrics e) {:call-count 0})
                        :transport-identity (transport-identity (:transport-config managed))
                        :credential-identity (credential-fingerprint (:transport-config managed))
                        :health {:last-ok (now-ms s)}})))))

(defn mark-broken
  "Report a CONNECTION-LEVEL failure against pool key `k`: demote a LIVE
   (:ready) entry to :broken with health {:last-error err-data} and
   STRIP the dead :client record.

   Stripping :client is load-bearing (WO-M3 \"broken 不发放\"): the
   bridge/source shared paths short-circuit on a present :client before
   ever reaching get-or-open!, so a lingering zombie record would be
   handed out forever and the entry would never heal. With it gone, the
   next caller's presence check falls through to get-or-open!, which
   owns the broken->reconnecting transition and the cooldown gate.
   (The stripped record is intentionally NOT closed here: closing is a
   side effect and must never run inside a swap!; lifecycle ownership
   of superseded clients belongs to M4/M5.)

   WO-M3 gating: only a :ready entry can be demoted. Reporters fire from
   shared-path call failures; a report arriving against an entry already
   being healed (:broken/:reconnecting/:cooldown) must never downgrade
   an in-flight attempt — a zombie call's late failure would otherwise
   corrupt the cooldown arithmetic — and marking an absent key is a
   no-op. Generation is PRESERVED: it counts open attempts and is
   incremented only by get-or-open! transitions.

   `err-data` must already be display-safe: the production call sites
   pass broken-err-data (sanitized + transport-redacted, INV-01)."
  [mgr-atom k err-data]
  (swap! mgr-atom
         (fn [s]
           (if (= :ready (get-in s [:pools k :state]))
             (-> s
                 (update-in [:pools k] merge {:state :broken
                                              :health {:last-error err-data}})
                 (update-in [:pools k] dissoc :client))
             s))))

(defn mark-ok
  "Refresh pool key `k`'s health to {:last-ok <now>} after a SUCCESSFUL
   shared-path call (WO-M3 counterpart of mark-broken). Replaces any
   prior :last-error — the connection just demonstrably worked. No-op
   when the key has no entry (non-pooled callers never report)."
  [mgr-atom k]
  (swap! mgr-atom
         (fn [s]
           (if (contains? (:pools s) k)
             (assoc-in s [:pools k :health] {:last-ok (now-ms s)})
             s))))

(defn set-metrics [mgr-atom k f]
  (swap! mgr-atom update-in [:pools k :metrics] (fn [m] (f (or m {:call-count 0})))))

;; --- failure reporting payload (INV-01 display safety) ------------------------

(def ^:private transport-family-types
  "Stable error types that mean the CONNECTION (not the tool, not the
   input) failed."
  #{:mcp/transport-error :mcp/protocol-error})

(def ^:private transport-family-classes
  "Java exception classes treated as connection-level evidence. KEPT IN
   LOCKSTEP with evoclj.mcp.client's private known-transport-classes /
   known-protocol-classes until M7 (error classification v2) unifies
   the two lists."
  #{"java.io.IOException"
    "java.net.SocketException"
    "java.net.ConnectException"
    "java.net.SocketTimeoutException"
    "java.util.concurrent.TimeoutException"
    "com.fasterxml.jackson.core.JsonParseException"
    "com.fasterxml.jackson.databind.JsonMappingException"})

(defn- gather-signatures!
  "Accumulate every [:type kw] / [:class str] signature reachable in
   bounded sanitized error data into volatile set `acc` (recurses
   through maps/vectors/seqs; sanitize already bounded depth and size)."
  [v acc]
  (cond
    (map? v)
    (do (when (keyword? (:error/type v)) (vswap! acc conj [:type (:error/type v)]))
        (when (string? (:error/class v)) (vswap! acc conj [:class (:error/class v)]))
        (doseq [[_ x] v] (gather-signatures! x acc)))
    (or (vector? v) (seq? v)) (run! #(gather-signatures! % acc) v)
    :else nil))

(defn broken-worthy?
  "True when Throwable `ex` is a connection-level failure that warrants
   mark-broken on a pooled entry (WO-M3 failure reporting).

   Two admission routes:
     1. evoclj.mcp.client/classify-mcp-error assigns the direct stable
        family type (:mcp/transport-error / :mcp/protocol-error); or
     2. the sanitized cause/data tree carries transport-family EVIDENCE.

   Route 2 exists because WO-T1 wire-verified that crash/malformed
   fake-server failures surface as the SDK requestTimeout
   (java.util.concurrent.TimeoutException) WRAPPED in the stable
   :mcp/call-tool-failed type — and the classifier short-circuits on
   that wrapper's stable type before its cause-chain scan. Unwrapping
   belongs to M7; until then the evidence walk keeps crash-style deaths
   on the healing path."
  [ex]
  (when (instance? Throwable ex)
    (let [acc (volatile! #{})]
      (gather-signatures! (err/sanitize ex) acc)
      (boolean
       (or (contains? transport-family-types
                      (:error/type (mcp-client/classify-mcp-error ex)))
           (some (fn [[tag v]]
                   (case tag
                     :type (contains? transport-family-types v)
                     :class (contains? transport-family-classes v)))
                 @acc))))))

(defn- redact-embedded-transports
  "Replace EVERY :mcp/transport-config value anywhere inside sanitized
   error data with its redact-transport diagnostic form (INV-01): error
   payloads from lower layers may embed the transport config under keys
   err/sanitize does not know about (BT9/M-hygiene tracks the general
   gap); the healing payload must not become a leak amplifier."
  [ed]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (contains? x :mcp/transport-config))
       (assoc x :mcp/transport-config (redact-transport (:mcp/transport-config x)))
       x))
   ed))

(defn broken-err-data
  "Display-safe diagnostic payload for mark-broken (INV-01): the fully
   sanitized error data of `ex`, with every embedded transport config
   swapped for its redacted form and — when the caller's own
   `transport-config` is supplied — a top-level :mcp/transport-config
   carrying the redacted diagnostic form of it."
  ([ex]
   (redact-embedded-transports (err/sanitize ex)))
  ([ex transport-config]
   (cond-> (broken-err-data ex)
     transport-config (assoc :mcp/transport-config (redact-transport transport-config)))))

;; single-flight: absent -> connecting -> ready/broken;
;;               broken -> reconnecting -> ready/broken/cooldown
(defn get-or-open!
  "Return the managed client record for pool key `k`, opening it via
   `open-fn` (zero-arg; returns a managed record shaped like
   evoclj.mcp.client/open!'s) when no live entry exists.

   UNIFIED RETURN CONTRACT (WO-M1): every return path yields the SAME
   KIND of value — the managed record itself, never the raw underlying
   client, never nil:

   - ready hit     -> the managed record stored in the pool entry;
   - first opener /
     reopener      -> open-fn's return value verbatim; it is stored as
                      the entry's :client and delivered to concurrent
                      waiters;
   - concurrent
     waiter        -> blocks on the single-flight promise, then returns
                      that same managed record; if the opener failed the
                      waiter THROWS the opener's Throwable (a Throwable
                      is never returned as a value).

   STATE MACHINE (WO-M3 — keep in lockstep with the implementation; h7
   of evoclj.mcp.manager-healing-test drives every state below through
   pool-snapshot):

     absent      --open attempt--> :connecting  (generation++)
     :ready      --mark-broken (connection-level call failure)--> :broken
     :connecting / :reconnecting --> concurrent callers park on the
                                    entry's single-flight promise
     :broken     --next get-or-open!--> :reconnecting (generation++)
                 --ok----> :ready  (:fail-count reset, health.last-ok)
                 --fail--> :broken (:fail-count incremented) - and once
                           :fail-count >= max-reopen-failures, straight
                           to :cooldown with cooldown-until = now +
                           cooldown-ms
     :cooldown   --window open----> THROWS typed :mcp/cooldown carrying
                                    {:last-error :retry-in-ms}; open-fn
                                    is NOT invoked
                 --window expired--> :reconnecting (generation++)

   mark-broken preserves :generation (it counts ATTEMPTS; one bump per
   open/reopen transition). A successful open resets :fail-count.
   Options live on the manager atom (see create-manager): configurable
   consecutive-failure threshold, window length, and clock.

   On open failure the entry is left :broken (or :cooldown) with its
   :promise cleared so a later call starts a fresh attempt (the opener
   itself rethrows)."
  [mgr-atom k open-fn]
  (let [slot (atom nil)]
    (swap! mgr-atom
           (fn [s]
             (let [e (get-in s [:pools k])
                   st (:state e)]
               (cond
                 (and e (= :ready st))
                 (do (reset! slot {:hit e}) s)

                 ;; WO-M3: :reconnecting is a REAL state — same
                 ;; single-flight wait discipline as a first :connecting
                 ;; attempt (M1 contract: all take the managed value or
                 ;; all throw)
                 (and e (contains? #{:connecting :reconnecting} st))
                 (do (reset! slot {:promise (:promise e)}) s)

                 ;; WO-M3: an armed, unexpired cooldown refuses WITHOUT
                 ;; touching open-fn
                 (and e (= :cooldown st) (< (now-ms s) (:cooldown-until e)))
                 (do (reset! slot {:cooldown {:last-error (get-in e [:health :last-error])
                                              :retry-in-ms (- (:cooldown-until e)
                                                              (now-ms s))}})
                     s)

                 :else
                 ;; fresh open (absent key) OR healing reopen (:broken, or
                 ;; an EXPIRED :cooldown): atomically claim the single-
                 ;; flight slot. broken->reopen IS the :reconnecting
                 ;; transition and bumps the generation once per attempt.
                 (let [reopening? (some? e)
                       p (promise)]
                   (reset! slot {:promise p :new? true})
                   (cond-> (-> s
                               (assoc-in [:pools k :state]
                                         (if reopening? :reconnecting :connecting))
                               (assoc-in [:pools k :promise] p)
                               (assoc-in [:pools k :generation]
                                         (inc (or (:generation e) 0))))
                     reopening? (update-in [:pools k] dissoc :cooldown-until)))))))
    (let [{:keys [hit promise new? cooldown]} @slot]
      (cond
        hit (:client hit)

        cooldown
        (throw (err/error :mcp/cooldown
                          "MCP connection reopen is in failure cooldown"
                          {:pool-key k
                           :last-error (:last-error cooldown)
                           :retry-in-ms (:retry-in-ms cooldown)}))

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
                                          (assoc-in [:pools k :health] {:last-ok (now-ms s)})
                                          (assoc-in [:pools k :fail-count] 0)
                                          (update-in [:pools k] dissoc :promise :cooldown-until))))
              (deliver p managed)
              managed)
            (catch Throwable ex
              (let [ed (err/sanitize ex)]
                (swap! mgr-atom
                       (fn [s]
                         (let [fc (inc (or (get-in s [:pools k :fail-count]) 0))
                               base (-> s
                                        (assoc-in [:pools k :state] :broken)
                                        (assoc-in [:pools k :health] {:last-error ed})
                                        (assoc-in [:pools k :fail-count] fc)
                                        (update-in [:pools k] dissoc :promise))]
                           (if (>= fc (get-in s [:opts :max-reopen-failures]))
                             (-> base
                                 (assoc-in [:pools k :state] :cooldown)
                                 (assoc-in [:pools k :cooldown-until]
                                           (+ (now-ms s)
                                              (get-in s [:opts :cooldown-ms]))))
                             base))))
                (deliver p ex)
                (throw ex)))))))))

(defn shutdown! [mgr-atom]
  (doseq [[_ e] (:pools @mgr-atom)]
    (when-let [c (:client e)] (try (mcp-client/close! c) (catch Throwable _ nil))))
  ;; preserve :opts — the configured clock/thresholds survive a pool reset
  (reset! mgr-atom {:pools {} :opts (:opts @mgr-atom)}))

(defmethod ig/init-key :mcp/manager [_ _] (create-manager))
(defmethod ig/halt-key! :mcp/manager [_ mgr] (shutdown! mgr))

;; # ponytail: global-lock ceiling — per-key locking would reduce contention but single atom swap! is sufficient for current scale
