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
   refuses to hammer a persistently dead endpoint (:cooldown gate).

   WO-M5 (refcount 收口): ownership becomes complete and honest —

     - OWNERS NEVER VANISH: acquire against an ABSENT entry registers a
       :pending-owners entry; every entry creation pre-seeds :owners from
       that registry, so an owner acquired before the first open survives.
     - TRANSITIONS PRESERVE HISTORY: installs merge into the existing
       entry (owners/generation/metrics), never rebuild it bare.
     - STALE RELEASES ARE INERT: releasing an owner that never registered
       mutates nothing — a late release can therefore never close a
       client installed after it fired (probe J).
     - NO OWNERLESS RESURRECTION: the last release during an in-flight
       open marks the entry :draining; the open outcome resolves it
       WITHOUT installing an unowned live client.
     - ZOMBIE HARVEST: dead clients stripped by mark-broken (and other
       superseded records) move to an out-of-pool tombstone queue (the
       :reaper agent) and are closed asynchronously — side effects never
       run inside a swap!. shutdown! flushes the queue synchronously."
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
          ;; WO-M5: owners registered while their entry does not exist yet
          :pending-owners {}
          ;; WO-M5 gap (a): out-of-pool tombstone queue. The agent's state
          ;; is the queue of zombie managed records awaiting async close;
          ;; actions run OFF the swap! path by construction.
          :reaper (agent [] :error-mode :continue)
          :opts {:max-reopen-failures max-reopen-failures
                 :cooldown-ms cooldown-ms
                 :now-fn (or now-fn System/currentTimeMillis)}})))

(defn- now-ms
  "Manager-configured wall clock read INSIDE a swap! (the fn is plain
   data in :opts, keeping swap! side-effect free)."
  [s]
  ((get-in s [:opts :now-fn])))

;; --- zombie client reaper (WO-M5 gap a) ----------------------------------------

(defn- reap-action!
  "Reaper agent action: close every queued tombstone record, then empty
   the queue. Runs on the reaper's thread — NEVER inside a swap!. close!
   is graceful and idempotent; a per-record failure is swallowed so one
   dying zombie cannot wedge the queue."
  [queue]
  (doseq [m queue]
    (try (mcp-client/close! m) (catch Throwable _ nil)))
  [])

(defn- enqueue-zombie!
  "Move `managed` onto the OUT-OF-POOL tombstone queue (:reaper agent)
   and schedule its asynchronous close. Called strictly AFTER the swap!
   that removed/superseded the record — the atom only ever carries plain
   data."
  [mgr-atom managed]
  (when managed
    (when-let [r (:reaper @mgr-atom)]
      (try
        (send-off r (fn [q] (reap-action! (conj (vec q) managed))))
        (catch Throwable _ nil)))))

(defn quiesce-reaper!
  "Test/diagnostic barrier: block until the reaper has serviced everything
   dispatched before this call. Agent actions run in global dispatch
   order, so harvests enqueued by other threads earlier are drained too;
   an enqueue racing this sentinel can still land after it — tests that
   need hard cross-thread guarantees poll process liveness instead.
   Returns the manager atom unchanged."
  [mgr-atom]
  (when-let [r (:reaper @mgr-atom)]
    (try
      (do (send-off r identity) (await r))
      (catch Throwable _ nil)))
  mgr-atom)

(defn pool-snapshot
  "Point-in-time READ-ONLY projection of the pool for tests and
   diagnostics (WO-M3). Per key it exposes only healing bookkeeping —
   {:state :generation :fail-count :health} plus :metrics/:owners/
   :cooldown-until/:has-client? when present. The live managed client
   record and the single-flight :promise are deliberately EXCLUDED so
   assertion messages never print opaque or pending objects. Shape:

     {:pools {k {...}}
       :pending-owners {k #{owner ...}}
       :opts {:max-reopen-failures n :cooldown-ms n}}"
  [mgr-atom]
  (let [{:keys [pools opts pending-owners]} @mgr-atom]
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
     :pending-owners (or pending-owners {})
     :opts (dissoc opts :now-fn)}))

(defn- entry-metrics [entry] (:metrics entry {:call-count 0 :latency-ms nil}))

;; single swap! operations
(defn pool-get [mgr-atom k]
  (get-in @mgr-atom [:pools k]))

(defn acquire
  "Register `owner-id` against pool key `k`; returns the CURRENT entry
   (nil while the entry is absent — contract unchanged).

   WO-M5: an owner arriving while NO entry exists is no longer dropped.
   It is registered in the manager's :pending-owners registry, and every
   entry creation (get-or-open! connect/reconnect transition, put-ready)
   pre-seeds its :owners from that registry in the same atomic swap, so
   an owner acquired before the first open still owns the connection."
  [mgr-atom k owner-id]
  (let [res (atom nil)]
    (swap! mgr-atom
           (fn [s]
             (if-let [e (get-in s [:pools k])]
               (do (reset! res e)
                   (update-in s [:pools k :owners] (fnil conj #{}) owner-id))
               (do (reset! res nil)
                   (update-in s [:pending-owners k] (fnil conj #{}) owner-id)))))
    @res))

(defn release
  "Remove `owner-id` from pool key `k`'s ownership (entry owners when the
   entry exists, the :pending-owners registry otherwise).

   WO-M5 refcount discipline:
   - A release naming an owner that never registered is TWO-TIER: inert
     while REGISTERED owners remain or an open attempt is in flight — a
     stale release can therefore never close a client installed after it
     fired, nor drain an attempt it knows nothing about (probe J) — but
     it LEGACY-SWEEPS a settled entry with NO registered owners (direct
     get-or-open!/put-ready callers, e.g. discover-tools, never acquire;
     their orphaned entries stay releasable exactly as before M5).
   - The LAST registered owner leaving tears the entry down. The pooled
     client record is closed SYNCHRONOUSLY AFTER the swap! completes
     (side effects never run inside swap!; close! is idempotent so even
     a swap! retry storm stays safe).
   - When the last owner leaves while an open attempt is IN FLIGHT (the
     entry carries a single-flight promise), teardown DEFERS to the open
     outcome: the entry is marked :draining and get-or-open!'s success /
     failure swaps resolve it — success hands the fresh record to the
     zombie reaper instead of installing an ownerless live entry; failure
     simply drops the husk."
  [mgr-atom k owner-id]
  (let [torn-down (atom nil)]
    (swap! mgr-atom
           (fn [s]
             (if-let [e (get-in s [:pools k])]
               (let [total (into (or (:owners e) #{})
                                 (get-in s [:pending-owners k] #{}))]
                 (if-not (contains? total owner-id)
                   ;; Unknown owner. Two tiers (WO-M5 + legacy cleanup):
                   ;; - owners remain OR an open attempt is in flight ->
                   ;;   strictly INERT: a stale release can never close a
                   ;;   client installed after it fired, never drain an
                   ;;   in-flight attempt it knows nothing about;
                   ;; - the entry is SETTLED and has NO registered owners
                   ;;   (direct get-or-open!/put-ready callers such as
                   ;;   discover-tools never acquire) -> LEGACY SWEEP:
                   ;;   tear down + close, as releases always did here.
                   (if (and (empty? total) (nil? (:promise e)))
                     (do (reset! torn-down (:client e))
                         (-> s
                             (update :pools dissoc k)
                             (update :pending-owners dissoc k)))
                     s)
                   (let [remaining (disj total owner-id)]
                     (if (seq remaining)
                       ;; owners remain: fold any defensive pending into owners
                       (-> s
                           (assoc-in [:pools k :owners] remaining)
                           (assoc-in [:pending-owners k] #{}))
                       (if (:promise e)
                         ;; last owner gone mid-open: defer to the outcome
                         (-> s
                             (assoc-in [:pools k :owners] #{})
                              (assoc-in [:pools k :draining] true)
                             (update :pending-owners dissoc k))
                         (do (reset! torn-down (:client e))
                             (-> s
                                 (update :pools dissoc k)
                                 (update :pending-owners dissoc k))))))))
               ;; no entry: maybe a PENDING owner to unregister
               (let [pend (get-in s [:pending-owners k] #{})]
                 (if (contains? pend owner-id)
                   (let [remaining (disj pend owner-id)]
                     (if (seq remaining)
                       (assoc-in s [:pending-owners k] remaining)
                       (update s :pending-owners dissoc k)))
                   ;; unknown owner, no entry: strict no-op
                   s)))))
    ;; post-swap side effect ONLY: best-effort close of the torn-down record
    (when-let [c @torn-down]
      (try (mcp-client/close! c) (catch Throwable _ nil)))
    nil))

(defn put-ready
  "Install `managed` as a live :ready entry for key `k`. Production code
   heals through get-or-open!; this seeds fixtures/tests (and hosts) with
   an already-open managed record.

   WO-M5: merges with any existing entry instead of replacing it blind —
   :generation counts this attempt (++), :owners/:metrics are preserved
   (pending owners fold in), health restarts at {:last-ok now}, and a
   SUPERSEDED :client record moves to the zombie reaper instead of
   leaking unowned. Returns the new entry."
  [mgr-atom k managed]
  (let [superseded (atom nil)]
    (swap! mgr-atom
           (fn [s]
             (let [e (get-in s [:pools k])
                   pending (get-in s [:pending-owners k] #{})]
               (reset! superseded (:client e))
               (assoc-in s [:pools k]
                         {:state :ready
                          :client managed
                          :owners (into (or (:owners e) #{}) pending)
                          :generation (inc (or (:generation e) 0))
                          :fail-count 0
                          :metrics (or (:metrics e) {:call-count 0})
                          :transport-identity (transport-identity (:transport-config managed))
                          :credential-identity (credential-fingerprint (:transport-config managed))
                          :health {:last-ok (now-ms s)}}))))
    (enqueue-zombie! mgr-atom @superseded)
    (get-in @mgr-atom [:pools k])))

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
   (WO-M5 gap (a): the stripped record is NOT closed inside this swap! —
   closing is a side effect — it moves to the OUT-OF-POOL tombstone queue
   (:reaper agent) right after the swap! and is closed asynchronously;
   shutdown! flushes whatever remains. Previously a stripped zombie's
   stdio child leaked until GC/JVM death.)

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
  (let [stripped (atom nil)]
    (swap! mgr-atom
           (fn [s]
             (if (= :ready (get-in s [:pools k :state]))
               (do (reset! stripped (get-in s [:pools k :client]))
                   (-> s
                       (update-in [:pools k] merge {:state :broken
                                                    :health {:last-error err-data}})
                       (update-in [:pools k] dissoc :client)))
               s)))
    ;; post-swap: dead record -> out-of-pool tombstone queue (async close)
    (enqueue-zombie! mgr-atom @stripped)))

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

(defn set-metrics
  "Apply `f` to key `k`'s :metrics (defaulting {:call-count 0}). WO-M5
   guard: a key torn down mid-call stays gone — metrics are never written
   onto an absent entry (which used to materialize a ghost {:metrics ...}
   husk with no state)."
  [mgr-atom k f]
  (swap! mgr-atom
         (fn [s]
           (if (contains? (:pools s) k)
             (update-in s [:pools k :metrics] (fn [m] (f (or m {:call-count 0}))))
             s))))

;; --- failure reporting payload (INV-01 display safety) ------------------------
;;
;; M7 unified the connection-level family with evoclj.mcp.client. The single
;; source of truth is mcp-client/transient-error-type?; the duplicate
;; class list and the sanitized-tree signature walker that used to live here
;; are deleted (INV-05 — single implementation principle). A failure is
;; healing-worthy iff the production classifier assigns it a transient family
;; type (:mcp/timeout / :mcp/transport-error / :mcp/protocol-error).

(defn broken-worthy?
  "True when Throwable `ex` is a connection-level failure that warrants
   mark-broken on a pooled entry (WO-M3 failure reporting).

   The verdict comes entirely from the production classifier
   (evoclj.mcp.client/classify-mcp-error): any transient family type —
   :mcp/timeout, :mcp/transport-error, or :mcp/protocol-error — is
   healing-worthy. WO-T1 wire-verified that crash/malformed fake-server
   deaths surface as the SDK requestTimeout (TimeoutException) wrapped in
   :mcp/call-tool-failed; M7's classifier now unwraps that wrapper and
   reports the timeout/transport evidence, so broken-worthy? needs no
   separate evidence walk."
  [ex]
  (when (instance? Throwable ex)
    (mcp-client/transient-error-type?
      (:error/type (mcp-client/classify-mcp-error ex)))))

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
   itself rethrows).

   WO-M5 OWNERSHIP RESOLUTION: the success/failure swaps mutate only an
   entry THIS attempt's promise still owns. When the last owner released
   mid-open (:draining), success hands the freshly opened record to the
   zombie reaper and DROPS the entry — no ownerless resurrection; failure
   likewise drops the husk. Owners registered while absent
   (:pending-owners) pre-seed the entry at creation and fold in at every
   successful install."
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
                 ;; WO-M5: owners registered while the entry was absent
                 ;; (:pending-owners) pre-seed / fold into the entry's
                 ;; owner set in THIS swap — an acquire can no longer
                 ;; vanish against the creation window.
                 (let [reopening? (some? e)
                       p (promise)]
                   (reset! slot {:promise p :new? true})
                   (cond-> (-> s
                               (assoc-in [:pools k :state]
                                         (if reopening? :reconnecting :connecting))
                               (assoc-in [:pools k :owners]
                                         (into (set (:owners e))
                                               (get-in s [:pending-owners k])))
                               (assoc-in [:pools k :promise] p)
                               (assoc-in [:pools k :generation]
                                         (inc (or (:generation e) 0)))
                               (update :pending-owners dissoc k))
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
            (let [managed (open-fn)
                  orphaned (atom nil)]
              ;; WO-M5: the success swap MERGES into the live entry instead
              ;; of rebuilding one bare, and installs ONLY while THIS
              ;; attempt still owns the single-flight slot. When the last
              ;; owner released mid-flight (:draining), or the entry
              ;; vanished / was replaced while we opened, the fresh record
              ;; is UNOWNED — it goes to the zombie reaper; it is never
              ;; resurrected as an ownerless :ready entry (probe J) nor
              ;; installed into a foreign lifecycle.
              (swap! mgr-atom
                     (fn [s]
                       (let [e (get-in s [:pools k])
                             mine? (and e (identical? p (:promise e)))]
                         (cond
                           (and mine? (not (:draining e)))
                           (-> s
                               (assoc-in [:pools k :state] :ready)
                               (assoc-in [:pools k :client] managed)
                               (assoc-in [:pools k :health] {:last-ok (now-ms s)})
                               (assoc-in [:pools k :fail-count] 0)
                               (update-in [:pools k :owners]
                                          (fn [o] (into (or o #{})
                                                        (get-in s [:pending-owners k]))))
                               (update-in [:pools k] dissoc :promise :cooldown-until)
                               (update-in [:pools k] dissoc :draining)
                               (update :pending-owners dissoc k))
                           mine?
                           ;; draining: nobody owns this outcome anymore
                           (do (reset! orphaned managed)
                               (-> s
                                   (update :pools dissoc k)
                                   (update :pending-owners dissoc k)))
                           :else
                           ;; entry gone (shutdown! or teardown won the race)
                           (do (reset! orphaned managed) s)))))
              (enqueue-zombie! mgr-atom @orphaned)
              (deliver p managed)
              managed)
            (catch Throwable ex
              (let [ed (err/sanitize ex)]
                (swap! mgr-atom
                       (fn [s]
                         (let [e (get-in s [:pools k])
                               mine? (and e (identical? p (:promise e)))]
                           (cond
                             ;; draining + failed open: drop the husk entirely
                             (and mine? (:draining e))
                             (-> s
                                 (update :pools dissoc k)
                                 (update :pending-owners dissoc k))
                             ;; WO-M5: only mutate an entry THIS attempt owns;
                             ;; a vanished/replaced entry stays untouched (a
                             ;; bare assoc-in used to recreate ghost husks)
                             mine?
                             (let [fc (inc (or (:fail-count e) 0))
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
                                 base))
                             :else s))))
                (deliver p ex)
                (throw ex)))))))))

(defn adopt-client!
  "WO-M5 gap (c): return a LOCALLY REOPENED managed record (the product of
   evoclj.mcp.client/ensure-open on the shared path) to the pool. CAS
   semantics: the record is installed only when the entry still holds
   EXACTLY the `stale` record the caller saw — identical? on the record,
   so a concurrent mark-broken strip, healing reopen, or teardown can
   never be clobbered. Returns true when adopted (the pool owns it again;
   callers must NOT close it); false otherwise (the caller owns it and
   must close it call-scoped). health.last-ok is refreshed; generation is
   untouched (it counts get-or-open!/put-ready attempts only)."
  [mgr-atom k stale managed]
  (let [adopted? (atom false)]
    (swap! mgr-atom
           (fn [s]
             (if (and (some? stale)
                      (identical? stale (get-in s [:pools k :client])))
               (do (reset! adopted? true)
                   (-> s
                       (assoc-in [:pools k :client] managed)
                       (assoc-in [:pools k :health] {:last-ok (now-ms s)})))
               s)))
    @adopted?))

(defn shutdown!
  "Close every pooled client and reset pool state. WO-M5: the zombie
   reaper queue is flushed SYNCHRONOUSLY first (await), so a stripped
   dead client never outlives shutdown; :opts AND the reaper agent
   survive so shutdown stays idempotent and the configured clock /
   thresholds keep working."
  [mgr-atom]
  (let [{:keys [pools reaper]} @mgr-atom]
    ;; flush queued tombstones FIRST: nothing outlives this call
    (when reaper
      (try
        (do (send-off reaper reap-action!) (await reaper))
        (catch Throwable _ nil)))
    (doseq [[_ e] pools]
      (when-let [c (:client e)] (try (mcp-client/close! c) (catch Throwable _ nil))))
    ;; preserve :opts + :reaper — configured clock/thresholds survive a reset
    (swap! mgr-atom assoc :pools {} :pending-owners {})
    mgr-atom))

(defmethod ig/init-key :mcp/manager [_ _] (create-manager))
(defmethod ig/halt-key! :mcp/manager [_ mgr] (shutdown! mgr))

;; # ponytail: global-lock ceiling — per-key locking would reduce contention but single atom swap! is sufficient for current scale
