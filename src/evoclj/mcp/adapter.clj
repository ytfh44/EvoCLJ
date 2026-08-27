(ns evoclj.mcp.adapter
  "ProtocolAdapter — the translation layer between MCP protocol versions
   and EvoCLJ's internal tool model.

   M16 wiring (closure-repair plan): the adapter is WIRE-ACTIVATED, not
   deleted. The 2025 surface (`:mcp-2025-11`) is the production default and
   MUST stay behaviorally identical to the pre-change discovery/normalize
   path — the 2025 adapter delegates discovery to the same
   `evoclj.mcp.client/list-all-tools` and the same schema normalizer the
   old path used, so the internal model is unchanged. The ONLY additive
   difference the 2025 adapter introduces is a protocol-version stamp
   (`:adapter/version`) on each discovered tool, which is what lets the
   kernel later branch on version without re-coupling the wire model.

   Fail-closed version selection (decision: 2026 unimplemented surface ->
   typed `:mcp/unsupported`): `select-adapter` accepts exactly the set of
   IMPLEMENTED versions. Every other version string — a 2026 surface we
   have not wired into production, an unknown keyword, or a malformed value
   — is rejected with a typed `:mcp/unsupported` error rather than silently
   falling back to 2025 or crashing on a half-built adapter. The
   `Adapter2026` record REMAINS DEFINED (it is not deleted) but is never
   handed out by `select-adapter`, so the structural placeholder survives
   while the runtime stays fail-closed.

   Selection is a PURE function of (ConnectionKey, version): the same
   conn-key + version always yields the same adapter decision, and the
   conn-key participates in the selection identity. No shared mutable state
   is introduced, so concurrent discovery calls cannot observe a torn
   selection."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as client]))

;; ---------------------------------------------------------------------------
;; version negotiation / fail-closed selection
;; ---------------------------------------------------------------------------

(def ^:const implemented-versions
  "The set of MCP protocol versions EvoCLJ actually wires into production.
   Anything outside this set is a 2026-or-later surface we have NOT
   implemented, an unknown keyword, or a malformed value — all rejected
   fail-closed with `:mcp/unsupported`. The 2025 surface is the only
   supported production version; 2026 stays a structural placeholder."
  #{:mcp-2025-11})

(def ^:const default-version
  "The negotiated version used when a source/bridge does not declare one.
   Pinned to 2025 so the pre-change behavior is preserved by default."
  :mcp-2025-11)

(defn unsupported-version?
  "True when `version` is NOT an implemented protocol version. A malformed
   value (nil, a string, a number, a 2026+ keyword, or anything else
   outside `implemented-versions`) is by definition not supported — fail-closed."
  [version]
  (not (contains? implemented-versions version)))

;; ---------------------------------------------------------------------------
;; the protocol
;; ---------------------------------------------------------------------------

(defprotocol ProtocolAdapter
  (discover [this ctx] "list+normalize tools")
  (wire-request [this contract] "enrich per-request _meta/headers/session")
  (on-notification [this event] "handle toolsChanged/progress/subscriptions")
  (cache-policy [this] "return {:ttl-ms :cache-scope} or nil")
  (continue [this task] "MRTR/Tasks continuation stub"))

(defrecord Adapter2025 [opts]
  ProtocolAdapter
  (discover [_ ctx]
    ;; The 2025 discovery is the SAME raw listTools call the pre-change path
    ;; used, so the internal model derived downstream (stable-descriptor) is
    ;; byte-identical. The adapter is purely the version-tagged seam.
    (client/list-all-tools (:client ctx)))
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

;; ---------------------------------------------------------------------------
;; version negotiation / fail-closed selection (defined after the adapter
;; records so the selection fns can construct them directly).
;; ---------------------------------------------------------------------------

(defn select-adapter
  "Pick the ProtocolAdapter for a negotiated `version`, fail-closed.

   Returns the adapter instance for an IMPLEMENTED version. For any
   unsupported version — a 2026 surface, an unknown keyword, or a malformed
   value — throws a typed `:mcp/unsupported` error that carries the
   offending `:mcp/version` and an `:error/reason :unsupported-surface`, so
   the failure is machine-classifiable and never a silent fallback or a
   crash on a half-built adapter.

   `connection-key` is accepted for selection-consistency: the decision is a
   pure function of (connection-key, version) — the same pair always yields
   the same result, and callers may assert that on a given conn-key a 2025
   version and a 2026 version select DIFFERENT outcomes. The conn-key does
   not change the 2025 answer (one adapter shape per version); it makes the
   selection observable as keyed rather than global."
  ([version]
   (select-adapter version nil))
  ([version connection-key]
   (when (unsupported-version? version)
      (throw (err/error :mcp/unsupported
                        (str "MCP protocol version is not implemented: "
                             (pr-str version))
                        {:mcp/version (err/sanitize version)
                         :connection-key (err/sanitize connection-key)
                         :error/reason :unsupported-surface
                         :supported implemented-versions})))
   ;; Only implemented versions reach here (unsupported-version? threw first).
   ;; The 2025 surface is the sole wired production version. Adapter2026 is
   ;; DEFINED (not deleted) but is never handed out, so the runtime stays
   ;; fail-closed for every 2026+ surface.
   (case version
     :mcp-2025-11 (adapter-2025))))

(defn adapter-for-connection
  "Production selection helper: given a `connection-key` (the pool identity
   from `evoclj.mcp.manager/connection-key`) and a negotiated `version`,
   return the adapter selected for that connection. Same (conn-key, version)
   pair always yields the same adapter decision — the selection is keyed on
   the connection identity, not merely on a global default."
  ([connection-key version]
   (select-adapter version connection-key))
  ([connection-key version opts]
   (select-adapter (or version default-version) connection-key)))
