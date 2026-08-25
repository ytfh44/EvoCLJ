(ns evoclj.mcp.source
  "McpSource - LiveSource for MCP dynamic environment (Phase 4).

  Uses McpManager for connection pool / reconnect / credentials / transport / shutdown,
  ProtocolAdapter via mcp-bridge (discovery + descriptor conversion), and
  on tools/list_changed only calls invalidate (mark dirty + registry refresh),
  never mutates registry directly.

  Snapshot payload is a pure, stable EDN map:
  Snapshot payload is a pure, stable EDN map. Each tool's :tool/id is the
  deterministic composite [server-id remote-name] tuple (M12), never the
  collision-prone bare keyword :mcp/<name>:
    {:tools {[\"server-a\" \"read_file\"] {:tool/id [\"server-a\" \"read_file\"] ...}} ...}}
  from the Revision, not stored in the payload, so ToolEntry immutability
  is preserved and each Revision's ToolEntries share the same manager
  connection."
  (:require [clojure.string :as str]
            [evoclj.environment.source :as src]
            [evoclj.kernel.error :as err]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.mcp.codec :as codec]
            [evoclj.mcp.json-schema :as json-schema]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.adapter :as adapter]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; helpers - deterministic composite tool-id (M12)
;;
;; Each MCP tool's LOCAL tool-id is a STABLE COMPOSITE of
;; [server-id, remote-name]. This guarantees tools from different servers
;; never alias, even when they share a remote name.
;;
;; Remote names are sanitized INJECTIVELY (single-valued): distinct remote
;; names ALWAYS map to distinct sanitized names, so the local id can never
;; silently merge two distinct remote tools. The sanitizer is a bijection on
;; strings (percent-encode the three unsafe chars % / space), so it has an
;; exact inverse and cannot produce a collision.
;;
;; If two DISTINCT remote tools STILL resolve to the same local tool-id,
;; discovery fails CLOSED with a typed :mcp/tool-id-collision error. We
;; never silently overwrite one tool with the other.
;; ---------------------------------------------------------------------------

(defn- sanitize-remote-name
  "INJECTIVE (single-valued) sanitization of an MCP remote tool name.

   Distinct remote names map to DISTINCT sanitized names, so the local
   id never silently collapses two distinct remote tools. Implemented as
   a BIJECTION: the three unsafe characters (%, space, /) are
   percent-encoded with a fixed, non-overlapping encoding, and the
   encoding of '%' guarantees the mapping stays one-to-one. The result is
   therefore fully reversible (no information lost, no two inputs merge)."
  [remote-name]
  (let [s (str remote-name)]
    ;; str/escape performs a single left-to-right pass: each matched char
    ;; is replaced by its literal value and the replacement is NOT re-scanned,
    ;; so '%' -> "%25" cannot itself re-trigger. Because no replacement
    ;; string shares a leading char with another key and every original
    ;; char maps to a unique output, the overall map is injective.
    (str/escape s {\% "%25" \space "%20" \/ "%2F"})))

(defn- composite-tool-id
  "Deterministic composite LOCAL tool-id for an MCP tool.

   Returns the stable tuple [server-id sanitized-remote-name]. The
   server-id and the (injectively sanitized) remote name are kept as two
   SEPARATE components so a tool named the same on two servers yields two
   distinct tuples and never aliases."
  [server-id remote-name]
  (let [sid (or server-id "unknown")
        sn (sanitize-remote-name remote-name)]
    [sid sn]))

(defn- detect-collisions!
  "Fail-closed collision guard (M12). Given the discovered raw tool maps
   and the computed local tool-id for each, throw :mcp/tool-id-collision
   when two DISTINCT remote tools resolve to the SAME local tool-id.

   `id-for` is the production function [server-id remote-name] -> local
   id, applied with the per-tool server-id. Returns the seq of
   [local-id raw-tool] pairs on success."
  [tools server-id id-for]
  (let [pairs (mapv (fn [t]
                      (let [rn (:mcp/name t)]
                        [(id-for server-id rn) t]))
                    tools)
        by-id (group-by first pairs)]
    (doseq [[lid group] by-id]
      ;; >1 DISTINCT tool entry sharing one local id => silent merge risk
      (when (> (count group) 1)
        (throw (err/error :mcp/tool-id-collision
                          "distinct MCP remote tools resolved to the same local tool-id; refusing to silently merge"
                          {:mcp/local-id (err/sanitize lid)
                           :mcp/server-id (err/sanitize server-id)
           :mcp/collisions (err/sanitize (distinct (map #(:mcp/name (second %)) group)))}))))
    pairs))

;; ---------------------------------------------------------------------------
;; descriptor conversion (stable, no volatile timestamps)
;; ---------------------------------------------------------------------------

(defn- stable-descriptor
  "Convert a raw MCP tool map (from mcp-client/list-all-tools, string-keyed
   JSON schema) into a stable tool descriptor for payload hashing.

   Volatile fields :mcp/generation, :mcp/captured-at, :mcp/last-refreshed
   are NOT stored in the payload - they are derived from the Revision
   when a ToolSurface is materialized, so identical tool sets stay
   identical revisions.

   FAIL-CLOSED (WO-M11 / INV-05 / INV-09): a tool MUST declare a real
   input and output schema. A missing or empty schema is NOT silently
   accepted as `:any` (that was a fail-open loophole). When the declared
   JSON schema cannot be expressed as a Malli primitive, the converter
   preserves it as a native-validated schema (never `:any`). When no
   schema is declared at all, this throws :mcp/schema-required so the
   discovery path cannot register a schema-less (wildcard) tool."
  [mcp-tool opts]
  (let [mcp-name (:mcp/name mcp-tool)
         server-id (:mcp/server-id opts)
         ;; M12 (fail-closed): every discovered tool MUST carry a server-id.
         _ (when (nil? server-id)
             (throw (err/error :mcp/config-invalid
                               "MCP tool requires :mcp/server-id to form a stable composite tool-id"
                               {:tool (err/sanitize mcp-name)})))
         tool-id (composite-tool-id server-id mcp-name)
        input-json (:mcp/input-schema mcp-tool)
        output-json (:mcp/output-schema mcp-tool)
        ;; Real schema declared? (non-nil, non-empty map). Missing/empty is
        ;; a fail-closed condition, not a `:any` default.
        input-present? (and (map? input-json) (seq input-json))
        output-present? (and (map? output-json) (seq output-json))
        _ (when-not input-present?
            (throw (err/error :mcp/schema-required
                              "MCP tool declares no input schema; schema-less tools are not allowed (fail-closed)"
                              {:tool (err/sanitize mcp-name)
                               :input-schema (err/sanitize input-json)})))
        _ (when-not output-present?
            (throw (err/error :mcp/schema-required
                              "MCP tool declares no output schema; schema-less tools are not allowed (fail-closed)"
                              {:tool (err/sanitize mcp-name)
                               :output-schema (err/sanitize output-json)})))
        ;; Single codec implementation (INV-05). json-schema->malli returns a
        ;; REAL schema here because the JSON is present; it never yields :any
        ;; for a non-empty schema.
        input-malli (codec/json-schema->malli input-json)
        output-malli (codec/json-schema->malli output-json)]
    (cond-> {:tool/id tool-id
             :effect :remote
             :input-schema input-malli
             :output-schema output-malli
             :required-action :invoke
             :version 1
             :mcp/name mcp-name
             :mcp/input-schema input-json
             :mcp/output-schema output-json}
      (:mcp/title mcp-tool) (assoc :mcp/title (:mcp/title mcp-tool))
      (:mcp/description mcp-tool) (assoc :mcp/description (:mcp/description mcp-tool))
      (:mcp/retry-safe? mcp-tool) (assoc :retry {:safe? true})
      (:mcp/server-id opts) (assoc :mcp/server-id (:mcp/server-id opts))
      (:connection/id opts) (assoc :mcp/connection-id (:connection/id opts))
      (contains? mcp-tool :mcp/output-schema-kind) (assoc :mcp/output-schema-kind (:mcp/output-schema-kind mcp-tool))
      (contains? mcp-tool :mcp/param-projections) (assoc :mcp/param-projections (:mcp/param-projections mcp-tool)))))

(defn- payload->sorted
  "Return a stable payload map with :tools sorted by tool-id string for
   deterministic hashing."
  [descriptors]
  (let [by-id (into {} (map (fn [d] [(:tool/id d) d]) descriptors))
        sorted (into (sorted-map) by-id)]
    {:tools sorted}))

;; ---------------------------------------------------------------------------
;; discovery
;; ---------------------------------------------------------------------------

(defn- close-owned!
  "WO-M4: best-effort close of a CALL-SCOPED (non-pooled) managed client
  record. evoclj.mcp.client/close! is already graceful and idempotent;
  the guard here only catches a Throwable escaping OUTSIDE that
  contract, reports it on stderr (swallowed failures must stay visible,
  not silent), and never masks the original call outcome.

  Mirror of evoclj.provider.mcp-bridge/close-owned! — keep in lockstep
  until M11."
  [managed]
  (when managed
    (try
      (mcp-client/close! managed)
      (catch Throwable t
        (try
          (binding [*out* *err*]
            (println "[evoclj.mcp.source] non-pooled client close failed:"
                     (pr-str (err/sanitize t))))
          ;; R2 (m1): the report path itself is error-proof — an Error
          ;; escaping this finally-position guard used to mask the real
          ;; outcome. Catch it too and fall back to one raw PrintStream
          ;; line (System/err cannot throw on IO failure); only what even
          ;; THAT throws is swallowed.
          (catch Throwable report-ex
            (try
              (.println System/err
                        (str "[evoclj.mcp.source] non-pooled client close-failure report also failed: "
                             (pr-str (err/sanitize report-ex))))
              (catch Throwable _ nil))))))))

(defn- discover-tools
  "Discover MCP tools. If :discover-fn is supplied in opts/Source, call it
   (for tests / stubbing). Otherwise use the live MCP client: through the
   manager's POOLED connection when a manager/connection-key is available
   (manager-owned lifetime, M1/M3 healing semantics), otherwise through a
   CALL-SCOPED client that this function opens and guarantees to CLOSE
   before returning or throwing — success and failure alike (WO-M4), so
   non-pooled discovery leaks no stdio subprocess. Returns a vector of
   stable descriptors.

   M16 wiring (ProtocolAdapter): discovery is routed through the negotiated
   ProtocolAdapter. The version is negotiated from `:mcp/version` in opts
   (default `:mcp-2025-11`, preserving the pre-change behavior). Adapter
   selection is FAIL-CLOSED: an unsupported/unimplemented version (e.g. a
   2026 surface) throws a typed `:mcp/unsupported` error before any client
   is opened. The 2025 adapter performs the SAME raw listTools call and the
   SAME schema normalization the pre-change path used, so the internal tool
   model is byte-identical; the only additive difference is the
   `:adapter/version` stamp each discovered descriptor receives (via
   `wire-request`), which is what lets the kernel later branch on protocol
   version without re-coupling the wire model."
  [source]
  (let [{:keys [transport-config manager discover-fn opts]} source
        ck (when (and manager transport-config)
             (manager/connection-key (assoc transport-config :connection/id (:connection/id opts))))
        ;; M16: negotiate the version and select the adapter fail-closed.
        ;; Throws :mcp/unsupported for any 2026/unimplemented/malformed
        ;; version BEFORE touching a client (fail-closed, no silent fallback).
        version (or (:mcp/version opts) adapter/default-version)
        _ (adapter/select-adapter version ck)
        a (adapter/adapter-for-connection ck version)
        stamp (fn [d] (merge d (select-keys (adapter/wire-request a {:tool/id (:tool/id d)}) [:adapter/version])))]
    (if discover-fn
      ;; test stub path - discover-fn returns raw MCP tool maps or stable descriptors
      (let [raw (discover-fn)]
        ;; if raw already looks like stable descriptors (has :tool/id), use as-is
        (if (and (seq raw) (:tool/id (first raw)))
          (mapv stamp raw)
          (mapv (comp stamp #(stable-descriptor % opts)) raw)))
      ;; live path
      (let [tools-change-cb (get source :tools-change-cb)
            open-fn (fn []
                      ;; WO-M2 / INV-01: real config into open! (execution input)
                      (mcp-client/open! transport-config
                                        (fn [] (when tools-change-cb (tools-change-cb)))
                                        nil))]
        (if ck
          ;; pooled: the manager owns the client's lifetime; never closed here
          (let [client (:client (manager/get-or-open! manager ck open-fn))]
            (when-not client
              (throw (err/error :mcp/discover-failed "no MCP client available" {:transport-config (err/sanitize transport-config)})))
            (let [raw-tools (adapter/discover a {:client client})]
              (mapv (comp stamp #(stable-descriptor % opts)) raw-tools)))
          ;; WO-M4: non-pooled discovery is call-scoped — the freshly opened
          ;; client is closed in finally whether listing succeeds or throws.
          (let [managed (open-fn)]
            (try
              (let [client (:client managed)]
                (when-not client
                  (throw (err/error :mcp/discover-failed "no MCP client available" {:transport-config (err/sanitize transport-config)})))
                (let [raw-tools (adapter/discover a {:client client})]
                  (mapv (comp stamp #(stable-descriptor % opts)) raw-tools)))
              (finally
                (close-owned! managed)))))))))

;; ---------------------------------------------------------------------------
;; ToolEntry - immutable Provider
;; ---------------------------------------------------------------------------

(defrecord ToolEntry [descriptor manager conn-key transport-config]
  proto/Provider
  (describe [_] descriptor)
  (normalize-request [_ intent]
    (let [payload (:payload intent)
          raw-args (:args payload)
          args (evoclj.mcp.canonical/value->canonical raw-args)]
      (when-not (boundary/edn-safe? raw-args)
        (throw (err/error :provider/input-invalid
                          "MCP provider input must be plain EDN-safe data"
                          {:value (err/sanitize raw-args)})))
      ;; The descriptor is guaranteed (fail-closed, stable-descriptor) to
      ;; carry a REAL input schema — never :any. Validate it directly via
      ;; the single Malli implementation; no reflection / ns-resolve.
      (let [schema (:input-schema descriptor)]
        (try
          (when-not (m/validate schema raw-args)
            (throw (err/error :provider/input-invalid
                              "MCP provider input failed input-schema validation"
                              {:value (err/sanitize raw-args)})))
          (catch Exception e
            (when (= :provider/input-invalid (:error/type (ex-data e))) (throw e)))))
      ;; native JSON-Schema validation of the declared input schema
      (when-let [js (:mcp/input-schema descriptor)]
        (when (and (map? js) (seq js))
          (try
            (when-not (json-schema/validate js args)
              (throw (err/error :provider/input-invalid
                                "MCP provider input failed JSON Schema validation"
                                {:value (err/sanitize args)})))
            (catch Exception e
              (when (= :provider/input-invalid (:error/type (ex-data e))) (throw e))
              ;; json-schema/validate throws on invalid; treat as input-invalid
              (throw (err/error :provider/input-invalid
                                "MCP provider input failed JSON Schema validation"
                                {:value (err/sanitize args) :cause (err/sanitize e)}))))))
      {:tool/id (:tool/id descriptor)
       :resource (canonical/canonical-resource descriptor args)
       :args args}))
  (execute-request! [_ authorized-request]
    (when-not (and (map? authorized-request)
                   (= (:tool/id descriptor) (:tool/id authorized-request)))
      (throw (err/error :provider/request-invalid
                        "MCP provider received a non-normalized request"
                        {:value (err/sanitize authorized-request)})))
    (let [args (:args authorized-request)
          mcp-name (or (:mcp/name descriptor) (name (:tool/id descriptor)))
          connection-id (:mcp/connection-id descriptor)
          server-id (:mcp/server-id descriptor)
          ;; WO-M3: hoisted so the failure-reporting catch below can see it
          shared? (and manager conn-key)
          ;; WO-M4: tracks the CALL-SCOPED client opened by this very call
          ;; on the non-shared path, so the finally below can close it no
          ;; matter how this scope exits. Shared records are pool-owned;
          ;; they are never closed here.
          owned (volatile! nil)]
      (try
        (let [opened (if shared?
                       (let [entry (manager/pool-get manager conn-key)
                             c (:client entry)]
                         ;; WO-M1: get-or-open! returns the managed record
                         ;; itself; use it directly, no re-wrapping.
                         ;; WO-M2: real config into open! (execution input)
                         (if c c
                           (manager/get-or-open!
                            manager conn-key
                            #(mcp-client/open! transport-config nil nil))))
                       ;; WO-M4 non-pooled fallback: this client belongs to
                       ;; THIS call only; opened here, closed in finally.
                       (let [fresh (mcp-client/open! transport-config nil nil)]
                         (vreset! owned fresh)
                         fresh))
              managed (mcp-client/ensure-open opened 2)
              ;; WO-M5 gap (c) — mirror of evoclj.provider.mcp-bridge:
              ;; ensure-open returns a DIFFERENT record when the pooled one
              ;; came back closed and had to be LOCALLY reopened. Offer it
              ;; back to the pool via adopt-client! (CAS against the stale
              ;; record); if adoption is refused — entry already healed,
              ;; stripped, or torn down — the record belongs to THIS call
              ;; and is tracked in `owned` for the finally close.
              locally-reopened? (not (identical? opened managed))
              adopted? (when (and shared? locally-reopened?)
                         (manager/adopt-client! manager conn-key opened managed))
              ;; Re-track after ensure-open: a just-opened record has
              ;; :closed? false, so ensure-open returns it unchanged and
              ;; cannot throw here — no leak window between the bindings.
              _ (vreset! owned
                         (cond
                           ;; call-scoped path: this call owns its client
                           (not shared?) managed
                           ;; local reopen the pool refused: orphaned product
                           (and locally-reopened? (not adopted?)) managed
                           ;; pooled hit or adopted reopen: pool owns it
                           :else nil))]
          (when (mcp-client/closed? managed)
            (throw (err/error :mcp/client-closed
                              "MCP managed client is closed"
                              {:open-count (or (:open-count managed) 0)})))
          (let [client (:client managed)
                raw-result (mcp-client/call-tool client mcp-name args)
                edn-result (codec/result->edn raw-result)]
            ;; structured output validation when present
            (let [sc (get-in edn-result [:value :mcp/structured-content])
                  out-schema (:mcp/output-schema descriptor)]
              (when (and (some? sc) (map? out-schema) (seq out-schema))
                (when-not (json-schema/validate out-schema sc)
                  (throw (err/error :provider/output-invalid "structuredContent failed mcp/output-schema" {:value (err/sanitize sc)})))))
            ;; provider output-schema validation. The descriptor carries a
            ;; REAL output schema (fail-closed), so validate it directly via
            ;; the single Malli implementation — no reflection / ns-resolve.
            (let [env (:value edn-result)
                  out (:output-schema descriptor)]
              (try
                (when-not (m/validate out env)
                  (throw (err/error :provider/output-invalid "envelope failed provider/output-schema" {:value (err/sanitize env)})))
                (catch Exception e
                  (when (= :provider/output-invalid (:error/type (ex-data e))) (throw e)))))
            (when (and manager conn-key)
              (try (manager/set-metrics manager conn-key #(-> % (update :call-count (fnil inc 0)) (assoc :latency-ms 0)))
                   (catch Exception _ nil)))
            ;; WO-M3 failure reporting, success side: refresh health.last-ok
            (when shared?
              (try (manager/mark-ok manager conn-key) (catch Exception _ nil)))
            (update edn-result :audit merge {:mcp/tool-name mcp-name
                                             :mcp/connection-id connection-id
                                             :mcp/server-id server-id})))
        (catch Throwable ex
          (if (= :mcp/tool-error (:error/type (ex-data ex)))
            (throw ex)
            (do
              ;; WO-M3 failure reporting, failure side: transport-family
              ;; failure on the shared pooled connection demotes the entry
              ;; so the next caller heals instead of reusing a dead client.
              ;; err-data is display-safe (sanitized + redacted, INV-01).
              (when (and shared? (manager/broken-worthy? ex))
                (try
                  (manager/mark-broken manager conn-key
                                       (manager/broken-err-data ex transport-config))
                  (catch Exception _ nil)))
              (let [category (:error/type (mcp-client/classify-mcp-error ex))]
                (if (mcp-client/transient-error-type? category)
                  (throw (err/error :provider/transient-error
                                    "MCP provider call-tool failed"
                                    {:tool-name mcp-name
                                     :mcp/connection-id connection-id
                                     :mcp/server-id server-id
                                     :mcp/transport-config (err/sanitize (manager/redact-transport transport-config))
                                     :cause (err/sanitize ex)}))
                  (throw (err/error :provider/execution-failed
                                    "MCP provider call-tool failed"
                                    {:tool-name mcp-name
                                     :mcp/connection-id connection-id
                                     :mcp/server-id server-id
                                     :mcp/transport-config (err/sanitize (manager/redact-transport transport-config))
                                     :cause (err/sanitize ex)})))))))
        (finally
          ;; WO-M4/WO-M5: success AND failure exits release the client THIS
          ;; call owns (call-scoped always; a refused-adoption local reopen
          ;; on the shared path). close! is idempotent/graceful,
          ;; close-owned! never masks the original outcome.
          (when @owned
            (close-owned! @owned)))))))

(defn make-tool-entry
  "Create an immutable ToolEntry from a stable descriptor and manager.
   Shares the same McpManager connection pool. With a nil manager the
   entry is CALL-SCOPED instead: every execute-request! opens a fresh
   client and closes it before returning or throwing (WO-M4)."
  [descriptor manager transport-config]
  (let [ck (when (and manager transport-config)
             (manager/connection-key (assoc transport-config :connection/id (:mcp/connection-id descriptor))))]
    (->ToolEntry descriptor manager ck transport-config)))

(defn tool-entries->surface
  "Derive a ToolSurface entries map from a Revision payload. Each entry is
   an immutable ToolEntry sharing the same manager connection. Generation
   and captured-at are stamped from the Revision."
  [payload manager transport-config revision]
  (let [tools (or (:tools payload) {})
        tools (if (map? tools) (vals tools) tools)
        gen (:revision/seq revision)
        cap (:captured-at revision)]
    (into {}
          (map (fn [d]
                 (let [enriched (-> d
                                    (assoc :mcp/generation (or gen 0))
                                    (assoc :mcp/captured-at (or cap (System/currentTimeMillis)))
                                    (assoc :mcp/last-refreshed (or cap (System/currentTimeMillis))))]
                   [(:tool/id enriched) (make-tool-entry enriched manager transport-config)]))
               tools))))

;; ---------------------------------------------------------------------------
;; McpSource
;; ---------------------------------------------------------------------------

(defrecord McpSource [source-id transport-config manager subs closed? opts discover-fn]
  src/LiveSource
  (snapshot! [this]
    (when @closed?
      (throw (err/error :mcp/source-closed "McpSource is closed" {:source/id source-id})))
    (let [descriptors (try
                        (discover-tools this)
                        ;; M16: preserve typed, fail-closed errors thrown inside
                        ;; discovery (e.g. :mcp/unsupported from adapter version
                        ;; selection) instead of masking them as a generic
                        ;; :mcp/discover-failed. Only genuinely untyped failures
                        ;; are wrapped as :mcp/discover-failed.
                        (catch Throwable e
                          (if (:error/type (ex-data e))
                            (throw e)
                            (throw (err/error :mcp/discover-failed
                                              "MCP discover failed"
                                              {:source/id source-id :cause (err/sanitize e)})))))
          ;; M12 (fail-closed): before the discovered tool set is accepted,
          ;; verify no two DISTINCT remote tools collapsed onto one local
          ;; composite id. detect-collisions! throws :mcp/tool-id-collision
          ;; on a genuine collision instead of silently dropping a tool.
          ;; M12 (fail-closed): before the discovered tool set is accepted,
          ;; verify no two DISTINCT remote tools collapsed onto one local
          ;; composite id. detect-collisions! throws :mcp/tool-id-collision
          ;; on a genuine collision instead of silently dropping a tool.
          _ (detect-collisions!
             (map (fn [d] {:mcp/name (:mcp/name d)}) descriptors)
             (:mcp/server-id opts)
             composite-tool-id)
          payload (payload->sorted descriptors)]
      {:source/id source-id
       :payload payload
       :captured-at (System/currentTimeMillis)}))
  (subscribe! [this invalidate-fn]
    (when @closed?
      (throw (err/error :mcp/source-closed "McpSource is closed" {:source/id source-id})))
    (let [id (random-uuid)
          close-fn (fn [] (swap! subs dissoc id))]
      (swap! subs assoc id invalidate-fn)
      ;; store a callback that will be invoked on tools/list_changed
      ;; the actual wiring happens inside discover-tools via get-or-open!
      {:subscription/id id :close! close-fn}))
  (close! [this]
    (when-not @closed?
      (reset! closed? true)
      (reset! subs {})
      ;; do not shutdown manager here - it is host-owned; just release if needed
      nil)
    nil))

(defn make-mcp-source
  "Create an McpSource LiveSource.

  Required opts:
    :source/id        - keyword or string identifying this source
    :transport-config - map accepted by evoclj.mcp.transport/transport-for

  Optional:
    :manager          - McpManager atom (defaults to a fresh manager)
    :discover-fn      - zero-arg fn returning a vector of raw MCP tool maps
                       (for tests / stubbing discovery without a live server)
    :connection/id    - shared connection id keyword
    :mcp/server-id    - server namespace string
  "
  [{:keys [source/id transport-config manager discover-fn] :as opts}]
  (when-not id
    (throw (err/error :mcp/config-invalid "McpSource requires :source/id" {:opts (err/sanitize opts)})))
  (when-not transport-config
    (throw (err/error :mcp/config-invalid "McpSource requires :transport-config" {:opts (err/sanitize opts)})))
  (let [mgr (or manager (manager/create-manager))
        subs (atom {})
        closed? (atom false)
        ;; tools-change callback that only calls invalidate, never mutates registry
        invalidate-all (fn []
                          (doseq [f (vals @subs)]
                            (try (f) (catch Exception _ nil))))]
    ;; we store the invalidate fn in a way that discover-tools can close over it
    ;; easiest: create source then assoc tools-change-cb via metadata
    (let [source (->McpSource id transport-config mgr subs closed? opts discover-fn)]
      ;; attach the callback via a separate atom field on the record's extra map
      ;; Clojure records allow assoc for extra fields if not defined? Use with-meta
      ;; Instead, store in a side atom that discover-tools reads
      ;; We use an atom inside source map via assoc
      (assoc source :tools-change-cb invalidate-all))))

(defn trigger-tools-changed!
  "Test helper: simulate a tools/list_changed notification, which should
   only call invalidate (mark dirty + trigger registry refresh), not
   directly mutate registry. Calls all subscribers' invalidate fns."
  [source]
  (when-let [cb (:tools-change-cb source)]
    (cb))
  ;; also directly call subs for sources created without :tools-change-cb assoc
  (doseq [f (vals @(:subs source))]
    (try (f) (catch Exception _ nil)))
  nil)
