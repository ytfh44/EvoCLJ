(ns evoclj.mcp.source
  "McpSource - LiveSource for MCP dynamic environment (Phase 4).

  Uses McpManager for connection pool / reconnect / credentials / transport / shutdown,
  ProtocolAdapter via mcp-bridge (discovery + descriptor conversion), and
  on tools/list_changed only calls invalidate (mark dirty + registry refresh),
  never mutates registry directly.

  Snapshot payload is a pure, stable EDN map:
    {:tools {\"mcp/read_file\" {:tool/id :mcp/read_file ...} ...}}

  Revision hashing uses pr-str of the sorted payload, so identical tool sets
  are identical revisions (no churn). Generation / captured-at are derived
  from the Revision, not stored in the payload, so ToolEntry immutability
  is preserved and each Revision's ToolEntries share the same manager
  connection."
  (:require [clojure.string :as str]
            [evoclj.environment.source :as src]
            [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.mcp.manager :as manager]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]))

;; ---------------------------------------------------------------------------
;; helpers - stable tool-id from MCP name
;; ---------------------------------------------------------------------------

(defn- mcp-name->tool-id
  "Stable keyword for an MCP tool name. `:mcp/<name>` with slashes and
   spaces sanitized to '-'. Uses string name as-is to stay reversible."
  [mcp-name]
  (let [n (-> (str mcp-name)
              (str/replace #"/" "-")
              (str/replace #"\s+" "-"))]
    (keyword "mcp" n)))

;; ---------------------------------------------------------------------------
;; descriptor conversion (stable, no volatile timestamps)
;; ---------------------------------------------------------------------------

(defn- stable-descriptor
  "Convert a raw MCP tool map (from mcp-client/list-all-tools, string-keyed
   JSON schema) into a stable tool descriptor for payload hashing.
   Volatile fields :mcp/generation, :mcp/captured-at, :mcp/last-refreshed
   are NOT stored in the payload - they are derived from the Revision
   when a ToolSurface is materialized, so identical tool sets stay
   identical revisions."
  [mcp-tool opts]
  (let [mcp-name (:mcp/name mcp-tool)
        tool-id (or (:tool/id opts) (mcp-name->tool-id mcp-name))
        input-json (:mcp/input-schema mcp-tool)
        output-json (:mcp/output-schema mcp-tool)
        ;; mcp-bridge json-schema->malli is private; resolve if present else :any
        malli-fn (try (deref (ns-resolve 'evoclj.provider.mcp-bridge 'json-schema->malli))
                      (catch Exception _ nil))
        input-malli (if (and malli-fn (map? input-json) (seq input-json))
                      (try (malli-fn input-json) (catch Exception _ :any))
                      :any)
        output-malli (if (and malli-fn (map? output-json) (seq output-json))
                       (try (malli-fn output-json) (catch Exception _ :any))
                       :any)]
    (cond-> {:tool/id tool-id
             :effect :remote
             :input-schema (if (= :any input-malli) :any input-malli)
             :output-schema (if (= :any output-malli) :any output-malli)
             :required-action :invoke
             :version 1
             :mcp/name mcp-name
             :mcp/input-schema (or input-json {})
             :mcp/output-schema (or output-json :any)}
      (:mcp/title mcp-tool) (assoc :mcp/title (:mcp/title mcp-tool))
      (:mcp/description mcp-tool) (assoc :mcp/description (:mcp/description mcp-tool))
      (:mcp/retry-safe? mcp-tool) (assoc :retry {:safe? true})
      (:mcp/server-id opts) (assoc :mcp/server-id (:mcp/server-id opts))
      (:connection/id opts) (assoc :mcp/connection-id (:connection/id opts))
      (contains? mcp-tool :mcp/output-schema-kind) (assoc :mcp/output-schema-kind (:mcp/output-schema-kind mcp-tool)))))

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
   stable descriptors."
  [source]
  (let [{:keys [transport-config manager discover-fn opts]} source
        ck (when (and manager transport-config)
             (manager/connection-key (assoc transport-config :connection/id (:connection/id opts))))]
    (if discover-fn
      ;; test stub path - discover-fn returns raw MCP tool maps or stable descriptors
      (let [raw (discover-fn)]
        ;; if raw already looks like stable descriptors (has :tool/id), use as-is
        (if (and (seq raw) (:tool/id (first raw)))
          raw
          (mapv #(stable-descriptor % opts) raw)))
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
            (let [raw-tools (mcp-client/list-all-tools client)]
              (mapv #(stable-descriptor % opts) raw-tools)))
          ;; WO-M4: non-pooled discovery is call-scoped — the freshly opened
          ;; client is closed in finally whether listing succeeds or throws.
          (let [managed (open-fn)]
            (try
              (let [client (:client managed)]
                (when-not client
                  (throw (err/error :mcp/discover-failed "no MCP client available" {:transport-config (err/sanitize transport-config)})))
                (let [raw-tools (mcp-client/list-all-tools client)]
                  (mapv #(stable-descriptor % opts) raw-tools)))
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
      (when-not (evoclj.sci.boundary/edn-safe? raw-args)
        (throw (err/error :provider/input-invalid
                          "MCP provider input must be plain EDN-safe data"
                          {:value (err/sanitize raw-args)})))
      (when-not (try (require 'malli.core) true (catch Exception _ false))
        nil)
      ;; malli validation when available
      (let [schema (:input-schema descriptor)]
        (when (and (not= :any schema) (not= schema :any))
          (try
            (when-not ((resolve 'malli.core/validate) schema raw-args)
              (throw (err/error :provider/input-invalid
                                "MCP provider input failed input-schema validation"
                                {:value (err/sanitize raw-args)})))
            (catch Exception e
              (when (= :provider/input-invalid (:error/type (ex-data e))) (throw e))))))
      ;; json-schema validation when present
      (when-let [js (:mcp/input-schema descriptor)]
        (when (and (map? js) (seq js))
          (try
            (when-not (evoclj.mcp.json-schema/validate js args)
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
       :resource (evoclj.mcp.canonical/canonical-resource (:tool/id descriptor) args)
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
              ;; Re-track after ensure-open: a just-opened record has
              ;; :closed? false, so ensure-open returns it unchanged and
              ;; cannot throw here — no leak window between the bindings.
              _ (when-not shared? (vreset! owned managed))]
          (when (mcp-client/closed? managed)
            (throw (err/error :mcp/client-closed
                              "MCP managed client is closed"
                              {:open-count (or (:open-count managed) 0)})))
          (let [client (:client managed)
                raw-result (mcp-client/call-tool client mcp-name args)
                edn-result ((deref (ns-resolve 'evoclj.provider.mcp-bridge 'result->edn))
                            raw-result)]
            ;; structured output validation when present
            (let [sc (get-in edn-result [:value :mcp/structured-content])
                  out-schema (:mcp/output-schema descriptor)]
              (when (and (some? sc) (map? out-schema) (seq out-schema))
                (when-not (evoclj.mcp.json-schema/validate out-schema sc)
                  (throw (err/error :provider/output-invalid "structuredContent failed mcp/output-schema" {:value (err/sanitize sc)})))))
            ;; provider output-schema validation
            (let [env (:value edn-result)
                  out (:output-schema descriptor)]
              (when (and (not= :any out) (not= out :any))
                (try
                  (when-not ((resolve 'malli.core/validate) out env)
                    (throw (err/error :provider/output-invalid "envelope failed provider/output-schema" {:value (err/sanitize env)})))
                  (catch Exception e
                    (when (= :provider/output-invalid (:error/type (ex-data e))) (throw e))))))
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
                (if (or (= category :mcp/transport-error)
                        (= category :mcp/protocol-error))
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
          ;; WO-M4: success AND failure exits release the call-scoped
          ;; client; close! is idempotent/graceful, close-owned! never
          ;; masks the original outcome.
          (when-not shared?
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
                        (catch Throwable e
                          (throw (err/error :mcp/discover-failed
                                            "MCP discover failed"
                                            {:source/id source-id :cause (err/sanitize e)}))))
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
