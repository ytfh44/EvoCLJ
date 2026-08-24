(ns evoclj.mcp.client
  "Thin Clojure wrapper over the MCP Java SDK synchronous client.

   Owns the McpSyncClient lifecycle for one MCP server connection.
   The caller opens the client (open!), uses it, then closes it
   (close!). Transport construction is delegated to
   evoclj.mcp.transport/transport-for so this namespace never touches
   transport classes directly.

   All functions throw ExceptionInfo with a stable :error/type on
   failure, consistent with the kernel error contract.

   Phase 1 (connection lifecycle): open! returns a managed client
   record; call-tool auto-reconnects on transient failures; with-client
   guarantees open!/close! pairing; :connection/id enables connection
   pooling across providers.

   Phase 2 (client capabilities): list-tools timestamps descriptors;
   call-tool reports raw size; call-tool-streaming placeholder.

   Phase 6 (observability): managed records carry transport metadata
   and monotonic call/latency counters; ping! validates liveness."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.mcp.transport :as transport])
  (:import [io.modelcontextprotocol.client McpClient McpSyncClient]
           [io.modelcontextprotocol.spec McpClientTransport McpError]
            [io.modelcontextprotocol.spec McpSchema$JSONRPCResponse$JSONRPCError]
           [io.modelcontextprotocol.spec McpSchema$CallToolRequest
            McpSchema$CallToolResult
            McpSchema$ListToolsResult
            McpSchema$Tool
            McpSchema$JsonSchema
            McpSchema$Content
            McpSchema$ProgressNotification]))

;; --- lifecycle ---------------------------------------------------------------

(def ^:private default-max-reopen-attempts 2)

(def singleton-object-mapper
  "SHARED Jackson ObjectMapper used for wire-size computation in call-tool.
   A single JVM-wide instance is correct and performant: ObjectMapper is
   thread-safe for read/write after construction, and we never reconfigure
   it. WO-M9 removes the previous per-call `(new ObjectMapper.)` allocation.
   This is the SAME instance every caller receives (verified by
   evoclj.mcp.structured-content-boundary-test); there is no per-call
   construction anywhere in the call path."
  (com.fasterxml.jackson.databind.ObjectMapper.))

(defn- now-iso
  "Return the current instant as an ISO-8601 string."
  []
  (str (java.time.Instant/now)))

(defn- elapsed-ms
  "Return the elapsed milliseconds between two epoch millis values."
  [start end]
  (long (- end start)))

(defn- abort-init-close!
  "WO-M4 R2: best-effort shutdown of a McpSyncClient whose initialize()
   just threw. The stdio transport has ALREADY spawned a server subprocess
   whose only reliable exit is this client closing stdin, so skipping the
   close strands the half-built client AND its child until JVM death.
   Idempotent and error-proof: a secondary close failure is reported on
   stderr and swallowed so it never masks the ORIGINAL initialize
   failure the caller is about to see."
  [^McpSyncClient client]
  (when client
    (try
      (.closeGracefully client)
      (catch Throwable t
        (try
          (binding [*out* *err*]
            (println "[evoclj.mcp.client] post-initialize-failure client close failed:"
                     (pr-str (err/sanitize t))))
          (catch Throwable report-ex
            ;; same discipline as close-owned!: even the *err* write may
            ;; throw (an Error here must not escape and mask the original
            ;; failure) — one raw PrintStream line keeps the swallow
            ;; visible without risking another throw.
            (try
              (.println System/err
                        (str "[evoclj.mcp.client] post-initialize-failure close-failure report also failed: "
                             (pr-str (err/sanitize report-ex))))
              (catch Throwable _ nil))))))))

(defn- build-client
  "Build and initialize a McpSyncClient from a transport config map.
   Returns the live client, or throws :mcp/initialize-failed.

   When initialize() throws, the half-built client is closed
   best-effort first (its stdio subprocess would otherwise be leaked);
   the thrown error's type and data shape are unchanged.

   `tools-change-consumer` is an optional zero-arg fn that the client
   will invoke when the server notifies of a tools list change.

   `progress-consumer` is an optional one-arg fn that the client will
   invoke when the server sends a progress notification. The fn is
   called with a single progress notification map."
  ([transport-config tools-change-consumer]
   (build-client transport-config tools-change-consumer nil))
  ([transport-config tools-change-consumer progress-consumer]
  (let [t (transport/transport-for transport-config)
        spec (McpClient/sync ^McpClientTransport t)
        spec (if tools-change-consumer
               (.toolsChangeConsumer
                 spec
                 (reify java.util.function.Consumer
                   (accept [_ _]
                     (tools-change-consumer))))
               spec)
        spec (if progress-consumer
               (.progressConsumer
                 spec
                 (reify java.util.function.Consumer
                   (accept [_ p]
                     (progress-consumer
                       {:progress/token (.token ^McpSchema$ProgressNotification p)
                        :progress/current (.current ^McpSchema$ProgressNotification p)
                        :progress/total (.total ^McpSchema$ProgressNotification p)
                        :progress/percent (when (and (.total ^McpSchema$ProgressNotification p) (pos? (.total ^McpSchema$ProgressNotification p)))
                                            (double (/ (.current ^McpSchema$ProgressNotification p) (.total ^McpSchema$ProgressNotification p))))}))))
               spec)
        ^McpSyncClient sync-client (.build ^McpClient spec)]
    (try
      (.initialize sync-client)
      sync-client
      (catch Throwable ex
        ;; WO-M4 R2: reap the half-built client's stdio subprocess FIRST
        ;; (best-effort, error-proof — see abort-init-close!), THEN rethrow
        ;; the original :mcp/initialize-failed wrapping. Type, message, and
        ;; {:cause ...} data shape are byte-identical to the pre-cleanup
        ;; contract; only the leak is gone.
        (abort-init-close! sync-client)
        (throw (err/error :mcp/initialize-failed
                          "MCP client initialize failed"
                          {:cause (err/sanitize ex)})))))))

(defn- safe-close
  "Close a McpSyncClient gracefully. Swallows close errors."
  [^McpSyncClient client]
  (when client
    (try
      (.closeGracefully client)
      (catch Throwable _ nil))))

(defn open!
  "Build a McpSyncClient from a transport config map, initialize the
   connection, and return a managed client record:

     {:client <McpSyncClient>
      :closed? false
      :last-error nil
      :open-count 1
      :transport-config <map>
      :transport-type <keyword>
      :opened-at <ISO-8601 string>
      :call-count 0
      :last-latency-ms nil
      :tools-change-consumer <fn?>
      :progress-consumer <fn?>}

   The caller should hand the record to call-tool / list-tools, then
   to close! when done. Throws :mcp/initialize-failed when
   initialize() itself throws.

   `tools-change-consumer` is an optional zero-arg fn that will be
   invoked on the underlying McpSyncClient when the server notifies
   of a tools list change.

   `progress-consumer` is an optional one-arg fn that will be invoked
   when the server sends a progress notification. It receives a
   single progress notification map."
  ([transport-config]
   (open! transport-config nil nil))
  ([transport-config tools-change-consumer]
   (open! transport-config tools-change-consumer nil))
  ([transport-config tools-change-consumer progress-consumer]
   (let [client (if progress-consumer
                  (build-client transport-config tools-change-consumer progress-consumer)
                  (build-client transport-config tools-change-consumer))]
     {:client client
      :closed? false
      :last-error nil
      :open-count 1
      :transport-config transport-config
      :transport-type (or (some-> transport-config :type keyword) :unknown)
      :opened-at (now-iso)
      :call-count 0
      :last-latency-ms nil
      :tools-change-consumer tools-change-consumer
      :progress-consumer progress-consumer})))

(defn close!
  "Close the managed client record gracefully. Idempotent — calling
   close! on a nil or already-closed record is a no-op. Returns the
   record (or nil) with :closed? true and :client nil so the caller
   can safely discard it."
  [managed]
  (cond
    (not (map? managed))
    managed

    (:closed? managed)
    managed

    :else
    (do
      (safe-close (:client managed))
      (assoc managed :closed? true :client nil :last-error nil))))

(defn closed?
  "True when the managed client record is nil or already closed."
  [managed]
  (or (nil? managed) (and (map? managed) (:closed? managed))))

(defn- reopen!
  "Attempt to reopen a closed or broken managed client record up to
   max-attempts times. Returns the reopened record, or throws the last
   error."
  [managed max-attempts]
  (let [transport-config (:transport-config managed)
        tools-change-consumer (:tools-change-consumer managed)
        progress-consumer (:progress-consumer managed)]
    (letfn [(attempt [n]
              (try
                (let [next (open! transport-config tools-change-consumer progress-consumer)]
                  (assoc next :open-count (inc (or (:open-count managed) 0))
                             :last-error nil
                             :transport-config transport-config
                             :transport-type (or (some-> transport-config :type keyword) :unknown)))
                (catch Throwable ex
                  (if (< n max-attempts)
                    (do
                      (Thread/sleep 100)
                      (attempt (inc n)))
                    (throw (err/error :mcp/reopen-failed
                                      "MCP client reopen failed"
                                      {:attempt n
                                       :max-attempts max-attempts
                                       :cause (err/sanitize ex)}))))))]
      (attempt 1))))

;; --- tool discovery ----------------------------------------------------------

(defn- java-schema->clj
  "Recursively convert a remote JSON Schema (a java.util.Map with STRING
   keys, as returned by the MCP Java SDK 2.0 `Tool.inputSchema()` /
   `outputSchema()`) into a plain persistent Clojure map with STRING keys.
   Nested java.util.Map and java.util.List values are converted the same
   way. Keys are intentionally kept as STRINGS (never keywordized) because
   JSON property names are strings and the downstream Malli converter and
   JSON-Schema validator both read string keys."
  [v]
  (cond
    (instance? java.util.Map v)
    (into {} (map (fn [[k val]] [(if (keyword? k) (name k) (str k))
                                 (java-schema->clj val)])
                  v))

    (instance? java.util.List v)
    (mapv java-schema->clj v)

    (instance? java.util.Set v)
    (into #{} (map java-schema->clj v))

    (instance? java.util.Collection v)
    (mapv java-schema->clj v)

    :else v))

(defn list-tools
  "Return a paginated result map from the live client:

     {:tools [<tool-descriptor-map> ...]
      :next-cursor <string?>
      :has-more? <boolean?>}

   Each descriptor carries:

     {:mcp/name        <string>        ; the server-side tool name
      :mcp/title       <string?>       ; human title, or nil
      :mcp/description <string?>       ; description, or nil
      :mcp/input-schema <map>          ; normalized remote inputSchema as
                                       ; a plain string-keyed Clojure map
                                       ; (or {} when absent); the bridge's
                                       ; json-schema->malli converts it
      :mcp/output-schema <map|vector|  ; normalized remote outputSchema,
                            :any>       ; :any when absent, or a vector
                                       ; Malli schema (legacy 2025-11-25)
      :mcp/output-schema-kind <kw?>    ; :json-schema when output-schema is
                                       ; a map, :malli when a vector
      :mcp/retry-safe? <boolean?>      ; true when the schema carries
                                       ; <nil> when absent
      :mcp/last-refreshed <string?>    ; ISO-8601 timestamp of this
                                       ; listTools call, or nil

   All values are plain EDN-safe data (Global Constraint 22).

   `cursor` is an optional opaque string from a previous
   :next-cursor. Throws :mcp/list-tools-failed on transport failure."
  ([^McpSyncClient client]
   (list-tools client nil))
  ([^McpSyncClient client cursor]
   (try
     (let [now (now-iso)
           ^McpSchema$ListToolsResult result (if cursor
                                               (.listTools client ^String cursor)
                                               (.listTools client))
           tools (.tools result)
           next-cursor (.nextCursor result)]
       {:tools (mapv (fn [^McpSchema$Tool t]
                       (let [schema (or (.inputSchema ^McpSchema$JsonSchema t) {})
                             output-schema (or (.outputSchema ^McpSchema$JsonSchema t) :any)
                             input-schema (if (instance? java.util.Map schema)
                                           (java-schema->clj schema)
                                           {})
                             out-schema (cond
                                          (instance? java.util.Map output-schema)
                                          (java-schema->clj output-schema)
                                          :else output-schema)]
                         (cond-> {:mcp/name        (.name t)
                                  :mcp/title       (.title t)
                                  :mcp/description (.description t)
                                  :mcp/input-schema input-schema
                                  :mcp/output-schema out-schema
                                  :mcp/retry-safe? (boolean
                                                     (some-> t .annotations
                                                             .idempotentHint
                                                             boolean))
                                  :mcp/last-refreshed now}
                           (instance? java.util.Map out-schema) (assoc :mcp/output-schema-kind :json-schema)
                           (vector? out-schema) (assoc :mcp/output-schema-kind :malli))))
                     tools)
        :next-cursor next-cursor
        :has-more? (some? next-cursor)})
     (catch Throwable ex
       (throw (err/error :mcp/list-tools-failed
                         "MCP listTools failed"
                         {:cause (err/sanitize ex)}))))))

(def ^:dynamic ^:private *default-max-tools*
  "Hard ceiling on the number of tools `list-all-tools` will aggregate across
   all pages before failing closed with `:mcp/pagination-exceeded`.

   The MCP Java SDK 2.0.0 auto-follows `nextCursor` *inside a single*
   `listTools` call (WO-T1: one production call already walks every server
   page and returns the whole aggregate). A hostile or misconfigured server
   advertising a huge tool set would therefore blow the caller's memory the
   moment that one call returns, and — at the raw layer — a never-terminating
   cursor would block the SDK-internal loop indefinitely. This EvoCLJ-layer
   bound is the fail-closed guard (WO-M6; fail-closed per INV-04): it is
   enforced in `list-all-tools`, NOT delegated to the SDK. Callers may pass a
   tighter `:max-tools` via opts; configuration layers may rebind this var."
   10000)

(defn list-all-tools
  "Return a single vector of all plain Clojure tool-descriptor maps by
   following :next-cursor pagination until exhausted.

   `opts` may carry:
     :max-tools <pos-int>  hard ceiling on the aggregated tool count; when
       the running total would exceed it, throws `:mcp/pagination-exceeded`
       (fail-closed). Defaults to `*default-max-tools*`.

   Throws `:mcp/list-tools-failed` on transport failure and
   `:mcp/pagination-exceeded` when the aggregated tool count exceeds the
   configured cap (or the cap itself is not a positive integer)."
  ([^McpSyncClient client]
   (list-all-tools client {}))
  ([^McpSyncClient client {:keys [max-tools] :or {max-tools *default-max-tools*}}]
   ;; fail-closed on an unusable cap (e.g. cap = 0 / negative / nil): a
   ;; non-positive ceiling would let the next page push the aggregate past
   ;; any finite bound, so reject it up front rather than silently accept.
   (when-not (pos-int? max-tools)
     (throw (err/error :mcp/pagination-exceeded
                       "list-all-tools max-tools must be a positive integer"
                       {:max-tools (pr-str max-tools)
                        :error/reason :invalid-max-tools})))
   (loop [acc []
          cursor nil
          pages 0]
     (let [result (list-tools client cursor)
           tools (:tools result)
           next (into acc tools)
           pages (inc pages)]
       (cond
         ;; fail-closed: aggregated count exceeds the configured cap
         (> (count next) max-tools)
         (throw (err/error :mcp/pagination-exceeded
                           "list-all-tools exceeded the pagination tool-count cap"
                           {:max-tools max-tools
                            :observed (count next)
                            :pages pages
                            :error/reason :tool-count-exceeded}))
         ;; normal termination: no more pages
         (not (:has-more? result))
         next
         ;; keep aggregating the next page
         :else
         (recur next (:next-cursor result) pages))))))

;; --- tool invocation ---------------------------------------------------------

(defn call-tool
  "Call a single MCP tool by name with the given args map (plain EDN
   data). Returns the parsed result value:

     {:mcp/content [<content-block-maps>]
      :mcp/is-error <boolean?>
      :mcp/raw-size-bytes <int?>        ; total serialized size of the
                                       ; response content blocks in bytes,
                                       ; or nil when unknown}

   The caller is responsible for interpreting content blocks. Throws
   :mcp/call-tool-failed on transport failure and
   :mcp/tool-error when the server returns isError=true.

   Clojure keyword keys in `args` are converted to strings before
   serialization so the MCP JSON mapper can encode them."
  [^McpSyncClient client tool-name args]
  (when-not (string? tool-name)
    (throw (err/error :mcp/call-invalid
                      "MCP tool name must be a string"
                      {:tool-name (pr-str tool-name)})))
  (when-not (map? args)
    (throw (err/error :mcp/call-invalid
                      "MCP tool args must be a plain map"
                      {:args (pr-str args)})))
  (let [args (canonical/value->canonical args)]
    (try
      (let [^McpSchema$CallToolResult result
            (.callTool client (McpSchema$CallToolRequest. ^String tool-name args))
            content-block-maps
            (mapv (fn [^McpSchema$Content c]
                    (case (.type c)
                      "text" {:content/type :text
                              :content/text (.text c)}
                      "image" {:content/type :image
                               :content/data (.data c)
                               :content/mime-type (some-> c .mimeType .toString)}
                      "audio" {:content/type :audio
                               :content/data (.data c)
                               :content/mime-type (some-> c .mimeType .toString)}
                      "resource" {:content/type :resource
                                  :content/uri (some-> c .uri .toString)
                                  :content/mime-type (some-> c .mimeType .toString)
                                  :content/text (some-> c .text .toString)}
                      "resource_link" {:content/type :resource-link
                                       :content/uri (some-> c .uri .toString)
                                       :content/mime-type (some-> c .mimeType .toString)}
                      {:content/type :unknown
                       :content/raw (str c)}))
                  (.content result))
            is-error (boolean (.isError result))
            structured-content (canonical/java-value->edn (.structuredContent result))
            wire-bytes (try
                         ;; WO-M9: serialize the wire envelope with the SHARED
                         ;; singleton ObjectMapper. NB: `mapper` and `envelope`
                         ;; are distinct bindings - a previous edit shadowed the
                         ;; mapper with the envelope, which silently fell through
                         ;; to the byte-length fallback and defeated the
                         ;; singleton entirely. Keeping them separate is what
                         ;; makes Mutation C (per-call `new ObjectMapper.`) and
                         ;; the singleton-usage test observable.
                         (let [mapper singleton-object-mapper
                               envelope {"content" content-block-maps
                                          "structuredContent" structured-content
                                          "isError" is-error}]
                           (alength (.writeValueAsBytes mapper envelope)))
                         (catch Throwable _
                           (long (reduce + 0 (map #(alength (.getBytes (str %) "UTF-8")) content-block-maps)))))]
        (cond-> {:mcp/content content-block-maps
                 :mcp/is-error is-error
                 :mcp/tool-status (if is-error :error :ok)}
          (some? structured-content) (assoc :mcp/structured-content structured-content)
          (pos? wire-bytes) (assoc :mcp/raw-size-bytes wire-bytes)))
      (catch Throwable ex
        (let [mcp-code (when (instance? McpError ex)
                         (some-> ex .getJsonRpcError .code))]
          (throw (err/error :mcp/call-tool-failed
                            (str "MCP callTool " tool-name " failed")
                            (cond-> {:tool-name tool-name
                                     :args (err/sanitize args)
                                     :cause (err/sanitize ex)}
                              (some? mcp-code) (assoc :mcp/error-code mcp-code)))))))))

;; --- error classification v2 (M7) --------------------------------------------
;;
;; classify-mcp-error turns a raw Throwable (or an EvoCLJ ExceptionInfo that
;; wraps one) into a sanitized, typed error map. The classification is
;; fail-closed and produces INDEPENDENT, machine-readable categories:
;;
;;   :mcp/timeout        — TimeoutException / SocketTimeoutException, AND the
;;                         MCP JSON-RPC request-timeout code -32001. Timeouts
;;                         are their OWN family and are never folded into the
;;                         generic :mcp/transport-error bucket.
;;   :mcp/transport-error— IOException / SocketException / ConnectException,
;;                         and the JSON-RPC connection-closed code -32000.
;;   :mcp/protocol-error — Jackson parse/mapping failures, and the JSON-RPC
;;                         parse/invalid-request codes -32700 / -32600.
;;   :mcp/method-not-found / :mcp/invalid-params / :mcp/internal-error —
;;                         typed JSON-RPC codes -32601 / -32602 / -32603.
;;   :mcp/json-rpc-error — any other MCP JSON-RPC error code (carries :mcp/error-code).
;;   :mcp/unknown-error  — nothing above matched (fail-closed default).
;;
;; JSON-RPC evidence is read from two sources:
;;   1. a :mcp/error-code stashed in ex-data when an McpError is wrapped at the
;;      call-tool boundary (so the numeric code survives sanitization);
;;   2. the cause chain, walked through the SANITIZED error tree because the
;;      production wrappers store the original throwable as a :cause map (the
;;      live .getCause is nil after err/error wraps it).
;;
;; The classifier is pure: no shared mutable state is touched (INV-05).

(def ^:private timeout-classes
  "Java exception classes that mean a wall-clock deadline was exceeded."
  #{"java.util.concurrent.TimeoutException" "java.net.SocketTimeoutException"})

(def ^:private transport-classes
  "Java exception classes that mean the CONNECTION (not the tool/input) failed."
  #{"java.io.IOException" "java.net.SocketException" "java.net.ConnectException"})

(def ^:private protocol-classes
  "Java exception classes that mean a malformed JSON payload on the wire."
  #{"com.fasterxml.jackson.core.JsonParseException" "com.fasterxml.jackson.databind.JsonMappingException"})

(def ^:private transient-error-types
  "Error types that represent a retryable, connection-level failure. A timeout,
   transport break, or protocol break on a shared pooled connection should be
   healed (mark-broken / reopen) rather than treated as a terminal business
   error."
  #{:mcp/timeout :mcp/transport-error :mcp/protocol-error})

(defn transient-error-type?
  "True when `error-type` is a retryable connection-level (transient) MCP
   failure family."
  [error-type]
  (contains? transient-error-types error-type))

(defn- json-rpc-code->type
  "Map a JSON-RPC error `code` to a typed MCP error category. Unknown codes
   fall back to :mcp/json-rpc-error (the code is still carried on the map)."
  [code]
  (case code
    -32700 :mcp/protocol-error
    -32600 :mcp/protocol-error
    -32601 :mcp/method-not-found
    -32602 :mcp/invalid-params
    -32603 :mcp/internal-error
    -32000 :mcp/transport-error
    -32001 :mcp/timeout
    :mcp/json-rpc-error))

(defn- walk-classes-and-code
  "Walk the SANITIZED error tree collecting every :error/class string and the
   first :mcp/error-code encountered. The production wrappers keep the
   original throwable as a :cause map (under ex-data :cause) and stash JSON-RPC
   codes under :error/data, so the whole bounded sanitized tree is visited."
  [node]
  (loop [stack [node] classes #{} code nil]
    (if (seq stack)
      (let [n (first stack)]
        (cond
          (map? n)
          (recur (into (rest stack) (vals n))
                 (cond-> classes
                   (string? (:error/class n)) (conj (:error/class n)))
                 (or code (:mcp/error-code n)))
          (or (vector? n) (seq? n))
          (recur (into (rest stack) (vec n)) classes code)
          :else (recur (rest stack) classes code)))
      {:classes classes :code code})))
(defn- live-mcp-code
  "Walk the LIVE throwable cause chain and return the first JSON-RPC error
   code carried by an io.modelcontextprotocol.spec.McpError, or nil. A raw
   McpError (the SDK's JSON-RPC error response) is not yet wrapped, so its
   code lives on the instance — not in ex-data."
  [^Throwable ex]
  (loop [t ex]
    (when t
      (if (instance? McpError t)
        (some-> t .getJsonRpcError .code)
        (recur (.getCause t))))))

(defn- categorize-error
  "Return a map with :error/type (and, for JSON-RPC errors, :mcp/error-code)
   for the given Throwable. Priority: explicit JSON-RPC code > timeout class >
   transport class > protocol class > already-typed EvoCLJ error > unknown.
   The JSON-RPC code is read from two sources: a live McpError instance in the
   cause chain, and a :mcp/error-code stashed in ex-data when an McpError is
   wrapped at the call-tool boundary."
  [^Throwable ex]
  (let [data (err/error-data ex)
        {:keys [classes code]} (walk-classes-and-code data)
        code (or code (live-mcp-code ex))
        stable (:error/type (ex-data ex))]
    (cond
      ;; 1. JSON-RPC code wins even when wrapped in :mcp/call-tool-failed.
      (some? code)
      {:error/type (json-rpc-code->type code) :mcp/error-code code}
      ;; 2. timeout is its own family (never folded into transport).
      (some timeout-classes classes) {:error/type :mcp/timeout}
      ;; 3. transport break.
      (some transport-classes classes) {:error/type :mcp/transport-error}
      ;; 4. protocol (wire) break.
      (some protocol-classes classes) {:error/type :mcp/protocol-error}
      ;; 5. an already-typed EvoCLJ error keeps its identity.
      (and stable (not= stable :error/unknown)) {:error/type stable}
      ;; 6. fail-closed default.
      :else {:error/type :mcp/unknown-error})))

(defn classify-mcp-error
  "Return a sanitized error map with an enriched :error/type for the given
   Throwable. For MCP-originated JSON-RPC errors the map also carries
   :mcp/error-code (the numeric JSON-RPC code). Timeouts are reported as the
   independent :mcp/timeout category — they are never folded into
   :mcp/transport-error. Wraps the throwable's class, message, and cause chain
   without leaking Java objects across the Agent boundary."
  [^Throwable ex]
  (err/sanitize
    (let [cat (categorize-error ex)]
      (assoc (err/error-data ex)
             :error/type (:error/type cat)
             :mcp/error-code (:mcp/error-code cat)))))

(defn reduce-content-blocks
  "Honest rename for legacy non-streaming block reduction. Was call-tool-streaming which falsely promised protocol-level streaming. For true async streaming use adapter async channel."
  [client tool-name args]
  (reify clojure.lang.IReduceInit
    (reduce [_ f init]
      (let [result (call-tool client tool-name args)
            blocks (:mcp/content result)]
        (reduce f init blocks)))))
(def call-tool-streaming
  "Deprecated alias for reduce-content-blocks. Will be removed."
  reduce-content-blocks)

;; --- managed client helpers --------------------------------------------------

(defn with-client
  "Open a managed client record from `transport-config`, bind it to
   `managed`, execute `body`, then close the client. Returns the value
   of `body`. Guarantees open!/close! pairing even when `body`
   throws."
  [transport-config body]
  (let [managed (open! transport-config)]
    (try
      (body managed)
      (finally
        (close! managed)))))

(defn ensure-open
  "Return a live McpSyncClient from `managed`. If the managed record is
   closed or broken, attempt to reopen it (up to max-attempts).
   Throws :mcp/reopen-failed when all attempts fail."
  [managed max-attempts]
  (cond
    (nil? managed)
    nil

    (:closed? managed)
    (reopen! managed max-attempts)

    :else
    managed))

(defn call-tool-managed
  "Call a single MCP tool using a managed client record. The managed
   record is expected to contain :transport-config for reopening. On
   transient failures, attempts to reopen the client up to
   max-attempts times before failing.

   Returns the same shape as call-tool. Throws :mcp/call-tool-failed
   or :mcp/tool-error.

   Updates the managed record's :call-count and :last-latency-ms on
   success for observability."
  ([managed tool-name args]
   (call-tool-managed managed tool-name args default-max-reopen-attempts))
  ([managed tool-name args max-attempts]
   (let [managed (ensure-open managed max-attempts)]
     (when (closed? managed)
       (throw (err/error :mcp/client-closed
                         "MCP managed client is closed"
                         {:open-count (or (:open-count managed) 0)})))
     (try
       (let [start (System/currentTimeMillis)
             result (call-tool (:client managed) tool-name args)
             latency (elapsed-ms start (System/currentTimeMillis))
             updated (assoc managed
                            :call-count (inc (or (:call-count managed) 0))
                            :last-latency-ms latency
                            :last-error nil)]
         (assoc result
                :mcp/call-count (:call-count updated)
                :mcp/last-latency-ms latency
                :mcp/transport-type (:transport-type updated)
                :mcp/transport-config (err/sanitize (:transport-config updated))))
       (catch Throwable ex
         (let [updated (assoc managed
                              :last-error (err/error-data ex))]
           (throw (err/error :mcp/call-tool-failed
                             (str "MCP callTool " tool-name " failed")
                             {:tool-name tool-name
                              :args (err/sanitize args)
                              :cause (err/sanitize ex)
                              :managed updated
                              :open-count (or (:open-count managed) 0)}))))))))

(defn ping!
  "Validate liveness of a managed MCP client over the REAL transport by
   calling the MCP Java SDK's `McpSyncClient.ping()` — a genuine JSON-RPC
   `ping` request is sent across the wire and must round-trip, so liveness
   is actually verified (not faked by counting tools).

   Returns a plain EDN liveness map on success:

       {:mcp/ping :ok
        :mcp/ping-roundtrip-ms <pos-int>
        :mcp/ping-at <ISO-8601 string>}

   Throws :mcp/ping-failed when the managed record is closed/nil or the
   live ping request fails. The thrown error carries
   `:mcp/ping-cause-type` — the underlying error category produced by
   `classify-mcp-error` (e.g. :mcp/timeout / :mcp/transport-error) — so a
   failed liveness probe is itself typed and fail-closed."
  [managed]
  (if (closed? managed)
    (throw (err/error :mcp/ping-failed
                      "MCP client is closed"
                      {:open-count (or (:open-count managed) 0)}))
    (let [client (:client (ensure-open managed default-max-reopen-attempts))
          start (System/currentTimeMillis)]
      (try
        (.ping ^McpSyncClient client)
        (let [rt (elapsed-ms start (System/currentTimeMillis))]
          {:mcp/ping :ok
           :mcp/ping-roundtrip-ms (long (max 0 rt))
           :mcp/ping-at (now-iso)})
        (catch Throwable ex
          (let [classified (classify-mcp-error ex)]
            (throw (err/error :mcp/ping-failed
                              "MCP ping failed"
                              {:mcp/ping-cause-type (:error/type classified)
                               :mcp/ping-roundtrip-ms (long (max 0 (elapsed-ms start (System/currentTimeMillis))))
                               :cause (err/sanitize ex)}))))))))

;; --- optional keepalive (M8) -------------------------------------------------
;;
;; Keepalive is OPT-IN: callers must pass :keepalive? true to
;; `start-keepalive!`. The default (no opt-in) returns nil and starts NO
;; background thread, so liveness probing never happens unless explicitly
;; requested. When enabled, a daemon thread pings the live client on a
;; fixed interval and records the result in a shared liveness atom that the
;; caller owns (returned in the control map), so external code can observe
;; connection health without being coupled to the ping loop.

(defn- keepalive-step!
  "One ping cycle: update `liveness` with the result of `ping!`. Never
   throws out of the loop — a failed ping is recorded as :failed so the
   loop keeps running (and the failure is observable)."
  [managed liveness]
  (let [snapshot (try
                   (ping! managed)
                   (catch Throwable ex
                     {:mcp/ping :failed
                      :mcp/ping-cause-type (:error/type (ex-data ex))}))
        verdict (if (= :ok (:mcp/ping snapshot)) :ok :failed)]
    (swap! liveness assoc
           :mcp/keepalive verdict
           :mcp/keepalive-at (now-iso)
           :mcp/last-ping snapshot)
    snapshot))

(defn start-keepalive!
  "Start OPTIONAL periodic liveness probing for a managed MCP client.

   opts:
     :keepalive?        <boolean>  enable the loop (DEFAULT false — opt-in only)
     :interval-ms      <pos-int>  ping period (default 30000)
     :liveness         <atom?>    caller-owned atom to record health into;
                                   a fresh atom is created when omitted

   Returns nil when keepalive is NOT enabled (default-off, nothing runs).
   When enabled, returns a control map:

       {:keepalive? true
        :stop!      <fn>      ; halts the loop and joins the thread (bounded)
        :liveness   <atom>    ; {:mcp/keepalive :ok|:failed ...}
        :thread     <Thread>}

   The loop is a daemon thread that pings on `:interval-ms` and updates the
   liveness atom. Fail-closed: a failed ping records :failed (with the
   classified cause type) rather than terminating the loop or masking the
   failure. stop! is idempotent and bounded — it interrupts the thread and
   joins with a timeout, never blocking forever."
  ([managed] (start-keepalive! managed {}))
  ([managed {:keys [keepalive? interval-ms liveness]
             :or {keepalive? false interval-ms 30000}}]
   ;; OPTIONAL by contract: without explicit opt-in, do nothing — return
   ;; nil and start NO background thread (default safe/off).
   (when keepalive?
     (let [liveness (or liveness (atom {:mcp/keepalive :unknown}))
           interval-ms (long (max 1 (or interval-ms 30000)))
           stop? (atom false)
           thread (Thread.
                    (fn []
                      (while (not @stop?)
                        (try
                          (keepalive-step! managed liveness)
                          (catch Throwable _ nil))
                        (let [deadline (+ (System/currentTimeMillis) interval-ms)]
                          (while (and (not @stop?)
                                      (< (System/currentTimeMillis) deadline))
                            (try (Thread/sleep 20) (catch Throwable _ nil))))))
                    (str "evoclj-mcp-keepalive-" (java.util.UUID/randomUUID)))]
       (.setDaemon thread true)
       (.start thread)
       {:keepalive? true
        :stop! (fn stop!
                 ([]
                  (stop! 5000))
                 ([join-ms]
                  (when (compare-and-set! stop? false true)
                    (try (.interrupt thread) (catch Throwable _ nil))
                    (try (.join thread (long join-ms)) (catch Throwable _ nil)))
                  nil))
        :liveness liveness
        :thread thread}))))
