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
            [evoclj.mcp.transport :as transport])
  (:import [io.modelcontextprotocol.client McpClient McpSyncClient]
           [io.modelcontextprotocol.spec McpClientTransport]
           [io.modelcontextprotocol.spec McpSchema$CallToolRequest
            McpSchema$CallToolResult
            McpSchema$ListToolsResult
            McpSchema$Tool
            McpSchema$JsonSchema
            McpSchema$Content]))

;; --- lifecycle ---------------------------------------------------------------

(def ^:private default-max-reopen-attempts 2)

(defn- now-iso
  "Return the current instant as an ISO-8601 string."
  []
  (str (java.time.Instant/now)))

(defn- elapsed-ms
  "Return the elapsed milliseconds between two epoch millis values."
  [start end]
  (long (- end start)))

(defn- build-client
  "Build and initialize a McpSyncClient from a transport config map.
   Returns the live client, or throws :mcp/initialize-failed.

   `tools-change-consumer` is an optional zero-arg fn that the client
   will invoke when the server notifies of a tools list change."
  [transport-config tools-change-consumer]
  (let [t (transport/transport-for transport-config)
        spec (McpClient/sync ^McpClientTransport t)
        spec (if tools-change-consumer
               (.toolsChangeConsumer
                 spec
                 (reify java.util.function.Consumer
                   (accept [_ _]
                     (tools-change-consumer))))
               spec)
        ^McpSyncClient sync-client (.build ^McpClient spec)]
    (try
      (.initialize sync-client)
      sync-client
      (catch Throwable ex
        (throw (err/error :mcp/initialize-failed
                          "MCP client initialize failed"
                          {:cause (err/sanitize ex)}))))))

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
      :last-latency-ms nil}

   The caller should hand the record to call-tool / list-tools, then
   to close! when done. Throws :mcp/initialize-failed when
   initialize() itself throws.

   `tools-change-consumer` is an optional zero-arg fn that will be
   invoked on the underlying McpSyncClient when the server notifies
   of a tools list change."
  ([transport-config]
   (open! transport-config nil))
  ([transport-config tools-change-consumer]
   (let [client (build-client transport-config tools-change-consumer)]
     {:client client
      :closed? false
      :last-error nil
      :open-count 1
      :transport-config transport-config
      :transport-type (or (some-> transport-config :type keyword) :unknown)
      :opened-at (now-iso)
      :call-count 0
      :last-latency-ms nil})))

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
  (let [transport-config (:transport-config managed)]
    (letfn [(attempt [n]
              (try
                (let [next (open! transport-config)]
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

(defn list-tools
  "Return a paginated result map from the live client:

     {:tools [<tool-descriptor-map> ...]
      :next-cursor <string?>
      :has-more? <boolean?>}

   Each descriptor carries:

     {:mcp/name        <string>        ; the server-side tool name
      :mcp/title       <string?>       ; human title, or nil
      :mcp/description <string?>       ; description, or nil
      :mcp/input-schema <malli schema> ; JSON Schema converted to Malli
      :mcp/output-schema <any>         ; remote outputSchema when present,
                                       ; otherwise :any
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
                             output-schema (or (.outputSchema ^McpSchema$JsonSchema t) :any)]
                         (cond-> {:mcp/name        (.name t)
                                  :mcp/title       (.title t)
                                  :mcp/description (.description t)
                                  :mcp/input-schema (cond
                                                      (map? schema) schema
                                                      (vector? schema) schema
                                                      :else {})
                                  :mcp/output-schema output-schema
                                  :mcp/retry-safe? (boolean
                                                     (some-> t .annotations
                                                             .idempotentHint
                                                             boolean))
                                  :mcp/last-refreshed now}
                           (map? output-schema) (assoc :mcp/output-schema-kind :json-schema)
                           (vector? output-schema) (assoc :mcp/output-schema-kind :malli))))
                     tools)
        :next-cursor next-cursor
        :has-more? (some? next-cursor)})
     (catch Throwable ex
       (throw (err/error :mcp/list-tools-failed
                         "MCP listTools failed"
                         {:cause (err/sanitize ex)}))))))

(defn list-all-tools
  "Return a single vector of all plain Clojure tool-descriptor maps by
   following :next-cursor pagination until exhausted.

   Throws :mcp/list-tools-failed on transport failure."
  [^McpSyncClient client]
  (loop [acc []
         cursor nil]
    (let [result (list-tools client cursor)
          tools (:tools result)]
      (if (:has-more? result)
        (recur (into acc tools) (:next-cursor result))
        (into acc tools)))))

;; --- tool invocation ---------------------------------------------------------

(defn- edn->json-compatible
  "Recursively convert Clojure keyword keys to strings so the MCP JSON
   mapper can serialize them correctly."
  [v]
  (cond
    (map? v)
    (into (empty v)
          (map (fn [[k v]] [(if (keyword? k) (name k) k) (edn->json-compatible v)]))
          v)

    (vector? v)
    (mapv edn->json-compatible v)

    (seq? v)
    (map edn->json-compatible v)

    :else v))

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
  (let [args (edn->json-compatible args)]
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
                      "resource" {:content/type :resource
                                  :content/uri (some-> c .uri .toString)
                                  :content/mime-type (some-> c .mimeType .toString)
                                  :content/text (some-> c .text .toString)}
                      {:content/type :unknown
                       :content/raw (str c)}))
                  (.content result))
            is-error (.isError result)
            raw-size (long (reduce + 0 (map #(.length (str %)) (.content result))))]
        (if is-error
          (throw (err/error :mcp/tool-error
                            (str "MCP tool " tool-name " returned isError=true")
                            {:tool-name tool-name
                             :content (vec content-block-maps)}))
          (cond-> {:mcp/content content-block-maps
                   :mcp/is-error false}
            (pos? raw-size) (assoc :mcp/raw-size-bytes raw-size))))
      (catch Throwable ex
        (if (= :mcp/tool-error (:error/type (ex-data ex)))
          (throw ex)
          (throw (err/error :mcp/call-tool-failed
                            (str "MCP callTool " tool-name " failed")
                            {:tool-name tool-name
                             :args (err/sanitize args)
                             :cause (err/sanitize ex)})))))))

(defn call-tool-streaming
  "Placeholder for streaming tool calls. The current MCP Java SDK does
   not expose a streaming call-tool API, so this falls back to
   call-tool and returns the whole result as a single-element
   reducible collection.

   Returns a reducible of content-block maps (the same maps
   call-tool returns inside :mcp/content). Throws the same errors as
   call-tool."
  [client tool-name args]
  (reify clojure.lang.IReduceInit
    (reduce [_ f init]
      (let [result (call-tool client tool-name args)
            blocks (:mcp/content result)]
        (reduce f init blocks)))))

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
             latency (elapsed-ms start (System/currentTimeMillis))]
         (assoc result
                :mcp/call-count (inc (or (:call-count managed) 0))
                :mcp/last-latency-ms latency
                :mcp/transport-type (:transport-type managed)
                :mcp/transport-config (err/sanitize (:transport-config managed))))
       (catch Throwable ex
         (if (= :mcp/tool-error (:error/type (ex-data ex)))
           (throw ex)
           (throw (err/error :mcp/call-tool-failed
                             (str "MCP callTool " tool-name " failed")
                             {:tool-name tool-name
                              :args (err/sanitize args)
                              :cause (err/sanitize ex)}))))))))

(defn ping!
  "Validate liveness of a managed MCP client by calling list-tools and
   returning the tool count. Throws :mcp/ping-failed when the client
   is closed or listTools throws."
  [managed]
  (if (closed? managed)
    (throw (err/error :mcp/ping-failed
                      "MCP client is closed"
                      {:open-count (or (:open-count managed) 0)}))
    (let [client (:client (ensure-open managed default-max-reopen-attempts))]
      (try
        (count (list-all-tools client))
        (catch Throwable ex
          (throw (err/error :mcp/ping-failed
                            "MCP ping failed"
                            {:cause (err/sanitize ex)})))))))
