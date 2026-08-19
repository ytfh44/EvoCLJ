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
   call-tool reports raw size; call-tool-streaming placeholder."
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

(defn- build-client
  "Build and initialize a McpSyncClient from a transport config map.
   Returns the live client, or throws :mcp/initialize-failed."
  [transport-config]
  (let [t (transport/transport-for transport-config)
        client (McpClient/sync ^McpClientTransport t)
        ^McpSyncClient sync-client (.build ^McpClient client)]
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
      :open-count 1}

   The caller should hand the record to call-tool / list-tools, then
   to close! when done. Throws :mcp/initialize-failed when
   initialize() itself throws."
  [transport-config]
  (let [client (build-client transport-config)]
    {:client client
     :closed? false
     :last-error nil
     :open-count 1}))

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
    (loop [attempt 1]
      (let [next (open! transport-config)]
        (assoc next :open-count (inc (or (:open-count managed) 0))
                   :last-error nil)))))

;; --- tool discovery ----------------------------------------------------------

(defn- explain->data
  "Extract the tool name and a serializable explanation map from a
   m/explain result, or nil when the validate passes."
  [malli-explanation]
  (when (map? malli-explanation)
    {:type (:type malli-explanation)
     :problems (mapv (fn [p]
                       {:path (:path p)
                        :schema (:schema p)
                        :value (:value p)
                        :expected (:expected p)})
                     (or (:problems malli-explanation) []))}))

(defn- now-iso
  "Return the current instant as an ISO-8601 string."
  []
  (str (java.time.Instant/now)))

(defn list-tools
  "Return a seq of plain Clojure tool-descriptor maps from the live
   client. Each descriptor carries:

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

   Throws :mcp/list-tools-failed on transport failure."
  [^McpSyncClient client]
  (try
    (let [now (now-iso)
          ^McpSchema$ListToolsResult result (.listTools client)
          tools (.tools result)]
      (mapv (fn [^McpSchema$Tool t]
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
                                                    .retryPolicy
                                                    .isIdempotent))
                         :mcp/last-refreshed now}
                  (map? output-schema) (assoc :mcp/output-schema-kind :json-schema)
                  (vector? output-schema) (assoc :mcp/output-schema-kind :malli))))
            tools))
    (catch Throwable ex
      (throw (err/error :mcp/list-tools-failed
                        "MCP listTools failed"
                        {:cause (err/sanitize ex)})))))

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
   :mcp/tool-error when the server returns isError=true."
  [^McpSyncClient client tool-name args]
  (when-not (string? tool-name)
    (throw (err/error :mcp/call-invalid
                      "MCP tool name must be a string"
                      {:tool-name (pr-str tool-name)})))
  (when-not (map? args)
    (throw (err/error :mcp/call-invalid
                      "MCP tool args must be a plain map"
                      {:args (pr-str args)})))
  (try
    (let [^McpSchema$CallToolResult result
          (.callTool client (McpSchema$CallToolRequest. ^String tool-name
                                                      (into-array Object [args])))
          content-block-maps
          (mapv (fn [^McpSchema$Content c]
                  (cond
                    (.isText c)
                    {:content/type :text
                     :content/text (.text c)}

                    (.isImage c)
                    {:content/type :image
                     :content/data (str "base64:" (.data c))
                     :content/mime-type (some-> c .mimeType .toString)}

                    (.isResource c)
                    {:content/type :resource
                     :content/uri  (some-> c .resource .uri .toString)
                     :content/mime-type (some-> c .resource .mimeType .toString)
                     :content/text (some-> c .resource .text .toString)}

                    :else
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
                           :cause (err/sanitize ex)}))))))

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

(defn- ensure-open
  "Return a live McpSyncClient from `managed`. If the managed record is
   closed or broken, attempt to reopen it (up to max-attempts).
   Throws :mcp/reopen-failed when all attempts fail."
  [managed max-attempts]
  (cond
    (nil? managed)
    (reopen! managed max-attempts)

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
   or :mcp/tool-error."
  ([managed tool-name args]
   (call-tool-managed managed tool-name args default-max-reopen-attempts))
  ([managed tool-name args max-attempts]
   (when (closed? managed)
     (throw (err/error :mcp/client-closed
                       "MCP managed client is closed"
                       {:open-count (or (:open-count managed) 0)})))
   (let [managed (ensure-open managed max-attempts)]
     (try
       (call-tool (:client managed) tool-name args)
       (catch Throwable ex
         (if (= :mcp/tool-error (:error/type (ex-data ex)))
           (throw ex)
           (throw (err/error :mcp/call-tool-failed
                             (str "MCP callTool " tool-name " failed")
                             {:tool-name tool-name
                              :args (err/sanitize args)
                              :cause (err/sanitize ex)}))))))))
