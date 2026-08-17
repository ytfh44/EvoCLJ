(ns evoclj.mcp.client
  "Thin Clojure wrapper over the MCP Java SDK synchronous client.

   Owns the McpSyncClient lifecycle for one MCP server connection.
   The caller opens the client (open!), uses it, then closes it
   (close!). Transport construction is delegated to
   evoclj.mcp.transport/transport-for so this namespace never touches
   transport classes directly.

   All functions throw ExceptionInfo with a stable :error/type on
   failure, consistent with the kernel error contract."
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

(defn open!
  "Build a McpSyncClient from a transport config map (see
   transport/transport-for for accepted shapes), initialize the
   connection, and return the live client. The returned client is a
   plain Java object; pass it to list-tools / call-tool, then hand it
   to close! when done.

   Throws :mcp/initialize-failed when initialize() itself throws."
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

(defn close
  "Close the McpSyncClient gracefully. Idempotent — calling close on a
   nil or already-closed client is a no-op. Swallows close errors so
   halt! paths never leak a failed-close exception."
  [^McpSyncClient client]
  (when client
    (try
      (.closeGracefully client)
      (catch Throwable _ nil))))

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

(defn list-tools
  "Return a seq of plain Clojure tool-descriptor maps from the live
   client. Each descriptor carries:

     {:mcp/name        <string>        ; the server-side tool name
      :mcp/title       <string?>       ; human title, or nil
      :mcp/description <string?>       ; description, or nil
      :mcp/input-schema <malli schema> ; JSON Schema converted to Malli
      :mcp/output-schema :any          ; MCP output is content-blocks
      :mcp/retry-safe? <boolean?>      ; true when the schema carries
                                       ; <nil> when absent
   All values are plain EDN-safe data (Global Constraint 22).

   Throws :mcp/list-tools-failed on transport failure."
   [^McpSyncClient client]
  (try
    (let [^McpSchema$ListToolsResult result (.listTools client)
          tools (.tools result)]
      (mapv (fn [^McpSchema$Tool t]
              (let [schema (or (.inputSchema ^McpSchema$JsonSchema t) {})]
                {:mcp/name        (.name t)
                 :mcp/title       (.title t)
                 :mcp/description (.description t)
                 :mcp/input-schema (cond
                                     (map? schema) schema
                                     (vector? schema) schema
                                     :else {})
                 :mcp/output-schema :any
                 :mcp/retry-safe? (boolean
                                    (some-> t .annotations
                                            .retryPolicy
                                            .isIdempotent))}))
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
      :mcp/is-error <boolean?>}

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
          is-error (.isError result)]
      (if is-error
        (throw (err/error :mcp/tool-error
                          (str "MCP tool " tool-name " returned isError=true")
                          {:tool-name tool-name
                           :content (vec content-block-maps)}))
        {:mcp/content content-block-maps
         :mcp/is-error false}))
    (catch Throwable ex
      (if (= :mcp/tool-error (:error/type (ex-data ex)))
        (throw ex)
        (throw (err/error :mcp/call-tool-failed
                          (str "MCP callTool " tool-name " failed")
                          {:tool-name tool-name
                           :args (err/sanitize args)
                           :cause (err/sanitize ex)}))))))
