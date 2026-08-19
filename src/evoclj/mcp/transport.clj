(ns evoclj.mcp.transport
  "Thin Clojure wrapper over the MCP Java SDK transport constructors.

   Each public function builds and returns a Java transport object; the
   caller owns the lifecycle (open/close). No MCP protocol logic lives
   here — just constructor calls with keyword-to-Java-enum coercion for
   the stdio command map.

   Phase 3 (transport coverage): stdio accepts :cwd and :env; SSE/HTTP
   transports accept :headers; TLS configuration is supported via
   :tls-context."
  (:import [io.modelcontextprotocol.client.transport
            StdioClientTransport
            HttpClientSseClientTransport
            HttpClientStreamableHttpTransport
            ServerParameters$Builder
            ServerParameters]
           [io.modelcontextprotocol.json.jackson3
            JacksonMcpJsonMapperSupplier]
           [io.modelcontextprotocol.json
            McpJsonMapperSupplier]
           [java.nio.file Files Paths]
           [java.util Map]
           [javax.net.ssl SSLContext]))

;; --- stdio -------------------------------------------------------------------

(defn- build-server-parameters
  "Build a ServerParameters from a plain Clojure map {:command <string>
   :args [<strings>] :cwd <string?> :env {<string> <string>}}. The
   Builder Java API is ServerParameters.builder(command).args(varargs)
   .directory(file) .environment(map) .build(); we call each setter
   when the corresponding key is present."
  [{:keys [command args cwd env]}]
  (let [b (ServerParameters/builder ^String command)]
    (when (seq args)
      (.args ^ServerParameters$Builder b (into-array String args)))
    (when (and cwd (string? cwd))
      (let [path (Paths/get cwd (make-array String 0))]
        (when (Files/exists path (make-array java.nio.file.LinkOption 0))
          (.directory ^ServerParameters$Builder b path))))
    (when (and env (map? env))
      (.environment ^ServerParameters$Builder b (into-array [env])))
    (.build ^ServerParameters$Builder b)))

(defn stdio-transport
  "Build a StdioClientTransport from a config map {:command <string>
   :args [<strings>] :cwd <string?> :env {<string> <string>}}.
   Returns the transport; the caller passes it to
   McpClient.sync(.build()) or McpClient.async(.build()).

   StdioClientTransport requires two constructor arguments:
   ServerParameters and McpJsonMapper. We obtain the mapper from
   JacksonMcpJsonMapperSupplier (the jackson3 module, already on the
   classpath via the mcp-core transitive dependency)."
  [{:keys [command args] :as config}]
  (when-not (string? command)
    (throw (ex-info "MCP stdio transport requires :command as a string"
                    {:error/type :mcp/transport-invalid
                     :transport/type :stdio
                     :config (pr-str config)})))
  (let [params (build-server-parameters
                {:command command
                 :args (or args [])
                 :cwd (:cwd config)
                 :env (:env config)})
        ^McpJsonMapperSupplier supplier (JacksonMcpJsonMapperSupplier.)
        mapper (.get ^McpJsonMapperSupplier supplier)]
    (StdioClientTransport. ^ServerParameters params ^McpJsonMapper mapper)))

;; --- SSE ---------------------------------------------------------------------

(defn- apply-http-options
  "Apply optional :headers and :tls-context to an HTTP client transport
   builder. Returns the builder. :headers is a map of string -> string;
   :tls-context is either a javax.net.ssl.SSLContext or a map with
   :trust-managers."
  [builder headers tls-context]
  (let [builder (cond
                  (and (map? headers) (seq headers))
                  (.headers builder ^java.util.Map (doto (java.util.HashMap.)
                                                     (.putAll ^java.util.Map (java.util.HashMap. headers))))
                  :else builder)]
    (cond
      (instance? javax.net.ssl.SSLContext tls-context)
      (.tlsContext builder ^javax.net.ssl.SSLContext tls-context)
      (and (map? tls-context) (seq (:trust-managers tls-context)))
      (let [tm-array (.toArray ^java.util.Collection (:trust-managers tls-context))
            ctx (javax.net.ssl.SSLContext/getInstance "TLS")]
        (.init ctx nil tm-array (java.security.SecureRandom.))
        (.tlsContext builder ctx))
      :else builder)))

(defn sse-transport
  "Build an HttpClientSseClientTransport for the given SSE endpoint URL
   (e.g. \"http://localhost:3000/sse\"). The core mcp module ships
   HttpClientSseClientTransport in mcp-core.

   Accepts optional :headers and :tls-context from the config map.

   Uses the builder pattern: builder(baseUri).headers(map).tlsContext(ctx)
   .build() returns the transport."
  ([url]
   (sse-transport url nil nil))
  ([url headers tls-context]
   (when-not (string? url)
     (throw (ex-info "MCP SSE transport requires a URL string"
                     {:error/type :mcp/transport-invalid
                      :transport/type :sse
                      :url (pr-str url)})))
   (let [b (HttpClientSseClientTransport/builder ^String url)]
     (apply-http-options b headers tls-context)
     (.build b))))

;; --- Streamable HTTP ---------------------------------------------------------

(defn streamable-http-transport
  "Build an HttpClientStreamableHttpTransport for the given base URL and
   optional endpoint path (default \"/mcp\"). Requires the
   mcp-json-jackson3 artifact on the classpath.

   Accepts optional :headers and :tls-context from the config map.

   Uses the builder pattern: builder(url).endpoint(path).headers(map)
   .tlsContext(ctx).build()."
  ([url]
   (streamable-http-transport url "/mcp" nil nil))
  ([url endpoint]
   (streamable-http-transport url endpoint nil nil))
  ([url endpoint headers tls-context]
   (when-not (string? url)
     (throw (ex-info "MCP streamable HTTP transport requires a URL string"
                     {:error/type :mcp/transport-invalid
                      :transport/type :http
                      :url (pr-str url)})))
    (let [b (HttpClientStreamableHttpTransport/builder ^String url)]
      (.endpoint b ^String (or endpoint "/mcp"))
      (apply-http-options b headers tls-context)
      (.build b))))

;; --- transport factory -------------------------------------------------------

(defn transport-for
  "Dispatch on a transport config map and return the corresponding
   transport object. Config shapes:

     {:type :stdio  :command \"npx\" :args [\"-y\" \"server\"]}
     {:type :sse    :url \"http://localhost:3000/sse\"}
     {:type :http   :url \"http://localhost:3000\" :endpoint \"/mcp\"}

   Unknown :type throws :mcp/transport-invalid."
  [{:keys [type] :as config}]
  (case type
    :stdio (stdio-transport config)
    :sse   (sse-transport (:url config) (:headers config) (:tls-context config))
    :http  (streamable-http-transport (:url config) (:endpoint config) (:headers config) (:tls-context config))
    (throw (ex-info (str "unknown MCP transport type: " type)
                    {:error/type :mcp/transport-invalid
                     :transport/type type
                     :config (pr-str config)}))))
