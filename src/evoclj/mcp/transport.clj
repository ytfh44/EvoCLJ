(ns evoclj.mcp.transport
  "Thin Clojure wrapper over the MCP Java SDK transport constructors.

   Each public function builds and returns a Java transport object; the
   caller owns the lifecycle (open/close). No MCP protocol logic lives
   here — just constructor calls with keyword-to-Java-enum coercion for
   the stdio command map.

   Phase 3 (transport coverage): stdio accepts :cwd and :env; SSE/HTTP
   transports accept :headers; TLS configuration is supported via
   :tls-context."
  (:import
   [io.modelcontextprotocol.client.transport
    StdioClientTransport
    HttpClientSseClientTransport
    HttpClientStreamableHttpTransport
    ServerParameters$Builder
    ServerParameters]
   [io.modelcontextprotocol.client.transport.customizer
    McpSyncHttpClientRequestCustomizer]
   [io.modelcontextprotocol.json.jackson3
    JacksonMcpJsonMapperSupplier]
   [io.modelcontextprotocol.json
    McpJsonMapperSupplier]
   [java.nio.file Files Paths]
   [java.util Map HashMap]
   [java.util.function Consumer]
   [java.net.http HttpRequest$Builder HttpClient$Builder]
   [javax.net.ssl SSLContext]))

(defn- config-diag
  "Return a non-secret diagnostic summary of a transport config.
   Never embeds secret values (:env values, :headers values, tokens).
   Used in error payloads instead of (pr-str config) to avoid leaking
   secrets into exception boundaries."
  [config]
  {:transport/type (:type config)
   :connection/id (:connection/id config)
   :command-present? (boolean (:command config))
   :args-count (count (or (:args config) []))
   :env-keys (sort (keys (or (:env config) {})))
   :header-names (sort (keys (or (:headers config) {})))})
;; --- stdio -------------------------------------------------------------------

(defn- build-server-parameters
  "Build a ServerParameters from a plain Clojure map {:command <string>
   :args [<strings>] :cwd <string?> :env {<string> <string>}}.
   SDK 2.0.0 API: ServerParameters$Builder has env(Map) and addEnvVar,
   but NO environment(Map) and NO directory(Path). If :cwd is present
   we fail closed with a typed error (honest fail-closed vs. silent drop)."
  [{:keys [command args cwd env] :as config}]
  (let [b (ServerParameters/builder ^String command)]
    (when (seq args)
      (.args ^ServerParameters$Builder b (into-array String args)))
    ;; SDK 2.0.0: ServerParameters$Builder has NO .directory method.
    ;; Fail closed with honest typed error instead of silent misconfig.
    (when cwd
      (throw (ex-info "MCP stdio :cwd is unsupported on MCP Java SDK 2.0.0 (no ServerParameters directory setter)"
                      {:error/type :mcp/transport-invalid
                       :transport/type :stdio
                       :config-diag (config-diag config)})))
    (when (and env (map? env))
      (.env ^ServerParameters$Builder b
            ^java.util.Map (java.util.HashMap. ^java.util.Map env)))
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
                     :config-diag (config-diag config)})))
  (let [params (build-server-parameters config)
        ^McpJsonMapperSupplier supplier (JacksonMcpJsonMapperSupplier.)
        mapper (.get ^McpJsonMapperSupplier supplier)]
    (StdioClientTransport. ^ServerParameters params ^McpJsonMapper mapper)))

(defn- header-customizer
  "SDK 2.0.0 request customizer that stamps `headers` onto every outgoing
   HTTP request. Named (not inline) so it is directly testable against a
   real HttpRequest$Builder without a network round-trip."
  [headers]
  (reify io.modelcontextprotocol.client.transport.customizer.McpSyncHttpClientRequestCustomizer
    (customize [_ rb _method _uri _body _ctx]
      (doseq [[k v] headers]
        (.setHeader ^java.net.http.HttpRequest$Builder rb
                    ^String k ^String v)))))

(defn- apply-http-options
  "Apply optional :headers and :tls-context to an HTTP client transport
   builder (SSE or Streamable HTTP). SDK 2.0.0 API: no .headers(Map) or
   .tlsContext(SSLContext) — use httpRequestCustomizer for headers and
   customizeClient for TLS. Returns the builder."
  [builder headers tls-context]
  (let [builder (cond-> builder
                  (and (map? headers) (seq headers))
                  (.httpRequestCustomizer (header-customizer headers)))]
    (cond
      (instance? javax.net.ssl.SSLContext tls-context)
      (.customizeClient builder
                        (reify java.util.function.Consumer
                          (accept [_ cb]
                            (.sslContext ^java.net.http.HttpClient$Builder cb tls-context))))
      (and (map? tls-context) (seq (:trust-managers tls-context)))
      (let [tm-array (into-array javax.net.ssl.TrustManager
                                 (:trust-managers tls-context))
            ctx (javax.net.ssl.SSLContext/getInstance "TLS")]
        (.init ctx nil tm-array (java.security.SecureRandom.))
        (.customizeClient builder
                          (reify java.util.function.Consumer
                            (accept [_ cb]
                              (.sslContext ^java.net.http.HttpClient$Builder cb ctx)))))
      :else builder)))

(defn sse-transport
  "Build an HttpClientSseClientTransport for the given SSE endpoint URL
   (e.g. \"http://localhost:3000/sse\").

   DEPRECATED: HTTP+SSE is deprecated in MCP 2025-03-26 and later.
   Prefer `streamable-http-transport` for new deployments. SSE is
   retained only for backward compatibility with legacy servers.

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
                     :config-diag (config-diag config)}))))
