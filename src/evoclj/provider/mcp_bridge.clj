(ns evoclj.provider.mcp-bridge
  "MCP provider bridge: adapts a remote MCP server tool into the
   kernel's Provider protocol.

   The bridge owns ONE MCP connection (an `evoclj.mcp.client/open!`
   managed record). It is constructed from a transport config plus a
   `:tool/mcp-name` (the server-side tool name). The bridge translates
   between EvoCLJ intents and MCP `callTool` requests, and between MCP
   content-block results and plain Clojure EDN.

   Phase 1 (connection lifecycle): the bridge uses managed client
   records with auto-reconnect. When `:connection/id` is provided,
   providers with the same id share a single underlying McpSyncClient
   (connection pooling). All values crossing the protocol boundary are
   plain validated Clojure data (Global Constraint 22).

   Phase 5 (security boundaries): content-block output is sandboxed so
   binary image data and opaque resource blobs never surface as EDN;
   instead safe placeholder metadata is returned. Error data and
   successful results carry MCP-aware audit metadata (tool name,
   connection id, server id) attached as metadata on success and as a
   map key on error."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; connection pool
;; ---------------------------------------------------------------------------

(def ^:private default-max-reopen-attempts 2)

(def ^:private connection-pool
  "Global atom mapping :connection/id -> {:managed <record> :refcount <n>}."
  (atom {}))

(def ^:private provider-refresh-fns
  "Global atom mapping :tool/id -> {:refresh-fn <fn> :descriptor-atom <atom>}."
  (atom {}))

(defn- pool-get
  "Look up a managed client by connection-id. Returns nil when absent
   or closed."
  [connection-id]
  (let [entry (get @connection-pool connection-id)]
    (when (and (map? entry) (not (:closed? (:managed entry))))
      entry)))

(defn- pool-put!
  "Store a managed client under connection-id in the pool.
   Asserts that the transport-config matches any existing entry for
   the same connection-id to prevent cross-server contamination."
  [connection-id managed]
  (let [existing (get @connection-pool connection-id)]
    (when (and existing (not (:closed? (:managed existing))))
      (let [existing-config (some-> existing :managed :transport-config)
            new-config (some-> managed :transport-config)]
        (when (and existing-config new-config
                   (not= existing-config new-config))
          (throw (err/error :mcp/pool-conflict
                            "connection-id already bound to a different transport-config"
                            {:connection/id connection-id
                             :existing existing-config
                             :new new-config}))))))
  (swap! connection-pool assoc connection-id {:managed managed :refcount 1}))

(defn- pool-acquire!
  "Increment the refcount for an existing pool entry. Returns the entry."
  [connection-id]
  (let [entry (get @connection-pool connection-id)]
    (when entry
      (swap! connection-pool assoc connection-id
             (update entry :refcount inc))
      entry)))

(defn- pool-release!
  "Decrement the refcount for a pool entry. When it reaches 0, close the
   managed client and remove the entry. Returns the updated entry or nil."
  [connection-id]
  (let [entry (get @connection-pool connection-id)]
    (when entry
      (let [new-refcount (dec (:refcount entry))]
        (if (pos? new-refcount)
          (do
            (swap! connection-pool assoc connection-id
                   (assoc entry :refcount new-refcount))
            entry)
          (do
            (mcp-client/close! (:managed entry))
            (swap! connection-pool dissoc connection-id)
            nil))))))

(defn- pool-remove!
  "Remove a connection-id from the pool (used on explicit close)."
  [connection-id]
  (when-let [entry (get @connection-pool connection-id)]
    (mcp-client/close! (:managed entry)))
  (swap! connection-pool dissoc connection-id))

(defn shutdown-pool!
  "Close all pooled connections and empty the pool. Intended for host halt."
  []
  (doseq [[_ {:keys [managed]}] @connection-pool]
    (mcp-client/close! managed))
  (reset! connection-pool {}))

;; ---------------------------------------------------------------------------
;; descriptor helper
;; ---------------------------------------------------------------------------

(defn- mcp-tool-descriptor
  "Build the static descriptor for an MCP-backed tool."
  [opts]
  (let [tool-id (:tool/id opts)
        mcp-name (:tool/mcp-name opts)]
    (when-not (keyword? tool-id)
      (throw (err/error :provider/config-invalid
                        "mcp-provider requires :tool/id as a keyword"
                        {:value (err/sanitize opts)})))
    (when-not (string? mcp-name)
      (throw (err/error :provider/config-invalid
                        "mcp-provider requires :tool/mcp-name as a string"
                        {:value (err/sanitize opts)})))
    (let [input-schema  (:input-schema opts)
          output-schema (:output-schema opts)
          retry-safe?   (or (:retry-safe? opts) false)
          connection-id (:connection/id opts)
          server-id     (:mcp/server-id opts)]
      (cond-> {:tool/id         tool-id
               :effect          :remote
               :input-schema    input-schema
               :output-schema   output-schema
               :required-action :invoke
               :version         1}
        retry-safe? (assoc :retry {:safe? true})
        connection-id (assoc :mcp/connection-id connection-id)
        server-id (assoc :mcp/server-id server-id)))))

;; ---------------------------------------------------------------------------
;; content-block result -> plain Clojure
;; ---------------------------------------------------------------------------

(defn- content-block->edn
  "Convert one MCP content-block map into a plain EDN value.

   Output sandboxing: `:image` blocks are replaced with a safe
   placeholder so base64 binary data never reaches the EDN layer;
   `:resource` blocks are reduced to safe metadata keys only
   (`:uri`, `:mimeType`). `:text` blocks pass through as strings."
  [block]
  (case (:content/type block)
    :text (:content/text block)
    :image {:mcp/content-type :image
            :mcp/sandboxed true
            :mime-type (:content/mime-type block)}
    :resource (let [uri (:content/uri block)
                    mime (:content/mime-type block)]
                (cond
                  (and uri mime) {:uri uri :mimeType mime}
                  uri {:uri uri}
                  :else {:mcp/content-type :resource
                         :mcp/sandboxed true}))
    (:content/raw block)))

(defn- result->edn
  "Convert the full call-tool result map into a plain EDN value.

   MCP-aware audit metadata is attached as metadata on successful
   results when the value supports metadata (IObj)."
  [result]
  (let [blocks (:mcp/content result)
        edn-blocks (mapv content-block->edn blocks)
        audit {:mcp/block-count (count blocks)
               :mcp/is-error (:mcp/is-error result)}]
    (case (count edn-blocks)
      1 (let [v (first edn-blocks)]
          (if (instance? clojure.lang.IObj v)
            (with-meta v audit)
            {:value v :mcp/audit audit}))
      (with-meta edn-blocks audit))))

;; ---------------------------------------------------------------------------
;; the provider
;; ---------------------------------------------------------------------------

(defn- build-refresh-fn
  "Build a no-arg fn that forces this provider's descriptor to refresh
   from the remote MCP server on the next call (by resetting the cached
   :mcp/last-refreshed timestamp to nil)."
  [descriptor-atom]
  (fn []
    (reset! descriptor-atom
            (assoc @descriptor-atom :mcp/last-refreshed nil))))

(defn mcp-provider
  "Build an MCP-backed provider.

  Required opts:
    :transport-config - transport config map accepted by transport/transport-for
    :tool/id          - EvoCLJ tool id keyword
    :tool/mcp-name    - server-side MCP tool name string
    :input-schema     - Malli schema for normalized args
    :output-schema    - Malli schema for result value

  Optional:
    :retry-safe?      - boolean, true when the tool is idempotent (default false)
    :connection/id    - keyword; providers sharing this id share a single
                        underlying McpSyncClient (connection pooling).
    :mcp/server-id    - string; isolates this tool within a server
                        namespace for multi-server setups.
    :discovery/auto-register? - boolean, when true the descriptor is
                        refreshed from the remote server on each call
                        (default false)."
  [opts]
  (when-not (contains? opts :transport-config)
    (throw (err/error :provider/config-invalid
                      "mcp-provider requires :transport-config"
                      {:value (err/sanitize opts)})))
  (when-not (contains? opts :tool/id)
    (throw (err/error :provider/config-invalid
                      "mcp-provider requires :tool/id"
                      {:value (err/sanitize opts)})))
  (when-not (contains? opts :tool/mcp-name)
    (throw (err/error :provider/config-invalid
                      "mcp-provider requires :tool/mcp-name"
                      {:value (err/sanitize opts)})))
  (let [descriptor    (mcp-tool-descriptor opts)
        transport-cfg (:transport-config opts)
        mcp-name      (:tool/mcp-name opts)
        tool-id       (:tool/id descriptor)
        connection-id (:connection/id opts)
        shared?       (some? connection-id)
        server-id     (:mcp/server-id opts)
        refresh-ms    (:schema/refresh-interval-ms opts)
        descriptor    (if refresh-ms
                        (assoc descriptor :mcp/last-refreshed (System/currentTimeMillis))
                        descriptor)
        descriptor-atom (atom descriptor)
        client-atom   (atom nil)
        refresh-fn    (build-refresh-fn descriptor-atom)]
    (swap! provider-refresh-fns assoc tool-id {:refresh-fn refresh-fn
                                               :descriptor-atom descriptor-atom})
    (reify proto/Provider
      (describe [_]
        @descriptor-atom)

      (normalize-request [_ intent]
        (let [descriptor @descriptor-atom
              payload (:payload intent)
              args (:args payload)]
          (when-not (boundary/edn-safe? args)
            (throw (err/error :provider/input-invalid
                              "MCP provider input must be plain EDN-safe data"
                              {:value (err/sanitize args)})))
          (when-not (m/validate (:input-schema descriptor) args)
            (throw (err/error :provider/input-invalid
                              "MCP provider input failed input-schema validation"
                              {:value (err/sanitize args)
                               :explanation (err/sanitize (m/explain (:input-schema descriptor) args))})))
          {:tool/id    tool-id
           :resource   {:kind :tool :id tool-id}
           :args       args}))

      (execute-request! [_ authorized-request]
        (when-not (and (map? authorized-request)
                       (= tool-id (:tool/id authorized-request)))
          (throw (err/error :provider/request-invalid
                            "MCP provider received a non-normalized request"
                            {:value (err/sanitize authorized-request)})))
        (let [args (:args authorized-request)]
          (try
            (let [managed (if shared?
                            (or (get (pool-acquire! connection-id) :managed)
                                (let [m (mcp-client/open! transport-cfg)]
                                  (pool-put! connection-id m)
                                  m))
                            (or @client-atom
                                (let [m (mcp-client/open! transport-cfg)]
                                  (reset! client-atom m)
                                  m)))
                  managed (mcp-client/ensure-open managed default-max-reopen-attempts)]
              (when (mcp-client/closed? managed)
                (throw (err/error :mcp/client-closed
                                  "MCP managed client is closed"
                                  {:open-count (or (:open-count managed) 0)})))
              (let [client (:client managed)]
                (when refresh-ms
                  (let [descriptor @descriptor-atom
                        last-refreshed (:mcp/last-refreshed descriptor)
                        now (System/currentTimeMillis)]
                    (when (or (nil? last-refreshed)
                              (>= (- now last-refreshed) (long refresh-ms)))
                      (try
                        (let [tools (mcp-client/list-tools client)
                              matching (some #(when (= mcp-name (:mcp/name %)) %) tools)]
                          (when matching
                            (reset! descriptor-atom
                                    (assoc descriptor
                                           :input-schema (:mcp/input-schema matching)
                                           :output-schema (:mcp/output-schema matching)
                                           :mcp/last-refreshed (System/currentTimeMillis)))))
                        (catch Throwable _ nil)))))
                (let [raw-result (mcp-client/call-tool client mcp-name args)
                      edn-result (result->edn raw-result)
                      audit {:mcp/tool-name mcp-name
                             :mcp/connection-id connection-id
                             :mcp/server-id server-id}]
                  (if (:mcp/is-error raw-result)
                    (assoc edn-result :mcp/audit (merge (:mcp/audit edn-result) audit))
                    (with-meta edn-result (merge (meta edn-result) {:mcp/audit audit}))))))
            (catch Throwable ex
              (if (= :mcp/tool-error (:error/type (ex-data ex)))
                (throw ex)
                (throw (err/error :provider/transient-error
                                  "MCP provider call-tool failed"
                                  {:tool-name mcp-name
                                   :mcp/connection-id connection-id
                                   :mcp/server-id server-id
                                   :mcp/transport-config (err/sanitize transport-cfg)
                                   :cause (err/sanitize ex)}))))))))))

(defn refresh-provider!
  "Force a schema refresh for an MCP-backed provider by resetting its
   cached :mcp/last-refreshed timestamp. The next call to describe or
   execute-request! will re-fetch the descriptor from the remote server
   (when :schema/refresh-interval-ms is configured). Returns nil."
  [provider]
  (let [tool-id (-> provider proto/describe :tool/id)
        entry (get @provider-refresh-fns tool-id)]
    (when-let [refresh-fn (-> entry :refresh-fn)]
      (refresh-fn)
      nil)))

(defn refresh-all-mcp-providers!
  "Force a schema refresh for all registered MCP providers. Returns a
   map of tool ids to their current descriptors."
  []
  (reduce-kv (fn [acc tool-id {:keys [refresh-fn descriptor-atom]}]
               (when refresh-fn
                 (refresh-fn))
               (assoc acc tool-id @descriptor-atom))
             {}
             @provider-refresh-fns))
