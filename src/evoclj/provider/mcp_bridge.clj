(ns evoclj.provider.mcp-bridge
  "MCP provider bridge: adapts a remote MCP server tool into the
   kernel's Provider protocol.

   The bridge owns ONE MCP connection (an `evoclj.mcp.client/open!`
   client). It is constructed from a transport config plus a
   `:tool/mcp-name` (the server-side tool name). The bridge translates
   between EvoCLJ intents and MCP `callTool` requests, and between MCP
   content-block results and plain Clojure EDN.

   All values crossing the protocol boundary are plain validated Clojure
   data (Global Constraint 22)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

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
          retry-safe?   (or (:retry-safe? opts) false)]
      (cond-> {:tool/id         tool-id
               :effect          :remote
               :input-schema    input-schema
               :output-schema   output-schema
               :required-action :invoke}
        retry-safe? (assoc :retry {:safe? true})
        mcp-name    (assoc :mcp/tool-name mcp-name)))))

;; ---------------------------------------------------------------------------
;; content-block result -> plain Clojure
;; ---------------------------------------------------------------------------

(defn- content-block->edn
  "Convert one MCP content-block map into a plain EDN value."
  [block]
  (case (:content/type block)
    :text (:content/text block)
    :image (with-meta block {:mcp/content-type :image})
    :resource (with-meta block {:mcp/content-type :resource})
    (:content/raw block)))

(defn- result->edn
  "Convert the full call-tool result map into a plain EDN value."
  [result]
  (let [blocks (:mcp/content result)
        edn-blocks (mapv content-block->edn blocks)]
    (if (:mcp/is-error result)
      {:error :mcp/tool-error
       :content edn-blocks}
      (case (count edn-blocks)
        1 (first edn-blocks)
        edn-blocks))))

;; ---------------------------------------------------------------------------
;; the provider
;; ---------------------------------------------------------------------------

(defn mcp-provider
  "Build an MCP-backed provider.

  Required opts:
    :transport-config - transport config map accepted by transport/transport-for
    :tool/id          - EvoCLJ tool id keyword
    :tool/mcp-name    - server-side MCP tool name string
    :input-schema     - Malli schema for normalized args
    :output-schema    - Malli schema for result value

  Optional:
    :retry-safe?      - boolean, true when the tool is idempotent (default false)"
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
        client-atom   (atom nil)]
    (reify proto/Provider
      (describe [_]
        descriptor)

      (normalize-request [_ intent]
        (let [payload (:payload intent)]
          (when-not (boundary/edn-safe? payload)
            (throw (err/error :provider/input-invalid
                              "MCP provider input must be plain EDN-safe data"
                              {:value (err/sanitize payload)})))
          (when-not (m/validate (:input-schema descriptor) payload)
            (throw (err/error :provider/input-invalid
                              "MCP provider input failed input-schema validation"
                              {:value (err/sanitize payload)
                               :explanation (err/sanitize (m/explain (:input-schema descriptor) payload))})))
          {:tool/id    tool-id
           :resource   {:kind :mcp-tool :id mcp-name}
           :args       payload}))

      (execute-request! [_ authorized-request]
        (when-not (and (map? authorized-request)
                       (= tool-id (:tool/id authorized-request)))
          (throw (err/error :provider/request-invalid
                            "MCP provider received a non-normalized request"
                            {:value (err/sanitize authorized-request)})))
        (let [args (:args authorized-request)]
          (try
            (let [client (or @client-atom
                             (let [c (mcp-client/open! transport-cfg)]
                               (reset! client-atom c)
                               c))]
              (result->edn (mcp-client/call-tool client mcp-name args)))
            (catch Throwable ex
              (throw (err/error :provider/transient-error
                                "MCP provider call-tool failed"
                                {:tool-name mcp-name
                                 :cause (err/sanitize ex)}))))))))
)