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
   binary image/audio data and opaque resource blobs never surface as
   EDN; instead safe placeholder metadata is returned. Results carry
   MCP-aware audit data (tool name, connection id, server id, block
   count, is-error) as an explicit `:audit` map key — never as Clojure
   metadata."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.mcp.json-schema :as json-schema]
            [evoclj.mcp.manager :as manager]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [malli.core :as m]))

;; ---------------------------------------------------------------------------
;; JSON Schema -> Malli conversion
;; ---------------------------------------------------------------------------

(def ^:private unsupported-json-schema-keys
  "JSON Schema keywords the converter cannot prove equivalent to a Malli
   primitive. When present, the original schema is preserved and validated
   by the native validator (fail-closed), never degraded to :any."
  #{"oneOf" "anyOf" "allOf" "$ref" "$defs"
    "pattern" "format" "dependentSchemas" "unevaluatedProperties"})

(declare json-schema->malli)

;; defined later in this namespace (content-block -> EDN section)
(declare java-value->edn)

(defn- maybe-nilable
  "Wrap `node` in :maybe when the schema declares `nullable: true`.
   (:nilable is not registered in Malli 0.20.1; :maybe is.)"
  [node schema]
  (if (true? (get schema "nullable")) [:maybe node] node))

(defn- object->malli
  [schema]
  (let [props (get schema "properties" {})
        required (set (get schema "required" []))
        closed? (false? (get schema "additionalProperties" true))
        entries (map (fn [[k v]]
                       (if (contains? required k)
                         [k (json-schema->malli v)]
                         [k {:optional true} (json-schema->malli v)]))
                     props)
        m (if closed?
            (into [:map {:closed true}] entries)
            (into [:map] entries))]
    (maybe-nilable m schema)))

(defn- string->malli
  [schema]
  (let [min-l (get schema "minLength")
        max-l (get schema "maxLength")
        node (cond
               (and min-l max-l) [:string {:min min-l :max max-l}]
               (some? min-l)      [:string {:min min-l}]
               (some? max-l)      [:string {:max max-l}]
               :else              :string)]
    (maybe-nilable node schema)))

(defn- number->malli
  [schema]
  (let [integer? (= "integer" (get schema "type"))
        min-v (get schema "minimum")
        max-v (get schema "maximum")
        opts (cond-> {}
               (some? min-v) (assoc :min min-v)
               (some? max-v) (assoc :max max-v))
        ;; "integer" -> :int. JSON "number" accepts both integers and
        ;; floats; Malli's :number is not registered, so union :int and
        ;; :double (both honor :min/:max).
        node (if integer?
               (if (seq opts) [:int opts] :int)
               (if (seq opts) [:or [:int opts] [:double opts]] [:or :int :double]))]
    (maybe-nilable node schema)))

(defn- array->malli
  [schema]
  (let [items (get schema "items")
        node (if (and items (map? items))
               [:vector (json-schema->malli items)]
               [:vector :any])]
    (maybe-nilable node schema)))

(defn- wrap-json-schema-validator
  "Fallback for schema constructs we cannot prove equivalent to a Malli
   primitive: preserve the (normalized string-keyed) schema and validate
   it with the native validator inside a Malli :fn. Fail-closed."
  [schema]
  [:fn {:error/message "json-schema"}
   (fn [v] (json-schema/validate schema v))])

(defn- json-schema->malli
  "Convert a (string-keyed) JSON Schema map to an equivalent Malli schema.

   Operates on string-keyed maps (the shape produced by
   evoclj.mcp.client/java-schema->clj) and also accepts a
   java.util.Map with string keys.

   Handles the common MCP subset: object (properties + required +
   additionalProperties), string, integer, number, boolean, array (items),
   enum, const, null/nullable, and minLength/maxLength/minimum/maximum.

   Fail-closed: constructs the converter cannot prove equivalent to a
   Malli primitive (oneOf/anyOf/allOf/$ref/$defs/pattern/format/
   dependentSchemas/unevaluatedProperties) preserve the original schema
   and validate it with the native validator. :any is returned ONLY when
   the schema is genuinely empty or absent."
  [schema]
  (let [s (cond
            (instance? java.util.Map schema) (java-value->edn schema)
            (map? schema) schema
            :else nil)]
    (cond
      (nil? s) :any
      (empty? s) :any
      (some #(contains? s %) unsupported-json-schema-keys)
      (wrap-json-schema-validator s)
      (contains? s "enum")
      (maybe-nilable (into [:enum] (get s "enum")) s)
      (contains? s "const")
      (maybe-nilable [:enum (get s "const")] s)
      (= "object"  (get s "type")) (object->malli s)
      (= "string"  (get s "type")) (string->malli s)
      (= "integer" (get s "type")) (number->malli s)
      (= "number"  (get s "type")) (number->malli s)
      (= "boolean" (get s "type")) (maybe-nilable :boolean s)
      (= "array"   (get s "type")) (array->malli s)
      (= "null"    (get s "type")) (maybe-nilable :nil s)
      :else
      (if (seq s)
        (wrap-json-schema-validator s)
        :any))))

;; ---------------------------------------------------------------------------
;; connection pool
;; ---------------------------------------------------------------------------

(def ^:private default-max-reopen-attempts 2)

;; Global atoms removed — state is host-owned via :mcp/manager. Fallback for non-Integrant use.
(def ^:private fallback-manager (manager/create-manager))
(defn- mgr [opts] (or (:manager opts) (:mcp/manager opts) fallback-manager))
(defn shutdown-pool! [] (manager/shutdown! fallback-manager))

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
          mcp-in  (:mcp/input-schema opts (:mcp/input-schema-json opts))
          mcp-out (:mcp/output-schema opts (:mcp/output-schema-json opts))
          retry-safe?   (or (:retry-safe? opts) false)
          connection-id (:connection/id opts)
          server-id     (:mcp/server-id opts)]
      (cond-> {:tool/id         tool-id
               :effect          :remote
               :input-schema    (or input-schema :any)
               :output-schema   (or output-schema :any)
               :provider/input-schema (or input-schema :any)
               :provider/output-schema (or output-schema :any)
               :mcp/input-schema (or mcp-in {})
               :mcp/output-schema (or mcp-out :any)
               :mcp/input-schema-json (or mcp-in {})
               :mcp/output-schema-json (or mcp-out :any)
               :mcp/schema-source (if mcp-in :json-schema-fallback :malli)
               :required-action :invoke
               :version         1
               :mcp/generation  0
               :mcp/captured-at (System/currentTimeMillis)}
        retry-safe? (assoc :retry {:safe? true})
        connection-id (assoc :mcp/connection-id connection-id)
        server-id (assoc :mcp/server-id server-id)))))

;; ---------------------------------------------------------------------------
;; content-block result -> plain Clojure
;; ---------------------------------------------------------------------------

(defn- content-block->edn
  "Convert one MCP content-block map into a plain EDN value.

   Output sandboxing: binary/opaque blocks (`:image`, `:audio`,
   `:resource-link`) are replaced with safe placeholders so base64
   binary data and opaque resource blobs never reach the EDN layer;
   `:text` blocks pass through as strings."
  [block]
  (case (:content/type block)
    :text (:content/text block)
    :image {:mcp/content-type :image
            :mcp/sandboxed true
            :mime-type (:content/mime-type block)}
    :audio {:mcp/content-type :audio
            :mcp/sandboxed true
            :mime-type (:content/mime-type block)}
    :resource-link {:mcp/content-type :resource-link
                    :mcp/sandboxed true
                    :uri (:content/uri block)
                    :mime-type (:content/mime-type block)}
    :resource (let [uri (:content/uri block)
                    mime (:content/mime-type block)]
                (cond
                  (and uri mime) {:uri uri :mimeType mime}
                  uri {:uri uri}
                  :else {:mcp/content-type :resource
                         :mcp/sandboxed true}))
    (:content/raw block)))

(defn- java-value->edn
  "Recursively convert a value returned by the MCP Java SDK 2.0
   (structuredContent is a java.util.Map with STRING keys) into plain
   persistent Clojure data:

     java.util.Map        -> persistent map (string keys preserved)
     java.util.List       -> vector
     java.util.Set        -> set
     java.util.Collection -> vector
     strings/numbers/bool -> passed through unchanged

   The result is always plain EDN-safe data for the protocol boundary."
  [v]
  (cond
    (instance? java.util.Map v)
    (into {} (map (fn [[k val]] [k (java-value->edn val)]) v))

    (instance? java.util.List v)
    (mapv java-value->edn v)

    (instance? java.util.Set v)
    (into #{} (map java-value->edn v))

    (instance? java.util.Collection v)
    (mapv java-value->edn v)

    :else v))

(defn- result->edn
  "Convert the full call-tool result map into a plain EDN envelope.

   ALWAYS returns a plain-data map of the shape
   `{:value <envelope> :audit <map>}` — audit is an EXPLICIT key, never
   Clojure metadata.

   The `<envelope>` carries BOTH channels:
     (a) `:mcp/model-content`      — the sandboxed content blocks
         (vector of content-block->edn results), and
     (b) `:mcp/structured-content` — the server's structured content
         (java.util.Map with string keys), when present, normalized by
         java-value->edn.

   The `:audit` map carries `:mcp/block-count` and `:mcp/is-error`; the
   provider's execute-request! merges tool/connection/server ids into
   the same `:audit` key."
  [result]
  (let [blocks (:mcp/content result)
        edn-blocks (mapv content-block->edn blocks)
        sc (:mcp/structured-content result)
        audit {:mcp/block-count (count blocks)
               :mcp/is-error (boolean (:mcp/is-error result))}
        envelope (cond-> {:mcp/model-content edn-blocks
                          :mcp/tool-status (or (:mcp/tool-status result) (if (:mcp/is-error result) :error :ok))
                          :mcp/is-error (boolean (:mcp/is-error result))}
                   (some? sc) (assoc :mcp/structured-content (java-value->edn sc))
                   (true) (assoc :mcp/is-error (boolean (:mcp/is-error result))))]
    {:value envelope
     :audit audit}))

;; ---------------------------------------------------------------------------
;; the provider
;; ---------------------------------------------------------------------------

(defn- build-refresh-fn
  "Build a no-arg fn that forces this provider's descriptor to refresh
   from the remote MCP server on the next call (by resetting the cached
   :mcp/last-refreshed timestamp to nil) and atomically bumping
   :mcp/generation so a frozen CallContract can detect staleness.
   Generation bump is via swap! — no stale-read lost update."
  [descriptor-atom]
  (fn []
    (swap! descriptor-atom
           (fn [d]
             (-> d
                 (assoc :mcp/last-refreshed nil)
                 (update :mcp/generation (fnil inc 0))
                 (assoc :mcp/captured-at (System/currentTimeMillis)))))))

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
                        namespace for multi-server setups."
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
        descriptor    (cond-> descriptor
                        (not (contains? descriptor :mcp/generation)) (assoc :mcp/generation 0)
                        (not (contains? descriptor :mcp/captured-at)) (assoc :mcp/captured-at (System/currentTimeMillis))
                        refresh-ms (assoc :mcp/last-refreshed (System/currentTimeMillis))
                        (and (not refresh-ms) (not (contains? descriptor :mcp/last-refreshed))) (assoc :mcp/last-refreshed (System/currentTimeMillis)))
        descriptor-atom (atom descriptor)
        client-atom   (atom nil)
        refresh-fn    (build-refresh-fn descriptor-atom)
        mgr-atom (mgr opts)
        ck (manager/connection-key (assoc transport-cfg :connection/id connection-id))
        _ (when shared? (manager/acquire mgr-atom ck tool-id))]
    (swap! mgr-atom update :refresh-registry assoc tool-id {:refresh-fn refresh-fn :descriptor-atom descriptor-atom})
    (reify proto/Provider
      (describe [_]
        @descriptor-atom)

      (normalize-request [_ intent]
        (let [descriptor @descriptor-atom
              payload (:payload intent)
              raw-args (:args payload)
              args (canonical/value->canonical raw-args)]
          (when-not (boundary/edn-safe? raw-args)
            (throw (err/error :provider/input-invalid
                              "MCP provider input must be plain EDN-safe data"
                              {:value (err/sanitize raw-args)})))
          (when-not (m/validate (:provider/input-schema descriptor) raw-args)
            (throw (err/error :provider/input-invalid
                              "MCP provider input failed input-schema validation"
                              {:value (err/sanitize raw-args)
                               :explanation (err/sanitize (m/explain (:provider/input-schema descriptor) raw-args))})))
          (when-not (json-schema/validate (:mcp/input-schema descriptor) args)
            (throw (err/error :provider/input-invalid
                              "MCP provider input failed JSON Schema validation"
                              {:value (err/sanitize args)})))
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
                            (let [entry (manager/pool-get mgr-atom ck)
                                  c (:client entry)]
                              (if c c
                                (let [cl (manager/get-or-open! mgr-atom ck #(mcp-client/open! (manager/normalize-transport transport-cfg)))]
                                  {:client cl :transport-config transport-cfg})))
                            (or @client-atom
                                (let [m (mcp-client/open! (manager/normalize-transport transport-cfg))]
                                  (reset! client-atom m) m)))
                  managed (mcp-client/ensure-open managed default-max-reopen-attempts)]
              (when (mcp-client/closed? managed)
                (throw (err/error :mcp/client-closed
                                  "MCP managed client is closed"
                                  {:open-count (or (:open-count managed) 0)})))
              (let [client (:client managed)
                    ;; Step 1: inline refresh REMOVED — refresh must happen
                    ;; BEFORE call-started (dispatch pipeline's refresh-if-needed).
                    ;; Any reset! after managed acquisition would violate
                    ;; D_normalize=D_authorize=D_execute=D_validate.
                    ]
                (let [raw-result (mcp-client/call-tool client mcp-name args)
                      edn-result (result->edn raw-result)
                      audit {:mcp/tool-name mcp-name
                             :mcp/connection-id connection-id
                             :mcp/server-id server-id}
                      sc (get-in edn-result [:value :mcp/structured-content])
                      desc @descriptor-atom]
                  (when (some? sc)
                    (when-not (json-schema/validate (:mcp/output-schema desc) sc)
                      (throw (err/error :provider/output-invalid "structuredContent failed mcp/output-schema" {:value (err/sanitize sc)}))))
                  (let [env (:value edn-result)]
                    (when-not (m/validate (:provider/output-schema desc) env)
                      (throw (err/error :provider/output-invalid "envelope failed provider/output-schema" {:value (err/sanitize env)}))))
                  (when shared? (manager/set-metrics mgr-atom ck #(-> % (update :call-count (fnil inc 0)) (assoc :latency-ms 0))))
                  (update edn-result :audit merge audit))))
            (catch Throwable ex
              (if (= :mcp/tool-error (:error/type (ex-data ex)))
                (throw ex)
                (let [category (:error/type (mcp-client/classify-mcp-error ex))]
                  (if (or (= category :mcp/transport-error)
                          (= category :mcp/protocol-error))
                    ;; transient/retryable: transport or protocol failures
                    (throw (err/error :provider/transient-error
                                      "MCP provider call-tool failed"
                                      {:tool-name mcp-name
                                       :mcp/connection-id connection-id
                                       :mcp/server-id server-id
                                       :mcp/transport-config (err/sanitize (manager/normalize-transport transport-cfg))
                                       :cause (err/sanitize ex)}))
                    ;; permanent: business/config/unknown failures are NOT retried
                    (throw (err/error :provider/execution-failed
                                      "MCP provider call-tool failed"
                                      {:tool-name mcp-name
                                       :mcp/connection-id connection-id
                                       :mcp/server-id server-id
                                       :mcp/transport-config (err/sanitize (manager/normalize-transport transport-cfg))
                                       :cause (err/sanitize ex)}))))))))))))

(defn refresh-provider! [provider]
  (let [tool-id (-> provider proto/describe :tool/id)
        entry (or (get (:refresh-registry @fallback-manager) tool-id) (get (:refresh-registry @fallback-manager) tool-id))]
    (when-let [f (:refresh-fn entry)] (f) nil)))
(defn refresh-all-mcp-providers! []
  (reduce-kv (fn [a k {:keys [refresh-fn descriptor-atom]}] (when refresh-fn (refresh-fn)) (assoc a k @descriptor-atom)) {} (:refresh-registry @fallback-manager)))
(defn dispose! [provider] (let [tool-id (-> provider proto/describe :tool/id) ck2 (manager/connection-key {:connection/id (-> provider proto/describe :mcp/connection-id)})] (manager/release fallback-manager ck2 tool-id)))
