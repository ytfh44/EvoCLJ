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

(defn maybe-nilable
  "Wrap `node` in :maybe when the schema declares `nullable: true`.
   (:nilable is not registered in Malli 0.20.1; :maybe is.)"
  [node schema]
  (if (true? (get schema "nullable")) [:maybe node] node))

(defn object->malli
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

(defn string->malli
  [schema]
  (let [min-l (get schema "minLength")
        max-l (get schema "maxLength")
        node (cond
               (and min-l max-l) [:string {:min min-l :max max-l}]
               (some? min-l)      [:string {:min min-l}]
               (some? max-l)      [:string {:max max-l}]
               :else              :string)]
    (maybe-nilable node schema)))

(defn number->malli
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

(defn array->malli
  [schema]
  (let [items (get schema "items")
        node (if (and items (map? items))
               [:vector (json-schema->malli items)]
               [:vector :any])]
    (maybe-nilable node schema)))

(defn wrap-json-schema-validator
  "Fallback for schema constructs we cannot prove equivalent to a Malli
   primitive: preserve the (normalized string-keyed) schema and validate
   it with the native validator inside a Malli :fn. Fail-closed."
  [schema]
  [:fn {:error/message "json-schema"}
   (fn [v] (json-schema/validate schema v))])

(defn json-schema->malli
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
               :mcp/generation  (or (:mcp/generation opts) 0)
               :mcp/captured-at (or (:mcp/captured-at opts) (System/currentTimeMillis))
               :mcp/last-refreshed (or (:mcp/last-refreshed opts) (System/currentTimeMillis))}
        retry-safe? (assoc :retry {:safe? true})
        connection-id (assoc :mcp/connection-id connection-id)
        server-id (assoc :mcp/server-id server-id)))))

;; ---------------------------------------------------------------------------
;; content-block result -> plain Clojure
;; ---------------------------------------------------------------------------

(defn content-block->edn
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

(defn java-value->edn
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

(defn result->edn
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
                   (some? sc) (assoc :mcp/structured-content (java-value->edn sc)))]
    {:value envelope
     :audit audit}))

;; ---------------------------------------------------------------------------
;; the provider
;; ---------------------------------------------------------------------------

(defrecord ToolEntry [descriptor manager conn-key transport-config mcp-name tool-id connection-id server-id]
  proto/Provider
  (describe [_] descriptor)
  (normalize-request [_ intent]
    (let [payload (:payload intent)
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
       :resource   (canonical/canonical-resource tool-id args)
       :args       args}))
  (execute-request! [_ authorized-request]
    (when-not (and (map? authorized-request)
                   (= tool-id (:tool/id authorized-request)))
      (throw (err/error :provider/request-invalid
                        "MCP provider received a non-normalized request"
                        {:value (err/sanitize authorized-request)})))
    (let [args (:args authorized-request)]
      (try
        (let [shared? (some? connection-id)
              ;; WO-M1: get-or-open! returns the managed record itself
              ;; (never a raw client), so it is used directly — same as
              ;; the pool-hit value. No {:client ...} re-wrapping.
              managed (if shared?
                        (let [entry (manager/pool-get manager conn-key)
                              c (:client entry)]
                          (if c c
                            (manager/get-or-open! manager conn-key #(mcp-client/open! (manager/normalize-transport transport-config)))))
                        (mcp-client/open! (manager/normalize-transport transport-config)))
              managed (mcp-client/ensure-open managed default-max-reopen-attempts)]
          (when (mcp-client/closed? managed)
            (throw (err/error :mcp/client-closed
                              "MCP managed client is closed"
                              {:open-count (or (:open-count managed) 0)})))
          (let [client (:client managed)
                raw-result (mcp-client/call-tool client mcp-name args)
                edn-result (result->edn raw-result)
                audit {:mcp/tool-name mcp-name
                       :mcp/connection-id connection-id
                       :mcp/server-id server-id}
                sc (get-in edn-result [:value :mcp/structured-content])
                desc descriptor]
            (when (some? sc)
              (when-not (json-schema/validate (:mcp/output-schema desc) sc)
                (throw (err/error :provider/output-invalid "structuredContent failed mcp/output-schema" {:value (err/sanitize sc)}))))
            (let [env (:value edn-result)]
              (when-not (m/validate (:provider/output-schema desc) env)
                (throw (err/error :provider/output-invalid "envelope failed provider/output-schema" {:value (err/sanitize env)}))))
            (when shared? (try (manager/set-metrics manager conn-key #(-> % (update :call-count (fnil inc 0)) (assoc :latency-ms 0))) (catch Exception _ nil)))
            (update edn-result :audit merge audit)))
        (catch Throwable ex
          (if (= :mcp/tool-error (:error/type (ex-data ex)))
            (throw ex)
            (let [category (:error/type (mcp-client/classify-mcp-error ex))]
              (if (or (= category :mcp/transport-error)
                      (= category :mcp/protocol-error))
                (throw (err/error :provider/transient-error
                                  "MCP provider call-tool failed"
                                  {:tool-name mcp-name
                                   :mcp/connection-id connection-id
                                   :mcp/server-id server-id
                                   :mcp/transport-config (err/sanitize (manager/normalize-transport transport-config))
                                   :cause (err/sanitize ex)}))
                (throw (err/error :provider/execution-failed
                                  "MCP provider call-tool failed"
                                  {:tool-name mcp-name
                                   :mcp/connection-id connection-id
                                   :mcp/server-id server-id
                                   :mcp/transport-config (err/sanitize (manager/normalize-transport transport-config))
                                   :cause (err/sanitize ex)}))))))))))

(defn make-tool-entry
  "Create an immutable ToolEntry. Each entry is a distinct immutable value
   sharing the same manager connection pool. Generation is part of the
   descriptor and never mutated in place."
  [opts]
  (let [descriptor (mcp-tool-descriptor opts)
        transport-cfg (:transport-config opts)
        mcp-name (:tool/mcp-name opts)
        tool-id (:tool/id descriptor)
        connection-id (:connection/id opts)
        server-id (:mcp/server-id opts)
        mgr-atom (mgr opts)
        ck (manager/connection-key (assoc transport-cfg :connection/id connection-id))]
    (when (some? connection-id)
      (manager/acquire mgr-atom ck tool-id))
    (->ToolEntry descriptor mgr-atom ck transport-cfg mcp-name tool-id connection-id server-id)))

(defn mcp-provider
  "Build an MCP-backed provider (immutable ToolEntry). See make-tool-entry."
  [opts]
  (make-tool-entry opts))

(def ^:private legacy-registry (atom {}))

(defn refresh-provider!
  "Deprecated: previously mutated descriptor-atom in place. Now returns a new
   immutable ToolEntry with bumped :mcp/generation and nil :mcp/last-refreshed.
   The original provider instance is unchanged (immutable)."
  [provider]
  (let [desc (proto/describe provider)
        tool-id (:tool/id desc)]
    (when-not tool-id
      (throw (err/error :provider/not-a-provider "provider missing :tool/id" {:provider (err/sanitize provider)})))
    (let [new-desc (-> desc
                       (assoc :mcp/last-refreshed nil)
                       (update :mcp/generation (fnil inc 0))
                       (assoc :mcp/captured-at (System/currentTimeMillis)))
          opts {:tool/id tool-id
                :tool/mcp-name (or (:mcp/name desc) (name tool-id))
                :transport-config {}
                :input-schema (:provider/input-schema desc)
                :output-schema (:provider/output-schema desc)
                :mcp/generation (:mcp/generation new-desc)
                :mcp/captured-at (:mcp/captured-at new-desc)
                :mcp/last-refreshed nil}]
      (let [new-entry (make-tool-entry (merge opts {:mcp/input-schema (:mcp/input-schema desc)
                                                    :mcp/output-schema (:mcp/output-schema desc)}))]
        (let [bumped (assoc new-entry :descriptor new-desc)]
          (swap! legacy-registry assoc tool-id bumped)
          bumped)))))

(defn refresh-all-mcp-providers!
  "Deprecated: previously mutated each descriptor. Now returns map of tool-id -> new descriptor."
  []
  (reduce-kv (fn [m k v]
               (let [new-entry (refresh-provider! v)]
                 (assoc m k (:descriptor new-entry))))
             {}
             @legacy-registry))
(defn dispose! [provider] (let [tool-id (-> provider proto/describe :tool/id)
        desc (proto/describe provider)
        cid (:mcp/connection-id desc)]
    (when cid
      (let [ck2 (manager/connection-key {:connection/id cid :type :stdio})]
        (try (manager/release fallback-manager ck2 tool-id) (catch Throwable _ nil))
        ;; also try generic type lookup via fallback pools
        (doseq [[k _] (:pools @fallback-manager)]
          (when (= cid (second k)) (try (manager/release fallback-manager k tool-id) (catch Throwable _ nil))))))))
