(ns evoclj.mcp.codec
  "Single, authoritative MCP codec for evoclj.

  This namespace is THE ONLY implementation of the MCP transport/result
  conversion shared by every consumer (evoclj.provider.mcp-bridge,
  evoclj.mcp.source, and the CLI paths). It exists to satisfy the Single
  Implementation Principle (INV-05): no caller may reflectively reach into
  another module (ns-resolve / requiring-resolve) to obtain these
  functions, and there must be no hand-copied parallel variant that can
  drift.

  Responsibilities:
    - JSON Schema -> Malli conversion (json-schema->malli)
    - REAL-schema predicates + fail-closed gate (real-schema?,
      require-real-schema!)
    - MCP content-block / structuredContent -> plain EDN (content-block->edn,
      java-value->edn, result->edn)

  All values crossing the protocol boundary are plain validated Clojure
  data (Global Constraint 22). :any is ONLY ever returned by
  json-schema->malli when a schema is genuinely empty or absent; every
  consumer is expected to treat :any as a fail-closed signal, never as a
  silently-accepted wildcard (GC-14 / INV-09).

  D1 (Tool value object): the REAL-schema predicate defined here is the
  single source of truth for Tool schema validation. evoclj.tool.specs
  delegates its :input-schema / :output-schema REAL checks to
  real-schema? / require-real-schema! here (INV-05), and stable-descriptor
  in evoclj.mcp.source delegates final descriptor validation to
  evoclj.tool.specs/validate-descriptor, preserving a single chain."
  (:require [evoclj.kernel.error :as err]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.mcp.json-schema :as json-schema])
  (:import [com.fasterxml.jackson.databind ObjectMapper]))
;; forward declaration: the converter recursion (object->malli -> json-schema->malli)
;; is defined just below.
(declare json-schema->malli)

;; ---------------------------------------------------------------------------
;; JSON Schema -> Malli conversion
;; ---------------------------------------------------------------------------

(def ^:private unsupported-json-schema-keys
  "JSON Schema keywords the converter cannot prove equivalent to a Malli
   primitive. When present, the original schema is preserved and validated
   by the native validator (fail-closed), never degraded to :any."
  #{"oneOf" "anyOf" "allOf" "$ref" "$defs"
     "pattern" "format" "dependentSchemas" "unevaluatedProperties"})

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
   the schema is genuinely empty or absent — callers MUST treat that as a
   fail-closed signal, never silently accept it as a wildcard."
  [schema]
  (let [s (cond
            (instance? java.util.Map schema) (canonical/java-value->edn schema)
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
;; REAL-schema predicate + fail-closed gate
;; ---------------------------------------------------------------------------

(defn real-schema?
  "A schema value is REAL (fail-closed) only when it is an explicit,
   declared schema — never `nil`, never the `:any` wildcard, never a bare
   non-schema scalar.

     - a vector  -> a Malli schema (e.g. [:map [:text :string]], [:or ...])
     - a keyword -> a Malli primitive schema (:string, :int, :boolean, ...),
                    EXCEPT `:any` which is the fail-open wildcard and is
                    explicitly rejected.
     - a map     -> either a Malli map schema (:map/:vector/...) or a
                    JSON-schema map (string-keyed with constructors such as
                    \"type\"/\"properties\"/\"$ref\"/...)

   WO-M9 deletes the previous `(or schema :any)` default: defaulting to
   `:any` is a fail-open loophole (GC-14/INV-09), so a missing or `:any`
   schema must be REJECTED, not silently accepted. This predicate mirrors
   the canonical rule used by evoclj.provider.mcp-bridge so there is
   exactly one definition (INV-05)."
  [s]
  (cond
    (vector? s) true
    (keyword? s) (not= :any s)
    (map? s)    (or (contains? s :map) (contains? s :vector)
                    (contains? s :enum) (contains? s :maybe)
                    (contains? s :or) (contains? s :and) (contains? s :fn)
                    (some (fn [[k _]] (and (string? k)
                                           (#{"type" "properties" "$ref"
                                              "oneOf" "anyOf" "allOf"
                                              "items" "enum" "const"}
                                            k)))
                          s))
    :else false))

(defn require-real-schema!
  "Fail-closed gate (WO-M9 / M11): throw :provider/schema-required unless
   `s` is a REAL declared schema. Rejects nil / :any / garbage. Shared by
   the bridge and the MCP source so the rule is single-sourced (INV-05)."
  [which s opts]
  (when-not (real-schema? s)
    (throw (err/error :provider/schema-required
                      (str which " must be a declared schema; missing or :any is not allowed (fail-closed)")
                      {:value (err/sanitize s)
                       :opts (err/sanitize opts)}))))

;; ---------------------------------------------------------------------------
;; content-block / structuredContent -> plain EDN
;; ---------------------------------------------------------------------------

(defn content-block->edn
  "Convert one MCP content-block map into a plain EDN value.

   Output sandboxing: binary/opaque blocks (`:image`, `:audio`,
   `:resource-link`) are replaced with safe placeholders so base64
   binary data and opaque resource blobs never reach the EDN layer;
   `:text` blocks pass through as strings.

   This is THE single implementation shared by bridge and source (INV-05)."
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
  "Single boundary converter for the structuredContent EDN boundary.
   Delegates to evoclj.mcp.canonical/java-value->edn (the sole
   implementation) so there is exactly one converter (INV-05)."
  [v]
  (canonical/java-value->edn v))

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
   provider's execute-request! merges tool/connection/server ids into the
   same `:audit` key.

   Single implementation (INV-05): source and bridge both call this."
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
