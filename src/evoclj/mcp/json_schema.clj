(ns evoclj.mcp.json-schema
  (:require [cheshire.core :as json])
  ;; networknt json-schema-validator 3.0.0 (required by MCP Java SDK 2.0.0)
  ;; replaced JsonSchemaFactory/SpecVersion/JsonSchema with the SchemaRegistry
  ;; API; validation now goes through JSON strings via InputFormat/JSON.
  (:import [com.networknt.schema InputFormat Schema SchemaRegistry]
           [com.networknt.schema.dialect Dialects]))

(def ^:private prewarm-failure
  "Diagnostic for a failed load-time prewarm (nil when warm-up succeeded).
   Prewarm exists only to pay first-call classloading cost; its failure must
   never abort loading this namespace, on which mcp.source and mcp-bridge
   depend unconditionally."
  (atom nil))

(defn- new-registry ^SchemaRegistry []
  (SchemaRegistry/withDefaultDialect (Dialects/getDraft202012)))

(def ^:private registry
  (let [r (new-registry)]
    ;; Warm both validation paths at load time: on 3.0.0 the first
    ;; getSchema+validate pays ~450ms of classloading (tools.jackson 3 +
    ;; dialect machinery), which otherwise trips the 200ms per-call budget
    ;; below and would make the first real tool call input-invalid.
    ;; Best-effort: a throwing prewarm is contained here so namespace load
    ;; never fails; callers merely pay the cold-start cost once.
    (try
      (let [s (.getSchema r "{\"type\":\"object\",\"properties\":{\"n\":{\"type\":\"integer\"}},\"required\":[\"n\"]}"
                          InputFormat/JSON)]
        (.validate s "{}" InputFormat/JSON)
        (.validate s "{\"n\":\"x\"}" InputFormat/JSON))
      (catch Throwable t
        (let [diag {:class (.getName (class t))
                    :message (.getMessage t)}]
          (reset! prewarm-failure diag)
          (binding [*out* *err*]
            (println "[evoclj.mcp.json-schema] prewarm failed; continuing un-warmed:"
                     (pr-str diag))))))
    r))

(def ^:private max-nodes 1000)
(def ^:private max-depth 20)
(def ^:private max-regex-count 10)
(def ^:private max-time-ms 200)

(defn- node-count [x]
  (cond (map? x) (inc (reduce + 0 (map (fn [[_ v]] (node-count v)) x)))
        (vector? x) (inc (reduce + 0 (map node-count x)))
        (seq? x) (inc (reduce + 0 (map node-count x)))
        :else 1))

(defn- depth [x]
  (cond (map? x) (if (empty? x) 1 (inc (apply max (map (comp depth val) x))))
        (vector? x) (if (empty? x) 1 (inc (apply max (map depth x))))
        :else 1))

;; Budget/ref guards below inspect the serialized JSON text — exactly the
;; bytes handed to the validator. pr-str is the wrong basis: keyword keys
;; print unquoted (:pattern -> 0 matches) and EDN map entries print
;; space-separated ("$ref" "url"), so both checks were blind to them.

(defn- regex-count [schema-json] (count (re-seq #"\"pattern\"" schema-json)))

(defn- has-external-ref? [schema-json]
  (clojure.string/includes? schema-json "\"$ref\""))

(defn- deny-external-ref! [schema-json]
  (when (has-external-ref? schema-json)
    ;; JSON colon form only: cheshire always emits "$ref":"..." here.
    (doseq [[_ v] (re-seq #"\"\$ref\"\s*:\s*\"([^\"]+)\"" schema-json)]
      (when-not (clojure.string/starts-with? v "#")
        (throw (ex-info "external $ref denied" {:ref v}))))))

(defn validate [schema value]
  (let [start (System/nanoTime)
        ;; Serialize once, up front; budgets and the ref guard share this
        ;; result with the validator call (no second pass).
        schema-json (json/generate-string schema)
        value-json (json/generate-string value)]
    (when (> (regex-count schema-json) max-regex-count)
      (throw (ex-info "regex budget exceeded" {:max max-regex-count})))
    (when (> (node-count schema) max-nodes)
      (throw (ex-info "schema node-count budget exceeded" {:max max-nodes})))
    (when (> (depth schema) max-depth)
      (throw (ex-info "schema depth budget exceeded" {:max max-depth})))
    (deny-external-ref! schema-json)
    (let [^Schema js (.getSchema registry ^String schema-json InputFormat/JSON)
          errors (.validate js ^String value-json InputFormat/JSON)
          elapsed (/ (- (System/nanoTime) start) 1e6)]
      (when (> elapsed max-time-ms)
        (throw (ex-info "validation time budget exceeded" {:elapsed-ms elapsed})))
      (empty? errors))))
