(ns evoclj.mcp.json-schema
  (:require [cheshire.core :as json])
  (:import [com.networknt.schema JsonSchemaFactory SpecVersion$VersionFlag JsonSchema]
           [com.fasterxml.jackson.databind ObjectMapper]))

(def ^:private mapper (ObjectMapper.))
(def ^:private factory (JsonSchemaFactory/getInstance SpecVersion$VersionFlag/V202012))

(def ^:private max-nodes 1000)
(def ^:private max-depth 20)
(def ^:private max-regex-ms 100)

(defn- node-count [x]
  (cond (map? x) (inc (reduce + 0 (map (fn [[_ v]] (node-count v)) x)))
        (vector? x) (inc (reduce + 0 (map node-count x)))
        (seq? x) (inc (reduce + 0 (map node-count x)))
        :else 1))

(defn- depth [x]
  (cond (map? x) (if (empty? x) 1 (inc (apply max (map (comp depth val) x))))
        (vector? x) (if (empty? x) 1 (inc (apply max (map depth x))))
        :else 1))

(defn- has-external-ref? [schema]
  (let [s (str schema)]
    (or (clojure.string/includes? s "\"$ref\"")
        (clojure.string/includes? s "'$ref'"))))

(defn- deny-external-ref! [schema]
  (when (has-external-ref? schema)
    ;; only allow internal refs starting with #
    (let [refs (re-seq #"\"\$ref\"\s*:\s*\"([^\"]+)\"" (pr-str schema))]
      (doseq [[_ v] refs]
        (when-not (clojure.string/starts-with? v "#")
          (throw (ex-info "external $ref denied" {:ref v})))))))

(defn validate
  [schema value]
  (when (> (node-count schema) max-nodes)
    (throw (ex-info "schema node-count budget exceeded" {:max max-nodes})))
  (when (> (depth schema) max-depth)
    (throw (ex-info "schema depth budget exceeded" {:max max-depth})))
  (deny-external-ref! schema)
  (let [schema-json (json/generate-string schema)
        value-json (json/generate-string value)
        ^JsonSchema js (.getSchema factory (.readTree mapper ^String schema-json))
        node (.readTree mapper ^String value-json)
        errors (.validate js node)]
    (empty? errors)))
