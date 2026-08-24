(ns evoclj.mcp.canonical
  (:require [clojure.string :as str]))

(defn normalize-path [s]
  (when (string? s)
    (let [abs? (.startsWith s "/")
          segs (->> (str/split s #"/") (remove #{"" "."})
                    (reduce (fn [a seg] (if (= seg "..") (if (seq a) (pop a) a) (conj a seg))) []))]
      (str (when abs? "/") (str/join "/" segs)))))

(defn value->canonical [v]
  (cond
    (map? v) (into {} (map (fn [[k val]] [(if (keyword? k) (if-let [ns (namespace k)] (str ns "/" (name k)) (name k)) (str k)) (value->canonical val)]) v))
    (vector? v) (mapv value->canonical v)
    (seq? v) (map value->canonical v)
    (set? v) (into #{} (map value->canonical v))
    :else v))

(defn canonical-resource [tool-id args]
  (let [args (value->canonical args)]
    (cond
      (and (string? (get args "path")) (#{:mcp/read_file :read_file} tool-id))
      {:kind :filesystem/path :path (normalize-path (get args "path")) :action :read}
      (string? (get args "path"))
      {:kind :filesystem/path :path (normalize-path (get args "path")) :action :read}
      :else {:kind :tool :id tool-id})))

;; ---------------------------------------------------------------------------
;; MCP Java SDK value -> plain EDN
;; ---------------------------------------------------------------------------

(defn java-value->edn
  "Recursively convert a value returned by the MCP Java SDK (the
   `structuredContent` field is a java.util.Map with STRING keys) into
   plain persistent Clojure data:

     java.util.Map        -> persistent map (STRING keys preserved)
     java.util.List       -> vector
     java.util.Set        -> set
     java.util.Collection -> vector
     strings/numbers/bool -> passed through unchanged

   This is THE single boundary converter for the structuredContent EDN
   boundary (M9 / WO-M9). Both evoclj.mcp.client and
   evoclj.provider.mcp-bridge delegate here so there is exactly one
   implementation (INV-05) and GC-22 (no raw Java objects cross the
   protocol boundary). String keys are intentionally preserved — they are
   NOT keywordized — so the EDN boundary stays explicit and typed."
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
