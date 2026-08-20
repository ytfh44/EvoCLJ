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
