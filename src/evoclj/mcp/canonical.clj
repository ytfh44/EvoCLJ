(ns evoclj.mcp.canonical
  (:require [clojure.string :as str]))

(defn normalize-path [s]
  (when (string? s)
    (let [abs? (.startsWith s "/")
          segs (->> (str/split s #"/") (remove #{"" "."})
                    (reduce (fn [a seg] (if (= seg "..") (if (seq a) (pop a) a) (conj a seg))) []))]
      (str (when abs? "/") (str/join "/" segs)))))

(defn value->canonical [v]
  (if (map? v)
    (into {} (map (fn [[k val]] [(if (keyword? k) (name k) (str k)) (value->canonical val)]) v))
    v))

(defn canonical-resource [tool-id args]
  (let [args (value->canonical args)]
    (cond
      (and (string? (get args "path")) (#{:mcp/read_file :read_file} tool-id))
      {:kind :filesystem/path :path (normalize-path (get args "path")) :action :read}
      (string? (get args "path"))
      {:kind :filesystem/path :path (normalize-path (get args "path")) :action :read}
      :else {:kind :tool :id tool-id})))
