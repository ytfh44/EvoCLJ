(ns evoclj.mcp.canonical)

(defn value->canonical
  "Convert Agent args to JSON-like EDN with STRING keys.
   Keyword keys -> string (name). String keys preserved.
   Recurses into maps/vectors."
  [v]
  (cond
    (map? v) (into {} (map (fn [[k val]] [(if (keyword? k) (name k) (str k)) (value->canonical val)]) v))
    (vector? v) (mapv value->canonical v)
    (seq? v) (map value->canonical v)
    (set? v) (into #{} (map value->canonical v))
    :else v))
