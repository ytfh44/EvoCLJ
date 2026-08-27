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

(defn- base-invoke-resource
  "Fail-closed safe default for a tool request whose parameters carry NO
   declared projection: the whole request is a single remote :invoke
   effect. Classification is NEVER inferred from parameter names (M13) —
   a parameter named \"file\"/\"path\"/\"url\" does NOT become a
   filesystem-read effect."
  [tool-id]
  {:kind :tool
   :id tool-id
   :mcp/remote-effect :invoke})

(defn- project-param
  "Apply ONE declarative projection spec to canonicalized args. Returns a
   canonical resource map, or nil when the spec's declared :param is
   absent from args.

   This is the PURE-DATA projection DSL (M13): the spec declares BOTH the
   source parameter name (`:param`) AND the resulting resource shape
   (`:resource-kind`, `:resource-path-key`, `:resource-id`,
   `:resource-action`, `:remote-effect`). No code special-cases by name —
   selection and classification are entirely data-driven. The parameter
   name referenced by `:param` is the parameter's DECLARED name from the
   tool schema, supplied as data, not inferred from a magic string."
  [spec args]
  (let [pname (:param spec)
        val (get args pname)]
    (when (some? val)
      (cond-> {:kind (:resource-kind spec)
               :action (or (:resource-action spec) :read)
               :mcp/remote-effect (or (:remote-effect spec) :invoke)}
        (:resource-path-key spec)
        (assoc (:resource-path-key spec)
               (if (string? val) (normalize-path val) val))
        (:resource-id spec)
        (assoc :id (:resource-id spec))))))

(defn canonical-resource
  "Project a normalized request's args into a canonical resource descriptor
   using the tool's DECLARED projection DSL — a vector of specs carried on
   the descriptor under :mcp/param-projections.

   - DECLARED: when the descriptor carries :mcp/param-projections, the
     FIRST spec whose declared :param is present in args is applied via
     evoclj.mcp.canonical/project-param. Classification comes from the
     declared spec, never from the parameter name itself.
   - UNDECLARED: when the descriptor declares no projection at all, the
     request falls back to the fail-closed default
     {:mcp/remote-effect :invoke} — a single remote invoke effect. A
     parameter named \"file\"/\"path\"/\"url\" with no declared projection
     does NOT become a filesystem-read effect.

   This replaces the M12-era name heuristic (the removed read-file-tool?
   special-cased tool/param names) so effect classification is driven by
   DECLARED metadata only (M13).

   `descriptor` is the provider's stable tool descriptor (it carries both
   :tool/id and, optionally, :mcp/param-projections); `args` is the
   canonicalized request argument map (string keys)."
  [descriptor args]
  (let [args (value->canonical args)]
    (if-let [specs (seq (:mcp/param-projections descriptor))]
      (or (some #(project-param % args) specs)
          ;; a projection was declared but no declared :param matched:
          ;; fail closed to the safe invoke default (never infer by name)
          (base-invoke-resource (:tool/id descriptor)))
      ;; no declared projection at all -> safe default
      (base-invoke-resource (:tool/id descriptor)))))

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
