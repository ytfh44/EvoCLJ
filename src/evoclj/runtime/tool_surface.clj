(ns evoclj.runtime.tool-surface
  "ToolSurface value object — decouples pin stability from refresh variability.

  Pin stability: the tool catalog snapshot is captured once at loop entry
  and stays constant for the whole tool loop (4 rounds). Even if the host
  publishes new tools between rounds, the loop keeps the pinned snapshot.

  Refresh variability: each round recomputes EffectiveContext from fresh
  SessionBindings and CAS. A binding created by activate_skill in round N
  is visible in round N+1.

  Fail-closed: without a CAS resolver, an unresolvable placeholder throws
  :assembler/placeholder-unresolved instead of emitting a degraded segment.

  P9 CodeMode declaration: when :ptc is enabled and the surface has tools,
  the code_execution wire tool (single source in evoclj.tool.specs) is
  included in :surface/tools for model visibility. Otherwise not declared
  (fail-safe). No execution is performed here."
  (:require [evoclj.context.materializer :as mat]
            [evoclj.kernel.error :as err]
            [evoclj.tool.specs :as tool-specs]))

(defn- ptc-enabled?
  "Interpret opts as a PTC enabled flag. Accepts:
   - boolean true/false
   - map {:enabled? bool} or {:ptc/enabled? bool}
   - map {:ptc {:enabled? bool}}
   Otherwise false (fail-safe)."
  [opts]
  (cond
    (boolean? opts) opts
    (map? opts) (boolean (or (:enabled? opts)
                             (:ptc/enabled? opts)
                             (get-in opts [:ptc :enabled?])))
    :else false))

(defn- maybe-include-code-execution
  "If ptc-enabled? and tools is a non-empty sequential collection,
  ensure the code_execution wire tool is included. Idempotent."
  [tools ptc-enabled?]
  (if (and ptc-enabled? (sequential? tools) (seq tools)
           (not (some #(= "code_execution" (:name %)) tools)))
    (conj (vec tools) tool-specs/code-execution-wire-tool)
    tools))

(defn pin
  "Capture a ToolSurface from a catalog snapshot.

  surface: catalog snapshot — a vector of tool descriptors or a map of
  source-id to revision-id. The snapshot is taken once and reused.

  opts (optional, P9): controls CodeMode declaration. When :ptc is
  enabled and surface has tools, code_execution is included in
  :surface/tools for model visibility; otherwise not declared (fail-safe).
  opts may be a boolean, {:enabled? bool}, {:ptc/enabled? bool}, or
  {:ptc {:enabled? bool}}.

  Returns ToolSurface {:surface/tools [...] :surface/pinned-at <ms> :surface/binding {...}}.
  :surface/binding is {:binding/id uuid :revision-ids surface :captured-at ms} for
  backward compatibility with assembler and scheduler call sites."
  ([surface]
   (pin surface nil))
  ([surface opts]
   (let [now (System/currentTimeMillis)
         bid (random-uuid)
         binding {:binding/id bid
                  :revision-ids surface
                  :captured-at now}
         enabled? (ptc-enabled? opts)
         tools (cond
                 (nil? surface) []
                 (map? surface) surface
                 (sequential? surface) (vec surface)
                 :else surface)
         tools (maybe-include-code-execution tools enabled?)]
     {:surface/tools tools
      :surface/pinned-at now
      :surface/binding binding})))

(defn pin-catalog
  "Alias for pin — kept for assembler compatibility."
  ([catalog] (pin catalog))
  ([catalog opts] (pin catalog opts)))

(defn surface-binding
  "Extract the pinned binding from a ToolSurface."
  [surface]
  (:surface/binding surface))

(defn tool-surface?
  "Predicate for ToolSurface shape."
  [x]
  (and (map? x)
       (contains? x :surface/tools)
       (contains? x :surface/binding)))

(defn refresh-context
  "Recompute EffectiveContext from fresh SessionBindings and CAS.

  surface: ToolSurface returned by pin
  session-bindings: seq of ContextBinding
  cas: CAS resolver (config map, artifact map, or path) — may be nil

  Returns {:context EffectiveContext :surface Surface} where EffectiveContext
  is the materialized context for this round. Pin stability is preserved:
  surface is returned unchanged.

  Fail-closed: if session-bindings is non-empty and cas is nil, throws
  :assembler/placeholder-unresolved instead of emitting a degraded placeholder.

  Optional opts map (4th arg) may carry :catalog, :history, :policy forwarded
  to the materializer. Defaults: catalog {}, history \"\", policy nil."
  ([surface session-bindings cas]
   (refresh-context surface session-bindings cas {}))
  ([surface session-bindings cas {:keys [catalog history policy] :or {catalog {} history ""}}]
   (let [bindings (or session-bindings [])
         effective (if (seq bindings)
                     (if cas
                       (mat/materialize {:history (or history "")
                                         :bindings bindings
                                         :catalog (or catalog {})
                                         :policy policy
                                         :cas cas})
                       (throw (err/error :assembler/placeholder-unresolved
                                         "cannot resolve session binding placeholders without a CAS resolver"
                                         {:bindings (mapv :logical/id bindings)})))
                     {:effective/history (or history "")
                      :effective/segments []
                      :effective/bindings []})]
     {:context effective :surface surface})))
