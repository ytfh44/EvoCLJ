(ns evoclj.runtime.tool-surface
  "ToolSurface value object — decouples pin stability from refresh variability.

  Pin stability: the tool catalog snapshot is captured once at loop entry
  and stays constant for the whole tool loop (4 rounds). Even if the host
  publishes new tools between rounds, the loop keeps the pinned snapshot.

  Refresh variability: each round recomputes EffectiveContext from fresh
  SessionBindings and CAS. A binding created by activate_skill in round N
  is visible in round N+1.

  Fail-closed: without a CAS resolver, an unresolvable placeholder throws
  :assembler/placeholder-unresolved instead of emitting a degraded segment."
  (:require [evoclj.context.materializer :as mat]
            [evoclj.kernel.error :as err]))

(defn pin
  "Capture a ToolSurface from a catalog snapshot.

  surface: catalog snapshot — a vector of tool descriptors or a map of
  source-id to revision-id. The snapshot is taken once and reused.

  Returns ToolSurface {:surface/tools [...] :surface/pinned-at <ms> :surface/binding {...}}.
  :surface/binding is {:binding/id uuid :revision-ids surface :captured-at ms} for
  backward compatibility with assembler and scheduler call sites."
  [surface]
  (let [now (System/currentTimeMillis)
        bid (random-uuid)
        binding {:binding/id bid
                 :revision-ids surface
                 :captured-at now}
        tools (cond
                (nil? surface) []
                (map? surface) surface
                (sequential? surface) (vec surface)
                :else surface)]
    {:surface/tools tools
     :surface/pinned-at now
     :surface/binding binding}))

(defn pin-catalog
  "Alias for pin — kept for assembler compatibility."
  [catalog]
  (pin catalog))

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
