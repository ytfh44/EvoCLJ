(ns evoclj.environment.binding
  "Alias for generic CallBinding — re-exports evoclj.binding.call.
  Kept for environments that expect the binding at environment/binding path."
  (:require [evoclj.binding.call :as call]))

;; Re-export all public vars from call
(def freshness-values call/freshness-values)
(def FreshnessSchema call/FreshnessSchema)
(def CallBindingSchema call/CallBindingSchema)
(def valid-freshness? call/valid-freshness?)
(def generation call/generation)
(def captured-at call/captured-at)
(def stale? call/stale?)
(def validate-binding call/validate-binding)
(def capture-tool-binding call/capture-tool-binding)
(def capture call/capture)
(def freeze call/freeze)
(def binding->audit call/binding->audit)
(def contract->audit call/contract->audit)
(def binding->persisted call/binding->persisted)
(def binding->pure-data call/binding->pure-data)
(def tool-surface->binding call/tool-surface->binding)
(def stale-binding? call/stale-binding?)
(def mcp-tool-error? call/mcp-tool-error?)
(def tool-error? call/tool-error?)
(def attach-audit-to-result call/attach-audit-to-result)
