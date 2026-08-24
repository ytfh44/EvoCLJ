(ns evoclj.mcp.contract
  "CallContract - now a thin wrapper around generic CallBinding (evoclj.binding.call).

  Kept for backward compatibility. Delegates to evoclj.binding.call so existing
  code and tests remain equivalent while generic dispatch no longer depends
  on MCP-specific fields directly."
  (:require [evoclj.binding.call :as binding]
            [malli.core :as m]))

(def freshness-values binding/freshness-values)
(def FreshnessSchema binding/FreshnessSchema)
(def CallContractSchema binding/CallBindingSchema)

(defn valid-freshness? [v] (binding/valid-freshness? v))

(defn generation [x] (binding/generation x))
(defn captured-at [descriptor] (binding/captured-at descriptor))

(defn stale?
  ([descriptor] (binding/stale? descriptor))
  ([descriptor freshness] (binding/stale? descriptor freshness)))

(defn capture
  ([descriptor freshness]
   (capture descriptor nil nil freshness {}))
  ([descriptor normalized decision freshness]
   (capture descriptor normalized decision freshness {}))
  ([descriptor normalized decision freshness {:keys [stale? id captured-at]}]
   (let [opts (cond-> {:freshness freshness}
                (some? stale?) (assoc :stale? stale?)
                (some? id) (assoc :binding/id id)
                (some? captured-at) (assoc :captured-at captured-at))]
     (binding/capture descriptor normalized decision freshness opts))))

(defn freeze [& args] (apply binding/freeze args))

(defn validate-contract [c]
  ;; Single-sourced validation: CallContractSchema IS binding/CallBindingSchema
  ;; (aliased above), so validating both was a redundant double-check (M11).
  ;; One validation, one error path.
  (when-not (m/validate CallContractSchema c)
    (throw (ex-info "invalid CallContract" {:explanation (m/explain CallContractSchema c)})))
  c)

(defn contract->audit [contract] (binding/binding->audit contract))
