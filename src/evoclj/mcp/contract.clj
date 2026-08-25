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
                 (some? captured-at) (assoc :captured-at captured-at))
         b (binding/capture descriptor normalized decision freshness opts)]
     ;; The contract wrapper is the sanctioned compat surface: it decorates the
     ;; canonical CallBinding record with legacy :contract/* / :mcp/* alias keys
     ;; (single source of truth lives in evoclj.binding.call; the record itself
     ;; carries only canonical :binding/* + :revision/* keys — M21).
     (assoc b
            :contract/id (:binding/id b)
            :contract/generation (:revision/seq b)
            :contract/descriptor (:binding/descriptor b)
            :contract/freshness (:binding/freshness b)
            :contract/stale? (:binding/stale? b)
            :contract/captured-at (:binding/captured-at b)
            :mcp/generation (:revision/seq b)
            :mcp/stale? (:binding/stale? b)
            :mcp/freshness (:binding/freshness b)))))

(defn freeze [& args] (apply binding/freeze args))

(defn validate-contract [c]
  ;; Single-sourced validation: CallContractSchema IS binding/CallBindingSchema
  ;; (aliased above), so validating both was a redundant double-check (M11).
  ;; One validation, one error path. The contract record is decorated with
  ;; compat :contract/* / :mcp/* aliases, so we validate the canonical subset.
  (binding/validate-binding (select-keys c binding/canonical-binding-keys))
  c)

(defn contract->audit [contract] (binding/binding->audit contract))
