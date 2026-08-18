(ns evoclj.context.trigger
  "Determines WHEN to compress context.

   Two signals can trigger compression:
   1. Token count exceeds a configurable threshold.
   2. An explicit marker string is found in the context text.

   The decision is fully deterministic: the same context + config always
   produces the same trigger record. No randomness, no mutable state.

   A cooldown mechanism prevents re-triggering too aggressively: if the
   last compression saved fewer than `:trigger/cooldown-tokens` tokens,
   the trigger returns :cooldown even if the threshold is exceeded again.
   This prevents thrashing near the boundary."
  (:require [evoclj.context.error :as err]
            [evoclj.context.token-estimator :as token-estimator]))

;; ---------------------------------------------------------------------------
;; Token estimation
;; ---------------------------------------------------------------------------

(def ^:private default-estimator
  "Default token estimator: character-count proxy with ~4 chars per token."
  token-estimator/default-char-count-estimator)

(defn- estimate-tokens [s]
  (token-estimator/estimate-tokens default-estimator s))

;; ---------------------------------------------------------------------------
;; Trigger reasons
;; ---------------------------------------------------------------------------

(def ^:const reason-threshold
  "`:threshold-exceeded` — token count is at or above the configured
  threshold and cooldown allows compression."
  :threshold-exceeded)

(def ^:const reason-marker
  "`:marker-detected` — an explicit compression marker was found in the
  context text, regardless of token count."
  :marker-detected)

(def ^:const reason-cooldown
  "`:cooldown` — the threshold is exceeded but the cooldown is active
  because the last compression did not save enough tokens."
  :cooldown)

(def ^:const reason-none
  "`:no-trigger` — no compression signal is present."
  :no-trigger)

;; ---------------------------------------------------------------------------
;; Trigger record
;; ---------------------------------------------------------------------------

(defn- trigger-record
  "Build a trigger result map. `compressed?` is true when the caller
  should proceed with compression."
  [compressed? reason token-count threshold marker-found? last-savings]
  {:trigger/compressed? compressed?
   :trigger/reason reason
   :trigger/token-count token-count
   :trigger/threshold threshold
   :trigger/marker-found? marker-found?
   :trigger/last-savings (or last-savings 0)})

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn should-compress?
  "Return a trigger record indicating whether `context-str` should be
  compressed under `config`.

  `config` keys:
    :trigger/token-threshold  — max estimated tokens before compression (default 4000)
    :trigger/marker           — explicit compression marker string (default nil)
    :trigger/cooldown-tokens  — minimum tokens saved to reset cooldown (default 500)
    :trigger/token-estimator  — optional TokenEstimator instance (default char-count)

  Returns a map with :trigger/compressed? boolean and :trigger/reason
  keyword. Throws :context/trigger-invalid when config is malformed."
  [context-str config]
  (when-not (string? context-str)
    (throw (err/error :context/trigger-invalid
                      "context-str must be a string"
                      {:value (err/sanitize context-str)})))
  (when-not (map? config)
    (throw (err/error :context/trigger-invalid
                      "config must be a map"
                      {:value (err/sanitize config)})))
  (let [threshold (int (or (:trigger/token-threshold config) 4000))
        marker    (:trigger/marker config)
        cooldown  (int (or (:trigger/cooldown-tokens config) 500))
        last-savings (:trigger/last-savings config)
        estimator (or (:trigger/token-estimator config) default-estimator)
        token-count (token-estimator/estimate-tokens estimator context-str)
        marker-found? (and marker (clojure.string/includes? context-str marker))
        in-cooldown?  (and (some? last-savings) (< (int last-savings) cooldown))
        threshold-hit? (>= token-count threshold)]
    (cond
      marker-found?
      (trigger-record true reason-marker token-count threshold true last-savings)

      (and threshold-hit? (not in-cooldown?))
      (trigger-record true reason-threshold token-count threshold false last-savings)

      threshold-hit?
      (trigger-record false reason-cooldown token-count threshold false last-savings)

      :else
      (trigger-record false reason-none token-count threshold false last-savings))))

(defn compressed?
  "Convenience: true when `should-compress?` says to compress."
  [context-str config]
  (:trigger/compressed? (should-compress? context-str config)))

(defn trigger-reason
  "The :trigger/reason keyword from `should-compress?`."
  [context-str config]
  (:trigger/reason (should-compress? context-str config)))

(defn token-count
  "Estimated token count for `context-str` under the current estimator."
  ([context-str]
   (token-count context-str nil))
  ([context-str estimator]
   (token-estimator/estimate-tokens (or estimator default-estimator) context-str)))
