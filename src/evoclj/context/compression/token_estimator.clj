(ns evoclj.context.compression.token-estimator
  "Token estimation protocol and implementations for the context-compression
   subsystem.

   In v0 the default is a character-count proxy. A real implementation would
   use the model's tokenizer, but that requires a model-specific dependency
   we don't want here. The proxy is intentionally rough: it only needs to
   be CONSISTENT, not exact."

  (:require [evoclj.context.compression.error :as err]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol TokenEstimator
  (estimate-tokens [this s]
    "Estimate the number of tokens in string `s`. Returns a non-negative
    integer. The estimate should be consistent for the same input."))

;; ---------------------------------------------------------------------------
;; Default implementation: character-count proxy
;; ---------------------------------------------------------------------------

(defrecord CharCountEstimator [chars-per-token]
  TokenEstimator
  (estimate-tokens [this s]
    (when-not (string? s)
      (throw (err/error :context/trigger-invalid
                        "input must be a string"
                        {:value (err/sanitize s)})))
    (int (Math/ceil (/ (count s) (:chars-per-token this))))))

(def default-char-count-estimator
  "Default estimator using ~4 characters per token, suitable for mixed
  English/Clojure text."
  (->CharCountEstimator 4))

;; ---------------------------------------------------------------------------
;; Approximate cl100k tokenizer (tiktoken-compatible)
;; ---------------------------------------------------------------------------

(defrecord Cl100kEstimator []
  TokenEstimator
  (estimate-tokens [this s]
    (when-not (string? s)
      (throw (err/error :context/trigger-invalid
                        "input must be a string"
                        {:value (err/sanitize s)})))
    ;; Approximate cl100k tokenization using regex patterns.
    ;; This is a rough approximation that captures the main token boundaries.
    (let [tokens (-> s
                     (str/split #"[\s\n\r\t]+")
                     seq
                     (->> (remove str/blank?)))]
      (count tokens))))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn char-count-estimator
  ([] (->CharCountEstimator 4))
  ([chars-per-token]
   (->CharCountEstimator (int chars-per-token))))

(defn cl100k-estimator
  "Create a Cl100kEstimator instance."
  []
  (->Cl100kEstimator))

;; ---------------------------------------------------------------------------
;; Model-usage estimator (wraps actual LLM usage data)
;; ---------------------------------------------------------------------------

(defrecord ModelUsageEstimator [usage-map]
  TokenEstimator
  (estimate-tokens [_ _]
    (if (pos? (:output-tokens usage-map 0))
      (:output-tokens usage-map)
      (:input-tokens usage-map 0))))

(defn model-usage-estimator
  "Create a ModelUsageEstimator from a usage map shaped
   `{:input-tokens <int> :output-tokens <int>}`.

   Returns nil if `usage-map` is nil."
  [usage-map]
  (when (map? usage-map)
    (->ModelUsageEstimator usage-map)))
