(ns evoclj.eval.marathon
  "S3-5 — real-model marathon timings.

  run-marathon! measures wall-clock latency and token throughput for
  real model calls by calling a provider N times and returning
  deterministic aggregate statistics. The function is pure data in,
  pure data out: the only side effect is the provider calls themselves.

  The returned map carries:

      {:model/id <model-id>
       :trials <int>
       :mean-latency-ms <double>
       :p99-latency-ms <double>
       :mean-input-tokens <double>
       :mean-output-tokens <double>
       :mean-reasoning-tokens <double | nil>
       :errors <int>}

  Errors are counted, not thrown: a single failed trial does not abort
  the marathon; the remaining trials still run and the error count is
  recorded in the result."
  (:require [evoclj.kernel.error :as err]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.provider.protocol :as proto])
  (:import (java.util ArrayList Comparator Collections)
           (java.util.function ToDoubleFunction)))

;; --- schemas --------------------------------------------------------------------

(def MarathonResultSchema
  "The marathon result map returned by run-marathon!."
  [:map {:closed true}
   [:model/id keyword?]
   [:trials pos-int?]
   [:mean-latency-ms double?]
   [:p99-latency-ms double?]
   [:mean-input-tokens double?]
   [:mean-output-tokens double?]
   [:mean-reasoning-tokens {:optional true} double?]
   [:errors int?]])

;; --- internal helpers -----------------------------------------------------------

(defn- percentile
  "The percentile `p` (0.0-1.0) of a sorted vector of numbers.
  Uses linear interpolation between nearest ranks (the same
  convention as Python's numpy.percentile with method 'linear')."
  [sorted-vec p]
  (when (seq sorted-vec)
    (let [n (count sorted-vec)
          rank (* p (dec n))
          lower (int (Math/floor rank))
          upper (int (Math/ceil rank))
          frac (- rank lower)]
      (if (= lower upper)
        (double (nth sorted-vec lower))
        (+ (* (- 1.0 frac) (double (nth sorted-vec lower)))
           (* frac (double (nth sorted-vec upper))))))))

(defn- validate-args!
  "Validate the run-marathon! arguments. Throws typed ExceptionInfo on
  invalid input; returns nil on success."
  [provider model-id messages opts]
  (when-not provider
    (throw (err/error :eval/marathon-invalid "provider is required" {})))
  (when-not (keyword? model-id)
    (throw (err/error :eval/marathon-invalid "model-id must be a keyword"
                      {:model-id model-id})))
  (when-not (vector? messages)
    (throw (err/error :eval/marathon-invalid "messages must be a vector"
                      {:messages messages})))
  (when-let [n (:n opts)]
    (when-not (and (integer? n) (pos? n))
      (throw (err/error :eval/marathon-invalid "n must be a positive integer"
                        {:n n})))))

(defn- usage-from-result
  "Extract token counts from one provider result map. Returns nil when
  the result carries no usable usage data."
  [result]
  (when (map? result)
    (let [usage (:usage result)
          has-usage? (and (map? usage)
                          (contains? usage :model-input-tokens)
                          (contains? usage :model-output-tokens))]
      (when has-usage?
        {:input-tokens (:model-input-tokens usage 0)
         :output-tokens (:model-output-tokens usage 0)
         :reasoning-tokens (or (:model-reasoning-tokens usage) nil)}))))

(defn- run-one-trial
  "Run ONE provider call, measure its wall-clock latency, and return a
  trial result map. Catches any exception and returns an error record."
  [provider model-id messages]
  (try
    (let [t0 (System/nanoTime)
          result (proto/execute-request! provider
                                   {:model/id model-id
                                    :messages messages
                                    :options {}})
          latency-ms (quot (- (System/nanoTime) t0) 1000000)
          tokens (usage-from-result result)]
      (if tokens
        {:ok true
         :latency-ms latency-ms
         :input-tokens (:input-tokens tokens)
         :output-tokens (:output-tokens tokens)
         :reasoning-tokens (:reasoning-tokens tokens)}
        {:ok true
         :latency-ms latency-ms
         :input-tokens 0
         :output-tokens 0
         :reasoning-tokens nil}))
    (catch Throwable t
      {:ok false
       :latency-ms 0
       :error (err/error-data t)})))

;; --- public -------------------------------------------------------------------

(defn run-marathon!
  "Run a model provider N times and return aggregate timing and token
  statistics.

  Args:
    provider   — an evoclj.provider.protocol/Provider
    model-id   — a keyword, the model id (e.g. :gpt-4o)
    messages   — a vector of message maps {:role :content}
    opts       — optional map with:
      :n   — number of trials (default 3, minimum 1)

  Returns a Malli-validated map with :model/id, :trials, :mean-latency-ms,
  :p99-latency-ms, :mean-input-tokens, :mean-output-tokens,
  :mean-reasoning-tokens (nil when no trial reported reasoning tokens),
  and :errors (count of failed trials).

  Errors are counted, not thrown: a failed trial records its error data
  and the marathon continues. Validation errors (bad provider, bad n,
  ...) are still thrown as typed ExceptionInfo."
  ([provider model-id messages]
   (run-marathon! provider model-id messages {}))
  ([provider model-id messages opts]
   (validate-args! provider model-id messages opts)
   (let [n (or (:n opts) 3)
         trials (mapv (fn [_] (run-one-trial provider model-id messages))
                      (range n))
         successes (filterv :ok trials)
         failures (remove :ok trials)
         latencies (sort (mapv :latency-ms successes))
         inputs (mapv :input-tokens successes)
         outputs (mapv :output-tokens successes)
         reasoning-vals (vec (keep :reasoning-tokens successes))
         has-reasoning? (seq reasoning-vals)
         mean (fn [coll] (if (seq coll) (/ (reduce + 0.0 coll) (count coll)) 0.0))
         p99 (if (seq latencies) (percentile latencies 0.99) 0.0)]
     (let [result (cond-> {:model/id model-id
                           :trials n
                           :mean-latency-ms (mean latencies)
                           :p99-latency-ms p99
                           :mean-input-tokens (mean inputs)
                           :mean-output-tokens (mean outputs)
                           :errors (count failures)}
                      has-reasoning? (assoc :mean-reasoning-tokens (mean reasoning-vals)))]
       ;; Malli validation at the boundary (Global Constraint 22)
       (when-let [expl (m/explain MarathonResultSchema result)]
         (throw (err/error :eval/marathon-invalid
                           "marathon result does not satisfy MarathonResultSchema"
                           {:errors (me/humanize expl)})))
       result))))
