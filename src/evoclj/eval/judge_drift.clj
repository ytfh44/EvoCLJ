(ns evoclj.eval.judge-drift
  "Quantify LLM-as-judge verdict distribution drift on a fixed calibration set.

  A judge's verdict distribution is a map of verdict keys to probabilities in
  [0,1], e.g. `{:equivalent 0.8 :not-equivalent 0.2}` (assumed already
  normalized, though normalization is not required by these functions).

  `drift-score` compares the current verdict distribution against a baseline
  derived from recent history (the trailing N=5 distributions). The baseline is
  the per-key mean across those histories. The drift is the L1 distance
  (sum of absolute per-key differences) between baseline and current.

  A higher drift means the judge's behaviour has shifted further away from its
  recent historical baseline, which should trigger a recalibration review
  (wired in by a later task). The computation is a pure function: no IO, no
  randomness, fully deterministic.")

(def ^:const ^:private window-size
  "How many trailing historical distributions to consider for the baseline."
  5)

(defn baseline
  "Return the mean verdict distribution over the trailing (at most N=5)
  historical distributions in `history` (a vector, newest last).

  Each value of the result is the arithmetic mean across histories of that key;
  only keys present in the windowed history are returned. If `history` is empty,
  returns an empty map (callers such as `drift-score` handle this by falling
  back to `current`)."
  [history]
  (let [window (take-last window-size (vec history))]
    (if (empty? window)
      {}
      (let [n (count window)]
        (reduce-kv
          (fn [acc k sum] (assoc acc k (/ sum n)))
          {}
          (reduce
            (fn [acc dist]
              (reduce-kv
                (fn [acc' k v] (update acc' k (fnil + 0) v))
                acc
                dist))
            {}
            window))))))

(defn drift-score
  "Quantify how far the current verdict distribution has drifted from the
  recent historical baseline.

  `history` is a vector of past verdict distributions (newest last); `current`
  is the current distribution.

  The baseline is the per-key mean over the trailing N=5 history distributions
  (see `baseline`). If `history` is empty, the baseline falls back to `current`,
  yielding a drift of 0.

  Returns the L1 distance between baseline and current: the sum over all keys
  of `|baseline[k] - current[k]|`. Range is [0, 2] (each key contributes at
  most 1.0 and the probabilities are bounded in [0,1])."
  [history current]
  (let [base (if (empty? history)
               current
               (baseline history))
        keys (distinct (concat (keys base) (keys current)))]
    (reduce
      (fn [acc k]
        (+ acc (Math/abs (- (double (get base k 0.0))
                            (double (get current k 0.0))))))
      0.0
      keys)))
