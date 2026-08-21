(ns evoclj.evolution.loop-policy
  "Deterministic loop-continuation policy for the evolution runtime
  (component).

  `decide-continue?` is a PURE, DETERMINISTIC function: given the
  generation-summary `history` (a vector, NEWEST generation LAST) and a
  `config` map, it returns one map

      {:decision <kw> :reason <string>}

  choosing whether the evolution loop should keep generating the next
  generation or stop. The decision is selected by a fixed priority
  order, so the same inputs always yield the same output:

    1. :stop-max-gen     — the generation budget is exhausted:
                           (count history) >= max-generations.
    2. :stop-regression  — the latest generation scored BELOW its
                           parent (utility < parent/utility) and the
                           config forbids regression.
    3. :stop-plateau     — utility has not improved inside the trailing
                           plateau window: the window's spread (max -
                           min) is smaller than min-improvement.
    4. :continue         — none of the stop conditions fired: the run is
                           still improving or within budget.

  No randomness, IO, time, or global state is consulted — only the
  supplied data and the documented defaults. This keeps the policy
  replan-safe and GC22-clean: it consumes and returns plain validated
  Clojure data only.")

(defn decide-continue?
  "Decide whether the evolution loop should produce another generation.

  `history` is a vector of per-generation summary maps, NEWEST LAST,
  each of the shape

      {:generation/id \"generation-3\"
       :utility 0.72
       :parent/utility 0.70}

  `config` is a map, any of whose keys may be omitted (defaults are
  applied via `get`):

      :plateau-window     int    (default 5)
      :min-improvement    double (default 0.01)
      :max-generations    int    (default 20)
      :stop-on-regression? boolean (default true)

  Returns one of:

      {:decision :stop-max-gen :reason \"reached max-generations N\"}
      {:decision :stop-regression
       :reason \"latest generation regressed vs parent\"}
      {:decision :stop-plateau
       :reason \"utility plateau over window W (spread X < min Y)\"}
      {:decision :continue
       :reason \"improving or within budget\"}

  The three stop rules are checked IN PRIORITY ORDER; the first that
  fires wins. Plateau and regression both look only at the trailing
  `history`, so the decision never depends on earlier generations
  beyond the plateau window.

  Pure and deterministic: identical arguments always return identical
  results; it performs no IO, uses no random source, and reads no
  global state."
  [history config]
  (let [n (count history)
        plateau-window (get config :plateau-window 5)
        min-improvement (double (get config :min-improvement 0.01))
        max-generations (get config :max-generations 20)
        stop-on-regression? (get config :stop-on-regression? true)]
    (cond
      ;; 1. exhausted the generation budget
      (>= n max-generations)
      {:decision :stop-max-gen
       :reason (str "reached max-generations " max-generations)}

      ;; 2. latest generation scored below its parent
      (and stop-on-regression?
           (>= n 2)
           (let [latest (peek history)
                 parent-utility (:parent/utility latest)]
             (< (:utility latest) parent-utility)))
      {:decision :stop-regression
       :reason "latest generation regressed vs parent"}

      ;; 3. no improvement across the trailing plateau window
      (let [window (take-last (min plateau-window n) history)
            utilities (mapv :utility window)
            spread (when (>= (count utilities) 2)
                     (- (apply max utilities) (apply min utilities)))]
        (and spread (< spread min-improvement)))
      (let [window (take-last (min plateau-window n) history)
            utilities (mapv :utility window)
            spread (- (apply max utilities) (apply min utilities))]
        {:decision :stop-plateau
         :reason (format "utility plateau over window %d (spread %.4f < min %.4f)"
                         (min plateau-window n) spread min-improvement)})

      ;; 4. nothing stopped us
      :else
      {:decision :continue
       :reason "improving or within budget"})))
