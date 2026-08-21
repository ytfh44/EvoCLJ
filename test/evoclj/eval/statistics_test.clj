(ns evoclj.eval.statistics-test
  "component tests: repeated rollout statistics WITHOUT pretending
  certainty (descriptive statistics only).

  The normative interface:

      (summarize-paired-deltas pairs)
      ;; => {:n ... :mean-delta ... :median-delta ... :wins ... :losses ... :ties ...}

  The scenarios, in the task's numbered order:

  - Step 1 (deterministic, single repetition): hand-built pairs with
    exactly known deltas — all ties, all wins, all losses, a mixed
    sample with a known mean/median, an even-count median, and the
    empty sample.
  - Step 2 (stochastic fixture, multiple paired repetitions): a
    deterministically-seeded fixture where the candidate holds a small
    noisy advantage over the parent across every repetition — wins/loss
    counts and the mean/median delta must track the injected advantage.
  - Step 3 (store raw observations, recompute summaries): summarizing
    twice over the SAME raw pairs yields identical results, and
    summarizing never consumes or mutates the stored observations.
  - Step 4 (no probability/calibration claims): the output key set is
    EXACTLY #{:n :mean-delta :median-delta :wins :losses :ties} — no
    p-value, confidence interval, significance, or probability field
    may appear.
  - Step 5 (profile-declared sample requirements): promotion-checks
    exposes :min-pairs and :max-candidate-failure-rate as data checks
    returning reason data in the component shape; a check applies ONLY
    when the profile declares it."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.statistics :as statistics]))

;; --- fixtures ----------------------------------------------------------------

(defn- approx
  "Equality within 1e-6 (for double arithmetic)."
  [a b]
  (< (Math/abs (double (- a b))) 1e-6))

(defn- within
  "Equality within an explicit tolerance (for noisy sample statistics)."
  [tol a b]
  (< (Math/abs (double (- a b))) tol))

(defn- stochastic-pairs
  "A deterministically-seeded stochastic fixture: `n` paired
  observations where the candidate holds a small noisy advantage
  (~0.1) over the parent. The fixed seed makes the fixture
  reproducible — the same pairs regenerate on every run, exercising
  the multi-repetition path with variance."
  [n seed]
  (let [rng (java.util.Random. ^long seed)]
    (vec
     (repeatedly
      n
      (fn []
        (let [parent (double (.nextDouble rng))
              noise (- (double (.nextDouble rng)) 0.5)]
          {:parent parent
           :candidate (+ parent 0.1 (* 0.05 noise))}))))))

;; --- Step 1: deterministic cases with one repetition ---------------------------

(deftest deterministic-single-repetition-cases
  (testing "all ties: every delta is zero"
    (is (= {:n 2 :mean-delta 0.0 :median-delta 0.0 :wins 0 :losses 0 :ties 2}
           (statistics/summarize-paired-deltas
            [{:parent 1 :candidate 1} {:parent 2 :candidate 2}]))))
  (testing "all wins: the candidate is strictly better on every pair"
    (is (= {:n 2 :mean-delta 2.0 :median-delta 2.0 :wins 2 :losses 0 :ties 0}
           (statistics/summarize-paired-deltas
            [{:parent 0 :candidate 1} {:parent 0 :candidate 3}]))))
  (testing "all losses: the candidate is strictly worse on every pair"
    (is (= {:n 2 :mean-delta -2.0 :median-delta -2.0 :wins 0 :losses 2 :ties 0}
           (statistics/summarize-paired-deltas
            [{:parent 1 :candidate 0} {:parent 3 :candidate 0}]))))
  (testing "mixed wins/losses/ties with a known mean and median"
    (let [s (statistics/summarize-paired-deltas
             [{:parent 1 :candidate 2}   ; delta +1
              {:parent 2 :candidate 1}   ; delta -1
              {:parent 1 :candidate 1}   ; delta  0
              {:parent 0 :candidate 2}   ; delta +2
              {:parent 5 :candidate 4}]) ; delta -1
          ]
      (is (= 5 (:n s)))
      (is (approx (:mean-delta s) 0.2))
      (is (approx (:median-delta s) 0.0))
      (is (= 2 (:wins s)))
      (is (= 2 (:losses s)))
      (is (= 1 (:ties s)))))
  (testing "even-count median averages the two middle deltas"
    (is (approx (:median-delta
                 (statistics/summarize-paired-deltas
                  [{:parent 0 :candidate 1} ; delta +1
                   {:parent 0 :candidate 2} ; delta +2
                   {:parent 0 :candidate 3} ; delta +3
                   {:parent 0 :candidate 4}])) ; delta +4
                2.5)))
  (testing "empty sample: count zero, central statistics nil, counts zero"
    (is (= {:n 0 :mean-delta nil :median-delta nil :wins 0 :losses 0 :ties 0}
           (statistics/summarize-paired-deltas [])))))

;; --- Step 2: stochastic fixture with multiple paired repetitions ---------------

(deftest stochastic-multi-repetition-fixture
  (let [pairs (stochastic-pairs 400 42)
        s (statistics/summarize-paired-deltas pairs)]
    (testing "the noisy advantage is positive on every repetition"
      (is (= 400 (:n s)))
      (is (= 400 (:wins s)))
      (is (= 0 (+ (:losses s) (:ties s)))))
    (testing "mean and median deltas track the injected advantage"
      ;; tolerance is sampling-noise-aware: the injected advantage is
      ;; exactly 0.1, but a 400-repetition fixture carries sampling
      ;; noise on the mean/median (~1e-3), so 0.01 bounds the claim
      (is (within 0.01 (:mean-delta s) 0.1))
      (is (within 0.01 (:median-delta s) 0.1)))))

;; --- Step 3: raw observations are stored and summaries are recomputable --------

(deftest summaries-are-recomputable-from-stored-raw-pairs
  (let [raw [{:parent 0.72 :candidate 0.79}
             {:parent 0.75 :candidate 0.70}
             {:parent 0.60 :candidate 0.60}]
        first-pass (statistics/summarize-paired-deltas raw)]
    (testing "summarizing twice over the SAME raw pairs yields identical results"
      (is (= first-pass (statistics/summarize-paired-deltas raw))))
    (testing "summarizing never consumes or mutates the stored observations"
      (is (= [{:parent 0.72 :candidate 0.79}
              {:parent 0.75 :candidate 0.70}
              {:parent 0.60 :candidate 0.60}]
             raw)))
    (testing "the raw pairs map to a stable content-hash artifact ref"
      (is (string? (statistics/pairs-artifact-ref raw)))
      (is (= (statistics/pairs-artifact-ref raw)
             (statistics/pairs-artifact-ref raw)))
      (is (not= (statistics/pairs-artifact-ref raw)
                (statistics/pairs-artifact-ref
                 (conj raw {:parent 0 :candidate 0})))))))

;; --- Step 4: no probability/calibration claims ---------------------------------

(deftest summary-claims-no-probability-or-confidence
  (let [s (statistics/summarize-paired-deltas
           [{:parent 0.5 :candidate 0.6}
            {:parent 0.5 :candidate 0.4}])]
    (testing "the descriptive key set is EXACT — no formal-inference field"
      (is (= #{:n :mean-delta :median-delta :wins :losses :ties}
             (set (keys s)))))
    (testing "no probability/confidence key ever appears"
      (is (empty? (select-keys s [:p-value :p :confidence-interval :ci
                                  :significance :alpha :effect-size
                                  :probability :calibration :z :t-stat]))))))

;; --- Step 5: profile-declared sample requirements ------------------------------

(deftest promotion-checks-enforce-min-pairs
  (let [summary (statistics/summarize-paired-deltas
                 [{:parent 0 :candidate 1}
                  {:parent 0 :candidate 1}
                  {:parent 0 :candidate 1}])]
    (testing "a high-risk profile requiring more pairs fails with explicit reason data"
      (is (= [{:dimension :paired
               :rule :below-min-pairs
               :metric :pairs/n
               :detail {:n 3 :min-pairs 10}}]
             (statistics/promotion-checks
              summary {:promotion {:min-pairs 10}}))))
    (testing "enough pairs passes the min-pairs check"
      (is (= [] (statistics/promotion-checks
                 summary {:promotion {:min-pairs 3}}))))))

(deftest promotion-checks-enforce-max-candidate-failure-rate
  (let [summary (statistics/summarize-paired-deltas
                 [{:parent 0 :candidate 1} ; win
                  {:parent 1 :candidate 0} ; loss
                  {:parent 1 :candidate 0} ; loss
                  {:parent 0 :candidate 1}])] ; win
    (testing "losses beyond the maximum failure rate fail with reason data"
      (is (= [{:dimension :paired
               :rule :above-max-candidate-failure-rate
               :metric :candidate/failure-rate
               :detail {:losses 2 :n 4 :failure-rate 0.5
                        :max-candidate-failure-rate 0.25}}]
             (statistics/promotion-checks
              summary {:promotion {:max-candidate-failure-rate 0.25}}))))
    (testing "a failure rate at the declared maximum still passes"
      (is (= [] (statistics/promotion-checks
                 summary {:promotion {:max-candidate-failure-rate 0.5}}))))))

(deftest promotion-checks-report-both-limits
  (let [summary (statistics/summarize-paired-deltas
                 [{:parent 0 :candidate 1} ; win
                  {:parent 1 :candidate 0}])] ; loss -> rate 0.5
    (is (= [{:dimension :paired
             :rule :below-min-pairs
             :metric :pairs/n
             :detail {:n 2 :min-pairs 5}}
            {:dimension :paired
             :rule :above-max-candidate-failure-rate
             :metric :candidate/failure-rate
             :detail {:losses 1 :n 2 :failure-rate 0.5
                      :max-candidate-failure-rate 0.25}}]
           (statistics/promotion-checks
            summary {:promotion {:min-pairs 5
                                 :max-candidate-failure-rate 0.25}})))))

(deftest promotion-checks-are-inert-without-declared-limits
  (let [summary (statistics/summarize-paired-deltas
                 [{:parent 0 :candidate 1} {:parent 1 :candidate 0}])]
    (testing "a profile declaring neither limit triggers no checks"
      (is (= [] (statistics/promotion-checks
                 summary {:promotion {:strategy :paired-comparison}}))))
    (testing "a profile without a :promotion block triggers no checks"
      (is (= [] (statistics/promotion-checks summary {}))))))

(deftest promotion-checks-handle-empty-samples
  (let [summary (statistics/summarize-paired-deltas [])]
    (testing "an empty sample fails min-pairs"
      (is (= [{:dimension :paired
               :rule :below-min-pairs
               :metric :pairs/n
               :detail {:n 0 :min-pairs 1}}]
             (statistics/promotion-checks
              summary {:promotion {:min-pairs 1}}))))
    (testing "no failure-rate check on an empty sample (a rate is undefined)"
      (is (= [] (statistics/promotion-checks
                 summary {:promotion {:max-candidate-failure-rate 0.25}}))))))

;; --- boundary validation --------------------------------------------------------

(deftest summarize-paired-deltas-validates-its-input
  (testing "non-sequential input"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"paired observations"
                          (statistics/summarize-paired-deltas
                           {:parent 1 :candidate 2}))))
  (testing "a pair that is not a map"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"paired observations"
                          (statistics/summarize-paired-deltas [1 2]))))
  (testing "a pair missing the candidate score"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"paired observation"
                          (statistics/summarize-paired-deltas [{:parent 1}]))))
  (testing "non-numeric scores"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"paired observation"
                          (statistics/summarize-paired-deltas
                           [{:parent "a" :candidate 2}])))))
