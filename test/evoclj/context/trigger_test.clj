(ns evoclj.context.trigger-test
  (:require [clojure.test :as t]
            [evoclj.context.trigger :as trig]
            [evoclj.context.compression.error :as err]))

;; ---------------------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------------------

(defn- long-str [n]
  (apply str (repeat n "x")))

(defn- cfg [& {:keys [threshold marker cooldown last-savings]}]
  (cond-> {}
    threshold (assoc :trigger/token-threshold threshold)
    marker    (assoc :trigger/marker marker)
    cooldown  (assoc :trigger/cooldown-tokens cooldown)
    last-savings (assoc :trigger/last-savings last-savings)))

;; ---------------------------------------------------------------------------
;; token estimation
;; ---------------------------------------------------------------------------

(t/deftest token-count-estimates-proportionally
  (t/is (= 1 (trig/token-count "abcd")))
  (t/is (= 2 (trig/token-count "abcdefgh"))))

;; ---------------------------------------------------------------------------
;; should-compress? — threshold
;; ---------------------------------------------------------------------------

(t/deftest no-trigger-below-threshold
  (let [s (long-str 100)
        result (trig/should-compress? s (cfg :threshold 1000))]
    (t/is (false? (:trigger/compressed? result)))
    (t/is (= :no-trigger (:trigger/reason result)))))

(t/deftest threshold-trigger-at-boundary
  (let [s (long-str 4000) ; 4000 chars / 4 = 1000 tokens, at threshold
        result (trig/should-compress? s (cfg :threshold 1000))]
    (t/is (true? (:trigger/compressed? result)))
    (t/is (= :threshold-exceeded (:trigger/reason result)))))

(t/deftest threshold-trigger-above-boundary
  (let [s (long-str 8000) ; ~2000 tokens
        result (trig/should-compress? s (cfg :threshold 1000))]
    (t/is (true? (:trigger/compressed? result)))
    (t/is (= :threshold-exceeded (:trigger/reason result)))))

;; ---------------------------------------------------------------------------
;; should-compress? — marker
;; ---------------------------------------------------------------------------

(t/deftest marker-trigger-overrides-threshold
  (let [s (str "some text <!-- COMPRESS --> more text")
        result (trig/should-compress? s (cfg :threshold 10000 :marker "<!-- COMPRESS -->"))]
    (t/is (true? (:trigger/compressed? result)))
    (t/is (= :marker-detected (:trigger/reason result)))
    (t/is (true? (:trigger/marker-found? result)))))

(t/deftest no-marker-when-absent
  (let [s "plain text without marker"
        result (trig/should-compress? s (cfg :threshold 100 :marker "<!-- COMPRESS -->"))]
    (t/is (false? (:trigger/compressed? result)))
    (t/is (= :no-trigger (:trigger/reason result)))
    (t/is (false? (:trigger/marker-found? result)))))

;; ---------------------------------------------------------------------------
;; should-compress? — cooldown
;; ---------------------------------------------------------------------------

(t/deftest cooldown-prevents-retrigger
  (let [s (long-str 8000)
        result (trig/should-compress? s (cfg :threshold 1000 :cooldown 500 :last-savings 100))]
    (t/is (false? (:trigger/compressed? result)))
    (t/is (= :cooldown (:trigger/reason result)))))

(t/deftest cooldown-expires-when-savings-exceeded
  (let [s (long-str 8000)
        result (trig/should-compress? s (cfg :threshold 1000 :cooldown 500 :last-savings 600))]
    (t/is (true? (:trigger/compressed? result)))
    (t/is (= :threshold-exceeded (:trigger/reason result)))))

(t/deftest cooldown-expires-when-no-last-savings
  (let [s (long-str 8000)
        result (trig/should-compress? s (cfg :threshold 1000 :cooldown 500))]
    (t/is (true? (:trigger/compressed? result)))
    (t/is (= :threshold-exceeded (:trigger/reason result)))))

;; ---------------------------------------------------------------------------
;; convenience wrappers
;; ---------------------------------------------------------------------------

(t/deftest compressed?-wrapper
  (t/is (true? (trig/compressed? (long-str 8000) (cfg :threshold 1000))))
  (t/is (false? (trig/compressed? (long-str 100) (cfg :threshold 1000)))))

(t/deftest trigger-reason-wrapper
  (t/is (= :no-trigger (trig/trigger-reason (long-str 100) (cfg :threshold 1000))))
  (t/is (= :threshold-exceeded (trig/trigger-reason (long-str 8000) (cfg :threshold 1000)))))

;; ---------------------------------------------------------------------------
;; error handling
;; ---------------------------------------------------------------------------

(t/deftest throws-on-non-string-context
  (try
    (trig/should-compress? nil (cfg :threshold 1000))
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/trigger-invalid (:error/type (ex-data e)))))))

(t/deftest throws-on-non-map-config
  (try
    (trig/should-compress? "text" "not a map")
    (t/is false "should have thrown")
    (catch Exception e
      (t/is (= :context/trigger-invalid (:error/type (ex-data e)))))))

(t/deftest default-config-works
  (let [s (long-str 16000)
        result (trig/should-compress? s {})]
    (t/is (true? (:trigger/compressed? result)))
    (t/is (= 4000 (:trigger/token-count result)))
    (t/is (= 4000 (:trigger/threshold result)))))

(t/run-tests)