(ns verify3-canary
  "Semantic verification #3 — deterministic canary allocation.
  Model: bucket = sha256(key)[0:16] mod 10000 / 10000 ~ U[0,1) under
  the SHA-256 uniformity assumption, so P(canary) = allocation p and
  the count over n independent keys ~ Binomial(n, p).
  Real code: evoclj.promotion.canary/routing-bucket."
  (:require [evoclj.promotion.canary :as canary]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def n 10000)
(def keys (mapv #(format "routing-key-%05d" %) (range n)))

(defn stats [p]
  (let [obs (count (filter #(< % p) (map canary/routing-bucket keys)))
        mu (* n p)
        sigma (Math/sqrt (* n p (- 1 p)))
        z (/ (- obs mu) sigma)]
    {:p p :obs obs :mu mu :sigma sigma :z z}))

(doseq [p [0.10 0.25 0.50]]
  (let [{:keys [obs mu sigma z]} (stats p)]
    (check! (str "canary allocation p=" p " within 4-sigma of Binomial(" n "," p ")")
            (< (Math/abs z) 4)
            (format "obs=%d mu=%.0f sigma=%.1f z=%.2f" obs mu sigma z))))

;; determinism: same key, same bucket, every time
(check! "routing-bucket is a pure function (same key -> same bucket)"
        (every? (fn [k] (= (canary/routing-bucket k) (canary/routing-bucket k))) keys)
        "10000 keys x 2 calls identical")

;; uniformity: bucket mean/variance vs U[0,1)
(let [bs (map canary/routing-bucket keys)
      mean (/ (reduce + 0.0 bs) n)
      var (/ (reduce + 0.0 (map #(Math/pow (- % 0.5) 2) bs)) n)]
  (check! "bucket distribution ~ U[0,1) (mean 0.5, var 1/12)"
          (and (< (Math/abs (- mean 0.5)) 0.02)
               (< (Math/abs (- var (/ 1.0 12))) 0.002))
          (format "mean=%.4f var=%.5f (expected 0.5, %.5f)" mean var (/ 1.0 12))))

;; ladder monotonicity: allocation p1 < p2 implies canary-set(p1) subset canary-set(p2)
(let [s10 (set (filter #(< (canary/routing-bucket %) 0.10) keys))
      s25 (set (filter #(< (canary/routing-bucket %) 0.25) keys))]
  (check! "ladder monotonicity: canary(10%) subset canary(25%)"
          (every? s25 s10)
          (str (count s10) " ⊆ " (count s25))))
(println "VERIFY3 DONE")
