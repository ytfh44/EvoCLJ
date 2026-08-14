(ns verify4-eligibility
  "Semantic verification #4 — lexicographic eligibility (GC 14).
  Model: eligible? = AND(no-hard-violation,
                         utility-delta >= min-delta,
                         every cost ratio <= max-cost-regression,
                         complexity guard if configured).
  Properties checked by exhaustive enumeration over a small domain,
  against the REAL evoclj.eval.compare/eligibility."
  (:require [evoclj.eval.compare :as compare]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def profile {:eval/profile-id :verify
              :evolution-set {:source :evolution-fixture}
              :selection-set {:source :selection-fixture :visibility :kernel-only}
              :audit-set {:source :audit-fixture :visibility :operator-only}
              :repetitions 1
              :promotion {:strategy :canary
                          :min-delta 0.05
                          :max-cost-regression 1.10}})

(defn summary [hard-ok? util-delta cost-ratio]
  ;; hard entries pass only when they carry NO violations AND both
  ;; sides are :pass (metrics/hard-violations contract)
  {:hard {:safety {:parent :pass :candidate :pass
                   :violations (if hard-ok? [] [{:kind :policy-violation}])}}
   :utility {:task/success {:parent 0.5 :candidate (+ 0.5 util-delta)}}
   :cost {:tokens/task {:parent 1000 :candidate (* 1000 cost-ratio)}}
   :complexity {:genome-bytes {:parent 1000 :candidate 1000}}})

(def hard-vals [true false])
(def util-vals [-0.10 0.0 0.049 0.05 0.20])
(def cost-vals [1.0 1.05 1.10 1.11 1.50])

(defn eligible? [h u c] (:eligible? (compare/eligibility (summary h u c) profile)))

;; P1: hard violation dominates — ineligible for EVERY utility/cost combo
(doseq [u util-vals c cost-vals]
  (check! (format "hard violation => ineligible (u=%s c=%s)" u c)
          (false? (eligible? false u c))
          "no utility/cost can compensate"))

;; P2: below min-delta => ineligible (no hard violation, any cost)
(doseq [u [-0.10 0.0 0.049] c cost-vals]
  (check! (format "utility %.3f < min-delta 0.05 => ineligible (c=%s)" u c)
          (false? (eligible? true u c))
          "min-delta is a floor, not a preference"))

;; P3: cost regression beyond ceiling => ineligible even with big utility gain
(doseq [c [1.11 1.50]]
  (check! (format "cost ratio %.2f > 1.10 => ineligible despite u=0.20" c)
          (false? (eligible? true 0.20 c))
          "cost guardrail binds after utility"))

;; P4: fully passing => eligible
(let [combos [[0.05 1.10] [0.20 1.0] [0.20 1.05]]]
  (check! "hard ok + u>=min-delta + cost<=1.10 => eligible"
          (every? (fn [[u c]] (true? (eligible? true u c))) combos)
          "all-pass combinations are eligible"))

;; P5: monotonicity of the decision in utility (fix hard=ok, cost=1.0):
;; eligible?(u) true for u >= min-delta; ineligible for u < min-delta.
(let [res (map (fn [u] [u (eligible? true u 1.0)]) util-vals)]
  (check! "eligibility is a step function at min-delta (utility dimension)"
          (= [false false false true true] (mapv second res))
          (pr-str res)))
(println "VERIFY4 DONE")
