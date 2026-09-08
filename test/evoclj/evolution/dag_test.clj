(ns evoclj.evolution.dag-test
  (:require [clojure.test :refer [deftest is]]
            [evoclj.evolution.dag :as dag]))

(deftest parent-order-and-digest-are-canonical
  (let [a (dag/canonical-parent-edges [{:parent-id "z" :provenance :a}
                                      {:parent-id "a" :provenance :b}])
        b (dag/canonical-parent-edges [{:parent-id "a" :provenance :other}
                                      {:parent-id "z" :provenance :else}])]
    (is (= a b))
    (is (= ["a" "z"] (mapv :parent-id a)))
    (is (= (dag/parent-edge-key a "sha256:merge")
           (dag/parent-edge-key b "sha256:merge")))))

(deftest merge-requires-base-and-rejects-overlap
  (let [base {:files {"a.txt" "one\ntwo\n"}}
        replace-a {:op :replace-text :file "a.txt"
                   :anchor 1 :text "ONE"}
        replace-b {:op :replace-text :file "a.txt"
                   :anchor 2 :text "TWO"}]
    (is (try (dag/plan-merge {:parents ["g-a" "g-b"] :ops [replace-a]})
             false
             (catch clojure.lang.ExceptionInfo _ true)))
    (let [merged (dag/plan-merge {:parents ["g-b" "g-a"] :base base
                                  :ops [replace-a replace-b]})]
      (is (= "ONE\nTWO\n" (get-in merged [:files "a.txt"])))
      (is (every? :expect/hash (:ops merged))))))

(deftest frontier-is-bounded-and-stable
  (let [xs [{:id "b" :score 3} {:id "a" :score 3} {:id "c" :score 1}]
        out (dag/bounded-frontier xs {:k 2 :eval-budget 3 :score-fn :score})]
    (is (= ["a" "b"] (mapv :id (:selected out))))
    (is (= ["c"] (mapv :id (:unselected out))))
    (is (= 3 (:evaluated out)))))
