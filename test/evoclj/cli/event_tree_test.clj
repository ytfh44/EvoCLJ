(ns evoclj.cli.event-tree-test
  "Feature O1: events! --tree returns the session's causal trace as
  a nested tree (children chained to their cause event)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.cli.session :as session]))

(defn- ev [seq-id type cause]
  {:event/seq seq-id :event/type type :cause/event-id cause})

(deftest event-tree-nests-causal-chains
  (testing "children are the events chained to their cause"
    (let [events [(ev 1 :session/created nil)
                  (ev 2 :session/started 1)
                  (ev 3 :intent/proposed 2)
                  (ev 4 :intent/authorized 3)
                  (ev 5 :provider/call-completed 4)]
          tree (session/event-tree events)]
      (is (= 1 (count (:roots tree))))
      (let [root (first (:roots tree))]
        (is (= 1 (:event/seq root)))
        (is (= :session/created (:event/type root)))
        (is (= 1 (count (:children root))))
        (is (= 2 (:event/seq (first (:children root)))))
        (let [proposed (first (:children (first (:children root))))]
          (is (= 3 (:event/seq proposed)))
          (is (= 1 (count (:children proposed)))))))))

(deftest event-tree-multiple-roots-and-orphans
  (testing "multiple roots and dangling causes are reported"
    (let [events [(ev 1 :session/created nil)
                  (ev 2 :session/started 1)
                  (ev 9 :intent/denied 42)]
          tree (session/event-tree events)]
      (is (= 1 (count (:roots tree))))
      (is (= 1 (count (:orphans tree))))
      (is (= 9 (:event/seq (first (:orphans tree))))
          "a cause pointing at an unknown seq is an orphan"))))

(deftest event-tree-empty
  (testing "an empty trace has no roots and no orphans"
    (let [tree (session/event-tree [])]
      (is (empty? (:roots tree)))
      (is (empty? (:orphans tree))))))
