(ns evoclj.context.policy-test
  (:require [clojure.test :as t]
            [evoclj.context.policy :as policy]))

(t/deftest allowed-when-nil-policy
  (let [b {:logical/id [:skill "debugging"]}]
    (t/is (policy/allowed? nil b))))

(t/deftest allowed-with-allowlist
  (let [pol {:policy/allowed #{[:skill "debugging"]}}
        b1 {:logical/id [:skill "debugging"]}
        b2 {:logical/id [:skill "other"]}]
    (t/is (policy/allowed? pol b1))
    (t/is (not (policy/allowed? pol b2)))))

(t/deftest denied-takes-precedence
  (let [pol {:policy/allowed #{[:skill "debugging"] [:skill "other"]}
             :policy/denied #{[:skill "other"]}}
        b {:logical/id [:skill "other"]}]
    (t/is (not (policy/allowed? pol b)))))

(t/deftest filter-bindings-respects-max
  (let [bindings [{:logical/id [:skill "a"]} {:logical/id [:skill "b"]} {:logical/id [:skill "c"]}]
        pol {:policy/max-segments 2}
        filtered (policy/filter-bindings pol bindings)]
    (t/is (= 2 (count filtered)))))
