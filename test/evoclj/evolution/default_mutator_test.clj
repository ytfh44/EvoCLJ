(ns evoclj.evolution.default-mutator-test
  (:require [clojure.test :refer [deftest is]]
            [evoclj.evolution.core :as evolution]
            [evoclj.evolution.default-mutator :as dm]))

(deftest default-mutator-satisfies-protocol
  (is (satisfies? evolution/Mutator (dm/default-mutator))))

(deftest propose-mutations-returns-single-mutation
  (let [m (dm/default-mutator)
        context {:parent-genome {:files {"programs/route.clj" {:digest "sha256:parent"}}}
                 :diagnosis {}}
        result (evolution/propose-mutations m context)]
    (is (= 1 (count result)))
    (let [mutation (first result)]
      (is (= :program (:risk mutation)))
      (is (= "default/noop" (:hypothesis/id mutation)))
      (is (= 1 (count (:ops mutation))))
      (is (= :replace-form (get-in mutation [:ops 0 :op])))
      (is (= "sha256:parent" (get-in mutation [:ops 0 :expect/hash]))))))

(deftest propose-mutations-is-deterministic
  (let [m (dm/default-mutator)
        context {:parent-genome {:files {"programs/route.clj" {:digest "sha256:parent"}}}
                 :diagnosis {}}
        r1 (evolution/propose-mutations m context)
        r2 (evolution/propose-mutations m context)]
    (is (= r1 r2))))

(deftest invalid-context-throws-typed-error
  (let [m (dm/default-mutator)
        e (is (thrown-with-msg? clojure.lang.ExceptionInfo #"the Mutator context must be a map"
                                (evolution/propose-mutations m nil)))]
    (is (= :mutation/context-invalid (:error/type (ex-data e))))))
