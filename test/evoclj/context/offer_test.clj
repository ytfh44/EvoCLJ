(ns evoclj.context.offer-test
  (:require [clojure.test :as t]
            [evoclj.context.offer :as offer]
            [evoclj.genome.hash :as hash]))

(t/deftest make-offer-valid
  (let [rev (hash/text-digest "skill content")]
    (let [o (offer/make-offer {:logical-id [:skill "debugging"]
                               :revision-id rev
                               :bundle-id "bundle:1"
                               :name "debugging"
                               :description "debug skill"})]
      (t/is (offer/offer? o))
      (t/is (= [:skill "debugging"] (:offer/logical-id o)))
      (t/is (= rev (:offer/revision-id o))))))

(t/deftest make-offer-rejects-bad-revision
  (t/is (thrown? clojure.lang.ExceptionInfo
                 (offer/make-offer {:logical-id [:skill "x"]
                                    :revision-id "bad-id"
                                    :bundle-id "bundle:1"}))))

(t/deftest catalog-projection-lookup
  (let [rev1 (hash/text-digest "a")
        rev2 (hash/text-digest "b")
        o1 (offer/make-offer {:logical-id [:skill "a"] :revision-id rev1 :bundle-id "b1"})
        o2 (offer/make-offer {:logical-id [:skill "b"] :revision-id rev2 :bundle-id "b2"})
        proj (offer/catalog-projection [o1 o2])]
    (t/is (= o1 (offer/current-offer proj [:skill "a"])))
    (t/is (= o2 (offer/current-offer proj [:skill "b"])))
    (t/is (nil? (offer/current-offer proj [:skill "missing"])))))
