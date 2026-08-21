(ns evoclj.context.binding-test
  (:require [clojure.test :as t]
            [evoclj.context.binding :as binding]
            [evoclj.context.offer :as offer]
            [evoclj.genome.hash :as hash]))

(defn- rev [s] (hash/text-digest s))

(t/deftest make-binding-valid
  (let [r (rev "content")
        b (binding/make-binding {:logical-id [:skill "debugging"]
                                 :revision-id r
                                 :bundle-id "bundle:1"})]
    (t/is (binding/binding? b))
    (t/is (= [:skill "debugging"] (:logical/id b)))
    (t/is (= r (:revision/id b)))
    (t/is (= :session (:scope b)))
    (t/is (= :active (:state b)))))

(t/deftest binding-from-offer-pins-revision
  (let [r (rev "skill-a")
        offer (offer/make-offer {:logical-id [:skill "debugging"] :revision-id r :bundle-id "bundle:a"})
        b (binding/binding-from-offer offer)]
    (t/is (= r (:revision/id b)))
    (t/is (= [:skill "debugging"] (:logical/id b)))
    (t/is (= "bundle:a" (:bundle/id b)))))

(t/deftest store-activate-and-list
  (let [store (binding/create-store)
        r1 (rev "a")
        o1 (offer/make-offer {:logical-id [:skill "debugging"] :revision-id r1 :bundle-id "bundle:a"})
        b1 (binding/activate! store o1)]
    (t/is (= b1 (binding/get-binding store [:skill "debugging"])))
    (t/is (= 1 (count (binding/list-active store))))
    ;; activate different skill
    (let [r2 (rev "b")
          o2 (offer/make-offer {:logical-id [:skill "other"] :revision-id r2 :bundle-id "bundle:b"})
          b2 (binding/activate! store o2)]
      (t/is (= 2 (count (binding/list-active store))))
      (t/is (= b2 (binding/get-binding store [:skill "other"]))))))

(t/deftest store-deactivate
  (let [store (binding/create-store)
        r (rev "a")
        o (offer/make-offer {:logical-id [:skill "debugging"] :revision-id r :bundle-id "bundle:a"})
        _ (binding/activate! store o)
        removed (binding/deactivate! store [:skill "debugging"])]
    (t/is (some? removed))
    (t/is (nil? (binding/get-binding store [:skill "debugging"])))
    (t/is (empty? (binding/list-active store)))))

(t/deftest store-replace-keeps-new-binding
  (let [store (binding/create-store)
        r1 (rev "a")
        r2 (rev "b")
        o1 (offer/make-offer {:logical-id [:skill "debugging"] :revision-id r1 :bundle-id "bundle:a"})
        o2 (offer/make-offer {:logical-id [:skill "debugging"] :revision-id r2 :bundle-id "bundle:b"})
        b1 (binding/activate! store o1)
        b2 (binding/activate! store o2)]
    (t/is (not= (:binding/id b1) (:binding/id b2)))
    (t/is (= r2 (:revision/id (binding/get-binding store [:skill "debugging"]))))))
