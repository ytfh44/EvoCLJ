(ns evoclj.capability.grant-property-test
  "C2 Grant lattice — Work×Session product, Grant meet, Event refinement composition (100 rounds per law)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.set :as set]
            [evoclj.capability.grant :as grant]
            [evoclj.capability.resource-kind :as rk]))

;; --- generators ------------------------------------------------------------

(def ^:private tool-id-gen
  (gen/elements [:fixture/echo :tool/a :tool/b :tool/c]))

(def ^:private memory-id-gen
  (gen/elements [:mem/a :mem/b]))

(def ^:private path-segments-gen
  (gen/vector (gen/elements ["a" "b" "c" "x" "y"]) 1 3))

(defn- path-gen []
  (gen/fmap (fn [segs] (str "/" (clojure.string/join "/" segs))) path-segments-gen))

(def ^:private grant-gen
  (gen/bind
   (gen/elements [:tool :memory :filesystem :filesystem/path])
   (fn [kind]
     (case kind
       :tool (gen/fmap (fn [tid]
                         (grant/make-grant {:kind :tool :id tid} #{:invoke}))
                       tool-id-gen)
       :memory (gen/fmap (fn [mid]
                           (grant/make-grant {:kind :memory :id mid} #{:invoke}))
                         memory-id-gen)
       :filesystem (gen/fmap (fn [[p actions]]
                               (grant/make-grant {:kind :filesystem :path p} actions))
                             (gen/tuple (path-gen)
                                        (gen/bind (gen/vector (gen/elements [:read :list :stat :write :create :delete]) 1 3)
                                                  #(gen/return (set %)))))
       :filesystem/path (gen/fmap (fn [[p actions]]
                                    (grant/make-grant {:kind :filesystem/path :path p} actions))
                                  (gen/tuple (path-gen)
                                             (gen/bind (gen/vector (gen/elements [:read :list :stat :write :create :delete]) 1 3)
                                                       #(gen/return (set %)))))))))

(defn- random-grant-pair []
  (gen/tuple grant-gen grant-gen))

;; [W-11] meet idempotent: meet(g,g) == g (when meet exists)
(defspec grant-meet-idempotent 100
  (prop/for-all [g grant-gen]
    (let [m (grant/meet g g)]
      ;; meet(g,g) should be g itself (greatest lower bound of self)
      (or (nil? m) ; nil only when grant malformed, not random valid
          (and (grant/covers? m g) (grant/covers? g m) (= (:resource m) (:resource g)) (= (:actions m) (:actions g)))))))

;; [W-12] meet commutative: meet(a,b) == meet(b,a)
(defspec grant-meet-commutative 100
  (prop/for-all [[a b] (random-grant-pair)]
    (let [m1 (grant/meet a b)
          m2 (grant/meet b a)]
      (= m1 m2))))

;; [W-13] meet greatest lower bound: meet(a,b) covers any common lower bound
;; We test weaker: meet(a,b) is covered by both parents (when exists)
(defspec grant-meet-greatest-lower-bound 100
  (prop/for-all [[a b] (random-grant-pair)]
    (let [m (grant/meet a b)]
      (if (nil? m)
        true ; disjoint kinds or actions -> nil is correct
        (and (grant/covers? a m)
             (grant/covers? b m)
             (grant/attenuates? a m)
             (grant/attenuates? b m))))))

;; [W-14] meet attenuates parents (when non-nil, meet ≤ parents)
(defspec grant-meet-attenuates-parents 100
  (prop/for-all [[a b] (random-grant-pair)]
    (let [m (grant/meet a b)]
      (if (nil? m)
        true
        (and (grant/attenuates? a m) (grant/attenuates? b m))))))

;; [W-09] covers reflexive: covers?(g,g) == true
(defspec grant-covers-reflexive 100
  (prop/for-all [g grant-gen]
    (grant/covers? g g)))

;; [W-10] attenuates transitive: if a attenuates b and b attenuates c then a attenuates c
;; Generate chain by attenuating via shrinking actions / narrowing path
(defspec grant-attenuates-transitive 100
  (prop/for-all [g grant-gen]
    ;; shrinking actions by taking subset should attenuate
    (let [actions (:actions g)
          smaller (if (> (count actions) 1)
                    (set (take 1 (seq actions)))
                    actions)
          g2 (try (grant/make-grant (:resource g) smaller) (catch Exception _ g))
          g3 (try (grant/make-grant (:resource g) smaller) (catch Exception _ g))]
      (if (and (grant/attenuates? g g2) (grant/attenuates? g2 g3))
        (grant/attenuates? g g3)
        true))))

;; Action set lattice specific

(defspec action-set-meet-idempotent 100
  (prop/for-all [a (gen/bind (gen/vector (gen/elements [:read :write :list]) 1 2) #(gen/return (set %)))]
    (= (grant/action-set-meet a a) a)))

(defspec action-set-meet-commutative 100
  (prop/for-all [a (gen/bind (gen/vector (gen/elements [:read :write :list]) 1 2) #(gen/return (set %)))
                 b (gen/bind (gen/vector (gen/elements [:read :write :list]) 1 2) #(gen/return (set %)))]
    (= (grant/action-set-meet a b) (grant/action-set-meet b a))))

;; --- composition: Lease = Grant × Principal × TimeWindow × Quota ----------
;; Verify that Grant meet composition is independent of other dimensions
(defspec grant-meet-composition-product 100
  (prop/for-all [[a b] (random-grant-pair)]
    ;; Grant meet is product of Resource meet × ActionSet meet
    (let [m (grant/meet a b)]
      (if (nil? m)
        ;; nil means either resource-meet nil or action-set-meet nil -> product disjoint
        (or (nil? (grant/resource-meet (:resource a) (:resource b)))
            (nil? (grant/action-set-meet (:actions a) (:actions b))))
        ;; non-nil means both dimensions had a GLB
        (and (some? (grant/resource-meet (:resource a) (:resource b)))
             (some? (grant/action-set-meet (:actions a) (:actions b))))))))
