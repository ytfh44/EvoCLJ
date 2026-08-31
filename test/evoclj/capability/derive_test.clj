(ns evoclj.capability.derive-test
  "P4 — derive-lease! attenuation: narrowing only, audit chain.
  Mirrors Wolfram [W-08..W-11]."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.mint :as mint]
            [evoclj.capability.schema :as schema])
  (:import (java.util Date UUID)))

(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private session-a (UUID/fromString "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"))
(def ^:private session-b (UUID/fromString "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"))

(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 1700003600000)) ; +1h
(def ^:private earlier-expires (Date. 1700001800000)) ; +30m, shorter
(def ^:private later-expires (Date. 1700007200000)) ; +2h, longer
(def ^:private later-issued (Date. 1700000900000)) ; +15m

(defn- parent-opts
  []
  {:cap-id (UUID/fromString "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
   :subject {:session/id session-a :phenotype/id phenotype-p1}
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:read :list :stat}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(defn- parent-lease
  ([] (parent-lease nil))
  ([registry]
   (let [reg (or registry (mint/create-lease-registry))]
     (mint/mint-lease! reg (parent-opts)))))

(defn- attenuation-invalid?
  [f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= :capability/attenuation-invalid (:error/type (ex-data e))))))

;; --- narrow Ok (actions subset, fewer calls, shorter window) passes ---

(deftest narrow-attenuation-passes
  (testing "narrow derived lease passes: actions subset, fewer max-calls, shorter window"
    (let [registry (mint/create-lease-registry)
          parent (mint/mint-lease! registry (parent-opts))
          child (mint/derive-lease! registry parent
                                     {:actions #{:read}
                                      :constraints {:max-calls 5}
                                      :issued-at later-issued
                                      :expires-at earlier-expires})]
      (is (schema/lease? child) "derived must be sealed")
      (is (= #{:read} (:actions child)) "actions narrowed")
      (is (= 5 (get-in child [:constraints :max-calls])) "max-calls narrowed")
      (is (= earlier-expires (:expires-at child)) "expires shortened")
      (is (= later-issued (:issued-at child)) "issued moved forward")
      (is (= (:resource parent) (:resource child)) "resource stays same")
      (is (= child (mint/get-lease registry (:cap/id child))) "recorded in registry")
      ;; audit chain
      (is (= (:cap/id parent) (get-in child [:constraints :cap/attenuated-from]))
          "constraints :cap/attenuated-from equals parent capId")
      (is (= (:cap/id parent) (get-in child [:constraints :attenuated-from]))
          "constraints :attenuated-from also set"))))

;; --- widen actions -> fails :capability/attenuation-invalid ---

(deftest widen-actions-fails-attenuation-invalid
  (testing "widening actions beyond parent fails with :capability/attenuation-invalid"
    (let [parent (parent-lease)
          widen #(mint/derive-lease! nil parent {:actions #{:read :write}})]
      (is (attenuation-invalid? widen)
          "adding :write outside parent #{:read :list :stat} must be rejected"))
    (testing "adding :invoke outside parent set fails"
      (let [parent (parent-lease)]
        (is (attenuation-invalid? #(mint/derive-lease! nil parent {:actions #{:read :invoke}})))))))

;; --- extend expires -> fails ---

(deftest extend-expires-fails-attenuation-invalid
  (testing "extending expires beyond parent fails"
    (let [parent (parent-lease)]
      (is (attenuation-invalid?
           #(mint/derive-lease! nil parent {:expires-at later-expires}))
          "child expires > parent expires must be rejected")))
  (testing "equal expires passes (boundary)"
    (let [parent (parent-lease)
          child (mint/derive-lease! nil parent {:expires-at expires-at})]
      (is (schema/lease? child))
      (is (= expires-at (:expires-at child))))))

;; --- max-calls beyond parent -> fails ---

(deftest max-calls-widen-fails-attenuation-invalid
  (testing "child max-calls > parent max-calls fails"
    (let [parent (parent-lease)]
      (is (attenuation-invalid?
           #(mint/derive-lease! nil parent {:constraints {:max-calls 20}}))
          "20 > parent 10 must be rejected")))
  (testing "child unlimited (nil) when parent finite fails"
    (let [parent (parent-lease)]
      (is (attenuation-invalid?
           #(mint/derive-lease! nil parent {:constraints {}}))
          "nil child max-calls widens past finite parent")))
  (testing "equal max-calls passes"
    (let [parent (parent-lease)
          child (mint/derive-lease! nil parent {:constraints {:max-calls 10}})]
      (is (schema/lease? child))
      (is (= 10 (get-in child [:constraints :max-calls])))))
  (testing "fewer max-calls passes"
    (let [parent (parent-lease)
          child (mint/derive-lease! nil parent {:constraints {:max-calls 3}})]
      (is (schema/lease? child))
      (is (= 3 (get-in child [:constraints :max-calls]))))))

;; --- chain audit ---

(deftest chain-audit-attenuated-from
  (testing "derived lease constraints carry :cap/attenuated-from == parent capId"
    (let [parent (parent-lease)
          child (mint/derive-lease! nil parent {:actions #{:read}
                                                :constraints {:max-calls 2}
                                                :expires-at earlier-expires})]
      (is (= (:cap/id parent) (get-in child [:constraints :cap/attenuated-from])))
      (is (= (:cap/id parent) (get-in child [:constraints :attenuated-from])))))
  (testing "chained derivation preserves downward closure (grandchild ⊆ child ⊆ parent)"
    (let [parent (parent-lease)
          child (mint/derive-lease! nil parent {:actions #{:read :list}
                                                :constraints {:max-calls 5}
                                                :expires-at earlier-expires})
          grandchild (mint/derive-lease! nil child {:actions #{:read}
                                                    :constraints {:max-calls 2}
                                                    :expires-at earlier-expires})]
      (is (= (:cap/id child) (get-in grandchild [:constraints :cap/attenuated-from])))
      (is (schema/lease? grandchild)))))

;; --- issued >= parent issued ---

(deftest issued-before-parent-fails
  (testing "child issued before parent issued fails"
    (let [parent (parent-lease)
          earlier (Date. 1699999000000)]
      (is (attenuation-invalid? #(mint/derive-lease! nil parent {:issued-at earlier}))))))

;; --- parent must be sealed and not revoked ---

(deftest parent-must-be-sealed-and-not-revoked
  (testing "non-sealed parent map is rejected"
    (is (attenuation-invalid?
         #(mint/derive-lease! nil {:cap/id (UUID/randomUUID) :fake true} {}))))
  (testing "revoked parent cannot be derived from"
    (let [registry (mint/create-lease-registry)
          parent (mint/mint-lease! registry (parent-opts))]
      (mint/revoke-lease! registry (:cap/id parent))
      (is (attenuation-invalid?
           #(mint/derive-lease! registry parent {:actions #{:read}}))))))

;; --- subject override for subagent delegation ---

(deftest subject-override-allowed
  (testing "subject override to child session still creates valid lease (resource same)"
    (let [parent (parent-lease)
          child-subject {:session/id session-b :phenotype/id phenotype-p1}
          child (mint/derive-lease! nil parent {:subject child-subject
                                                :actions #{:read}})]
      (is (schema/lease? child))
      (is (= child-subject (:subject child)))
      (is (= (:cap/id parent) (get-in child [:constraints :cap/attenuated-from])))))
  (testing "resource override to different resource fails"
    (let [parent (parent-lease)]
      (is (attenuation-invalid?
           #(mint/derive-lease! nil parent {:resource {:kind :tool :id :other/tool}}))))))
