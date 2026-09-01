(ns evoclj.capability.mint-test
  "P2 — unified mint-lease! single issuance surface.
  Covers Wolfram [W-04]/[W-07] positive window and registry recording.
  Mirrors P1 sealed-lease invariants but through the mint facade."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.mint :as mint]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [evoclj.mount.filesystem :as fs]
            [evoclj.mount.backend :as backend])
  (:import (java.util Date UUID)))

(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private session-a #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")

(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 1700003600000))

(defn- base-opts
  []
  {:cap-id #uuid "11111111-1111-4111-8111-111111111111"
   :principal {:principal/type :session :session/id session-a}
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:invoke}
   :constraints {}
   :issued-at issued-at
   :expires-at expires-at})

;; --- 1. mint creates sealed lease that validates ---

(deftest mint-creates-sealed-lease-that-validates
  (testing "mint-lease! returns a sealed CapabilityLease that passes validate-lease"
    (let [lease (mint/mint-lease! nil (base-opts))]
      (is (schema/lease? lease) "must be sealed")
      (is (identical? lease (schema/validate-lease lease)) "sealed lease validates")
      (is (= #uuid "11111111-1111-4111-8111-111111111111" (:cap/id lease)))
      (is (= {:principal/type :session :session/id session-a} (:principal lease)))
      (is (= {:kind :tool :id :fixture/echo} (:resource lease)))
      (is (= #{:invoke} (:actions lease))))))
;; --- 2. mint with zero window throws :capability/schema-invalid ---

(deftest mint-zero-window-throws-schema-invalid
  (testing "mint with issued == expires is rejected with :capability/schema-invalid"
    (let [opts (assoc (base-opts) :issued-at issued-at :expires-at issued-at)]
      (try
        (mint/mint-lease! nil opts)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :capability/schema-invalid (:error/type (ex-data e))))))))
  (testing "mint with expires before issued is rejected"
    (let [opts (assoc (base-opts) :issued-at expires-at :expires-at issued-at)]
      (try
        (mint/mint-lease! nil opts)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :capability/schema-invalid (:error/type (ex-data e)))))))))

;; --- 3. mint records in registry (get-lease returns it) ---

(deftest mint-records-in-registry
  (testing "mint with registry atom records lease so get-lease returns it"
    (let [registry (fs/create-lease-registry)
          opts (assoc (base-opts) :cap-id #uuid "22222222-2222-4222-8222-222222222222")
          lease (mint/mint-lease! registry opts)]
      (is (schema/lease? lease))
      (is (= lease (fs/get-lease registry (:cap/id lease)))
          "get-lease must return the sealed lease")
      (is (false? (fs/lease-revoked? registry (:cap/id lease)))
          "freshly minted lease is not revoked"))))

;; --- 4. multiple mints produce distinct capIds ---

(deftest multiple-mints-produce-distinct-cap-ids
  (testing "mints without explicit cap-id produce distinct UUIDs"
    (let [opts (dissoc (base-opts) :cap-id)
          a (mint/mint-lease! nil opts)
          b (mint/mint-lease! nil opts)
          c (mint/mint-lease! nil opts)]
      (is (not= (:cap/id a) (:cap/id b)))
      (is (not= (:cap/id b) (:cap/id c)))
      (is (not= (:cap/id a) (:cap/id c)))
      (is (= 3 (count (set [(:cap/id a) (:cap/id b) (:cap/id c)])))))))
