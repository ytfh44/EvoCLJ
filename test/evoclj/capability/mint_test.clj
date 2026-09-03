(ns evoclj.capability.mint-test
  "P2 — unified mint-lease! single issuance surface.
  Covers Wolfram [W-04]/[W-07] positive window and registry recording.
  Mirrors P1 sealed-lease invariants but through the mint facade."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.mint :as mint]
            [evoclj.capability.authority-store :as authority]
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

;; --- 5. fail-closed: authority whose durable write fails must not update cache ---

(defn- failing-authority
  "A purpose-built AuthorityStore whose insert-lease! always throws a typed
  :capability/authority-unavailable (simulating an unavailable DB)."
  []
  (reify authority/AuthorityStore
    (insert-lease! [_ _lease]
      (throw (err/error :capability/authority-unavailable "durable write failed" {})))
    (revoke! [_ _cap-id] nil)
    (hydrate! [_ _registry] 0)
    (active-by-principal [_ _principal] [])))

(deftest mint-fails-closed-on-authority-failure
  (testing "a failing durable authority throws and the memory cache is NOT updated"
    (let [registry (mint/create-lease-registry)
          cap-id #uuid "33333333-3333-4333-8333-333333333333"
          opts (assoc (base-opts) :cap-id cap-id)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (mint/mint-lease! (failing-authority) registry opts)))
      (is (nil? (mint/get-lease registry cap-id))
          "cache must NOT contain the lease after a failed durable write")
      (is (= 0 (mint/registry-version registry))
          "registry version must be unchanged after a failed durable write"))))

(deftest derive-fails-closed-on-authority-failure
  (testing "derive with a failing authority throws and the child is NOT cached"
    (let [registry (mint/create-lease-registry)
          parent (mint/mint-lease! registry (base-opts))
          child-cap-id #uuid "44444444-4444-4444-8444-444444444444"]
      (is (thrown? clojure.lang.ExceptionInfo
                   (mint/derive-lease! (failing-authority) registry parent
                                       {:actions #{:invoke} :cap-id child-cap-id})))
      (is (nil? (mint/get-lease registry child-cap-id))
          "child lease must NOT be cached after a failed durable write")
      (is (= 1 (mint/registry-version registry))
          "only the parent mint bumped the version; failed derive must not"))))
