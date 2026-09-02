(ns evoclj.capability.subject-anchor-test
  "I2 — Principal equality is identity; session pin validation separate.
  Verifies: mint with valid principal succeeds, different principals diverge, same principal matches."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.lease :as lease]
            [evoclj.capability.mint :as mint]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [evoclj.mount.filesystem :as fs])
  (:import (java.util Date UUID)))

(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 1700003600000))

(def ^:private session-a (UUID/fromString "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"))
(def ^:private session-b (UUID/fromString "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"))

(def ^:private principal-a {:principal/type :session :session/id session-a})
(def ^:private principal-b {:principal/type :session :session/id session-b})
(def ^:private job-a {:principal/type :job :job/id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc"})
(def ^:private operator {:principal/type :operator})

(defn- base-lease-opts [principal]
  {:cap-id (UUID/fromString "11111111-1111-4111-8111-111111111111")
   :principal principal
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(deftest mint-with-principal-succeeds
  (testing "mint-lease! with valid SessionPrincipal succeeds"
    (let [lease (mint/mint-lease! nil (base-lease-opts principal-a))]
      (is (schema/lease? lease))
      (is (= principal-a (:principal lease)))))
  (testing "mint with OperatorPrincipal succeeds"
    (let [lease (mint/mint-lease! nil (base-lease-opts operator))]
      (is (schema/lease? lease))))
  (testing "mint with missing principal is rejected"
    (let [opts (dissoc (base-lease-opts principal-a) :principal)]
      (is (= :capability/schema-invalid (:error/type (ex-data (try (mint/mint-lease! nil opts) nil (catch clojure.lang.ExceptionInfo e e)))))))))

(deftest principal-equality-is-identity
  (testing "same principal matches"
    (let [lease (mint/mint-lease! nil (base-lease-opts principal-a))]
      (is (lease/principal-matches? lease principal-a))))
  (testing "different session principals do not match"
    (let [lease (mint/mint-lease! nil (base-lease-opts principal-a))]
      (is (not (lease/principal-matches? lease principal-b)))))
  (testing "different principal types do not match"
    (let [lease (mint/mint-lease! nil (base-lease-opts principal-a))]
      (is (not (lease/principal-matches? lease job-a)))
      (is (not (lease/principal-matches? lease operator))))))

(deftest filesystem-lease-requires-principal
  (testing "filesystem lease with valid principal is verifiable"
    (let [reg (fs/create-lease-registry)
          lease (fs/issue-fs-lease reg {:principal principal-a :mount-id [:workspace "ws"] :path "" :actions #{:read} :issued-at issued-at :expires-at expires-at})]
      (is (schema/lease? lease))
      (is (= lease (fs/verify-fs-lease! lease {:now issued-at :principal principal-a :registry reg})))))
  (testing "filesystem lease with different principal fails principal-mismatch"
    (let [reg (fs/create-lease-registry)
          lease (fs/issue-fs-lease reg {:principal principal-a :mount-id [:workspace "ws"] :path "" :actions #{:read} :issued-at issued-at :expires-at expires-at})]
      (is (thrown? clojure.lang.ExceptionInfo (fs/verify-fs-lease! lease {:now issued-at :principal principal-b :registry reg}))))))
