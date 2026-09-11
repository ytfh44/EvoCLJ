(ns evoclj.capability.capability-handle-test
  "Fleet S5: sealed handles — arbitrary fn never accepted.

  Tests the S5 sealed CapabilityHandle / ActivationHandle. This is the
  required test proving arbitrary fn is not accepted as capability (DAG S5)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.core :as cap]
            [evoclj.promotion.activation :as activation]))

;; --- S5: CapabilityHandle is sealed — arbitrary fn never accepted ------------

(deftest capability-handle-is-sealed
  (testing "arbitrary fn is not a CapabilityHandle"
    (let [f (fn [] :evil)]
      (is (not (cap/capability-handle? f)) "raw fn is not a handle")
      (is (not (cap/capability-handle? {:handle/id (java.util.UUID/randomUUID)})) "plain map is not a handle")
      (is (not (cap/capability-handle? nil)) "nil is not a handle")))
  (testing "activation via raw fn throws :capability/handle-invalid"
    (let [f (fn [] :evil)]
      (is (thrown? clojure.lang.ExceptionInfo (cap/assert-capability-handle! f)))
      (try
        (cap/assert-capability-handle! f)
        (catch clojure.lang.ExceptionInfo e
          (is (= :capability/handle-invalid (:error/type (ex-data e))))))))
  (testing "make-capability-handle produces a sealed handle that passes predicate"
    (let [h (cap/make-capability-handle {:handle-id (java.util.UUID/randomUUID)
                                         :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
                                         :resource {:kind :tool :id :fixture/echo}
                                         :action :invoke
                                         :lease-id (java.util.UUID/randomUUID)})]
      (is (cap/capability-handle? h))
      (is (not (cap/capability-handle? {:handle/id (java.util.UUID/randomUUID) :secret "fake"})) "fake map not handle")))
  (testing "arbitrary fn cannot be used where handle is required — activation only via handle"
    (let [f (fn [] :activate)
          h (activation/make-activation-handle "G42")]
      (is (not (activation/activation-handle? f)) "fn is not activation handle")
      (is (activation/activation-handle? h) "sealed handle is recognized")
      (is (thrown? clojure.lang.ExceptionInfo (activation/assert-activation-handle! f)))
      (try
        (activation/assert-activation-handle! f)
        (catch clojure.lang.ExceptionInfo e
          (is (= :promotion/activation-denied (:error/type (ex-data e))))))
      (is (thrown? clojure.lang.ExceptionInfo (activation/activate-with-handle f (fn [] :ok))))
      (is (= :ok (activation/activate-with-handle h (fn [] :ok)))))))

;; --- S5: ActivationHandle sealed — arbitrary fn never grants activation rights

(deftest activation-handle-sealed
  (testing "promotion activation requires handle, not raw fn"
    (let [raw-fn (fn [] (throw (ex-info "should not run" {})))
          handle (activation/make-activation-handle "G99")]
      (is (not (activation/activation-handle? raw-fn)))
      (is (activation/activation-handle? handle))
      (is (not (activation/activation-handle? {:generation/id "G99" :handle/id (java.util.UUID/randomUUID)})))))
  (testing "make-activation-handle validates generation-id"
    (is (thrown? clojure.lang.ExceptionInfo (activation/make-activation-handle nil)))
    (is (thrown? clojure.lang.ExceptionInfo (activation/make-activation-handle 42)))))
