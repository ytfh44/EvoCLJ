(ns evoclj.capability.capability-handle-test
  "Fleet S5/S6: sealed handles and closed registry — arbitrary fn never accepted.

  Tests the S5 sealed CapabilityHandle / ActivationHandle and S6 closed
  ResourceKindRegistry. This is the required test proving arbitrary fn is
  not accepted as capability (DAG S5/S6)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.broker.registry :as reg]
            [evoclj.capability.core :as cap]
            [evoclj.capability.broker :as broker]
            [evoclj.intent.core :as intent]
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
                                         :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
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

;; --- S6: Broker registry closed — explicit allowlist, not arbitrary keyword --

(deftest broker-registry-is-closed
  (testing "default registry is sealed and contains only allowlisted kinds"
    (let [r (reg/default-registry)]
      (is (reg/registry? r))
      (is (= #{:tool :model :memory :filesystem :filesystem/path} reg/allowed-resource-kinds))
      (doseq [k reg/allowed-resource-kinds]
        (is (reg/registry-contains? r k)))))
  (testing "make-registry rejects arbitrary keyword not in allowlist"
    (is (thrown? clojure.lang.ExceptionInfo (reg/make-registry {:arbitrary/keyword [{:source :request :action-from :request}]})))
    (try
      (reg/make-registry {:evil/kind [{:source :request :action-from :request}]})
      (catch clojure.lang.ExceptionInfo e
        (is (= :registry/invalid-kind (:error/type (ex-data e)))))))
  (testing "broker authorize rejects non-sealed registry (arbitrary map)"
    (let [session-id #uuid "11111111-1111-4111-8111-111111111111"
          phenotype "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          intent (intent/tool-call session-id phenotype :node/tool 42 {:tool/id :fixture/echo :args {:text "hi"}} {:wall-ms 1000})
          normalized {:tool/id :fixture/echo :resource {:kind :tool :id :fixture/echo}}]
      (is (thrown? clojure.lang.ExceptionInfo
                   (broker/authorize {:intent intent
                                      :normalized-request normalized
                                      :leases []
                                      :now (java.util.Date.)
                                      :registry {:arbitrary/keyword [{:source :request :action-from :request}]}})))
      (try
        (broker/authorize {:intent intent :normalized-request normalized :leases [] :now (java.util.Date.) :registry {:bad/kind [{:source :request :action-from :request}]}})
        (catch clojure.lang.ExceptionInfo e
          (is (= :registry/invalid-kind (:error/type (ex-data e))))))))
  (testing "C1: kind recognition comes from the Descriptor registry, not the :registry override"
    (let [session-id #uuid "11111111-1111-4111-8111-111111111111"
          phenotype "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          intent (intent/tool-call session-id phenotype :node/tool 42 {:tool/id :fixture/echo :args {:text "hi"}} {:wall-ms 1000})
          r (reg/make-registry {:tool [{:source :request :action-from :request}]})]
      (is (reg/registry? r))
      ;; :filesystem is a registered built-in descriptor, so it is a KNOWN kind —
      ;; with no granting lease the broker returns :capability/missing, not unknown.
      (let [fs-norm {:tool/id :fixture/echo :resource {:kind :filesystem :path "/tmp/x"}}]
        (is (= :capability/missing
               (:reason (broker/authorize {:intent intent :normalized-request fs-norm :leases [] :now (java.util.Date.) :registry r})))))
      ;; a kind that is NOT registered in the Descriptor registry is unknown, fail-closed.
      (let [unknown-norm {:tool/id :fixture/echo :resource {:kind :no/such-kind :id :x}}]
        (is (= :capability/unknown-resource-kind
               (:reason (broker/authorize {:intent intent :normalized-request unknown-norm :leases [] :now (java.util.Date.) :registry r}))))))))

;; --- S5/S6 integration: definition > validation -------------------------------

(deftest registry-definition-is-single-source
  (testing "allowed-resource-kinds is the single definition; registry validates against it"
    (is (= reg/allowed-resource-kinds #{:tool :model :memory :filesystem :filesystem/path}))
    (is (contains? reg/allowed-resource-kinds :tool))
    (is (not (contains? reg/allowed-resource-kinds :arbitrary/keyword)))))
