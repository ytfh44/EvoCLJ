(ns evoclj.capability.policy-action-test
  "P6 — per-kind action sets, de-fold :invoke (de-collapse).

  Three checks:
  1. tool lease with #{:invoke} allows :invoke but denies :write
  2. filesystem lease with #{:read} allows :read but denies :write
  3. unknown action is denied fail-closed (unknown-action or action-denied)
  Also regression: existing :invoke behavior still works via fallback."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.policy :as policy]
            [evoclj.intent.core :as intent]))

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})

(def ^:private issued-at (java.util.Date. 1700000000000))
(def ^:private expires-at (java.util.Date. 1700003600000))
(def ^:private in-window (java.util.Date. 1700001800000))

(defn- tool-intent []
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/echo :args {:text "hi"}}
                    budget))

(defn- fs-intent []
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/path-resolve :args {:path "/work/secret"}}
                    budget))

(deftest tool-lease-invoke-allows-invoke-denies-write
  (testing "tool lease with #{:invoke} allows :invoke request but denies :write"
    (let [lease {:cap/id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                 :subject {:session/id session-id :phenotype/id phenotype-p1}
                 :resource {:kind :tool :id :fixture/echo}
                 :actions #{:invoke}
                 :constraints {:max-calls 10}
                 :issued-at issued-at
                 :expires-at expires-at}
          base-req {:tool/id :fixture/echo
                    :resource {:kind :tool :id :fixture/echo}
                    :args {:text "hi"}}
          invoke-req (assoc-in base-req [:resource :action] :invoke)
          write-req  (assoc-in base-req [:resource :action] :write)
          ;; also support top-level :action for tool per P6 spec
          write-req-top (assoc base-req :action :write)
          intent (tool-intent)]
      (testing "via broker: :invoke allows"
        (is (= :allow (:decision (broker/authorize {:intent intent
                                                    :normalized-request invoke-req
                                                    :leases [lease]
                                                    :usage {}
                                                    :now in-window}))))
        (is (= :allow (:decision (broker/authorize {:intent intent
                                                    :normalized-request base-req
                                                    :leases [lease]
                                                    :usage {}
                                                    :now in-window})))
            "backward compat: no :action defaults to :invoke"))
      (testing "via broker: :write is denied when lease only has :invoke"
        (let [d (broker/authorize {:intent intent
                                   :normalized-request write-req
                                   :leases [lease]
                                   :usage {}
                                   :now in-window})]
          (is (= :deny (:decision d)))
          (is (contains? #{:capability/action-denied :capability/unknown-action :capability/denied} (:reason d))))
        (let [d (broker/authorize {:intent intent
                                   :normalized-request write-req-top
                                   :leases [lease]
                                   :usage {}
                                   :now in-window})]
          (is (= :deny (:decision d)))
          (is (contains? #{:capability/action-denied :capability/unknown-action :capability/denied} (:reason d)))))
      (testing "via policy/decide directly"
        (let [subject {:session/id session-id :phenotype/id phenotype-p1}
              res {:kind :tool :id :fixture/echo}]
          (is (= :allow (:decision (policy/decide [lease] subject res :invoke in-window {}))))
          (is (= :capability/action-denied (:reason (policy/decide [lease] subject res :write in-window {}))))))
      (testing "tool lease with #{:read} allows :read but not :invoke"
        (let [read-lease (assoc lease :cap/id #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb" :actions #{:read})
              read-req (assoc-in base-req [:resource :action] :read)]
          (is (= :allow (:decision (broker/authorize {:intent intent
                                                      :normalized-request read-req
                                                      :leases [read-lease]
                                                      :usage {}
                                                      :now in-window}))))
          (is (= :deny (:decision (broker/authorize {:intent intent
                                                     :normalized-request invoke-req
                                                     :leases [read-lease]
                                                     :usage {}
                                                     :now in-window})))))))))

(deftest filesystem-lease-read-allows-read-denies-write
  (testing "filesystem lease with #{:read} allows :read but denies :write"
    (let [fs-lease {:cap/id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc"
                    :subject {:session/id session-id :phenotype/id phenotype-p1}
                    :resource {:kind :filesystem :path "/work"}
                    :actions #{:read}
                    :constraints {:max-calls 10}
                    :issued-at issued-at
                    :expires-at expires-at}
          read-req {:tool/id :fixture/path-resolve
                    :resource {:kind :filesystem :path "/work/secret" :action :read}
                    :args {:path "/work/secret"}}
          write-req {:tool/id :fixture/path-resolve
                     :resource {:kind :filesystem :path "/work/secret" :action :write}
                     :args {:path "/work/secret"}}
          intent (fs-intent)]
      (testing "via broker"
        (is (= :allow (:decision (broker/authorize {:intent intent
                                                    :normalized-request read-req
                                                    :leases [fs-lease]
                                                    :usage {}
                                                    :now in-window}))))
        (let [d (broker/authorize {:intent intent
                                   :normalized-request write-req
                                   :leases [fs-lease]
                                   :usage {}
                                   :now in-window})]
          (is (= :deny (:decision d)))
          (is (= :capability/action-denied (:reason d)))))
      (testing "via policy/decide directly"
        (let [subject {:session/id session-id :phenotype/id phenotype-p1}
              res {:kind :filesystem :path "/work/secret"}]
          (is (= :allow (:decision (policy/decide [fs-lease] subject res :read in-window {}))))
          (is (= :capability/action-denied (:reason (policy/decide [fs-lease] subject res :write in-window {}))))))
      (testing "filesystem lease with #{:write} allows :write"
        (let [write-lease (assoc fs-lease :cap/id #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd" :actions #{:write})]
          (is (= :allow (:decision (broker/authorize {:intent intent
                                                      :normalized-request write-req
                                                      :leases [write-lease]
                                                      :usage {}
                                                      :now in-window})))))))))

(deftest unknown-action-denied-fail-closed
  (testing "unknown action is denied fail-closed"
    (let [lease {:cap/id #uuid "eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"
                 :subject {:session/id session-id :phenotype/id phenotype-p1}
                 :resource {:kind :tool :id :fixture/echo}
                 :actions #{:invoke}
                 :constraints {:max-calls 10}
                 :issued-at issued-at
                 :expires-at expires-at}
          unknown-req {:tool/id :fixture/echo
                       :resource {:kind :tool :id :fixture/echo :action :unknown-verb}
                       :args {:text "hi"}}
          intent (tool-intent)
          d (broker/authorize {:intent intent
                               :normalized-request unknown-req
                               :leases [lease]
                               :usage {}
                               :now in-window})]
      (is (= :deny (:decision d)))
      (is (contains? #{:capability/unknown-action :capability/action-denied :capability/denied} (:reason d))
          "unknown action must be denied, not allowed"))
    (testing "unknown action via policy/decide is also denied"
      (let [lease {:cap/id #uuid "ffffffff-ffff-4fff-8fff-ffffffffffff"
                   :subject {:session/id session-id :phenotype/id phenotype-p1}
                   :resource {:kind :tool :id :fixture/echo}
                   :actions #{:invoke}
                   :constraints {}
                   :issued-at issued-at
                   :expires-at expires-at}
            subject {:session/id session-id :phenotype/id phenotype-p1}
            res {:kind :tool :id :fixture/echo}]
        (is (= :capability/action-denied (:reason (policy/decide [lease] subject res :unknown-verb in-window {}))))))))

(deftest regression-invoke-still-allows
  (testing "regression: existing :invoke leases still allow when no explicit action"
    (let [lease {:cap/id #uuid "10101010-1010-4101-8101-101010101010"
                 :subject {:session/id session-id :phenotype/id phenotype-p1}
                 :resource {:kind :tool :id :fixture/echo}
                 :actions #{:invoke}
                 :constraints {}
                 :issued-at issued-at
                 :expires-at expires-at}
          req {:tool/id :fixture/echo
               :resource {:kind :tool :id :fixture/echo}
               :args {:text "hi"}}
          intent (tool-intent)]
      (is (= :allow (:decision (broker/authorize {:intent intent
                                                  :normalized-request req
                                                  :leases [lease]
                                                  :usage {}
                                                  :now in-window}))))
      (is (= :allow (:decision (policy/decide [lease]
                                              {:session/id session-id :phenotype/id phenotype-p1}
                                              {:kind :tool :id :fixture/echo}
                                              :invoke in-window {}))))))
  (testing "model kind always :invoke (P6 fallback)"
    (let [model-lease {:cap/id #uuid "20202020-2020-4202-8202-202020202020"
                       :subject {:session/id session-id :phenotype/id phenotype-p1}
                       :resource {:kind :model :id "deepseek/deepseek-v4-flash"}
                       :actions #{:invoke}
                       :constraints {}
                       :issued-at issued-at
                       :expires-at expires-at}
          req {:model/id "deepseek/deepseek-v4-flash"
               :resource {:kind :model :id "deepseek/deepseek-v4-flash" :action :read}
               :messages [{:role :user :content "hi"}]}
          intent (intent/model-call session-id phenotype-p1 :node/planner cause-event-id
                                    {:model/id "deepseek/deepseek-v4-flash"
                                     :messages [{:role :user :content "hi"}]}
                                    budget)]
      ;; even though request carries :read, model kind forces :invoke
      (is (= :allow (:decision (broker/authorize {:intent intent
                                                  :normalized-request req
                                                  :leases [model-lease]
                                                  :usage {}
                                                  :now in-window})))))))
