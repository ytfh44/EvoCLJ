(ns evoclj.capability.revoke-generalization-test
  "P5 — revoke generalization to all kinds (tool/model/memory/filesystem).
  Covers Wolfram [W-12..W-13] fail-closed after revoke, idempotent revoke,
  and non-revoked still allows. Uses the unified LeaseRegistry helpers from
  capability/mint (delegated by mount/filesystem and capability/lease)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.mint :as mint]
            [evoclj.intent.core :as intent]
            [evoclj.mount.filesystem :as fs])
  (:import (java.util Date UUID)))

(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private session-a #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")

(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 1700003600000))
(def ^:private in-window (Date. 1700001800000))

(def ^:private subject-a {:principal/type :session :session/id session-a})

(defn- throws-type? [thunk expected-type]
  (try (thunk) false
       (catch clojure.lang.ExceptionInfo e
         (= expected-type (:error/type (ex-data e))))))

(defn- tool-normalized [tool-id]
  {:tool/id tool-id
   :resource {:kind :tool :id tool-id}
   :args {:text "hi"}})

(defn- tool-intent [session-id phenotype-id tool-id]
  (intent/tool-call session-id phenotype-id :node/test 1 {:tool/id tool-id :args {:text "hi"}} {:wall-ms 1000}))

(defn- model-normalized [model-id]
  {:tool/id :model/call
   :resource {:kind :model :id model-id}
   :model/id model-id
   :messages [{:role :user :content "hi"}]})

(defn- model-intent [session-id phenotype-id model-id]
  (intent/model-call session-id phenotype-id :node/test 1 {:model/id model-id :messages [{:role :user :content "hi"}]} {:wall-ms 1000}))

(defn- fs-intent [session-id phenotype-id mount-id path]
  (intent/tool-call session-a phenotype-p1 :node/test 1 {:tool/id :filesystem/generic :args {:mount/id mount-id :path path :operation :read}} {:wall-ms 1000}))

;; ---------------------------------------------------------------------------
;; 1. tool lease revoked -> broker deny with :capability/revoked
;; ---------------------------------------------------------------------------

(deftest tool-lease-revoked-broker-deny
  (testing "tool lease revoked -> broker deny with :capability/revoked (P5)"
    (let [registry (mint/create-lease-registry)
          lease (mint/mint-lease! registry
                                  {:subject subject-a
                                   :resource {:kind :tool :id :fixture/echo}
                                   :actions #{:invoke}
                                   :issued-at issued-at
                                   :expires-at expires-at})]
      (let [d (broker/authorize {:intent (tool-intent session-a phenotype-p1 :fixture/echo)
                                 :normalized-request (tool-normalized :fixture/echo)
                                 :leases [lease]
                                 :usage {}
                                 :now in-window
                                 :lease-registry registry})]
        (is (= :allow (:decision d)) "non-revoked tool lease should allow"))
      (mint/revoke-lease! registry (:cap/id lease))
      (is (mint/revoked? registry (:cap/id lease)))
      (is (mint/lease-revoked? registry (:cap/id lease)))
      (let [d2 (broker/authorize {:intent (tool-intent session-a phenotype-p1 :fixture/echo)
                                  :normalized-request (tool-normalized :fixture/echo)
                                  :leases [lease]
                                  :usage {}
                                  :now in-window
                                  :lease-registry registry})]
        (is (= :deny (:decision d2)))
        (is (= :capability/revoked (:reason d2)))))))

;; ---------------------------------------------------------------------------
;; 2. filesystem lease revoked -> deny (broker + verify-fs-lease!)
;; ---------------------------------------------------------------------------

(deftest filesystem-lease-revoked-deny
  (testing "filesystem lease revoked -> deny (broker and fs verify) (P5)"
    (let [registry (fs/create-lease-registry)
          mount-id [:workspace "test-ws"]
          fs-lease (fs/issue-fs-lease registry
                                      {:subject subject-a
                                       :mount-id mount-id
                                       :path ""
                                       :actions #{:read :list}
                                       :issued-at issued-at
                                       :expires-at expires-at})
          tool-lease (mint/mint-lease! registry {:subject subject-a :resource {:kind :tool :id :filesystem/generic} :actions #{:invoke} :issued-at issued-at :expires-at expires-at})]
      (is (= fs-lease (fs/get-lease registry (:cap/id fs-lease))))
      (is (= fs-lease (mint/get-lease registry (:cap/id fs-lease))))
      (let [d (broker/authorize {:intent (fs-intent session-a phenotype-p1 mount-id "")
                                 :normalized-request {:tool/id :filesystem/generic :resource {:kind :filesystem/path :mount/id mount-id :path "" :action :read}}
                                 :leases [tool-lease fs-lease]
                                 :usage {}
                                 :now in-window
                                 :lease-registry registry})]
        (is (= :allow (:decision d)) "non-revoked fs lease should allow via broker (both tool + fs)"))
      (is (identical? fs-lease (fs/verify-fs-lease! fs-lease {:now in-window :subject subject-a :registry registry})))
      (fs/revoke-lease! registry (:cap/id fs-lease))
      (is (fs/lease-revoked? registry (:cap/id fs-lease)))
      (is (mint/revoked? registry (:cap/id fs-lease)))
      (let [d2 (broker/authorize {:intent (fs-intent session-a phenotype-p1 mount-id "")
                                  :normalized-request {:tool/id :filesystem/generic :resource {:kind :filesystem/path :mount/id mount-id :path "" :action :read}}
                                  :leases [tool-lease fs-lease]
                                  :usage {}
                                  :now in-window
                                  :lease-registry registry})]
        (is (= :deny (:decision d2)))
        (is (= :capability/revoked (:reason d2))))
      (is (throws-type? #(fs/verify-fs-lease! fs-lease {:now in-window :subject subject-a :registry registry})
                        :capability/revoked)))))

(deftest model-lease-revoked-deny
  (testing "model lease revoked -> broker deny with :capability/revoked (P5)"
    (let [registry (mint/create-lease-registry)
          model-id "deepseek/deepseek-v4-flash"
          lease (mint/mint-lease! registry
                                  {:subject subject-a
                                   :resource {:kind :model :id model-id}
                                   :actions #{:invoke}
                                   :issued-at issued-at
                                   :expires-at expires-at})]
      (let [d (broker/authorize {:intent (model-intent session-a phenotype-p1 model-id)
                                 :normalized-request (model-normalized model-id)
                                 :leases [lease]
                                 :usage {}
                                 :now in-window
                                 :lease-registry registry})]
        (is (= :allow (:decision d)) "non-revoked model lease should allow"))
      (mint/revoke-lease! registry (:cap/id lease))
      (is (true? (mint/revoked? registry (:cap/id lease))))
      (let [d2 (broker/authorize {:intent (model-intent session-a phenotype-p1 model-id)
                                  :normalized-request (model-normalized model-id)
                                  :leases [lease]
                                  :usage {}
                                  :now in-window
                                  :lease-registry registry})]
        (is (= :deny (:decision d2)))
        (is (= :capability/revoked (:reason d2)))))))

(deftest revoke-idempotent
  (testing "revoke is idempotent (double revoke same result) (P5)"
    (let [registry (mint/create-lease-registry)
          lease (mint/mint-lease! registry
                                  {:subject subject-a
                                   :resource {:kind :tool :id :fixture/echo}
                                   :actions #{:invoke}
                                   :issued-at issued-at
                                   :expires-at expires-at})
          cap-id (:cap/id lease)]
      (mint/revoke-lease! registry cap-id)
      (is (mint/revoked? registry cap-id))
      (let [snap1 @registry
            d1 (broker/authorize {:intent (tool-intent session-a phenotype-p1 :fixture/echo)
                                  :normalized-request (tool-normalized :fixture/echo)
                                  :leases [lease]
                                  :usage {}
                                  :now in-window
                                  :lease-registry registry})]
        (mint/revoke-lease! registry cap-id)
        (is (mint/revoked? registry cap-id) "still revoked after second revoke")
        (let [snap2 @registry
              d2 (broker/authorize {:intent (tool-intent session-a phenotype-p1 :fixture/echo)
                                    :normalized-request (tool-normalized :fixture/echo)
                                    :leases [lease]
                                    :usage {}
                                    :now in-window
                                    :lease-registry registry})]
          (is (= snap1 snap2) "registry unchanged after second revoke (idempotent)")
          (is (= d1 d2) "deny result identical after double revoke")
          (is (= :capability/revoked (:reason d2)))))
      (let [fs-reg (fs/create-lease-registry)
            mnt [:workspace "idem-ws"]
            fs-lease (fs/issue-fs-lease fs-reg {:subject subject-a :mount-id mnt :path "a/b" :actions #{:read} :issued-at issued-at :expires-at expires-at})
            fid (:cap/id fs-lease)]
        (fs/revoke-lease! fs-reg fid)
        (fs/revoke-lease! fs-reg fid)
        (is (fs/lease-revoked? fs-reg fid) "fs double revoke still revoked")))))

(deftest non-revoked-still-allows
  (testing "non-revoked leases still allow (positive control) (P5)"
    (let [tool-reg (mint/create-lease-registry)
          fs-reg (fs/create-lease-registry)
          model-reg (mint/create-lease-registry)
          tool-lease (mint/mint-lease! tool-reg {:subject subject-a :resource {:kind :tool :id :fixture/echo} :actions #{:invoke} :issued-at issued-at :expires-at expires-at})
          mount-id [:workspace "allow-ws"]
          fs-tool-lease (mint/mint-lease! fs-reg {:subject subject-a :resource {:kind :tool :id :filesystem/generic} :actions #{:invoke} :issued-at issued-at :expires-at expires-at})
          fs-lease (fs/issue-fs-lease fs-reg {:subject subject-a :mount-id mount-id :path "" :actions #{:read} :issued-at issued-at :expires-at expires-at})
          model-id "deepseek/deepseek-v4-flash"
          model-lease (mint/mint-lease! model-reg {:subject subject-a :resource {:kind :model :id model-id} :actions #{:invoke} :issued-at issued-at :expires-at expires-at})
          mem-lease (mint/mint-lease! tool-reg {:subject subject-a :resource {:kind :memory :id :mem/key} :actions #{:invoke} :issued-at issued-at :expires-at expires-at})]
      (is (= :allow (:decision (broker/authorize {:intent (tool-intent session-a phenotype-p1 :fixture/echo)
                                                  :normalized-request (tool-normalized :fixture/echo)
                                                  :leases [tool-lease]
                                                  :usage {}
                                                  :now in-window
                                                  :lease-registry tool-reg}))))
      (is (= :allow (:decision (broker/authorize {:intent (fs-intent session-a phenotype-p1 mount-id "")
                                                  :normalized-request {:tool/id :filesystem/generic :resource {:kind :filesystem/path :mount/id mount-id :path "" :action :read}}
                                                  :leases [fs-tool-lease fs-lease]
                                                  :usage {}
                                                  :now in-window
                                                  :lease-registry fs-reg}))))
      (is (= :allow (:decision (broker/authorize {:intent (model-intent session-a phenotype-p1 model-id)
                                                  :normalized-request (model-normalized model-id)
                                                  :leases [model-lease]
                                                  :usage {}
                                                  :now in-window
                                                  :lease-registry model-reg}))))
      (let [mem-intent (intent/memory-read session-a phenotype-p1 :node/test 1 {:memory/key :mem/key} {:wall-ms 1000})
            mem-norm {:resource {:kind :memory :id :mem/key}}]
        (is (= :allow (:decision (broker/authorize {:intent mem-intent
                                                    :normalized-request mem-norm
                                                    :leases [mem-lease]
                                                    :usage {}
                                                    :now in-window
                                                    :lease-registry tool-reg}))))))))
