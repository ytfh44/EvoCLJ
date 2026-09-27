(ns evoclj.capability.effects-test
  "PLT5 tests for the Effects ⊆ Requested ⊆ Granted capability lattice."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.core :as capability]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.node.types :as node-types]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]))

(def ^:private code-id
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private session-id
  #uuid "11111111-1111-4111-8111-111111111111")

(def ^:private issued-at
  (java.util.Date. 1700000000000))

(def ^:private expires-at
  (java.util.Date. 1700003600000))

(defn- lease
  [resource actions]
  {:cap/id (random-uuid)
   :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
   :resource resource
   :actions actions
   :constraints {}
   :issued-at issued-at
   :expires-at expires-at})

(deftest topology-effects-are-derived-from-node-kinds
  (testing "static Effects contain categories, not resource grants"
    (is (= #{:model/call :tool/call :memory/read :memory/write}
           (capability/topology-effects
            {:nodes {:model {:node/type :llm
                             :tools [{:name "echo"}]}
                     :tool {:node/type :tool}
                     :read {:node/type :memory/read}
                     :write {:node/type :memory/write}}}))))
  (testing "pure nodes do not require an external effect"
    (is (= #{}
           (capability/topology-effects
            {:nodes {:sci {:node/type :sci}
                     :emit {:node/type :emit}
                     :loop {:node/type :loop}}}))))
  (testing "the dispatch is exhaustive over the node vocabulary"
    (is (= #{:tool/call :model/call :memory/read :memory/write}
           (into #{} (mapcat (fn [t] (capability/node-effects {:node/type t})))
                 [:tool :llm :memory/read :memory/write])))
    (is (every? (fn [t] (set? (capability/node-effects {:node/type t})))
                [:sci :emit :loop])))
  (testing "an unknown node type fails closed"
    (let [e (try (capability/node-effects {:node/type :node/bogus})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :capability/unknown-node-type (:error/type (ex-data e))))
      (is (= :node/bogus (:node/type (ex-data e)))))
    (let [e (try (capability/node-effects {:node/type nil})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :capability/unknown-node-type (:error/type (ex-data e))))))
  (testing "the vocabulary has one definition"
    (is (= node-types/node-types
           (set (keys node-types/required-keys)))
        "every node type declares its required keys")
    (is (every? (set node-types/attribute-keys)
                [:model :program :tool :memory :next :exit :body :until]))))


(deftest granted-effects-come-from-lease-resource-and-action
  (let [grants [(lease {:kind :model :id "provider/model"} #{:invoke})
                (lease {:kind :tool :id :fixture/echo} #{:invoke})
               (lease {:kind :memory :id :session} #{:read})
               (lease {:kind :memory :id :session} #{:write})
                (lease {:kind :filesystem :path "/tmp"} #{:read})]]
    (is (= #{:model/call :tool/call :memory/read :memory/write :filesystem/read}
           (capability/granted-effects grants)))))

(deftest capability-lattice-enforces-both-inclusions
  (testing "a complete lattice is accepted"
    (is (= {:effects #{:tool/call}
            :requested #{:tool/call}
            :granted #{:tool/call}}
           (capability/validate-effect-lattice!
            #{:tool/call} #{:tool/call} #{:tool/call}))))
  (testing "Effects must be declared in Requested"
    (let [e (try (capability/validate-effect-lattice!
                  #{:tool/call} #{} #{:tool/call})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :capability/lattice-invalid (:error/type (ex-data e))))
      (is (= :effect-not-requested (:reason (ex-data e))))
      (is (= [:tool/call] (:missing (ex-data e))))))
  (testing "Requested must be present in Granted"
    (let [e (try (capability/validate-effect-lattice!
                  #{} #{:tool/call} #{})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :capability/lattice-invalid (:error/type (ex-data e))))
      (is (= :requested-not-granted (:reason (ex-data e))))
      (is (= [:tool/call] (:missing (ex-data e)))))))

(deftest dispatcher-rejects-an-effect-outside-requested
  (let [reg (registry/create-registry)
        executions (atom 0)
        _ (registry/register! reg (fixture/echo-provider
                                   {:execution-count executions}))
        broker (dispatch/make-broker-context
                {:registry reg
                 :leases [(lease {:kind :model :id "provider/model"} #{:invoke})]
                 :effects #{:model/call}
                 :requested-capabilities #{:model/call}})
        tool-intent (intent/tool-call
                     session-id code-id :node/tool 7
                     {:tool/id :fixture/echo :args {:text "blocked"}}
                     {:wall-ms 1000})
        result (dispatch/dispatch! broker tool-intent)]
    (is (= :error (:result/status result)))
    (is (= :capability/denied (:error/type result)))
    (is (= :capability/not-requested (get-in result [:error/data :reason])))
    (is (= 0 @executions) "an undeclared effect never reaches a provider")))
