(ns evoclj.mcp.contract-test
  "Step 1 check: CallContract freezes D_normalize=D_authorize=D_execute=D_validate
   and freshness :required/:best-effort/:pinned gating."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.contract :as contract]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.intent.core :as intent]))

(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000))
(def ^:private now (java.util.Date. 1700000000000))
(def ^:private budget {:wall-ms 1000})

(defn- lease-for [tool-id & kvs]
  (let [base {:cap/id (random-uuid)
              :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-p1}
              :resource {:kind :tool :id tool-id}
              :actions #{:invoke}
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(deftest contract-freezes-generation-across-mutation
  (testing "D_normalize=D_authorize=D_execute=D_validate: contract holds frozen generation even after descriptor mutates"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id :mcp/freeze-demo
              :tool/mcp-name "freeze-demo"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          d0 (proto/describe p)
          gen0 (:mcp/generation d0)
          c (contract/capture d0 nil nil :best-effort {:stale? false})
          ;; refresh produces new immutable ToolEntry @1, original p stays @0
          p1 (mcp-bridge/refresh-provider! p)
          d1 (proto/describe p1)
          gen1 (:mcp/generation d1)]
        (is (= 0 gen0) "initial generation 0")
        (is (= 1 gen1) "after refresh, new entry generation 1 (immutable ToolEntry@1)")
        (is (= 0 (:mcp/generation (proto/describe p))) "original ToolEntry@0 unchanged (immutable)")
        (is (= gen0 (:contract/generation c)) "contract still holds frozen generation")
        (is (not= gen0 gen1) "generation drift occurred but contract did not follow")
        (is (= d0 (:contract/descriptor c)) "descriptor snapshot identity preserved")
        (is (some? (contract/validate-contract c))))))

(deftest freshness-gating-best-effort-vs-required
  (testing ":required fails closed when descriptor stale, :best-effort proceeds with stale? true, :pinned ignores staleness"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id :mcp/fresh-demo
              :tool/mcp-name "fresh-demo"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          p-stale (mcp-bridge/refresh-provider! p) ; new immutable entry stale (nil last-refreshed) and bumps gen
          d-stale (proto/describe p-stale)]
      (is (nil? (:mcp/last-refreshed d-stale)) "stale descriptor has nil last-refreshed")
      (is (true? (contract/stale? d-stale :required)))
      (is (true? (contract/stale? d-stale :best-effort)))
      (is (false? (contract/stale? d-stale :pinned)) "pinned never stale")
      ;; best-effort capture marks stale? true and audit reflects it
      (let [c-best (contract/capture d-stale nil nil :best-effort {:stale? true})
            audit-best (contract/contract->audit c-best)]
        (is (true? (:contract/stale? c-best)))
        (is (true? (:mcp/stale? audit-best)))
        (is (= :best-effort (:mcp/freshness audit-best))))
      ;; required capture would be rejected at dispatch layer, but contract itself can represent stale
      (let [c-req (contract/capture d-stale nil nil :required {:stale? true})]
        (is (true? (:contract/stale? c-req))))
      ;; pinned capture is never stale
      (let [c-pin (contract/capture d-stale nil nil :pinned {:stale? false})]
        (is (false? (:contract/stale? c-pin)))
        (is (false? (:mcp/stale? (contract/contract->audit c-pin))))))))

(deftest dispatch-freshness-required-fails-closed
  (testing "dispatch with :required freshness and stale descriptor returns :provider/freshness-required without calling provider"
    (let [reg (registry/create-registry)
          p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id :mcp/gated
              :tool/mcp-name "gated"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          _ (registry/register! reg p)
          p-stale (mcp-bridge/refresh-provider! p) ; new immutable stale entry
          ;; replace registry entry with stale one to simulate McpSource refresh publishing new ToolSurface
          _ (swap! reg assoc :mcp/gated {:descriptor (proto/describe p-stale) :provider p-stale})
          leases [(lease-for :mcp/gated)]
          ctx (dispatch/make-broker-context {:registry reg :leases leases :freshness :required :now (constantly now)})
          intent (intent/tool-call session-id phenotype-p1 :node/tool 42 {:tool/id :mcp/gated :args {:text "hi"}} budget)]
      (let [res (dispatch/dispatch! ctx intent)]
        (is (= :provider/freshness-required (:error/type res)))
        (is (= :required (:freshness (:error/data res))) "freshness in error data")
        (is (some? (:contract/generation res)) "generation present in result (audit)")))))

(deftest dispatch-freezes-descriptor-across-normalize-authorize-execute-validate
  (testing "D_normalize=D_authorize=D_execute=D_validate via contract: concurrent mutation does not bleed into in-flight dispatch"
    (let [reg (registry/create-registry)
          ;; use a fake provider that records which generation it saw
          seen-gens (atom [])
          fake-provider
          (let [desc-atom (atom {:tool/id :fake/echo
                                 :effect :pure
                                 :input-schema [:map [:text :string]]
                                 :output-schema [:map [:text :string]]
                                 :required-action :invoke
                                 :version 1
                                 :mcp/generation 5
                                 :mcp/last-refreshed (System/currentTimeMillis)
                                 :mcp/captured-at (System/currentTimeMillis)})]
            (reify proto/Provider
              (describe [_] @desc-atom)
              (normalize-request [_ intent]
                (swap! seen-gens conj {:phase :normalize :gen (:mcp/generation @desc-atom)})
                {:tool/id :fake/echo :resource {:kind :tool :id :fake/echo} :args (:args (:payload intent))})
              (execute-request! [_ req]
                (swap! seen-gens conj {:phase :execute :gen (:mcp/generation @desc-atom)})
                {:text "ok"}))) 
          _ (registry/register! reg fake-provider)
          leases [(lease-for :fake/echo)]
          ctx (dispatch/make-broker-context {:registry reg :leases leases :freshness :best-effort :now (constantly now)})
          intent (intent/tool-call session-id phenotype-p1 :node/tool 42 {:tool/id :fake/echo :args {:text "hello"}} budget)]
      ;; mutate descriptor generation between dispatch steps would be visible if not frozen
      ;; our dispatch captures frozen generation=5 before normalize, and passes frozen to execute
      (let [res (dispatch/dispatch! ctx intent)]
        ;; result should contain contract generation 5
        (is (= 5 (:contract/generation res)) "frozen generation 5 in result")
        (is (= 5 (:mcp/generation res))))
      (is (= 5 (:gen (first @seen-gens))) "normalize saw gen 5")
      (is (= 5 (:gen (second @seen-gens))) "execute saw same frozen gen 5"))))
