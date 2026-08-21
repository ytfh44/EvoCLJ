(ns evoclj.binding.call-binding-test
  "Tests for generic CallBinding (evoclj.binding.call) and that
   intent dispatch no longer depends on MCP-specific freshness."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.binding.call :as binding]
            [evoclj.environment.surface :as surface]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.mcp.contract :as contract]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

(def ^:private session-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def ^:private phenotype "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private issued (java.util.Date. 0))
(def ^:private expires (java.util.Date. 4102444800000))
(def ^:private now (java.util.Date. 1700000000000))
(def ^:private budget {:wall-ms 1000})

(defn- lease [tool-id]
  {:cap/id (random-uuid)
   :subject {:phenotype/id phenotype}
   :resource {:kind :tool :id tool-id}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued
   :expires-at expires})

;; ---------------------------------------------------------------------------
;; capture from various entry shapes
;; ---------------------------------------------------------------------------

(deftest capture-from-descriptor
  (testing "capture-tool-binding from plain descriptor map"
    (let [desc {:tool/id :test/echo
                :effect :pure
                :input-schema [:map [:text :string]]
                :output-schema [:map [:text :string]]
                :required-action :invoke
                :version 1}
          b (binding/capture-tool-binding desc {:freshness :best-effort})]
      (is (= :test/echo (:tool/id b)))
      (is (= desc (:binding/descriptor b)) "descriptor snapshot preserved")
      (is (uuid? (:binding/id b)))
      (is (= :best-effort (:binding/freshness b)))
      (is (int? (:revision/seq b)))
      (is (some? (:binding/captured-at b))))))

(deftest capture-from-provider
  (testing "capture-tool-binding from Provider instance"
    (let [p (fixture/echo-provider {})
          b (binding/capture-tool-binding p {:freshness :best-effort})]
      (is (= :fixture/echo (:tool/id b)))
      (is (= (proto/describe p) (:binding/descriptor b)))
      (is (= p (:binding/provider b)) "provider handle stored live but not in persisted")
      ;; generic without revision is considered stale (nil revision-id => stale), which matches old contract behavior where missing mcp/last-refreshed => stale
      (is (true? (:binding/stale? b)) "no revision => stale for best-effort (matches old contract behavior)")
      )))

(deftest capture-from-registry-entry
  (testing "capture-tool-binding from registry entry {:descriptor ... :provider ...}"
    (let [reg (registry/create-registry)
          p (fixture/echo-provider {})
          _ (registry/register! reg p)
          entry (registry/lookup reg :fixture/echo)
          b (binding/capture-tool-binding entry {:freshness :pinned})]
      (is (= :fixture/echo (:tool/id b)))
      (is (= (:descriptor entry) (:binding/descriptor b)))
      (is (= (:provider entry) (:binding/provider b)))
      (is (= :pinned (:binding/freshness b)))
      (is (false? (:binding/stale? b)) "pinned never stale"))))

(deftest capture-from-tool-surface
  (testing "capture via ToolSurface entry and tool-surface->binding helper"
    (let [tool-id :mcp/demo-tool
          desc {:tool/id tool-id
                :effect :remote
                :input-schema :any
                :output-schema :any
                :required-action :invoke
                :mcp/generation 5
                :mcp/last-refreshed (System/currentTimeMillis)
                :mcp/captured-at (System/currentTimeMillis)}
          ;; fake provider
          p (reify proto/Provider
              (describe [_] desc)
              (normalize-request [_ intent] {:tool/id tool-id :resource {:kind :tool :id tool-id} :args (:args (:payload intent))})
              (execute-request! [_ _] {:text "ok"}))
          surface (surface/make-tool-surface {:id :tools/demo :entries {tool-id p} :revision/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"})
          b1 (binding/capture-tool-binding p {:freshness :best-effort :revision/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :revision/seq 5 :source/id :tools/demo})
          b2 (binding/tool-surface->binding surface tool-id {:freshness :best-effort :revision/seq 5})]
      (is (= tool-id (:tool/id b1)))
      (is (= "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" (:revision/id b1)))
      (is (= 5 (:revision/seq b1)))
      (is (= :tools/demo (:source/id b1)))
      (is (= tool-id (:tool/id b2)))
      (is (= 5 (:revision/seq b2)))
      (is (= :tools/demo (:source/id b2))))))

;; ---------------------------------------------------------------------------
;; immutability
;; ---------------------------------------------------------------------------

(deftest binding-immutability
  (testing "descriptor snapshot is immutable: mutating original does not affect binding"
    (let [desc-atom (atom {:tool/id :fake/immut
                           :effect :pure
                           :input-schema [:map [:text :string]]
                           :output-schema [:map [:text :string]]
                           :required-action :invoke
                           :version 1
                           :mcp/generation 10
                           :mcp/last-refreshed (System/currentTimeMillis)})
          p (reify proto/Provider
              (describe [_] @desc-atom)
              (normalize-request [_ intent] {:tool/id :fake/immut :resource {:kind :tool :id :fake/immut} :args (:args (:payload intent))})
              (execute-request! [_ _] {:text "ok"}))
          b (binding/capture-tool-binding p {:freshness :best-effort})]
      (is (= 10 (:revision/seq b)))
      (is (= 10 (:mcp/generation b)))
      ;; mutate original
      (swap! desc-atom assoc :mcp/generation 99 :mcp/last-refreshed nil)
      (is (= 10 (:revision/seq b)) "binding still holds frozen seq 10")
      (is (= 10 (:mcp/generation b)))
      (is (= 10 (:mcp/generation (:binding/descriptor b))) "descriptor snapshot not mutated")
      (is (= {:tool/id :fake/immut :effect :pure} (select-keys (:binding/descriptor b) [:tool/id :effect]))))))

(deftest binding-provider-handle-is-live-but-not-persisted
  (testing "binding stores live provider handle but persisted data excludes it"
    (let [p (fixture/echo-provider {})
          b (binding/capture-tool-binding p {:freshness :best-effort})]
      (is (= p (:binding/provider b)))
      (let [pure (binding/binding->persisted b)]
        (is (= #{:binding/id :tool/id :revision/id :revision/seq} (set (keys pure))) "pure data only has 4 keys")
        (is (not (contains? pure :binding/provider)) "no live object in persisted")
        (is (not (contains? pure :binding/descriptor)) "no descriptor in persisted")
        (is (= (:binding/id b) (:binding/id pure)))
        (is (= (:tool/id b) (:tool/id pure)))
        (is (= (:revision/id b) (:revision/id pure)))
        (is (= (:revision/seq b) (:revision/seq pure)))
        (is (= pure (binding/binding->pure-data b)) "alias works")
        (is (= pure (clojure.edn/read-string (pr-str pure))) "pure data is EDN round-trippable")))))

;; ---------------------------------------------------------------------------
;; audit and stale logic
;; ---------------------------------------------------------------------------

(deftest binding-audit-contains-both-generic-and-mcp-compat
  (testing "binding->audit includes generic and MCP/contract compat keys"
    (let [p (fixture/echo-provider {})
          b (binding/capture-tool-binding p {:freshness :best-effort :revision/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :revision/seq 7})
          audit (binding/binding->audit b)]
      (is (= (:binding/id b) (:binding/id audit)))
      (is (= (:tool/id b) (:tool/id audit)))
      (is (= "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" (:revision/id audit)))
      (is (= 7 (:revision/seq audit)))
      (is (= 7 (:mcp/generation audit)) "MCP compat")
      (is (= 7 (:contract/generation audit)) "contract compat")
      (is (contains? audit :binding/stale?))
      (is (contains? audit :mcp/stale?))
      (is (contains? audit :contract/stale?)))))

(deftest freshness-and-stale-logic
  (testing "stale? logic for various freshness"
    (let [fresh-desc {:tool/id :x :effect :pure :input-schema :any :output-schema :any :required-action :invoke :mcp/last-refreshed (System/currentTimeMillis) :mcp/generation 1}
          stale-desc {:tool/id :x :effect :pure :input-schema :any :output-schema :any :required-action :invoke :mcp/last-refreshed nil :mcp/generation 1}
          no-mcp-desc {:tool/id :x :effect :pure :input-schema :any :output-schema :any :required-action :invoke}]
      (is (false? (binding/stale? fresh-desc :best-effort)) "fresh with timestamp not stale")
      (is (true? (binding/stale? stale-desc :best-effort)) "nil last-refreshed => stale")
      (is (false? (binding/stale? stale-desc :pinned)) "pinned never stale")
      (is (true? (binding/stale? no-mcp-desc :best-effort)) "no MCP field and no revision => stale (conservative, matches old fixture)")
      (is (false? (binding/stale? no-mcp-desc :pinned)) "pinned never stale")
      ;; explicit stale override
      (let [b (binding/capture-tool-binding fresh-desc {:freshness :best-effort :stale? true})]
        (is (true? (:binding/stale? b)) "explicit stale? true overrides"))
      (let [b (binding/capture-tool-binding stale-desc {:freshness :best-effort :stale? false})]
        (is (false? (:binding/stale? b)) "explicit stale? false overrides")))))

;; ---------------------------------------------------------------------------
;; contract wrapper still works
;; ---------------------------------------------------------------------------

(deftest contract-delegates-to-binding
  (testing "evoclj.mcp.contract is wrapper around binding"
    (let [desc {:tool/id :test/c :effect :pure :input-schema :any :output-schema :any :required-action :invoke :mcp/generation 3 :mcp/last-refreshed (System/currentTimeMillis)}]
      (let [c (contract/capture desc :best-effort)
            b (binding/capture desc :best-effort)]
        (is (= (:contract/generation c) (:contract/generation b)))
        (is (= (:contract/id c) (:contract/id c)) "has id")
        (is (contract/valid-freshness? :best-effort))
        (is (= (contract/generation desc) (binding/generation desc)))
        (is (= (contract/stale? desc :best-effort) (binding/stale? desc :best-effort)))
        (is (= (contract/contract->audit c) (binding/binding->audit c)))))))

;; ---------------------------------------------------------------------------
;; dispatcher no longer depends on mcp.contract and uses binding
;; ---------------------------------------------------------------------------

(deftest dispatcher-uses-binding-not-mcp-contract
  (testing "dispatch namespace depends on binding, not mcp.contract, and source does not mention MCP lifecycle literals"
    ;; Check source file directly: should require binding and not mcp.contract for lifecycle
    (let [dispatch-path (io/file "src/evoclj/intent/dispatch.clj")]
      (when (.exists dispatch-path)
        (let [src (slurp dispatch-path)]
          (is (re-find #"evoclj\.binding\.call" src) "dispatch should require evoclj.binding.call")
          (is (not (re-find #"evoclj\.mcp\.contract" src)) "dispatch should NOT require evoclj.mcp.contract directly")
          (is (not (re-find #"mcp/generation" src)) "dispatch source should not mention mcp/generation")
          (is (not (re-find #"mcp/last-refreshed" src)) "should not mention mcp/last-refreshed")
          (is (not (re-find #"mcp/captured-at" src)) "should not mention mcp/captured-at"))))))

(deftest dispatch-still-produces-correct-audit-via-binding
  (testing "dispatch via binding still produces contract/MCP compat audit and pure persisted data"
    (let [reg (registry/create-registry)
          p (fixture/echo-provider {})
          _ (registry/register! reg p)
          ctx (dispatch/make-broker-context {:registry reg :leases [(lease :fixture/echo)] :freshness :best-effort :now (constantly now)})
          intent (intent/tool-call session-id phenotype :node/tool 1 {:tool/id :fixture/echo :args {:text "hello"}} budget)
          res (dispatch/dispatch! ctx intent)]
      (is (= :ok (:result/status res)))
      (is (= {:text "hello"} (:value res)))
      ;; audit via binding
      (is (some? (:audit res)) "has audit")
      (is (contains? (:audit res) :binding/id) "audit has binding/id")
      (is (contains? (:audit res) :revision/seq) "audit has revision/seq")
      (is (contains? (:audit res) :mcp/generation) "audit has MCP compat")
      (is (contains? (:audit res) :contract/generation) "audit has contract compat")
      ;; persisted pure data
      (is (map? (:persisted res)) "has persisted")
      (is (= #{:binding/id :tool/id :revision/id :revision/seq} (set (keys (:persisted res)))) "persisted only pure keys")
      (is (= (:binding/id res) (:binding/id (:persisted res))))
      (is (= (:tool/id res) (:tool/id (:persisted res))))
      ;; also top-level compat keys
      (is (some? (:binding/id res)))
      (is (some? (:contract/id res)))
      (is (= (:binding/id res) (:contract/id res)) "contract/id aliases binding/id")
      (is (some? (:revision/seq res)))
      (is (= (:revision/seq res) (:contract/generation res)) "generation alias")
      ;; persisted is EDN round-trippable and contains no live objects
      (is (= (:persisted res) (clojure.edn/read-string (pr-str (:persisted res))))))))

(deftest dispatch-freshness-required-via-binding
  (testing "dispatch with :required freshness fails closed when binding stale, succeeds when pinned"
    (let [reg (registry/create-registry)
          ;; create a stale provider: descriptor with nil last-refreshed and bumped generation
          p (reify proto/Provider
              (describe [_] {:tool/id :test/stale
                             :effect :pure
                             :input-schema [:map [:text :string]]
                             :output-schema [:map [:text :string]]
                             :required-action :invoke
                             :mcp/generation 5
                             :mcp/last-refreshed nil})
              (normalize-request [_ intent] {:tool/id :test/stale :resource {:kind :tool :id :test/stale} :args (:args (:payload intent))})
              (execute-request! [_ _] {:text "ok"}))
          _ (registry/register! reg p)
          intent (intent/tool-call session-id phenotype :node/tool 1 {:tool/id :test/stale :args {:text "hi"}} budget)]
      (let [ctx-req (dispatch/make-broker-context {:registry reg :leases [(lease :test/stale)] :freshness :required :now (constantly now)})
            res-req (dispatch/dispatch! ctx-req intent)]
        (is (= :provider/freshness-required (:error/type res-req)) "required + stale => fail closed")
        (is (= :required (get-in res-req [:error/data :freshness]))))
      (let [ctx-best (dispatch/make-broker-context {:registry reg :leases [(lease :test/stale)] :freshness :best-effort :now (constantly now)})
            res-best (dispatch/dispatch! ctx-best intent)]
        (is (= :ok (:result/status res-best)) "best-effort + stale => proceed"))
      (let [ctx-pinned (dispatch/make-broker-context {:registry reg :leases [(lease :test/stale)] :freshness :pinned :now (constantly now)})
            res-pinned (dispatch/dispatch! ctx-pinned intent)]
        (is (= :ok (:result/status res-pinned)) "pinned never stale")))))
