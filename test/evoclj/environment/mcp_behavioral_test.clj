(ns evoclj.environment.mcp-behavioral-test
  "regression of current correct MCP behavior as first batch.
  Covers: in-flight frozen descriptor, removed tool rejects new calls,
  newly discovered tool does not auto-grant, filesystem second auth,
  failed refresh does not split call.
  Written against real provider/dispatch/broker; must stay green after refactors."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.mcp.contract :as contract]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

(def ^:private phenotype "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private sid #uuid "22222222-2222-4222-8222-222222222222")
(def ^:private now-val (java.util.Date. 1700000000000))
(def ^:private budget {:wall-ms 1000})
(def ^:private issued (java.util.Date. 0))
(def ^:private expires (java.util.Date. 4102444800000))

(defn- lease [tool-id]
  {:cap/id (random-uuid) :subject {:phenotype/id phenotype}
   :resource {:kind :tool :id tool-id}
   :actions #{:invoke} :constraints {:max-calls 100}
   :issued-at issued :expires-at expires})

;; ---- 1. in-flight call still uses frozen descriptor across refresh ----
(deftest mcp-in-flight-frozen-descriptor
  (testing "in-flight contract keeps frozen generation after concurrent refresh; later call sees new generation"
    (let [reg (registry/create-registry)
          p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id :mcp/inflight
              :tool/mcp-name "inflight"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          _ (registry/register! reg p)
          d0 (proto/describe p)
          c0 (contract/capture d0 nil nil :best-effort {:stale? false})
          gen0 (:contract/generation c0)]
      (mcp-bridge/refresh-provider! p)
      (let [d1 (proto/describe p)
            gen1 (:mcp/generation d1)]
        (is (not= gen0 gen1) "refresh bumped generation")
        (is (= gen0 (:contract/generation c0)))
        (is (= d0 (:contract/descriptor c0)))
        (let [c1 (contract/capture d1 nil nil :best-effort {:stale? false})]
          (is (= gen1 (:contract/generation c1))))))))

;; ---- 2. removed tool no longer accepts new calls ----
(deftest mcp-removed-tool-rejects-new-calls
  (testing "tool removed from catalog is not callable for new dispatch"
    (let [reg (registry/create-registry)
          p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id :mcp/removable
              :tool/mcp-name "removable"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          _ (registry/register! reg p)
          leases [(lease :mcp/removable)]
          ctx (dispatch/make-broker-context {:registry reg :leases leases :freshness :required :now (constantly now-val)})
          intent-ok (intent/tool-call sid phenotype :node/tool 1 {:tool/id :mcp/removable :args {:text "hi"}} budget)]
      ;; simulate catalog disappearance
      (swap! reg dissoc :mcp/removable)
      (let [res (dispatch/dispatch! ctx intent-ok)]
        (is (= :provider/not-found (:error/type res)) "removed tool not found for new call")))))

;; ---- 3. newly discovered tool does not auto-grant ----
(deftest mcp-newly-discovered-does-not-auto-grant
  (testing "newly catalogued tool without lease is denied by broker"
    (let [reg (registry/create-registry)
          p-new (mcp-bridge/mcp-provider
                 {:transport-config {:type :stdio :command "echo" :args []}
                  :tool/id :mcp/newly
                  :tool/mcp-name "newly"
                  :input-schema [:map [:text :string]]
                  :output-schema [:map [:text :string]]})
          _ (registry/register! reg p-new)
          ctx (dispatch/make-broker-context {:registry reg :leases [] :freshness :best-effort :now (constantly now-val)})
          intent-new (intent/tool-call sid phenotype :node/tool 2 {:tool/id :mcp/newly :args {:text "hello"}} budget)
          res (dispatch/dispatch! ctx intent-new)]
      (is (= :capability/denied (:error/type res)) "no lease means denied")
      (is (= :deny (get-in res [:authorization :decision]))))))

;; ---- 4. filesystem/path second authorization still holds ----
(deftest mcp-filesystem-second-authorization-holds
  (testing "even when tool is visible, filesystem/path resource still requires its own lease"
    (let [reg (registry/create-registry)
          fs-provider
          (reify proto/Provider
            (describe [_] {:tool/id :fs/read
                           :effect :pure
                           :input-schema [:map [:path :string]]
                           :output-schema [:map [:content :string]]
                           :required-action :read
                           :version 1})
            (normalize-request [_ intent]
              (let [p (get-in intent [:payload :args :path])]
                {:tool/id :fs/read :resource {:kind :filesystem/path :path p} :args {:path p}}))
            (execute-request! [_ req] {:content (str "content:" (:path (:resource req)))}))
          _ (registry/register! reg fs-provider)
          tool-lease {:cap/id (random-uuid) :subject {:phenotype/id phenotype}
                      :resource {:kind :tool :id :fs/read}
                      :actions #{:invoke} :constraints {:max-calls 10}
                      :issued-at issued :expires-at expires}
          ctx-tool-only (dispatch/make-broker-context {:registry reg :leases [tool-lease] :now (constantly now-val)})
          intent-read (intent/tool-call sid phenotype :node/tool 3 {:tool/id :fs/read :args {:path "/workspace/secret/foo.md"}} budget)
          res-denied (dispatch/dispatch! ctx-tool-only intent-read)]
      (is (= :capability/denied (:error/type res-denied)) "tool lease alone not enough for path")
      (let [path-lease {:cap/id (random-uuid) :subject {:phenotype/id phenotype}
                        :resource {:kind :filesystem/path :path "/workspace"}
                        :actions #{:invoke} :constraints {:max-calls 10}
                        :issued-at issued :expires-at expires}
            ctx-both (dispatch/make-broker-context {:registry reg :leases [tool-lease path-lease] :now (constantly now-val)})
            res-ok (dispatch/dispatch! ctx-both intent-read)]
        (when (not= :ok (:result/status res-ok))
          (println "DEBUG res-ok:" (pr-str res-ok)))
        (is (= :ok (:result/status res-ok)) "both leases allow call")))))

;; ---- 5. failed refresh does not produce half-old half-new call ----
(deftest mcp-failed-refresh-does-not-split-call
  (testing "call started before failed refresh keeps single frozen descriptor across normalize/authorize/execute/validate"
    (let [reg (registry/create-registry)
          seen (atom [])
          fake
          (let [desc-atom (atom {:tool/id :fake/split
                                 :effect :pure
                                 :input-schema [:map [:text :string]]
                                 :output-schema [:map [:text :string]]
                                 :required-action :invoke
                                 :version 1
                                 :mcp/generation 7
                                 :mcp/last-refreshed (System/currentTimeMillis)})]
            (reify proto/Provider
              (describe [_] @desc-atom)
              (normalize-request [_ intent]
                (swap! seen conj {:phase :normalize :gen (:mcp/generation @desc-atom)})
                {:tool/id :fake/split :resource {:kind :tool :id :fake/split} :args (:args (:payload intent))})
              (execute-request! [_ _]
                (swap! seen conj {:phase :execute :gen (:mcp/generation @desc-atom)})
                {:text "ok"})))
          _ (registry/register! reg fake)
          leases [(lease :fake/split)]
          ctx (dispatch/make-broker-context {:registry reg :leases leases :freshness :best-effort :now (constantly now-val)})
          intent-v (intent/tool-call sid phenotype :node/tool 4 {:tool/id :fake/split :args {:text "hi"}} budget)
          d0 (proto/describe fake)
          c0 (contract/capture d0 nil nil :best-effort)]
      ;; failed refresh must not bump generation
      (is (= 7 (:contract/generation c0)))
      (let [res (dispatch/dispatch! ctx intent-v)]
        (is (= :ok (:result/status res)))
        (is (= 7 (:contract/generation res)) "frozen generation remains 7")
        (is (= 7 (:mcp/generation res)) "mcp generation still frozen")
        (is (= #{7} (set (map :gen @seen))) "normalize and execute saw same generation")))))
