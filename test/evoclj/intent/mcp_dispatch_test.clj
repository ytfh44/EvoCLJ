(ns evoclj.intent.mcp-dispatch-test
  "Broker integration tests for MCP providers against the real
   sequential-thinking server (Phase 7 extended)."
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.registry :as registry]))

(defn- available?
  []
  (.exists (io/file "node_modules"
                    "@modelcontextprotocol"
                    "server-sequential-thinking"
                    "dist"
                    "index.js")))

(defn- transport-config
  []
  {:type :stdio
   :command "node"
   :args [(str (io/file "node_modules"
                        "@modelcontextprotocol"
                        "server-sequential-thinking"
                        "dist"
                        "index.js"))]})

(def ^:private session-id #uuid "22222222-2222-4222-8222-222222222222")
(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})
(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000))
(def ^:private now (java.util.Date. 1700000000000))

(defn- lease
  [& kvs]
  (let [base {:cap/id #uuid "33333333-3333-4333-8333-333333333333"
              :subject {:phenotype/id phenotype-p1}
              :resource {:kind :tool :id :mcp/sequential-thinking}
              :actions #{:invoke}
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(defn- mcp-intent
  [args]
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :mcp/sequential-thinking :args args}
                    budget))

(deftest mcp-broker-pipeline-allowed-and-denied
  (testing "full broker pipeline with real MCP server"
    (when (available?)
      (let [provider (mcp-bridge/mcp-provider
                       {:transport-config (transport-config)
                        :tool/id :mcp/sequential-thinking
                        :tool/mcp-name "sequentialthinking"
                        :input-schema [:map
                                       [:thought :string]
                                       [:thoughtNumber :int]
                                       [:totalThoughts :int]
                                       [:nextThoughtNeeded :boolean]]
                        :output-schema [:map
                                        [:value :any]]
                        :retry-safe? true})
            reg (registry/create-registry)]
        (registry/register! reg provider)
        (let [lease (lease)
              ctx (dispatch/make-broker-context
                    {:registry reg
                     :leases [lease]
                     :usage (atom {})
                     :now (constantly now)})]
          (testing "allowed request executes through MCP provider"
              (let [r (dispatch/dispatch! ctx
                                        (assoc-in (mcp-intent {:thought "test"
                                                               :thoughtNumber 1
                                                               :totalThoughts 1
                                                               :nextThoughtNeeded false})
                                                  [:metadata :idempotency/key] "req-1"))]
              (is (= :ok (:result/status r)) (:error r))
              (is (= {:decision :allow
                      :lease-id (:cap/id lease)} (:authorization r)))
              (is (map? (:value r)))
              (let [raw (get-in r [:value :value])]
                (is (string? raw))
                (let [parsed (cheshire.core/parse-string raw true)]
                  (is (= 1 (:thoughtNumber parsed)))
                  (is (= 1 (:totalThoughts parsed)))
                  (is (false? (:nextThoughtNeeded parsed)))))))
          (testing "denied request never reaches the provider"
            (let [ctx-no-lease (dispatch/make-broker-context
                                 {:registry reg
                                  :leases []
                                  :usage (atom {})
                                  :now (constantly now)})
                  r (dispatch/dispatch! ctx-no-lease
                                        (assoc-in (mcp-intent {:thought "test"
                                                               :thoughtNumber 1
                                                               :totalThoughts 1
                                                               :nextThoughtNeeded false})
                                                  [:metadata :idempotency/key] "req-2"))]
              (is (= :error (:result/status r)))
              (is (= :capability/denied (:error/type r))))))))))
