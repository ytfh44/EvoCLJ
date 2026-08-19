(ns evoclj.mcp.integration-test
  "Real-world integration tests against the official MCP sequential-thinking
   server (Phase 7).

   These tests require Node.js and the @modelcontextprotocol/server-sequential-thinking
   package to be installed locally. They are skipped automatically when the
   server binary is not available."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.kernel.error :as err]))

(defn- sequential-thinking-available?
  "True when the sequential-thinking server bundle is installed."
  []
  (let [server-js (io/file "node_modules"
                           "@modelcontextprotocol"
                           "server-sequential-thinking"
                           "dist"
                           "index.js")]
    (.exists server-js)))

(defn- integration-transport-config
  "Build the stdio transport config for the sequential-thinking server."
  []
  {:type :stdio
   :command "node"
   :args [(str (io/file "node_modules"
                        "@modelcontextprotocol"
                        "server-sequential-thinking"
                        "dist"
                        "index.js"))]})

(deftest sequential-thinking-list-tools
  (testing "the real sequential-thinking server exposes sequentialthinking"
    (when (sequential-thinking-available?)
      (mcp-client/with-client (integration-transport-config)
        (fn [managed]
          (let [tools (:tools (mcp-client/list-tools (:client managed)))]
            (is (pos? (count tools)) "server should return at least one tool")
            (is (some #(= "sequentialthinking" (:mcp/name %)) tools)
                "server should expose sequentialthinking tool")))))))

(deftest sequential-thinking-call-tool
  "More tests here..."
  (when (sequential-thinking-available?)
    (mcp-client/with-client (integration-transport-config)
      (fn [managed]
        (let [result (mcp-client/call-tool-managed managed "sequentialthinking"
                                                    {:thought "integration test"
                                                     :thoughtNumber 1
                                                     :totalThoughts 1
                                                     :nextThoughtNeeded false})]
          (is (false? (:mcp/is-error result)))
          (let [content (:mcp/content result)]
            (is (pos? (count content)))))))))
