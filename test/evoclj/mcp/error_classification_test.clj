(ns evoclj.mcp.error-classification-test
  "Behavioral tests for M7 — MCP error classification v2.

  Asserts the production classifier (evoclj.mcp.client/classify-mcp-error)
  assigns INDEPENDENT, TYPED categories:
    - timeouts (java.util.concurrent.TimeoutException /
      java.net.SocketTimeoutException, and the MCP JSON-RPC request-timeout
      code -32001) become :mcp/timeout, never folded into :mcp/transport-error;
    - an MCP-originated JSON-RPC error (io.modelcontextprotocol.spec.McpError)
      carries the numeric :mcp/error-code and a typed category derived from it;
    - generic transport / protocol failures keep their own families;
    - dead branches (cause-chain :error/type scans) are gone and the old
      mis-classifications no longer happen.

  These tests feed REAL SDK throwables and real EvoCLJ ExceptionInfo values
  through classify-mcp-error; they do not inject functions to bypass any
  production component (INV-09)."
  (:require [clojure.test :refer [deftest testing is]]
            [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as mcp-client])
  (:import [io.modelcontextprotocol.spec McpError
            McpSchema$JSONRPCResponse$JSONRPCError]))

;; --- helpers ----------------------------------------------------------------

(defn- json-rpc-error
  "Build a REAL MCP SDK JSON-RPC error response object (not a fake map)."
  [code message]
  (McpSchema$JSONRPCResponse$JSONRPCError. (int code) message))

(defn- mcp-error
  "Build a REAL io.modelcontextprotocol.spec.McpError carrying `code`."
  [code message]
  (McpError. (json-rpc-error code message)))

(defn- classify-type
  [^Throwable ex]
  (:error/type (mcp-client/classify-mcp-error ex)))

(defn- classify-code
  [^Throwable ex]
  (:mcp/error-code (mcp-client/classify-mcp-error ex)))

;; --- 1. happy path: generic transport ---------------------------------------

(deftest generic-transport-classified-as-transport-error
  (testing "a bare IOException is the transport family, not unknown/timeout"
    (let [t (java.io.IOException. "connection refused")]
      (is (= :mcp/transport-error (classify-type t)))
      (is (nil? (classify-code t))))))

;; --- 2. new branch: timeout is independent ----------------------------------

(deftest timeout-is-independent-category
  (testing "java.util.concurrent.TimeoutException is :mcp/timeout, NOT transport"
    (let [t (java.util.concurrent.TimeoutException. "request timed out")]
      (is (= :mcp/timeout (classify-type t)))
      (is (not= :mcp/transport-error (classify-type t)))
      (is (nil? (classify-code t)))))
  (testing "java.net.SocketTimeoutException is also :mcp/timeout"
    (let [t (java.net.SocketTimeoutException. "read timed out")]
      (is (= :mcp/timeout (classify-type t))))))

;; --- 3. new branch: JSON-RPC error carries the proper code ------------------

(deftest json-rpc-error-carries-proper-code
  (testing "method not found (-32601) maps to :mcp/method-not-found + code"
    (let [t (mcp-error -32601 "Method not found")]
      (is (= :mcp/method-not-found (classify-type t)))
      (is (= -32601 (classify-code t)))))
  (testing "invalid params (-32602) maps to :mcp/invalid-params + code"
    (let [t (mcp-error -32602 "Invalid params")]
      (is (= :mcp/invalid-params (classify-type t)))
      (is (= -32602 (classify-code t)))))
  (testing "internal error (-32603) maps to :mcp/internal-error + code"
    (let [t (mcp-error -32603 "Internal error")]
      (is (= :mcp/internal-error (classify-type t)))
      (is (= -32603 (classify-code t))))))

(deftest json-rpc-request-timeout-code-is-timeout
  (testing "the MCP request-timeout code (-32001) is the timeout family"
    (let [t (mcp-error -32001 "Request timed out")]
      (is (= :mcp/timeout (classify-type t)))
      (is (= -32001 (classify-code t))))))

(deftest json-rpc-unknown-code-is-generic-json-rpc-error
  (testing "an unmapped JSON-RPC code still carries the code and a base type"
    (let [t (mcp-error -32099 "server-specific error")]
      (is (= :mcp/json-rpc-error (classify-type t)))
      (is (= -32099 (classify-code t))))))

;; --- 4. fault cases ----------------------------------------------------------

(deftest unknown-non-classified-exception-is-unknown-error
  (testing "a plain RuntimeException with no transport/json-rpc signal is unknown"
    (let [t (RuntimeException. "boom")]
      (is (= :mcp/unknown-error (classify-type t)))
      (is (nil? (classify-code t))))))

(deftest json-rpc-error-wrapped-in-call-tool-failed-still-carries-code
  (testing "the production call-tool wrapper must NOT hide the JSON-RPC code"
    ;; Mirror evoclj.mcp.client/call-tool EXACTLY: an McpError is wrapped in
    ;; :mcp/call-tool-failed with its :mcp/error-code preserved in ex-data and
    ;; a sanitized :cause. The classifier must recover the JSON-RPC type/code.
    (let [wrapped (err/error :mcp/call-tool-failed
                             "MCP callTool echo failed"
                             {:tool-name "echo"
                              :mcp/error-code -32602
                              :cause (err/sanitize (mcp-error -32602 "Invalid params"))})]
      (is (= :mcp/invalid-params (classify-type wrapped))
          "JSON-RPC code survives the call-tool-failed wrapper")
      (is (= -32602 (classify-code wrapped))
          "numeric code survives the call-tool-failed wrapper")
      (is (not= :mcp/call-tool-failed (classify-type wrapped))
          "old mis-classification (fold to wrapper type) is gone"))))

(deftest timeout-wrapped-in-call-tool-failed-is-timeout
  (testing "a timeout cause inside :mcp/call-tool-failed is reported as timeout"
    (let [wrapped (err/error :mcp/call-tool-failed
                             "MCP callTool echo failed"
                             {:tool-name "echo"
                              :cause (err/sanitize (java.util.concurrent.TimeoutException. "rt"))})]
      (is (= :mcp/timeout (classify-type wrapped)))
      (is (not= :mcp/transport-error (classify-type wrapped))))))

;; --- 5. regression: old mis-classifications are gone ------------------------

(deftest regression-timeout-no-longer-folded-into-transport
  (testing "a TimeoutException must never be reported as :mcp/transport-error"
    (is (not= :mcp/transport-error
              (classify-type (java.util.concurrent.TimeoutException. "rt")))))
  (testing "a SocketTimeoutException must never be reported as :mcp/transport-error"
    (is (not= :mcp/transport-error
              (classify-type (java.net.SocketTimeoutException. "rt"))))))

(deftest regression-json-rpc-not-folded-into-call-tool-failed
  (testing "an McpError is never reported as the generic :mcp/call-tool-failed"
    (is (not= :mcp/call-tool-failed
              (classify-type (mcp-error -32603 "Internal error")))))
  (testing "the JSON-RPC code is present (not silently dropped)"
    (is (some? (classify-code (mcp-error -32601 "Method not found"))))))

;; --- 6. doc/behavior consistency ---------------------------------------------

(deftest docstring-matches-emitted-categories
  (testing "classify-mcp-error's docstring enumerates the independent timeout category"
    (let [doc (:doc (meta #'mcp-client/classify-mcp-error))]
      (is (some? doc))
      (is (re-find #":mcp/timeout" doc)
          "docstring must name the independent timeout category")
      (is (re-find #":mcp/error-code" doc)
          "docstring must describe the JSON-RPC error code field")))
  (testing "a timeout really is independent of transport at the predicate level"
    (is (mcp-client/transient-error-type? :mcp/timeout))
    (is (mcp-client/transient-error-type? :mcp/transport-error))
    (is (not (mcp-client/transient-error-type? :mcp/method-not-found)))))
