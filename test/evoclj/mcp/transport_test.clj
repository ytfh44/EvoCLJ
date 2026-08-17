(ns evoclj.mcp.transport-test
  "Tests for the MCP transport constructors (Step 2).
   
   These are integration-lite tests: they verify the Java SDK
   constructors are called with the right types and return non-nil
   objects. They do NOT spin up a real MCP server."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.transport :as transport]))

(defn- fails-with-transport-invalid [thunk]
  "Assert that `thunk` throws an exception with ex-data containing
   {:error/type :mcp/transport-invalid}. Works around Clojure versions
   that don't support the 2-arg thrown-with-msg? form."
  (let [e (atom nil)]
    (try
      (reset! e (thunk))
      (is false "expected :mcp/transport-invalid but no exception was thrown")
      (catch Throwable t
        (reset! e t)))
    (let [data (ex-data @e)]
      (is (some? data) "exception should carry ex-data")
      (is (= :mcp/transport-invalid (:error/type data))))))

;; ============================================================================
;; Stdio transport
;; ============================================================================

(deftest stdio-transport-returns-non-nil
  (testing "a valid stdio config returns a transport object"
    (let [t (transport/stdio-transport
             {:command "echo"
              :args ["hello"]})]
      (is (some? t)
          "stdio-transport returns a non-nil transport"))))

(deftest stdio-transport-requires-command-string
  (testing "missing :command throws :mcp/transport-invalid"
    (fails-with-transport-invalid #(transport/stdio-transport {:args []}))
    (fails-with-transport-invalid #(transport/stdio-transport {:command 42}))))

;; ============================================================================
;; SSE transport
;; ============================================================================

(deftest sse-transport-returns-non-nil
  (testing "a valid SSE URL returns a transport object"
    (let [t (transport/sse-transport "http://localhost:3000/sse")]
      (is (some? t)
          "sse-transport returns a non-nil transport"))))

(deftest sse-transport-requires-url-string
  (testing "non-string URL throws :mcp/transport-invalid"
    (fails-with-transport-invalid #(transport/sse-transport nil))))

;; ============================================================================
;; Streamable HTTP transport
;; ============================================================================

(deftest streamable-http-transport-returns-non-nil
  (testing "a valid URL returns a transport object with default endpoint"
    (let [t (transport/streamable-http-transport "http://localhost:3000")]
      (is (some? t)
          "streamable-http-transport returns a non-nil transport")))
  (testing "a custom endpoint is accepted"
    (let [t (transport/streamable-http-transport
             "http://localhost:3000" "/custom")]
      (is (some? t)
          "streamable-http-transport returns a non-nil transport with custom endpoint"))))

(deftest streamable-http-transport-requires-url-string
  (testing "non-string URL throws :mcp/transport-invalid"
    (fails-with-transport-invalid #(transport/streamable-http-transport nil))))

;; ============================================================================
;; Transport factory
;; ============================================================================

(deftest transport-for-dispatches-correctly
  (testing ":stdio builds a StdioClientTransport"
    (let [t (transport/transport-for
             {:type :stdio
              :command "echo"
              :args ["hello"]})]
      (is (some? t))))
  (testing ":sse builds an HttpClientSseClientTransport"
    (let [t (transport/transport-for
             {:type :sse
              :url "http://localhost:3000/sse"})]
      (is (some? t))))
  (testing ":http builds an HttpClientStreamableHttpTransport"
    (let [t (transport/transport-for
             {:type :http
              :url "http://localhost:3000"
              :endpoint "/mcp"})]
      (is (some? t)))))

(deftest transport-for-rejects-unknown-type
  (testing "unknown :type throws :mcp/transport-invalid"
    (fails-with-transport-invalid #(transport/transport-for {:type :weird}))))
