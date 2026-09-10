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

;; ============================================================================
;; SDK 2.0.0 API drift fixes (#36): the three secret-carrying config
;; channels (:env / :headers / :tls-context) must construct successfully.
;; Before the fix each threw a reflection error ("No matching method").
;; ============================================================================

(deftest stdio-env-channel-constructs
  (testing "#36: stdio :env flows through ServerParameters$Builder.env(Map)"
    (is (some? (transport/transport-for
                {:type :stdio :command "node" :args ["-x"]
                 :env {"TOKEN" "sk-canary" "PATH" "/usr/bin"}}))
        "stdio transport with :env constructs")
    (is (some? (transport/stdio-transport
                {:command "node" :env {"TOKEN" "sk-canary"}}))
        "the private builder path accepts :env")))

(deftest stdio-cwd-fails-closed
  (testing "#36: :cwd is unsupported by SDK 2.0.0 ServerParameters — typed error"
    (let [t (try (transport/stdio-transport {:command "node" :cwd "/tmp"})
                 nil
                 (catch Throwable e e))
          d (ex-data t)]
      (is (some? t) "a :cwd config is rejected")
      (is (= :mcp/transport-invalid (:error/type d)) "typed transport error")
      (is (not (re-find #"No matching method" (pr-str d)))
          "no reflection error leaks — explicit fail-closed instead"))))

(deftest http-headers-channel-constructs
  (testing "#36: :headers flows through httpRequestCustomizer"
    (is (some? (transport/transport-for
                {:type :http :url "http://127.0.0.1:1"
                 :headers {"Authorization" "Bearer sk-canary"}}))
        "streamable HTTP transport with :headers constructs")
    (is (some? (transport/transport-for
                {:type :sse :url "http://127.0.0.1:1/sse"
                 :headers {"X-Custom" "value"}}))
        "SSE transport with :headers constructs")))

(deftest http-header-customizer-stamps-headers
  (testing "#36: the customizer actually stamps headers on a real HttpRequest"
    (let [c (#'transport/header-customizer {"Authorization" "Bearer sk-canary"
                                            "X-Env" "prod"})
          uri (java.net.URI/create "http://127.0.0.1:1/mcp")
          rb (java.net.http.HttpRequest/newBuilder uri)]
      (.customize
       ^io.modelcontextprotocol.client.transport.customizer.McpSyncHttpClientRequestCustomizer
       c rb "POST" uri "{}" nil)
      (let [h (.headers (.build rb))]
        (is (= "Bearer sk-canary"
               (.orElse (.firstValue h "Authorization") nil))
            "Authorization header was stamped by the customizer")
        (is (= "prod" (.orElse (.firstValue h "X-Env") nil))
            "custom header was stamped by the customizer")))))

(deftest tls-context-channel-constructs
  (testing "#36: :tls-context flows through customizeClient(HttpClient$Builder)"
    (is (some? (transport/transport-for
                {:type :http :url "http://127.0.0.1:1"
                 :tls-context (javax.net.ssl.SSLContext/getDefault)}))
        "streamable HTTP transport with SSLContext constructs")
    (let [tmf (javax.net.ssl.TrustManagerFactory/getInstance
               (javax.net.ssl.TrustManagerFactory/getDefaultAlgorithm))]
      (let [ks (cast java.security.KeyStore nil)]
        (.init ^javax.net.ssl.TrustManagerFactory tmf ks))
      (let [tms (.getTrustManagers ^javax.net.ssl.TrustManagerFactory tmf)]
        (is (some? (transport/transport-for
                    {:type :sse :url "http://127.0.0.1:1/sse"
                     :tls-context {:trust-managers tms}}))
            "trust-managers map shape is accepted")))))

;; ============================================================================
;; #35: transport error payloads must not carry secret values
;; ============================================================================

(deftest transport-errors-do-not-leak-secrets
  (testing "#35: stdio-invalid and unknown-type errors carry :config-diag, never secrets"
    (let [env-secret "sk-canary-env-must-not-leak"
          header-secret "Bearer canary-header-must-not-leak"]
      ;; stdio invalid (missing :command)
      (let [d (ex-data (try (transport/stdio-transport
                             {:args ["--token" "arg-canary"]
                              :env {"TOKEN" env-secret}
                              :headers {"Authorization" header-secret}})
                            (catch Throwable e e)))
            s (pr-str d)]
        (is (= :mcp/transport-invalid (:error/type d)))
        (is (not (.contains ^String s env-secret)) "env value not in error data")
        (is (not (.contains ^String s header-secret)) "header value not in error data")
        (is (not (.contains ^String s "arg-canary")) "args values not in error data")
        (is (some? (:config-diag d)) ":config-diag present")
        (is (not (.contains ^String (pr-str (:config-diag d)) env-secret))
            "the diagnostic summary itself carries no secret values"))
      ;; unknown transport type
      (let [d (ex-data (try (transport/transport-for
                             {:type :weird :env {"TOKEN" env-secret}
                              :headers {"Authorization" header-secret}})
                            (catch Throwable e e)))
            s (pr-str d)]
        (is (= :mcp/transport-invalid (:error/type d)))
        (is (not (.contains ^String s env-secret)) "env value not in error data")
        (is (not (.contains ^String s header-secret)) "header value not in error data")
        (is (some? (:config-diag d)) ":config-diag present")))))
