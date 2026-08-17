(ns evoclj.provider.mcp-bridge-test
  "Unit tests for evoclj.provider.mcp-bridge (no real MCP server required)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]))

;; ---------------------------------------------------------------------------
;; describe
;; ---------------------------------------------------------------------------

(deftest describe-returns-static-descriptor
  (testing "a minimal config produces a valid descriptor"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config  {:type :stdio :command "echo" :args []}
              :tool/id           :mcp/echo
              :tool/mcp-name     "echo"
              :input-schema      [:map [:text :string]]
              :output-schema     [:map [:text :string]]})
          d (proto/describe p)]
      (is (= :mcp/echo (:tool/id d)))
      (is (= :remote (:effect d)))
      (is (= [:map [:text :string]] (:input-schema d)))
      (is (= [:map [:text :string]] (:output-schema d)))
      (is (= :invoke (:required-action d))))))

(deftest describe-includes-retry-when-safe
  (testing ":retry-safe? true adds :retry {:safe? true}"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config  {:type :stdio :command "echo" :args []}
              :tool/id           :mcp/echo
              :tool/mcp-name     "echo"
              :input-schema      [:map [:text :string]]
              :output-schema     [:map [:text :string]]
              :retry-safe?       true})
          d (proto/describe p)]
      (is (= {:safe? true} (:retry d))))))

(deftest describe-rejects-missing-tool-id
  (testing "missing :tool/id throws :provider/config-invalid"
    (let [e (atom nil)]
      (try
        (reset! e (mcp-bridge/mcp-provider
                    {:transport-config {:type :stdio :command "echo" :args []}
                     :tool/mcp-name     "echo"
                     :input-schema      [:map [:text :string]]
                     :output-schema     [:map [:text :string]]}))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :provider/config-invalid (:error/type (ex-data @e)))))))

(deftest describe-rejects-missing-mcp-name
  (testing "missing :tool/mcp-name throws :provider/config-invalid"
    (let [e (atom nil)]
      (try
        (reset! e (mcp-bridge/mcp-provider
                    {:transport-config {:type :stdio :command "echo" :args []}
                     :tool/id          :mcp/echo
                     :input-schema     [:map [:text :string]]
                     :output-schema    [:map [:text :string]]}))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :provider/config-invalid (:error/type (ex-data @e)))))))

;; ---------------------------------------------------------------------------
;; normalize-request
;; ---------------------------------------------------------------------------

(deftest normalize-request-validates-and-maps
  (testing "valid payload passes through with canonical resource"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id          :mcp/echo
              :tool/mcp-name    "echo"
              :input-schema     [:map [:text :string]]
              :output-schema    [:map [:text :string]]})
          intent {:payload {:text "hello"}}
          nr (proto/normalize-request p intent)]
      (is (= :mcp/echo (:tool/id nr)))
      (is (= {:kind :mcp-tool :id "echo"} (:resource nr)))
      (is (= {:text "hello"} (:args nr))))))

(deftest normalize-request-rejects-non-edn-safe
  (testing "non-EDN-safe payload throws :provider/input-invalid"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id          :mcp/echo
              :tool/mcp-name    "echo"
              :input-schema     [:map [:text :string]]
              :output-schema    [:map [:text :string]]})
          intent {:payload #{"set" "literal"}}]
      (let [e (atom nil)]
        (try
          (reset! e (proto/normalize-request p intent))
          (is false "expected exception")
          (catch Throwable t
            (reset! e t)))
        (is (= :provider/input-invalid (:error/type (ex-data @e))))))))

;; ---------------------------------------------------------------------------
;; execute-request!
;; ---------------------------------------------------------------------------

(deftest execute-request-rejects-non-normalized
  (testing "request with wrong :tool/id throws :provider/request-invalid"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id          :mcp/echo
              :tool/mcp-name    "echo"
              :input-schema     [:map [:text :string]]
              :output-schema    [:map [:text :string]]})
          bad-req {:tool/id :other/tool :args {}}]
      (let [e (atom nil)]
        (try
          (reset! e (proto/execute-request! p bad-req))
          (is false "expected exception")
          (catch Throwable t
            (reset! e t)))
        (is (= :provider/request-invalid (:error/type (ex-data @e))))))))
