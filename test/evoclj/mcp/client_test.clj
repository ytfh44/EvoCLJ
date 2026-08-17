(ns evoclj.mcp.client-test
  "Unit tests for evoclj.mcp.client (no real MCP server required)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.client :as client]))

;; ---------------------------------------------------------------------------
;; call-tool validation (these run without a live client)
;; ---------------------------------------------------------------------------

(deftest call-tool-rejects-non-string-name
  (testing "non-string tool name throws :mcp/call-invalid"
    (let [e (atom nil)]
      (try
        (reset! e (client/call-tool nil nil {}))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :mcp/call-invalid (:error/type (ex-data @e)))))))

(deftest call-tool-rejects-non-map-args
  (testing "non-map args throws :mcp/call-invalid"
    (let [e (atom nil)]
      (try
        (reset! e (client/call-tool nil "foo" []))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :mcp/call-invalid (:error/type (ex-data @e)))))))

(deftest call-tool-rejects-nil-args
  (testing "nil args throws :mcp/call-invalid"
    (let [e (atom nil)]
      (try
        (reset! e (client/call-tool nil "foo" nil))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :mcp/call-invalid (:error/type (ex-data @e)))))))
