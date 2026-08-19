(ns evoclj.mcp.client-test
  "Unit tests for evoclj.mcp.client (no real MCP server required)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.client :as client]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]))

(defn- fake-managed
  "Build a fake managed record for tests that do not touch a live client."
  [& [overrides]]
  (merge {:client nil
          :closed? false
          :last-error nil
          :open-count 1
          :transport-config {:type :stdio :command "echo" :args []}
          :transport-type :stdio
          :opened-at "2025-01-01T00:00:00Z"
          :call-count 0
          :last-latency-ms nil}
         overrides))

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

;; ---------------------------------------------------------------------------
;; managed client lifecycle helpers (no real client needed)
;; ---------------------------------------------------------------------------

(deftest closed?-returns-true-for-nil
  (testing "closed? returns true for nil"
    (is (client/closed? nil))))

(deftest closed?-returns-true-for-closed-record
  (testing "closed? returns true for a closed managed record"
    (let [m {:client nil :closed? true :last-error nil :open-count 1}]
      (is (client/closed? m)))))

(deftest closed?-returns-false-for-open-record
  (testing "closed? returns false for an open managed record"
    (let [m {:client :fake :closed? false :last-error nil :open-count 1}]
      (is (not (client/closed? m))))))

(deftest close!-marks-closed
  (testing "close! sets closed? true and client nil"
    (let [m {:client :fake-client :closed? false :last-error nil :open-count 1}
          closed (client/close! m)]
      (is (:closed? closed))
      (is (nil? (:client closed))))))

(deftest close!-is-idempotent
  (testing "closing an already-closed record is a no-op"
    (let [m {:client :fake-client :closed? false :last-error nil :open-count 1}
          once (client/close! m)
          twice (client/close! once)]
      (is (:closed? twice))
      (is (nil? (:client twice))))))

(deftest close!-is-noop-for-nil
  (testing "close! on nil is a no-op"
    (is (nil? (client/close! nil)))))

;; ---------------------------------------------------------------------------
;; call-tool-managed rejects closed client
;; ---------------------------------------------------------------------------

(deftest call-tool-managed-rejects-closed
  (testing "call-tool-managed throws :mcp/client-closed on closed record"
    (let [m {:client :fake :closed? true :last-error nil :open-count 1}]
      (let [e (atom nil)]
        (try
          (reset! e (client/call-tool-managed m "foo" {}))
          (is false "expected exception")
          (catch Throwable t
            (reset! e t)))
        (is (= :mcp/client-closed (:error/type (ex-data @e))))))))

;; ---------------------------------------------------------------------------
;; connection pool behavior (via mcp-bridge integration)
;; ---------------------------------------------------------------------------

(deftest shared-connection-id-reuses-connection
  (testing "two providers with same :connection/id share a client"
    (let [p1 (mcp-bridge/mcp-provider
              {:transport-config {:type :stdio :command "echo" :args []}
               :tool/id :mcp/echo-1
               :tool/mcp-name "echo"
               :input-schema [:map [:text :string]]
               :output-schema [:map [:text :string]]
               :connection/id :shared/stdio})
          p2 (mcp-bridge/mcp-provider
              {:transport-config {:type :stdio :command "echo" :args []}
               :tool/id :mcp/echo-2
               :tool/mcp-name "echo"
               :input-schema [:map [:text :string]]
               :output-schema [:map [:text :string]]
               :connection/id :shared/stdio})]
      ;; Both descriptors should carry the same connection id
      (is (= :shared/stdio (:mcp/connection-id (proto/describe p1))))
      (is (= :shared/stdio (:mcp/connection-id (proto/describe p2)))))))

;; ---------------------------------------------------------------------------
;; observability: open! metadata and ping!
;; ---------------------------------------------------------------------------

(deftest open!-carries-transport-metadata
  (testing "open! returns transport metadata and zeroed counters"
    (with-redefs [evoclj.mcp.client/build-client
                  (fn [_]
                    {:fake-client true})]
      (let [m (client/open! {:type :stdio :command "echo" :args []})]
        (is (= :stdio (:transport-type m)))
        (is (string? (:opened-at m)))
        (is (= 0 (:call-count m)))
        (is (nil? (:last-latency-ms m)))
        (is (= {:type :stdio :command "echo" :args []} (:transport-config m)))))))

(deftest ping!-rejects-closed-managed
  (testing "ping! throws :mcp/ping-failed on closed record"
    (let [m (fake-managed {:closed? true})]
      (let [e (atom nil)]
        (try
          (reset! e (client/ping! m))
          (is false "expected exception")
          (catch Throwable t
            (reset! e t)))
        (is (= :mcp/ping-failed (:error/type (ex-data @e))))))))

(deftest ping!-rejects-nil-managed
  (testing "ping! throws :mcp/ping-failed on nil"
    (let [e (atom nil)]
      (try
        (reset! e (client/ping! nil))
        (is false "expected exception")
        (catch Throwable t
          (reset! e t)))
      (is (= :mcp/ping-failed (:error/type (ex-data @e)))))))
