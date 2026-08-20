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

(deftest describe-includes-connection-id-when-provided
  (testing ":connection/id adds :mcp/connection-id to descriptor"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config  {:type :stdio :command "echo" :args []}
              :tool/id           :mcp/echo
              :tool/mcp-name     "echo"
              :input-schema      [:map [:text :string]]
              :output-schema     [:map [:text :string]]
              :connection/id     :shared/stdio})
          d (proto/describe p)]
      (is (= :shared/stdio (:mcp/connection-id d))))))

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
          intent {:payload {:tool/id :mcp/echo :args {:text "hello"}}}
          nr (proto/normalize-request p intent)]
      (is (= :mcp/echo (:tool/id nr)))
      (is (= {:kind :tool :id :mcp/echo} (:resource nr)))
      (is (= {:text "hello"} (:args nr))))))

(deftest normalize-request-rejects-non-edn-safe
  (testing "non-EDN-safe payload throws :provider/input-invalid"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id          :mcp/echo
              :tool/mcp-name    "echo"
              :input-schema     [:map [:text :string]]
              :output-schema    [:map [:text :string]]})
          intent {:payload {:tool/id :mcp/echo :args #{"set" "literal"}}}]
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

;; ---------------------------------------------------------------------------
;; server-id and refresh
;; ---------------------------------------------------------------------------

(deftest describe-includes-server-id-when-provided
  (testing ":mcp/server-id adds :mcp/server-id to descriptor"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id           :mcp/echo
              :tool/mcp-name     "echo"
              :input-schema      [:map [:text :string]]
              :output-schema     [:map [:text :string]]
              :mcp/server-id     "server-1"})
          d (proto/describe p)]
      (is (= "server-1" (:mcp/server-id d))))))

(deftest refresh-provider-resets-last-refreshed
  (testing "refresh-provider! resets :mcp/last-refreshed to nil"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id           :mcp/echo
              :tool/mcp-name     "echo"
              :input-schema      [:map [:text :string]]
              :output-schema     [:map [:text :string]]
              :schema/refresh-interval-ms 60000})
          d1 (proto/describe p)
          _ (mcp-bridge/refresh-provider! p)
          d2 (proto/describe p)]
      (is (some? (:mcp/last-refreshed d1)))
      (is (nil? (:mcp/last-refreshed d2))))))

(deftest refresh-provider-noop-when-not-mcp
  (testing "refresh-provider! on non-MCP provider is a no-op"
    (let [p (mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id           :mcp/echo/no-refresh
              :tool/mcp-name     "echo"
              :input-schema      [:map [:text :string]]
              :output-schema     [:map [:text :string]]})]
      (is (nil? (mcp-bridge/refresh-provider! p))))))

;; ---------------------------------------------------------------------------
;; json-schema->malli
;; ---------------------------------------------------------------------------

(deftest json-schema->malli-basic-types
  (testing "string, integer, number, boolean map to correct Malli types"
    (let [f (find-var 'evoclj.provider.mcp-bridge/json-schema->malli)]
      (is (= :string (f {:type "string"})))
      (is (= :int (f {:type "integer"})))
      (is (= :num (f {:type "number"})))
      (is (= :boolean (f {:type "boolean"}))))))

(deftest json-schema->malli-object
  (testing "object with required and optional properties"
    (let [f (find-var 'evoclj.provider.mcp-bridge/json-schema->malli)
          schema {:type "object"
                  :required ["name"]
                  :properties {"name" {:type "string"}
                               "age" {:type "integer"}}}]
      (is (= [:map ["name" :string] [:optional "age" :int]]
             (f schema))))))

(deftest json-schema->malli-array
  (testing "array with map items and without items"
    (let [f (find-var 'evoclj.provider.mcp-bridge/json-schema->malli)]
      (is (= [:vector [:map [:optional "x" :int]]]
             (f {:type "array" :items {:type "object"
                                       :properties {"x" {:type "integer"}}}})))
      (is (= [:vector :any]
             (f {:type "array"}))))))

(deftest json-schema->malli-edge-cases
  (testing "empty schema, nil, and non-map input return :any"
    (let [f (find-var 'evoclj.provider.mcp-bridge/json-schema->malli)]
      (is (= :any (f {})))
      (is (= :any (f nil)))
      (is (= :any (f "not-a-map")))
      (is (= :any (f {:type "unknown"}))))))

;; ---------------------------------------------------------------------------
;; content-block sandboxing and result audit
;; ---------------------------------------------------------------------------

(deftest content-block-sandboxes-image
  (testing ":image blocks return a safe placeholder without binary data"
    (let [block {:content/type :image
                 :content/data "base64blob"
                 :content/mime-type "image/png"}
          result ((find-var 'evoclj.provider.mcp-bridge/content-block->edn) block)]
      (is (= :image (:mcp/content-type result)))
      (is (true? (:mcp/sandboxed result)))
      (is (nil? (:content/data result)))
      (is (= "image/png" (:mime-type result))))))

(deftest content-block-sandboxes-audio
  (testing ":audio blocks return a safe placeholder without binary data"
    (let [block {:content/type :audio
                 :content/data "base64audio"
                 :content/mime-type "audio/wav"}
          result ((find-var 'evoclj.provider.mcp-bridge/content-block->edn) block)]
      (is (= :audio (:mcp/content-type result)))
      (is (true? (:mcp/sandboxed result)))
      (is (nil? (:content/data result)))
      (is (= "audio/wav" (:mime-type result))))))

(deftest content-block-sandboxes-resource-link
  (testing ":resource-link blocks return a safe placeholder with uri and mime-type"
    (let [block {:content/type :resource-link
                 :content/uri "https://example.com/x"
                 :content/mime-type "text/plain"}
          result ((find-var 'evoclj.provider.mcp-bridge/content-block->edn) block)]
      (is (= :resource-link (:mcp/content-type result)))
      (is (true? (:mcp/sandboxed result)))
      (is (= "https://example.com/x" (:uri result)))
      (is (= "text/plain" (:mime-type result))))))

(deftest content-block-sandboxes-resource
  (testing ":resource blocks return only safe metadata keys"
    (let [block {:content/type :resource
                 :content/uri "file:///tmp/x"
                 :content/mime-type "text/plain"
                 :content/text "secret data"}
          result ((find-var 'evoclj.provider.mcp-bridge/content-block->edn) block)]
      (is (= "file:///tmp/x" (:uri result)))
      (is (= "text/plain" (:mimeType result)))
      (is (nil? (:text result))))))

(deftest result->edn-multi-block-envelope
  (testing "multi-block result returns a {:value :audit} envelope"
    (let [result {:mcp/content [{:content/type :text :content/text "a"}
                                {:content/type :text :content/text "b"}]
                  :mcp/is-error false}
          edn ((find-var 'evoclj.provider.mcp-bridge/result->edn) result)]
      (is (map? edn))
      (is (= ["a" "b"] (:mcp/model-content (:value edn))))
      (let [audit (:audit edn)]
        (is (= 2 (:mcp/block-count audit)))
        (is (false? (:mcp/is-error audit)))))))

(deftest result->edn-single-text-block-envelope
  (testing "single :text block returns a {:value :audit} envelope with text in :mcp/model-content"
    (let [result {:mcp/content [{:content/type :text :content/text "hello"}]
                  :mcp/is-error false}
          edn ((find-var 'evoclj.provider.mcp-bridge/result->edn) result)]
      (is (map? edn))
      (is (= ["hello"] (:mcp/model-content (:value edn))))
      (let [audit (:audit edn)]
        (is (= 1 (:mcp/block-count audit)))
        (is (false? (:mcp/is-error audit)))))))

(deftest result->edn-audio-block-envelope
  (testing ":audio block in :mcp/model-content is sandboxed, not nil"
    (let [result {:mcp/content [{:content/type :audio
                                 :content/data "base64audio"
                                 :content/mime-type "audio/wav"}]
                  :mcp/is-error false}
          edn ((find-var 'evoclj.provider.mcp-bridge/result->edn) result)]
      (is (= [{:mcp/content-type :audio
               :mcp/sandboxed true
               :mime-type "audio/wav"}]
             (:mcp/model-content (:value edn)))))))

(deftest result->edn-structured-content-channel
  (testing "when :mcp/structured-content is present it becomes :mcp/structured-content in the envelope"
    (let [sc (doto (java.util.LinkedHashMap.)
               (.put "temperature" (java.lang.Double. 0.7))
               (.put "n" (java.lang.Integer. 2)))
          result {:mcp/content [{:content/type :text :content/text "ok"}]
                  :mcp/is-error false
                  :mcp/structured-content sc}
          edn ((find-var 'evoclj.provider.mcp-bridge/result->edn) result)]
      (is (= ["ok"] (:mcp/model-content (:value edn))))
      (let [structured (:mcp/structured-content (:value edn))]
        (is (map? structured))
        ;; string keys are preserved (no keywordization)
        (is (= 0.7 (get structured "temperature")))
        (is (= 2 (get structured "n")))))))

(deftest result->edn-is-error-audit
  (testing "is-error=true still yields a {:value :audit} envelope with :mcp/is-error true"
    (let [result {:mcp/content [{:content/type :text :content/text "err"}]
                  :mcp/is-error true}
          edn ((find-var 'evoclj.provider.mcp-bridge/result->edn) result)]
      (is (map? edn))
      (is (= ["err"] (:mcp/model-content (:value edn))))
      (let [audit (:audit edn)]
        (is (= 1 (:mcp/block-count audit)))
        (is (true? (:mcp/is-error audit)))))))
