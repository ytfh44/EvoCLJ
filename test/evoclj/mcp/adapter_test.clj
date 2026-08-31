(ns evoclj.mcp.adapter-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.adapter :as adapter]))

(deftest parity-modulo-wire-fields
  (let [a25 (adapter/adapter-2025)
        a26 (adapter/adapter-2026 {:ttl-ms 60000})
        c {:tool/id :x :args {:a 1}}]
    (is (= :mcp-2025-11 (:adapter/version (adapter/wire-request a25 c))))
    (is (= :mcp-2026-07 (:adapter/version (adapter/wire-request a26 c))))))

(deftest cache-and-subscription-and-continue
  (let [a26 (adapter/adapter-2026 {:ttl-ms 60000})]
    (is (some? (adapter/cache-policy a26)))
    (is (nil? (adapter/cache-policy (adapter/adapter-2025))))
    (adapter/on-notification a26 {:event :tools-changed})
    (is (= :continuing (:status (adapter/continue a26 {:id 1})))))
  ;; A6: 2025 no longer throws :mcp/not-supported — it degrades to a queued
  ;; command envelope so recovery can redeliver the continuation.
  (is (= :queued (:status (adapter/continue (adapter/adapter-2025) {:id 1}))))
  (is (uuid? (:command-id (adapter/continue (adapter/adapter-2025) {:id 2}))) "2025 continue carries a command-id for audit"))
