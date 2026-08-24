(ns evoclj.mcp.canonical-resource-test
  (:require [clojure.test :refer [deftest is]]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.capability.broker :as broker]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]))

(deftest read-file-shadow-denied-despite-tool-lease
  (let [tool-id :mcp/read_file
        provider (mcp-bridge/mcp-provider {:transport-config {:type :stdio :command "echo"}
                                           :tool/id tool-id
                                           :tool/mcp-name "read_file"
                                           :input-schema [:map]
                                           :output-schema [:map]})
        intent {:intent/id #uuid "00000000-0000-0000-0000-000000000001"
                :intent/type :intent/tool-call
                :phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                :session/id #uuid "00000000-0000-0000-0000-000000000002"
                :node/id :node/tool :cause/event-id 1
                :payload {:tool/id tool-id :args {:path "/etc/shadow"}}
                :budget {:wall-ms 1000} :metadata {}}
        normalized (proto/normalize-request provider intent)
        lease {:cap/id #uuid "00000000-0000-0000-0000-000000000010"
               :subject {:phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
               :resource {:kind :tool :id tool-id} :actions #{:invoke} :constraints {}
               :issued-at #inst "2020-01-01" :expires-at #inst "2030-01-01"}
        fs-lease {:cap/id #uuid "00000000-0000-0000-0000-000000000011"
                  :subject {:phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
                  :resource {:kind :filesystem/path :path "/workspace"} :actions #{:invoke} :constraints {}
                  :issued-at #inst "2020-01-01" :expires-at #inst "2030-01-01"}
        decision (broker/authorize {:intent intent :normalized-request normalized :leases [lease fs-lease] :usage {} :now #inst "2025-01-01"})]
    (is (= "/etc/shadow" (:path (:resource normalized))))
    (is (= :deny (:decision decision)))
    (is (= :capability/scope-denied (:reason decision)))))

(deftest normalized-traversal-judged-correctly
  (is (= "secret" (:path (canonical/canonical-resource :mcp/read_file {"path" "a/../secret"}))))
  (is (= "/workspace/secret" (:path (canonical/canonical-resource :mcp/read_file {"path" "/workspace/a/../secret"}))))
  (let [r (canonical/canonical-resource :read_file {"path" "a/../secret"})]
    (is (= :filesystem/path (:kind r)))))
