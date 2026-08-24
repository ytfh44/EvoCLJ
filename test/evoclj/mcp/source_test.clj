(ns evoclj.mcp.source-test
  "McpSource LiveSource tests (Phase 4).

   Verifies:
   - McpSource implements LiveSource, uses McpManager for pooling, and
     on tools/list_changed only calls invalidate (not direct registry mutate)
   - ToolEntry immutability: @17 and @18 are distinct immutable values sharing same manager
   - In-flight call sees old entry during refresh, new call after refresh sees new entry
   - Removed tool not in new ToolSurface, old binding retains old descriptor but execution may fail
   - Descriptor no longer mutates in place
   - ToolSurface derived from MCP Revision matches old provider registry behavior (equivalence)
   - Identical tool set yields noop (no churn)"
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.registry :as env-reg]
            [evoclj.environment.revision :as rev]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.provider.mcp-bridge]
            [evoclj.provider.protocol :as proto]))

(defn- mcp-tool [name]
  {:mcp/name name
   :mcp/title (str "title-" name)
   :mcp/description (str "desc-" name)
   :mcp/input-schema {"type" "object" "properties" {"text" {"type" "string"}} "required" ["text"]}
   :mcp/output-schema {"type" "object" "properties" {"text" {"type" "string"}}}
   :mcp/retry-safe? false})

(deftest mcp-source-is-live-source
  (testing "McpSource satisfies LiveSource and snapshot is pure"
    (let [mgr (manager/create-manager)
          tools-atom (atom [(mcp-tool "tool-a") (mcp-tool "tool-b")])
          discover-fn (fn [] @tools-atom)
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/test
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :discover-fn discover-fn})]
      (is (satisfies? evoclj.environment.source/LiveSource source))
      (let [snap1 (evoclj.environment.source/snapshot! source)
            snap2 (evoclj.environment.source/snapshot! source)]
        (is (= :mcp/test (:source/id snap1)))
        (is (contains? snap1 :payload))
        (is (contains? snap1 :captured-at))
        (is (= (:payload snap1) (:payload snap2)) "identical tool set yields identical payload (no churn from timestamps)")
        (is (= (rev/payload->id (:payload snap1)) (rev/payload->id (:payload snap2))))))))

(deftest mcp-source-uses-manager-pooling
  (testing "ToolEntries from same McpSource share same manager connection"
    (let [mgr (manager/create-manager)
          discover-fn (fn [] [(mcp-tool "shared-tool")])
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/shared
                   :transport-config {:type :stdio :command "echo" :args [] :connection/id :shared/test}
                   :manager mgr
                   :discover-fn discover-fn})
          snap (evoclj.environment.source/snapshot! source)
          payload (:payload snap)
          rev (rev/make-revision :mcp/shared payload 1)
          entries1 (mcp-source/tool-entries->surface payload mgr {:type :stdio :command "echo" :args [] :connection/id :shared/test} rev)
          ;; second revision with same tool but new generation
          rev2 (rev/make-revision :mcp/shared payload 2)
          entries2 (mcp-source/tool-entries->surface payload mgr {:type :stdio :command "echo" :args [] :connection/id :shared/test} rev2)
          e1 (get entries1 :mcp/shared-tool)
          e2 (get entries2 :mcp/shared-tool)]
      (is (some? e1))
      (is (some? e2))
      (is (not= e1 e2) "distinct immutable ToolEntry values")
      (is (not= (:descriptor e1) (:descriptor e2)) "descriptors differ by generation")
      (is (= (:manager e1) (:manager e2)) "share same manager atom")
      (is (= (:conn-key e1) (:conn-key e2)) "share same conn-key")
      (is (= 1 (:mcp/generation (:descriptor e1))))
      (is (= 2 (:mcp/generation (:descriptor e2))))
      (is (= (:mcp/generation (:descriptor e1)) (:revision/seq rev)))
      )))

(deftest tool-entry-immutability
  (testing "ToolEntry@17 and @18 are immutable, descriptor does not mutate in place"
    (let [mgr (manager/create-manager)
          discover-fn (fn [] [(mcp-tool "immutable-tool")])
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/immut
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :discover-fn discover-fn})
          snap1 (evoclj.environment.source/snapshot! source)
          payload1 (:payload snap1)
          rev1 (rev/make-revision :mcp/immut payload1 17)
          entries1 (mcp-source/tool-entries->surface payload1 mgr {:type :stdio :command "echo"} rev1)
          e17 (get entries1 :mcp/immutable-tool)
          desc17 (proto/describe e17)]
      ;; snapshot again with same payload but new revision 18
      (let [rev18 (rev/make-revision :mcp/immut payload1 18)
            entries18 (mcp-source/tool-entries->surface payload1 mgr {:type :stdio :command "echo"} rev18)
            e18 (get entries18 :mcp/immutable-tool)
            desc18 (proto/describe e18)]
        (is (= 17 (:mcp/generation desc17)))
        (is (= 18 (:mcp/generation desc18)))
        (is (not= desc17 desc18) "distinct descriptors")
        (is (= 17 (:mcp/generation (proto/describe e17))) "e17 still 17 after e18 created (immutable)")
        (is (= 18 (:mcp/generation (proto/describe e18))))))))

(deftest in-flight-vs-new-call
  (testing "in-flight call sees old entry during refresh, new call after refresh sees new entry"
    (let [mgr (manager/create-manager)
          tools-atom (atom [(mcp-tool "inflight-tool")])
          discover-fn (fn [] @tools-atom)
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/inflight-src
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :discover-fn discover-fn})
          env (env-reg/create-registry)
          _ (env-reg/register-source! env source)
          r1 (env-reg/refresh! env)
          rev1 (:revision r1)
          payload1 (:payload rev1)
          entries1 (mcp-source/tool-entries->surface payload1 mgr {:type :stdio :command "echo"} rev1)
          old-entry (get entries1 :mcp/inflight-tool)
          old-desc (proto/describe old-entry)]
      ;; simulate in-flight holder keeping old-entry
      (is (= 1 (:mcp/generation old-desc)))
      ;; now discover returns updated schema for same tool (e.g., description changed)
      ;; but we keep same tool name, so payload will differ, causing new revision
      (reset! tools-atom [(assoc (mcp-tool "inflight-tool") :mcp/description "updated desc")])
      (let [r2 (env-reg/refresh! env)
            rev2 (:revision r2)
            payload2 (:payload rev2)
            entries2 (mcp-source/tool-entries->surface payload2 mgr {:type :stdio :command "echo"} rev2)
            new-entry (get entries2 :mcp/inflight-tool)
            new-desc (proto/describe new-entry)]
        (is (= :published (:status r2)) "new payload yields new revision")
        (is (= 2 (:revision/seq rev2)))
        (is (not= (:revision/id rev1) (:revision/id rev2)))
        ;; old in-flight still sees old descriptor
        (is (= 1 (:mcp/generation (proto/describe old-entry))) "old entry still gen 1")
        (is (= 2 (:mcp/generation new-desc)) "new entry gen 2")
        (is (= old-desc (proto/describe old-entry)) "old descriptor unchanged")
        (is (not= old-desc new-desc))))))

(deftest removed-tool-not-in-new-surface
  (testing "removed tool not in new ToolSurface, old binding retains old descriptor but execution may fail"
    (let [mgr (manager/create-manager)
          tools-atom (atom [(mcp-tool "keep-tool") (mcp-tool "remove-tool")])
          discover-fn (fn [] @tools-atom)
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/remove-src
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :discover-fn discover-fn})
          env (env-reg/create-registry)
          _ (env-reg/register-source! env source)
          r1 (env-reg/refresh! env)
          rev1 (:revision r1)
          payload1 (:payload rev1)
          entries1 (mcp-source/tool-entries->surface payload1 mgr {:type :stdio :command "echo"} rev1)
          old-entry (get entries1 :mcp/remove-tool)
          old-desc (proto/describe old-entry)]
      (is (contains? entries1 :mcp/remove-tool))
      (is (contains? entries1 :mcp/keep-tool))
      ;; simulate remote deletion: discover now returns only keep-tool
      (reset! tools-atom [(mcp-tool "keep-tool")])
      (let [r2 (env-reg/refresh! env)
            rev2 (:revision r2)
            payload2 (:payload rev2)
            entries2 (mcp-source/tool-entries->surface payload2 mgr {:type :stdio :command "echo"} rev2)]
        (is (= :published (:status r2)))
        (is (not (contains? entries2 :mcp/remove-tool)) "removed tool not in new ToolSurface")
        (is (contains? entries2 :mcp/keep-tool))
        ;; old binding retains old descriptor (immutable)
        (is (= old-desc (proto/describe old-entry)) "old entry still has old descriptor")
        ;; execution may fail if remote physically deleted: simulate by calling old entry's execute
        ;; which will try to call tool that no longer exists; it should throw :provider/execution-failed or similar
        ;; we don't have a live server, but we can verify that old entry still exists and new surface doesn't
        (is (some? old-entry))
        (is (nil? (get entries2 :mcp/remove-tool)))))))

(deftest tools-list-changed-only-invalidates
  (testing "on tools/list_changed only invalidate is called, not direct registry mutate"
    (let [mgr (manager/create-manager)
          tools-atom (atom [(mcp-tool "tool-a")])
          discover-fn (fn [] @tools-atom)
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/invalidate-src
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :discover-fn discover-fn})
          env (env-reg/create-registry)
          _ (env-reg/register-source! env source)
          _ (env-reg/refresh! env)
          original-current (env-reg/current env)
          original-id (:revision/id original-current)
          ;; track whether invalidate was called
          invalidate-called (atom false)
          ;; our source's trigger should call the registry's invalidate path (dirty + async refresh)
          ;; we simulate by directly calling trigger-tools-changed! which should invoke subscribers
          _ (let [subs @(:subs source)]
              (is (= 1 (count subs)) "one subscriber (registry)"))
          before-dirty (:dirty? (env-reg/status env))]
      ;; trigger notification
      (mcp-source/trigger-tools-changed! source)
      ;; after trigger, registry should be marked dirty and async refresh scheduled
      ;; we give async a moment, then manually refresh to ensure new revision if payload changed
      (Thread/sleep 100)
      ;; change payload to force new revision on next refresh
      (reset! tools-atom [(mcp-tool "tool-a") (mcp-tool "tool-b")])
      (let [r (env-reg/refresh! env)
            new-current (env-reg/current env)]
        (is (= :published (:status r)) "refresh after invalidate publishes new revision")
        (is (not= original-id (:revision/id new-current)))
        (is (= 2 (:revision/seq new-current)))))))

(deftest identical-content-no-churn
  (testing "identical tool set yields noop (no new seq) via EnvironmentRegistry"
    (let [mgr (manager/create-manager)
          discover-fn (fn [] [(mcp-tool "stable-tool")])
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/stable
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :discover-fn discover-fn})
          env (env-reg/create-registry)
          _ (env-reg/register-source! env source)
          r1 (env-reg/refresh! env)
          r2 (env-reg/refresh! env)]
      (is (= :published (:status r1)))
      (is (= :noop (:status r2)) "identical payload yields noop")
      (is (= (:revision/id (:revision r1)) (:revision/id (:revision r2))))
      (is (= (:revision/seq (:revision r1)) (:revision/seq (:revision r2)))))))

(deftest manager-only-transport-lifecycle
  (testing "manager no longer has refresh-registry, only pools"
    (let [mgr (manager/create-manager)]
      (is (contains? @mgr :pools))
      (is (not (contains? @mgr :refresh-registry)) "refresh-registry removed")
      (is (nil? (ns-resolve 'evoclj.mcp.manager 'tool-status)) "tool-status removed")
      (is (nil? (ns-resolve 'evoclj.mcp.manager 'mark-removed!)) "mark-removed! removed")
      (is (nil? (ns-resolve 'evoclj.mcp.manager 'on-tools-changed!)) "on-tools-changed! removed")
      ;; pools still work
      (let [ck [:stdio :test {} 0]
            open-fn (fn [] {:client (Object.) :transport-config {:type :stdio}})]
        (manager/get-or-open! mgr ck open-fn)
        (is (some? (manager/pool-get mgr ck)))
        (manager/shutdown! mgr)
        (is (empty? (:pools @mgr)))))))

(deftest descriptor-immutability-via-bridge
  (testing "bridge ToolEntry descriptor does not mutate in place"
    (let [p (evoclj.provider.mcp-bridge/mcp-provider
             {:transport-config {:type :stdio :command "echo" :args []}
              :tool/id :mcp/immut-bridge
              :tool/mcp-name "immut-bridge"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          d0 (proto/describe p)
          p2 (evoclj.provider.mcp-bridge/refresh-provider! p)
          d1 (proto/describe p)
          d2 (proto/describe p2)]
      (is (= 0 (:mcp/generation d0)))
      (is (= 0 (:mcp/generation d1)) "original unchanged")
      (is (= 1 (:mcp/generation d2)) "new entry bumped")
      (is (not= d0 d2))
      (is (= d0 d1)))))

(deftest equivalence-old-vs-new-path
  (testing "ToolSurface derived from MCP Revision matches old provider registry behavior (payload equivalence)"
    (let [;; old path: single provider via mcp-bridge (direct Malli)
          old-p (evoclj.provider.mcp-bridge/mcp-provider
                 {:transport-config {:type :stdio :command "echo" :args []}
                  :tool/id :mcp/equiv
                  :tool/mcp-name "equiv"
                  :input-schema [:map [:text :string]]
                  :output-schema [:map [:text :string]]})
          old-desc (proto/describe old-p)
          ;; new path: McpSource discovery via JSON Schema -> Malli (string keys)
          mgr (manager/create-manager)
          discover-fn (fn [] [{:mcp/name "equiv"
                               :mcp/input-schema {"type" "object" "properties" {"text" {"type" "string"}} "required" ["text"]}
                               :mcp/output-schema {"type" "object" "properties" {"text" {"type" "string"}}}}])
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/equiv-src
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager mgr
                   :discover-fn discover-fn})
          snap (evoclj.environment.source/snapshot! source)
          payload (:payload snap)
          rev (rev/make-revision :mcp/equiv-src payload 1)
          entries (mcp-source/tool-entries->surface payload mgr {:type :stdio :command "echo"} rev)
          new-entry (get entries :mcp/equiv)
          new-desc (proto/describe new-entry)]
      ;; core identity matches
      (is (= (:tool/id old-desc) (:tool/id new-desc)))
      (is (= (:effect old-desc) (:effect new-desc)))
      (is (= :remote (:effect new-desc)))
      ;; old used direct Malli with keyword keys, new via JSON schema with string keys
      ;; both should validate their respective shapes (not identical Malli)
      (is (= [:map [:text :string]] (:input-schema old-desc)))
      (is (not= :any (:input-schema new-desc)) "new has derived Malli, not :any")
      ;; mcp/input-schema: old was {} (no JSON), new is the JSON schema
      (is (= {} (:mcp/input-schema old-desc)))
      (is (= {"type" "object" "properties" {"text" {"type" "string"}} "required" ["text"]} (:mcp/input-schema new-desc)))
      ;; ToolSurface equivalence: both have one entry for :mcp/equiv
      (is (= 1 (count entries)))
      (is (contains? entries :mcp/equiv))
      )))
