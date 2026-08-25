(ns evoclj.mcp.refresh-schema-test
  "M18 tests: refresh-schema-now! real implementation, refresh-provider!
   transport-config preservation, and production-side last-refreshed update.

   These tests drive the PRODUCTION discovery path via the McpSource
   :discover-fn seam, a supported production option of make-mcp-source.
   refresh-schema-now! exercises the same discover-tools -> collision-check
   -> payload path that snapshot! uses, plus the new production-side state
   update (last-refreshed atom + cached payload).

   Required paths: happy refresh re-fetches + updates tools; refresh is not a
   no-op; last-refreshed updated on production side; remote error fails
   closed; refresh-provider! preserves transport-config; legacy invalidation
   still holds; closed source rejected with typed error; concurrent
   refreshes leave consistent production state."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.source :as env-source]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]))

(defn- mcp-tool [name]
  {:mcp/name name
   :mcp/title (str "title-" name)
   :mcp/description (str "desc-" name)
   :mcp/input-schema {"type" "object" "properties" {"text" {"type" "string"}} "required" ["text"]}
   :mcp/output-schema {"type" "object" "properties" {"text" {"type" "string"}}}
   :mcp/retry-safe? false})

(defn- make-source [id tools-atom]
  (mcp-source/make-mcp-source
   {:source/id id
    :transport-config {:type :stdio :command "echo" :args []}
    :manager (manager/create-manager)
    :mcp/server-id (name id)
    :discover-fn (fn [] @tools-atom)}))

(deftest refresh-schema-now-reloads-tools
  (testing "happy path: refresh re-fetches and updates the tool set"
    (let [tools-atom (atom [(mcp-tool "tool-a")])
          source (make-source :mcp/refresh-a tools-atom)
          r1 (mcp-source/refresh-schema-now! source)
          ids1 (set (keys (:tools (:payload r1))))]
      (is (some? r1) "refresh returns a snapshot map")
      (is (= :mcp/refresh-a (:source/id r1)))
      (is (contains? ids1 ["refresh-a" "tool-a"]) "first refresh sees tool-a")
      (reset! tools-atom [(mcp-tool "tool-a") (mcp-tool "tool-b")])
      (let [r2 (mcp-source/refresh-schema-now! source)
            ids2 (set (keys (:tools (:payload r2))))]
        (is (contains? ids2 ["refresh-a" "tool-a"]) "tool-a still present")
        (is (contains? ids2 ["refresh-a" "tool-b"]) "tool-b now discovered")
        (is (not= ids1 ids2) "tool set changed after refresh")))))

(deftest refresh-schema-now-is-not-a-noop
  (testing "regression: the stub/no-op refresh is gone and it actually re-reads"
    (let [tools-atom (atom [(mcp-tool "noop-tool")])
          source (make-source :mcp/noop tools-atom)
          r1 (mcp-source/refresh-schema-now! source)
          _ (reset! tools-atom [(assoc (mcp-tool "noop-tool") :mcp/description "CHANGED")])
          r2 (mcp-source/refresh-schema-now! source)
          desc1 (get-in r1 [:payload :tools ["noop" "noop-tool"] :mcp/description])
          desc2 (get-in r2 [:payload :tools ["noop" "noop-tool"] :mcp/description])]
      (is (= "desc-noop-tool" desc1) "first refresh saw original description")
      (is (= "CHANGED" desc2) "second refresh re-read the changed remote description"))))

(deftest refresh-updates-last-refreshed-production-side
  (testing "branch: last-refreshed is updated on the production side after refresh"
    (let [tools-atom (atom [(mcp-tool "lr-tool")])
          source (make-source :mcp/lr tools-atom)]
      (is (nil? @(:last-refreshed source)) "no refresh yet -> nil")
      (mcp-source/refresh-schema-now! source)
      (let [lr @(:last-refreshed source)]
        (is (some? lr) "last-refreshed atom is set on the production side")
        (is (number? lr) "last-refreshed is a numeric timestamp")
        (Thread/sleep 5)
        (mcp-source/refresh-schema-now! source)
        (is (>= @(:last-refreshed source) lr) "second refresh advances/holds last-refreshed")
        (is (contains? (:tools @(:cached-payload source)) ["lr" "lr-tool"]))))))

(deftest refresh-fails-closed-on-remote-error
  (testing "fault: a discovery error fails closed and leaves prior state intact"
    (let [fail? (atom false)
          discover (fn []
                     (if @fail?
                       (throw (ex-info "boom" {:error/type :mcp/discover-failed}))
                       [(mcp-tool "good-tool")]))
          source (mcp-source/make-mcp-source
                  {:source/id :mcp/fail
                   :transport-config {:type :stdio :command "echo" :args []}
                   :manager (manager/create-manager)
                   :mcp/server-id "fail"
                   :discover-fn discover})]
      (mcp-source/refresh-schema-now! source)
      (let [lr-before @(:last-refreshed source)
            payload-before @(:cached-payload source)]
        (reset! fail? true)
        (is (thrown? Exception (mcp-source/refresh-schema-now! source))
            "refresh propagates the discovery error (fail-closed)")
        (is (= lr-before @(:last-refreshed source)) "last-refreshed preserved on failure")
        (is (= payload-before @(:cached-payload source)) "cached payload preserved on failure")))))

(deftest refresh-provider-preserves-transport-config
  (testing "branch: refresh-provider! preserves transport-config rather than clobbering to {}"
    (let [cfg {:type :stdio :command "my-server" :args ["--port" "1234"] :env {:TOKEN "x"}}
          p (mcp-bridge/mcp-provider
             {:transport-config cfg
              :tool/id :mcp/preserve
              :tool/mcp-name "preserve"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          p2 (mcp-bridge/refresh-provider! p)]
      (is (= cfg (:transport-config p2))
          "transport-config is preserved, not clobbered to {}"))))

(deftest refresh-provider-still-invalidates
  (testing "regression: refresh-provider! still invalidates last-refreshed and bumps generation"
    (let [cfg {:type :stdio :command "my-server"}
          p (mcp-bridge/mcp-provider
             {:transport-config cfg
              :tool/id :mcp/inval
              :tool/mcp-name "inval"
              :input-schema [:map [:text :string]]
              :output-schema [:map [:text :string]]})
          d0 (proto/describe p)
          p2 (mcp-bridge/refresh-provider! p)
          d2 (proto/describe p2)]
      (is (= 0 (:mcp/generation d0)))
      (is (= 1 (:mcp/generation d2)) "generation bumped on refresh")
      (is (nil? (:mcp/last-refreshed d2)) "legacy invalidation: last-refreshed reset to nil")
      (is (= d0 (proto/describe p)) "original provider instance unchanged (immutable)"))))

(deftest refresh-schema-now-closed-source-rejected
  (testing "doc/behavior: closed source rejects refresh with a typed error"
    (let [tools-atom (atom [(mcp-tool "x")])
          source (make-source :mcp/closed-src tools-atom)]
      (env-source/close! source)
      (let [err (try (mcp-source/refresh-schema-now! source)
                     (catch Exception e e))]
        (is (some? err) "refresh on a closed source throws")
        (is (= :mcp/source-closed (:error/type (ex-data err)))
            "typed :mcp/source-closed error")))))

(deftest refresh-schema-now-concurrent
  (testing "concurrency: two concurrent refreshes leave consistent production state"
    (let [tools-atom (atom [(mcp-tool "c-tool")])
          source (make-source :mcp/concurrent tools-atom)
          f1 (future
               (reset! tools-atom [(mcp-tool "c-tool") (mcp-tool "c-1")])
               (mcp-source/refresh-schema-now! source))
          f2 (future
               (reset! tools-atom [(mcp-tool "c-tool") (mcp-tool "c-2")])
               (mcp-source/refresh-schema-now! source))]
      (let [r1 @f1
            r2 @f2]
        (is (some? r1))
        (is (some? r2))
        (is (some? @(:last-refreshed source)) "last-refreshed non-nil after concurrent refresh")
        (let [final-payload (:tools @(:cached-payload source))]
          (is (contains? final-payload ["concurrent" "c-tool"]) "base tool always present")
          (is (or (contains? final-payload ["concurrent" "c-1"])
                  (contains? final-payload ["concurrent" "c-2"]))
              "cached payload reflects one of the refreshes (no torn state)"))))))
