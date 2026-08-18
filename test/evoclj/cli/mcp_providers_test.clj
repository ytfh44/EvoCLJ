(ns evoclj.cli.mcp-providers-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.cli.session :as cli-session]
            [evoclj.provider.registry :as registry]))

(defn- dummy-mcp-cfg []
  {:provider/type :mcp/bridge
   :tool/id :test/mcp-tool
   :tool/mcp-name "server-tool"
   :transport-config {}
   :input-schema [:map [:text :string]]
   :output-schema [:map [:text :string]]})

(deftest mcp-providers-omitted-when-not-in-config
  (testing "returns nil when config has no :mcp-providers"
    (is (nil? (cli-session/mcp-providers-from-config
                {:provider/registry
                 {:providers [{:provider/type :fixture/echo}]}}))))
  (testing "returns nil when :provider/registry is absent"
    (is (nil? (cli-session/mcp-providers-from-config {}))))
  (testing "returns nil when config is empty"
    (is (nil? (cli-session/mcp-providers-from-config {})))))

(deftest mcp-providers-returned-when-present
  (testing "returns the :mcp-providers collection when present"
    (let [cfgs [{:tool/id :a} {:tool/id :b}]]
      (is (= cfgs
             (cli-session/mcp-providers-from-config
               {:provider/registry
                {:providers [{:provider/type :fixture/echo}]
                 :mcp-providers cfgs}}))))))

(deftest valid-mcp-provider-is-registered
  (testing "registers a valid MCP provider config and stores its descriptor"
    (let [reg (registry/create-registry)
          system {:provider/registry reg}]
      (cli-session/register-mcp-providers! system [(dummy-mcp-cfg)])
      (let [entry (registry/lookup reg :test/mcp-tool)]
        (is (some? entry))
        (is (= :test/mcp-tool (get-in entry [:descriptor :tool/id])))
        (is (= :remote (get-in entry [:descriptor :effect])))))))

(deftest duplicate-mcp-provider-tool-id-is-rejected
  (testing "propagates :provider/duplicate-tool-id when the tool id is already registered"
    (let [reg (registry/create-registry)
          system {:provider/registry reg}]
      (cli-session/register-mcp-providers! system [(dummy-mcp-cfg)])
      (is (thrown? clojure.lang.ExceptionInfo
                   (cli-session/register-mcp-providers! system [(dummy-mcp-cfg)]))))))

(deftest missing-tool-id-rejected
  (testing "throws :provider/config-invalid when :tool/id is missing"
    (let [reg (registry/create-registry)
          system {:provider/registry reg}
          cfg (dissoc (dummy-mcp-cfg) :tool/id)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"MCP provider config requires :tool/id"
                            (cli-session/register-mcp-providers! system [cfg]))))))

(deftest missing-tool-mcp-name-rejected
  (testing "throws :provider/config-invalid when :tool/mcp-name is missing"
    (let [reg (registry/create-registry)
          system {:provider/registry reg}
          cfg (dissoc (dummy-mcp-cfg) :tool/mcp-name)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"MCP provider config requires :tool/mcp-name"
                            (cli-session/register-mcp-providers! system [cfg]))))))

(deftest non-map-mcp-provider-config-rejected
  (testing "throws :provider/config-invalid when a config entry is not a map"
    (let [reg (registry/create-registry)
          system {:provider/registry reg}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"MCP provider config must be a map"
                            (cli-session/register-mcp-providers! system ["bad"]))))))

(deftest build-system-wires-mcp-providers
  (testing "register-mcp-providers! is invoked from build-system when config carries :mcp-providers"
    (let [dir (str (java.nio.file.Files/createTempDirectory
                     "evoclj-cli-mcp-" (make-array java.nio.file.attribute.FileAttribute 0))
                   "/state")]
      (try
        (let [cfg {:state-dir dir
                    :overrides
                    {:provider/registry
                     {:providers [{:provider/type :fixture/echo}
                                  {:provider/type :fixture/non-idempotent}]
                      :mcp-providers [(dummy-mcp-cfg)]}
                     :evolution/system {:mutator :none}}}
              system (cli-session/build-system cfg)]
          (is (some? (registry/lookup (:provider/registry system) :test/mcp-tool)))
          (is (= :remote (get-in (registry/lookup (:provider/registry system) :test/mcp-tool)
                                 [:descriptor :effect]))))
        (finally
          (when (java.nio.file.Files/exists (java.nio.file.Paths/get dir (make-array String 0))
                                            (make-array java.nio.file.LinkOption 0))
            (clojure.java.io/delete-file
             (str (java.nio.file.Paths/get dir (make-array String 0)))
             true)))))))

(deftest build-system-skips-mcp-when-absent
  (testing "build-system does not error when :mcp-providers is absent"
    (let [dir (str (java.nio.file.Files/createTempDirectory
                     "evoclj-cli-mcp-" (make-array java.nio.file.attribute.FileAttribute 0))
                   "/state")]
      (try
        (let [cfg {:state-dir dir
                    :overrides
                    {:provider/registry
                     {:providers [{:provider/type :fixture/echo}
                                  {:provider/type :fixture/non-idempotent}]}
                     :evolution/system {:mutator :none}}}
              system (cli-session/build-system cfg)]
          (is (some? (:provider/registry system))))
        (finally
          (when (java.nio.file.Files/exists (java.nio.file.Paths/get dir (make-array String 0))
                                            (make-array java.nio.file.LinkOption 0))
            (clojure.java.io/delete-file
             (str (java.nio.file.Paths/get dir (make-array String 0)))
             true)))))))
