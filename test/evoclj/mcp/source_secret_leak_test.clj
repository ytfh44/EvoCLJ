(ns evoclj.mcp.source-secret-leak-test
  "A stdio transport config's :env is a SECRET CHANNEL, and it must not
  survive an error boundary.

  Real stdio transports carry string-keyed env (mcp/transport.clj
  transmits them as strings), and the names are arbitrary vendor
  strings — \"GITHUB_TOKEN\", \"X-Custom-Credential\" — so no fixed secret
  name set can enumerate them. mcp/source.clj:618 hands err/sanitize the
  whole opts map with the transport config one level down, so it is the
  RECURSIVE :env rule in kernel/error.clj that closes the path, not the
  shallow manager/redact-transport."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.source :as mcp-source]))

(def ^:private secret-config
  {:type :stdio
   :command "node"
   :args ["server.js"]
   :env {"GITHUB_TOKEN" "ghp_x" "X-Custom-Credential" "s"}})

(defn- ex-data-of
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest missing-source-id-error-never-carries-the-env-secret
  (let [d (ex-data-of #(mcp-source/make-mcp-source
                        {:source/id nil
                         :transport-config secret-config}))]
    (is (= :mcp/config-invalid (:error/type d))
        "the missing :source/id is still the reported failure")
    (is (not (str/includes? (pr-str d) "ghp_x"))
        "the GITHUB_TOKEN value never reaches the terminal")
    (is (not (str/includes? (pr-str d) "X-Custom-Credential"))
        "nor does any vendor header NAME — the whole channel is dropped")
    (is (= "[REDACTED]" (get-in d [:opts :transport-config :env]))
        "the nested :env is redacted in place, not merely omitted")))

(deftest missing-transport-config-error-never-carries-the-env-secret
  (let [d (ex-data-of #(mcp-source/make-mcp-source
                        {:source/id :mcp/leak
                         :transport-config nil
                         :opts-secret secret-config}))]
    (is (= :mcp/config-invalid (:error/type d)))
    (is (not (str/includes? (pr-str d) "ghp_x"))
        "a sibling secret-bearing map is redacted on the same pass")))
