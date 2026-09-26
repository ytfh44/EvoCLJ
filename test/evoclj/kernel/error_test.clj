(ns evoclj.kernel.error-test
  "Tests for the typed error contract (component).

  Failures crossing EvoCLJ module boundaries must carry a stable
  machine-readable :error/type and must be fully serializable:
  no Throwable objects, class objects, lazy seqs, or functions
  (Global Constraint 22)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.kernel.error :as err]))

(deftest typed-error-is-data-readable
  (let [e (err/error :genome/invalid "bad genome" {:path "manifest.edn"})]
    (is (= :genome/invalid (:error/type (ex-data e))))
    (is (= "manifest.edn" (:path (ex-data e))))))

(deftest error-returns-exception-info
  (let [e (err/error :genome/io "missing file" {:path "x.edn"})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= "missing file" (ex-message e)))))

(deftest error-type-is-stable-over-data
  ;; A caller-supplied :error/type must not shadow the contract type.
  (let [e (err/error :genome/invalid "bad" {:error/type :user/claimed :path "x"})]
    (is (= :genome/invalid (:error/type (ex-data e))))
    (is (= "x" (:path (ex-data e))))))

(deftest error-data-is-serializable
  (let [e (err/error :genome/invalid "bad genome"
                     {:path "manifest.edn" :n 42 :tags #{:a :b} :v [1 2 3]})
        d (err/error-data e)]
    (is (= :genome/invalid (:error/type d)))
    (is (= "bad genome" (:error/message d)))
    (is (= {:path "manifest.edn" :n 42 :tags #{:a :b} :v [1 2 3]} (:error/data d)))
    (is (= d (edn/read-string (pr-str d))))))

(deftest error-data-excludes-unsafe-values
  ;; component 4: error-data must not contain a Throwable object, Java
  ;; class instance, lazy sequence, or function — and the result must
  ;; round-trip through pr-str/edn read-string.
  (let [e (err/error :kernel/unsafe "unsafe payload"
                     {:throwable (ex-info "inner" {:n 1})
                      :class-object String
                      :lazy-seq (map inc (range))       ; infinite lazy seq
                      :function (fn [x] x)
                      :nested {:deep (range 5)}})
        d (err/error-data e)
        nodes (tree-seq coll? seq d)]
    (is (= d (edn/read-string (pr-str d))))
    (is (not-any? #(instance? Throwable %) nodes))
    (is (not-any? class? nodes))
    (is (not-any? fn? nodes))
    (is (not-any? #(instance? clojure.lang.LazySeq %) nodes))
    ;; Unsafe values are transformed, not silently dropped.
    (is (= :evoclj.kernel.error/fn (:function (:error/data d))))
    (is (= 'java.lang.String (:class-object (:error/data d))))
    (is (= {:n 1} (:error/data (:throwable (:error/data d)))))
    (is (= [0 1 2 3 4] (:deep (:nested (:error/data d)))))))

(deftest error-data-of-plain-throwable
  (let [d (err/error-data (IllegalStateException. "boom"))]
    (is (= :error/unknown (:error/type d)))
    (is (= "boom" (:error/message d)))
    (is (= "java.lang.IllegalStateException" (:error/class d)))
    (is (= d (edn/read-string (pr-str d))))))

(deftest sanitize-redacts-secret-keys
  (testing "known secret keys are replaced with [REDACTED]"
    (let [data {:api-key "sk-123"
                :password "hunter2"
                :token "bearer abc"
                :safe "keep-me"
                :nested {:apiKey "secret" :host "localhost"}}
          result (err/sanitize data)]
      (is (= "[REDACTED]" (:api-key result)))
      (is (= "[REDACTED]" (:password result)))
      (is (= "[REDACTED]" (:token result)))
      (is (= "keep-me" (:safe result)))
      (is (= "[REDACTED]" (get-in result [:nested :apiKey])))
      (is (= "localhost" (get-in result [:nested :host]))))))

(deftest sanitize-redacts-transport-config-secrets
  (testing "the whole :env channel is redacted, whatever the vendor keys"
    ;; Real stdio transports carry STRING keys (mcp/transport.clj transmits
    ;; them as strings), and no fixed name set can enumerate arbitrary
    ;; vendor names — "GITHUB_TOKEN" lower-cases to :github_token, which is
    ;; not a member of any list. The channel itself is the secret.
    (let [cfg {:type :stdio
               :command "server"
               :env {"GITHUB_TOKEN" "ghp_x"
                     "X-Custom-Credential" "s"
                     "PATH" "/usr/bin"}}
          result (err/sanitize cfg)
          serialized (pr-str result)]
      (is (= "server" (:command result)))
      (is (not (str/includes? serialized "ghp_x"))
          "a real string-keyed token never survives sanitization")
      (is (not (str/includes? serialized "X-Custom-Credential"))
          "an unguessable vendor header name is covered too")
      (is (= "[REDACTED]" (:env result))))
  (testing ":headers is redacted on the same reasoning"
    (let [result (err/sanitize {:type :http
                                :headers {"X-Custom-Credential" "s"}})]
      (is (not (str/includes? (pr-str result) "X-Custom-Credential")))))
  (testing "a NESTED transport config is redacted — the recursion is what
            makes sanitize-map the real guard"
    ;; mcp/source.clj:618 hands err/sanitize the whole opts map, with the
    ;; transport config one level down. manager/redact-transport is shallow
    ;; and cannot reach this; only the recursive :env rule does.
    (let [opts {:source/id :mcp/x
                :transport-config {:type :stdio
                                   :command "server"
                                   :env {"GITHUB_TOKEN" "ghp_x"}}}
          result (err/sanitize opts)
          serialized (pr-str result)]
      (is (= :mcp/x (:source/id result)))
      (is (not (str/includes? serialized "ghp_x"))
          "a nested transport secret never reaches the terminal")
      (is (= "[REDACTED]"
             (get-in result [:transport-config :env])))))))

(deftest sanitize-redacts-string-key-secrets
  (testing "string-key headers like \"Authorization\" are also redacted"
    (let [cfg {"Authorization" "Bearer sk-secret"
               "X-Api-Key" "abc"
               "Content-Type" "application/json"
               :safe-key "keep"}
          result (err/sanitize cfg)]
      (is (= "[REDACTED]" (get result "Authorization")))
      (is (= "[REDACTED]" (get result "X-Api-Key")))
      (is (= "application/json" (get result "Content-Type")))
      (is (= "keep" (:safe-key result))))))
