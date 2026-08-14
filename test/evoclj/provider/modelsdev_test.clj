(ns evoclj.provider.modelsdev-test
  "Tests for the models.dev catalog service (evoclj.provider.modelsdev).

  Uses a local com.sun.net.httpserver.HttpServer serving a small
  deterministic api.json fixture, so every test is offline and
  reproducible: fresh fetch, cache fallback, no-cache fail-closed,
  classification (npm styles + overrides + base-url tables),
  dialect markers, and lookup/filter behavior."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.kernel.error :as err]
            [evoclj.provider.modelsdev :as md])
  (:import (com.sun.net.httpserver HttpServer HttpExchange HttpHandler)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths Path)))

(def ^:private fixture-body
  (slurp "test/fixtures/modelsdev/api.json"))

(defn- start-fixture-server
  "Start an HttpServer serving the fixture api.json at /api.json;
  returns {:server ... :port ... :base-url ...}."
  []
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)]
    (.createContext server "/api.json"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [bytes (.getBytes fixture-body "UTF-8")]
                          (.sendResponseHeaders exchange 200 (count bytes))
                          (with-open [os (.getResponseBody exchange)]
                            (.write os bytes))))))
    (.start server)
    {:server server
     :port (.getPort (.getAddress server))
     :base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))}))

(defn- tmp-cache-dir
  "A fresh temp cache dir under test/tmp (cleaned by the fixture)."
  []
  (let [d (str "test/tmp-md-" (System/nanoTime))]
    (Files/createDirectories (Paths/get d (make-array String 0))
                             (make-array java.nio.file.attribute.FileAttribute 0))
    d))

(defn- delete-tree!
  "Recursively delete a directory (Windows-safe: walk first, then
  delete leaves)."
  [path-str]
  (let [root (Paths/get path-str (make-array String 0))]
    (when (Files/exists root (make-array java.nio.file.LinkOption 0))
      (let [walk (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
        (try
          (let [sorted (reverse (sort-by str (iterator-seq (.iterator walk))))]
            (doseq [p sorted] (Files/deleteIfExists p)))
          (finally (.close walk)))))))

(def ^:private cleanup-paths (atom []))

(use-fixtures :each
  (fn [f]
    (reset! cleanup-paths [])
    (f)
    (doseq [p @cleanup-paths] (delete-tree! p))))

(defn- catalog-config
  "Build a catalog config pointing at the given base-url with a fresh
  cache dir registered for cleanup."
  [base-url & {:keys [extra] :as _opts}]
  (let [dir (tmp-cache-dir)]
    (swap! cleanup-paths conj dir)
    (merge {:catalog/url (str base-url "/api.json")
            :catalog/cache-dir dir
            :catalog/ttl-hours 24
            :catalog/timeout-ms 5000}
           (or extra {}))))

(defn- throws-error-type?
  "True when (f) throws an ExceptionInfo whose :error/type equals
  type; false otherwise (including non-ExceptionInfo throws)."
  [type f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= type (:error/type (ex-data e))))))

(defn- throws-error-type?
  "True when (f) throws an ExceptionInfo whose :error/type equals
  type; false otherwise (including non-ExceptionInfo throws)."
  [type f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= type (:error/type (ex-data e))))))

(defn- index-of
  "The model index from a refresh result."
  [result]
  (get-in result [:catalog/data :catalog/models]))
(deftest refresh-fetches-and-indexes
  (testing "fresh fetch classifies every fixture model"
    (let [srv (start-fixture-server)
          result (md/refresh-catalog! (catalog-config (:base-url srv)))
          index (index-of result)]
      (try
        (is (= :catalog/fresh (:catalog/status result)))
        (is (= 4 (count index)))
        (let [ds (md/lookup-model index "deepseek/deepseek-v4-flash")]
          (is (= :openai-compatible (:model/style ds)))
          (is (= :supported (:model/status ds)))
          (is (= "https://api.deepseek.com" (:model/base-url ds)))
          (is (= "DEEPSEEK_API_KEY" (:model/api-key-env ds)))
          (is (= :reasoning_content (get-in ds [:model/dialect :interleaved])))
          (is (= [{:type :toggle} {:type :effort, :values ["low" "high" "max"]}]
                 (get-in ds [:model/dialect :reasoning-options])))
          (is (true? (get-in ds [:model/capabilities :reasoning])))
          (is (= {:context 1000000 :output 384000} (:model/limits ds))))
        (let [cl (md/lookup-model index "anthropic/claude-opus-4-7")]
          (is (= :anthropic (:model/style cl)))
          (is (= :supported (:model/status cl)))
          (is (= "ANTHROPIC_API_KEY" (:model/api-key-env cl))))
        (let [az (md/lookup-model index "azure/gpt-4o")]
          (testing "azure is an openai-compatible dialect needing an endpoint"
            (is (= :openai-compatible (:model/style az)))
            (is (= :needs-config (:model/status az)))
            (is (nil? (:model/base-url az)))))
        (let [sp (md/lookup-model index "someproprietary/x-1")]
          (testing "unknown style is honestly unsupported"
            (is (nil? (:model/style sp)))
            (is (= :unsupported (:model/status sp)))))
        (is (= 2 (count (md/list-models index :status :supported))))
        (is (= 1 (count (md/list-models index :status :needs-config))))
        (is (= 1 (count (md/list-models index :status :unsupported))))
        (is (= 1 (count (md/list-models index :style :anthropic))))
        (is (= 2 (count (md/list-models index :style :openai-compatible))))
      (finally (.stop (:server srv) 0))))))

(deftest cache-fallback-when-offline
  (testing "a dead source falls back to the cached copy"
    (let [srv (start-fixture-server)
          cfg (catalog-config (:base-url srv))
          fresh (md/refresh-catalog! cfg)]
      (is (= :catalog/fresh (:catalog/status fresh)))
      (.stop (:server srv) 0)
      (let [result (md/refresh-catalog! cfg)]
        (is (= :catalog/cached (:catalog/status result)))
        (is (= 4 (count (index-of result)))))))
  (testing "no cache and no network fails closed without throwing"
    (let [cfg (catalog-config "http://127.0.0.1:1")]
      (let [result (md/refresh-catalog! cfg)]
        (is (= :catalog/unavailable (:catalog/status result)))
        (is (some? (:catalog/error result)))))))

(deftest cache-write-read-roundtrip
  (testing "write-cache! then read-cache returns the same body"
    (let [dir (tmp-cache-dir)
          _ (swap! cleanup-paths conj dir)
          at (java.util.Date.)
          {:keys [catalog/cache-file]} (md/write-cache! dir fixture-body at "http://x/api.json")]
      (is (Files/exists (Paths/get cache-file (make-array String 0))
                        (make-array java.nio.file.LinkOption 0)))
      (let [cached (md/read-cache dir)]
        (is (= fixture-body (:catalog/body cached)))
        (is (= at (:catalog/fetched-at cached)))
        (is (= "http://x/api.json" (:catalog/source-url cached)))))))

(deftest parse-rejects-bad-catalog
  (testing "malformed JSON fails with :catalog/parse-invalid"
    (is (throws-error-type? :catalog/parse-invalid
                          (fn [] (md/parse-catalog "not json at all" (java.util.Date.))))))
  (testing "a provider entry with a non-vector env fails validation"
    (let [good-env-but-bad (str "{\"p1\":{\"id\":\"p1\",\"env\":\"nope\",\"models\":{}}}")]
      (is (throws-error-type? :catalog/parse-invalid
                            (fn [] (md/parse-catalog good-env-but-bad (java.util.Date.))))))))

(deftest config-validation
  (testing "a missing :catalog/url is rejected"
    (is (throws-error-type? :catalog/config-invalid
                          (fn [] (md/refresh-catalog! {:catalog/cache-dir "x"
                                                       :catalog/ttl-hours 1})))))
  (testing "an unknown model id is not found"
    (let [srv (start-fixture-server)
          result (md/refresh-catalog! (catalog-config (:base-url srv)))
          index (index-of result)]
      (try
        (is (nil? (md/lookup-model index "nope/nope")))
        (is (= [] (md/list-models index :provider :nope)))
        (finally (.stop (:server srv) 0))))))

(deftest operator-overrides
  (testing "operator base-urls/style/dialect overrides win"
    (let [srv (start-fixture-server)
          cfg (catalog-config (:base-url srv)
                              :extra {:catalog/base-urls {:azure "https://my-azure.openai.azure.com/openai"}
                                      :catalog/dialect-overrides {:deepseek {:server-side-search :web-search-tool}}})
          result (md/refresh-catalog! cfg)
          index (index-of result)]
      (try
        (let [az (md/lookup-model index "azure/gpt-4o")]
          (is (= :supported (:model/status az)))
          (is (= "https://my-azure.openai.azure.com/openai" (:model/base-url az))))
        (let [ds (md/lookup-model index "deepseek/deepseek-v4-flash")]
          (is (= :web-search-tool (get-in ds [:model/dialect :server-side-search]))))
        (finally (.stop (:server srv) 0))))))
