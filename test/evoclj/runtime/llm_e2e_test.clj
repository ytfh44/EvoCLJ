(ns evoclj.runtime.llm-e2e-test
  "Post-v0 extension 1 e2e: a Genome with a real :llm node runs end to
  end through the REAL pipeline against a LOCAL FAKE OpenAI-compatible
  endpoint:

    load the llm-fixture Genome
      -> compile (Resolution maps :planner -> deepseek/deepseek-v4-flash)
      -> instantiate the Phenotype
      -> create a pinned session
      -> run-session! (scheduler) with a :model lease
      -> broker dispatch of :intent/model-call through the model
         registry to the fake endpoint
      -> completed session with the model output

  The fake endpoint is a local HttpServer speaking the chat-completions
  wire dialect (including DeepSeek-style reasoning_content), so the
  whole real-model path — adapter, dialect layer, registry, dispatch,
  lease, llm node — is exercised with zero network and zero keys."
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [evoclj.compiler.core :as core]
            [evoclj.genome.load :as load]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.model-registry :as mreg]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- the fake model endpoint --------------------------------------------------

(def ^:private fake-response
  "{\"id\":\"x\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"hello from the fake model\",\"reasoning_content\":\"deep thought\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":8,\"completion_tokens\":5,\"total_tokens\":13}}")

(defn- start-fake-endpoint
  "Start a local fake chat/completions server; returns
  {:server <HttpServer> :base-url <str>}."
  []
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)]
    (.createContext server "/chat/completions"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [bytes (.getBytes fake-response "UTF-8")]
                          (.sendResponseHeaders exchange 200 (count bytes))
                          (with-open [os (.getResponseBody exchange)]
                            (.write os bytes))))))
    (.start server)
    {:server server
     :base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))}))

(def ^:private servers (atom []))

(use-fixtures :each
  (fn [f]
    (reset! servers [])
    (f)
    (doseq [s @servers] (.stop s 0))))

;; --- temp stores ---------------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-llm-e2e-" ".db" (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir []
  (let [d (Files/createTempDirectory "evoclj-llm-e2e-cas-" (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree! [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup! []
  (doseq [p @temp-paths] (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

;; --- the run -------------------------------------------------------------------

(def ^:private generation-id "generation-1")

(def ^:private provider-catalog
  {:reasoning/high {:provider :deepseek
                    :provider-model "deepseek/deepseek-v4-flash"
                    :adapter-version "1"}})

(defn- model-index-for
  "A one-model models.dev index pointing at the fake endpoint."
  [base-url]
  {"deepseek/deepseek-v4-flash"
   {:model/id "deepseek/deepseek-v4-flash"
    :model/provider :deepseek
    :model/style :openai-compatible
    :model/status :supported
    :model/base-url base-url
    :model/api-key-env "DEEPSEEK_API_KEY"
    :model/dialect {:interleaved :reasoning_content
                    :reasoning-options [{:type :toggle}]
                    :server-side-search :off
                    :extra-params {}}
    :model/cost {:input 0.14 :output 0.28}}})

(defn- model-lease
  [phenotype-id]
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
     :resource {:kind :model :id "deepseek/*"}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(deftest llm-genome-runs-end-to-end
  (let [{:keys [server base-url]} (start-fake-endpoint)
        _ (swap! servers conj server)
        root (.toPath (io/file "test/fixtures/modelsdev-genome"))
        loaded (assoc (load/load-genome root) :programs [])
        compiled (core/compile-genome loaded provider-catalog)
        genome-id (:compiled/genome-id compiled)
        resolution-id (:compiled/resolution-id compiled)
        phenotype-id (:compiled/phenotype-id compiled)
        db-path (temp-db-path)
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        _ (do
            (doseq [[artifact-id media-type]
                    [[genome-id "application/octet-stream"]
                     [resolution-id "application/edn"]
                     [phenotype-id "application/edn"]]]
              (artifact/ensure-artifact! db artifact-id media-type 0))
            (artifact/ensure-genome! db genome-id))
        _ (sqlite/with-db [conn db]
            (jdbc/insert! conn :generations
                          {:id generation-id
                           :genome_id genome-id
                           :resolution_id resolution-id
                           :parent_id nil
                           :state "active"
                           :current 1
                           :created_at "2025-01-01T00:00:00Z"}))
        cas-root (temp-cas-dir)
        cas-store (cas/->cas cas-root)
        reg (registry/create-registry)
        model-reg (mreg/build-model-registry
                   (model-index-for base-url)
                   {:registry/api-keys {:deepseek "sk-test"}})
        usage (atom {})
        lease (model-lease phenotype-id)
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases [lease] :usage usage}
             :program-sources {}})
        executor {:phenotype ph
                  :stores {:sqlite db :cas cas-store}
                  :dispatch (dispatch/make-broker-context
                             {:registry reg
                              :model-registry model-reg
                              :leases [lease]
                              :usage usage})}
        sid (:session/id
             (session/create-session!
              db
              {:genome/id genome-id
               :resolution/id resolution-id
               :phenotype/id phenotype-id
               :generation/id generation-id}))
        _ (event/append-event! db
                               {:session/id sid
                                :generation/id generation-id
                                :phenotype/id phenotype-id
                                :event/type :session/created
                                :prev/event-id nil
                                :payload-ref nil
                                :metadata {}})
        result (scheduler/run-session! executor sid {:op :ask :text "hi"})]
    (is (= :completed (:status result)))
    (is (some? (:output-ref result)))
    (let [output (String. ^bytes (cas/get-bytes cas-store (:output-ref result))
                          StandardCharsets/UTF_8)]
      (is (str/includes? output "hello from the fake model"))
      (is (str/includes? output "deep thought")))))
