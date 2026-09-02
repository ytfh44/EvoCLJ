(ns evoclj.runtime.tool-loop-e2e-test
  "Post-v0 extension 1 e2e: the model tool-calling loop. A Genome
  with an :llm node that declares a fixture tool runs through the
  REAL pipeline against a local fake OpenAI-compatible endpoint that
  answers with tool_calls on the first turn and the final text after
  the tool result arrives. The scheduler executes the requested tool
  through the broker (fixture echo) and feeds the result back — the
  full loop: model -> tool -> model -> emit."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [evoclj.compiler.core :as core]
            [evoclj.genome.load :as load]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.model-registry :as mreg]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
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

;; --- the fake endpoint: first turn tool_calls, then the final text --------

(def ^:private tool-call-response
  "{\"id\":\"r1\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"echo_tool\",\"arguments\":\"{\\\"text\\\":\\\"hello from tool\\\"}\"}}]},\"finish_reason\":\"tool_calls\"}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":4}}")

(def ^:private final-response
  "{\"id\":\"r2\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"tool said: hello from tool\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":20,\"completion_tokens\":6}}")

(defn- start-fake-endpoint
  []
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)
        requests (atom [])]
    (.createContext server "/chat/completions"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [body (slurp (.getRequestBody exchange))
                              _ (swap! requests conj body)
                              has-tool-msg (str/includes? body "\"role\":\"tool\"")
                              resp (if has-tool-msg final-response tool-call-response)
                              bytes (.getBytes resp "UTF-8")]
                          (.sendResponseHeaders exchange 200 (count bytes))
                          (with-open [os (.getResponseBody exchange)]
                            (.write os bytes))))))
    (.start server)
    {:server server
     :base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
     :requests requests}))

(def ^:private servers (atom []))
(use-fixtures :each
  (fn [f]
    (reset! servers [])
    (f)
    (doseq [{:keys [server]} @servers] (.stop server 0))))

(def ^:private temp-paths (atom []))
(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-tool-e2e-" ".db" (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))
(defn- temp-cas-dir []
  (let [d (Files/createTempDirectory "evoclj-tool-e2e-cas-" (make-array FileAttribute 0))]
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

(def ^:private generation-id "generation-1")

(def ^:private provider-catalog
  {:reasoning/high {:provider :lmstudio
                    :provider-model "lmstudio/qwen-tool"
                    :adapter-version "1"}})

(defn- model-index-for [base-url]
  {"lmstudio/qwen-tool"
   {:model/id "lmstudio/qwen-tool"
    :model/provider :lmstudio
    :model/style :openai-compatible
    :model/status :supported
    :model/base-url base-url
    :model/api-key-env "LMSTUDIO_API_KEY"
    :model/dialect {:interleaved :none :reasoning-options []
                    :server-side-search :off :extra-params {}}
    :model/cost {:input 0 :output 0}}})

(defn- tool-genome
  "A genome bundle whose :llm node declares the echo_tool function
  tool mapping to :fixture/echo."
  []
  (let [dir (Files/createTempDirectory "evoclj-tool-genome-" (make-array FileAttribute 0))]
    (swap! temp-paths conj (str dir))
    (let [root (str dir)]
      (spit (str root "/manifest.edn")
            "{:genome/format 1 :agent/id :tool :agent/entry :graph/tool
             :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
             :modules {:topology \"topology.edn\" :models \"models.edn\"
                       :memory \"memory.edn\" :evolution \"evolution.edn\"}
             :capabilities/requested #{:model/call :tool/call}
             :evolution {:max-risk :behavioral :mutable #{:parameters}}
             :metadata {:name \"tool-fixture\"}}")
      (spit (str root "/topology.edn")
            "{:graph/id :graph/tool :entry :node/llm
             :nodes {:node/llm {:node/type :llm :model :planner :next :node/emit
                                :tools [{:name \"echo_tool\"
                                         :description \"echo the text back\"
                                         :tool :fixture/echo
                                         :parameters {:type \"object\"
                                                      :properties {:text {:type \"string\"}}}}]}
                     :node/emit {:node/type :emit}}
             :limits {:max-steps 16}}")
      (spit (str root "/models.edn") "{:models {:planner {:alias :reasoning/high}}}")
      (spit (str root "/memory.edn") "{:memory/seed {}}")
      (spit (str root "/evolution.edn") "{:evolution/enabled false}")
      root)))

(deftest tool-loop-runs-end-to-end
  (let [{:keys [server base-url requests]} (start-fake-endpoint)
        _ (swap! servers conj server)
        root (tool-genome)
        loaded (load/load-genome (java.nio.file.Paths/get root (make-array String 0)))
        compiled (core/compile-genome loaded provider-catalog)
        genome-id (:compiled/genome-id compiled)
        resolution-id (:compiled/resolution-id compiled)
        phenotype-id (:compiled/phenotype-id compiled)
        db-path (temp-db-path)
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        _ (sqlite/with-db [conn db]
            (doseq [artifact-id [genome-id resolution-id phenotype-id]]
              (jdbc/execute!
               conn
               ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
                 VALUES (?, 'application/octet-stream', 0, datetime('now'))"
                artifact-id]))
            (jdbc/execute!
             conn
             ["INSERT OR IGNORE INTO genomes (id, created_at)
              VALUES (?, datetime('now'))"
              genome-id])
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
        executions (atom 0)
        _ (registry/register! reg (fixture/echo-provider {:execution-count executions}))
        model-reg (mreg/build-model-registry
                   (model-index-for base-url)
                   {:registry/api-keys {:lmstudio "lm-studio"}})
        usage (atom {})
        lease (fn [resource]
                {:cap/id (random-uuid)
                 :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
                 :resource resource
                 :actions #{:invoke}
                 :constraints {:max-calls 10}
                 :issued-at (java.util.Date.)
                 :expires-at (java.util.Date. (+ (.getTime (java.util.Date.)) 60000))})
        model-lease (lease {:kind :model :id "lmstudio/*"})
        tool-lease (lease {:kind :tool :id :fixture/echo})
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases [model-lease tool-lease] :usage usage}
             :program-sources {}})
        executor {:phenotype ph
                  :stores {:sqlite db :cas cas-store}
                  :dispatch (dispatch/make-broker-context
                             {:registry reg
                              :model-registry model-reg
                              :leases [model-lease tool-lease]
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
        result (scheduler/run-session! executor sid {:op :ask :text "use the tool"})]
    (is (= :completed (:status result)))
    (is (some? (:output-ref result)))
    (let [output (String. ^bytes (cas/get-bytes cas-store (:output-ref result))
                          StandardCharsets/UTF_8)]
      (is (str/includes? output "tool said: hello from tool")))
    (is (= 1 @executions) "the fixture tool really ran exactly once")
    (is (= 2 (count @requests)) "two model turns: tool_calls then final")
    ;; the second request carries the assistant tool-call declaration
    ;; AND the tool result message
    (let [second-req (json/parse-string (second @requests) true)
          roles (mapv :role (:messages second-req))]
      (is (some #(= "tool" %) roles))
      (is (some #(= "assistant" %) roles)))
    ;; events: the loop is model-call -> tool-call -> model-call, each
    ;; through the broker's full effect protocol (the tool executes as
    ;; its own :intent/tool-call with authorization and provider events)
    (let [events (event/events-for-session db sid)
          authorized (filter #(= :intent/authorized (:event/type %)) events)]
      (is (= 3 (count authorized)) "model-call, tool-call, model-call")
      (is (= #{:intent/model-call :intent/tool-call}
             (set (map #(get-in % [:metadata :intent/type]) authorized)))
          "both the model call and the tool call are authorized intents")
      (is (= 3 (count (filter #(= :provider/call-completed (:event/type %)) events)))
          "two model turns plus the tool execution"))))
