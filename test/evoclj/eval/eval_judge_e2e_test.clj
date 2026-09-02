(ns evoclj.eval.eval-judge-e2e-test
  "Feature V1 e2e: a selection case declaring :output/equiv?
  :equivalence/llm-judge is scored by a REAL judge that dispatches
  through the broker to the fake OpenAI-compatible endpoint. The
  same endpoint serves the :llm node's task text and the judge's
  equivalent verdict, so the request log proves both ran."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.judge :as judge]
            [evoclj.eval.paired :as paired]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.model-registry :as mreg]
            [evoclj.store.cas :as cas])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

;; --- the fake endpoint: task text OR judge verdict --------------------------

(defn- canned-task-text
  "The canned assistant text the :llm node emits (the judge then
  judges it equivalent)."
  []
  "hello from model")

(defn- judge-verdict-json
  "The judge's canned verdict."
  []
  (json/generate-string {:equivalent true}))

(defn- start-fake-endpoint
  "Serve canned chat-completions: task text when the request carries
  the llm task prompt, the judge verdict when it carries the judge's
  EXPECTED-output rendering. Returns {:server :base-url :requests}.
  Judge requests contain 'EXPECTED output' (the judge user message)."
  []
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)
        requests (atom [])]
    (.createContext server "/chat/completions"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [body (slurp (.getRequestBody exchange))
                              _ (swap! requests conj body)
                              judge? (str/includes? body "EXPECTED output")
                              content (if judge?
                                        (judge-verdict-json)
                                        (canned-task-text))
                              resp (json/generate-string
                                    {:id "r1"
                                     :choices [{:index 0
                                                :message {:role "assistant"
                                                          :content content}
                                                :finish_reason "stop"}],
                                     :usage {:prompt_tokens 10 :completion_tokens 6}})
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
    (doseq [{:keys [server]} @servers] (when server (.stop server 0)))))

;; --- genome bundle: :llm node -> :emit --------------------------------------

(defn- write-file! [path content]
  (spit path content))

(defn- llm-genome
  "A genome bundle whose topology is a plain :llm node feeding :emit."
  []
  (let [dir (Files/createTempDirectory
             "evoclj-judge-genome-" (make-array FileAttribute 0))
        root (str dir)]
    (write-file! (str root "/manifest.edn")
                 (pr-str {:genome/format 1 :agent/id :judge-eval
                          :agent/entry :graph/llm
                          :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                          :modules {:topology "topology.edn"
                                    :models "models.edn"
                                    :memory "memory.edn"
                                    :evolution "evolution.edn"}
                          :capabilities/requested #{:model/call}
                          :evolution {:max-risk :behavioral
                                      :mutable #{:parameters}}
                          :metadata {:name "judge-eval-fixture"}}))
    (write-file! (str root "/topology.edn")
                 (pr-str {:graph/id :graph/llm :entry :node/llm
                          :nodes {:node/llm {:node/type :llm :model :planner
                                              :next :node/emit}
                                  :node/emit {:node/type :emit}}
                          :limits {:max-steps 16}}))
    (write-file! (str root "/models.edn")
                 (pr-str {:models {:planner {:alias :reasoning/high}}}))
    (write-file! (str root "/memory.edn") (pr-str {:memory/seed {}}))
    (write-file! (str root "/evolution.edn") (pr-str {:evolution/enabled false}))
    root))

;; --- model registry + broker wiring (the real dispatch path) -----------------

(defn- model-idx-entry [base-url]
  {"lmstudio/fake"
   {:model/id "lmstudio/fake"
    :model/provider :lmstudio
    :model/style :openai-compatible
    :model/status :supported
    :model/base-url base-url
    :model/api-key-env "LMSTUDIO_API_KEY"
    :model/dialect {:interleaved :none :reasoning-options []
                    :server-side-search :off :extra-params {}}
    :model/cost {:input 0 :output 0}}})

(defn- model-lease [phenotype-id]
  (let [now (Date.)]
    {:cap/id (UUID/randomUUID)
     :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
     :resource {:kind :model :id "lmstudio/*"}
     :actions #{:invoke}
     :constraints {:max-calls 1000}
     :issued-at now
     :expires-at (Date. (+ (.getTime now) 60000))}))

(defn- judge-model-call
  "A :model-call closure for the judge built exactly like the host's
  (kernel/system build-model-call): one :intent/model-call dispatched
  through a local broker context with the registry and lease injected.
  Attribution is a fixed judge uuid (the host uses deterministic ids;

  the test uses a fixed uuid for the same reason)."
  [broker model-registry lease phenotype-id]
  (let [ctx (assoc broker :model-registry model-registry
                   :leases (conj (:leases broker) lease))
        session-id (UUID/fromString "00000000-0000-0000-0000-0000000000cc")]
    (fn [model-id messages options]
      (let [intent (intent/model-call
                    session-id phenotype-id :node/judge 0
                    {:model/id model-id :messages messages :options options}
                    {:wall-ms 1000 :max-steps 1})]
        (dispatch/dispatch! ctx intent)))))

(deftest judge-scores-sides-through-real-dispatch
  (testing "a case declaring :output/equiv? :equivalence/llm-judge is
            scored by a judge that dispatches through the broker to the
            fake endpoint — both sides score 1.0 and the endpoint saw
            the llm node calls AND the judge calls"
    (let [{:keys [server base-url requests]} (start-fake-endpoint)
          _ (swap! servers conj server)
          root (llm-genome)
          model-reg (mreg/build-model-registry
                     (model-idx-entry base-url)
                     {:registry/api-keys {:lmstudio "lm-studio"}})
          ;; the runner's broker (llm node execution)
          registry (evoclj.provider.registry/create-registry)
          broker (dispatch/make-broker-context
                  {:registry registry :leases [] :usage (atom {})})
          phen-id "sha256:0000000000000000000000000000000000000000000000000000000000000000"
          lease (model-lease phen-id)
          judge-fn (judge/llm-judge
                    {:model-call (judge-model-call broker model-reg lease phen-id)
                     :model/id "lmstudio/fake"})
          evaluator {:provider/catalog
                     {:reasoning/high {:provider :lmstudio
                                       :provider-model "lmstudio/fake"
                                       :adapter-version "1"}}
                     :selection/cases
                     {:sel/llm
                      {:case/id :sel/llm
                       :task-input {:op :ask :text "give me the canned text"}
                       :expected-output {:text "hello from model"}
                       :tools #{}
                       :output/equiv? :equivalence/llm-judge}}
                     :equivalence/by-keyword
                     (judge/merge-judge {} judge-fn)
                     :selection/fixtures {}
                     :genome/roots {"P1" root "C1" root}
                     :model/registry model-reg
                     :model/resource {:kind :model :id "lmstudio/*"}}
          request {:parent-generation "P1"
                   :candidate-id "C1"
                   :case-set [:sel/llm]
                   :repetitions 1
                   :seed "judge-e2e"}
          result (paired/run-paired-selection! evaluator request)
          reqs @requests]
      (is (= 1.0 (get-in result [:parent :score])) "parent judged equivalent")
      (is (= 1.0 (get-in result [:candidate :score])) "candidate judged equivalent")
      (is (>= (count reqs) 4) "two llm-node calls + two judge calls")
      (is (= 2 (count (filter #(str/includes? % "EXPECTED output") reqs)))
          "exactly two judge calls (one per side)"))))

(deftest judge-without-registry-fails-closed
  (testing "a case opting into :equivalence/llm-judge WITHOUT the
            keyword registered fails the pair with the typed
            :eval/paired-equiv-unknown error (no silent fallback)"
    (let [{:keys [server base-url]} (start-fake-endpoint)
          _ (swap! servers conj server)
          root (llm-genome)
          evaluator {:provider/catalog
                     {:reasoning/high {:provider :lmstudio
                                       :provider-model "lmstudio/fake"
                                       :adapter-version "1"}}
                     :selection/cases
                     {:sel/llm
                      {:case/id :sel/llm
                       :task-input {:op :ask :text "give me the canned text"}
                       :expected-output {:text "hello"}
                       :tools #{}
                       :output/equiv? :equivalence/llm-judge}}
                     :selection/fixtures {}
                     :genome/roots {"P1" root "C1" root}}
          request {:parent-generation "P1"
                   :candidate-id "C1"
                   :case-set [:sel/llm]
                   :repetitions 1
                   :seed "judge-e2e"}
          e (try (paired/run-paired-selection! evaluator request) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :eval/paired-equiv-unknown (:error/type (ex-data e)))))))
