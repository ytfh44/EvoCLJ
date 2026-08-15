(ns evoclj.eval.eval-llm-e2e-test
  "Offline end-to-end proof that the G5 paired evaluator REALLY runs
  a Genome whose topology contains an :llm node through a real model
  provider, threading the kernel-owned :model/registry + a model lease
  into the runner's broker context (evoclj.eval.runner post-v0
  extension 1).

  FULLY OFFLINE: a local com.sun.net.httpserver.HttpServer serves a
  canned chat-completions JSON (plain assistant text — the llm node
  needs no tool calls). The :llm node -> :emit topology mirrors the
  tool-genome in evoclj.runtime.tool-loop-e2e-test; the evaluator's
  :provider/catalog resolves the models.edn alias to the fake
  lmstudio/fake model id that the offline model registry keys on.

  The two tests bracket the new contract:

  - eval-runs-llm-nodes-through-real-models : a paired run with
    :model/registry + :model/resource grants the model lease
    {:kind :model :id \"lmstudio/*\"} so the llm node's
    :intent/model-call really dispatches through the broker to the
    fake endpoint (requests >= 2 — one per side), both sides
    :completed, and the canned text reaches the emitted output.
  - eval-fails-closed-without-model-registry : the same evaluator
    minus :model/registry injects NO :model-registry and NO model
    lease, so the llm node's model call fails closed with
    :provider/not-found :reason :no-model-registry BEFORE any provider
    executes — the fake endpoint receives ZERO requests and no canned
    text ever reaches the output. There is no silent fallback: an
    :llm topology simply cannot produce model output without the
    registry."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.paired :as paired]
            [evoclj.provider.model-registry :as mreg])
  (:import (com.sun.net.httpserver HttpServer HttpHandler)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)))

;; --- the canned model response ----------------------------------------------

(def ^:private canned-text
  "The canned assistant text the fake endpoint serves; the llm node
  feeds it to the emit node, so it lands in the emitted side output."
  "hello from fake model: eval-llm-e2e")

(defn- canned-response
  "A chat-completions body carrying only assistant content (no tool
  calls) — the llm node -> emit path needs no tool round."
  []
  (json/generate-string
   {:id "c-eval-llm"
    :choices [{:index 0
               :message {:role "assistant" :content canned-text}
               :finish_reason "stop"}]
    :usage {:prompt_tokens 10 :completion_tokens 6}}))

(defn- start-fake-endpoint
  "Serve the canned chat-completions on an ephemeral port. Returns
  {:server :base-url :requests} (requests = atom of request bodies)."
  []
  (let [server (HttpServer/create (InetSocketAddress. 0) 0)
        requests (atom [])]
    (.createContext server "/chat/completions"
                    (reify HttpHandler
                      (handle [_ exchange]
                        (let [body (slurp (.getRequestBody exchange))
                              _ (swap! requests conj body)
                              resp (canned-response)
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
  "A genome bundle whose topology is a plain :llm node (alias :planner)
  feeding the :emit node — no tools. The models.edn alias resolves
  through the provider catalog (:reasoning/high -> lmstudio/fake). All
  modules are written as pr-str EDN (EDN-safe, Global Constraint 22)."
  []
  (let [dir (java.nio.file.Files/createTempDirectory
             "evoclj-eval-llm-genome-" (make-array java.nio.file.attribute.FileAttribute 0))
        root (str dir)]
    (write-file! (str root "/manifest.edn")
                 (pr-str {:genome/format 1
                          :agent/id :llm-eval
                          :agent/entry :graph/llm
                          :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                          :modules {:topology "topology.edn"
                                    :models "models.edn"
                                    :memory "memory.edn"
                                    :evolution "evolution.edn"}
                          :capabilities/requested #{:model/call}
                          :evolution {:max-risk :behavioral :mutable #{:parameters}}
                          :metadata {:name "eval-llm-fixture"}}))
    (write-file! (str root "/topology.edn")
                 (pr-str {:graph/id :graph/llm
                          :entry :node/llm
                          :nodes {:node/llm {:node/type :llm
                                             :model :planner
                                             :next :node/emit}
                                  :node/emit {:node/type :emit}}
                          :limits {:max-steps 16}}))
    (write-file! (str root "/models.edn")
                 (pr-str {:models {:planner {:alias :reasoning/high}}}))
    (write-file! (str root "/memory.edn") (pr-str {:memory/seed {}}))
    (write-file! (str root "/evolution.edn") (pr-str {:evolution/enabled false}))
    root))

;; --- offline model registry (host-config conventions) -----------------------

(defn- model-idx-entry
  "The models.dev catalog index entry for the fake lmstudio model."
  [base-url]
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

(defn- offline-model-registry
  "Build the model registry atom over the fake lmstudio model with an
  explicit api key (no environment read needed)."
  [base-url]
  (mreg/build-model-registry
   (model-idx-entry base-url)
   {:registry/api-keys {:lmstudio "lm-studio"}}))

(defn- provider-catalog
  "The evaluator's :provider/catalog — resolves the models.edn :planner
  alias to the fake lmstudio/fake model the registry keys on."
  []
  {:reasoning/high {:provider :lmstudio
                    :provider-model "lmstudio/fake"
                    :adapter-version "1"}})

;; --- evaluator / request helpers --------------------------------------------

(defn- selection-case
  "ONE selection case for the paired run: an empty :tools set (the llm
  topology calls no fixtures), a task-input prompt the llm node turns
  into its user message, and a placeholder oracle (the output is not
  asserted on score)."
  []
  {:case/id :sel/llm
   :task-input {:op :ask :text "give me the canned text"}
   :expected-output []
   :tools #{}})

(defn- evaluator
  "A minimal valid paired evaluator for an :llm topology. model-cfg is
  nil (no model execution — fail closed) or a map carrying
  :model/registry and :model/resource."
  [root model-cfg]
  (merge {:provider/catalog (provider-catalog)
          :selection/cases {:sel/llm (selection-case)}
          :selection/fixtures {}
          :genome/roots {"P1" root "C1" root}}
         model-cfg))

(defn- request
  []
  {:parent-generation "P1"
   :candidate-id "C1"
   :case-set [:sel/llm]
   :repetitions 1})

;; --- the tests ---------------------------------------------------------------

(deftest eval-runs-llm-nodes-through-real-models
  (let [{:keys [server base-url requests]} (start-fake-endpoint)
        _ (swap! servers conj server)
        root (llm-genome)
        registry (offline-model-registry base-url)
        ev (evaluator root
                      {:model/registry registry
                       :model/resource {:kind :model :id "lmstudio/*"}})
        result (paired/run-paired-selection! ev (request))
        pair (first (:pairs result))
        parent-side (get-in pair [:sides :parent])
        candidate-side (get-in pair [:sides :candidate])]
    (testing "the paired run really dispatched a model call per side"
      (is (>= (count @requests) 2)
          (str "expected >=2 model calls (one per side), got "
               (count @requests))))
    (testing "every side that ran completed"
      (is (= :completed (:side/status parent-side)))
      (is (= :completed (:side/status candidate-side))))
    (testing "the canned text reached the emitted output"
      (doseq [side [parent-side candidate-side]]
        (is (str/includes? (pr-str (:side/outputs side)) canned-text)
            (str "side " (:side/id side) " output reflects the canned text"))))))

(deftest eval-fails-closed-without-model-registry
  (let [{:keys [server base-url requests]} (start-fake-endpoint)
        _ (swap! servers conj server)
        root (llm-genome)
        ;; NO :model/registry, NO :model/resource — real model execution
        ;; is simply not wired for this evaluator.
        ev (evaluator root nil)
        result (paired/run-paired-selection! ev (request))
        parent-side (get-in (first (:pairs result)) [:sides :parent])
        candidate-side (get-in (first (:pairs result)) [:sides :candidate])]
    (testing "the fake endpoint was NEVER contacted — no provider executes"
      (is (zero? (count @requests))
          "without :model/registry the llm model call fails closed before any HTTP call"))
    (testing "no canned model text ever reaches the side output"
      (doseq [side [parent-side candidate-side]]
        (is (not (str/includes? (pr-str (:side/outputs side)) canned-text)))))
    (testing "the runner fails closed — no silent fallback"
      ;; The :intent/model-call is recorded as an :intent/failed event
      ;; carrying :error/type :provider/not-found and
      ;; :reason :no-model-registry (nothing executes — zero HTTP calls;
      ;; there is no silent fixture fallback). The scheduler lets the
      ;; :emit node complete, so the side is :completed with EMPTY
      ;; outputs; the meaningful assertions are the zero request count
      ;; and the absence of any model-derived text.
      (is (not (seq (:side/outputs parent-side)))
          "parent side produced no model output")
      (is (not (seq (:side/outputs candidate-side)))
          "candidate side produced no model output"))))

