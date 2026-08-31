(ns evoclj.ptc.codemode-true-loop-test
  "P10 true loop: CodeModeOrchestrator executes code via SandboxExecute with toolFns.
  Tests cover: plain code, echo sequential, 33 calls limit, codeBytes limit, disabled gate."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [evoclj.runtime.orchestrator :as orch]
            [evoclj.sci.computation :as comp]
            [evoclj.intent.pipeline :as pipeline]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]))

(defn- error-type [e]
  (:error/type (ex-data e)))

(def ^:private hex-a (apply str (repeat 64 "a")))
(def ^:private phenotype-id (str "sha256:" hex-a))

(defn- fake-executor []
  {:ptc {:enabled? true}
   :stores {:tool-catalog (atom []) :sqlite nil :cas nil}
   :dispatch {}})

(defn- pin []
  {:session/id (random-uuid)
   :phenotype/id phenotype-id
   :generation/id "g1"})

(defn- cause []
  {:event/id 1})

(defn- model-intent [tools]
  {:intent/type :intent/model-call
   :payload {:model/id "test/model"
             :messages [{:role :user :content "hi"}]
             :tools tools
             :options {:max-tool-rounds 0}}
   :budget {:wall-ms 1000}})

(deftest codemode-code-no-tool
  (testing "code (+ 1 2) with no tools evaluates to 3 via sandbox"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          intent (model-intent [])]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id 1})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "ref")
                    dispatch/dispatch! (fn [_ _]
                                         {:result/status :ok
                                          :value {:model/output {:text "hi"}
                                                  :tool-calls [{:tool/call-id "call_1"
                                                                :tool/name "code_execution"
                                                                :tool/arguments {:code "(+ 1 2)" :language "clojure"}}]
                                                  :code {:language :clojure :source "(+ 1 2)"}}
                                          :authorization {:decision :allow}})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :ok (:outcome res)) "sandbox ok should be :ok")
          (is (= 3 (peek (:outputs res))) "last output is code result 3")
          (is (= :ok (get-in res [:code-result :status])))
          (is (= 3 (get-in res [:code-result :value])))
          (is (map? (:tool-result res)))
          (is (= "call_1" (:tool-call-id (:tool-result res))))
          (is (str/includes? (:content (:tool-result res)) "3")))))))

(deftest codemode-code-calling-echo-sequential
  (testing "code calling echo tool sequentially — each toolFn crosses broker via pipeline"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          ;; declare echo so tool-map contains it
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          echo-calls (atom 0)]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id 1})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "ref")
                    dispatch/dispatch! (fn [_ _]
                                         {:result/status :ok
                                          :value {:model/output {:text "hi"}
                                                  :tool-calls [{:tool/call-id "call_1"
                                                                :tool/name "code_execution"
                                                                :tool/arguments {:code "(do (tool/echo {\"a\" 1}) (tool/echo {\"b\" 2}))" :language "clojure"}}]
                                                  :code {:language :clojure :source "(do (tool/echo {\"a\" 1}) (tool/echo {\"b\" 2}))"}}
                                          :authorization {:decision :allow}})
                    pipeline/pipeline (fn [_ intent]
                                        (swap! echo-calls inc)
                                        {:result/status :ok
                                         :value (assoc (:args (:payload intent)) :echoed true :n @echo-calls)})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :ok (:outcome res)))
          (is (= 2 @echo-calls) "two broker crossings via pipeline")
          (is (= {"b" 2 :echoed true :n 2} (peek (:outputs res))) "last tool echo wins")
          (is (= 2 (get-in res [:code-result :usage :tool-calls]))))))))

(deftest codemode-33-calls-limit-exceeded
  (testing "33 tool calls exceeds limit 32 -> :sci/limit-exceeded"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          code-str (str "(do " (apply str (repeat 33 "(tool/echo {\"x\" 1}) ")) ")")]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id 1})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "ref")
                    dispatch/dispatch! (fn [_ _]
                                         {:result/status :ok
                                          :value {:model/output {:text "hi"}
                                                  :tool-calls [{:tool/call-id "call_1"
                                                                :tool/name "code_execution"
                                                                :tool/arguments {:code code-str :language "clojure"}}]
                                                  :code {:language :clojure :source code-str}}
                                          :authorization {:decision :allow}})
                    pipeline/pipeline (fn [_ intent]
                                        {:result/status :ok :value (:args (:payload intent))})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :failed (:outcome res)) "limit exceeded should be :failed")
          (is (= :error (get-in res [:code-result :status])))
          (is (= :sci/limit-exceeded (get-in res [:code-result :error :error/type])))
          (is (= :max-tool-calls (get-in res [:code-result :error :error/data :limit])))
          ;; also outputs should contain error map
          (is (= :sci/limit-exceeded (:error/type (peek (:outputs res))))))))))

(deftest codemode-code-bytes-over-limit
  (testing "codeBytes over 8192 -> error"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          big (apply str (repeat 9000 "x"))
          intent (model-intent [])]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id 1})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "ref")
                    dispatch/dispatch! (fn [_ _]
                                         {:result/status :ok
                                          :value {:model/output {:text "hi"}
                                                  :tool-calls [{:tool/call-id "call_1"
                                                                :tool/name "code_execution"
                                                                :tool/arguments {:code big :language "clojure"}}]
                                                  :code {:language :clojure :source big}}
                                          :authorization {:decision :allow}})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :failed (:outcome res)))
          (is (= :error (get-in res [:code-result :status])))
          (is (= :sci/limit-exceeded (get-in res [:code-result :error :error/type])))
          (is (= :code-bytes (get-in res [:code-result :error :error/data :limit]))))))))

(deftest codemode-disabled-throws
  (testing "CodeMode disabled still throws :ptc/not-enabled"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor {:ptc {:enabled? false} :stores {:tool-catalog (atom [])} :dispatch {}}
          pi (pin)
          ca (cause)
          intent (model-intent [])]
      (try
        (orch/orchestrate cm executor pi ca intent [])
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e)))
          (is (= "PTC is disabled" (.getMessage e)))))))
  (testing "nil computation also fails safe"
    (let [cm (orch/->CodeModeOrchestrator nil)
          executor {:ptc {:enabled? true} :stores {:tool-catalog (atom [])} :dispatch {}}
          pi (pin)
          ca (cause)
          intent (model-intent [])]
      (try
        (orch/orchestrate cm executor pi ca intent [])
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :ptc/not-enabled (error-type e))))))))
