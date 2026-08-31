(ns evoclj.ptc.e2e-test
  "P11 e2e: sequential, branching, try/catch, limits, 1vs4 equivalence, EDN boundary.

  All tests are deterministic and go through the production PTC path:
  - SCI computation via evoclj.sci.computation/execute-code (closed host-surface, single limitsCheck)
  - CodeModeOrchestrator true loop via evoclj.runtime.orchestrator (pin stable, broker via pipeline)

  Non-goal: no src behavior change — tests only."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [evoclj.runtime.orchestrator :as orch]
            [evoclj.sci.computation :as comp]
            [evoclj.sci.boundary :as boundary]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.intent.pipeline :as pipeline]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Shared helpers (mirror codemode_true_loop_test pattern)
;; ---------------------------------------------------------------------------

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
             :options {:max-tool-rounds 4}}
   :budget {:wall-ms 1000}})

;; ---------------------------------------------------------------------------
;; 1. Sequential two tools via code
;; ---------------------------------------------------------------------------

(deftest sequential-two-tools-via-code
  (testing "code calling two tools sequentially executes both via toolFns"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
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
                                                                :tool/arguments {:code "(do (tool/echo {\"a\" 1}) (tool/echo {\"b\" 2}))"
                                                                                :language "clojure"}}]
                                                  :code {:language :clojure
                                                         :source "(do (tool/echo {\"a\" 1}) (tool/echo {\"b\" 2}))"}}
                                          :authorization {:decision :allow}})
                    pipeline/pipeline (fn [_ intent]
                                        (swap! echo-calls inc)
                                        {:result/status :ok
                                         :value (assoc (:args (:payload intent)) :echoed true :n @echo-calls)})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :ok (:outcome res)) "orchestration succeeds")
          (is (= 2 @echo-calls) "two broker crossings via pipeline")
          (is (= {"b" 2 :echoed true :n 2} (peek (:outputs res))) "last tool result wins")
          (is (= 2 (get-in res [:code-result :usage :tool-calls]))))))))

(deftest sequential-two-tools-via-computation-direct
  (testing "computation/execute-code sequential two calls returns last value"
    (let [c (comp/make-computation {})
          calls (atom 0)
          tool-fns {"echo" (fn [arg]
                             (swap! calls inc)
                             (assoc arg :n @calls))}]
      (let [res (comp/execute-code c "(do (tool/echo {\"a\" 1}) (tool/echo {\"b\" 2}))" tool-fns)]
        (is (= :ok (:status res)))
        (is (= {"b" 2 :n 2} (:value res)))
        (is (= 2 (:tool-calls (:usage res))))
        (is (= 2 @calls))))))

;; ---------------------------------------------------------------------------
;; 2. Branching if
;; ---------------------------------------------------------------------------

(deftest branching-if-selects-correct-tool
  (testing "if true selects first tool, if false selects second"
    (let [c (comp/make-computation {})
          tool-fns {"echo-a" (fn [_] {:which :a})
                    "echo-b" (fn [_] {:which :b})}]
      (let [res-true (comp/execute-code c "(if true (tool/echo-a {}) (tool/echo-b {}))" tool-fns)
            res-false (comp/execute-code c "(if false (tool/echo-a {}) (tool/echo-b {}))" tool-fns)]
        (is (= :ok (:status res-true)))
        (is (= {:which :a} (:value res-true)))
        (is (= 1 (:tool-calls (:usage res-true))))
        (is (= :ok (:status res-false)))
        (is (= {:which :b} (:value res-false)))
        (is (= 1 (:tool-calls (:usage res-false))))))))

(deftest branching-if-via-orchestrator
  (testing "orchestrator branching: code with if dispatches correct tool"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          tools [{:name "echo-a" :tool :echo-a :description "a"}
                 {:name "echo-b" :tool :echo-b :description "b"}]
          intent (model-intent tools)
          chosen (atom nil)]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id 1})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "ref")
                    dispatch/dispatch! (fn [_ _]
                                         {:result/status :ok
                                          :value {:model/output {:text "hi"}
                                                  :tool-calls [{:tool/call-id "call_1"
                                                                :tool/name "code_execution"
                                                                :tool/arguments {:code "(if true (tool/echo-a {:x 1}) (tool/echo-b {:x 2}))"
                                                                                :language "clojure"}}]
                                                  :code {:language :clojure
                                                         :source "(if true (tool/echo-a {:x 1}) (tool/echo-b {:x 2}))"}}
                                          :authorization {:decision :allow}})
                    pipeline/pipeline (fn [_ intent]
                                        (let [tid (get-in intent [:payload :tool/id])
                                              args (get-in intent [:payload :args])]
                                          (reset! chosen tid)
                                          {:result/status :ok :value {:chosen tid :args args}}))]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :ok (:outcome res)))
          (is (= :echo-a @chosen) "branch true chose echo-a")
          (is (= 1 (get-in res [:code-result :usage :tool-calls]))))))))

;; ---------------------------------------------------------------------------
;; 3. try/catch with tool failure — try is not in allow set, so fail-closed
;; ---------------------------------------------------------------------------

(deftest try-catch-with-tool-failure-caught
  (testing "try is not allowed in PTC host-surface => code rejected fail-closed (cannot catch)"
    (let [c (comp/make-computation {})
          tool-fns {"failing" (fn [_]
                                (throw (err/error :tool/failure "tool boom" {:reason :boom})))}]
      (let [res (comp/execute-code c "(try (tool/failing {}) (catch Exception e :caught))" tool-fns)]
        (is (= :error (:status res)) "try is not allowed => :error")
        (is (some? (get-in res [:error :error/type])))))))

(deftest try-catch-with-tool-failure-uncaught
  (testing "tool failure uncaught surfaces as :error with :tool/failure"
    (let [c (comp/make-computation {})
          tool-fns {"failing" (fn [_]
                                (throw (err/error :tool/failure "tool boom" {:reason :boom})))}]
      (let [res (comp/execute-code c "(tool/failing {})" tool-fns)]
        (is (= :error (:status res)))
        (is (= :tool/failure (get-in res [:error :error/type])))
        (is (= 1 (:tool-calls (:usage res))))))))

(deftest try-catch-with-tool-failure-via-orchestrator
  (testing "orchestrator try/catch: try not in allow set => code rejected fail-closed"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          intent (model-intent [{:name "echo" :tool :echo :description "echo"}])]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id 1})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "ref")
                    dispatch/dispatch! (fn [_ _]
                                         {:result/status :ok
                                          :value {:model/output {:text "hi"}
                                                  :tool-calls [{:tool/call-id "call_1"
                                                                :tool/name "code_execution"
                                                                :tool/arguments {:code "(try (tool/echo {:x 1}) (catch Exception e :caught))"
                                                                                :language "clojure"}}]
                                                  :code {:language :clojure
                                                         :source "(try (tool/echo {:x 1}) (catch Exception e :caught))"}}
                                          :authorization {:decision :allow}})
                    pipeline/pipeline (fn [_ _]
                                        {:result/status :ok :value {:echoed true}})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :failed (:outcome res)) "try not allowed => :failed")
          (is (= :error (get-in res [:code-result :status]))))))))

;; ---------------------------------------------------------------------------
;; 4. Limits: 32 ok 33 fail, codeBytes over fail
;; ---------------------------------------------------------------------------

(deftest limits-32-ok
  (testing "32 tool calls is within limit 32 => :ok"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          code-str (str "(do " (apply str (repeat 32 "(tool/echo {\"x\" 1}) ")) ")")]
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
          (is (= :ok (:outcome res)))
          (is (= :ok (get-in res [:code-result :status])))
          (is (= 32 (get-in res [:code-result :usage :tool-calls]))))))))

(deftest limits-33-fail
  (testing "33 tool calls exceeds limit 32 => :sci/limit-exceeded"
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
          (is (= :failed (:outcome res)))
          (is (= :error (get-in res [:code-result :status])))
          (is (= :sci/limit-exceeded (get-in res [:code-result :error :error/type])))
          (is (= :max-tool-calls (get-in res [:code-result :error :error/data :limit])))
          (is (= :sci/limit-exceeded (:error/type (peek (:outputs res))))))))))

(deftest code-bytes-over-limit-fails
  (testing "codeBytes over 8192 => :sci/limit-exceeded :code-bytes"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          big-code (apply str (repeat 8200 "x"))]
      (with-redefs [evoclj.runtime.orchestrator/append-event! (fn [_ _ _ _ _ _] {:event/id 1})
                    evoclj.runtime.orchestrator/put-payload! (fn [_ _] "ref")
                    dispatch/dispatch! (fn [_ _]
                                         {:result/status :ok
                                          :value {:model/output {:text "hi"}
                                                  :tool-calls [{:tool/call-id "call_1"
                                                                :tool/name "code_execution"
                                                                :tool/arguments {:code big-code :language "clojure"}}]
                                                  :code {:language :clojure :source big-code}}
                                          :authorization {:decision :allow}})
                    pipeline/pipeline (fn [_ intent]
                                        {:result/status :ok :value (:args (:payload intent))})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :failed (:outcome res)))
          (is (= :error (get-in res [:code-result :status])))
          (is (= :sci/limit-exceeded (get-in res [:code-result :error :error/type])))
          (is (= :code-bytes (get-in res [:code-result :error :error/data :limit]))))))))

(deftest code-bytes-direct-computation
  (testing "direct computation codeBytes over also fails"
    (let [c (comp/make-computation {})
          big (apply str (repeat 8200 "x"))
          res (comp/execute-code c big {})]
      (is (= :error (:status res)))
      (is (= :sci/limit-exceeded (get-in res [:error :error/type])))
      (is (= :code-bytes (get-in res [:error :error/data :limit]))))))

;; ---------------------------------------------------------------------------
;; 5. 1 roundtrip vs 4 roundtrips equivalence (same final output)
;; ---------------------------------------------------------------------------

(deftest one-roundtrip-vs-four-roundtrips-equivalence
  (testing "one code_execution with 4 tool calls equals four separate tool-call roundtrips"
    (let [c (comp/make-computation {})
          tool-fns {"echo" (fn [arg] {:v (:v arg) :echoed true})}
          one-code "(let [a (tool/echo {:v 1}) b (tool/echo {:v 2}) c (tool/echo {:v 3}) d (tool/echo {:v 4})] (+ (:v a) (:v b) (:v c) (:v d)))"
          one-res (comp/execute-code c one-code tool-fns)
          r1 (comp/execute-code c "(tool/echo {:v 1})" tool-fns)
          r2 (comp/execute-code c "(tool/echo {:v 2})" tool-fns)
          r3 (comp/execute-code c "(tool/echo {:v 3})" tool-fns)
          r4 (comp/execute-code c "(tool/echo {:v 4})" tool-fns)
          four-sum (+ (:v (:value r1)) (:v (:value r2)) (:v (:value r3)) (:v (:value r4)))]
      (is (= :ok (:status one-res)))
      (is (= 10 (:value one-res)) "one roundtrip sum is 10")
      (is (= :ok (:status r1)))
      (is (= :ok (:status r2)))
      (is (= :ok (:status r3)))
      (is (= :ok (:status r4)))
      (is (= 10 four-sum) "four roundtrips sum is also 10")
      (is (= (:value one-res) four-sum) "equivalence: same final output"))))

(deftest one-vs-four-via-orchestrator
  (testing "orchestrator one code roundtrip with 4 toolFns equals same result as 4 traditional tool calls"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          code-str "(let [a (tool/echo {:v 1}) b (tool/echo {:v 2}) c (tool/echo {:v 3}) d (tool/echo {:v 4})] (+ (:v a) (:v b) (:v c) (:v d)))"]
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
                                        (let [v (get-in intent [:payload :args :v])]
                                          {:result/status :ok :value {:v v}}))]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :ok (:outcome res)))
          (is (= 10 (peek (:outputs res))) "one code roundtrip yields 10")
          (is (= 10 (:value (:code-result res))) "code-result also 10")
          (is (= 4 (get-in res [:code-result :usage :tool-calls]))))))))

;; ---------------------------------------------------------------------------
;; 6. materialize-edn rejects Java object via tool
;; ---------------------------------------------------------------------------

(deftest materialize-edn-rejects-java-object-via-tool
  (testing "tool returning Java object is rejected by EDN boundary"
    (let [c (comp/make-computation {})
          tool-fns {"bad" (fn [_] (java.io.File. "/tmp/foo"))}
          res (comp/execute-code c "(tool/bad {})" tool-fns)]
      (is (= :error (:status res)))
      (is (= :edn/unsupported (get-in res [:error :error/type])))
      (is (str/includes? (str (get-in res [:error :error/data :reason])) "java") "reason mentions java object"))))

(deftest materialize-edn-rejects-java-object-direct-return
  (testing "code returning Java object directly is also rejected (or not allowed at parse time)"
    (let [c (comp/make-computation {})
          res (comp/execute-code c "(Object.)" {})]
      (is (= :error (:status res)) "direct Java interop not in allow set => error"))))

(deftest materialize-edn-rejects-java-object-via-orchestrator
  (testing "orchestrator code returning Java object via tool is fail-closed"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          bad-tool {:name "bad" :tool :bad :description "bad"}
          intent (model-intent [bad-tool])
          code-str "(tool/bad {})"]
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
                    pipeline/pipeline (fn [_ _]
                                        {:result/status :ok :value (java.io.File. "/tmp/pwn")})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :failed (:outcome res)))
          (is (= :error (get-in res [:code-result :status])))
          (is (= :edn/unsupported (get-in res [:code-result :error :error/type]))))))))
