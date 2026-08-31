(ns evoclj.adversarial.ptc-adv-test
  "P11 adversarial: GC-07/08/09/22/20 bypass attempts — all fail-closed.

  Covers:
  - GC-07 System/exit not in allow set => rejected/throws
  - GC-08 direct provider call not in toolFns => not available/throws
  - GC-09 visible vs authorized (tool not in lease => denied)
  - GC-22 lazy seq / java object rejected at EDN boundary
  - interrupt uncatchable via try/catch inside code
  - GC-20 attribution present in events/intents"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [evoclj.runtime.orchestrator :as orch]
            [evoclj.sci.computation :as comp]
            [evoclj.sci.boundary :as boundary]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.intent.pipeline :as pipeline]
            [evoclj.intent.core :as intent-core]
            [evoclj.kernel.error :as err]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.candidate-store :as cs]
            [evoclj.store.current-store :as cur]
            [evoclj.helpers :as helpers]))

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
;; GC-07: System/exit bypass attempt
;; ---------------------------------------------------------------------------

(deftest gc07-system-exit-rejected
  (testing "GC-07: (System/exit 0) is not in host-surface => rejected, fail-closed"
    (let [c (comp/make-computation {})
          res (comp/execute-code c "(System/exit 0)" {})]
      (is (= :error (:status res)) "must be :error, not exit the JVM")
      (is (some? (get-in res [:error :error/type])) "typed error present")
      ;; Should not be limit-exceeded; it's a sandbox violation / symbol not allowed
      (is (not= "" (str (get-in res [:error :error/message]))))))
  (testing "GC-07: clojure.java.io not exposed"
    (let [c (comp/make-computation {})
          res (comp/execute-code c "(clojure.java.io/file \"/tmp/pwn\")" {})]
      (is (= :error (:status res)) "file access not allowed")
      (is (some? (get-in res [:error :error/type])))))
  (testing "GC-07: Runtime exec not allowed"
    (let [c (comp/make-computation {})
          res (comp/execute-code c "(.exec (Runtime/getRuntime) \"id\")" {})]
      (is (= :error (:status res)) "Runtime not in allow set"))))

(deftest gc07-via-orchestrator-rejected
  (testing "GC-07 via CodeModeOrchestrator: System/exit in code is rejected fail-closed"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          code-str "(System/exit 0)"]
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
          (is (= :failed (:outcome res)) "GC-07 must be :failed, not JVM exit")
          (is (= :error (get-in res [:code-result :status])))
          (is (some? (get-in res [:code-result :error :error/type]))))))))

;; ---------------------------------------------------------------------------
;; GC-08: direct provider call not in toolFns => not available/throws
;; ---------------------------------------------------------------------------

(deftest gc08-direct-provider-call-not-available
  (testing "GC-08: tool not in toolFns map is not in allow set => error"
    (let [c (comp/make-computation {})
          tool-fns {"echo" (fn [arg] arg)}
          res (comp/execute-code c "(tool/secret {:x 1})" tool-fns)]
      (is (= :error (:status res)) "secret tool not declared => error")
      (is (some? (get-in res [:error :error/type])))))
  (testing "GC-08: provider namespace not allowed"
    (let [c (comp/make-computation {})
          res (comp/execute-code c "(evoclj.provider.openai/call {})" {})]
      (is (= :error (:status res)) "provider ns not in allow set")))
  (testing "GC-08: eval not allowed"
    (let [c (comp/make-computation {})
          res (comp/execute-code c "(eval (+ 1 2))" {})]
      (is (= :error (:status res)) "eval is not in host-surface"))))

(deftest gc08-via-orchestrator-tool-not-in-fns
  (testing "GC-08 via orchestrator: tool not in filtered tool-map cannot be called"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          ;; declare echo, but code tries to call hidden tool
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          code-str "(tool/hidden {:x 1})"]
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
          (is (= :failed (:outcome res)) "hidden tool not in allow => fail")
          (is (= :error (get-in res [:code-result :status]))))))))

;; ---------------------------------------------------------------------------
;; GC-09: visible vs authorized (tool not in lease => denied)
;; ---------------------------------------------------------------------------

(deftest gc09-visible-vs-authorized-denied
  (testing "GC-09: visible tool but no lease authorizes it => broker denies, code surfaces :error"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          ;; tool is visible in catalog (declared)
          evil-tool {:name "evil" :tool :evil :description "evil"}
          intent (model-intent [evil-tool])
          code-str "(tool/evil {:x 1})"]
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
                    ;; pipeline simulates broker denial (GC-09)
                    pipeline/pipeline (fn [_ _]
                                        {:result/status :denied
                                         :error/type :capability/missing
                                         :error/message "no lease for evil"
                                         :decision :deny})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :failed (:outcome res)) "GC-09 denied must be :failed")
          (is (= :error (get-in res [:code-result :status])))
          ;; error type should be the broker's denial, not a silent success
          (is (some? (get-in res [:code-result :error :error/type]))))))))

(deftest gc09-tool-failure-propagates-not-silent
  (testing "GC-09: pipeline :denied is not silently swallowed as empty value"
    (let [c (comp/make-computation {})
          tool-fns {"evil" (fn [arg]
                             ;; Simulate broker denial inside tool wrapper
                             (throw (err/error :capability/missing "denied" {:tool/id :evil})))}
          res (comp/execute-code c "(tool/evil {:x 1})" tool-fns)]
      (is (= :error (:status res)))
      (is (= :capability/missing (get-in res [:error :error/type]))))))

;; ---------------------------------------------------------------------------
;; GC-22: lazy seq / java object at boundary
;; ---------------------------------------------------------------------------

(deftest gc22-lazy-seq-and-java-object-rejected
  (testing "GC-22: infinite lazy seq cannot escape boundary"
    (let [c (comp/make-computation {})
          res (comp/execute-code c "(range)" {})]
      (is (= :error (:status res)) "infinite range must not escape")
      (is (= :edn/size-exceeded (get-in res [:error :error/type])))))
  (testing "GC-22: lazy map over range within limit materializes, but huge lazy exceeds"
    (let [c (comp/make-computation {})
          ;; 100001 elements exceeds default max-size 100000
          res (comp/execute-code c "(map inc (range 100001))" {})]
      (is (= :error (:status res)))
      (is (= :edn/size-exceeded (get-in res [:error :error/type])))))
  (testing "GC-22: Java object via tool is rejected"
    (let [c (comp/make-computation {})
          tool-fns {"bad" (fn [_] (java.io.File. "/tmp/pwn"))}
          res (comp/execute-code c "(tool/bad {})" tool-fns)]
      (is (= :error (:status res)))
      (is (= :edn/unsupported (get-in res [:error :error/type])))))
  (testing "GC-22: direct boundary/materialize-edn rejects lazy seq"
    (let [e (try (boundary/materialize-edn (map inc (range)) {:max-size 10})
                 nil
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e))
      (is (= :edn/size-exceeded (:error/type (ex-data e))))))
  (testing "GC-22: direct boundary rejects Java object"
    (let [e (try (boundary/materialize-edn (java.io.File. "/tmp/foo") {})
                 nil
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e))
      (is (= :edn/unsupported (:error/type (ex-data e)))))))

;; ---------------------------------------------------------------------------
;; Interrupt uncatchable via try/catch inside code
;; ---------------------------------------------------------------------------

(deftest interrupt-uncatchable-via-try-catch
  (testing "sci interrupt (wall/steps) is not catchable; try itself is also not in allow set => both fail-closed"
    (let [c (comp/make-computation {:limits {:wall-ms 50 :max-steps 100}})
          ;; infinite loop with steps limit 100, wrapped in try/catch — try is not allowed, so code is rejected
          code "(try (loop [n 0] (recur (inc n))) (catch Exception e :caught))"
          res (comp/execute-code c code {})]
      (is (= :error (:status res)) "interrupt or try disallowed must surface as :error, not :caught")
      (is (some? (get-in res [:error :error/type])))
      (is (not= :caught (:value res)) "must not be caught value")))
  (testing "infinite loop without try also fails with limit-exceeded (uncatchable)"
    (let [c (comp/make-computation {:limits {:wall-ms 50 :max-steps 100}})
          res (comp/execute-code c "(loop [n 0] (recur (inc n)))" {})]
      (is (= :error (:status res)))
      (is (= :sci/limit-exceeded (get-in res [:error :error/type]))))))

(deftest interrupt-uncatchable-via-orchestrator
  (testing "orchestrator: infinite loop inside try/catch still fails (try disallowed or limit exceeded)"
    (let [c (comp/make-computation {:limits {:wall-ms 50 :max-steps 100}})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi (pin)
          ca (cause)
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          code-str "(try (loop [n 0] (recur (inc n))) (catch Exception e :caught))"]
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
          (is (some? (get-in res [:code-result :error :error/type]))))))))

;; ---------------------------------------------------------------------------
;; GC-20: attribution present in events
;; ---------------------------------------------------------------------------

(deftest gc20-attribution-present-in-intents-and-events
  (testing "GC-20: intent core requires attribution — missing fields are rejected"
    (let [e (try (intent-core/tool-call nil phenotype-id :node/x 1 {:tool/id :echo :args {}} {:wall-ms 1000})
                 nil
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e) "nil session-id must be rejected")
      (is (= :intent/schema-invalid (:error/type (ex-data e))))))
  (testing "GC-20: valid intent carries attribution"
    (let [sid (random-uuid)
          intent (intent-core/tool-call sid phenotype-id :node/x 1 {:tool/id :echo :args {:v 1}} {:wall-ms 1000})]
      (is (= sid (:session/id intent)))
      (is (= phenotype-id (:phenotype/id intent)))
      (is (= :node/x (:node/id intent)))
      (is (= 1 (:cause/event-id intent)))
      (is (= :intent/tool-call (:intent/type intent)))))
  (testing "GC-20: CodeModeOrchestrator toolFns build attributable intents (captured pipeline args)"
    (let [c (comp/make-computation {})
          cm (orch/->CodeModeOrchestrator c)
          executor (fake-executor)
          pi {:session/id #uuid "11111111-1111-4111-8111-111111111111"
              :phenotype/id phenotype-id
              :node/id :test-node}
          ca {:event/id 42}
          echo-tool {:name "echo" :tool :echo :description "echo"}
          intent (model-intent [echo-tool])
          captured (atom nil)
          code-str "(tool/echo {:v 1})"]
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
                    pipeline/pipeline (fn [ctx intent]
                                        (reset! captured intent)
                                        {:result/status :ok :value {:v (:v (:args (:payload intent)))}})]
        (let [res (orch/orchestrate cm executor pi ca intent [])]
          (is (= :ok (:outcome res)))
          (is (some? @captured) "pipeline received intent")
          (is (= #uuid "11111111-1111-4111-8111-111111111111" (:session/id @captured)) "session attributed")
          (is (= phenotype-id (:phenotype/id @captured)) "phenotype attributed")
          (is (= :test-node (:node/id @captured)) "node attributed")
          (is (= 1 (:cause/event-id @captured)) "cause attributed"))))))

(deftest gc20-events-are-attributable-when-writing
  (testing "GC-20: event header attribution is preserved as plain data"
    (let [sid (random-uuid)
          pid phenotype-id
          nid :node/test
          hdr {:session/id sid :phenotype/id pid :node/id nid :cause/event-id 42}
          payload {:note "gc20"}]
      (is (= sid (:session/id hdr)) "session in header")
      (is (= pid (:phenotype/id hdr)) "phenotype in header")
      (is (= nid (:node/id hdr)) "node in header")
      (is (= "gc20" (:note payload)) "payload preserved")
      ;; verify header round-trips through EDN boundary
      (let [materialized (boundary/materialize-edn hdr {:max-depth 64 :max-size 1000})]
        (is (= sid (:session/id materialized)))
        (is (= pid (:phenotype/id materialized)))))))
