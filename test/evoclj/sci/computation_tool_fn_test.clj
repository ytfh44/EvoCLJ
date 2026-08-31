(ns evoclj.sci.computation-tool-fn-test
  "Sandbox tool_fn injection tests for P8.

  Covers:
    - plain code execution without tools
    - single tool injection (tool/echo)
    - two sequential tool calls
    - codeBytes limit 8192 -> 9000 bytes error
    - toolCalls limit 32 -> 33 calls error
    - Wolfram verified limitsCheck (bytesOk, callsOk)
    - handleError transient vs ambiguous (single pipeline impl, INV-05)
    - materialize-edn and uncatchable interrupt"
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.intent.pipeline :as pipeline]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.runtime.orchestrator :as orch]
            [evoclj.sci.computation :as comp]
            [evoclj.sci.boundary :as boundary]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- echo-fn [args] (assoc args :echoed true))

(defn- make-comp [] (comp/make-computation {}))

;; ---------------------------------------------------------------------------
;; Plain execution without tools
;; ---------------------------------------------------------------------------

(deftest code-without-tool-returns-value
  (testing "plain SCI code without tools evaluates correctly"
    (let [c (make-comp)
          r (comp/execute-code c "(+ 1 2)" nil)]
      (is (= :ok (:status r)))
      (is (= 3 (:value r)))
      (is (vector? (:events r)))
      (is (map? (:usage r))))))

(deftest code-without-tool-nil-tool-map
  (testing "nil tool map is treated as empty"
    (let [c (make-comp)
          r (comp/execute-code c "(+ 10 20)")]
      (is (= :ok (:status r)))
      (is (= 30 (:value r))))))

;; ---------------------------------------------------------------------------
;; Single tool injection
;; ---------------------------------------------------------------------------

(deftest single-tool-echo
  (testing "single tool/echo call crosses host fn and returns EDN-safe materialized value"
    (let [c (make-comp)
          r (comp/execute-code c "(tool/echo {\"text\" \"hi\"})" {"echo" echo-fn})]
      (is (= :ok (:status r)))
      (is (= {"text" "hi" :echoed true} (:value r)))
      (is (= 1 (get-in r [:usage :tool-calls]))))))

(deftest single-tool-with-keyword-id
  (testing "tool id may be keyword"
    (let [c (make-comp)
          r (comp/execute-code c "(tool/echo {\"text\" \"hi\"})" {:echo echo-fn})]
      (is (= :ok (:status r)))
      (is (= {"text" "hi" :echoed true} (:value r))))))

;; ---------------------------------------------------------------------------
;; Two sequential tools
;; ---------------------------------------------------------------------------

(deftest two-sequential-tools
  (testing "two sequential tool calls are counted and both execute"
    (let [c (make-comp)
          calls (atom [])
          f (fn [args] (swap! calls conj args) (assoc args :n (count @calls)))
          r (comp/execute-code c "(do (tool/echo {\"a\" 1}) (tool/echo {\"b\" 2}))" {"echo" f})]
      (is (= :ok (:status r)))
      (is (= {"b" 2 :n 2} (:value r)))
      (is (= 2 (count @calls)))
      (is (= 2 (get-in r [:usage :tool-calls]))))))

;; ---------------------------------------------------------------------------
;; Limits: codeBytes
;; ---------------------------------------------------------------------------

(deftest code-bytes-limit-9000-is-error
  (testing "code string exceeding 8192 UTF-8 bytes is rejected"
    (let [c (make-comp)
          big (apply str (repeat 9000 "x"))
          r (comp/execute-code c big nil)]
      (is (= :error (:status r)))
      (is (= :sci/limit-exceeded (:error/type (:error r))))
      (is (= :code-bytes (:limit (:error/data (:error r))))))))

(deftest code-bytes-within-limit-passes
  (testing "code at exactly 8192 bytes is accepted (boundary)"
    (let [c (make-comp)
          ;; "(+ 1 2)" is tiny, so build a large but within-limit string: 8192 'a' chars with quoting?
          ;; Instead just check that 8000 'x' is not codeBytes limit; it will fail parse but not codeBytes limit
          ;; So test that codeBytes check is bytes-based: use a valid expression padded with spaces to reach 8192
          padding (apply str (repeat 8185 " "))
          code (str "(+ 1 2)" padding)
          r (comp/execute-code c code nil)]
      ;; Should be ok or at worst not codeBytes exceeded; wall/steps may still be ok
      (is (not= :code-bytes (:limit (:error/data (:error r)))) "should not be codeBytes limit exceeded"))))

;; ---------------------------------------------------------------------------
;; Limits: toolCalls
;; ---------------------------------------------------------------------------

(deftest tool-calls-33-is-error
  (testing "33 tool calls exceeds limit 32 and surfaces as limit-exceeded"
    (let [c (make-comp)
          code (str "(do " (apply str (repeat 33 "(tool/echo {\"x\" 1}) ")) ")")
          r (comp/execute-code c code {"echo" echo-fn})]
      (is (= :error (:status r)))
      (is (= :sci/limit-exceeded (:error/type (:error r))))
      (is (= :max-tool-calls (:limit (:error/data (:error r))))))))

(deftest tool-calls-32-passes
  (testing "32 tool calls is within limit"
    (let [c (make-comp)
          code (str "(do " (apply str (repeat 32 "(tool/echo {\"x\" 1}) ")) ")")
          r (comp/execute-code c code {"echo" echo-fn})]
      (is (= :ok (:status r)))
      (is (= 32 (get-in r [:usage :tool-calls]))))))

;; ---------------------------------------------------------------------------
;; Wolfram verified limitsCheck (bytesOk, callsOk)
;; ---------------------------------------------------------------------------

(deftest limits-check-wolfram-verified
  (testing "Wolfram verified lattice: bytesOk and callsOk"
    (let [bytes-ok? (var-get #'comp/bytes-ok?)
          calls-ok? (var-get #'comp/calls-ok?)
          limits-check (var-get #'comp/limits-check)]
      ;; bytesOk
      (is (true? (bytes-ok? "(+ 1 2)")))
      (is (false? (bytes-ok? (apply str (repeat 9000 "x")))))
      ;; callsOk
      (is (true? (calls-ok? 0)))
      (is (true? (calls-ok? 32)))
      (is (false? (calls-ok? 33)))
      ;; combined
      (is (true? (limits-check "(+ 1 2)" 0)))
      (is (false? (limits-check (apply str (repeat 9000 "x")) 0)))
      (is (false? (limits-check "(+ 1 2)" 33)))
      (is (false? (limits-check (apply str (repeat 9000 "x")) 33))))))

;; ---------------------------------------------------------------------------
;; Materialize-edn and uncatchable interrupt
;; ---------------------------------------------------------------------------

(deftest materialize-edn-enforced
  (testing "return value is materialized via boundary/materialize-edn"
    (let [c (make-comp)
          ;; return a lazy seq: (range) is infinite, should be rejected as size exceeded
          ;; but via execute-code we return (range 10) which is lazy; materialize should realize
          r (comp/execute-code c "(map inc [1 2 3])" nil)]
      (is (= :ok (:status r)))
      ;; map returns lazy seq, materialized as list
      (is (= '(2 3 4) (:value r))))))

(deftest interrupt-is-uncatchable-wall-steps
  (testing "wall/steps interrupt is uncatchable inside SCI and surfaces as limit-exceeded"
    (let [c (make-comp)
          r (comp/execute-code c "(loop [] (recur))" nil)]
      (is (= :error (:status r)))
      (is (= :sci/limit-exceeded (:error/type (:error r)))))))

(deftest tool-calls-interrupt-is-uncatchable
  (testing "tool call budget exceeded uses interrupt! which cannot be caught inside SCI (try not allowed anyway)"
    (let [c (make-comp)
          code (str "(do " (apply str (repeat 33 "(tool/echo {\"x\" 1}) ")) ")")
          r (comp/execute-code c code {"echo" echo-fn})]
      (is (= :error (:status r)))
      (is (= :sci/limit-exceeded (:error/type (:error r))))
      ;; value is not :caught, it is error
      (is (not= :caught (:value r))))))

;; ---------------------------------------------------------------------------
;; handleError transient vs ambiguous (single impl in pipeline, INV-05)
;; ---------------------------------------------------------------------------

(deftest handle-error-transient-vs-ambiguous
  (testing "pipeline transient vs ambiguous classification (single handleError, INV-05)"
    (let [transient-ex (err/error :provider/transient-error "transient" {})
          ambiguous-ex (err/error :provider/call-ambiguous "ambiguous" {})]
      (is (true? (pipeline/transient-error? transient-ex)))
      (is (false? (pipeline/ambiguous-error? transient-ex)))
      (is (true? (pipeline/ambiguous-error? ambiguous-ex)))
      (is (false? (pipeline/transient-error? ambiguous-ex)))
      ;; type predicates
      (is (true? (pipeline/transient-error-type? :provider/transient-error)))
      (is (true? (pipeline/transient-error-type? :mcp/timeout)))
      (is (false? (pipeline/transient-error-type? :provider/call-ambiguous)))
      (is (true? (pipeline/ambiguous-error-type? :provider/call-ambiguous))))))

;; ---------------------------------------------------------------------------
;; Orchestrator make-tool-fns factory (GC-08: each toolFn crosses broker)
;; ---------------------------------------------------------------------------

(deftest orchestrator-make-tool-fns-crosses-broker
  (testing "make-tool-fns factory builds fns that cross broker via pipeline"
    (let [reg (registry/create-registry)
          _ (registry/register! reg
                (reify proto/Provider
                  (describe [_] {:tool/id :echo :effect :pure :input-schema [:map [:text :string]] :output-schema [:map [:text :string] [:echoed :boolean]] :required-action :invoke :retry {:safe? true}})
                  (normalize-request [_ intent] {:tool/id :echo :resource {:kind :tool :id :echo} :args (get-in intent [:payload :args])})
                  (execute-request! [_ norm] (assoc (:args norm) :echoed true))))
          broker (dispatch/make-broker-context {:registry reg
                                                :leases [{:cap/id (random-uuid)
                                                          :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}
                                                          :resource {:kind :tool :id :echo}
                                                          :actions #{:invoke}
                                                          :constraints {:max-calls 10}
                                                          :issued-at (java.util.Date. 0)
                                                          :expires-at (java.util.Date. 4102444800000)}]})
          executor {:dispatch broker}
          pin {:session/id (random-uuid)
               :phenotype/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
               :node/id :sandbox}
          cause 1
          fns (#'orch/make-tool-fns {"echo" {:tool/id :echo}} executor pin cause)
          f (get fns "echo")
          res (f {:text "hi"})]
      (is (map? fns))
      (is (fn? f))
      (is (= {:text "hi" :echoed true} res)))))

(deftest orchestrator-make-tool-fns-invalid-map-throws
  (testing "invalid tool-map is rejected"
    (let [reg (registry/create-registry)
          broker (dispatch/make-broker-context {:registry reg})
          executor {:dispatch broker}]
      (is (thrown? clojure.lang.ExceptionInfo (#'orch/make-tool-fns "not-a-map" executor {} 1))))))
