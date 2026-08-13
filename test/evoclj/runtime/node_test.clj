(ns evoclj.runtime.node-test
  "Task 6.2 tests for the node handler protocol and pure transitions.

  step computes ONE pure transition for one node:

    {:transition/status :continue | :complete | :failed
     :outputs [...]
     :intents [...]
     :next [:node/x]}

  Handlers NEVER call providers or the broker: a :tool node only EMITS
  a typed intent (Step 2 asserts a spy provider's counter stays at
  zero), the :sci node invokes its program inside the phenotype's
  isolated SCI runtime and converts the decision into validated
  intents, and the :emit node is terminal — it completes with the
  accumulated outputs. One shared Malli schema
  (evoclj.runtime.node/TransitionSchema) validates EVERY handler
  result (Step 3), and the registry resolves the four implemented
  types (:emit, :sci, :tool, :loop) while throwing explicit
  :node/not-implemented-yet typed errors for :llm, :route, and
  :memory/* so the compiler's accepted types and the runtime's
  executable types stay consistent (Step 4)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.topology :as topology]
            [evoclj.intent.core :as intent]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.node :as node]
            [evoclj.sci.context :as context]
            [evoclj.sci.execute :as execute]
            [malli.core :as m]))

;; --- fixture helpers --------------------------------------------------------

(def ^:private hex64
  "64 hex chars for a canonical \"sha256:<64 hex>\" phenotype id."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(defn- rs
  "A valid per-session runtime-state map; merge `overrides` on top."
  ([] (rs {}))
  ([overrides]
   (merge {:session/id (random-uuid)
           :phenotype/id (str "sha256:" hex64)
           :node/id :node/router
           :outputs []
           :budget {:wall-ms 1000}}
          overrides)))

(defn- event
  "A valid causal input-event map; merge `overrides` on top."
  ([] (event {}))
  ([overrides]
   (merge {:event/id 7 :event/type :node/started :payload nil}
          overrides)))

(defn- load-program
  "Load a program source into a fresh isolated SCI runtime map."
  [program-id entry source]
  (execute/load-program! {:context (context/make-context {}) :programs {}}
                         {:program/id program-id :entry entry}
                         source))

(defn- seed-sci-runtime
  "A runtime map with the REAL seed route program loaded."
  []
  (load-program :program/route 'agent.route/run
                (slurp (io/resource
                        "fixtures/genomes/minimal-valid/programs/route.clj"))))

(def ^:private echo-node
  "A compiled :tool node requesting :fixture/echo."
  {:node/type :tool :tool :fixture/echo :next :node/emit})

(def ^:private sci-node
  "A compiled :sci node running the seed route program."
  {:node/type :sci :program :program/route :next :node/finish})

(defn- step-error
  "The ExceptionInfo thrown by a handler step, or nil."
  [handler rs node input]
  (try (node/step handler rs node input)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

;; ============================================================================
;; Step 1 — the :emit, :sci, and :tool handlers
;; ============================================================================

(deftest emit-handler-completes-with-the-accumulated-outputs
  (testing "the :emit node is terminal: :complete, accumulated outputs, no :next, no intents"
    (let [t (node/step ((node/handler-for :emit))
                       (rs {:outputs [:a {:b 2} "c"]})
                       {:node/type :emit}
                       nil)]
      (is (= :complete (:transition/status t)))
      (is (= [:a {:b 2} "c"] (:outputs t)))
      (is (= [] (:intents t)))
      (is (not (contains? t :next)))))
  (testing "an empty accumulated list completes with []"
    (let [t (node/step ((node/handler-for :emit)) (rs {:outputs []})
                       {:node/type :emit} nil)]
      (is (= :complete (:transition/status t)))
      (is (= [] (:outputs t))))))

(deftest sci-handler-invokes-the-program-and-emits-typed-intents
  (testing "the echo route produces one validated tool-call intent with full attribution"
    (let [sci-runtime (seed-sci-runtime)
          rs (rs {:sci-runtime sci-runtime})
          input (event {:payload {:op :echo :text "hi"}})
          t (node/step ((node/handler-for :sci)) rs sci-node input)
          intent (first (:intents t))]
      (is (= :continue (:transition/status t)))
      (is (= [:node/finish] (:next t)))
      (is (= [{:action {:intent/type :intent/tool-call
                        :payload {:tool/id :fixture/echo :args {:text "hi"}}}}]
             (:outputs t)))
      (is (= 1 (count (:intents t))))
      (is (= :intent/tool-call (:intent/type intent)))
      (is (= {:tool/id :fixture/echo :args {:text "hi"}} (:payload intent)))
      (is (= (:session/id rs) (:session/id intent)))
      (is (= (:phenotype/id rs) (:phenotype/id intent)))
      (is (= (:node/id rs) (:node/id intent)))
      (is (= (:event/id input) (:cause/event-id intent)))
      (is (uuid? (:intent/id intent)))
      (is (= {:wall-ms 1000} (:budget intent)))))
  (testing "the finish route produces a validated finish intent"
    (let [t (node/step ((node/handler-for :sci)) (rs {:sci-runtime (seed-sci-runtime)})
                       sci-node (event {:payload {:op :finish :value 42}}))]
      (is (= :intent/finish (:intent/type (first (:intents t)))))
      (is (= {:value 42} (:payload (first (:intents t)))))))
  (testing "a :sci node without :next continues with an empty successor list"
    (let [t (node/step ((node/handler-for :sci)) (rs {:sci-runtime (seed-sci-runtime)})
                       {:node/type :sci :program :program/route}
                       (event {:payload {:op :echo :text "hi"}}))]
      (is (= :continue (:transition/status t)))
      (is (= [] (:next t))))))

(deftest sci-handler-maps-program-failures-to-failed-transitions
  (testing "a program error becomes a :failed transition with serializable error data"
    (let [runtime (load-program :program/boom 'test.boom/run
                                "(ns test.boom)\n(defn run [x] (throw (ex-info \"boom\" {:error/type :test/boom})))")
          t (node/step ((node/handler-for :sci)) (rs {:sci-runtime runtime})
                       {:node/type :sci :program :program/boom} (event {}))]
      (is (= :failed (:transition/status t)))
      (is (= :test/boom (get-in t [:error :error/type])))
      (is (map? (:error t)))))
  (testing "a non-decision program output becomes a :failed transition"
    (let [runtime (load-program :program/garbage 'test.garbage/run
                                "(ns test.garbage)\n(defn run [x] :not-a-decision)")
          t (node/step ((node/handler-for :sci)) (rs {:sci-runtime runtime})
                       {:node/type :sci :program :program/garbage} (event {}))]
      (is (= :failed (:transition/status t)))
      (is (= :node/invalid (get-in t [:error :error/type])))
      (is (= :invalid-program-output (get-in t [:error :error/data :reason])))))
  (testing "a decision with an unknown intent type becomes a :failed transition"
    (let [runtime (load-program :program/weird 'test.weird/run
                                "(ns test.weird)\n(defn run [x] {:action {:intent/type :intent/teleport :payload {}}})")
          t (node/step ((node/handler-for :sci)) (rs {:sci-runtime runtime})
                       {:node/type :sci :program :program/weird} (event {}))]
      (is (= :failed (:transition/status t)))
      (is (= :node/invalid (get-in t [:error :error/type])))
      (is (= :unknown-intent-type (get-in t [:error :error/data :reason]))))))

(deftest tool-handler-emits-a-typed-tool-call-intent
  (testing "the tool node builds a validated tool-call from node config + input"
    (let [sid (random-uuid)
          rs (rs {:session/id sid :node/id :node/echo-tool :budget {:wall-ms 2500}})
          input (event {:event/id 41 :payload {:text "hello"}})
          t (node/step ((node/handler-for :tool)) rs echo-node input)
          intent (first (:intents t))]
      (is (= :continue (:transition/status t)))
      (is (= [:node/emit] (:next t)))
      (is (= [] (:outputs t)))
      (is (= 1 (count (:intents t))))
      (is (= :intent/tool-call (:intent/type intent)))
      (is (= {:tool/id :fixture/echo :args {:text "hello"}} (:payload intent)))
      (is (= sid (:session/id intent)))
      (is (= (:phenotype/id rs) (:phenotype/id intent)))
      (is (= :node/echo-tool (:node/id intent)))
      (is (= 41 (:cause/event-id intent)))
      (is (= {:wall-ms 2500} (:budget intent)))
      (is (uuid? (:intent/id intent)))))
  (testing "a tool node without :next continues with an empty successor list"
    (let [t (node/step ((node/handler-for :tool)) (rs)
                       {:node/type :tool :tool :fixture/echo}
                       (event {:payload {:text "hi"}}))]
      (is (= [] (:next t)))
      (is (= :intent/tool-call (:intent/type (first (:intents t)))))))
  (testing "the default budget applies when runtime-state carries none"
    (let [t (node/step ((node/handler-for :tool)) (dissoc (rs) :budget)
                       echo-node (event {:payload {:text "hi"}}))]
      (is (= {:wall-ms 1000} (:budget (first (:intents t))))))))

;; ============================================================================
;; Step 2 — handlers never call providers
;; ============================================================================

(deftest handlers-never-call-providers
  (let [calls (atom 0)
        provider (fixture/echo-provider {:execution-count calls})
        reg (registry/create-registry)
        _ (registry/register! reg provider)]
    (testing "a :tool handler run never increments the provider counter"
      (let [t (node/step ((node/handler-for :tool))
                         (rs {:providers {:registry reg}})
                         echo-node
                         (event {:payload {:text "hi"}}))
            intent (first (:intents t))]
        (is (= 0 @calls))
        (is (= :intent/tool-call (:intent/type intent)))
        (is (= :fixture/echo (get-in intent [:payload :tool/id])))
        (is (= "hi" (get-in intent [:payload :args :text])))
        ;; a complete validated intent for the broker, not a provider call
        (is (uuid? (:intent/id intent)))))
    (testing "a :sci handler run (route deciding a tool call) never increments the provider counter"
      (let [t (node/step ((node/handler-for :sci))
                         (rs {:sci-runtime (seed-sci-runtime)
                              :providers {:registry reg}})
                         sci-node
                         (event {:payload {:op :echo :text "hi"}}))]
        (is (= 0 @calls))
        (is (= :intent/tool-call (:intent/type (first (:intents t)))))))))

;; ============================================================================
;; Step 3 — every handler result validates against the one shared schema
;; ============================================================================

(deftest every-handler-result-validates-against-the-shared-schema
  (let [results [(node/step ((node/handler-for :emit))
                            (rs {:outputs [:a "b" {:c 3}]})
                            {:node/type :emit}
                            nil)
                 (node/step ((node/handler-for :sci))
                            (rs {:sci-runtime (seed-sci-runtime)})
                            sci-node
                            (event {:payload {:op :echo :text "hi"}}))
                 (node/step ((node/handler-for :sci))
                            (rs {:sci-runtime (load-program
                                               :program/boom 'test.boom/run
                                               "(ns test.boom)\n(defn run [x] (throw (ex-info \"boom\" {:error/type :test/boom})))")})
                            {:node/type :sci :program :program/boom}
                            (event {}))
                 (node/step ((node/handler-for :tool))
                            (rs)
                            echo-node
                            (event {:payload {:text "hi"}}))]]
    (doseq [r results]
      (is (m/validate node/TransitionSchema r))
      (is (= r (node/validate-transition! r))))))

(deftest invalid-transitions-are-rejected
  (let [valid-intent (intent/tool-call (random-uuid)
                                       (str "sha256:" hex64)
                                       :node/x 1
                                       {:tool/id :fixture/echo :args {}}
                                       {:wall-ms 1000})
        raw-request {:intent/type :intent/tool-call
                     :payload {:tool/id :fixture/echo :args {}}}
        rejections [nil
                    {}
                    {:transition/status :bogus :outputs [] :intents []}
                    ;; :continue requires :next
                    {:transition/status :continue :outputs [] :intents []}
                    ;; :next must be a vector of keywords
                    {:transition/status :continue :outputs [] :intents []
                     :next :node/x}
                    {:transition/status :continue :outputs [] :intents []
                     :next [42]}
                    ;; :outputs must be a vector
                    {:transition/status :continue :outputs {} :intents [] :next []}
                    ;; maps are closed: no extra keys
                    {:transition/status :continue :outputs [] :intents [] :next []
                     :extra 1}
                    {:transition/status :complete :outputs [] :intents [valid-intent]
                     :extra 1}
                    ;; intents must be FULLY validated intents, not raw requests
                    {:transition/status :complete :outputs [] :intents [raw-request]}
                    ;; :failed requires a map :error
                    {:transition/status :failed :outputs [] :intents []}
                    {:transition/status :failed :outputs [] :intents [] :error :nope}
                    ;; :intents must be a vector
                    {:transition/status :complete :outputs [] :intents :nope}]]
    (doseq [r rejections]
      (is (thrown? clojure.lang.ExceptionInfo (node/validate-transition! r))
          (pr-str r))))
  (testing "rejections carry the :node/transition-invalid type"
    (let [e (try (node/validate-transition! {})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :node/transition-invalid (:error/type (ex-data e)))))))

;; ============================================================================
;; Step 4 — the node-type registry
;; ============================================================================

(deftest registry-resolves-known-types-to-handler-constructors
  (doseq [t [:emit :sci :tool :loop]]
    (let [ctor (node/handler-for t)]
      (is (fn? ctor) (str t))
      (is (satisfies? node/NodeHandler (ctor)) (str t)))))

(deftest registry-throws-typed-errors-for-unimplemented-and-unknown-types
  (testing "every accepted-but-unimplemented v0 type throws :node/not-implemented-yet"
    (doseq [t [:llm :route :memory/read :memory/write]]
      (let [e (try (node/handler-for t) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (= :node/not-implemented-yet (:error/type (ex-data e))) (str t))
        (is (= t (:node/type (ex-data e))) (str t)))))
  (testing "anything outside the v0 type set throws :node/unknown-type"
    (doseq [t [:node/bogus :unknown nil "sci" 42]]
      (let [e (try (node/handler-for t) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (= :node/unknown-type (:error/type (ex-data e))) (pr-str t))))))

(deftest registry-covers-exactly-the-compiler-node-type-set
  (is (= topology/supported-node-types
         (into node/known-unimplemented-types
               (keys node/node-handler-registry)))))

;; ============================================================================
;; fail-closed handler inputs
;; ============================================================================

(deftest malformed-handler-inputs-fail-closed
  (let [handler ((node/handler-for :tool))]
    (testing "a non-map runtime-state is rejected"
      (let [e (step-error handler :nope echo-node (event {}))]
        (is (= :node/runtime-invalid (:error/type (ex-data e))))
        (is (= :not-a-map (:reason (ex-data e))))))
    (testing "a missing session id is rejected"
      (let [e (step-error handler (dissoc (rs) :session/id) echo-node (event {}))]
        (is (= :node/runtime-invalid (:error/type (ex-data e))))
        (is (= :session-id-invalid (:reason (ex-data e))))))
    (testing "a missing :outputs vector is rejected"
      (let [e (step-error handler (dissoc (rs) :outputs) echo-node (event {}))]
        (is (= :outputs-invalid (:reason (ex-data e))))))
    (testing "a malformed :budget is rejected"
      (let [e (step-error handler (assoc (rs) :budget {:wall-ms -1}) echo-node (event {}))]
        (is (= :budget-invalid (:reason (ex-data e))))))
    (testing "a tool node without :tool is rejected"
      (let [e (step-error handler (rs) {:node/type :tool} (event {}))]
        (is (= :node/invalid (:error/type (ex-data e))))
        (is (= :missing-required-key (:reason (ex-data e))))))
    (testing "a node whose type mismatches the handler is rejected"
      (let [e (step-error handler (rs) {:node/type :sci :program :program/route} (event {}))]
        (is (= :type-mismatch (:reason (ex-data e))))))
    (testing "an input-event without :event/id is rejected"
      (let [e (step-error handler (rs) echo-node {:payload {:text "hi"}})]
        (is (= :node/input-invalid (:error/type (ex-data e))))
        (is (= :event-id-invalid (:reason (ex-data e))))))
    (testing "a non-map tool args payload is rejected"
      (let [e (step-error handler (rs) echo-node (event {:payload :nope}))]
        (is (= :args-invalid (:reason (ex-data e))))))
    (testing "a :sci node without :sci-runtime in runtime-state is rejected"
      (let [e (step-error ((node/handler-for :sci)) (rs) sci-node (event {}))]
        (is (= :node/runtime-invalid (:error/type (ex-data e))))
        (is (= :sci-runtime-missing (:reason (ex-data e))))))))
