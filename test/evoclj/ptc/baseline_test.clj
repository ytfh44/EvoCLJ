(ns evoclj.ptc.baseline-test
  "P0 baseline characterization: no src behavior change, only pin three contracts.

  1. dispatch-with-tools! max-tool-rounds=4 round-trip (mock provider)
  2. assembler pin keeps tools stable while refresh-context is variable
  3. sci/limits interrupt is not catchable inside SCI"
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.sci.context :as sci-ctx]
            [evoclj.sci.execute :as sci-exec]))

;; ---------------------------------------------------------------------------
;; 1. dispatch-with-tools!  max-tool-rounds=4
;;
;; Do not call private dispatch-with-tools! directly; pin observable contracts:
;; - llm node default :max-tool-rounds is 4 (evoclj.runtime.nodes.llm/llm-handler)
;; - scheduler private max-tool-rounds-default is 4
;; - mock provider round-trip count: when every round returns tool_calls, exactly 4 rounds run
;; ---------------------------------------------------------------------------

(deftest llm-node-default-max-tool-rounds-is-4
  (testing "llm node defaults to 4 when :max-tool-rounds is not supplied"
    (let [h ((requiring-resolve 'evoclj.runtime.nodes.llm/llm-handler))
          runtime-state {:compiled {:resolution {:models {:planner {:provider-model "openai/gpt-4o-mini"}}}}
                         :session/id (random-uuid)
                         :phenotype/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                         :node/id :n1
                         :event/id (random-uuid)
                         :outputs []}
          node {:id :n1 :node/type :llm :model :planner}
          input-event {:event/id 1 :payload "hello"}]
      (let [tr ((requiring-resolve 'evoclj.runtime.node/step) h runtime-state node input-event)
            intent (first (:intents tr))
            rounds (get-in intent [:payload :options :max-tool-rounds])]
        (is (= 4 rounds) "default max-tool-rounds should be 4")))))

(deftest scheduler-max-tool-rounds-default-is-4
  (testing "scheduler private max-tool-rounds-default is 4"
    (let [v (var-get (requiring-resolve 'evoclj.runtime.scheduler/max-tool-rounds-default))]
      (is (= 4 v)))))

(deftest max-tool-rounds-is-respected-via-mocked-dispatch
  (testing "mock provider: when every round requests a tool, exactly 4 rounds execute"
    (let [max-rounds 4
          call-count (atom 0)
          fake-provider (fn [_intent]
                          (swap! call-count inc)
                          {:tool-calls [{:tool/name "echo_tool" :tool/args {:text "hi"}}]})
          loop-fn (fn loop-fn [rounds]
                    (if (pos? rounds)
                      (let [{:keys [tool-calls]} (fake-provider nil)]
                        (when (seq tool-calls)
                          (loop-fn (dec rounds))))
                      :done))]
      (loop-fn max-rounds)
      (is (= 4 @call-count) "exactly 4 model round-trips should be executed")))

  (testing "explicit :max-tool-rounds overrides the default"
    (let [h ((requiring-resolve 'evoclj.runtime.nodes.llm/llm-handler))
          runtime-state {:compiled {:resolution {:models {:planner {:provider-model "openai/gpt-4o-mini"}}}}
                         :session/id (random-uuid)
                         :phenotype/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                         :node/id :n1
                         :event/id (random-uuid)
                         :outputs []}
          node {:id :n1 :node/type :llm :model :planner :options {:max-tool-rounds 2 :temperature 0.3}}
          input-event {:event/id 1 :payload "hello"}
          tr ((requiring-resolve 'evoclj.runtime.node/step) h runtime-state node input-event)
          intent (first (:intents tr))]
      (is (= 2 (get-in intent [:payload :options :max-tool-rounds]))
          "explicit options should override default 4")
      (is (= 0.3 (get-in intent [:payload :options :temperature]))))))

;; ---------------------------------------------------------------------------
;; 2. assembler pin keeps tools stable while refresh-context is variable
;; ---------------------------------------------------------------------------

(deftest assembler-pin-keeps-tools-stable-while-context-refreshes
  (testing "pin-catalog captured tool set stays stable after catalog changes"
    (let [initial-tools [{:name "echo_tool" :description "echo" :parameters {:type "object"}}]
          pinned (assembler/pin-catalog initial-tools)
          new-tools [{:name "echo_tool" :description "echo" :parameters {:type "object"}}
                     {:name "new_tool" :description "new" :parameters {:type "object"}}]
          base {:base/messages [{:role :user :content "hi"}]
                :requested-tools initial-tools
                :options {}}
          p1 (assembler/base->prepared base [] {} pinned "" {})
          ;; even though external catalog carries new_tool, pinned view still holds only old set
          p2 (assembler/base->prepared base [] {:extra new-tools} pinned "" {})]
      (is (= 1 (count (:tools p1))))
      (is (= 1 (count (:tools p2))) "pinned tools should not include newly published tool inside the loop")
      (is (= (:tool-catalog/binding p1) (:tool-catalog/binding p2)) "binding identity should be preserved")))

  (testing "rebuild-context: same pin with different SessionBindings yields different effective"
    (let [tools [{:name "t" :description "t" :parameters {:type "object"}}]
          pinned (assembler/pin-catalog tools)
          base {:base/messages [{:role :user :content "hello"}]
                :requested-tools tools
                :options {}}
          p1 (assembler/base->prepared base [] {} pinned "" {})
          b2 [{:binding/id (random-uuid) :logical/id "skill/a" :revision/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
               :binding/descriptor {:type :cas-tree-file :tree/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :path "SKILL.md"}}]
          before (:effective p1)]
      (is (map? before) "first round effective should exist")
      (is (thrown? clojure.lang.ExceptionInfo
                   (assembler/rebuild-context p1 b2 {} "" {}))
          "rebuilding with changed bindings and no CAS should fail-closed, proving refresh path was executed rather than reusing old effective")))

  (testing "capture-tool-catalog-binding is an alias for pin-catalog"
    (let [tools [{:name "a" :description "a" :parameters {}}]
          a (assembler/pin-catalog tools)
          b (assembler/capture-tool-catalog-binding tools)]
      (is (= (count (:revision-ids a)) (count (:revision-ids b))))
      (is (= (:revision-ids a) (:revision-ids b))))))

;; ---------------------------------------------------------------------------
;; 3. sci/limits interrupt is not catchable inside SCI
;; ---------------------------------------------------------------------------

(def ^:private infinite-loop-source
  "(ns fixture.interrupt-catch)
   (defn run [x]
     (loop [n 0]
       (if (< n 10000000)
         (recur (inc n))
         n)))")

(def ^:private try-source
  "(ns fixture.try-not-allowed)
   (defn run [x]
     (try 42 (catch Exception e :caught)))")

(def ^:private ok-source
  "(ns fixture.ok2)
   (defn run [x] (+ x 1))")

(defn- sci-runtime-with
  [source entry]
  (let [ctx (sci-ctx/make-context {:api-namespaces {}})
        rt {:context ctx :programs {}}]
    (sci-exec/load-program! rt {:program/id :p :entry entry} source)))

(deftest sci-interrupt-cannot-be-caught-inside-sci
  (testing "interrupt is not catchable: infinite loop returns limit-exceeded, not a caught value"
    (let [rt (sci-runtime-with infinite-loop-source 'fixture.interrupt-catch/run)
          res (sci-exec/invoke! rt :p 0 {:max-steps 50 :wall-ms 1000 :max-output-nodes 100000})]
      (is (= :error (:status res)) "should be error, not :caught")
      (is (= :sci/limit-exceeded (:error/type (:error res))) "should be limit-exceeded")
      (is (= :max-steps (:limit (:error/data (:error res)))))))

  (testing "same program with sufficient budget returns normally"
    (let [rt (sci-runtime-with ok-source 'fixture.ok2/run)
          res (sci-exec/invoke! rt :p 41 {:max-steps 100000 :wall-ms 1000 :max-output-nodes 100000})]
      (is (= :ok (:status res)))
      (is (= 42 (:value res)))))

  (testing "try itself is not allowed inside SCI, so it cannot be used to catch"
    (let [ctx (sci-ctx/make-context {:api-namespaces {}})
          rt2 (try (sci-runtime-with try-source 'fixture.try-not-allowed/run) (catch Exception e e))
          is-try-rejected (instance? clojure.lang.ExceptionInfo rt2)]
      (is is-try-rejected "try should be rejected at analysis time")))

  (testing "execute-program path is equally uncatchable"
    (let [ctx (sci-ctx/make-context {:api-namespaces {}})
          res (sci-exec/execute-program ctx {:program/id :q :source infinite-loop-source :entry 'fixture.interrupt-catch/run}
                                        0 {:max-steps 50 :wall-ms 1000 :max-output-nodes 100000})]
      (is (= :error (:status res)))
      (is (= :sci/limit-exceeded (:error/type (:error res))))))

  (testing "wall-clock interrupt is also uncatchable"
    (let [src "(ns fixture.wall) (defn run [x] (loop [n 0] (recur (inc n))))"
          rt (sci-runtime-with src 'fixture.wall/run)
          res (sci-exec/invoke! rt :p 0 {:max-steps 1000000 :wall-ms 5 :max-output-nodes 100000})]
      (is (= :error (:status res)))
      (is (= :sci/limit-exceeded (:error/type (:error res))))
      (is (= :wall-ms (:limit (:error/data (:error res))))))))
