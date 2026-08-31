(ns evoclj.runtime.p9-code-execution-declaration-test
  "P9 CodeMode declaration tests:
   - ToolSurface pin includes code_execution when :ptc enabled and surface has tools
   - assembler base->prepared emits code_execution when enabled, otherwise not (fail-safe)
   - provider wire-tools includes code_execution correctly"
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.runtime.tool-surface :as tool-surface]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.tool.specs :as tool-specs]
            [evoclj.provider.request :as req]))

(def ^:private sample-tool
  {:name "evolution_evidence"
   :description "evidence"
   :parameters {:type "object" :properties {}}
   :tool :evolution/evidence})

(def ^:private another-tool
  {:name "evolution_history"
   :description "history"
   :parameters {:type "object" :properties {}}
   :tool :evolution/history})

;; ---------------------------------------------------------------------------
;; ToolSurface
;; ---------------------------------------------------------------------------

(deftest tool-surface-pin-includes-code-execution-when-enabled
  (testing "pin includes code_execution when ptc enabled and surface has tools"
    (let [surface [sample-tool]
          pinned (tool-surface/pin surface {:enabled? true})]
      (is (= 2 (count (:surface/tools pinned))))
      (is (some #(= "code_execution" (:name %)) (:surface/tools pinned)))
      (let [wire (some #(when (= "code_execution" (:name %)) %) (:surface/tools pinned))]
        (is (= "Execute SCI Clojure code with toolFns" (:description wire)))
        (is (= :ptc/code-execution (:tool wire)))
        (is (= {:type "object"
                :properties {:code {:type "string" :description "SCI Clojure source code to execute"}
                             :language {:type "string" :description "Language identifier, must be sci-clojure"}}
                :required ["code"]}
               (:parameters wire))))))

  (testing "pin with {:ptc {:enabled? true}} also enables"
    (let [pinned (tool-surface/pin [sample-tool] {:ptc {:enabled? true}})]
      (is (some #(= "code_execution" (:name %)) (:surface/tools pinned)))))

  (testing "pin with boolean true enables"
    (let [pinned (tool-surface/pin [sample-tool] true)]
      (is (some #(= "code_execution" (:name %)) (:surface/tools pinned)))))

  (testing "pin with multiple tools appends code_execution once"
    (let [pinned (tool-surface/pin [sample-tool another-tool] {:enabled? true})]
      (is (= 3 (count (:surface/tools pinned))))
      (is (= 1 (count (filter #(= "code_execution" (:name %)) (:surface/tools pinned)))))))

  (testing "pin is idempotent — already containing code_execution not duplicated"
    (let [with-code (conj [sample-tool] tool-specs/code-execution-wire-tool)
          pinned (tool-surface/pin with-code {:enabled? true})]
      (is (= 2 (count (:surface/tools pinned))))
      (is (= 1 (count (filter #(= "code_execution" (:name %)) (:surface/tools pinned))))))))

(deftest tool-surface-pin-fail-safe
  (testing "pin without ptc enabled does not include code_execution"
    (let [pinned (tool-surface/pin [sample-tool])]
      (is (= 1 (count (:surface/tools pinned))))
      (is (not (some #(= "code_execution" (:name %)) (:surface/tools pinned))))))

  (testing "pin with {:enabled? false} does not include"
    (let [pinned (tool-surface/pin [sample-tool] {:enabled? false})]
      (is (not (some #(= "code_execution" (:name %)) (:surface/tools pinned))))))

  (testing "pin with empty surface does not include even when enabled (fail-safe: surface has tools condition)"
    (let [pinned (tool-surface/pin [] {:enabled? true})]
      (is (empty? (filter #(= "code_execution" (:name %)) (:surface/tools pinned))))))

  (testing "pin with map catalog does not inject (only sequential)"
    (let [catalog {:src-a "rev-1"}
          pinned (tool-surface/pin catalog {:enabled? true})]
      (is (= catalog (:surface/tools pinned)))))

  (testing "pin with nil surface yields empty and no code_execution even when enabled"
    (let [pinned (tool-surface/pin nil {:enabled? true})]
      (is (empty? (:surface/tools pinned))))))

;; ---------------------------------------------------------------------------
;; Assembler
;; ---------------------------------------------------------------------------

(deftest assembler-emits-code-execution-when-enabled
  (testing "base->prepared with ptc enabled emits code_execution alongside existing tools"
    (let [base {:base/messages [{:role "system" :content "hi"}]
                :requested-tools [sample-tool]}
          prepared (assembler/base->prepared base [] {} nil "" {:ptc {:enabled? true}})]
      (is (= 2 (count (:tools prepared))))
      (is (some #(= "code_execution" (:name %)) (:tools prepared)))
      (let [wire (some #(when (= "code_execution" (:name %)) %) (:tools prepared))]
        (is (= "Execute SCI Clojure code with toolFns" (:description wire)))
        (is (= :ptc/code-execution (:tool wire)))
        (is (contains? (:tool-map prepared) "code_execution"))
        (is (= :ptc/code-execution (:tool (get (:tool-map prepared) "code_execution")))))))

  (testing "assembler with {:enabled? true} opts also enables"
    (let [base {:base/messages [] :requested-tools [sample-tool]}
          prepared (assembler/base->prepared base [] {} nil "" {:enabled? true})]
      (is (some #(= "code_execution" (:name %)) (:tools prepared)))))

  (testing "assembler idempotent"
    (let [base {:base/messages [] :requested-tools [sample-tool tool-specs/code-execution-wire-tool]}
          prepared (assembler/base->prepared base [] {} nil "" {:ptc {:enabled? true}})]
      (is (= 2 (count (:tools prepared))))
      (is (= 1 (count (filter #(= "code_execution" (:name %)) (:tools prepared)))))))

  (testing "assemble wrapper forwards ptc"
    (let [base {:base/messages [] :requested-tools [sample-tool]}
          prepared (assembler/assemble base {:catalog {} :ptc {:enabled? true}})]
      (is (some #(= "code_execution" (:name %)) (:tools prepared))))))

(deftest assembler-fail-safe
  (testing "without ptc does not emit code_execution"
    (let [base {:base/messages [] :requested-tools [sample-tool]}
          prepared (assembler/base->prepared base [] {} nil "" {})]
      (is (= 1 (count (:tools prepared))))
      (is (not (some #(= "code_execution" (:name %)) (:tools prepared))))
      (is (not (contains? (:tool-map prepared) "code_execution")))))

  (testing "with ptc disabled explicitly does not emit"
    (let [base {:base/messages [] :requested-tools [sample-tool]}
          prepared (assembler/base->prepared base [] {} nil "" {:ptc {:enabled? false}})]
      (is (not (some #(= "code_execution" (:name %)) (:tools prepared))))))

  (testing "empty requested-tools does not emit even when enabled"
    (let [base {:base/messages [] :requested-tools []}
          prepared (assembler/base->prepared base [] {} nil "" {:ptc {:enabled? true}})]
      (is (empty? (:tools prepared)))
      (is (not (contains? (:tool-map prepared) "code_execution")))))

  (testing "nil requested-tools treated as empty"
    (let [base {:base/messages []}
          prepared (assembler/base->prepared base [] {} nil "" {:ptc {:enabled? true}})]
      (is (empty? (:tools prepared))))))

;; ---------------------------------------------------------------------------
;; Provider wire mapping
;; ---------------------------------------------------------------------------

(deftest provider-wire-tools-preserves-code-execution
  (testing "wire-tools maps code_execution with :tool/id and build-request strips it"
    (let [tools [sample-tool tool-specs/code-execution-wire-tool]
          wired (req/wire-tools tools)]
      (is (= 2 (count wired)))
      (let [code-wired (some #(when (= "code_execution" (get-in % [:function :name])) %) wired)]
        (is (some? code-wired))
        (is (= :ptc/code-execution (:tool/id code-wired)))
        (is (= "Execute SCI Clojure code with toolFns" (get-in code-wired [:function :description]))))
      ;; build-request strips :tool/id before sending to provider
      (let [req {:model/id "openai/gpt-4" :messages [{:role "user" :content "hi"}] :tools tools :options {}}
            built (req/build-request :openai req {})]
        (is (some #(= "code_execution" (get-in % [:function :name])) (get-in built [:extra :tools])))
        (is (every? #(not (contains? % :tool/id)) (get-in built [:extra :tools])))))))

(deftest single-source-c-tool
  (testing "tool-specs single source is reused by assembler and tool-surface"
    (is (= tool-specs/code-execution-wire-tool
           (some #(when (= "code_execution" (:name %))
                    %)
                 (:surface/tools (tool-surface/pin [sample-tool] {:enabled? true})))))
    (is (= tool-specs/code-execution-wire-tool
           (some #(when (= "code_execution" (:name %)) %)
                 (:tools (assembler/base->prepared {:base/messages [] :requested-tools [sample-tool]}
                                                   [] {} nil "" {:ptc {:enabled? true}})))))
    (is (= :ptc/code-execution tool-specs/code-execution-tool-id))
    (is (= :ptc/code-execution (:tool tool-specs/code-execution-wire-tool)))
    (is (= :ptc/code-execution (:tool/id tool-specs/code-execution-tool)))
    (is (tool-specs/code-execution-wire-tool? tool-specs/code-execution-wire-tool))))
