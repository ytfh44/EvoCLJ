(ns evoclj.intent.subagent-intent-test
  "S1 subagent intent types: spawn / result / cancel with payload schemas (GC-22)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.intent.schema :as schema]
            [malli.core :as m]))

;; --- fixtures ---------------------------------------------------------------

(def ^:private intent-id #uuid "22222222-2222-4222-8222-222222222222")
(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private child-session-id #uuid "33333333-3333-4333-8333-333333333333")
(def ^:private phenotype-id
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private cas-ref
  "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(def ^:private node-id :node/tool)
(def ^:private cause-event-id 17)
(def ^:private budget {:wall-ms 1000})

(defn- base-intent [type payload]
  {:intent/id intent-id
   :intent/type type
   :session/id session-id
   :phenotype/id phenotype-id
   :node/id node-id
   :cause/event-id cause-event-id
   :payload payload
   :budget budget
   :metadata {}})

(defn- is-schema-invalid [x]
  (try
    (schema/validate-intent x)
    (is false (str "expected :intent/schema-invalid but validated: " (pr-str x)))
    (catch clojure.lang.ExceptionInfo e
      (is (= :intent/schema-invalid (:error/type (ex-data e)))
          (str "expected :intent/schema-invalid, got " (pr-str (ex-data e)))))))

(defn- is-not-edn-safe [x]
  (try
    (schema/validate-intent x)
    (is false (str "expected :intent/not-edn-safe but validated: " (pr-str x)))
    (catch clojure.lang.ExceptionInfo e
      (is (= :intent/not-edn-safe (:error/type (ex-data e)))))))

;; ============================================================================
;; Group 1 — new types valid with minimal payloads (including open keys)
;; ============================================================================

(deftest new-subagent-types-valid-with-minimal-payloads
  (testing "subagent-spawn minimal + open keys"
    (let [payload {:parent/session-id session-id
                   :child/spec {:genome/id "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                                :task {:op :echo :text "hi"}}
                   :child/capabilities []}
          intent (base-intent :intent/subagent-spawn payload)
          result (schema/validate-intent intent)]
      (is (= intent result))
      (is (= result (edn/read-string (pr-str result))) "EDN round-trip")))
  (testing "subagent-spawn with lease maps and extra key (open payload)"
    (let [payload {:parent/session-id session-id
                   :child/spec {:genome/id "genome-1" :task "do work"}
                   :child/capabilities [{:cap/id (random-uuid) :resource {:kind :tool :id :fixture/echo}}]
                   :extra "allowed"}
          intent (base-intent :intent/subagent-spawn payload)]
      (is (= intent (schema/validate-intent intent)))))
  (testing "subagent-result minimal"
    (let [payload {:parent/session-id session-id
                   :child/session-id child-session-id
                   :result/cas-ref cas-ref}
          intent (base-intent :intent/subagent-result payload)
          result (schema/validate-intent intent)]
      (is (= intent result))
      (is (= result (edn/read-string (pr-str result))))))
  (testing "subagent-result with extra key (open)"
    (let [payload {:parent/session-id session-id
                   :child/session-id child-session-id
                   :result/cas-ref cas-ref
                   :extra 123}
          intent (base-intent :intent/subagent-result payload)]
      (is (= intent (schema/validate-intent intent)))))
  (testing "subagent-cancel minimal for each reason"
    (doseq [reason [:user-request :parent-cancel :timeout]]
      (let [payload {:target/session-id child-session-id
                     :reason reason}
            intent (base-intent :intent/subagent-cancel payload)]
        (is (= intent (schema/validate-intent intent))
            (str "reason " reason " should validate"))
        (is (= intent (edn/read-string (pr-str (schema/validate-intent intent))))))))
  (testing "subagent-cancel with extra key (open)"
    (let [payload {:target/session-id child-session-id
                   :reason :timeout
                   :note "parent deadline exceeded"}
          intent (base-intent :intent/subagent-cancel payload)]
      (is (= intent (schema/validate-intent intent)))))
  (testing "IntentTypeSchema includes new types"
    (is (m/validate schema/IntentTypeSchema :intent/subagent-spawn))
    (is (m/validate schema/IntentTypeSchema :intent/subagent-result))
    (is (m/validate schema/IntentTypeSchema :intent/subagent-cancel))))

;; ============================================================================
;; Group 2 — bad payloads fail with typed error (GC-22, schema checks)
;; ============================================================================

(deftest bad-payloads-fail-with-typed-error
  (testing "missing parent/session-id on spawn"
    (is-schema-invalid
     (base-intent :intent/subagent-spawn
                  {:child/spec {:genome/id "g1"}
                   :child/capabilities []})))
  (testing "missing parent/session-id on result"
    (is-schema-invalid
     (base-intent :intent/subagent-result
                  {:child/session-id child-session-id
                   :result/cas-ref cas-ref})))
  (testing "missing target/session-id on cancel"
    (is-schema-invalid
     (base-intent :intent/subagent-cancel
                  {:reason :timeout})))
  (testing "wrong type for parent/session-id (not uuid)"
    (is-schema-invalid
     (base-intent :intent/subagent-spawn
                  {:parent/session-id "not-a-uuid"
                   :child/spec {}
                   :child/capabilities []})))
  (testing "invalid cas-ref format fails"
    (is-schema-invalid
     (base-intent :intent/subagent-result
                  {:parent/session-id session-id
                   :child/session-id child-session-id
                   :result/cas-ref "not-a-cas-ref"})))
  (testing "invalid cancel reason fails"
    (is-schema-invalid
     (base-intent :intent/subagent-cancel
                  {:target/session-id child-session-id
                   :reason :bogus})))
  (testing "GC-22: raw Java object in payload rejected as not-edn-safe"
    (is-not-edn-safe
     (base-intent :intent/subagent-spawn
                  {:parent/session-id session-id
                   :child/spec {:genome/id "g1"}
                   :child/capabilities []
                   :raw (java.io.File. "/tmp/x")}))))

;; ============================================================================
;; Group 3 — regression: existing types still valid + unknown rejected
;; ============================================================================

(deftest existing-types-still-valid-regression
  (testing ":intent/tool-call still passes"
    (let [intent (base-intent :intent/tool-call {:tool/id :fixture/echo :args {:text "hello"}})]
      (is (= intent (schema/validate-intent intent)))
      (is (= intent (edn/read-string (pr-str (schema/validate-intent intent)))))))
  (testing "all original six types still validate"
    (doseq [[type payload] [[:intent/model-call {:model/id :model/planner :messages [{:role :user :content "hi"}]}]
                            [:intent/tool-call {:tool/id :fixture/echo :args {:text "hi"}}]
                            [:intent/memory-read {:memory/key :episode/t1}]
                            [:intent/memory-write {:memory/key :episode/t1 :memory/content {:ok true}}]
                            [:intent/finish {:value 42}]
                            [:intent/fail {:message "oops"}]]]
      (is (= (base-intent type payload) (schema/validate-intent (base-intent type payload)))
          (str type " should still validate"))))
  (testing "unknown type still rejected (schema-invalid)"
    (is-schema-invalid
     (base-intent :not-an-intent {:whatever 1}))
    (is-schema-invalid
     (assoc (base-intent :intent/tool-call {:tool/id :fixture/echo :args {}})
            :intent/type :intent/unknown-type))))
