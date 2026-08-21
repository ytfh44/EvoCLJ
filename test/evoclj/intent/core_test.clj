(ns evoclj.intent.core-test
  "Tests for the v0 Intent ABI: schemas and canonical normalization
  (component).

  An Intent is the ONLY way evolvable code requests an effect — a
  validated, immutable, plain-data map. The base shape and the six v0
  intent types are normative; the attribution fields (session,
  phenotype, node, cause event) are required by Global Constraint 20
  and never invented by a constructor or normalizer.

  Step 1 asserts that every v0 intent type validates unchanged and that
  each type's payload contract is enforced. Step 2 asserts the
  rejections: missing attribution fields, unknown intent type, Java
  objects / lazy sequences in the payload (Global Constraint 22), and
  negative budgets. Step 3 asserts normalize-intent is
  order-insensitive — semantically equal maps differing only in key
  ordering normalize to equal values with identical serialization —
  and that normalization never invents authorization (no capability,
  lease, grant, or decision keys are ever added). Step 4 asserts the
  pure constructors build full, validated, EDN-round-trippable intents
  from kernel-provided attribution."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as core]
            [evoclj.intent.schema :as schema]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private intent-id #uuid "22222222-2222-4222-8222-222222222222")
(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-id
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private node-id :node/tool)
(def ^:private cause-event-id 17)
(def ^:private budget {:wall-ms 1000})

(def ^:private payloads
  {:intent/model-call {:model/id :model/planner
                       :messages [{:role :user :content "plan the task"}]}
   :intent/tool-call {:tool/id :fixture/echo :args {:text "hello"}}
   :intent/memory-read {:memory/key :episode/task-1 :memory/limit 10}
   :intent/memory-write {:memory/key :episode/task-1 :memory/content {:result :ok}}
   :intent/finish {:value 7}
   :intent/fail {:message "task failed" :value {:reason :fixture}}})

(defn- intent-for
  "A full, valid intent map of the given v0 type."
  [type]
  {:intent/id intent-id
   :intent/type type
   :session/id session-id
   :phenotype/id phenotype-id
   :node/id node-id
   :cause/event-id cause-event-id
   :payload (get payloads type)
   :budget budget
   :metadata {}})

(def ^:private all-intents
  (mapv intent-for (keys payloads)))

(def ^:private base-keys
  #{:intent/id :intent/type :session/id :phenotype/id
    :node/id :cause/event-id :payload :budget :metadata})

;; --- shared helpers --------------------------------------------------------

(defn- intent-error
  "The ExceptionInfo thrown by normalize-intent for x, or nil when it
  normalizes successfully."
  [x]
  (try (core/normalize-intent x)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- is-schema-invalid
  "Assert normalize-intent rejects x with :intent/schema-invalid."
  [x]
  (let [e (intent-error x)]
    (is (some? e) "normalize-intent rejects the value")
    (is (= :intent/schema-invalid (:error/type (ex-data e))))))

(defn- is-not-edn-safe
  "Assert normalize-intent rejects x with :intent/not-edn-safe."
  [x]
  (let [e (intent-error x)]
    (is (some? e) "normalize-intent rejects the value")
    (is (= :intent/not-edn-safe (:error/type (ex-data e))))))

(defn- is-ctor-error
  "Assert the constructor thunk f throws an ExceptionInfo with the given
  :error/type."
  [f expected]
  (let [e (try (f) nil (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e) "constructor rejects the arguments")
    (is (= expected (:error/type (ex-data e))))))

;; ============================================================================
;; Step 1 — schema tests for every v0 intent type
;; ============================================================================

(deftest every-v0-intent-type-validates
  (testing "each of the six v0 intent types validates and is returned unchanged"
    (doseq [m all-intents]
      (is (identical? m (schema/validate-intent m))
          (pr-str (:intent/type m))))))

(deftest every-v0-intent-type-normalizes-to-itself
  (testing "a valid intent normalizes to an equal value"
    (doseq [m all-intents]
      (is (= m (core/normalize-intent m))
          (pr-str (:intent/type m))))))

(deftest per-type-payload-contracts
  (testing "tool-call payload requires keyword :tool/id and map :args"
    (is-schema-invalid
     (assoc-in (intent-for :intent/tool-call) [:payload :tool/id] "fixture/echo"))
    (is-schema-invalid
     (assoc-in (intent-for :intent/tool-call) [:payload :args] "nope")))
  (testing "model-call payload requires keyword :model/id and vector :messages"
    (is-schema-invalid
     (assoc-in (intent-for :intent/model-call) [:payload :model/id] 7))
    (is-schema-invalid
     (assoc-in (intent-for :intent/model-call) [:payload :messages] "nope")))
  (testing "memory-read payload requires keyword :memory/key"
    (is-schema-invalid
     (assoc-in (intent-for :intent/memory-read) [:payload :memory/key] 7)))
  (testing "memory-write payload requires keyword :memory/key"
    (is-schema-invalid
     (assoc-in (intent-for :intent/memory-write) [:payload :memory/key] 7)))
  (testing "finish payload requires :value"
    (is-schema-invalid (assoc (intent-for :intent/finish) :payload {})))
  (testing "fail payload requires string :message"
    (is-schema-invalid
     (assoc (intent-for :intent/fail) :payload {:value 1}))))

;; ============================================================================
;; Step 2 — rejection tests
;; ============================================================================

(deftest rejects-missing-attribution-fields
  (testing "every base field is required — none may be omitted"
    (doseq [k base-keys]
      (is-schema-invalid (dissoc (intent-for :intent/tool-call) k)))))

(deftest rejects-unknown-intent-type
  (is-schema-invalid
   (assoc (intent-for :intent/tool-call) :intent/type :intent/teleport))
  (is-schema-invalid
   (assoc (intent-for :intent/tool-call) :intent/type :not-an-intent)))

(deftest rejects-non-map-intent
  (is-schema-invalid [:intent/type :intent/tool-call])
  (is-schema-invalid "not-an-intent"))

(deftest rejects-java-object-in-payload
  (testing "a raw Java object nested in the payload cannot cross the boundary"
    (is-not-edn-safe
     (assoc-in (intent-for :intent/tool-call)
               [:payload :args :text]
               (java.io.File. "C:/tmp/secret.txt"))))
  (testing "a Java object as the payload value itself is rejected"
    (is-not-edn-safe
     (assoc (intent-for :intent/tool-call)
            :payload (java.io.File. "C:/tmp/secret.txt")))))

(deftest rejects-lazy-sequence-in-payload
  (testing "a lazy sequence is a suspended computation, not data"
    (is-not-edn-safe
     (assoc-in (intent-for :intent/tool-call)
               [:payload :args :text]
               (range)))))

(deftest rejects-negative-budget
  (testing "a negative :wall-ms budget is rejected"
    (is-schema-invalid
     (assoc-in (intent-for :intent/tool-call) [:budget :wall-ms] -1)))
  (testing "a negative :max-steps budget is rejected"
    (is-schema-invalid
     (assoc-in (intent-for :intent/tool-call) [:budget :max-steps] -5)))
  (testing "a non-integer :wall-ms budget is rejected"
    (is-schema-invalid
     (assoc-in (intent-for :intent/tool-call) [:budget :wall-ms] "fast"))))

;; ============================================================================
;; Step 3 — normalize-intent order independence and no invented authorization
;; ============================================================================

(deftest normalize-is-order-independent
  (testing "top-level and nested map ordering never changes the normalized value"
    (let [a {:payload {:args {:text "hi"} :tool/id :fixture/echo}
             :intent/id intent-id
             :cause/event-id cause-event-id
             :phenotype/id phenotype-id
             :node/id node-id
             :session/id session-id
             :budget {:wall-ms 1000}
             :metadata {}
             :intent/type :intent/tool-call}
          b {:intent/id intent-id
             :intent/type :intent/tool-call
             :session/id session-id
             :phenotype/id phenotype-id
             :node/id node-id
             :cause/event-id cause-event-id
             :payload {:tool/id :fixture/echo :args {:text "hi"}}
             :budget {:wall-ms 1000}
             :metadata {}}]
      (is (= a b) "the two source maps carry the same intent data")
      (is (= (core/normalize-intent a) (core/normalize-intent b)))
      (is (= (pr-str (core/normalize-intent a))
             (pr-str (core/normalize-intent b)))
          "canonical serialization is identical"))))

(deftest normalize-never-invents-authorization
  (let [n (core/normalize-intent (intent-for :intent/tool-call))]
    (testing "the normalized value has exactly the base Intent keys — nothing added"
      (is (= base-keys (set (keys n)))))
    (testing "no capability, lease, grant, or decision keys are introduced"
      (is (not-any? (fn [k]
                      (or (= k :cap/id) (= k :lease/id)
                          (re-find #"^(cap|lease|grant|authorization|auth)" (name k))))
                    (keys n))))
    (testing "the normalized value is plain serializable EDN"
      (is (= n (edn/read-string (pr-str n)))))))

(deftest normalize-rejects-invalid-input
  (is-schema-invalid (dissoc (intent-for :intent/tool-call) :session/id))
  (is-schema-invalid (assoc (intent-for :intent/tool-call)
                            :intent/type :intent/teleport))
  (is-not-edn-safe (assoc-in (intent-for :intent/tool-call)
                             [:payload :args] (java.io.File. "."))))

;; ============================================================================
;; Step 4 — pure helper constructors
;; ============================================================================

(deftest constructors-build-full-validated-intents
  (doseq [[type ctor]
          {:intent/model-call core/model-call
           :intent/tool-call core/tool-call
           :intent/memory-read core/memory-read
           :intent/memory-write core/memory-write
           :intent/finish core/finish
           :intent/fail core/fail}]
    (let [payload (get payloads type)
          i (ctor session-id phenotype-id node-id cause-event-id payload budget)]
      (testing (str (name type) " carries attribution, payload, and budget")
        (is (uuid? (:intent/id i)))
        (is (= type (:intent/type i)))
        (is (= session-id (:session/id i)))
        (is (= phenotype-id (:phenotype/id i)))
        (is (= node-id (:node/id i)))
        (is (= cause-event-id (:cause/event-id i)))
        (is (= payload (:payload i)))
        (is (= budget (:budget i)))
        (is (= {} (:metadata i)))
        (is (= base-keys (set (keys i)))))
      (testing (str (name type) " is validated immutable data")
        (is (= i (core/normalize-intent i)))
        (is (= i (edn/read-string (pr-str i))))))))

(deftest constructors-assign-fresh-intent-ids
  (let [payload (get payloads :intent/tool-call)]
    (is (not= (:intent/id (core/tool-call session-id phenotype-id node-id
                                          cause-event-id payload budget))
              (:intent/id (core/tool-call session-id phenotype-id node-id
                                          cause-event-id payload budget))))))

(deftest constructors-reject-invalid-attribution
  (let [payload (get payloads :intent/tool-call)]
    (testing "a malformed phenotype id fails at construction"
      (is-ctor-error #(core/tool-call session-id "not-a-hash" node-id
                                      cause-event-id payload budget)
                     :intent/schema-invalid))
    (testing "a non-uuid session id fails at construction"
      (is-ctor-error #(core/tool-call "session" phenotype-id node-id
                                      cause-event-id payload budget)
                     :intent/schema-invalid))
    (testing "a negative budget fails at construction"
      (is-ctor-error #(core/tool-call session-id phenotype-id node-id
                                      cause-event-id payload {:wall-ms -1})
                     :intent/schema-invalid))
    (testing "a payload violating the type contract fails at construction"
      (is-ctor-error #(core/tool-call session-id phenotype-id node-id
                                      cause-event-id {:tool/id "x"} budget)
                     :intent/schema-invalid))
    (testing "a Java object in the payload fails at construction"
      (is-ctor-error #(core/tool-call session-id phenotype-id node-id
                                      cause-event-id
                                      {:tool/id :fixture/echo
                                       :args {:text (java.io.File. ".")}}
                                      budget)
                     :intent/not-edn-safe))))
