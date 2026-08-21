(ns evoclj.intent.dispatch-test
  "Tests for the effectful intent dispatcher (component).

  dispatch! is the effectful wrapper that turns an authorized Intent
  into a real provider effect, in the NORMATIVE dispatcher order
  (component Step 5): validate intent -> lookup provider -> normalize
  resource -> authorize -> execute once/retry per policy -> validate
  output -> return a typed result. It returns

    {:result/status :ok :intent/id ... :value ... :authorization ...
     :usage ...}

  or a typed error result {:result/status :error :error/type ...}.
  Authorization is PURE (evoclj.capability.broker) with a usage atom
  for call counts; only the dispatcher is effectful.

  Step 1 asserts an allowed fixture echo returns the typed :ok result.
  Step 2 asserts a DENIED request never reaches the provider: the
  fixture echo provider keeps an execution counter that must not
  increment on denial (Global Constraint 9 — a visible, requestable
  tool is not a grant). Step 3 asserts a simulated transient error
  retries the pure/idempotent echo fixture (:retry {:safe? true}) but
  NOT a non-idempotent fixture whose descriptor declares no :retry
  block (automatic retries are allowed only for :retry {:safe? true},
  and a non-idempotent action must never be blindly retried — the
  Transaction Boundaries protocol). Step 4 asserts malformed provider
  output is :provider/output-invalid and is never accepted as
  model-visible data. The Milestone 4 exit test asserts an intent
  CONSTRUCTED INSIDE the SCI sandbox (evo.api.intent / the M3 route
  fixture) is inert data until the host broker dispatches it, and a
  visible-but-ungranted tool is consistently denied."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.sci.context :as context]
            [sci.core :as sci]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private phenotype-p2
  "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})

;; A lease window that contains any realistic decision instant, so the
;; tests can pin the clock with (constantly now) or use the default.
(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000)) ; year 2100
(def ^:private now (java.util.Date. 1700000000000))

(def ^:private echo-cap-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")

(def ^:private echo-intent
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/echo :args {:text "hi"}}
                    budget))

(defn- lease
  "A valid :fixture/echo lease for phenotype-p1, optionally with
  assoc-style overrides."
  [& kvs]
  (let [base {:cap/id echo-cap-id
              :subject {:phenotype/id phenotype-p1}
              :resource {:kind :tool :id :fixture/echo}
              :actions #{:invoke}
              :constraints {:max-calls 10}
              :issued-at issued-at
              :expires-at expires-at}]
    (if (seq kvs) (apply assoc base kvs) base)))

(defn- broker-context
  "A dispatcher broker context over a fresh registry with the given
  providers registered, the given leases, a fresh usage atom, and a
  pinned decision clock."
  [providers leases]
  (let [reg (registry/create-registry)]
    (doseq [p providers]
      (registry/register! reg p))
    (dispatch/make-broker-context
     {:registry reg
      :leases leases
      :usage (atom {})
      :now (constantly now)})))

;; --- pathological test doubles (host bugs the dispatcher must catch) -------

(defn- broken-output-provider
  "A provider whose execute-request! returns a value violating its own
  :output-schema — a host-side bug the dispatcher must surface as
  :provider/output-invalid instead of trusting the value as
  model-visible data (component Step 4)."
  [counter]
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/broken-echo
       :effect :pure
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke})
    (normalize-request [_ intent]
      {:tool/id :fixture/broken-echo
       :resource {:kind :tool :id :fixture/broken-echo}
       :args (get-in intent [:payload :args])})
    (execute-request! [_ _]
      (swap! counter inc)
      {:text 42})))

(defn- write-provider
  "A non-pure (:effect :write) provider: per the component interface, a
  non-pure write must carry an idempotency key before execution, so the
  dispatcher must refuse to run it without one."
  [counter]
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/write
       :effect :write
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke})
    (normalize-request [_ intent]
      {:tool/id :fixture/write
       :resource {:kind :tool :id :fixture/write}
       :args (get-in intent [:payload :args])})
    (execute-request! [_ request]
      (swap! counter inc)
      {:text (get-in request [:args :text])})))

;; ============================================================================
;; Step 1 — allowed fixture echo
;; ============================================================================

(deftest allowed-echo-intent-returns-typed-ok-result
  (let [counter (atom 0)
        ctx (broker-context [(fixture/echo-provider {:execution-count counter})]
                            [(lease)])]
    (testing "dispatch! executes the authorized echo intent"
      (let [r (dispatch/dispatch! ctx echo-intent)]
        (is (= :ok (:result/status r)))
        (is (= (:intent/id echo-intent) (:intent/id r)))
        (is (= {:text "hi"} (:value r)))
        (is (= {:decision :allow :lease-id echo-cap-id} (:authorization r)))
        (is (= {echo-cap-id 1} (:usage r))
            "one authorized call consumes one slot of the lease budget")
        (is (= 1 @counter) "the provider really ran once"))
      (testing "results are plain serializable EDN (Global Constraint 22)"
        (let [r (dispatch/dispatch! ctx echo-intent)]
          (is (= r (edn/read-string (pr-str r)))))))))

;; ============================================================================
;; Step 2 — denied request never reaches the provider
;; ============================================================================

(deftest denied-intent-never-increments-the-provider-counter
  (testing "a visible-but-ungranted tool is denied without executing
            (Global Constraint 9)"
    (let [counter (atom 0)
          ctx (broker-context [(fixture/echo-provider {:execution-count counter})]
                              [])]
      (let [r (dispatch/dispatch! ctx echo-intent)]
        (is (= :error (:result/status r)))
        (is (= :capability/denied (:error/type r)))
        (is (= {:decision :deny :reason :capability/missing} (:authorization r)))
        (is (= 0 @counter)
            "the provider execution counter must NOT increment on denial")
        (is (= {} (:usage r))
            "a denied request consumes no lease budget")
        (is (= r (edn/read-string (pr-str r)))))))
  (testing "a wrong-subject lease also denies without executing"
    (let [counter (atom 0)
          ctx (broker-context [(fixture/echo-provider {:execution-count counter})]
                              [(lease :subject {:phenotype/id phenotype-p2})])]
      (let [r (dispatch/dispatch! ctx echo-intent)]
        (is (= :error (:result/status r)))
        (is (= :capability/subject-mismatch (get-in r [:authorization :reason])))
        (is (= 0 @counter))))))

;; ============================================================================
;; Step 3 — retry a pure/idempotent fixture, never a non-idempotent one
;; ============================================================================

(def ^:private non-idempotent-cap-id #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")

(def ^:private non-idempotent-intent
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/non-idempotent :args {:text "hi"}}
                    budget))

(defn- non-idempotent-lease
  []
  {:cap/id non-idempotent-cap-id
   :subject {:phenotype/id phenotype-p1}
   :resource {:kind :tool :id :fixture/non-idempotent}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(deftest transient-failures-retry-only-declared-safe-providers
  (testing "a pure/idempotent fixture declaring :retry {:safe? true} is retried"
    (let [counter (atom 0)
          ctx (broker-context
               [(fixture/echo-provider {:fail-count 1 :execution-count counter})]
               [(lease)])]
      (let [r (dispatch/dispatch! ctx echo-intent)]
        (is (= :ok (:result/status r)))
        (is (= {:text "hi"} (:value r))
            "the retried attempt returns the real provider value")
        (is (= 2 @counter)
            "the first attempt failed transiently and the retry succeeded")
        (is (= {echo-cap-id 2} (:usage r))
            "each provider attempt consumes one slot of the lease budget"))))
  (testing "a provider with NO :retry {:safe? true} is never retried"
    (let [counter (atom 0)
          ctx (broker-context
               [(fixture/non-idempotent-provider {:fail-count 1
                                                  :execution-count counter})]
               [(non-idempotent-lease)])]
      (let [r (dispatch/dispatch! ctx non-idempotent-intent)]
        (is (= :error (:result/status r)))
        (is (= :provider/transient-error (:error/type r)))
        (is (= 1 @counter)
            "exactly one attempt: a non-idempotent action is never
            blindly retried (Transaction Boundaries protocol)")
        (is (= {non-idempotent-cap-id 1} (:usage r)))
        (is (= r (edn/read-string (pr-str r)))))))
  (testing "a safe provider retries only up to max-attempts, then reports
            the transient failure as a typed error result"
    (let [counter (atom 0)
          ctx (dispatch/make-broker-context
               {:registry (let [reg (registry/create-registry)]
                            (registry/register! reg
                              (fixture/echo-provider {:fail-count 5
                                                      :execution-count counter}))
                            reg)
                :leases [(lease)]
                :usage (atom {})
                :now (constantly now)
                :max-attempts 3})]
      (let [r (dispatch/dispatch! ctx echo-intent)]
        (is (= :error (:result/status r)))
        (is (= :provider/transient-error (:error/type r)))
        (is (= 3 @counter) "three attempts, then the transient failure is reported")
        (is (= {echo-cap-id 3} (:usage r)))))))

;; ============================================================================
;; Step 4 — malformed provider output is :provider/output-invalid
;; ============================================================================

(def ^:private broken-cap-id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")

(def ^:private broken-intent
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/broken-echo :args {:text "hi"}}
                    budget))

(defn- broken-lease
  []
  {:cap/id broken-cap-id
   :subject {:phenotype/id phenotype-p1}
   :resource {:kind :tool :id :fixture/broken-echo}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(deftest malformed-provider-output-is-rejected
  (let [counter (atom 0)
        ctx (broker-context [(broken-output-provider counter)]
                            [(broken-lease)])]
    (testing "a provider output violating :output-schema is rejected"
      (let [r (dispatch/dispatch! ctx broken-intent)]
        (is (= :error (:result/status r)))
        (is (= :provider/output-invalid (:error/type r)))
        (is (nil? (:value r))
            "the invalid output is never accepted as model-visible data")
        (is (= 1 @counter)
            "the provider ran, but its output was rejected at the boundary")
        (testing "the invalid output appears only in sanitized error data"
          (is (contains? (:error/data r) :output))
          (is (contains? (:error/data r) :explanation)))
        (is (= r (edn/read-string (pr-str r))))))))

;; ============================================================================
;; Interface — non-pure writes require an idempotency key before execution
;; ============================================================================

(def ^:private write-cap-id #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd")

(defn- write-lease
  []
  {:cap/id write-cap-id
   :subject {:phenotype/id phenotype-p1}
   :resource {:kind :tool :id :fixture/write}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(deftest non-pure-writes-require-an-idempotency-key
  (let [counter (atom 0)
        ctx (broker-context [(write-provider counter)] [(write-lease)])]
    (testing "a non-pure write without an idempotency key is refused before
              execution"
      (let [r (dispatch/dispatch! ctx
                                  (intent/tool-call session-id phenotype-p1
                                                    :node/tool cause-event-id
                                                    {:tool/id :fixture/write
                                                     :args {:text "x"}}
                                                    budget))]
        (is (= :error (:result/status r)))
        (is (= :intent/idempotency-key-missing (:error/type r)))
        (is (= 0 @counter) "the write provider never executed")))
    (testing "the same write with an idempotency key in :metadata executes"
      (let [keyed (assoc-in (intent/tool-call session-id phenotype-p1
                                              :node/tool cause-event-id
                                              {:tool/id :fixture/write
                                               :args {:text "x"}}
                                              budget)
                            [:metadata :idempotency/key] "req-1")
            r (dispatch/dispatch! ctx keyed)]
        (is (= :ok (:result/status r)))
        (is (= {:text "x"} (:value r)))
        (is (= 1 @counter))
        (is (= {write-cap-id 1} (:usage r)))))))

;; ============================================================================
;; Milestone 4 exit test — SCI constructs the Intent, the host broker
;; turns it into an effect, visible-but-ungranted tools are denied
;; ============================================================================

(deftest sci-constructed-intents-are-executed-only-by-the-host-broker
  (let [route-source (slurp (io/resource
                             "fixtures/genomes/minimal-valid/programs/route.clj"))
        sci-ctx (context/make-context {})]
    (testing "a SCI program constructs a plain Intent value inside the sandbox"
      (let [sci-intent (sci/eval-string*
                        sci-ctx
                        "(evo.api.intent/tool-call {:tool/id :fixture/echo
                                                     :args {:text \"hi\"}})")]
        (is (= {:intent/type :intent/tool-call
                :payload {:tool/id :fixture/echo :args {:text "hi"}}}
               sci-intent))
        (is (= sci-intent (edn/read-string (pr-str sci-intent)))
            "the SCI-visible Intent is inert plain data (Global Constraint 22)")))
    (testing "the M3 route fixture emits the same Intent shape from inside SCI"
      (is (= {:action {:intent/type :intent/tool-call
                       :payload {:tool/id :fixture/echo :args {:text "hi"}}}}
             (context/run-form sci-ctx route-source 'agent.route/run
                               {:op :echo :text "hi"}))))
    (testing "only the host broker can turn the SCI-constructed Intent into a
              fixture effect"
      (let [routed (context/run-form sci-ctx route-source 'agent.route/run
                                     {:op :echo :text "hi"})
            payload (get-in routed [:action :payload])
            host-intent (intent/tool-call session-id phenotype-p1 :node/tool
                                          cause-event-id payload budget)]
        (testing "with a lease, dispatch! executes the SCI-constructed intent"
          (let [counter (atom 0)
                ctx (broker-context
                     [(fixture/echo-provider {:execution-count counter})]
                     [(lease)])
                r (dispatch/dispatch! ctx host-intent)]
            (is (= :ok (:result/status r)))
            (is (= {:text "hi"} (:value r)))
            (is (= 1 @counter))))
        (testing "without a lease, the visible-but-ungranted tool is
                  consistently denied and never executed"
          (let [counter (atom 0)
                ctx (broker-context
                     [(fixture/echo-provider {:execution-count counter})]
                     [])
                r (dispatch/dispatch! ctx host-intent)]
            (is (= :error (:result/status r)))
            (is (= :capability/denied (:error/type r)))
            (is (= 0 @counter))))))))
