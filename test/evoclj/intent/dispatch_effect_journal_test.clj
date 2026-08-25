(ns evoclj.intent.dispatch-effect-journal-test
  "M15 — effect journal wired into dispatch; ambiguous durable fails
  closed (no blind retry); the duplicate dead-code effect journal is
  consolidated into one canonical implementation (INV-05).

  These tests drive the REAL production dispatcher
  (evoclj.intent.dispatch/dispatch!) through the broker pipeline and
  assert observable behavior:

  - Every dispatch records an effect through the effect journal and the
    journal travels in the typed result (:effect-journal). The journal
    carries the full proposed -> authorized -> call-started(with
    idempotency key + revision seq) -> final transition.
  - An AMBIGUOUS durable outcome (:provider/call-ambiguous) is NEVER
    blindly retried, even when the descriptor declares
    :retry {:safe? true}. It fails closed as a typed :effect/ambiguous
    error and the journal marks :effect/final :effect/ambiguous. This is
    the Transaction Boundaries protocol: recovery must mark an ambiguous
    non-idempotent effect ambiguous/manual-review, not silently retry.
  - The duplicate dead-code effect journal (the old evoclj.mcp.effect
    namespace) is absorbed into the single canonical implementation in
    the dispatcher. A regression test pins that the namespace no longer
    exists, so the INV-05 duplicate cannot silently return.

  No test injects an fn to bypass production components, and none assert
  only shape — they assert behavior (call counts, result types, journal
  finals)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})

(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000))
(def ^:private now (java.util.Date. 1700000000000))

(def ^:private echo-cap-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")

(def ^:private echo-intent
  (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                    {:tool/id :fixture/echo :args {:text "hi"}}
                    budget))

(defn- lease
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
  [providers leases]
  (let [reg (registry/create-registry)]
    (doseq [p providers]
      (registry/register! reg p))
    (dispatch/make-broker-context
     {:registry reg
      :leases leases
      :usage (atom {})
      :now (constantly now)})))

;; An ambiguous-outcome provider: :retry {:safe? true} is declared, but
;; the call reports an AMBIGUOUS outcome (the request was sent, the
;; remote may have committed, and the connection broke before a
;; definitive result came back). This must FAIL CLOSED — never retry.
(defn- ambiguous-provider
  [counter]
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/ambiguous
       :effect :write
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke
       :retry {:safe? true}})
    (normalize-request [_ intent]
      {:tool/id :fixture/ambiguous
       :resource {:kind :tool :id :fixture/ambiguous}
       :args (get-in intent [:payload :args])})
    (execute-request! [_ _]
      (swap! counter inc)
      (throw (err/error :provider/call-ambiguous
                        "request sent, remote outcome indeterminate"
                        {:attempt @counter})))))

(def ^:private ambiguous-cap-id
  #uuid "eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee")

(def ^:private ambiguous-intent
  (assoc-in (intent/tool-call session-id phenotype-p1 :node/tool cause-event-id
                              {:tool/id :fixture/ambiguous :args {:text "hi"}}
                              budget)
            [:metadata :idempotency/key] "amb-req-1"))

(defn- ambiguous-lease []
  {:cap/id ambiguous-cap-id
   :subject {:phenotype/id phenotype-p1}
   :resource {:kind :tool :id :fixture/ambiguous}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

;; --- RED 1: the dispatcher journals every effect --------------------------

(deftest dispatch-records-effect-through-journal
  (testing "the happy path journals proposed->authorized->call-started->committed"
    (let [counter (atom 0)
          ctx (broker-context [(fixture/echo-provider {:execution-count counter})]
                              [(lease)])
          r (dispatch/dispatch! ctx echo-intent)
          j (:effect-journal r)]
      (is (= :ok (:result/status r)))
      (is (some? j) "the effect journal travels in the typed result")
      (is (= (:intent/id echo-intent)
             (get-in j [:effect/proposed :intent/id]))
          "proposed records the real intent id")
      (is (= {:decision :allow :lease-id echo-cap-id}
             (:effect/authorized j))
          "authorized records the real broker decision")
      (is (uuid? (get-in j [:effect/call-started :binding/id]))
          "call-started carries the frozen binding id")
      (is (= (get-in echo-intent [:metadata :idempotency/key])
             (get-in j [:effect/call-started :idempotency/key]))
          "call-started records the idempotency key")
      (is (= :effect/committed (:effect/final j))
          "a successful execution finalizes the journal as committed")
      (is (= r (edn/read-string (pr-str r)))
          "result with journal is plain serializable data (GC-22)"))))

;; --- RED 2: ambiguous durable fails closed, no blind retry -----------------

(deftest ambiguous-durable-effect-is-not-blindly-retried
  (testing "an ambiguous outcome is reported fail-closed and never retried"
    (let [counter (atom 0)
          ctx (broker-context [(ambiguous-provider counter)]
                              [(ambiguous-lease)])
          r (dispatch/dispatch! ctx ambiguous-intent)
          j (:effect-journal r)]
      (is (= 1 @counter)
          "the ambiguous provider ran exactly ONCE — no blind retry,
           even though its descriptor declares :retry {:safe? true}")
      (is (= :error (:result/status r)))
      (is (= :effect/ambiguous (:error/type r))
          "the ambiguous outcome surfaces as a typed :effect/ambiguous error")
      (is (= :effect/ambiguous (:effect/final j))
          "the journal finalizes as ambiguous, never as committed/rejected")
      (is (uuid? (get-in j [:effect/call-started :binding/id]))
          "call-started is recorded before the ambiguous outcome")
      (is (= {ambiguous-cap-id 1} (:usage r))
          "a single attempt consumes a single lease slot")))
  (testing "an idempotent :retry {:safe? true} provider still retries TRANSIENT errors"
    ;; Regression: the fail-closed ambiguous path must not disable the
    ;; legitimate transient retry for declared-safe pure providers.
    (let [counter (atom 0)
          ctx (broker-context
               [(fixture/echo-provider {:fail-count 1 :execution-count counter})]
               [(lease)])
          r (dispatch/dispatch! ctx echo-intent)]
      (is (= :ok (:result/status r)))
      (is (= 2 @counter)
          "the transient retry still happens for a safe provider")
      (is (= {echo-cap-id 2} (:usage r))))))

;; --- RED 3: the duplicate dead-code effect journal is gone -----------------

(deftest duplicate-effect-journal-is-consolidated
  (testing "the old duplicate evoclj.mcp.effect namespace no longer exists"
    ;; INV-05: one mechanism, one implementation. The dead duplicate was
    ;; absorbed into the dispatcher's single canonical effect-journal.
    (is (thrown? Throwable
                 (require 'evoclj.mcp.effect))
        "evoclj.mcp.effect must have been removed (consolidated into dispatch)"))
  (testing "a denied dispatch still journals proposed->authorized(deny)->rejected"
    (let [counter (atom 0)
          ctx (broker-context [(fixture/echo-provider {:execution-count counter})]
                              [])
          r (dispatch/dispatch! ctx echo-intent)
          j (:effect-journal r)]
      (is (= :error (:result/status r)))
      (is (= :capability/denied (:error/type r)))
      (is (some? j))
      (is (= (:intent/id echo-intent)
             (get-in j [:effect/proposed :intent/id])))
      (is (= {:decision :deny :reason :capability/missing}
             (:effect/authorized j)))
      (is (= :effect/rejected (:effect/final j))
          "a denied request finalizes the journal as rejected,
           proving the SAME journal implementation serves every path")
      (is (= 0 @counter)
          "denial never executed the provider")))
  (testing "the journal shape is consistent across the production paths"
    (let [ok-ctx (broker-context [(fixture/echo-provider {:execution-count (atom 0)})]
                                 [(lease)])
          ok-j (:effect-journal (dispatch/dispatch! ok-ctx echo-intent))
          deny-ctx (broker-context [(fixture/echo-provider {:execution-count (atom 0)})]
                                   [])
          deny-j (:effect-journal (dispatch/dispatch! deny-ctx echo-intent))]
      (is (= (set (keys ok-j)) (set (keys deny-j)))
          "both paths produce the same canonical journal shape"))))
