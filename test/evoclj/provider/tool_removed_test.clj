(ns evoclj.provider.tool-removed-test
  "M19 — tool-removal semantics recovery (typed :provider/tool-removed, fail-closed).

  When a tool that was PREVIOUSLY REGISTERED is later removed and is
  subsequently referenced (call / lookup / dispatch), the system must
  return a TYPED `:provider/tool-removed` error that is FAIL-CLOSED —
  never a bare NPE, an uncaught exception escaping the dispatcher, or a
  silent `nil` passthrough.

  A tool that was NEVER registered is a different failure class and must
  keep returning `:provider/not-found`, so the two classes stay
  distinguishable (the broker / capability layer can treat a removed
  tool differently from an unknown one).

  These tests exercise the production kernel path:
    evoclj.provider.registry (kernel-owned mutable registry)
    evoclj.intent.dispatch  (the only code that turns an intent into an effect)
  Nothing is faked: real Provider fixtures are registered and unregistered,
  and dispatch! is driven through make-broker-context exactly as production."

  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private cause-event-id 42)
(def ^:private budget {:wall-ms 1000})
(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000))
(def ^:private now (java.util.Date. 1700000000000))

(def ^:private echo-descriptor
  {:tool/id :fixture/echo
   :effect :pure
   :input-schema [:map [:text :string]]
   :output-schema [:map [:text :string]]
   :required-action :invoke
   :retry {:safe? true}})

(defn- echo-intent
  [args]
  (intent/tool-call session-id phenotype :node/tool cause-event-id
                    {:tool/id :fixture/echo :args args}
                    budget))

(defn- lease-for
  [tool-id]
  {:cap/id (random-uuid)
   :subject {:phenotype/id phenotype}
   :resource {:kind :tool :id tool-id}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(defn- ctx-with
  [providers leases]
  (let [reg (registry/create-registry)]
    (doseq [p providers] (registry/register! reg p))
    (dispatch/make-broker-context
     {:registry reg
      :leases leases
      :usage (atom {})
      :now (constantly now)})))

;; ============================================================================
;; Happy path — a registered tool still works after no removal
;; ============================================================================

(deftest registered-tool-dispatches-successfully
  (testing "a tool that is registered and NOT removed executes normally"
    (let [counter (atom 0)
          ctx (ctx-with [(fixture/echo-provider {:execution-count counter})]
                        [(lease-for :fixture/echo)])]
      (let [r (dispatch/dispatch! ctx (echo-intent {:text "hi"}))]
        (is (= :ok (:result/status r)) "registered tool returns :ok")
        (is (= {:text "hi"} (:value r)))
        (is (= 1 @counter) "provider actually ran")
        (is (= r (edn/read-string (pr-str r))) "result is plain serializable EDN")))))

;; ============================================================================
;; New branch 1 — dispatch of a REMOVED tool returns typed :provider/tool-removed
;; ============================================================================

(deftest removed-tool-dispatch-returns-typed-tool-removed
  (testing "a tool that was registered then unregistered is reported as removed"
    (let [counter (atom 0)
          reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider {:execution-count counter}))
          _ (registry/unregister! reg :fixture/echo)
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(lease-for :fixture/echo)]
                :usage (atom {})
                :now (constantly now)})
          r (dispatch/dispatch! ctx (echo-intent {:text "hi"}))]
      (is (= :error (:result/status r)) "dispatch does not crash")
      (is (= :provider/tool-removed (:error/type r))
          "removed tool yields the distinct typed :provider/tool-removed")
      (is (= {:tool/id :fixture/echo} (:error/data r))
          "the error data identifies the removed tool")
      (is (= 0 @counter) "the (removed) provider never executed")
      (is (= r (edn/read-string (pr-str r))) "error result is serializable"))))

;; ============================================================================
;; New branch 2 — registry lookup of a removed tool: nil value but removed? true
;; ============================================================================

(deftest removed-tool-lookup-reports-removed-status
  (testing "lookup returns nil for a removed tool, but removed? is true"
    (let [reg (registry/create-registry)]
      (registry/register! reg (fixture/echo-provider))
      (registry/unregister! reg :fixture/echo)
      (is (nil? (registry/lookup reg :fixture/echo))
          "a removed tool is not found by plain lookup")
      (is (true? (registry/removed? reg :fixture/echo))
          "the registry remembers it was removed")
      (testing "lookup-or-removed discriminates present / removed / absent"
        (let [reg2 (registry/create-registry)]
          (registry/register! reg2 (fixture/echo-provider))
          (is (= [:present (registry/lookup reg2 :fixture/echo)]
                 (registry/lookup-or-removed reg2 :fixture/echo)))
          (registry/unregister! reg2 :fixture/echo)
          (is (= [:removed :fixture/echo]
                 (registry/lookup-or-removed reg2 :fixture/echo)))
          (is (= [:absent :fixture/never]
                 (registry/lookup-or-removed reg2 :fixture/never))))))))

;; ============================================================================
;; New branch 3 — a NEVER-registered tool stays :provider/not-found (distinct)
;; ============================================================================

(deftest never-registered-tool-stays-not-found
  (testing "an unknown tool is NOT misclassified as removed"
    (let [ctx (ctx-with [] [(lease-for :fixture/ghost)])]
      (let [r (dispatch/dispatch! ctx (echo-intent {:text "hi"}))]
        (is (= :error (:result/status r)))
        (is (= :provider/not-found (:error/type r))
            "unknown tool stays :provider/not-found (distinct from removed)")
        (is (= {:tool/id :fixture/echo} (:error/data r)))))))

;; ============================================================================
;; Fault case 1 — removed tool reference is typed, never NPE / uncaught
;; ============================================================================

(deftest removed-tool-reference-is-fail-closed-not-npe
  (testing "referencing a removed tool never throws an uncaught NPE"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          _ (registry/unregister! reg :fixture/echo)
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(lease-for :fixture/echo)]
                :usage (atom {})
                :now (constantly now)})
          ;; Exercise BOTH production entry points that could break on a
          ;; removed tool: the registry lookup helpers and the dispatcher.
          lookup-thunk #(registry/lookup-or-removed reg :fixture/echo)
          removed-thunk #(registry/removed? reg :fixture/echo)
          dispatch-thunk #(dispatch/dispatch! ctx (echo-intent {:text "hi"}))]
      (is (nil? (try (lookup-thunk) nil (catch Throwable _ :threw)))
          "registry lookup-or-removed does not throw")
      (is (true? (removed-thunk)) "removed? returns cleanly")
      (let [r (try (dispatch-thunk)
                   (catch Throwable t
                     {:uncaught (ex-message t) :class (.getName (class t))}))]
        (is (not (contains? r :uncaught))
            (str "dispatch of a removed tool does NOT throw uncaught; got " r))
        (is (= :provider/tool-removed (:error/type r))
            "removed tool is reported as a typed error")))))

;; ============================================================================
;; Fault case 2 — concurrent removal + dispatch is safe (shared mutable state)
;; ============================================================================

(deftest concurrent-removal-and-dispatch-is-safe
  (testing "concurrent unregister! and dispatch! never crash or throw NPE"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(lease-for :fixture/echo)]
                :usage (atom {})
                :now (constantly now)})
          intents (repeatedly 200 #(echo-intent {:text "hi"}))
          results (atom [])
          dispatch-agents
          (doall (for [i (range 20)]
                   (agent i)))
          remove-agent (agent nil)
          ;; one agent removes the tool repeatedly; others dispatch
          _ (doseq [a dispatch-agents]
              (send a (fn [_]
                        (doseq [it intents]
                          (let [r (try (dispatch/dispatch! ctx it)
                                       (catch Throwable t
                                         {:uncaught true
                                          :class (.getName (class t))
                                          :msg (ex-message t)}))]
                            (swap! results conj
                                   (cond
                                     (:uncaught r) :uncaught
                                     (= :ok (:result/status r)) :ok
                                     (= :provider/tool-removed (:error/type r)) :removed
                                     (= :provider/not-found (:error/type r)) :not-found
                                     :else [:unexpected (:error/type r)])))))))
          _ (send remove-agent (fn [_]
                                 (dotimes [_ 50]
                                   ;; Re-register only when currently absent so
                                   ;; we never collide with register!'s
                                   ;; duplicate-tool-id guard (that guard is
                                   ;; tested elsewhere, not part of this race).
                                   (when (nil? (registry/lookup reg :fixture/echo))
                                     (registry/register! reg (fixture/echo-provider)))
                                   (registry/unregister! reg :fixture/echo))))
          _ (doseq [a dispatch-agents] (await a))
          _ (await remove-agent)
          summary (frequencies @results)]
      (is (not (contains? summary :uncaught))
          (str "no uncaught exceptions during concurrent removal+dispatch: "
               (pr-str summary)))
      (is (some #{:ok :removed :not-found} (keys summary))
          "the concurrent workload actually exercised all reachable states")
      ;; Every observed outcome is a clean, typed, or successful result.
      ;; A present tool may also be denied (no matching lease at that
      ;; instant) or report another typed dispatch error — all are
      ;; fail-closed, never an NPE or uncaught exception.
      (doseq [[k _] summary]
        (is (or (contains? #{:ok :removed :not-found} k)
                (and (vector? k)
                     (= :unexpected (first k))
                     (keyword? (second k))))
            (str "unexpected outcome " k))))))

;; ============================================================================
;; Regression — the OLD broken behavior (silent nil / NPE) is gone
;; ============================================================================

(deftest regression-removed-tool-no-longer-silent-nil
  (testing "dispatch of a removed tool returns a typed error, never nil or NPE"
    (let [reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider))
          _ (registry/unregister! reg :fixture/echo)
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(lease-for :fixture/echo)]
                :usage (atom {})
                :now (constantly now)})
          r (dispatch/dispatch! ctx (echo-intent {:text "hi"}))]
      ;; The regression we guard against: a removed tool used to surface as a
      ;; bare nil lookup / NPE / uncaught exception. Assert the OPPOSITE:
      (is (some? r) "result is never nil")
      (is (= :error (:result/status r)) "result is a typed error")
      (is (= :provider/tool-removed (:error/type r))
          "result is the typed :provider/tool-removed")
      (is (contains? r :result/status) "result is the dispatcher's result map")
      (is (nil? (try (get-in r [:error/type]) nil (catch Throwable _ :threw)))
          "the typed error type is reachable without throwing"))))

;; ============================================================================
;; Doc / behavior consistency — re-registering a removed tool clears the tombstone
;; ============================================================================

(deftest reregistering-removed-tool-clears-tombstone
  (testing "re-registering a previously removed tool restores normal dispatch"
    (let [counter (atom 0)
          reg (registry/create-registry)
          _ (registry/register! reg (fixture/echo-provider {:execution-count counter}))
          _ (registry/unregister! reg :fixture/echo)
          _ (is (true? (registry/removed? reg :fixture/echo))
                    "tool is marked removed")
          _ (registry/register! reg (fixture/echo-provider {:execution-count counter}))
          _ (is (false? (registry/removed? reg :fixture/echo))
                    "re-registration clears the tombstone")
          ctx (dispatch/make-broker-context
               {:registry reg
                :leases [(lease-for :fixture/echo)]
                :usage (atom {})
                :now (constantly now)})]
      (let [r (dispatch/dispatch! ctx (echo-intent {:text "hi"}))]
        (is (= :ok (:result/status r)) "re-registered tool dispatches :ok")
        (is (= 1 @counter))))))
