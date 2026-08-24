(ns evoclj.mcp.latency-writeback-test
  "M10 — latency-ms measured and written back (RED → GREEN).

   Every assertion here drives the REAL production
   `evoclj.mcp.client/call-tool` (and `call-tool-managed`) over a genuine
   stdio MCP subprocess spawned by the fake-server harness — no fn
   injection, no replication of production timing logic (INV-09). The ONE
   approved harness is the fake MCP server, exactly as M9 used it.

   The production contract under test:
   - `call-tool` measures the ACTUAL wall-clock elapsed time around the
     real `.callTool` SDK round-trip and writes it back as
     `:mcp/latency-ms` on the result map (a non-negative long reflecting
     real elapsed time, never nil/hardcoded on the success path).
   - On a failed/unmeasurable call `call-tool` does NOT emit any latency,
     so no bogus positive latency can be read back.
   - `call-tool-managed` reuses that measured latency (no re-measurement
     wrapper) and, on failure, writes no `:mcp/last-latency-ms`.
   - The provider bridge writes the REAL measured latency onto the pooled
     entry's runtime stats (replacing the former hardcoded `:latency-ms 0`
     placeholder)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.client :as mcp]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.support.fake-server :as fake]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]))

;; --- tiny helpers ----------------------------------------------------------

(defn- capture-throw
  "Call `f`; return ::no-throw if it returned normally, else the Throwable."
  [f]
  (try (f) ::no-throw (catch Throwable t t)))

(defn- elapsed-ms
  [t0]
  (/ (- (System/nanoTime) t0) 1e6))

(def ^:private slow-delay-ms 900)

;; ---------------------------------------------------------------------------
;; Path 1 — happy path: a real op yields a plausible positive :mcp/latency-ms
;; ---------------------------------------------------------------------------

(deftest call-tool-writes-back-measured-latency-happy-path
  (testing "a real production call-tool over the fake server returns a measured :mcp/latency-ms that is a non-negative number (not nil, not hardcoded)"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [t0 (System/nanoTime)
                result (mcp/call-tool (:client managed) "echo" {:n 1})
                measured (elapsed-ms t0)]
            (is (number? (:mcp/latency-ms result))
                ":mcp/latency-ms is present on the call-tool result")
            (is (not (nil? (:mcp/latency-ms result)))
                ":mcp/latency-ms is not nil")
            (is (>= (:mcp/latency-ms result) 0)
                ":mcp/latency-ms is non-negative")
            (is (<= (:mcp/latency-ms result) (+ measured 500))
                (str ":mcp/latency-ms ("
                     (:mcp/latency-ms result)
                     " ms) must not exceed our own wall measurement ("
                     measured " ms) by more than slack")))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; Path 2 — branch: latency is REAL, not a constant (prove via slow knob)
;; ---------------------------------------------------------------------------

(deftest call-tool-latency-reflects-real-elapsed-not-constant
  (testing "with a real server-side delay, the measured :mcp/latency-ms is >= the injected delay (proves it is the real elapsed time, not a hardcoded value)"
    (fake/with-fake-server [srv {:mode :slow :delay-ms slow-delay-ms}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [result (mcp/call-tool (:client managed) "echo" {:n 1})]
            (is (>= (:mcp/latency-ms result) (* 0.9 slow-delay-ms))
                (str "measured latency "
                     (:mcp/latency-ms result)
                     " ms must be >= the injected delay "
                     slow-delay-ms " ms (setTimeout never fires early)")))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; Path 3 — branch: latency is ABSENT when the op is unmeasurable (failure)
;; ---------------------------------------------------------------------------

(deftest call-tool-failure-carries-no-latency
  (testing "when call-tool fails (args guard / transport failure) the failure carries NO :mcp/latency-ms — fail-closed, no bogus positive latency"
    ;; 3a: the public args guard throws before any SDK call (non-map args).
    (let [e (capture-throw #(mcp/call-tool nil "echo" []))]
      (is (not= ::no-throw e) "args-guard raised")
      (is (= :mcp/call-invalid (:error/type (ex-data e))))
      (is (nil? (:mcp/latency-ms (ex-data e)))
          "the args-guard failure carries no :mcp/latency-ms"))
    ;; 3b: a transport-level failure (nil client -> NPE in the SDK call)
    ;; also yields no latency on the thrown error.
    (let [e (capture-throw #(mcp/call-tool nil "echo" {}))]
      (is (not= ::no-throw e) "transport failure raised")
      (is (some? (:error/type (ex-data e)))
          "a typed error was produced")
      (is (nil? (:mcp/latency-ms (ex-data e)))
          "transport failure carries no :mcp/latency-ms"))))

;; ---------------------------------------------------------------------------
;; Path 4 — fault: call-tool-managed error yields no bogus :mcp/last-latency-ms
;; ---------------------------------------------------------------------------

(deftest call-tool-managed-failure-writes-no-bogus-latency
  (testing "call-tool-managed that throws does NOT write a :mcp/last-latency-ms onto the managed record or the error"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [managed (mcp/open! (:config srv))
            e (atom nil)]
        (try
          ;; call with non-map args -> call-tool's args guard throws, and
          ;; the managed wrapper must NOT emit a :mcp/last-latency-ms.
          (mcp/call-tool-managed managed "echo" [])
          (is false "expected exception")
          (catch Throwable t
            (reset! e t))
          (finally
            (mcp/close! managed)))
        (let [data (ex-data @e)]
          (is (some? @e) "call-tool-managed raised")
          (is (nil? (:mcp/last-latency-ms data))
              "the managed failure carries no :mcp/last-latency-ms")
          ;; The managed record carried in the error must also be intact
          ;; (its :last-latency-ms is the pre-call value, not a fabricated
          ;; positive number).
          (when-let [m (:managed data)]
            (is (nil? (:last-latency-ms m))
                "managed record in the error keeps no bogus :last-latency-ms")))))))

;; ---------------------------------------------------------------------------
;; Path 5 — fault: extremely fast op yields a small non-negative number
;; ---------------------------------------------------------------------------

(deftest call-tool-fast-op-yields-small-nonnegative-latency
  (testing "a fast success op yields a non-negative :mcp/latency-ms (>= 0) and is well within the slow-window — never negative, never a huge constant"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [result (mcp/call-tool (:client managed) "echo" {})]
            (is (>= (:mcp/latency-ms result) 0)
                "fast op latency is non-negative (floored at 0)")
            (is (< (:mcp/latency-ms result) slow-delay-ms)
                (str "fast op latency "
                     (:mcp/latency-ms result)
                     " ms is well under the slow-window "
                     slow-delay-ms " ms")))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; Path 6 — regression: the old unmeasured/hardcoded path is GONE
;; ---------------------------------------------------------------------------

(deftest call-tool-result-always-carries-latency-regression
  (testing "regression: call-tool NEVER returns a result without :mcp/latency-ms on the success path (the prior unmeasured/hardcoded path is gone)"
    (fake/with-fake-server [srv {:mode :ok :tool-count 2}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (doseq [i (range 2)]
            (let [result (mcp/call-tool (:client managed)
                                         (str "fake-tool-" i) {:n i})]
              (is (contains? result :mcp/latency-ms)
                  (str "call-tool result for fake-tool-" i " carries :mcp/latency-ms"))
              (is (number? (:mcp/latency-ms result))
                  ":mcp/latency-ms is a real number, not absent/nil")))
          (finally
            (mcp/close! managed)))))))

(deftest call-tool-managed-reuses-measured-latency-not-rewrapper
  (testing "call-tool-managed surfaces the SAME :mcp/latency-ms that call-tool measured (no double-counted wrapper overhead, no hardcoded 0)"
    (fake/with-fake-server [srv {:mode :slow :delay-ms slow-delay-ms}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [result (mcp/call-tool-managed managed "echo" {:n 1})]
            (is (number? (:mcp/last-latency-ms result))
                ":mcp/last-latency-ms is present on the managed result")
            ;; It must reflect the real delay, not a hardcoded placeholder 0.
            (is (>= (:mcp/last-latency-ms result) (* 0.9 slow-delay-ms))
                (str "managed :mcp/last-latency-ms "
                     (:mcp/last-latency-ms result)
                     " reflects the real delay, not the old hardcoded 0")))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; Path 6 (bridge level) — regression: pooled entry latency is REAL, not the
;; old hardcoded 0 placeholder (drives the genuine provider+manager path).
;; ---------------------------------------------------------------------------

(deftest bridge-pooled-entry-latency-uses-real-measured-value-not-hardcoded-zero
  (testing "the provider bridge writes the REAL measured latency (not the old hardcoded 0) onto the pooled entry's runtime stats after a real slow call"
    (fake/with-fake-server [srv {:mode :slow :delay-ms slow-delay-ms}]
      (let [mgr (manager/create-manager)
            conn-id :m2/shared
            ck (manager/connection-key (assoc (:config srv) :connection/id conn-id))
            provider (mcp-bridge/mcp-provider
                       {:transport-config (:config srv)
                        :tool/id :m2/echo
                        :tool/mcp-name "echo"
                        :input-schema [:map]
                        :output-schema [:map]
                        :connection/id conn-id
                        :mcp/server-id :fake
                        :manager mgr})
            req (proto/normalize-request provider
                                         {:payload {:tool/id :m2/echo :args {}}})]
        (try
          (let [result (proto/execute-request! provider req)
                entry (manager/pool-get mgr ck)]
            (is (some? result) "execute-request! returned a result")
            (is (some? entry) "a pooled entry exists for the shared connection")
            (let [metrics (:metrics entry)]
              (is (some? metrics) "pooled entry carries :metrics")
              ;; The latency must reflect the real injected delay, proving
              ;; the old `(assoc :latency-ms 0)` placeholder is gone.
              (is (>= (:latency-ms metrics) (* 0.9 slow-delay-ms))
                  (str "pooled :latency-ms "
                       (:latency-ms metrics)
                       " reflects the real delay, not the old hardcoded 0"))))
          (finally
            (manager/release mgr ck :m2/echo)))))))
