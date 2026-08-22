(ns evoclj.mcp.support.real-server-test
  "WO-T1 tests for the REAL sequential-thinking server wrapper.

  Skips with a printed reason when the npm package is not installed
  (WO-T1: existence assertion, tests skip with printed reason when
  missing). When available:

  - happy path goes through the PRODUCTION client (open!/list-tools/
    call-tool) exactly like every other harness test;
  - lifecycle proves supervised start!/stop! with zero orphaned
    processes and restart reuse;
  - the availability contract itself is asserted both ways so the skip
    branch is first-class, tested behavior.

  Every wait is bounded <= 10s (WO-T1 rule); node cold-start fits well
  inside those bounds."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.client :as mcp]
            [evoclj.mcp.support.fake-server :as fake]
            [evoclj.mcp.support.real-server :as real]))

(defn- skip!
  "Print a WO-compliant skip reason."
  [test-id]
  (println (str "[SKIP] " test-id
                ": real server not installed (missing " real/bin-relpath
                ") — install @modelcontextprotocol/server-sequential-thinking"
                " to enable")))

;; ---------------------------------------------------------------------------
;; availability contract (always runs, both branches)
;; ---------------------------------------------------------------------------

(deftest availability-contract-is-consistent
  (testing "available? agrees with assert-available!; transport-config is a valid stdio config when available"
    (let [avail? (real/available?)]
      (is (boolean? avail?))
      (if avail?
        (let [{:keys [bin entry]} (real/assert-available!)
              cfg (real/transport-config)]
          (is (.exists ^java.io.File bin) ".cmd anchor exists")
          (is (.exists ^java.io.File entry) "entry script exists")
          (is (= :stdio (:type cfg)))
          (is (= "node" (:command cfg)))
          (is (= 1 (count (:args cfg))) "exactly the entry script argument")
          (is (re-find #"server-sequential-thinking" (first (:args cfg)))
              "entry points at the sequential-thinking package"))
        (do
          (skip! "availability-contract-is-consistent")
          (let [t (try (real/assert-available!)
                       ::no-throw
                       (catch Throwable e e))]
            (is (instance? clojure.lang.ExceptionInfo t)
                "assert-available! throws typed ex-info when missing")
            (when (instance? clojure.lang.ExceptionInfo t)
              (is (= :support/real-server-missing (:error/type (ex-data t))))
              (is (contains? (ex-data t) :missing)))))))))

;; ---------------------------------------------------------------------------
;; happy path: production client against the REAL server
;; ---------------------------------------------------------------------------

(deftest real-server-happy-path-through-production-client
  (testing "real sequential-thinking: production open! handshake, list-tools exposes tool, call-tool answers"
    (if-not (real/available?)
      (skip! "real-server-happy-path-through-production-client")
      (let [managed (mcp/open! (real/transport-config))]
        (try
          (is (some? (:client managed)) "handshake completed via production open!")
          (let [tools (mcp/list-all-tools (:client managed))
                names (set (map :mcp/name tools))]
            (is (pos? (count tools)) "server lists at least one tool")
            (is (contains? names "sequentialthinking")
                "the documented sequentialthinking tool is exposed"))
          (let [result (mcp/call-tool-managed managed "sequentialthinking"
                                              {:thought "wo-t1 harness probe"
                                               :thoughtNumber 1
                                               :totalThoughts 1
                                               :nextThoughtNeeded false})]
            (is (false? (:mcp/is-error result)) "call succeeds without isError")
            (is (pos? (count (:mcp/content result))) "non-empty content blocks"))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; lifecycle: supervised start!/stop!, restart reuse, no orphans
;; ---------------------------------------------------------------------------

(deftest real-server-lifecycle-start-stop-restart-no-orphans
  (testing "supervised real server dies on stop!, restarts cleanly, leaves no orphan processes"
    (if-not (real/available?)
      (skip! "real-server-lifecycle-start-stop-restart-no-orphans")
      (do
        (let [srv (real/start!)]
          (try
            (is (real/alive? srv) "supervised process alive right after start!")
            (finally
              (real/stop! srv)))
          (is (not (real/alive? srv)) "process dead after stop!")
          (is (nil? (real/stop! srv)) "second stop! is an idempotent no-op")
          (is (empty? (fake/await-no-process-matching
                       (real/process-matching-pattern) 8000))
              "no sequential-thinking processes survive stop!"))
        ;; restart reuse (docstring contract)
        (real/with-real-server [srv2]
          (let [cfg (:config srv2)
                managed (mcp/open! cfg)]
            (try
              (is (pos? (count (:tools (mcp/list-tools (:client managed)))))
                  "fresh instance serves through the production client")
              (finally
                (mcp/close! managed)))))
        (is (empty? (fake/await-no-process-matching
                     (real/process-matching-pattern) 8000))
            "zero orphans after full cycle including client children")))))
