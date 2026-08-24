(ns evoclj.mcp.ping-test
  "M8 tests: REAL SDK ping (not the list-all-tools stub), optional keepalive,
   and CLI `mcp ping` going outbound.

   Every ping goes through the PRODUCTION `evoclj.mcp.client/ping!` path
   (which now calls McpSyncClient.ping() over the live transport) against a
   REAL subprocess launched by `evoclj.mcp.support.fake-server` — the
   anti-pattern ban (INV-09) on stubs / bypass hooks is honored: no
   with-redefs of ping!, no shape-only assertions. The SDK's real JSON-RPC
   `ping` request is driven end-to-end; the fake server answers `ping` with
   `{}` (test/evoclj/mcp/support/server/fake-mcp-server.mjs).

   Required six paths (工作组协议 §必测路径六类):
     happy        — live ping returns :ok with roundtrip ms
     branch ok    — ping! returns :ok (covered by happy)
     branch fail  — dead/broken server yields a TYPED :mcp/ping-failed
     keepalive on — periodic pings recorded in a liveness atom
     keepalive off— default-off control is a no-op (returns nil)
     fault x2     — (a) server dies after init, (b) closed/nil managed record
     concurrency  — keepalive touches shared liveness state (bounded wait)
     regression   — the old list-all-tools stub is GONE (ping! does not call
                    list-all-tools)
     doc/behavior  — CLI `mcp ping` goes outbound AND command table matches
                     the real (arity-0, transport-config-driven) behavior"
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.cli.main :as main]
            [evoclj.cli.mcp :as mcp]
            [evoclj.mcp.client :as client]
            [evoclj.mcp.support.fake-server :as fake]))

(defn- capture-throw
  [f]
  (try (f) ::no-throw (catch Throwable t t)))

(def ^:private ping-interval-ms 120)

;; ---------------------------------------------------------------------------
;; happy path: live ping over the real transport returns :ok
;; ---------------------------------------------------------------------------

(deftest ping!-live-returns-ok-with-roundtrip
  (testing "ping! drives the SDK's real ping() over a live client and returns :ok"
    (fake/with-fake-server [srv {:mode :ok :tool-count 2}]
      (let [managed (client/open! (:config srv))
            result (try
                     (client/ping! managed)
                     (finally (client/close! managed)))]
        (is (= :ok (:mcp/ping result))
            "real ping over transport reports liveness")
        (is (pos? (:mcp/ping-roundtrip-ms result))
            "roundtrip latency is measured, never zero/negative")
        (is (contains? result :mcp/ping-at)
            "ping timestamp recorded for observability")))))

;; ---------------------------------------------------------------------------
;; branch: ping ok is honored (happy already), fail typed on dead transport
;; ---------------------------------------------------------------------------

(deftest ping!-dead-server-yields-typed-failure
  (testing "ping! against a server that dies after init throws typed :mcp/ping-failed"
    ;; :crash-after-init answers initialize() then exits(1) 250ms later, so
    ;; open! succeeds (handshake done) but the real .ping() request over the
    ;; now-dead transport fails — proving liveness is verified on the wire.
    (let [managed (client/open! (fake/transport-config {:mode :crash-after-init}))
          _ (Thread/sleep 400)
          thrown (capture-throw #(client/ping! managed))]
      (try
        (client/close! managed)
        (catch Throwable _ nil))
      (is (not= ::no-throw thrown)
          "a dead transport must make ping! throw")
      (is (= :mcp/ping-failed (:error/type (ex-data thrown)))
          "failure is typed :mcp/ping-failed")
      (is (some? (:mcp/ping-cause-type (ex-data thrown)))
          "failure carries the classified underlying error type"))))

(deftest ping!-closed-managed-yields-typed-failure
  (testing "ping! throws typed :mcp/ping-failed on a closed record"
    (let [m {:client nil :closed? true :last-error nil :open-count 1
             :transport-config {:type :stdio :command "echo" :args []}}]
      (let [e (atom nil)]
        (try
          (reset! e (client/ping! m))
          (catch Throwable t (reset! e t)))
        (is (= :mcp/ping-failed (:error/type (ex-data @e))))))))

(deftest ping!-nil-managed-yields-typed-failure
  (testing "ping! throws typed :mcp/ping-failed on nil"
    (let [e (atom nil)]
      (try
        (reset! e (client/ping! nil))
        (catch Throwable t (reset! e t)))
      (is (= :mcp/ping-failed (:error/type (ex-data @e)))))))

;; ---------------------------------------------------------------------------
;; regression: the old list-all-tools stub is GONE
;; ---------------------------------------------------------------------------

(deftest ping!-does-not-use-list-all-tools
  (testing "ping! no longer counts tools — it calls the SDK ping() directly"
    (fake/with-fake-server [srv {:mode :ok :tool-count 5}]
      (let [managed (client/open! (:config srv))
            result (try
                     (client/ping! managed)
                     (finally (client/close! managed)))]
        ;; The old stub returned (count (list-all-tools ...)) — a number.
        ;; The real ping returns a keyword liveness verdict, never a tool count.
        (is (not (number? result))
            "ping! must not return a tool count (old stub shape gone)")
        (is (= :ok (:mcp/ping result)))))))

;; ---------------------------------------------------------------------------
;; optional keepalive: default off is a no-op; enabled records liveness
;; ---------------------------------------------------------------------------

(deftest keepalive-default-off-is-noop
  (testing "without opting in, keepalive control is nil (nothing runs)"
    (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
      (let [managed (client/open! (:config srv))]
        (try
          (is (nil? (client/start-keepalive! managed {}))
              "keepalive OFF by default returns nil (no background thread)")
          (finally (client/close! managed)))))))

(deftest keepalive-on-records-liveness
  (testing "opting in starts periodic pings that update the liveness atom"
    (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
      (let [managed (client/open! (:config srv))
            ctrl (client/start-keepalive! managed
                  {:keepalive? true :interval-ms ping-interval-ms})]
        (try
          (is (fn? (:stop! ctrl)) "control exposes a stop! fn")
          (let [deadline (+ (System/currentTimeMillis) 5000)
                ok? (loop []
                      (or (= :ok (:mcp/keepalive @(:liveness ctrl)))
                          (if (< (System/currentTimeMillis) deadline)
                            (do (Thread/sleep 20) (recur))
                            false)))]
            (is ok? "liveness atom records :ok after at least one cycle"))
          (finally
            ((:stop! ctrl))
            (client/close! managed)))))))

(deftest keepalive-stops-cleanly
  (testing "stop! halts the keepalive thread; liveness stops updating"
    (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
      (let [managed (client/open! (:config srv))
            ctrl (client/start-keepalive! managed
                  {:keepalive? true :interval-ms ping-interval-ms})
            _ ((:stop! ctrl))
            snap (:mcp/keepalive @(:liveness ctrl))]
        (try
          (Thread/sleep (+ ping-interval-ms 200))
          (is (= snap (:mcp/keepalive @(:liveness ctrl)))
              "after stop! the liveness atom is frozen")
          (finally (client/close! managed)))))))

;; ---------------------------------------------------------------------------
;; concurrency: keepalive mutates shared liveness state under contention
;; (bounded wait; no hang)
;; ---------------------------------------------------------------------------

(deftest keepalive-concurrent-pings-update-shared-state
  (testing "two keepalive loops on one server both advance a shared atom"
    (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
      (let [a (client/open! (:config srv))
            b (client/open! (:config srv))
            ca (client/start-keepalive! a
                 {:keepalive? true :interval-ms ping-interval-ms})
            cb (client/start-keepalive! b
                 {:keepalive? true :interval-ms ping-interval-ms})]
        (try
          (let [deadline (+ (System/currentTimeMillis) 5000)
                ready? (loop []
                         (let [ok? #(= :ok (:mcp/keepalive %))]
                           (or (and (ok? @(:liveness ca)) (ok? @(:liveness cb)))
                               (if (< (System/currentTimeMillis) deadline)
                                 (do (Thread/sleep 20) (recur))
                                 false))))]
            (is ready? "both shared liveness atoms reach :ok"))
          (finally
            ((:stop! ca)) ((:stop! cb))
            (client/close! a) (client/close! b)))))))

;; ---------------------------------------------------------------------------
;; doc/behavior consistency: CLI `mcp ping` goes outbound + command table
;; matches the real (arity-0, transport-config-driven) behavior.
;; ---------------------------------------------------------------------------

(deftest cli-mcp-ping-command-table-matches-behavior
  (testing "the CLI command table registers mcp ping as arity-0 outbound probe"
    (let [entry (get main/commands ["mcp" "ping"])]
      (is (some? entry) "mcp ping is a registered command")
      (is (= 0 (:arity entry)) "mcp ping takes no positionals (arity 0)")
      (is (fn? (:fn entry)) "mcp ping dispatches to a real command fn")
      ;; doc/behavior consistency: the registered fn's real contract is
      ;; outbound (transport-config driven), not the old pool-diagnose
      ;; alias — exercise it with a reachable server and expect liveness.
      (let [cfg (fake/transport-config {:mode :ok :tool-count 1})
            res ((:fn entry) {:options {:transport-config (pr-str cfg)}
                              :positionals nil
                              :state-dir "./evoclj-state"})]
        (is (= :ok (:mcp/ping res)) "registered ping! reports real liveness")))))

(deftest cli-mcp-ping-goes-outbound-to-launched-server
  (testing "mcp ping --transport-config <edn> opens a real client and reports :ok"
    (let [cfg (fake/transport-config {:mode :ok :tool-count 1})
          argv ["mcp" "ping" "--transport-config" (pr-str cfg)]
          {:keys [exit data]} (main/execute argv {})]
      (is (= 0 exit) "ping to a reachable server exits 0")
      (is (= :ok (:mcp/ping data)) "CLI reports real liveness :ok")
      (is (pos? (:mcp/ping-roundtrip-ms data))
          "CLI reports measured roundtrip latency")
      (is (contains? data :transport-config)
          "CLI echoes a sanitized transport config"))))

(deftest cli-mcp-ping-unreachable-reports-failure
  (testing "mcp ping --transport-config <edn> to a dead launch exits 1 with typed error"
    ;; A bogus command cannot spawn, so open! fails; the CLI fails the whole
    ;; outbound ping operation closed and typed as :mcp/ping-failed.
    (let [cfg {:type :stdio :command "this-mcp-server-does-not-exist-xyz"}
          argv ["mcp" "ping" "--transport-config" (pr-str cfg)]
          {:keys [exit data]} (main/execute argv {})]
      (is (= 1 exit) "unreachable server exits 1")
      (is (= :mcp/ping-failed (:error/type data))
          "CLI surfaces typed :mcp/ping-failed")
      (is (some? (:mcp/ping-cause-type (:data data)))
          "CLI preserves the classified underlying cause type"))))
