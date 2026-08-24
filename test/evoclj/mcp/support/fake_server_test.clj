(ns evoclj.mcp.support.fake-server-test
  "WO-T1 tests for the fake MCP stdio server harness.

  Every client interaction goes through the PRODUCTION
  `evoclj.mcp.client` code path (open!/list-tools/list-all-tools/
  call-tool/classify-mcp-error) — the anti-pattern ban on bypassing
  production client code is honored throughout. The ONE documented
  exception: infinite-cursor / wire-shape probes use bounded RAW stdio
  frames and no production function at all; WO-T1 explicitly allows
  that form for this knob (client-side bounded controlled call, or no
  production function), because SDK 2.0.0 auto-follows cursors inside a
  single listTools() call, making ANY production listing unbounded
  against that mode (DEVIATION RECORD 3 in fake_server.clj).

  Wait discipline: every deref/join/poll in this file is bounded and
  every bound is <= 10s (WO-T1 rule), with ONE dispatcher-approved
  exception: orphan audits may use the baseline-diff helper's 15s poll
  + 3s second-chance windows under sustained load (DEVIATION RECORD 4
  in fake_server.clj; evidence docs/codebase/m1-full2.txt).
  infinite-cursor is probed ONLY
  with bounded raw stdio JSON-RPC frames — SDK 2.0.0's listTools()
  auto-follows cursors internally, so even ONE production listing call
  is unbounded against that mode (see DEVIATION RECORD 3 in
  fake_server.clj); the unbounded production list-all-tools is NEVER
  driven against it, and neither is any other production function.

  Slow-mode deviation (approved): see fake_server.clj — the tests prove
  the delay knob is real (measured latency >= FAKE_DELAY_MS) and that a
  sub-timeout delay passes the production chain unclassified-as-error."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [evoclj.mcp.client :as mcp]
            [evoclj.mcp.support.fake-server :as fake]))

;; --- tiny helpers ------------------------------------------------------------

(defn- capture-throw
  "Call `f`; return ::no-throw, or the Throwable it raised."
  [f]
  (try (f) ::no-throw (catch Throwable t t)))

(defn- elapsed-ms
  [t0]
  (/ (- (System/nanoTime) t0) 1e6))

(def ^:private slow-delay-ms 900)

;; ---------------------------------------------------------------------------
;; Bounded RAW stdio probes (no production client code involved).
;;
;; Purpose: wire-level assertions (page shape, endless cursors) that
;; SDK 2.0.0's auto-following listTools() cannot express, and fault knobs
;; (infinite-cursor) that must never touch ANY production listing call.
;; Every wait here is deadline-bounded; teardown is destroyForcibly +
;; bounded waitFor.
;; ---------------------------------------------------------------------------

(defn- spawn-raw-server
  "Spawn the fake server Node script directly via ProcessBuilder with the
  same knob args `fake/knob-args` produces. Returns a map with :process,
  :stdin (OutputStream), :lines (atom vec of received JSON lines), and
  :reader-done (promise, delivered when stdout hits EOF). Stderr is
  inherited so script diagnostics surface in test output."
  [opts]
  (let [argv (vec (cons "node"
                        (into [(fake/script-path)] (fake/knob-args opts))))
        pb (ProcessBuilder. ^java.util.List argv)
        _ (.redirectError pb java.lang.ProcessBuilder$Redirect/INHERIT)
        p (.start pb)
        lines (atom [])
        done (promise)]
    (future
      (try
        (let [rdr (java.io.BufferedReader.
                   (java.io.InputStreamReader.
                    (.getInputStream ^java.lang.Process p) "UTF-8"))]
          (loop []
            (when-let [l (try (.readLine rdr) (catch Exception _ nil))]
              (swap! lines conj l)
              (recur))))
        (catch Exception _ nil)
        (finally (deliver done true))))
    {:process p :stdin (.getOutputStream p) :lines lines :reader-done done}))

(defn- await-line-count
  "Bounded poll until `raw` has received >= n lines. Returns the count at
  whichever comes first: n reached or deadline exceeded (deterministic
  failure downstream via count assertion)."
  [raw n timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
    (loop []
      (let [c (count @(:lines raw))]
        (if (or (>= c n) (>= (System/currentTimeMillis) deadline))
          c
          (do (Thread/sleep 25) (recur)))))))

(defn- raw-send
  [raw msg]
  (doto ^java.io.OutputStream (:stdin raw)
    (.write (.getBytes (str (json/generate-string msg) "\n") "UTF-8"))
    (.flush)))

(defn- nth-line
  "Parse line i of the captured raw conversation as JSON."
  [raw i]
  (json/parse-string (nth @(:lines raw) i) true))

(defn- kill-raw!
  "Bounded force-teardown of a raw probe process tree. Retries
  destroyForcibly within `timeout-ms`; if the process somehow survives
  (observed transiently on cold node.exe starts under this host's
  Windows setup), falls back to `taskkill /F /T /PID`, which is the
  guaranteed Windows-safe tree kill. Returns true only when the process
  is verifiably dead."
  ([raw] (kill-raw! raw 5000))
  ([raw timeout-ms]
   (let [^java.lang.Process p (:process raw)
         ^java.lang.ProcessHandle h (.toHandle p)
         deadline (+ (System/currentTimeMillis) (long timeout-ms))]
     (loop []
       (when (.isAlive h)
         (.destroyForcibly h))
       (try
         (.get (.onExit h) 1000 java.util.concurrent.TimeUnit/MILLISECONDS)
         (catch Exception _ nil))
       (if (.isAlive h)
         (if (< (System/currentTimeMillis) deadline)
           (recur)
           ;; last-resort guaranteed kill (bounded)
           (do
             (try
               (let [tk (.start (doto (ProcessBuilder.
                                       ["taskkill" "/F" "/T" "/PID"
                                        (str (.pid h))])
                                  (.inheritIO)))]
                 (.waitFor tk 5000 java.util.concurrent.TimeUnit/MILLISECONDS))
               (catch Exception _ nil))
             (not (.isAlive h))))
         true)))))

(defn- raw-handshake!
  "Drive initialize + notifications/initialized over a raw probe and
  return the index of the initialize RESPONSE line. Bounded."
  [raw]
  (raw-send raw {:jsonrpc "2.0" :id 1 :method "initialize"
                 :params {:protocolVersion "2025-06-18"
                          :capabilities {}
                          :clientInfo {:name "wo-t1-raw-probe" :version "0"}}})
  ;; line 0 = initialize response (notifications are never answered)
  (await-line-count raw 1 5000)
  (raw-send raw {:jsonrpc "2.0" :method "notifications/initialized"})
  0)


;; ---------------------------------------------------------------------------
;; Core happy path (dispatch step 3)
;; ---------------------------------------------------------------------------

(deftest ok-mode-open-handshake-succeeds
  (testing "ok mode: production open! completes the MCP handshake over stdio"
    (fake/with-fake-server [srv {:mode :ok :tool-count 3}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (is (some? (:client managed)) "managed record carries a live client")
          (is (false? (:closed? managed)))
          (is (= :stdio (:transport-type managed)))
          (finally
            (mcp/close! managed)))))))

(deftest ok-mode-list-and-call-tool-through-production-client
  (testing "ok mode: list-all-tools returns the configured tools and call-tool echoes args"
    (fake/with-fake-server [srv {:mode :ok :tool-count 3}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [tools (mcp/list-all-tools (:client managed))
                result (mcp/call-tool (:client managed) "echo"
                                      {:note "ping-echo" :n 7})]
            (is (= 3 (count tools)) "configured FAKE_TOOL_COUNT tools come back")
            (is (= ["fake-tool-0" "fake-tool-1" "fake-tool-2"]
                   (mapv :mcp/name tools)))
            (is (false? (:mcp/is-error result)))
            (let [block (first (:mcp/content result))]
              (is (= :text (:content/type block)))
              (is (= {:note "ping-echo" :n 7}
                     (json/parse-string (:content/text block) true))
                  "server echoes the arguments JSON verbatim")))
          (finally
            (mcp/close! managed)))))))

(deftest stop-kills-process-and-leaves-no-orphans
  (testing "stop! terminates the supervised process; no fake-server descendants survive"
    ;; 负载鲁棒性加固 (M1-full2)：基线差集孤儿审计。pre-existing 在本测试
    ;; spawn 任何进程之前采样；最终断言只统计"新增"匹配。为什么安全：
    ;; m1-full2.txt 显示一个先于本套件残留的 node.exe 进程（同一 stale PID
    ;; 31348）熬过了全部 stop!/kill 等待窗（Windows 异步终止 >5s/8s），
    ;; 击穿了全套件所有孤儿断言（连 real-server 的不同 pattern 也命中——
    ;; Windows 下 JDK 常不暴露 commandLine，审计退化为"任意存活 node 后代"）。
    ;; 把它记在本测试头上检测不到本测试拥有的任何缺陷；基线差集恢复原语义
    ;; 对象"本测试收割自己生成的整棵进程树"。等待窗 5000ms -> 15000ms，
    ;; 另加一次有界 3s 二次机会复查以容忍 Windows 异步 kill 滞后
    ;; (fake_server.clj DEVIATION RECORD 4, dispatcher-approved)。
    (let [pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)
          srv (fake/start! {:mode :ok :tool-count 1})]
      (try
        (is (fake/alive? srv) "supervised process alive right after start!")
        (finally
          (fake/stop! srv)))
      (is (not (fake/alive? srv)) "process dead after stop!")
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "no NEW fake-server node processes remain (this test's own tree fully reaped; pre-suite residue excluded by baseline diff)"))))

;; ---------------------------------------------------------------------------
;; slow knob — bounded-delay injection proof (approved deviation)
;; ---------------------------------------------------------------------------

(deftest slow-knob-delays-but-production-chain-classifies-ok
  (testing "FAKE_DELAY_MS is really injected; a sub-timeout delay is NOT misclassified as transient/error"
    (fake/with-fake-server [srv {:mode :slow :delay-ms slow-delay-ms}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [t0 (System/nanoTime)
                res (mcp/call-tool (:client managed) "echo" {:n 1})
                ms (elapsed-ms t0)]
            (is (>= ms (* 0.9 slow-delay-ms))
                "measured response latency >= FAKE_DELAY_MS (setTimeout never fires early)")
            (is (false? (:mcp/is-error res))
                "delay shorter than the SDK timeout flows through the production chain cleanly"))
          (finally
            (mcp/close! managed)))))))

(deftest slow-knob-off-control-roundtrip-is-fast
  (testing "control: with the knob OFF the slow-mode premise (delayed server) does not exist"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [t0 (System/nanoTime)
                _ (mcp/call-tool (:client managed) "echo" {})
                ms (elapsed-ms t0)]
            (is (< ms slow-delay-ms)
                "round-trip completes in less than the delay window used by the slow test"))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; malformed knob — typed protocol-level failure
;;
;; SDK 2.0.0 reality (verified): the stdio session silently DROPS an
;; unparseable inbound line and keeps waiting for a well-formed response,
;; so the pending listTools dies at the SDK's default 20s requestTimeout
;; (reactor TimeoutException). The honest typed shape through production
;; code is therefore :mcp/list-tools-failed carrying a
;; java.util.concurrent.TimeoutException cause-class — which client.clj's
;; classifier maps to the independent :mcp/timeout family. The
;; >10s wall is production behavior, covered by the dispatcher-approved
;; requestTimeout deviation (DEVIATION RECORD 1 in fake_server.clj);
;; M7 (requestTimeout config) will shorten this test.
;; ---------------------------------------------------------------------------

(def ^:private malformed-expected-wall-ms 25000)

(defn- sanitized-cause-classes
  "Set of :error/class strings along the SANITIZED cause chain embedded
  in an err/error ex-data (:cause -> :error/cause -> ...)."
  [d]
  (loop [m (:cause d), acc #{}]
    (if (map? m)
      (recur (:error/cause m) (conj acc (:error/class m)))
      acc)))

(defn- sanitized-cause-messages
  "Concatenated :error/message strings along the sanitized cause chain."
  [d]
  (loop [m (:cause d), acc ""]
    (if (map? m)
      (recur (:error/cause m) (str acc " " (:error/message m)))
      acc)))

(deftest malformed-yields-typed-list-tools-failure
  (testing "malformed JSON response surfaces as a typed listTools failure whose cause is the transport timeout family"
    (fake/with-fake-server [srv {:mode :malformed}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [t0 (System/nanoTime)
                t (capture-throw #(mcp/list-tools (:client managed)))
                d (ex-data t)]
            (is (instance? clojure.lang.ExceptionInfo t) "typed ExceptionInfo")
            (is (= :mcp/list-tools-failed (:error/type d)) "stable :error/type")
            ;; wire garbage was injected for the tools/list request...
            (is (< (elapsed-ms t0) (+ malformed-expected-wall-ms slow-delay-ms))
                "failure arrives within the SDK requestTimeout envelope (bounded)")
            ;; ...and the SDK's observable reaction is its requestTimeout
            (is (contains? (sanitized-cause-classes d)
                           "java.util.concurrent.TimeoutException")
                "sanitized cause chain carries the SDK requestTimeout exception")
            (is (re-find #"(?i)timeout|terminal signal"
                         (sanitized-cause-messages d))
                "timeout evidence present — protocol breakage was not answered"))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; huge knob
;; ---------------------------------------------------------------------------

(deftest huge-knob-single-tool-with-huge-description
  (testing "FAKE_MODE=huge: exactly one tool whose description carries FAKE_TOOL_COUNT KB"
    (fake/with-fake-server [srv {:mode :huge :tool-count 3}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [tools (:tools (mcp/list-tools (:client managed)))]
            (is (= 1 (count tools)) "single tool")
            (let [d (:mcp/description (first tools))]
              (is (.startsWith ^String d "huge-description:3KB:")
                  "description announces the configured KB count")
              (is (>= (count d) (* 3 1024))
                  "description body carries >= FAKE_TOOL_COUNT KB of text")))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; many-pages knob — pagination that TERMINATES
;;
;; SDK 2.0.0 semantics (DEVIATION RECORD 3): one production list-tools call
;; auto-follows every page and aggregates, so the production-level assertion
;; is "one call collects all pages"; page SHAPE is asserted at raw wire level.
;; ---------------------------------------------------------------------------

(deftest many-pages-paginates-and-terminates
  (testing "FAKE_MODE=many-pages: production client aggregates all FAKE_TOOL_COUNT tools in one terminating listing"
    (fake/with-fake-server [srv {:mode :many-pages :tool-count 13 :page-size 5}]
      (let [managed (mcp/open! (:config srv))]
        (try
          ;; one SDK listTools call walks all ceil(13/5)=3 pages internally
          ;; and returns them aggregated with no dangling cursor
          (let [res (mcp/list-tools (:client managed))
                names (mapv :mcp/name (:tools res))]
            (is (= 13 (count (:tools res)))
                "single production call collects all 3 pages worth of tools")
            (is (= 13 (count (distinct names))) "no duplicates across pages")
            (is (nil? (:next-cursor res)) "terminal page: cursor exhausted")
            (is (false? (:has-more? res)))
            (is (every? #(re-find #"^fake-tool-\d+$" %) names))
            ;; list-all-tools over an already-aggregated single page: same set
            (let [all (mcp/list-all-tools (:client managed))]
              (is (= 13 (count all)))
              (is (= (sort names) (sort (mapv :mcp/name all))))))
          (finally
            (mcp/close! managed))))))

  (testing "wire level: responses are page-shaped (PAGE_SIZE) and the LAST page carries no nextCursor"
    (let [raw (spawn-raw-server {:mode :many-pages :tool-count 13 :page-size 5})]
      (try
        (raw-handshake! raw)
        ;; collect pages by hand until a page without nextCursor (bounded)
        (loop [cursor nil, page-sizes [], pages 0]
          (when (< pages 10) ; hard bound — deterministic, never loops forever
            (raw-send raw {:jsonrpc "2.0" :id (+ 10 pages)
                           :method "tools/list"
                           :params (if cursor {:cursor cursor} {})})
            (await-line-count raw (+ pages 2) 5000)
            (let [resp (nth-line raw (+ pages 1))
                  n (count (get-in resp [:result :tools]))
                  next-cursor (get-in resp [:result :nextCursor])]
              (if next-cursor
                (do (is (= 5 n) "intermediate pages carry exactly FAKE_PAGE_SIZE tools")
                    (recur next-cursor (conj page-sizes n) (inc pages)))
                (do (is (= 3 n) "last page carries the remainder")
                    (is (= [5 5] page-sizes)
                        "exactly two intermediate pages preceded termination"))))))
        (finally
          (is (true? (kill-raw! raw)) "raw probe process torn down"))))))

;; ---------------------------------------------------------------------------
;; infinite-cursor knob — bounded RAW probes ONLY (no production functions)
;;
;; WO-T1 allowance: \"客户端侧带超时的受控调用 或 完全不接生产函数\". With SDK
;; 2.0.0's auto-following listTools() even ONE production listing call is
;; unbounded against this mode, so this test speaks raw stdio frames with
;; hard bounds and never constructs any evoclj.mcp.client object.
;; ---------------------------------------------------------------------------

(deftest infinite-cursor-controlled-bounded-probe
  (testing "infinite-cursor: raw bounded frames show endless FRESH cursors; zero production calls involved"
    (let [raw (spawn-raw-server {:mode :infinite-cursor :tool-count 6 :page-size 3})]
      (try
        (raw-handshake! raw)
        ;; request 1: first page. Line indices: 0 = initialize response,
        ;; 1 = tools/list #1 response, 2 = tools/list #2 response.
        (raw-send raw {:jsonrpc "2.0" :id 2 :method "tools/list" :params {}})
        (is (= 2 (await-line-count raw 2 5000)) "first response arrived")
        (let [p1 (nth-line raw 1)]
          (is (= 3 (count (get-in p1 [:result :tools]))) "page size honored")
          (let [c1 (get-in p1 [:result :nextCursor])]
            (is (some? c1) "page 1 claims more")
            ;; request 2: follow the minted cursor once
            (raw-send raw {:jsonrpc "2.0" :id 3 :method "tools/list"
                           :params {:cursor c1}})
            (is (= 3 (await-line-count raw 3 5000)) "second response arrived")
            (let [p2 (nth-line raw 2)
                  c2 (get-in p2 [:result :nextCursor])]
              (is (true? (and (some? c2) (not= "" c2)))
                  "page 2 ALSO claims more (never terminates)")
              (is (not= c1 c2)
                  "server keeps minting fresh cursors — M6 guard justified"))))
        (finally
          (is (true? (kill-raw! raw)) "raw probe process torn down cleanly")))))

  (testing "knob OFF contrast: ok mode at wire level terminates immediately (premise absent)"
    (let [raw (spawn-raw-server {:mode :ok :tool-count 4})]
      (try
        (raw-handshake! raw)
        (raw-send raw {:jsonrpc "2.0" :id 2 :method "tools/list" :params {}})
        (is (= 2 (await-line-count raw 2 5000)))
        (let [p (nth-line raw 1)]
          (is (= 4 (count (get-in p [:result :tools]))))
          (is (nil? (get-in p [:result :nextCursor]))
              "ok mode: single terminal page — no infinite-cursor premise"))
        (finally
          (is (true? (kill-raw! raw))))))))

;; ---------------------------------------------------------------------------
;; crash-after-init knob — typed failure + transport-timeout evidence +
;; handle cleanup
;; ---------------------------------------------------------------------------

(deftest crash-after-init-transport-error-then-clean-process-state
  (testing "crash-after-init: open! succeeds, next call fails typed with transport-timeout evidence, and no fake-server process survives close!"
    ;; 基线差集孤儿审计（同 stop-kills-process-and-leaves-no-orphans 的
    ;; M1-full2 论证）：pre-existing 先于本测试采样，断言只看新增匹配；
    ;; 等待窗 8000ms -> 15000ms + 一次有界 3s 二次机会（容忍 Windows
    ;; 异步 kill 滞后；DEVIATION RECORD 4）。语义对象不变：本用例的
    ;; client child 与 supervised twin 都被收割。
    (let [pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)
          audit (atom nil)]
      (fake/with-fake-server [srv {:mode :crash-after-init}]
        (let [managed (mcp/open! (:config srv))]
          (try
            ;; the script exits 250ms after notifications/initialized; wait it out
            (Thread/sleep 700)
            (let [t (capture-throw #(mcp/call-tool (:client managed) "echo" {}))
                  d (ex-data t)]
              (is (map? d) "call failed (server process is gone)")
              (is (= :mcp/call-tool-failed (:error/type d)) "typed failure")
              ;; SDK 2.0.0 + Windows pipe buffering: the tools/call write
              ;; succeeds into the dead child's pipe buffer, so the failure
              ;; surfaces as the SDK requestTimeout (TimeoutException). M7
              ;; classifies this as the INDEPENDENT :mcp/timeout family (no
              ;; longer folded into :mcp/call-tool-failed / :mcp/transport-error).
              (is (contains? (sanitized-cause-classes d)
                             "java.util.concurrent.TimeoutException")
                  "transport-timeout family evidence in sanitized cause chain")
              (is (= :mcp/timeout
                     (:error/type (try (mcp/classify-mcp-error t)
                                       (catch Throwable _ nil))))
                  "production classifier reports the independent :mcp/timeout category"))
            (finally
              (mcp/close! managed)))))
      ;; after BOTH the client teardown and the wrapper stop!:
      (reset! audit (fake/await-no-new-process-matching
                     fake/fake-server-process-pattern pre-existing))
      (is (empty? @audit)
          "crashed client child + supervised twin are both reaped (handle cleanup; baseline-diffed against pre-suite residue)"))))

;; ---------------------------------------------------------------------------
;; no-response knob — bounded silence proof (M15 seed)
;; ---------------------------------------------------------------------------

(deftest no-response-stays-silent-and-alive-until-teardown
  (testing "no-response: tools/call gets no answer while the process stays alive; teardown unblocks the pending call"
    (fake/with-fake-server [srv {:mode :no-response}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [result (atom ::never-ran)
                f (future (reset! result
                                  (capture-throw
                                   #(mcp/call-tool (:client managed) "echo" {:q 1}))))]
            (is (= ::pending (deref f 4000 ::pending))
                "production call-tool still unanswered after 4s (silence is real)")
            (is (pos? (count (fake/processes-matching
                              fake/fake-server-process-pattern)))
                "the silent serving process is still alive")
            ;; teardown: closing the managed client tears down its stdio child,
            ;; which unblocks the pending call. close! itself is bounded by an
            ;; 8s join; if it stalls, the janitor force-kills the tree.
            (let [closed? (atom false)
                  cf (future (mcp/close! managed) (reset! closed? true))]
              (is (true? (deref cf 8000 false)) "close! completes within 8s")
              (when-not @closed?
                (fake/kill-matching! fake/fake-server-process-pattern))
              (let [r (deref f 2000 ::still-pending)]
                (is (not= ::still-pending r) "pending call resolved after teardown")
                (is (instance? Throwable r)
                    "teardown surfaces as a transport failure, not a fake answer"))))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; controls: every fault knob OFF => no fault premise exists
;; ---------------------------------------------------------------------------

(deftest fault-knob-controls-premises-hold-under-ok-mode
  (testing "mode=ok: malformed/many-pages/infinite-cursor/huge/crash/no-response premises are all absent"
    (fake/with-fake-server [srv {:mode :ok :tool-count 2}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (let [client (:client managed)
                page (mcp/list-tools client)]
            ;; malformed control: tools/list answers normally
            (is (= 2 (count (:tools page))))
            ;; many-pages + infinite-cursor control: single terminating page
            (is (false? (:has-more? page)))
            (is (nil? (:next-cursor page)))
            ;; huge control: descriptions are short
            (is (every? #(< (count (:mcp/description %)) 1024) (:tools page)))
            ;; crash-after-init control: server survived handshake + list
            (is (pos? (count (fake/processes-matching
                              fake/fake-server-process-pattern)))
                "server process still alive under ok mode")
            ;; no-response control: tools/call answers promptly
            (let [t0 (System/nanoTime)
                  r (mcp/call-tool client "echo" {:x 1})
                  ms (elapsed-ms t0)]
              (is (false? (:mcp/is-error r)))
              (is (< ms 4000) "answered well within the 4s silence window")))
          (finally
            (mcp/close! managed)))))))

;; ---------------------------------------------------------------------------
;; lifecycle: idempotent stop!, nil tolerance, restart reuse, no orphans
;; ---------------------------------------------------------------------------

(deftest stop-idempotent-nil-tolerant-and-restart-reusable
  (testing "stop! twice + stop! nil are no-ops; a fresh start! after stop! serves again (docstring contract)"
    (let [srv (fake/start! {:mode :ok :tool-count 1})]
      (is (fake/alive? srv))
      (fake/stop! srv)
      (is (not (fake/alive? srv)))
      (is (nil? (fake/stop! srv)) "second stop! is a no-op returning nil")
      (is (nil? (fake/stop! nil)) "stop! tolerates nil"))
    (fake/with-fake-server [srv {:mode :ok :tool-count 2}]
      (let [managed (mcp/open! (:config srv))]
        (try
          (is (= 2 (count (mcp/list-all-tools (:client managed))))
              "fresh server instance serves normally after the previous stop!")
          (finally
            (mcp/close! managed)))))))

(deftest five-start-stop-cycles-leave-no-orphan-processes
  (testing "five full start!/open!/close!/stop! cycles leave zero fake-server descendants"
    ;; 基线差集 + 窗口 8000ms -> 15000ms + 一次 3s 二次机会 —— 与
    ;; stop-kills-process-and-leaves-no-orphans 相同的 M1-full2 论证：
    ;; m1-full2.txt 中同一 stale PID（先于本套件残留）击穿本断言；
    ;; 基线差集排除跨套件残留，语义对象"五个 cycle 自身收割干净"不变。
    (let [pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      (dotimes [_ 5]
        (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
          (let [managed (mcp/open! (:config srv))]
            (try
              (is (pos? (mcp/ping! managed)) "server answers ping (via list-all-tools)")
              (finally
                (mcp/close! managed))))))
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "zero NEW fake-server node processes after five cycles (own tree fully reaped)"))))

;; ---------------------------------------------------------------------------
;; concurrency: two production clients against the same fake server config
;; ---------------------------------------------------------------------------

(deftest two-clients-on-same-fake-server-do-not-interfere
  (testing "two simultaneous open!s each get their own stdio child; both echo correctly"
    (fake/with-fake-server [srv {:mode :ok :tool-count 4}]
      (let [a (mcp/open! (:config srv))
            b (mcp/open! (:config srv))]
        (try
          ;; supervised twin + two independent client children
          (is (>= (count (fake/processes-matching
                          fake/fake-server-process-pattern))
                  3)
              "three fake-server processes coexist (1 twin + 2 client children)")
          (let [fa (future (mcp/call-tool (:client a) "echo" {:who "a"}))
                fb (future (mcp/call-tool (:client b) "echo" {:who "b"}))
                ra (deref fa 8000 ::timeout)
                rb (deref fb 8000 ::timeout)]
            (is (not= ::timeout ra) "client A answered within 8s")
            (is (not= ::timeout rb) "client B answered within 8s")
            (is (= {:who "a"} (json/parse-string
                               (get-in ra [:mcp/content 0 :content/text]) true)))
            (is (= {:who "b"} (json/parse-string
                               (get-in rb [:mcp/content 0 :content/text]) true))
                "each client's echo is its own — no cross-talk"))
          (finally
            (mcp/close! a)
            (mcp/close! b)))))))
