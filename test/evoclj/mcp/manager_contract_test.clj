(ns evoclj.mcp.manager-contract-test
  "WO-M1 [P0] — get-or-open! return-contract unification.

   CONTRACT UNDER TEST (post-fix): every return path of
   evoclj.mcp.manager/get-or-open! yields the SAME managed record value
   that open-fn returned (or the one stored in the pool) — never the raw
   underlying client, and a waiter whose opener failed THROWS the
   opener's Throwable instead of receiving it as a promise value.

   The seven mandatory paths from WO-M1.md:
     1. happy first open  -> identity of the managed record
     2. ready hit         -> identity of the injected managed record
     3. latch double-caller single-flight -> same managed, one open call
     4. waiter-throw regression: open throws -> BOTH callers throw
     5. N=8 race          -> all same managed / all throw, calls = 1
     6. bridge shared path via T1 fake server: execute-request! twice ok
     7. failure leaves pool entry :broken with :promise cleared
   plus one round-2 addition:
     8. live discovery: empty-pool first open through REAL discover-tools
        over the T1 fake server -> tools found, pool entry :ready

   Tests 1/3/4/5/7 must FAIL on the pre-fix baseline (mutation check);
   2 and 6 are regression guards for behavior the fix must preserve."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.source :as env-src]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.mcp.support.fake-server :as fake]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.support.concurrency :as conc]))

(def ^:private wo-ex-data
  "Canonical ex-data every failure assertion compares against."
  {:error/type :wo-m1/test-open-failure :n 42})

(defn- fake-managed
  "A minimal managed-record-shaped map, same keys production
   evoclj.mcp.client/open! returns. `raw` stands in for a raw Java
   McpSyncClient — any non-map sentinel works."
  [raw]
  {:client raw
   :closed? false
   :open-count 1
   :call-count 0
   :transport-config {:type :stdio :command "fake" :args []}})

(defn- counting-open-fn
  "open-fn double: counts invocations, returns a fresh fake managed
   wrapping the SAME `raw` sentinel every time."
  [calls raw]
  #(do (swap! calls inc) (fake-managed raw)))

(defn- try-call
  "Run `f`; capture outcome as data so raced/future workers never lose a
   Throwable: {:status :result :value v} | {:status :thrown :value t}."
  [f]
  (try
    {:status :result :value (f)}
    (catch Throwable t
      {:status :thrown :value t})))

;; ---------------------------------------------------------------------------
;; 1. happy path: first open returns the managed record itself
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 opener-path-returns-managed-record-identity
  (testing "first open: return value IS the managed record, not the raw client"
    (let [mgr (manager/create-manager)
          k [:stdio :wo-m1/opener]
          raw (Object.)
          calls (atom 0)
          got (manager/get-or-open! mgr k (counting-open-fn calls raw))]
      (is (map? got) "return value is a managed record map")
      (is (not= raw got) "top-level value must not be the raw client")
      (is (identical? raw (:client got)) "managed record wraps the raw client at :client")
      (is (= 1 @calls))
      ;; the pool stores exactly what was returned to the opener
      (let [entry (manager/pool-get mgr k)]
        (is (= :ready (:state entry)))
        (is (identical? got (:client entry)) "pool :client is the identical managed record")))))

;; ---------------------------------------------------------------------------
;; 2. ready hit: injected managed comes back identically
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 hit-path-returns-injected-managed-identity
  (testing "put-ready then get-or-open!: same managed record, open-fn untouched"
    (let [mgr (manager/create-manager)
          k [:stdio :wo-m1/hit]
          raw (Object.)
          managed (fake-managed raw)]
      (manager/put-ready mgr k managed)
      (let [hit (manager/get-or-open! mgr k
                                      #(throw (ex-info "open-fn must not run on a ready hit"
                                                       {:error/type :wo-m1/open-fn-ran})))]
        (is (identical? managed hit))))))

;; ---------------------------------------------------------------------------
;; 3. latch double-caller: single-flight, both callers get ONE managed
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 waiter-single-flight-both-callers-get-same-managed
  (testing "opener blocks inside open-fn; concurrent caller waits on the promise"
    (let [mgr (manager/create-manager)
          k [:stdio :wo-m1/single-flight]
          raw (Object.)
          calls (atom 0)
          open-entered (promise)
          release-open (promise)
          open-fn #(do (swap! calls inc)
                       (deliver open-entered true)
                       @release-open
                       (fake-managed raw))
          opener (future (try-call #(manager/get-or-open! mgr k open-fn)))
          _ @open-entered                     ;; opener is parked inside open-fn
          waiter (future (try-call #(manager/get-or-open! mgr k open-fn)))
          _ (deliver release-open true)
          r1 @opener
          r2 @waiter]
      (is (= :result (:status r1)) (pr-str r1))
      (is (= :result (:status r2)) (pr-str r2))
      (is (= 1 @calls) "open-fn invoked exactly once across both callers")
      (let [m1 (:value r1)
            m2 (:value r2)
            pooled (:client (manager/pool-get mgr k))]
        (is (identical? m1 m2) "opener and waiter received the identical managed record")
        (is (identical? m1 pooled) "and it is the record stored in the pool")
        (is (identical? raw (:client m1)) "the shared managed record wraps the one raw client")))))

;; ---------------------------------------------------------------------------
;; 4. CORE REGRESSION: open failure -> opener AND waiter both THROW
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 open-failure-opener-and-waiter-both-throw
  (testing "a Throwable is never delivered as a promise VALUE to waiters"
    (let [mgr (manager/create-manager)
          k [:stdio :wo-m1/waiter-fail]
          ex (ex-info "wo-m1 open failed" wo-ex-data)
          calls (atom 0)
          open-entered (promise)
          release-open (promise)
          open-fn #(do (swap! calls inc)
                       (deliver open-entered true)
                       @release-open
                       (throw ex))
          opener (future (try-call #(manager/get-or-open! mgr k open-fn)))
          _ @open-entered
          waiter (future (try-call #(manager/get-or-open! mgr k open-fn)))
          ;; grace window: hold the opener parked inside its failing
          ;; open-fn long enough for the waiter future to actually land in
          ;; get-or-open!'s :connecting branch and park on the single-flight
          ;; promise BEFORE the failure tears the entry down. Without it,
          ;; suite-order scheduling lets the waiter lose that race, take the
          ;; fresh-open path instead, and pass vacuously — the counter below
          ;; turns that silent idle-pass into a deterministic failure.
          _ (Thread/sleep 150)
          _ (deliver release-open true)
          r1 @opener
          r2 @waiter]
      (is (= :thrown (:status r1)) "opener rethrows")
      (is (identical? ex (:value r1)))
      (is (= :thrown (:status r2))
          "waiter THROWS instead of receiving the Throwable as a value")
      (is (identical? ex (:value r2)) "waiter throws the SAME exception object")
      (is (= wo-ex-data (ex-data (:value r2))) "ex-data survives to the waiter")
      (is (= 1 @calls)
          "single flight held: waiter joined the :connecting window; calls=2 means it missed the window and re-opened")
      ;; belt-and-braces: whatever came back as a value is not a Throwable
      (doseq [[who r] [["opener" r1] ["waiter" r2]]]
        (when (= :result (:status r))
          (is (not (instance? Throwable (:value r))) (str who " got a Throwable as a value")))))))

;; ---------------------------------------------------------------------------
;; 5. N=8 race: success and failure variants under a common start gate
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 eight-caller-race-single-flight-same-managed-or-all-throw
  (testing "success variant: 8 simultaneous first-openers share one managed record"
    (let [mgr (manager/create-manager)
          k [:stdio :wo-m1/race8-ok]
          raw (Object.)
          calls (atom 0)
          open-fn #(do (swap! calls inc)
                       (Thread/sleep 20)   ;; widen the connecting window for waiters
                       (fake-managed raw))
          results (conc/raced (repeat 8 #(manager/get-or-open! mgr k open-fn))
                              :timeout-ms 8000)]
      (is (every? #(= :result (:status %)) results) (pr-str results))
      (let [vs (mapv :value results)]
        (is (every? #(and (map? %) (identical? raw (:client %))) vs)
            "every caller holds a managed record wrapping the one raw client")
        (is (every? #(identical? (first vs) %) (rest vs))
            "all 8 callers received the identical managed record"))
      (is (= 1 @calls) "open-fn called exactly once under an 8-way race")))
  (testing "failure variant: all 8 throw the same exception, none receives it as a value"
    (let [mgr (manager/create-manager)
          k [:stdio :wo-m1/race8-fail]
          ex (ex-info "race8 fail" {:error/type :wo-m1/race-fail})
          results (conc/raced
                   (repeat 8 (fn []
                               (try-call (fn []
                                           (manager/get-or-open!
                                            mgr k
                                            (fn [] (Thread/sleep 10) (throw ex)))))))
                   :timeout-ms 8000)]
      ;; thunks capture outcomes as data (try-call), so the raced-level
      ;; :status stays :result; the CONTRACT lives one level down:
      ;; every caller's captured outcome must be :thrown with THE ex.
      (is (every? #(-> % :value :status (= :thrown)) results)
          (pr-str (mapv (juxt :thread :status #(-> % :value :status)) results)))
      (is (every? #(identical? ex (-> % :value :value)) results)
          "every racing caller throws the identical exception object"))))

;; ---------------------------------------------------------------------------
;; 6. bridge regression: real execute-request! over the T1 fake server,
;;    shared connection path — first call (opener) AND second (pool hit)
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 bridge-shared-path-two-executes-both-succeed
  (testing "execute-request! twice over one shared fake-server connection"
    (let [mgr (manager/create-manager)
          ck (atom nil)
          ;; 基线差集孤儿审计（M1-full2 加固）：pre-existing 在本用例
          ;; spawn 任何进程前采样，末尾断言只统计"新增"匹配。为什么安全：
          ;; m1-full2.txt 显示一个先于本套件残留的 stale node.exe PID
          ;; 熬过全部 stop!/kill 窗口（Windows 异步终止 >8s），把跨套件
          ;; 残留误记到本用例；基线差集恢复原语义对象"本用例的 shared
          ;; client 经 release 收割干净"。窗口 8000ms -> 15000ms + 一次
          ;; 有界 3s 二次机会（DEVIATION RECORD 4, dispatcher-approved）。
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
        (let [tcfg (:config srv)
              p (mcp-bridge/mcp-provider
                 {:transport-config tcfg
                  :tool/id          :wo-m1/echo
                  :tool/mcp-name    "echo"
                  :input-schema     [:map]
                  :output-schema    [:map]
                  :connection/id    :wo-m1/shared
                  :manager          mgr})
              req (proto/normalize-request p {:payload {:tool/id :wo-m1/echo :args {}}})
              r1 (proto/execute-request! p req)   ;; opener path through get-or-open!
              r2 (proto/execute-request! p req)]  ;; pool-hit path
          (reset! ck (manager/connection-key (assoc tcfg :connection/id :wo-m1/shared)))
          (is (false? (get-in r1 [:value :mcp/is-error])) (pr-str r1))
          (is (= :ok (get-in r1 [:value :mcp/tool-status])))
          (is (false? (get-in r2 [:value :mcp/is-error])) (pr-str r2))
          (is (= :ok (get-in r2 [:value :mcp/tool-status])))
          (is (= (get-in r1 [:value :mcp/model-content])
                 (get-in r2 [:value :mcp/model-content]))
              "first (opener) and second (pool hit) calls agree on content shape")
          (let [entry (manager/pool-get mgr @ck)]
            (is (some? (:client entry)) "shared pool holds one managed client after both calls")
            (is (= :ready (:state entry))))))
      ;; supervised server is now stopped; close the CLIENT child via pool
      ;; release so no node process outlives this suite (later suites run
      ;; strict no-orphan audits in this same JVM).
      (manager/release mgr @ck :wo-m1/echo)
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "shared client closed via release; zero NEW fake-server processes remain (baseline-diffed)"))))

;; ---------------------------------------------------------------------------
;; 7. failure bookkeeping: :broken state, :promise cleared
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 open-failure-leaves-broken-entry-with-promise-cleared
  (testing "after a failed open the entry is :broken and carries no :promise"
    (let [mgr (manager/create-manager)
          k [:stdio :wo-m1/broken]
          open-fn #(throw (ex-info "boom" wo-ex-data))]
      (is (some? (try (manager/get-or-open! mgr k open-fn) nil
                      (catch Throwable t t))))
      (let [entry (manager/pool-get mgr k)]
        (is (= :broken (:state entry)))
        (is (contains? entry :health) "health carries the last error")
        (is (false? (contains? entry :promise))
            ":promise cleared so the next round opens fresh (healing itself is M3)")))))

;; ---------------------------------------------------------------------------
;; 8. live discovery: empty-pool first open through REAL discover-tools
;;    over the T1 fake server — the unified return contract is what makes
;;    source.clj's live path work ((:client managed) must be the raw
;;    client, i.e. managed must be the managed record, never nil/raw).
;;    Companion to the stub-driven discovery cases in source_test.clj,
;;    driven here by the T1 fake-server harness instead of :discover-fn.
;; ---------------------------------------------------------------------------

(deftest ^:wo-m1 live-discovery-empty-pool-first-open-real-discover-tools
  (testing "McpSource WITHOUT :discover-fn: real list-tools over the fake server; pool entry lands :ready"
    (let [mgr (manager/create-manager)
          ck (atom nil)
          ;; 基线差集孤儿审计（M1-full2 加固，同 bridge-shared-path 用例
          ;; 的注释论证）：排除先于本套件残留的 stale node.exe PID；
          ;; 窗口 8000ms -> 15000ms + 一次有界 3s 二次机会（DEVIATION
          ;; RECORD 4）。语义对象不变：本用例经 release 收割自己的 client。
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      (fake/with-fake-server [srv {:mode :ok :tool-count 2}]
        (let [tcfg (:config srv)
              ;; no :discover-fn anywhere -> evoclj.mcp.source/discover-tools
              ;; takes its LIVE path: manager/get-or-open! on an EMPTY pool
              ;; (first open through the production open-fn) followed by
              ;; mcp-client/list-all-tools against the real client.
              source (mcp-source/make-mcp-source
                      {:source/id        :wo-m1/live-disc
                       :transport-config tcfg
                       :manager          mgr
                       ;; top-level key: discover-tools derives the SAME
                       ;; connection key from (:connection/id opts)
                       :connection/id    :wo-m1/live})]
          (reset! ck (manager/connection-key (assoc tcfg :connection/id :wo-m1/live)))
          (let [snap (env-src/snapshot! source)
                tools (get-in snap [:payload :tools])]
            (is (= 2 (count tools)) (pr-str (keys tools)))
            (is (contains? tools :mcp/fake-tool-0) "fake-server tool 0 discovered under its stable tool-id")
            (is (contains? tools :mcp/fake-tool-1) "fake-server tool 1 discovered under its stable tool-id")
            (is (= "fake-tool-0" (get-in tools [:mcp/fake-tool-0 :mcp/name])))
            (is (= "fake-tool-1" (get-in tools [:mcp/fake-tool-1 :mcp/name])))
            (is (= {"type" "object" "properties" {} "required" []}
                   (get-in tools [:mcp/fake-tool-0 :mcp/input-schema]))
                "raw remote inputSchema normalized to string-keyed EDN")
            (let [entry (manager/pool-get mgr @ck)]
              (is (= :ready (:state entry)) (pr-str entry))
              (is (map? (:client entry)) "pool stores the MANAGED record as :client, not nil/raw")
              (is (false? (:closed? (:client entry))) "pooled managed record is live")))))
      ;; supervised server is now stopped; close the CLIENT child via pool
      ;; release so no node process outlives this suite (same discipline
      ;; as test 6: later suites run strict no-orphan audits in this JVM).
      (manager/release mgr @ck :wo-m1/live-disc)
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "shared client closed via release; zero NEW fake-server processes remain (baseline-diffed)"))))
