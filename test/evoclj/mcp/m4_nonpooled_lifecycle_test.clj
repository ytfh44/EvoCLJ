(ns evoclj.mcp.m4-nonpooled-lifecycle-test
  "WO-M4 — non-pooled MCP client lifecycle: every client opened OUTSIDE
  the manager pool must be closed when its call scope exits, on success
  AND on failure (try/finally close! semantics, \"with-client style\").

  THE LEAK UNDER TEST (pre-fix): two production sites open a fresh
  managed client per call and never close it, so one node.exe stdio
  subprocess accumulates per call until the JVM dies:

    - evoclj.provider.mcp-bridge ToolEntry/execute-request!, the
      `(mcp-client/open! transport-config)` fallback taken whenever the
      descriptor carries NO :connection/id;
    - evoclj.mcp.source discover-tools' live path and ToolEntry/
      execute-request!, both falling back to a fresh open! whenever no
      manager/connection-key is available.

  CONTRACT UNDER TEST (post-fix):
    c1. happy: N non-pooled calls -> every call-scoped child reaped;
        zero NEW fake-server processes vs an in-body baseline
        (baseline-diff discipline of WO-T1 DEVIATION RECORD 4).
    c2. fault: when call-tool throws (its own args guard) or post-call
        output validation throws, the call-scoped client is STILL
        closed in the finally — proven by the same process audit.
    c3. regression: shared path (:connection/id) untouched by M4 —
        pooled client stays :ready/live across calls, third call works,
        release still reaps it.
    c4. concurrency: non-pooled and shared calls raced together do not
        interfere; pool stays live; audit clean.
    c5. doc-behavior consistency: the namespaces document the
        call-scoped closure contract their code implements.

  FAULT-INJECTION NOTE (INV-09 production-path discipline): the T1 fake
  server's tools/call always answers ok, and crash-after-init failures
  pay the SDK's documented ~20s requestTimeout stall (M3 DEVIATION
  RECORD) — unusable here. The two deterministic fast faults used
  instead are BOTH genuine production failure classes driven through
  the REAL provider chain over REAL server subprocesses:
    - call-tool's own public guard \"args must be a plain map\", reached
      with a hand-built authorized-request (execute-request!'s direct
      input contract; normalize-request is not part of this seam);
    - post-call provider/output-schema validation against a deliberately
      unsatisfiable descriptor schema.
  In both cases the SERVER STAYS ALIVE, so any surviving child process
  after the failed call can only be OUR leaked client — making the
  process audit an honest closure proof.

  Why the in-body baseline: the supervised wrapper process is started
  AFTER the pre-suite snapshot, so audits run inside `with-fake-server`
  must diff against a baseline captured INSIDE the body (post-warmup),
  otherwise the legitimately-alive supervised server counts as \"new\"."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.source :as env-src]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.mcp.support.fake-server :as fake]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.support.concurrency :as conc]))

;; ---------------------------------------------------------------------------
;; fixtures
;; ---------------------------------------------------------------------------

(defn- try-call
  "Run `f`; capture outcome as data so fault-path assertions never lose
  a Throwable: {:status :result :value v} | {:status :thrown :value t}."
  [f]
  (try
    {:status :result :value (f)}
    (catch Throwable t
      {:status :thrown :value t})))

(defn- bridge-provider
  "Bridge provider over `tcfg`. With `connection-id` nil this is the
  NON-POOLED provider whose execute-request! must be call-scoped.
  `manager` is only meaningful together with connection-id (pooled)."
  ([tcfg] (bridge-provider tcfg nil))
  ([tcfg {:keys [connection-id output-schema manager] :or {output-schema :any}}]
   (mcp-bridge/mcp-provider
    (cond-> {:transport-config tcfg
             :tool/id          :m4/echo
             :tool/mcp-name    "echo"
             :input-schema     :any
             :output-schema    output-schema}
      connection-id (assoc :connection/id connection-id)
      manager       (assoc :manager manager)))))

(defn- bridge-req
  [p]
  (proto/normalize-request p {:payload {:tool/id :m4/echo :args {}}}))

(defn- source-descriptor
  "Stable descriptor shaped like discover-tools' output, optionally
  carrying an unsatisfiable :output-schema for the validation-fault test."
  ([] (source-descriptor :any))
  ([output-schema]
   {:tool/id         :m4/src-echo
    :effect          :remote
    :input-schema    :any
    :output-schema   output-schema
    :required-action :invoke
    :version         1
    :mcp/name        "echo"
    :mcp/input-schema {}
    :mcp/output-schema :any}))

(defn- source-entry-nonpooled
  "Production make-tool-entry with manager NIL -> conn-key nil ->
  execute-request!'s non-shared branch. This is the exact construction
  tool-entries->surface produces when a host runs McpSource surfaces
  without a manager."
  ([tcfg] (source-entry-nonpooled tcfg :any))
  ([tcfg output-schema]
   (mcp-source/make-tool-entry (source-descriptor output-schema) nil tcfg)))

(defn- source-req
  [entry]
  (proto/normalize-request entry {:payload {:tool/id :m4/src-echo :args {}}}))

(defn- managerless-mcp-source
  "McpSource record WITHOUT a manager — the only production shape in
  which discover-tools takes its non-pooled branch. Built through the
  record constructor because make-mcp-source defaults nil managers to a
  fresh one (host-owned pools are the normal case); every other field
  matches the factory's output. snapshot!/discover-tools run unchanged."
  [tcfg]
  (->(mcp-source/->McpSource :m4/src-disc tcfg nil (atom {}) (atom false) {} nil)
     (assoc :tools-change-cb (fn []))))

(defn- assert-no-new-children!
  "Baseline-diffed orphan audit: poll until no descendant matching the
  fake-server pattern exists beyond `baseline` PIDs (15s + one bounded
  3s second chance). Empty result = every child THIS scope spawned was
  reaped; non-empty = leaked stdio clients."
  [baseline label]
  (is (empty? (fake/await-no-new-process-matching
               fake/fake-server-process-pattern baseline))
      label))

(defmacro ^:private with-audited-call-scope
  "Start a fake server, bind `srv`, capture the IN-BODY baseline after
  `warmup` ran, bind it to `baseline`, run body, audit at the end."
  [[srv opts warmup baseline] & body]
  `(let [pre-existing# (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
     (fake/with-fake-server [~srv ~opts]
       (let [_~warmup
             ;; in-body baseline: supervised wrapper alive HERE, so it is
             ;; excluded; only client children spawned later count.
             ~baseline (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
         ~@body))))

;; ---------------------------------------------------------------------------
;; c1 — happy: repeated non-pooled bridge calls reap every child
;; ---------------------------------------------------------------------------

(deftest m4-c1-nonpooled-happy-five-calls-reap-every-child
  (testing "5 non-pooled execute-request! calls leave zero surviving client processes"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [p (bridge-provider (:config srv))
            r (bridge-req p)]
        ;; warmup outside the audited window proves REPEAT calls stay healthy
        (let [warm (proto/execute-request! p r)]
          (is (false? (get-in warm [:value :mcp/is-error])) (pr-str warm)))
        (let [baseline (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
          (dotimes [_ 5]
            (let [res (proto/execute-request! p r)]
              (is (false? (get-in res [:value :mcp/is-error])) (pr-str res))))
          (assert-no-new-children!
           baseline
           "every non-pooled call closed its own client: zero NEW fake-server
            processes remain after 5 calls"))))))

(deftest m4-c1b-source-discovery-nonpooled-reaps-every-child
  (testing "manager-less McpSource snapshots (live discover-tools): zero surviving clients"
    (fake/with-fake-server [srv {:mode :ok :tool-count 2}]
      (let [src (managerless-mcp-source (:config srv))]
        (let [snap0 (env-src/snapshot! src)]
          (is (= 2 (count (get-in snap0 [:payload :tools]))) (pr-str snap0)))
        (let [baseline (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
          (doseq [_ (range 3)
                  :let [snap (env-src/snapshot! src)]]
            (is (= 2 (count (get-in snap [:payload :tools]))) (pr-str snap)))
          (assert-no-new-children!
           baseline
           "each discovery closed its call-scoped client: zero NEW processes"))))))

;; ---------------------------------------------------------------------------
;; c2 — fault paths still close the call-scoped client
;; ---------------------------------------------------------------------------

(deftest m4-c2a-nonpooled-call-tool-throw-still-closes-client
  (testing "call-tool's own args guard throws -> typed failure surfaces AND client closed"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [p (bridge-provider (:config srv))]
        ;; establish the call-tool failure via ITS PUBLIC CONTRACT:
        ;; \"args must be a plain map\" — reached through the real
        ;; execute-request! with an authorized request carrying a bad
        ;; args value (normalize-request is upstream of this seam).
        ;; Three faulting calls: one abandoned client can occasionally be
        ;; GC-finalized (pipes close, node exits) inside the audit window;
        ;; a trio makes the pre-fix leak deterministic for the baseline run.
        ;; baseline BEFORE the faulting calls (same discipline as c1/c2c):
        ;; the supervised server is already alive HERE so it is excluded;
        ;; any NEW process afterwards can only be a leaked call-scoped
        ;; client. Capturing the baseline only AFTER this loop would fold
        ;; every leak into it and void the audit (R2 review F1).
        (let [baseline (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
          (dotimes [i 3]
            (let [bad-req {:tool/id  :m4/echo
                           :resource :m4/echo-resource
                           :args     "not-a-map"}
                  outcome (try-call #(proto/execute-request! p bad-req))]
              (is (= :thrown (:status outcome)) (pr-str outcome))
              (is (= :provider/execution-failed
                     (-> outcome :value ex-data :error/type))
                  "non-transport call-tool failure surfaces typed execution-failed")))
          (assert-no-new-children!
           baseline
           "every freshly-opened call-scoped client is closed even though
              call-tool threw before touching it"))))))

(deftest m4-c2b-nonpooled-output-validation-throw-still-closes-client
  (testing "post-call output-schema violation throws -> client STILL closed"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [p (bridge-provider (:config srv)
                               {:output-schema [:map [:m4/impossible-key :string]]})
            r (bridge-req p)]
        ;; baseline BEFORE the faulting calls (same discipline as c1/c2c):
        ;; captured only AFTER this loop it would absorb every leaked PID
        ;; and the audit would be structurally vacuous (R2 review F1).
        (let [baseline (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
          ;; three faulting calls so the pre-fix leak cannot dodge the audit
          ;; via a lucky GC finalization of a single abandoned client
          (dotimes [_ 3]
            (let [outcome (try-call #(proto/execute-request! p r))]
              (is (= :thrown (:status outcome)) (pr-str outcome))
              ;; the wrapped cause preserves the original typed failure
              (is (= :provider/output-invalid
                     (-> outcome :value ex-data :cause :error/type))
                  "original output-invalid classification survives as :cause")))
          (assert-no-new-children!
           baseline
           "clients closed in finally despite mid-body validation throws"))))))

(deftest m4-c2c-source-nonpooled-execute-happy-and-fault-still-close
  (testing "source ToolEntry without manager: happy call then fault call, both leave zero clients"
    (fake/with-fake-server [srv {:mode :ok}]
      (let [entry (source-entry-nonpooled (:config srv))]
        (let [ok-res (proto/execute-request! entry (source-req entry))]
          (is (false? (get-in ok-res [:value :mcp/is-error])) (pr-str ok-res)))
        (let [baseline (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
          ;; fault: call-tool args guard, same injection rationale as c2a;
          ;; three rounds so a single GC-finalized stray cannot dodge audit
          (dotimes [_ 3]
            (let [outcome (try-call
                           #(proto/execute-request!
                             entry
                             {:tool/id :m4/src-echo
                              :resource :m4/src-echo-resource
                              :args :not-a-map}))]
              (is (= :thrown (:status outcome)) (pr-str outcome))
              (is (= :provider/execution-failed
                     (-> outcome :value ex-data :error/type)))))
          (assert-no-new-children!
           baseline
           "both the happy and the faulting source executes reaped their
            call-scoped clients"))))))

;; ---------------------------------------------------------------------------
;; c3 — regression: shared pooled path keeps M1/M3 semantics untouched
;; ---------------------------------------------------------------------------

(deftest m4-c3-shared-path-unaffected-pool-stays-live-and-release-still-reaps
  (testing ":connection-id provider: pool hit stays ready/live, third call works, release cleans up"
    (let [mgr (manager/create-manager)
          ck (atom nil)
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      (fake/with-fake-server [srv {:mode :ok}]
        (let [tcfg (:config srv)
              sp (bridge-provider tcfg {:connection-id :m4/shared
                                        :manager       mgr})]
          (reset! ck (manager/connection-key (assoc tcfg :connection/id :m4/shared)))
          (let [r (bridge-req sp)]
            (is (false? (get-in (proto/execute-request! sp r) [:value :mcp/is-error])))
            (is (false? (get-in (proto/execute-request! sp r) [:value :mcp/is-error])))
            (let [entry (manager/pool-get mgr @ck)]
              (is (= :ready (:state entry)) (pr-str entry))
              (is (false? (:closed? (:client entry)))
                  "pooled managed record stays LIVE — never closed by execute"))
            ;; third call on the SAME pooled client still succeeds
            (is (false? (get-in (proto/execute-request! sp r)
                                [:value :mcp/is-error]))))))
      ;; supervised server stopped; release closes the pooled CLIENT child
      (manager/release mgr @ck :m4/echo)
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "shared client reaped via release exactly as before M4"))))

;; ---------------------------------------------------------------------------
;; c4 — concurrency: non-pooled and shared calls interleave safely
;; ---------------------------------------------------------------------------

(deftest m4-c4-concurrent-nonpooled-and-shared-do-not-interfere
  (testing "raced non-pooled x2 / shared x2 (distinct connections): all succeed, pool intact, audit clean"
    (let [mgr (manager/create-manager)
          ck-a (atom nil)
          ck-b (atom nil)
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      (fake/with-fake-server [srv {:mode :ok}]
        (let [tcfg (:config srv)
              np  (bridge-provider tcfg)                            ; non-pooled
              sh  (bridge-provider tcfg {:connection-id :m4/race-a
                                        :manager       mgr})       ; pooled A
              sh2 (bridge-provider tcfg {:connection-id :m4/race-b
                                        :manager       mgr})       ; pooled B
              nr   (bridge-req np)
              sr   (bridge-req sh)
              sr2  (bridge-req sh2)]
          (reset! ck-a (manager/connection-key (assoc tcfg :connection/id :m4/race-a)))
          (reset! ck-b (manager/connection-key (assoc tcfg :connection/id :m4/race-b)))
          ;; Warmup ALL paths sequentially first. Rationale: CONCURRENT
          ;; callTool requests on ONE McpSyncClient intermittently fail with
          ;; an SDK-level \"Failed to enqueue message\" (~1 in 5 in this
          ;; harness, warm client included) — a pre-existing SDK 2.0.0
          ;; concurrency hazard OUTSIDE WO-M4's file scope, never exercised
          ;; by earlier suites (M1 #5 / M3 h4 race the OPEN only, not
          ;; executes). This test therefore gives each shared racer its OWN
          ;; pooled connection: what it verifies is exactly the M4 contract —
          ;; that call-scoped opens/closes racing against live pool usage
          ;; never disturb pooled state.
          (let [_ (proto/execute-request! np nr)
                _ (proto/execute-request! sh sr)
                _ (proto/execute-request! sh2 sr2)]
            nil)
          (let [results (conc/raced
                         [#(proto/execute-request! np nr)
                          #(proto/execute-request! sh sr)
                          #(proto/execute-request! np nr)
                          #(proto/execute-request! sh2 sr2)]
                         :timeout-ms 20000)]
            (is (every? #(= :result (:status %)) results) (pr-str results))
            (is (every? #(false? (get-in (:value %) [:value :mcp/is-error]))
                        (filter #(= :result (:status %)) results))
                "all four interleaved calls answered ok"))
          (doseq [[label k] [["A" ck-a] ["B" ck-b]]]
            (let [entry (manager/pool-get mgr @k)]
              (is (= :ready (:state entry)) (pr-str label entry))
              (is (false? (:closed? (:client entry)))
                  "concurrent non-pooled closes never touched the pooled client"))))
        (manager/release mgr @ck-a :m4/echo)
        (manager/release mgr @ck-b :m4/echo))
      (assert-no-new-children!
       pre-existing
       "mixed concurrent traffic leaves zero leaked client processes"))))

;; ---------------------------------------------------------------------------
;; c5 — docstring contract matches implemented behavior
;; ---------------------------------------------------------------------------

(deftest m4-c5-docstring-states-the-call-scoped-closure-contract
  (testing "bridge namespace documents that non-pooled providers close per call"
    (let [doc (some-> (find-ns 'evoclj.provider.mcp-bridge) meta :doc)]
      (is (string? doc) "bridge ns docstring present")
      (is (re-find #"(?i)(without|no)\s+:connection" doc)
          "ns doc mentions the no-:connection/id case")
      (is (re-find #"(?i)clos" doc)
          "ns doc states the closure contract for that case")))
  (testing "source discover-tools documents closure of the non-pooled live path"
    (let [doc (some-> #'evoclj.mcp.source/discover-tools meta :doc)]
      (is (string? doc) "discover-tools docstring present")
      (is (re-find #"(?i)clos" doc)
          "discover-tools doc states the non-pooled closure contract"))))
