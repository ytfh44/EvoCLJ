(ns evoclj.mcp.manager-healing-test
  "WO-M3 — MCP manager healing loop: failure reporting, no handout of
  :broken entries, reconnecting as a real state, generation semantics,
  consecutive-failure cooldown.

  THE CONTRACT UNDER TEST — the pool entry lifecycle promised by
  evoclj.mcp.manager/get-or-open!'s state machine:

    absent ──open──▶ :connecting ──ok──▶ :ready
    :ready ──transport-family call failure──▶ :broken   (mark-broken)
    :broken ──next get-or-open!──▶ :reconnecting (generation++)
            ──ok──▶ :ready (fail-count reset, health.last-ok refreshed)
            ──fail──▶ :broken (fail-count++) or :cooldown once
                       fail-count >= max-reopen-failures
    :cooldown (window open) ──▶ THROWS typed :mcp/cooldown, open-fn untouched
    :cooldown (window expired) ──▶ :reconnecting (generation++)

  The seven mandatory WO-M3 paths:
    h1. ok first connect stamps health.last-ok; every successful shared
        call REFRESHES it (failure-reporting counterpart: mark-ok)
    h2. healing end to end over the T1 fake server: crash-after-init
        death -> :broken -> healthy server restarted under the SAME
        connection key -> next call reopens through the PRODUCTION
        bridge path, generation = 2
    h3. 3 consecutive failed reopens -> the 4th call throws typed
        :mcp/cooldown WITHOUT invoking open-fn (counter asserted);
        after the injected-clock window expires the retry goes through
    h4. reconnecting single-flight: latch-blocked open-fn, 5 concurrent
        callers -> open-fn exactly once, all receive ONE managed record
    h5. mark-broken err-data passes through redact-transport — a
        hostile exception embedding secret-bearing transport config
        surfaces only \"[REDACTED]\" (INV-01 regression)
    h6. the seven WO-M1 contract paths stay green (regression floor:
        evoclj.mcp.manager-contract-test is part of this work item's
        targeted rerun list; no duplicated assertions here)
    h7. the state machine documented on get-or-open! is exactly the set
        of states the implementation can be driven through (docstring
        text and pool-snapshot observations must agree)

  Production-path discipline (INV-09): h1/h2 drive the REAL bridge
  provider over REAL fake-server subprocesses. The crash scenario is
  made deterministic by seeding the pool through the production APIs
  (mcp-client/open! + manager/put-ready) and waiting out the scripted
  250ms-post-init death BEFORE the breaking call, so no open-vs-death
  race exists. The crashing and healthy servers share ONE transport
  config — hence ONE connection key — via a tiny `node -e` launcher
  whose behavior (--mode forwarded to the T1 fake server script) is
  controlled by a temp-file knob: exactly what an operator restarting a
  fixed server behind an unchanged connection endpoint looks like.
  DEVIATION RECORD (extends fake-server DEVIATION RECORD 1): the
  breaking call pays the SDK's default 20s requestTimeout because M7
  owns exposing requestTimeout configuration; one bounded 20s stall in
  h2 is accepted for the mandated crash-after-init path."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.support.fake-server :as fake]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.support.concurrency :as conc])
  (:import [java.io File]))

;; ---------------------------------------------------------------------------
;; fixtures (same conventions as evoclj.mcp.manager-contract-test)
;; ---------------------------------------------------------------------------

(defn- fake-managed
  "Minimal managed-record-shaped map, same keys production
  evoclj.mcp.client/open! returns; `raw` stands in for the raw client."
  [raw]
  {:client raw
   :closed? false
   :open-count 1
   :call-count 0
   :transport-config {:type :stdio :command "fake" :args []}})

(defn- try-call
  "Run `f`; capture outcome as data so raced/future workers never lose a
  Throwable: {:status :result :value v} | {:status :thrown :value t}."
  [f]
  (try
    {:status :result :value (f)}
    (catch Throwable t
      {:status :thrown :value t})))

(defn- snap-entry
  "Healing-relevant projection of pool key `k` from the read-only
  pool-snapshot."
  [mgr k]
  (get-in (manager/pool-snapshot mgr) [:pools k]))

(def ^:private wrapper-js
  "`node -e` launcher making ONE stdio endpoint behave as two different
  servers over time: it reads the mode named in the control file
  (argv[1]) and spawns the T1 fake server script (argv[2]) with that
  --mode over INHERITED stdio, exiting with the child's code. Stdin EOF
  is propagated to the child so teardown reaps the whole chain."
  (str "const fs=require('fs'),cp=require('child_process');"
       "let m='ok';"
       "try{m=fs.readFileSync(process.argv[1],'utf8').trim()||'ok'}catch(_){}"
       "const ch=cp.spawn(process.execPath,[process.argv[2],'--mode',m],{stdio:'inherit'});"
       "const die=()=>{try{ch.kill()}catch(_){}};"
       "process.stdin.on('end',die);process.stdin.on('close',die);"
       "ch.on('exit',(c)=>process.exit(typeof c==='number'?c:1));"))

(defn- wrapper-config
  "Stdio transport-config whose child behavior flips between
  crash-after-init and ok by rewriting the control file. The config —
  and therefore the manager connection key — NEVER changes."
  [^File ctl server-path]
  {:type :stdio
   :command "node"
   :args ["-e" wrapper-js (.getAbsolutePath ctl) server-path]})

;; ---------------------------------------------------------------------------
;; h1 — happy: first connect stamps health.last-ok, successes refresh it
;; ---------------------------------------------------------------------------

(deftest h1-ok-connect-stamps-and-successful-calls-refresh-last-ok
  (testing "WO-M3 #1: shared-path success refreshes health.last-ok past the open-time stamp"
    (let [mgr (manager/create-manager)
          ck (atom nil)
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
        (let [tcfg (:config srv)
              p (mcp-bridge/mcp-provider
                 {:transport-config tcfg
                  :tool/id          :m3/echo
                  :tool/mcp-name    "echo"
                  :input-schema     [:map]
                  :output-schema    [:map]
                  :connection/id    :m3/ok
                  :manager          mgr})
              req (proto/normalize-request p {:payload {:tool/id :m3/echo :args {}}})]
          (reset! ck (manager/connection-key (assoc tcfg :connection/id :m3/ok)))
          (is (false? (get-in (proto/execute-request! p req) [:value :mcp/is-error]))
              "first call (opener path) succeeds")
          (let [t1 (:health (snap-entry mgr @ck))]
            (is (pos? (:last-ok t1)) "first connect stamped health.last-ok")
            (is (nil? (:last-error t1)) "healthy entry carries no last-error")
            ;; Windows clock granularity is coarse; make the refresh visible
            (Thread/sleep 80)
            (is (false? (get-in (proto/execute-request! p req) [:value :mcp/is-error]))
                "second call (ready-hit path) succeeds")
            (let [t2 (:health (snap-entry mgr @ck))
                  e (snap-entry mgr @ck)]
              (is (> (:last-ok t2) (:last-ok t1))
                  "the successful POOL-HIT call refreshed last-ok (mark-ok wired)")
              (is (= 1 (:generation e)) "ready hits never bump generation")))))
      (manager/release mgr @ck :m3/echo)
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "shared client closed via release; zero NEW fake-server processes remain"))))

;; ---------------------------------------------------------------------------
;; h2 — CORE: crash-after-init -> :broken -> healthy restart -> reopen gen=2
;; ---------------------------------------------------------------------------

(deftest h2-crash-marks-broken-then-heals-through-production-reopen-gen2
  (testing "WO-M3 #2: a dead pooled server is reported broken and the next
            call REOPENS through the real bridge path onto a restarted,
            healthy server under the same connection key"
    (let [mgr (manager/create-manager)
          ctl (doto (File/createTempFile "m3-heal-ctl" ".txt") (.deleteOnExit))
          cfg (wrapper-config ctl (fake/script-path))
          ck (manager/connection-key (assoc cfg :connection/id :m3/heal))
          p (mcp-bridge/mcp-provider
             {:transport-config cfg
              :tool/id          :m3/heal
              :tool/mcp-name    "echo"
              :input-schema     [:map]
              :output-schema    [:map]
              :connection/id    :m3/heal
              :manager          mgr})
          req (proto/normalize-request p {:payload {:tool/id :m3/heal :args {:q 1}}})
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      (try
        ;; Seed the pool through PRODUCTION APIs (INV-09): a real
        ;; mcp-client/open! against the crash-mode endpoint, stored via
        ;; manager/put-ready (generation 1). No test double stands in for
        ;; any production component.
        (spit ctl "crash-after-init")
        (manager/put-ready mgr ck (mcp-client/open! cfg))
        (let [seeded (snap-entry mgr ck)]
          (is (= :ready (:state seeded)))
          (is (= 1 (:generation seeded)) "seeded live connection is generation 1"))
        ;; Wait out the scripted death (250ms after notifications/initialized)
        ;; so the breaking call below is deterministic — no open-vs-death race.
        (Thread/sleep 900)
        ;; THE BREAKING CALL: pool hit hands out the now-dead client; call-tool
        ;; fails at the SDK request timeout with transport-family evidence in
        ;; the sanitized cause chain (WO-T1 verified shape).
        (let [r1 (try-call #(proto/execute-request! p req))]
          (is (= :thrown (:status r1)) "the call against the crashed server fails")
          (is (contains? #{:provider/transient-error :provider/execution-failed}
                         (:error/type (ex-data (:value r1))))
              (pr-str (ex-data (:value r1))))
          (let [e (snap-entry mgr ck)]
            (is (= :broken (:state e)) "mark-broken demoted the READY entry" )
            (is (= 1 (:generation e)) "mark-broken preserves generation (it counts attempts)")
            (is (not (contains? e :has-client?))
                "the dead client record was stripped — a present :client would
                 let the bridge/source pool-hit shortcut hand out the zombie
                 forever, bypassing get-or-open! healing")
            (let [le (get-in e [:health :last-error])]
              (is (map? le) (pr-str le))
              ;; the wiring stores the RAW transport failure (highest
              ;; evidence fidelity), not the provider wrapper
              (is (= :mcp/call-tool-failed (:error/type le))
                  "stored last-error is the sanitized raw failure data")
              ;; transport-family evidence really landed (the crash presents
              ;; as the SDK requestTimeout per WO-T1)
              (is (re-find #"TimeoutException" (pr-str le))
                  "cause-chain evidence survived sanitization into last-error")
              ;; INV-01 hygiene on the stored payload
              (is (= le (edn/read-string (pr-str le)))
                  "stored last-error round-trips EDN"))))
        ;; The operator fixes the server: same endpoint, healthy mode now.
        (spit ctl "ok")
        (let [r2 (try-call #(proto/execute-request! p req))]
          (is (= :result (:status r2)) (pr-str r2))
          (is (false? (get-in r2 [:value :value :mcp/is-error]))
              "the healed call answers normally")
          (let [e (snap-entry mgr ck)]
            (is (= :ready (:state e)) (pr-str e))
            (is (= 2 (:generation e))
                "broken -> reconnecting bumped generation to exactly 2")
            (is (= 0 (:fail-count e)) "success reset the failure streak")
            (is (pos? (get-in e [:health :last-ok]))
                "healed entry carries a fresh health.last-ok")))
        (finally
          ;; reap the live client chain via the pool (stdin EOF cascades:
          ;; wrapper -> fake server grandchild)
          (manager/release mgr ck :m3/heal)
          (spit ctl "crash-after-init")))
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "no fake-server/wrapper processes survive the healing cycle"))))

;; ---------------------------------------------------------------------------
;; h3 — cooldown: 3 consecutive failed reopens arm the gate; 4th call throws
;; ---------------------------------------------------------------------------

(deftest h3-three-failed-reopens-arm-cooldown-fourth-call-is-typed-refusal
  (testing "WO-M3 #4: N=3 consecutive open/reopen failures -> the 4th call
            throws :mcp/cooldown WITHOUT invoking open-fn; expiry restores retry"
    (let [clock (atom 1000)
          mgr (manager/create-manager {:max-reopen-failures 3
                                       :cooldown-ms 500
                                       :now-fn #(deref clock)})
          k [:stdio :m3/cooldown]
          calls (atom 0)
          boom (ex-info "reopen always fails" {:error/type :m3/always-fails})
          open-fn #(do (swap! calls inc) (throw boom))]
      ;; failures 1..3: each invokes open-fn and lands :broken / :cooldown
      (doseq [i [1 2 3]]
        (is (= :thrown (:status (try-call #(manager/get-or-open! mgr k open-fn))))
            (str "attempt " i " throws through"))
        (is (= i @calls) (str "attempt " i " invoked open-fn")))
      (let [e (snap-entry mgr k)]
        (is (= :cooldown (:state e)) (pr-str e))
        (is (= 3 (:fail-count e)) "three consecutive failures counted")
        (is (= 1500 (:cooldown-until e)) "cooldown-until = now + cooldown-ms (injectable clock)")
        (is (= :m3/always-fails (get-in e [:health :last-error :error/type]))
            "cooldown entry keeps the triggering last-error"))
      ;; the 4th call: typed refusal, open-fn UNTOUCHED (counting assertion)
      (let [r (try-call #(manager/get-or-open! mgr k open-fn))]
        (is (= :thrown (:status r)))
        (is (= :mcp/cooldown (:error/type (ex-data (:value r))))
            (pr-str (ex-data (:value r))))
        (is (= 3 @calls) "open-fn NOT invoked while cooling down")
        (is (some? (get-in (ex-data (:value r)) [:last-error]))
            "the refusal carries the last error for diagnosis")
        (is (pos? (get-in (ex-data (:value r)) [:retry-in-ms]))
            "the refusal carries the remaining window"))
      ;; advance past the window: the next call retries open-fn
      (swap! clock + 501)
      (is (= :thrown (:status (try-call #(manager/get-or-open! mgr k open-fn)))))
      (is (= 4 @calls) "expired cooldown lets the reopen touch open-fn again")
      (let [e (snap-entry mgr k)]
        (is (= :cooldown (:state e)) "a still-failing reopen re-arms the cooldown")
        (is (= 4 (:fail-count e)))
        (is (= 2001 (:cooldown-until e)) "fresh window from the new failure time"))
      (swap! clock + 600)
      (let [raw (Object.)
            ok-open (fn [] (swap! calls inc) (fake-managed raw))
            got (try-call #(manager/get-or-open! mgr k ok-open))
            e (snap-entry mgr k)]
        (is (= :result (:status got)) (pr-str got))
        (is (= 5 @calls))
        (is (= :ready (:state e)) (pr-str e))
        (is (= 0 (:fail-count e)) "eventual success resets the streak")
        (is (identical? raw (:client (:client (manager/pool-get mgr k))))
            "healed entry serves the newly opened managed record")))))

;; ---------------------------------------------------------------------------
;; h4 — reconnecting is a real single-flight state (5 concurrent callers)
;; ---------------------------------------------------------------------------

(deftest h4-reconnecting-single-flight-five-callers-one-open-one-managed
  (testing "WO-M3 #5/#2: while a broken entry is being reopened (latched
            open-fn), concurrent callers park on the single-flight promise;
            open-fn runs exactly ONCE and everyone gets the SAME managed record"
    (let [mgr (manager/create-manager)
          k [:stdio :m3/reconnect-single-flight]
          raw1 (Object.)
          raw2 (Object.)
          _ (manager/put-ready mgr k (fake-managed raw1))    ;; live, generation 1
          _ (manager/mark-broken mgr k {:error/type :m3/simulated-death})
          _ (is (= :broken (:state (snap-entry mgr k))) "fixture: entry broken")
          calls (atom 0)
          entered (promise)
          release (promise)
          open-fn #(do (swap! calls inc)
                       (deliver entered true)
                       @release
                       (fake-managed raw2))
          collector (promise)
          cf (future (deliver collector
                              (conc/raced (repeat 5 (fn [] (try-call #(manager/get-or-open!
                                                                      mgr k open-fn))))
                                          :timeout-ms 8000)))
          _ @entered
          mid-state (:state (snap-entry mgr k))]
      (is (= :reconnecting mid-state)
          "entry observably sits in :reconnecting while open-fn is latched")
      (deliver release true)
      (let [results @collector]
        (is (every? #(= :result (:status %)) results) (pr-str results))
        (is (= 1 @calls) "open-fn invoked exactly once across 5 concurrent callers")
        ;; thunks capture try-call outcomes, so the managed record lives one
        ;; level down: (-> result :value :value)
        (let [vs (mapv (comp :value :value) results)]
          (is (every? #(and (map? %) (identical? raw2 (:client %))) vs)
              "every caller holds a managed record wrapping the NEW raw client")
          (is (every? #(identical? (first vs) %) (rest vs))
              "all 5 received the identical managed record (M1 contract: all take
               the managed value or all throw — nobody saw the stale one)"))
        (let [e (snap-entry mgr k)]
          (is (= :ready (:state e)))
          (is (= 2 (:generation e)) "one reopen attempt = one generation bump (1 -> 2)")
          (is (= 0 (:fail-count e))))))))

;; ---------------------------------------------------------------------------
;; h5 — INV-01 regression: mark-broken err-data is redacted, never plaintext
;; ---------------------------------------------------------------------------

(deftest h5-mark-broken-err-data-carries-only-redacted-secrets
  (testing "WO-M3 #5: broken-err-data (what the bridge/source wiring feeds
            mark-broken) redacts embedded transport configs — including ones
            HOSTILELY embedded inside the exception's own data — and stays EDN-clean.
            Canary channels chosen OUTSIDE err/sanitize's fixed secret-key list
            (:env values, custom-named headers): those are exactly the leak
            channel BT9/M-hygiene records for bare err/sanitize."
    (let [header-secret "Bearer SK-M3-CANARY-9f3b1c"
          env-secret "sk-m3-env-canary-do-not-leak"
          hostile-ex (ex-info
                      "transport blew up"
                      {:error/type :mcp/call-tool-failed
                       :mcp/transport-config
                       {:type :http :url "https://m3.example.test"
                        :headers {"X-Custom-Credential" header-secret}
                        :env {"M3_TOKEN" env-secret}}
                       ;; a second, DEEPER embedding an attacker-controlled
                       ;; error payload could carry
                       :nested {:chain [{:mcp/transport-config
                                         {:env {"M3_TOKEN" env-secret}}}]}})
          cfg {:type :http :url "https://m3.example.test"
               :headers {"X-Custom-Credential" header-secret}
               :env {"M3_TOKEN" env-secret}}
          ed (manager/broken-err-data hostile-ex cfg)]
      (is (= "[REDACTED]" (get-in ed [:mcp/transport-config :headers]))
          "top-level transport config redacted for display")
      (is (= "[REDACTED]" (get-in ed [:mcp/transport-config :env]))
          "env channel redacted too")
      (is (= "[REDACTED]"
             (get-in ed [:error/data :nested :chain 0 :mcp/transport-config :env]))
          "even deeply embedded transport configs are redacted")
      (let [s (pr-str ed)]
        (is (not (.contains ^String s header-secret))
            "header secret appears NOWHERE in the mark-broken payload")
        (is (not (.contains ^String s env-secret))
            "env secret appears NOWHERE in the mark-broken payload")
        (is (.contains ^String s "[REDACTED]")
            "the redaction marker is present"))
      (is (= ed (edn/read-string (pr-str ed)))
          "payload round-trips EDN (GC-22 boundary discipline)"))))

;; ---------------------------------------------------------------------------
;; h7 — the documented state machine and the implementation agree
;; ---------------------------------------------------------------------------

(deftest h7-documented-state-machine-matches-observable-states
  (testing "WO-M3 #7: every state named in get-or-open!'s docstring is
            drivable and observable via pool-snapshot, and the docstring names
            every state the implementation can produce"
    (let [docstring (:doc (meta #'manager/get-or-open!))
          doc-states [:connecting :reconnecting :ready :broken :cooldown]
          observed (atom [])
          note! (fn [s] (swap! observed conj s) s)
          clock (atom 5000)
          ;; N=1: a single failed reopen arms the cooldown immediately —
          ;; shortest deterministic path to :cooldown
          mgr (manager/create-manager {:max-reopen-failures 1
                                       :cooldown-ms 10000
                                       :now-fn #(deref clock)})
          k [:stdio :m3/tour]]
      ;; every documented state is NAMED in the docstring text
      (doseq [s doc-states]
        (is (.contains ^String docstring (name s))
            (str "docstring names state " s)))
      ;; 1. absent -> :connecting (opener parked inside open-fn)
      (let [l1 (promise) g1 (promise)
            t (future (try-call
                       (fn [] (manager/get-or-open!
                               mgr k
                               (fn [] (deliver l1 true) @g1
                                      (fake-managed (Object.)))))))]
        @l1
        (note! (:state (snap-entry mgr k)))
        (deliver g1 true)
        (is (= :result (:status @t)))
        ;; 2. -> :ready (observed only after the opener joined: the success
        ;; swap lands BEFORE the promise delivery, so join implies swapped)
        (note! (:state (snap-entry mgr k))))
      ;; 3. :ready -> :broken (mark-broken)
      (manager/mark-broken mgr k {:error/type :m3/tour-death})
      (note! (:state (snap-entry mgr k)))
      ;; 4. :broken -> :reconnecting (latched reopen; fresh promises —
      ;; promises deliver once, never reuse across steps)
      (let [l2 (promise) g2 (promise)
            t (future (try-call
                       (fn [] (manager/get-or-open!
                               mgr k
                               (fn [] (deliver l2 true) @g2
                                      (throw (ex-info "tour fail"
                                                      {:error/type :m3/tour-fail})))))))]
        @l2
        (note! (:state (snap-entry mgr k)))
        (deliver g2 true)
        (is (= :thrown (:status @t)))
        ;; 5. fail-count >= N -> :cooldown (swap precedes the rethrow, so the
        ;; joined future guarantees the state is observable now)
        (note! (:state (snap-entry mgr k))))
      (is (= (set doc-states) (set @observed))
          "the tour drove EXACTLY the documented state set, no gaps, no extras")
      (is (= (count doc-states) (count @observed))
          "no undocumented state ever appeared"))))
