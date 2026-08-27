(ns evoclj.mcp.manager-refcount-test
  "WO-M5 — refcount 收口: pending-owners, dispose! via the REAL manager
   reference, ownerless-resurrection regression (probe J), zombie-client
   reaping, and Integrant halt wiring.

   The lifecycle contract under test:

     A. OWNERSHIP REGISTRATION
        acquire on an ABSENT pool entry registers the owner as PENDING;
        entry creation pre-seeds :owners from pending; every transition
        that installs or preserves an entry preserves owners/generation/
        metrics. An owner that never registered is INERT to release.

     B. DISPOSE RETURNS TO THE ORIGIN POOL
        dispose! releases through the ToolEntry's OWN :manager/:conn-key
        — never a reconstructed key against a global fallback.

     C. NO OWNERLESS RESURRECTION (probe J)
        When the last owner releases while an open attempt is in flight,
        the entry is marked :draining and the open outcome NEVER becomes
        a live pooled client: success hands the fresh record to the
        zombie reaper, failure drops the entry. Stale/duplicate releases
        are no-ops BY CONSTRUCTION, so they can never close a client
        opened AFTER they fired — asserted here with real fake-server
        process liveness.

     D. ZOMBIE HARVEST
        mark-broken strips the dead :client into an out-of-pool tombstone
        queue drained asynchronously OUTSIDE any swap!; shutdown! (and
        therefore ig/halt-key! :mcp/manager) harvests everything.

   Production-path discipline (INV-09): r3/r4/r5 drive real fake-server
   subprocesses through the production bridge/manager APIs; unit paths
   use fake-managed records shaped exactly like evoclj.mcp.client/open!
   returns."
  (:require [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.support.fake-server :as fake]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.support.concurrency :as conc]
            [integrant.core :as ig])
  (:import [java.io File]))

;; ---------------------------------------------------------------------------
;; fixtures (same conventions as the M1/M3/M4 suites)
;; ---------------------------------------------------------------------------

(defn- fake-managed
  "Minimal managed-record-shaped map, same keys production open! returns."
  [raw]
  {:client raw
   :closed? false
   :open-count 1
   :call-count 0
   :transport-config {:type :stdio :command "fake" :args []}})

(defn- try-call
  "{:status :result|:thrown :value v} — raced/future workers keep Throwables
   as data."
  [f]
  (try
    {:status :result :value (f)}
    (catch Throwable t
      {:status :thrown :value t})))

(defn- snap-entry
  [mgr k]
  (get-in (manager/pool-snapshot mgr) [:pools k]))

(defn- quiesce!
  "Best-effort drain barrier for the manager's zombie reaper (same-thread
   dispatches are awaited deterministically)."
  [mgr]
  (manager/quiesce-reaper! mgr))

;; ---------------------------------------------------------------------------
;; (1) acquire-before-open: owners survive the absent-entry window
;; ---------------------------------------------------------------------------

(deftest r1-acquire-before-open-registers-pending-owner-and-survives-open
  (testing "WO-M5 #2: an owner acquired while the entry is ABSENT is
            registered pending, pre-seeded into the entry at creation,
            and still present after the open succeeds"
    (let [mgr (manager/create-manager)
          k [:stdio :r5/pending {} 0]]
      ;; entry absent -> the acquisition lands in the pending registry
      (is (nil? (manager/acquire mgr k :owner-a))
          "absent entry: acquire returns nil (contract unchanged)")
      (is (= {k #{:owner-a}} (:pending-owners (manager/pool-snapshot mgr)))
          "the pending owner is registered in the manager atom")
      ;; open succeeds -> pending is folded into the entry's owners
      (let [raw (Object.)
            got (manager/get-or-open! mgr k #(fake-managed raw))]
        (is (identical? raw (:client got)) "open result returned verbatim")
        (let [e (snap-entry mgr k)]
          (is (= :ready (:state e)) (pr-str e))
          (is (= #{:owner-a} (:owners e))
              "the pre-open owner was pre-seeded into the live entry"))
        (is (= {} (:pending-owners (manager/pool-snapshot mgr)))
            "pending registry cleared once the entry absorbed it"))
      ;; normal post-open acquisitions join the same owner set
      (manager/acquire mgr k :owner-b)
      (is (= #{:owner-a :owner-b} (:owners (snap-entry mgr k))))
      ;; refcounted release: entry survives until the LAST owner leaves
      (manager/release mgr k :owner-a)
      (is (= #{:owner-b} (:owners (snap-entry mgr k))) "entry alive with one owner")
      (is (some? (:client (manager/pool-get mgr k))) "client kept while owned")
      (manager/release mgr k :owner-b)
      (is (nil? (snap-entry mgr k)) "last release tears the entry down"))))

(deftest r2-pending-acquired-mid-flight-folds-into-successful-open
  (testing "WO-M5 #2: an owner arriving DURING :connecting is merged into
            the entry when the open succeeds"
    (let [mgr (manager/create-manager)
          k [:stdio :r5/midflight {} 0]
          entered (promise)
          gate (promise)
          open-fn #(do (deliver entered true) @gate (fake-managed (Object.)))
          f (future (try-call #(manager/get-or-open! mgr k open-fn)))]
      @entered
      (is (= :connecting (:state (snap-entry mgr k))) "fixture: mid-connect")
      ;; entry EXISTS in :connecting, so the owner joins its owner set
      ;; directly (the pending registry is only for the absent window)
      (manager/acquire mgr k :late-owner)
      (is (= #{:late-owner} (:owners (snap-entry mgr k)))
          "mid-flight acquisition lands on the live connecting entry")
      (deliver gate true)
      (is (= :result (:status @f)) (pr-str @f))
      (let [e (snap-entry mgr k)]
        (is (= :ready (:state e)))
        (is (= #{:late-owner} (:owners e))
            "owner survived the SUCCESS swap into :ready"))
      (is (= {} (:pending-owners (manager/pool-snapshot mgr)))))))

;; ---------------------------------------------------------------------------
;; (2) dispose! returns to the INJECTED manager, with the real conn-key
;; ---------------------------------------------------------------------------

(deftest r3-dispose-releases-through-the-injected-manager
  (testing "WO-M5 #1 + #4: dispose! tears down the entry in the manager the
            provider was BUILT with (old code released a reconstructed key
            against the global fallback, leaking the injected pool entry)"
    (let [mgr (manager/create-manager)]
      (fake/with-fake-server [srv {:mode :ok :tool-count 1}]
        (let [;; baseline AFTER the supervised server started: this test
              ;; audits only what IT spawned beyond the wrapper
              pre-existing (fake/processes-matching-pids
                            fake/fake-server-process-pattern)
              tcfg (:config srv)
              ck (manager/connection-key (assoc tcfg :connection/id :r5/disp))
              p (mcp-bridge/mcp-provider
                 {:transport-config tcfg
                  :tool/id          :r5/disp
                  :tool/mcp-name    "echo"
                  :input-schema     [:map]
                  :output-schema    [:map]
                  :connection/id    :r5/disp
                  :manager          mgr})
              req (proto/normalize-request p {:payload {:tool/id :r5/disp :args {}}})]
          ;; construction registered the owner pending under the REAL key
          (is (= {ck #{:r5/disp}} (:pending-owners (manager/pool-snapshot mgr)))
              "make-tool-entry registered its owner pending under its own conn-key")
          (is (false? (get-in (proto/execute-request! p req) [:value :mcp/is-error]))
              "one call opens the pooled connection")
          (let [e (snap-entry mgr ck)]
            (is (= :ready (:state e)) (pr-str e))
            (is (= #{:r5/disp} (:owners e)) "entry owned by the tool-id")
            (is (:has-client? e) "live pooled client present"))
          ;; THE FIX UNDER TEST: dispose goes to mgr/ck, not fallback/hack
          (mcp-bridge/dispose! p)
          (is (nil? (snap-entry mgr ck))
              "injected manager's entry torn down by dispose!")
          (is (empty? (:pools (manager/pool-snapshot mgr)))
              "no residue anywhere in the injected manager")
          (quiesce! mgr)
          (is (empty? (fake/await-no-new-process-matching
                       fake/fake-server-process-pattern pre-existing))
              "the pooled child process was reclaimed")))
      ;; dispose of a CALL-SCOPED provider (no connection id) is a safe no-op
      (let [q (mcp-bridge/mcp-provider
               {:transport-config {:type :stdio :command "noop" :args []}
                :tool/id :r5/scoped :tool/mcp-name "x"
                :input-schema [:map] :output-schema [:map]})]
        (is (nil? (mcp-bridge/dispose! q)) "dispose! without pooling is a no-op")))))

(deftest r3b-stale-release-is-inert-by-construction
  (testing "WO-M5 #2: releasing an owner that is NOT registered must not
            close anything nor remove the entry (the old code treated any
            release against empty owners as 'last one out' — the landmine
            behind probe J)"
    (let [mgr (manager/create-manager)
          k [:stdio :r5/stale {} 0]
          raw (Object.)]
      (manager/acquire mgr k :real-owner)
      (manager/put-ready mgr k (fake-managed raw))
      ;; stale owner nobody ever acquired
      (manager/release mgr k :ghost-owner)
      (let [e (snap-entry mgr k)]
        (is (= :ready (:state e)) "stale release left the entry alone")
        (is (= #{:real-owner} (:owners e)))
        (is (:has-client? e) "stale release did NOT close the pooled client"))
      ;; duplicate release beyond the last real owner is inert too
      (manager/release mgr k :real-owner)
      (is (nil? (snap-entry mgr k)) "the REAL last release tore it down")
      (manager/release mgr k :real-owner)
      (manager/release mgr k :ghost-owner)
      (is (nil? (snap-entry mgr k)) "post-teardown releases stay inert"))))

;; ---------------------------------------------------------------------------
;; (3) probe-J regression: late release must not close the reopened client
;; ---------------------------------------------------------------------------

(deftest r4-late-release-cannot-close-reopened-or-new-clients
  (testing "WO-M5 probe J: release racing a reopen must end with NO
            ownerless resurrected entry; stale releases fired afterwards
            must leave a freshly installed, newly-owned client ALIVE
            (fake-server process liveness asserted)"
    (let [mgr (manager/create-manager)
          tcfg (fake/transport-config {:mode :ok})
          ck (manager/connection-key (assoc tcfg :connection/id :r5/probe-j))
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)]
      ;; generation 1: owned, live
      (manager/acquire mgr ck :t)
      (let [g1 (mcp-client/open! tcfg)]
        (manager/put-ready mgr ck g1)
        (is (= #{:t} (:owners (snap-entry mgr ck)))))
      ;; connection dies -> mark-broken strips the dead client (zombie queue)
      (manager/mark-broken mgr ck {:error/type :r5/simulated-death})
      (let [e (snap-entry mgr ck)]
        (is (= :broken (:state e)))
        (is (not (contains? e :has-client?)) "dead client stripped from pool"))
      (quiesce! mgr)
      (is (empty? (fake/processes-matching fake/fake-server-process-pattern))
          "fixture hygiene: generation-1 child already harvested before the race")
      ;; latched REOPEN in flight
      (let [started (promise)
            gate (promise)
            open-fn #(do (deliver started true) @gate (mcp-client/open! tcfg))
            f (future (try-call #(manager/get-or-open! mgr ck open-fn)))]
        @started
        (is (= :reconnecting (:state (snap-entry mgr ck))))
        ;; N concurrent releases DURING the in-flight reopen
        (let [rels (conc/raced (repeat 6 #(manager/release mgr ck :t))
                               :timeout-ms 8000)]
          (is (every? #(= :result %) (map :status rels)) (pr-str rels)))
        (deliver gate true)
        (is (= :result (:status @f)) "the opener itself still returns the record")
        ;; INVARIANT: no ownerless resurrection
        (is (nil? (snap-entry mgr ck))
            "a draining reopen NEVER lands as an ownerless :ready entry")
        ;; M more LATE releases after everything settled
        (doseq [_ (range 4)]
          (manager/release mgr ck :t))
        (is (nil? (snap-entry mgr ck)) "late releases stay inert")
        ;; THE ACCEPTANCE CRITERION: install a NEW owned client, then fire
        ;; the stale owner's releases AGAIN — the new client must SURVIVE.
        (manager/acquire mgr ck :t2)
        (let [fresh (mcp-client/open! tcfg)
              pids-after (fn [] (fake/processes-matching-pids
                                 fake/fake-server-process-pattern))]
          (manager/put-ready mgr ck fresh)
          (let [new-pid-set (cset/difference (pids-after) pre-existing)]
            (is (= 1 (count new-pid-set))
                (str "exactly one new child (the fresh client): " (pr-str new-pid-set)))
            ;; stale-generation releases, serial AND raced
            (doseq [_ (range 3)] (manager/release mgr ck :t))
            (conc/raced (repeat 5 #(manager/release mgr ck :t)) :timeout-ms 8000)
            (let [e (snap-entry mgr ck)]
              (is (= :ready (:state e)) "new entry untouched by stale releases")
              (is (= #{:t2} (:owners e)))
              (is (:has-client? e)))
            (is (= new-pid-set (pids-after))
                "LATE RELEASE DID NOT CLOSE THE NEW CLIENT (process still alive)"))
          ;; cleanup: the real owner releases; final audit must be clean —
          ;; this also reaps the draining path's orphan record (its child).
          (manager/release mgr ck :t2)
          (quiesce! mgr)
          (is (nil? (snap-entry mgr ck)) "cleanup teardown worked")
          (is (empty? (fake/await-no-new-process-matching
                       fake/fake-server-process-pattern pre-existing))
              "zero fake-server processes remain: gen-1 zombie, reopen orphan,
               and fresh client all harvested"))))))

(deftest r4b-draining-reopen-failure-drops-the-entry-and-propagates
  (testing "probe J failure side: last owner releases during a failing
            reconnect -> the failure outcome drops the entry instead of
            leaving a broken husk, and the opener still throws"
    (let [mgr (manager/create-manager)
          k [:stdio :r5/draining-fail {} 0]]
      (manager/acquire mgr k :solo)
      (manager/put-ready mgr k (fake-managed (Object.)))
      (manager/mark-broken mgr k {:error/type :r5/death})
      (let [entered (promise)
            gate (promise)
            boom (ex-info "reopen fails" {:error/type :r5/reopen-boom})
            open-fn #(do (deliver entered true) @gate (throw boom))
            f (future (try-call #(manager/get-or-open! mgr k open-fn)))]
        @entered
        (manager/release mgr k :solo)          ; draining begins
        (deliver gate true)
        (let [r @f]
          (is (= :thrown (:status r)) "opener propagates the failure")
          (is (identical? boom (:value r))))
        (is (nil? (snap-entry mgr k))
            "draining failure removed the entry (no unowned :broken husk)")
        (is (= {} (:pending-owners (manager/pool-snapshot mgr))))))))

;; ---------------------------------------------------------------------------
;; (4) zombie harvest: stripped dead clients are actually closed
;; ---------------------------------------------------------------------------

(deftest r5-mark-broken-strip-eventually-harvests-the-dead-process
  (testing "WO-M5 gap (a): the client record stripped by mark-broken is
            moved to the out-of-pool tombstone queue and its stdio child
            process is reclaimed asynchronously"
    (let [mgr (manager/create-manager)
          tcfg (fake/transport-config {:mode :ok})
          ck (manager/connection-key (assoc tcfg :connection/id :r5/zombie))
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)
          zomb (mcp-client/open! tcfg)]
      (manager/acquire mgr ck :t)
      (manager/put-ready mgr ck zomb)
      (let [alive-before (count (fake/processes-matching
                                 fake/fake-server-process-pattern))]
        (is (pos? alive-before) "fixture: the pooled child is alive"))
      (manager/mark-broken mgr ck {:error/type :r5/simulated-death})
      (is (not (contains? (snap-entry mgr ck) :has-client?))
          "strip happened (pool no longer holds the record)")
      ;; async harvest: quiesce then bounded poll for process death
      (quiesce! mgr)
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "the STRIPPED client's process was eventually reclaimed")
      (manager/release mgr ck :t))))

(deftest r5b-shutdown-harvests-queued-zombies-and-live-clients
  (testing "shutdown! drains the tombstone queue even without a quiesce
            nudge and closes every pooled client (halt semantics)"
    (let [mgr (manager/create-manager)
          tcfg (fake/transport-config {:mode :ok})
          ck (manager/connection-key (assoc tcfg :connection/id :r5/shutdown))
          pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)
          c1 (mcp-client/open! tcfg)
          _ (manager/acquire mgr ck :t)
          _ (manager/put-ready mgr ck c1)
          ;; second key whose client gets stripped right before shutdown
          c2 (mcp-client/open! tcfg)
          ck2 (manager/connection-key (assoc tcfg :connection/id :r5/shutdown-2))]
      (manager/acquire mgr ck2 :t)
      (manager/put-ready mgr ck2 c2)
      (manager/mark-broken mgr ck2 {:error/type :r5/simulated-death})
      (manager/shutdown! mgr)
      (is (empty? (:pools (manager/pool-snapshot mgr))) "pools reset")
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "both children gone: the queued zombie AND the live pooled client")
      ;; shutdown twice stays safe (idempotent halt contract)
      (manager/shutdown! mgr)
      (is (empty? (:pools (manager/pool-snapshot mgr)))))))

;; ---------------------------------------------------------------------------
;; (5) Integrant wiring: :mcp/manager component + halt closes everything
;; ---------------------------------------------------------------------------

(deftest r6-system-edn-declares-mcp-manager-component
  (testing "resources/system.edn carries the :mcp/manager component so the
            host builds and halts it through Integrant"
    (let [cfg (ig/read-string (slurp (io/resource "system.edn")))]
      (is (contains? cfg :mcp/manager)
          ":mcp/manager present in the shipped host config"))))

(deftest r6b-ig-halt-of-injected-manager-closes-pooled-processes
  (testing "ig/init builds a manager component; ig/halt! (halt-key!
            :mcp/manager -> shutdown!) closes its pooled children"
    (let [pre-existing (fake/processes-matching-pids
                        fake/fake-server-process-pattern)
          system (ig/init {:mcp/manager {}})
          mgr (:mcp/manager system)
          tcfg (fake/transport-config {:mode :ok})
          ck (manager/connection-key (assoc tcfg :connection/id :r5/halt))]
      (try
        (is (some? mgr) "component built")
        (manager/acquire mgr ck :t)
        (manager/put-ready mgr ck (mcp-client/open! tcfg))
        (is (:has-client? (snap-entry mgr ck)) "child pooled and alive")
        (finally
          (ig/halt! system)))
      (is (empty? (fake/await-no-new-process-matching
                   fake/fake-server-process-pattern pre-existing))
          "halt! closed every pooled client process")
      (is (empty? (:pools (manager/pool-snapshot mgr)))))))

;; ---------------------------------------------------------------------------
;; preservation semantics pinned (guards the WO-M5 rewrite)
;; ---------------------------------------------------------------------------

(deftest r7-put-ready-preserves-owners-generation-metrics
  (testing "put-ready over an existing entry keeps owners/generation/metrics
            (and folds pending), only swapping the client record"
    (let [mgr (manager/create-manager)
          k [:stdio :r5/preserve {} 0]]
      (manager/acquire mgr k :keeper)
      (manager/put-ready mgr k (fake-managed (Object.)))
      (manager/set-metrics mgr k #(assoc % :call-count 7))
      ;; bump generation like a healing cycle would
      (manager/mark-broken mgr k {:error/type :r5/x})
      (manager/get-or-open! mgr k #(fake-managed (Object.)))
      (let [before (snap-entry mgr k)]
        (is (= 2 (:generation before)) "fixture: two attempts counted")
        (is (= 7 (get-in before [:metrics :call-count]))))
      (manager/acquire mgr k :joiner)   ; pending (entry exists -> owners)
      (let [raw3 (Object.)
            returned (manager/put-ready mgr k (fake-managed raw3))]
        (is (map? returned) "put-ready returns the entry")
        (let [e (snap-entry mgr k)]
          (is (= :ready (:state e)))
          (is (= #{:keeper :joiner} (:owners e)) "owners preserved+folded")
          (is (= 3 (:generation e)) "generation counts the put-ready attempt")
          (is (= 7 (get-in e [:metrics :call-count])) "metrics preserved")
          (is (identical? raw3 (:client (:client (manager/pool-get mgr k))))
              "client swapped to the new record"))))))

(deftest r7b-success-swap-preserves-entry-fields-on-healed-reopen
  (testing "the reopen success swap merges into the existing entry instead
            of building a bare {:state :ready ...} map (probe J root cause)"
    (let [mgr (manager/create-manager)
          k [:stdio :r5/merge {} 0]]
      (manager/acquire mgr k :o1)
      (manager/put-ready mgr k (fake-managed (Object.)))
      (manager/set-metrics mgr k #(assoc % :call-count 42))
      (manager/mark-broken mgr k {:error/type :r5/death})
      (let [raw2 (Object.)
            _ (manager/get-or-open! mgr k #(fake-managed raw2))
            e (snap-entry mgr k)]
        (is (= :ready (:state e)))
        (is (= #{:o1} (:owners e)) "owners survived the reopen success swap")
        (is (= 2 (:generation e)) "generation preserved across heal")
        (is (= 42 (get-in e [:metrics :call-count]))
            "metrics survived the reopen success swap")
        (is (identical? raw2 (:client (:client (manager/pool-get mgr k)))))))))
