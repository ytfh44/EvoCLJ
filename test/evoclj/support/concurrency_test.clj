(ns evoclj.support.concurrency-test
  "WO-T4 — deterministic concurrency test primitives (test-side kit).

  Required coverage:
  1. happy    — raced runs two thunks to completion and returns results
                ALIGNED BY INPUT POSITION (thread 0's value at index 0),
                even when completion order is reversed;
  2. branch   — a throwing thunk lands as :thrown on its own thread
                while the others return normally; a hung thunk is marked
                :timeout and total elapsed stays ≈ timeout-ms;
  3. fault    — a thunk throwing an Error (non-Exception Throwable) is
                reported as :thrown too — raced itself never rethrows;
  4. concurrency (bootstrap) — 8 barrier-call participants released
                together increment one shared atom exactly 8 times, and
                their post-barrier start timestamps span < 100 ms — a
                counting-window assertion proving the release really is
                simultaneous (unsynchronized thread starts cannot
                guarantee any such window);
  5. regression — evoclj.adversarial.concurrency-test does not require
                this kit (machine-checked here) and keeps passing when
                rerun next to this namespace via targeted runner
                invocation (same convention as failpoint-test);
  6. contract — public arglists == the WO signatures, docstrings name
                the synchronization mechanism and the :timeout marking,
                default constants equal the documented 10 s / 50 ms;
  7. bootstrap (raced) — WO normative CountDownLatch start gate: N >= 8
                 bodies stamp their first post-release instant (span
                 < 100 ms), each observes ALL n worker threads alive at
                 its first statement (release follows full start-up),
                 plus an all-arrived mutual-visibility handshake so a
                 missing/partial release cannot hide;
  8. isolation — raced bodies run WITHOUT the caller's thread-local
                 bindings: a ^:dynamic var bound by the caller reads as
                 the ROOT value inside every thunk;
  plus eventually's :attempts diagnostic equals the EXACT number of
  probes actually made (no off-by-one against 'probes made (>= 1)').

  Determinism policy (WO: 确定性失败优先): timeouts THROW or MARK, they
  never pass silently and never hang the suite. Timing assertions use
  lower bounds guaranteed by Thread/sleep semantics plus upper bounds
  with ≥ 2 s scheduler slack, so this suite fails only when a
  primitive is actually broken — never on load noise."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.support.concurrency :as conc])
  (:import (java.nio.file Files LinkOption Paths)
           (java.util.concurrent CountDownLatch)))

;; ---------------------------------------------------------------------------
;; 1. happy — results aligned per thread even when completion order reverses
;; ---------------------------------------------------------------------------

(deftest raced-happy-two-thunks-aligned-by-thread
  (testing "two thunks both complete; the result vector is aligned to
            the input order (index i = thread i), not completion order"
    (let [results (conc/raced
                   [(fn [] (Thread/sleep 40) :slow-left)
                    (fn [] {:quick "right"})])]
      (is (= 2 (count results)))
      (is (= [:result :result] (mapv :status results)))
      (is (= :slow-left (:value (nth results 0)))
          "thread 0's value at index 0 (despite finishing second)")
      (is (= {:quick "right"} (:value (nth results 1)))
          "thread 1's value at index 1 (despite finishing first)")
      (is (= ["evoclj-raced-0" "evoclj-raced-1"] (mapv :thread results))
          "each result carries its own named worker thread")))
  (testing "empty input -> empty result, nothing started"
    (is (= [] (conc/raced [])))))

;; ---------------------------------------------------------------------------
;; 2. branch — throwing / hung threads isolated, timeout bounded
;; ---------------------------------------------------------------------------

(deftest raced-throwing-thread-isolated-others-unaffected
  (let [sent (ex-info "boom" {:code 7})
        results (conc/raced
                 [(fn [] :fine)
                  (fn [] (throw sent))
                  (fn [] :also-fine)])]
    (testing "the healthy threads are untouched"
      (is (= :result (:status (nth results 0))))
      (is (= :fine (:value (nth results 0))))
      (is (= :result (:status (nth results 2))))
      (is (= :also-fine (:value (nth results 2)))))
    (testing "only the guilty thread reports :thrown, carrying the ORIGINAL
              throwable (no wrapping, no substitution)"
      (is (= :thrown (:status (nth results 1))))
      (is (identical? sent (:value (nth results 1)))))))

(deftest raced-timeout-marks-thread-and-bounds-elapsed
  (let [hang (CountDownLatch. 1)
        timeout-ms 400
        t0 (System/nanoTime)
        results (conc/raced
                 [(fn [] (.await hang))            ;; would hang forever
                  (fn [] :finished)]
                 :timeout-ms timeout-ms)
        elapsed-ms (/ (- (System/nanoTime) t0) 1e6)]
    (testing "the hung thread is marked :timeout (suite must not hang)"
      (let [r (nth results 0)]
        (is (= :timeout (:status r)))
        (is (nil? (:value r)) "timed-out threads carry no value")))
    (testing "the fast thread still completes inside the same race"
      (is (= :result (:status (nth results 1))))
      (is (= :finished (:value (nth results 1)))))
    (testing "total elapsed ≈ timeout-ms"
      (is (>= elapsed-ms timeout-ms)
          (str "elapsed " elapsed-ms " ms must reach the full budget"))
      (is (< elapsed-ms (+ timeout-ms 2000))
          (str "elapsed " elapsed-ms " ms must not overshoot the budget
by more than scheduler slack")))))

;; ---------------------------------------------------------------------------
;; 3. fault — non-Exception Throwables land in :thrown too
;; ---------------------------------------------------------------------------

(deftest raced-reports-error-throwables-as-thrown
  (let [boom (AssertionError. "t4-error")
        results (conc/raced
                 [(fn [] (throw boom))
                  (fn [] :survivor)])]
    (testing "an Error is captured as :thrown, never propagated out of raced"
      (is (= :thrown (:status (nth results 0))))
      (is (identical? boom (:value (nth results 0))))
      (is (= "t4-error"
             (.getMessage ^Throwable (:value (nth results 0))))))
    (is (= :result (:status (nth results 1))))
    (is (= :survivor (:value (nth results 1)))
        "raced returns normally regardless of worker failures — failures
are DATA the caller asserts on")))

;; ---------------------------------------------------------------------------
;; 4. concurrency bootstrap — 8 participants genuinely released together
;; ---------------------------------------------------------------------------

(deftest barrier-call-eight-participants-synchronize-and-count
  (let [counter (atom 0)
        starts (atom [])
        bump (fn []
               ;; first statement after the barrier rendezvous: the
               ;; post-release instant, recorded for the counting window
               (swap! starts conj (System/nanoTime))
               (swap! counter inc))
        results (conc/barrier-call (repeat 8 bump))
        spread-ms (/ (- (apply max @starts) (apply min @starts)) 1e6)]
    (testing "all 8 participants passed the barrier and completed"
      (is (= 8 (count results)))
      (is (every? #(= :result (:status %)) results)
          (str "statuses: " (pr-str (mapv :status results))))
      (is (= (set (map #(str "evoclj-barrier-" %) (range 8)))
             (set (mapv :thread results)))
          "eight distinct named participants")
      (is (= 8 @counter)
          "shared atom incremented exactly once per participant"))
    (testing "counting window: post-barrier start timestamps span < 100 ms —
              the bootstrap proof that the release really is synchronous"
      (is (= 8 (count @starts)))
      (is (< spread-ms 100.0)
          (str "post-barrier start spread was " spread-ms " ms")))))

(deftest barrier-call-values-aligned-and-participant-failure-reported
  (testing "return values aligned by participant position, as in raced"
    (let [results (conc/barrier-call (mapv (fn [i] #(str "v" i)) (range 4)))]
      (is (= ["v0" "v1" "v2" "v3"] (mapv :value results)))
      (is (every? #(= :result (:status %)) results))))
  (testing "a participant failing AFTER the rendezvous is isolated, as in raced"
    (let [sent (ex-info "barrier-boom" {:where :after-barrier})
          results (conc/barrier-call
                   [(fn [] (throw sent))
                    (fn [] :ok)])]
      (is (= :thrown (:status (nth results 0))))
      (is (identical? sent (:value (nth results 0))))
      (is (= :result (:status (nth results 1))))
      (is (= :ok (:value (nth results 1)))))))

;; ---------------------------------------------------------------------------
;; 4b. raced bootstrap — WO normative: the CountDownLatch start gate
;;     really releases every body together ("CountDownLatch 同步起跑")
;; ---------------------------------------------------------------------------

(deftest raced-eight-thunks-start-gate-synchronizes-release
  ;; Mirrors the barrier-call bootstrap above, hardened because raw
  ;; thread-start jitter on an idle box can masquerade as synchronization.
  ;; Three independent signals must ALL hold:
  ;;   (a) counting window — every body's FIRST statement stamps its
  ;;       post-release instant; the span stays < 100 ms;
  ;;   (b) gate ordering — an intact gate counts down only AFTER every
  ;;       worker thread was started, so each body must observe ALL n
  ;;       raced threads alive at its first statement. Remove the .await
  ;;       and early bodies run while the caller is still spawning their
  ;;       siblings, which this assertion catches deterministically;
  ;;   (c) all-arrived handshake — each body counts down after stamping,
  ;;       then awaits all n stamps, so success proves every body really
  ;;       executed its first statement (no hiding behind never-run
  ;;       threads).
  (let [n 16
        prefix "evoclj-raced-"
        observations (atom [])
        arrived (CountDownLatch. n)
        body (fn [i]
               ;; factory: returns the thunk for position i. Each thunk's
               ;; FIRST statement is the post-gate instant. atom+swap!
               ;; keeps concurrent appends lossless where a volatile
               ;; read-modify-write could silently drop one.
               (fn []
                 (let [stamp (System/nanoTime)
                       ;; same breath: how many raced workers are alive
                       ;; RIGHT NOW (thread liveness is globally visible
                       ;; once Thread.start returns).
                       siblings (count (filter (fn [^Thread t]
                                                 (.startsWith (.getName t) prefix))
                                               (keys (Thread/getAllStackTraces))))]
                   (swap! observations conj {:stamp stamp :siblings siblings}))
                 (.countDown arrived)
                 (.await arrived)
                 i))
        results (conc/raced (mapv body (range n)))
        stamps (mapv :stamp @observations)
        spread-ms (/ (- (apply max stamps) (apply min stamps)) 1e6)]
    (testing "all 16 thunks passed the gate, saw each other arrive, completed"
      (is (= n (count results)))
      (is (every? #(= :result (:status %)) results)
          (str "statuses: " (pr-str (mapv :status results))))
      (is (= (set (map #(str prefix %) (range n)))
             (set (mapv :thread results)))
          "sixteen distinct named worker threads")
      (is (= (range n) (mapv :value results))
          "values align by input position; the all-arrived handshake
           completed inside every body"))
    (testing "gate ordering: every body began only after ALL n worker
              threads already existed — forced by a release that waits
              for the full start-up"
      (is (= n (count @observations)) "one observation per body")
      (is (every? #(= n (:siblings %)) @observations)
          (str "sibling threads alive at first statement, per body: "
               (pr-str (mapv :siblings @observations)))))
    (testing "counting window: post-gate first-statement timestamps span
              < 100 ms — same bound as the barrier bootstrap. A working
              latch releases within microseconds of its countDown, so
              100 ms is orders of magnitude of scheduling slack (thread
              wakeup jitter, GC); independently scheduled starts carry
              no such ceiling."
      (is (< spread-ms 100.0)
          (str "post-gate start spread was " spread-ms " ms")))))

;; ---------------------------------------------------------------------------
;; raced isolation — fresh daemon threads, caller thread-locals absent
;; ---------------------------------------------------------------------------

(def ^:dynamic *t4-raced-binding-probe*
  "Root-binding probe for the raced thread-local isolation test."
  ::root-value)

(deftest raced-thunks-run-without-callers-thread-local-bindings
  (testing "a ^:dynamic var bound by the caller reads as the ROOT value
            inside raced bodies (docstring: bodies run 'WITHOUT the
            caller's thread-local bindings')"
    (let [results (binding [*t4-raced-binding-probe* ::caller-binding]
                    (conc/raced [(fn [] *t4-raced-binding-probe*)]))]
      (is (= :result (:status (nth results 0))))
      (is (= ::root-value (:value (nth results 0)))
          "worker saw the root binding, not the caller's thread-local"))))

;; ---------------------------------------------------------------------------
;; eventually — polling assertion helper
;; ---------------------------------------------------------------------------

(deftest eventually-returns-pred-value-once-truthy
  (let [calls (atom 0)
        t0 (System/nanoTime)
        ret (conc/eventually (fn [] (>= (swap! calls inc) 3))
                             :timeout-ms 5000 :interval-ms 20)
        elapsed-ms (/ (- (System/nanoTime) t0) 1e6)]
    (is (true? ret) "the truthy predicate answer is returned")
    (is (= 3 @calls) "probed exactly until the predicate turned truthy")
    (is (>= elapsed-ms 40.0) "the two inter-probe sleeps (~2 × 20 ms) happened")))

(deftest eventually-throws-diagnostic-ex-info-on-timeout
  (let [t0 (System/nanoTime)
        data (try
               (conc/eventually (constantly false)
                                :timeout-ms 250 :interval-ms 25)
               ::did-not-throw
               (catch clojure.lang.ExceptionInfo e (ex-data e)))
        elapsed-ms (/ (- (System/nanoTime) t0) 1e6)]
    (is (map? data)
        "deterministic failure: timeout THROWS, never quietly returns")
    (when (map? data)
      (is (= :eventually/timeout (:error/type data)))
      (is (= 250 (:timeout-ms data)))
      (is (= 25 (:interval-ms data)))
      (is (>= (:attempts data) 1) "at least one probe ran")
      (is (false? (:last-value data))
          "diagnostics keep the last predicate answer")
      (is (>= (:elapsed-ms data) 250)
          "diagnostics account for the full budget"))
    (is (< elapsed-ms 3000) "fails right at the deadline, not long after")))

(deftest eventually-attempts-equals-exact-probe-count
  (testing ":attempts equals the EXACT number of probes actually made —
            docstring contract ':attempts n ;; probes made (>= 1)', so
            the diagnostic must not under-report by one"
    (let [calls (atom 0)
          data (try
                 (conc/eventually (fn [] (swap! calls inc) false)
                                  :timeout-ms 100 :interval-ms 500)
                 ::did-not-throw
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (map? data) "timeout throws diagnostically")
      (when (map? data)
        ;; interval 500 ms > budget 100 ms pins the probe schedule
        ;; deterministically: probe 1 fires immediately, then exactly
        ;; one bounded sleep carries the clock to the deadline, then
        ;; probe 2 runs and the budget-exceeded branch throws.
        (is (= 2 @calls)
            "exactly two probes ran (1 immediate + 1 at budget end)")
        (is (= @calls (:attempts data))
            ":attempts mirrors the real probe count (was off by one)")
        (is (>= (:attempts data) 1))))))

(deftest eventually-default-interval-is-50ms
  (let [calls (atom 0)
        t0 (System/nanoTime)
        _ (conc/eventually (fn [] (>= (swap! calls inc) 4)))
        elapsed-ms (/ (- (System/nanoTime) t0) 1e6)]
    (is (= 4 @calls))
    (is (>= elapsed-ms 120.0)
        "three default-interval sleeps (~150 ms) happened")
    (is (< elapsed-ms 1000.0)
        "a much larger default interval would blow this band")))

(deftest eventually-default-timeout-is-10s
  (let [t0 (System/nanoTime)
        data (try
               (conc/eventually (constantly nil))
               ::did-not-throw
               (catch clojure.lang.ExceptionInfo e (ex-data e)))
        elapsed-ms (/ (- (System/nanoTime) t0) 1e6)]
    (is (= :eventually/timeout (:error/type data)))
    (is (= 10000 (:timeout-ms data)) "the documented default budget is 10 s")
    (is (= 50 (:interval-ms data)) "the documented default interval is 50 ms")
    (is (>= elapsed-ms 9500.0) "waited essentially the whole default budget")
    (is (< elapsed-ms 12500.0))))

;; ---------------------------------------------------------------------------
;; with-thread-dump-on-timeout — debug wrapper: dump stacks, then fail
;; ---------------------------------------------------------------------------

(deftest with-thread-dump-on-timeout-happy-and-thrown-pass-through
  (testing "body value returned when it finishes within budget"
    (is (= 42 (conc/with-thread-dump-on-timeout 5000 (+ 40 2)))))
  (testing "a throwing body is rethrown unchanged (no dump, no wrapping)"
    (let [sent (ex-info "wrapper-body" {:stage :body})]
      (is (identical? sent
                      (try
                        (conc/with-thread-dump-on-timeout 5000
                          (throw sent))
                        (catch Throwable t t)))))))

(deftest with-thread-dump-on-timeout-dumps-stacks-then-fails
  (let [hang (CountDownLatch. 1)
        err-buf (java.io.StringWriter.)
        captured (atom nil)
        t0 (System/nanoTime)
        data (binding [*err* err-buf]
               (try
                 (conc/with-thread-dump-on-timeout 300 (.await hang))
                 ::did-not-throw
                 (catch clojure.lang.ExceptionInfo e
                   (reset! captured e)
                   (ex-data e))))
        elapsed-ms (/ (- (System/nanoTime) t0) 1e6)
        dumped (str err-buf)]
    (is (= :concurrency/timeout (:error/type data))
        "timeout fails deterministically, with diagnostics")
    (is (re-find #"300" (.getMessage ^Throwable @captured))
        "the diagnostic names the exhausted budget")
    (is (re-find #"main" dumped)
        "a full thread dump (containing at least 'main') reached *err*")
    (is (>= elapsed-ms 300.0))
    (is (< elapsed-ms 3000.0))))

;; ---------------------------------------------------------------------------
;; deterministic-failure-first: malformed arguments fail loudly, up front
;; ---------------------------------------------------------------------------

(deftest invalid-arguments-fail-deterministically
  (testing "raced rejects non-function entries with an indexed diagnostic"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a function"
                          (conc/raced [(fn [] 1) :not-a-fn]))))
  (testing "raced rejects non-positive budgets"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"timeout-ms"
                          (conc/raced [(fn [] 1)] :timeout-ms 0)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"timeout-ms"
                          (conc/raced [(fn [] 1)] :timeout-ms -5))))
  (testing "barrier-call rejects empty participation and non-function entries"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"participant"
                          (conc/barrier-call [])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a function"
                          (conc/barrier-call [nil]))))
  (testing "eventually rejects a non-function predicate"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"predicate"
                          (conc/eventually "not-a-fn")))))

;; ---------------------------------------------------------------------------
;; 5 + 6. contract & regression — machine-checked spec agreement
;; ---------------------------------------------------------------------------

(defn- repo-root
  "Walk up from user.dir until a directory containing deps.edn."
  []
  (loop [^java.nio.file.Path dir (Paths/get (System/getProperty "user.dir")
                                            (make-array String 0))]
    (cond
      (Files/exists (.resolve dir "deps.edn") (make-array LinkOption 0)) dir
      (.getParent dir) (recur (.getParent dir))
      :else dir)))

(deftest contract-arglists-docstrings-and-defaults-match-spec
  (testing "public arglists equal the WO signatures"
    (is (= '([fns & {:keys [timeout-ms]}])
           (:arglists (meta #'conc/raced))))
    (is (= '([thunks & {:keys [timeout-ms]}])
           (:arglists (meta #'conc/barrier-call))))
    (is (= '([pred & {:keys [timeout-ms interval-ms]}])
           (:arglists (meta #'conc/eventually))))
    (is (= '([timeout-ms & body])
           (:arglists (meta #'conc/with-thread-dump-on-timeout)))))
  (testing "docstrings name the synchronization mechanism and the failure marking"
    (let [doc #(or (:doc (meta %)) "")]
      (is (re-find #"CountDownLatch" (doc #'conc/raced))
          "raced documents the CountDownLatch start gate")
      (is (re-find #":timeout" (doc #'conc/raced))
          "raced documents the :timeout marking")
      (is (re-find #"CyclicBarrier" (doc #'conc/barrier-call))
          "barrier-call documents the CyclicBarrier rendezvous")
      (is (re-find #"50" (doc #'conc/eventually))
          "eventually documents its default interval")
      (is (re-find #"10" (doc #'conc/eventually))
          "eventually documents its default timeout")
      (is (re-find #"must not block indefinitely" (doc #'conc/eventually))
          "eventually states the pred-must-not-block contract prominently")
      (is (re-find #"external watchdog" (doc #'conc/eventually))
          "eventually states interval sleeps are outside any watchdog")))
  (testing "default constants equal the documented 10 s / 50 ms"
    (is (= 10000 conc/default-timeout-ms))
    (is (= 50 conc/default-interval-ms)))
  (testing "regression seam: the existing adversarial suite does not reference
            this kit — it must keep passing standalone"
    (let [adversarial-src (slurp (.toFile (.resolve
                                           (repo-root)
                                           "test/evoclj/adversarial/concurrency_test.clj")))]
      (is (not (re-find #"evoclj\.support\.concurrency" adversarial-src))
          "no coupling: adversarial/concurrency_test predates and ignores this kit"))))
