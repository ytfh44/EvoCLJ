(ns evoclj.sci.limits-test
  "Tests for SCI execution limits and interruption (component).

  execute-program runs an evolvable program source inside the closed
  SCI context (component) under deterministic resource limits and
  returns {:status :ok :value <materialized EDN> :usage {:steps n
  :wall-ms ms}} or {:status :error :error <serializable error data>
  :usage {...}}.

  Limits are enforced with SCI's :interrupt-fn ONLY — a zero-arg host
  fn that SCI invokes on every interpreted fn/loop entry. The fn
  checks both the wall-clock deadline (System/nanoTime) and the step
  budget, and throws sci.interrupt/interrupt! (the typed, sandbox-
  uncatchable interrupt), so an intentionally infinite loop/recur
  fixture yields :sci/limit-exceeded and the test process never hangs
  and never needs a watchdog thread (Steps 1, 3). Excessive output is
  enforced by materializing the result through
  evoclj.sci.boundary/materialize-edn under the :max-output-nodes
  size cap, so it is a typed :edn/size-exceeded error, not a hang
  (Step 2). Internal exceptions are converted to stable serializable
  error data at the boundary (Step 4). Step 5 repeats the infinite-loop
  execution 100 times and asserts no worker threads are leaked — this
  interrupt-fn-only design creates zero threads."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.sci.context :as context]
            [evoclj.sci.execute :as execute]))

;; --- fixtures --------------------------------------------------------------

(defn- make-runtime
  "A Phenotype-style sci-runtime: a closed context plus a program
  registry keyed by :program/id (component wires compiled descriptors
  into such a registry)."
  [programs]
  {:context (context/make-context {})
   :programs programs})

(defn- program
  "A runtime program registry entry."
  [id source entry]
  {:program/id id :source source :entry entry})

(def infinite-loop-program
  "An intentionally infinite loop/recur fixture: run enters an
  unconditional loop that never terminates (component 1)."
  (program :fixture/loop
           "(ns fixture.loop)\n(defn run [x]\n  (loop [] (recur)))"
           'fixture.loop/run))

(def top-level-loop-program
  "A fixture whose SOURCE evaluation itself loops forever, so the
  interrupt must fire during the eval phase, before any entry call."
  (program :fixture/top-loop
           "(ns fixture.top-loop)\n(loop [] (recur))"
           'fixture.top-loop/run))

(def huge-output-program
  "A fixture returning more output nodes than :max-output-nodes."
  (program :fixture/huge
           "(ns fixture.huge)\n(defn run [x] (vec (range 2000)))"
           'fixture.huge/run))

(def lazy-output-program
  "A fixture returning an infinite lazy sequence, which must be realized
  under the output cap rather than escaping or hanging (component)."
  (program :fixture/lazy
           "(ns fixture.lazy)\n(defn run [x] (range))"
           'fixture.lazy/run))

(def typed-error-program
  "A fixture that raises its own typed error via ex-info."
  (program :fixture/typed
           "(ns fixture.typed)\n(defn run [x] (throw (ex-info \"boom\" {:error/type :program/failed})))"
           'fixture.typed/run))

(def plain-error-program
  "A fixture that throws a plain Java exception."
  (program :fixture/plain
           "(ns fixture.plain)\n(defn run [x] (throw (Exception. \"boom\")))"
           'fixture.plain/run))

(def ok-program
  "A pure decision program: (reduce + (range x))."
  (program :fixture/ok
           "(ns fixture.ok)\n(defn run [x] (reduce + (range x)))"
           'fixture.ok/run))

;; --- helpers ---------------------------------------------------------------

(defn- run
  "Execute `program` (a registry entry) with `input` and `limit-opts`
  in a fresh runtime."
  [program input limit-opts]
  (execute/execute-program (make-runtime {(:program/id program) program})
                           program input limit-opts))

(defn- elapsed-ms
  "Wall-clock milliseconds taken by (f)."
  [f]
  (let [t0 (System/nanoTime)]
    (f)
    (long (/ (- (System/nanoTime) t0) 1000000))))

(defn- thread-count
  "The number of live JVM threads."
  []
  (count (Thread/getAllStackTraces)))

;; ============================================================================
;; Step 1 — an intentionally infinite loop/recur fixture
;; ============================================================================

(deftest infinite-loop-is-interrupted-with-typed-limit-error
  (testing "the infinite loop/recur fixture is interrupted, never hung"
    (let [elapsed
          (elapsed-ms
           (fn []
             (let [result (run infinite-loop-program nil
                               {:wall-ms 10000 :max-steps 100000
                                :max-output-nodes 1000})]
               (is (= :error (:status result)))
               (is (= :sci/limit-exceeded (:error/type (:error result))))
               (is (contains? #{:max-steps :wall-ms}
                              (:limit (:error/data (:error result)))))
               (is (some? (:steps (:usage result)))))))]
      (is (< elapsed 5000)
          (str "infinite loop interrupted within a few seconds; took "
               elapsed "ms")))))

(deftest infinite-loop-during-source-evaluation-is-interrupted
  (testing "a top-level infinite loop in the source is interrupted during evaluation"
    (let [elapsed
          (elapsed-ms
           (fn []
             (let [result (run top-level-loop-program nil
                               {:wall-ms 10000 :max-steps 100000})]
               (is (= :error (:status result)))
               (is (= :sci/limit-exceeded (:error/type (:error result)))))))]
      (is (< elapsed 5000)
          (str "eval-phase interruption completed in " elapsed "ms")))))

;; ============================================================================
;; Step 2 — excessive output materialization
;; ============================================================================

(deftest excessive-output-is-a-typed-limit-error
  (testing "output larger than :max-output-nodes is a typed limit error, not a hang"
    (let [elapsed
          (elapsed-ms
           (fn []
             (let [result (run huge-output-program nil
                               {:max-steps 10000 :max-output-nodes 1000})]
               (is (= :error (:status result)))
               (is (= :edn/size-exceeded (:error/type (:error result))))
               (is (= 1000 (:limit (:error/data (:error result))))))))]
      (is (< elapsed 5000)
          (str "excessive output rejected fast; took " elapsed "ms")))))

(deftest infinite-lazy-output-cannot-escape
  (testing "an infinite lazy sequence returned by the program is realized under the cap"
    (let [result (run lazy-output-program nil
                      {:max-steps 10000 :max-output-nodes 1000})]
      (is (= :error (:status result)))
      (is (= :edn/size-exceeded (:error/type (:error result)))))))

;; ============================================================================
;; Step 3 — wall-clock cancellation plus step budget
;; ============================================================================

(deftest wall-clock-deadline-interrupts-execution
  (testing "a tight loop with a 1ms wall-clock budget is stopped by the deadline"
    (let [result (run infinite-loop-program nil
                      {:wall-ms 1 :max-steps 1000000})]
      (is (= :error (:status result)))
      (is (= :sci/limit-exceeded (:error/type (:error result))))
      (is (= :wall-ms (:limit (:error/data (:error result))))))))

;; ============================================================================
;; Step 4 — internal exceptions become stable serializable error data
;; ============================================================================

(deftest limit-error-data-round-trips-through-edn
  (let [result (run infinite-loop-program nil {:wall-ms 10000 :max-steps 1000})]
    (is (= :error (:status result)))
    (is (= :sci/limit-exceeded (:error/type (:error result))))
    (testing "the error map is plain serializable Clojure data (Global Constraint 22)"
      (is (= (:error result) (edn/read-string (pr-str (:error result))))))))

(deftest program-raised-typed-errors-keep-their-type
  (let [result (run typed-error-program nil {:max-steps 1000})]
    (is (= :error (:status result)))
    (is (= :program/failed (:error/type (:error result))))
    (is (= (:error result) (edn/read-string (pr-str (:error result)))))))

(deftest plain-program-errors-become-serializable-execution-errors
  (let [result (run plain-error-program nil {:max-steps 1000})]
    (is (= :error (:status result)))
    (is (= :sci/execution-error (:error/type (:error result))))
    (is (string? (:error/message (:error result))))
    (is (= (:error result) (edn/read-string (pr-str (:error result)))))))

(deftest non-edn-safe-input-is-rejected-at-the-boundary
  (let [result (run ok-program (fn [x] x) {:max-steps 100})]
    (is (= :error (:status result)))
    (is (= :program/input-invalid (:error/type (:error result))))))

;; ============================================================================
;; success path, usage, and runtime shapes
;; ============================================================================

(deftest successful-execution-returns-materialized-value-and-usage
  (let [result (run ok-program 100
                    {:wall-ms 1000 :max-steps 10000 :max-output-nodes 1000})]
    (is (= :ok (:status result)))
    (is (= 4950 (:value result)))
    (is (pos? (:steps (:usage result))))
    (is (nat-int? (:wall-ms (:usage result))))))

(deftest nil-limits-fall-back-to-defaults
  (let [result (run ok-program 10 nil)]
    (is (= :ok (:status result)))
    (is (= 45 (:value result)))))

(deftest standalone-source-mode-executes-with-a-bare-context
  (testing "a bare SCI context plus a descriptor carrying :source/:entry works standalone"
    (let [ctx (context/make-context {})
          descriptor {:program/id :standalone
                      :source "(ns s.run)\n(defn run [x] (inc x))"
                      :entry 's.run/run}
          result (execute/execute-program ctx descriptor 41 {:max-steps 100})]
      (is (= :ok (:status result)))
      (is (= 42 (:value result))))))

(deftest missing-program-is-a-typed-error
  (let [runtime (make-runtime {})
        result (execute/execute-program runtime {:program/id :nope} nil
                                        {:max-steps 100})]
    (is (= :error (:status result)))
    (is (= :program/invalid (:error/type (:error result))))))

(deftest malformed-limits-are-rejected-with-typed-errors
  (testing "unknown limit keys are rejected"
    (let [result (run ok-program 1 {:bogus 1})]
      (is (= :error (:status result)))
      (is (= :sci/limits-invalid (:error/type (:error result))))))
  (testing "negative limits are rejected"
    (let [result (run ok-program 1 {:max-steps -1})]
      (is (= :error (:status result)))
      (is (= :sci/limits-invalid (:error/type (:error result))))))
  (testing "non-integer limits are rejected"
    (let [result (run ok-program 1 {:wall-ms "fast"})]
      (is (= :error (:status result)))
      (is (= :sci/limits-invalid (:error/type (:error result)))))))

;; ============================================================================
;; Step 5 — no leaked worker threads across 100 interrupted runs
;; ============================================================================

(deftest no-worker-threads-are-leaked-across-100-runs
  (testing "the interrupt-fn-only design creates no threads: 100 infinite-loop
            executions leave the JVM thread count unchanged (small slack
            allowed for JVM-internal lazy threads)"
    (let [runtime (make-runtime {(:program/id infinite-loop-program)
                                 infinite-loop-program})
          ;; warm up once so JIT/compiler threads have settled
          _ (execute/execute-program runtime infinite-loop-program nil
                                     {:wall-ms 10000 :max-steps 1000})
          before (thread-count)
          elapsed
          (elapsed-ms
           (fn []
             (dotimes [_ 100]
               (let [result (execute/execute-program runtime infinite-loop-program nil
                                                     {:wall-ms 10000 :max-steps 1000})]
                 (is (= :error (:status result)))
                 (is (= :sci/limit-exceeded (:error/type (:error result))))))))
          after (thread-count)]
      (is (< elapsed 30000)
          (str "100 interrupted runs completed in " elapsed "ms"))
      (is (<= after (+ before 3))
          (str "no leaked worker threads: before " before ", after " after)))))
