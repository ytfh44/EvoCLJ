(ns evoclj.runtime.loop-test
  "component tests for explicit bounded loop semantics.

  A :loop node {:node/type :loop :body :node/body :until :program/done?
  :max-iterations 8 :next :node/finish} iterates its :body until the
  :until program (invoked inside the phenotype's isolated SCI runtime)
  returns a boolean done? flag, hard-capped at :max-iterations:

  - the handler reads the CURRENT iteration count from runtime-state's
    :loop-state map (keyed by the loop node id) — the counter travels
    in the scheduler's per-session runtime-state, NEVER in a SCI
    global var (Global Constraint 23);
  - when the predicate is satisfied the loop exits to :next;
  - when the counter reaches :max-iterations the handler returns a
    :failed transition typed :loop/max-iterations-exceeded, which the
    component scheduler routes to the :budget-exhausted session state
    (the typed budget outcome chosen here and documented in
    evoclj.runtime.scheduler);
  - the loop forwards its input payload as the step output only while
    the session's accumulated :outputs are still empty (the entry-node
    case, where the iteration accumulator must reach the body); once
    any output has accumulated the accumulator already travels as the
    last accumulated output, so the loop contributes nothing.

  The four normative scenarios, in the task's numbered order:

  - Step 1: a loop whose :until program is satisfied at iteration 3
    terminates normally with correct accumulation — the body (a :sci
    node echoing the payload's :text with one more \"!\") runs exactly
    three times and the accumulated outputs hold the three provider
    results in order.
  - Step 2: a loop whose predicate never succeeds terminates at
    :max-iterations with the typed budget outcome (:budget-exhausted
    session state and a :session/budget-exhausted event recording the
    {:max-iterations N} limit).
  - Step 3: the compiler still rejects ordinary :next graph cycles
    outside explicit :loop nodes, the normative :loop graph passes
    topology validation, and a malformed :loop node is rejected at
    compile time.
  - Step 4: loop state is session-local data — two sessions on ONE
    executor (ONE shared phenotype / SCI runtime) each run exactly
    three iterations and never see each other's counters, and no SCI
    global var holds the counter (the runtime's :programs registry
    entries still carry only {:source :entry}; the handler-level tests
    prove the counter is read from runtime-state's :loop-state).

  FIXTURE DESIGN: like the component scheduler tests, the CompiledGenome
  is constructed directly (pure data, topology validated through
  evoclj.compiler.topology/compile-topology) instead of through
  evoclj.compiler.core, because the scheduler reads only the pinned
  identity and the compiled :topology from the phenotype. The fixture
  graph is loop -> step -> loop (body :next back to the :loop — the
  sanctioned iteration edge) -> finish."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.compiler.topology :as topology]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.node :as node]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.sci.context :as context]
            [evoclj.sci.execute :as execute]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixture identity ------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private generation-id "generation-1")

;; --- temp stores ------------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-loop-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-loop-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifacts)."
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file, seeded with
  the generation row sessions are pinned to."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at "2025-01-01T00:00:00Z"}))
    db))

;; --- fixture topology and programs ------------------------------------------

(defn- loop-topology
  "The component fixture graph: :loop -> :step (body, :next back to the
  :loop) -> :finish. The :body edge is the sanctioned iteration edge;
  the body's :next back to the :loop is the only edge that closes the
  iteration, and it passes through the explicit :loop node."
  [{:keys [until max-iterations]}]
  {:graph/id :graph/loop
   :entry :node/loop
   :nodes
   {:node/loop {:node/type :loop
                :body :node/step
                :until until
                :max-iterations max-iterations
                :next :node/finish}
    :node/step {:node/type :sci
                :program :program/step
                :next :node/loop}
    :node/finish {:node/type :emit}}
   :limits {:max-steps 64}})

(defn- program-sources
  "The sources for every fixture program. :program/done? is satisfied at
  iteration 3; :program/early-done? at iteration 2 (the handler-level
  exit test); :program/never never is; :program/step appends one \"!\"
  to the payload's :text and asks :fixture/echo to echo it (so the
  accumulator visibly grows each iteration); :program/weird returns a
  non-boolean (a malformed until result)."
  []
  {:program/done? (str "(ns agent.done)\n"
                       "(defn done? [{:keys [iteration payload]}] (>= iteration 3))")
   :program/early-done? (str "(ns agent.early)\n"
                             "(defn done? [{:keys [iteration payload]}] (>= iteration 2))")
   :program/never (str "(ns agent.never)\n(defn done? [_] false)")
   :program/step (str "(ns agent.step)\n"
                      "(defn run [{:keys [text]}]"
                      "  {:action {:intent/type :intent/tool-call"
                      "           :payload {:tool/id :fixture/echo"
                      "                     :args {:text (str text \"!\")}}}})")
   :program/weird (str "(ns agent.weird)\n(defn done? [_] :not-a-boolean)")})

(defn- compiled-genome
  "A minimal CompiledGenome value carrying a loop fixture topology —
  constructed directly (see the namespace docstring)."
  [fixture-topology]
  {:compiled/genome-id genome-id
   :compiled/resolution-id resolution-id
   :compiled/phenotype-id phenotype-id
   :abi {}
   :manifest {}
   :topology (topology/compile-topology fixture-topology)
   :programs (into (sorted-map)
                   {:program/done? {:program/id :program/done? :entry 'agent.done/done?}
                    :program/early-done? {:program/id :program/early-done?
                                          :entry 'agent.early/done?}
                    :program/never {:program/id :program/never :entry 'agent.never/done?}
                    :program/step {:program/id :program/step :entry 'agent.step/run}
                    :program/weird {:program/id :program/weird :entry 'agent.weird/done?}})})

(defn- echo-lease
  "A valid CapabilityLease granting this phenotype's exact id the
  :fixture/echo :invoke action for the next minute."
  []
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :subject {:phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 100}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- build-executor
  "Build the executor map for a loop fixture topology: a live phenotype
  (every fixture program loaded into its isolated SCI runtime), the
  opened sqlite + cas stores, and a broker context carrying one
  :fixture/echo lease for the phenotype. Returns {:executor <executor
  map> :executions <atom>} where :executions counts real provider
  executions."
  [fixture-topology]
  (let [executions (atom 0)
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider
                                   {:execution-count executions}))
        usage (atom {})
        leases [(echo-lease)]
        compiled (compiled-genome fixture-topology)
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases leases :usage usage}
             :program-sources (program-sources)})]
    {:executor {:phenotype ph
                :stores {:sqlite (fresh-db) :cas (cas/->cas (temp-cas-dir))}
                :dispatch (dispatch/make-broker-context
                           {:registry reg
                            :leases leases
                            :usage usage})}
     :executions executions}))

(defn- create-pinned-session
  "create-session! pinned to the fixture identity, then append the
  :session/created root event (the host's job — the scheduler anchors
  its causal chain on it). Returns the session id."
  [executor]
  (let [db (:sqlite (:stores executor))
        sid (:session/id
             (session/create-session!
              db
              {:genome/id genome-id
               :resolution/id resolution-id
               :phenotype/id phenotype-id
               :generation/id generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id generation-id
                          :phenotype/id phenotype-id
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- artifact-edn
  "Read a CAS artifact back as EDN data."
  [executor artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas (:stores executor)) artifact-id)
            StandardCharsets/UTF_8)))

(defn- echo-results
  "The provider results accumulated in an outputs vector, in order
  (every :fixture/echo result is a map carrying a :text string)."
  [outputs]
  (->> outputs
       (filter #(and (map? %) (string? (:text %))))
       (mapv :text)))

;; --- handler unit fixtures --------------------------------------------------

(def ^:private unit-loop-node
  "A compiled :loop node for handler-level tests."
  {:node/id :node/loop
   :node/type :loop
   :body :node/step
   :until :program/early-done?
   :max-iterations 3
   :next :node/finish})

(defn- until-runtime
  "A runtime map with :program/early-done? loaded (satisfied at
  iteration 2, so the handler-level exit path is reachable under
  :max-iterations 3)."
  []
  (execute/load-program! {:context (context/make-context {}) :programs {}}
                         {:program/id :program/early-done? :entry 'agent.early/done?}
                         (:program/early-done? (program-sources))))

;; ============================================================================
;; Step 1 — a loop whose predicate is satisfied at iteration 3 terminates
;; ============================================================================

(deftest step-1-loop-terminates-after-three-iterations-with-correct-accumulation
  (let [{:keys [executor executions]} (build-executor
                                        (loop-topology {:until :program/done?
                                                        :max-iterations 8}))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:text "seed"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        by-type (group-by :event/type events)
        loop-starts (fn [] (count (filter #(= :node/loop
                                              (get-in % [:metadata :node/id]))
                                          (:node/started by-type))))]
    (testing "the loop terminates normally after three body iterations"
      (is (= :completed (:status result)))
      (is (= :completed (:state (session/get-session
                                 (:sqlite (:stores executor)) sid)))))
    (testing "the body ran exactly three times — one per iteration"
      (is (= 3 @executions))
      (is (= 3 (count (:provider/call-completed by-type)))))
    (testing "the :loop node was visited four times: three iteration passes plus one exit pass"
      (is (= 4 (loop-starts))))
    (testing "the accumulator threaded through the iterations (correct accumulation)"
      (is (= ["seed" "seed!" "seed!!" "seed!!!"]
             (echo-results (artifact-edn executor (:output-ref result))))))))

;; ============================================================================
;; Step 2 — a never-satisfied predicate terminates at :max-iterations
;; ============================================================================

(deftest step-2-never-satisfied-predicate-halts-at-max-iterations
  (let [{:keys [executor executions]} (build-executor
                                        (loop-topology {:until :program/never
                                                        :max-iterations 3}))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:text "seed"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        budget-event (last events)
        by-type (group-by :event/type events)
        loop-starts (fn [] (count (filter #(= :node/loop
                                              (get-in % [:metadata :node/id]))
                                          (:node/started by-type))))]
    (testing "the run terminates at :max-iterations with the typed budget outcome"
      (is (= :budget-exhausted (:status result)))
      (is (= :budget-exhausted
             (:state (session/get-session (:sqlite (:stores executor)) sid)))))
    (testing "the body ran exactly :max-iterations times, then the cap fired"
      (is (= 3 @executions))
      (is (= 3 (count (:provider/call-completed by-type))))
      (is (= 4 (loop-starts))))
    (testing "the budget event records the loop cap and preserves the outputs as evidence"
      (is (= :session/budget-exhausted (:event/type budget-event)))
      (is (= {:max-iterations 3} (get-in budget-event [:metadata :limits])))
      (is (= (:output-ref result) (:payload-ref budget-event)))
      (is (= ["seed" "seed!" "seed!!" "seed!!!"]
             (echo-results (artifact-edn executor (:output-ref result))))))
    (testing "no provider call happened after the cap fired"
      (is (= 3 @executions)))))

;; ============================================================================
;; Step 3 — the compiler still rejects ordinary cycles outside :loop nodes
;; ============================================================================

(deftest step-3-compiler-rejects-ordinary-cycles-outside-loop-nodes
  (testing "a raw :next cycle with no :loop node is still rejected"
    (let [e (try (topology/compile-topology
                  {:graph/id :graph/main
                   :entry :node/a
                   :nodes {:node/a {:node/type :sci :program :program/route
                                    :next :node/b}
                           :node/b {:node/type :sci :program :program/route
                                    :next :node/a}}})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :topology/cycle (:error/type (ex-data e))))
      (is (= [:node/a :node/b] (:nodes (ex-data e))))))
  (testing "the normative :loop graph — body :next back to the :loop — passes topology validation"
    (let [c (topology/compile-topology
             (loop-topology {:until :program/done? :max-iterations 8}))]
      (is (= :node/loop (:entry c)))
      (is (= [:node/finish] (get-in c [:adjacency :node/loop])))
      (is (= [:node/loop] (get-in c [:adjacency :node/step])))
      (is (= c (edn/read-string (pr-str c))))))
  (testing "a malformed :loop node is rejected at compile time"
    (doseq [pair [[{:graph/id :graph/main
                    :entry :node/loop
                    :nodes {:node/loop {:node/type :loop
                                        :until :program/done?
                                        :max-iterations 8
                                        :next :node/finish}
                            :node/finish {:node/type :emit}}}
                   :missing-required-key]
                  [{:graph/id :graph/main
                    :entry :node/loop
                    :nodes {:node/loop {:node/type :loop
                                        :body :node/step
                                        :until :program/done?
                                        :max-iterations 0
                                        :next :node/finish}
                            :node/finish {:node/type :emit}}}
                   :invalid-max-iterations]
                  [{:graph/id :graph/main
                    :entry :node/loop
                    :nodes {:node/loop {:node/type :loop
                                        :body :node/ghost
                                        :until :program/done?
                                        :max-iterations 8
                                        :next :node/finish}
                            :node/finish {:node/type :emit}}}
                   :dangling-body]]]
      (let [[bad reason] pair
            e (try (topology/compile-topology bad)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (instance? clojure.lang.ExceptionInfo e) (pr-str bad))
        (is (= :topology/invalid (:error/type (ex-data e))) (pr-str bad))
        (is (= reason (:reason (ex-data e))) (pr-str bad))))))

;; ============================================================================
;; Step 4 — loop state is session-local data, never a SCI global var
;; ============================================================================

(deftest step-4-loop-state-is-session-local-not-a-sci-global
  (let [{:keys [executor executions]} (build-executor
                                        (loop-topology {:until :program/done?
                                                        :max-iterations 8}))
        db (:sqlite (:stores executor))
        sid1 (create-pinned-session executor)
        result1 (scheduler/run-session! executor sid1 {:text "one"})
        sid2 (create-pinned-session executor)
        result2 (scheduler/run-session! executor sid2 {:text "two"})
        provider-events (fn [sid]
                          (count (filter #(= :provider/call-completed (:event/type %))
                                         (event/events-for-session db sid))))]
    (testing "both sessions terminate normally — the second session ran its own three iterations"
      (is (= :completed (:status result1)))
      (is (= :completed (:status result2)))
      (is (= 3 (provider-events sid1)))
      (is (= 3 (provider-events sid2)))
      (is (= 6 @executions)))
    (testing "the outputs of the two sessions never interfered"
      (is (= ["one" "one!" "one!!" "one!!!"]
             (echo-results (artifact-edn executor (:output-ref result1)))))
      (is (= ["two" "two!" "two!!" "two!!!"]
             (echo-results (artifact-edn executor (:output-ref result2))))))
    (testing "no SCI global var holds the loop counter"
      (let [runtime (:sci-runtime (:phenotype executor))
            registry (:programs runtime)]
        (is (not (contains? runtime :loop-state)))
        (is (every? (fn [entry] (= #{:source :entry} (set (keys entry))))
                    (vals registry)))
        (is (= #{:program/done? :program/early-done? :program/never
                 :program/step :program/weird}
               (set (keys registry))))))))

;; ============================================================================
;; handler level — the loop counter travels in runtime-state's :loop-state
;; ============================================================================

(deftest loop-handler-reads-its-counter-from-runtime-state
  (let [handler ((node/handler-for :loop))
        runtime (until-runtime)
        rs (fn [overrides]
             (merge {:session/id (random-uuid)
                     :phenotype/id (str "sha256:" hex64)
                     :node/id :node/loop
                     :outputs []
                     :sci-runtime runtime}
                    overrides))
        ev (fn [payload] {:event/id 7 :event/type :node/started :payload payload})]
    (testing "iteration 0: the predicate is not satisfied yet -> the body runs"
      (let [t (node/step handler (rs {}) unit-loop-node (ev {:text "seed"}))]
        (is (= :continue (:transition/status t)))
        (is (= [:node/step] (:next t)))
        (is (= [] (:intents t)))
        (is (= [{:text "seed"}] (:outputs t)) "the entry payload must reach the body")))
    (testing "iteration 2: the predicate is satisfied -> the loop exits to :next"
      (let [t (node/step handler (rs {:loop-state {:node/loop 2}})
                         unit-loop-node (ev {:text "seed!"}))]
        (is (= :continue (:transition/status t)))
        (is (= [:node/finish] (:next t)))
        (is (= [{:text "seed!"}] (:outputs t)))))
    (testing "iteration 3 hits the :max-iterations cap -> the typed loop-exhaust outcome"
      (let [t (node/step handler (rs {:loop-state {:node/loop 3}})
                         unit-loop-node (ev {:text "seed!!"}))]
        (is (= :failed (:transition/status t)))
        (is (= :loop/max-iterations-exceeded (:error/type (:error t))))
        (is (= 3 (:max-iterations (:error t))))))
    (testing "a non-boolean until result is a malformed loop predicate"
      (let [weird-runtime (execute/load-program!
                           {:context (context/make-context {}) :programs {}}
                           {:program/id :program/weird :entry 'agent.weird/done?}
                           (:program/weird (program-sources)))
            t (node/step handler (rs {:sci-runtime weird-runtime})
                         (assoc unit-loop-node :until :program/weird)
                         (ev {:text "seed"}))]
        (is (= :failed (:transition/status t)))
        (is (= :node/invalid (:error/type (:error t))))
        (is (= :invalid-until-result (get-in (:error t) [:error/data :reason])))))
    (testing "a :loop node without :sci-runtime in runtime-state fails closed"
      (let [e (try (node/step handler (dissoc (rs {}) :sci-runtime)
                              unit-loop-node (ev {:text "seed"}))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= :node/runtime-invalid (:error/type (ex-data e))))
        (is (= :sci-runtime-missing (:reason (ex-data e))))))))
