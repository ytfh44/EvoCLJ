(ns evoclj.eval.replay-test
  "G4 historical replay with representative cases (Task 8.3).

  run-replay! re-executes a candidate against replay cases built from
  stored Episodes (evoclj.eval.replay). Each replay case carries a
  recorded intent trace with FIXTURABLE external providers: replay
  NEVER repeats real external writes. The replay-mode-aware provider
  wrapper (evoclj.eval.replay/replay-provider, built on
  evoclj.provider.protocol) serves recorded responses for read-type
  calls and denies write-type intents per the case mode:

    :fixture       every intent (reads AND writes) is served its
                   recorded response as a pure fixture computation —
                   nothing real happens (the wrapped fixture
                   provider's execute-request! never runs).
    :recorded-read reads are served recorded responses; write-type
                   intents are DENIED (fail-closed; replays never
                   repeat real writes). The denial is recorded in the
                   run's intent evidence but is not itself a
                   regression — the case is judged on output.
    :forbid-write  reads are served recorded responses; any write-type
                   intent is denied AND counted as a regression.

  The harness is the documented lighter-harness choice: instead of the
  scheduler's full persistence stack (SQLite migration, session rows,
  CAS, causal event log — evidence for LIVE runs, redundant when the
  recorded run is already the persisted evidence), the candidate's
  compiled topology is re-walked with the SAME node handlers
  (evoclj.runtime.node/step) and the SAME broker
  (evoclj.intent.dispatch/dispatch!) — a fresh session id and a fresh
  isolated SCI runtime per case (Global Constraint 23).

  Tests map to the task steps: Step 1 case construction from Episodes,
  Step 2 provider replay modes, Step 3 fix-one/break-one surfaced in
  one report, Step 4 output equivalence predicates, Step 5 :critical
  cases and :hard-failure?."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.eval.replay :as replay]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- helpers ---------------------------------------------------------------

(defn- write-file!
  "Write `content` as UTF-8 to `path`, creating parent directories."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(defn- route-source
  "A route program for the replay test candidates: {:op :echo :text t}
  emits a :fixture/echo (or `tool-id`) tool-call with (transform t);
  {:op :write :text t} emits a :fixture/write tool-call; anything else
  emits a finish intent. `transform-expr` is the body of the private
  transform fn; `tool-id` is the echoed tool."
  [tool-id transform-expr]
  (str "(ns agent.route)\n"
       "(defn- transform [text] " transform-expr ")\n"
       "(defn run [input]\n"
       "  (let [op (get input :op)]\n"
       "    (case op\n"
       "      :echo {:action {:intent/type :intent/tool-call\n"
       "                      :payload {:tool/id " (pr-str tool-id) "\n"
       "                                :args {:text (transform (get input :text))}}}}\n"
       "      :write {:action {:intent/type :intent/tool-call\n"
       "                       :payload {:tool/id :fixture/write\n"
       "                                 :args {:text (get input :text)}}}}\n"
       "      {:action {:intent/type :intent/finish :payload {:value input}}})))\n"))

(defn- bundle!
  "Build a candidate genome bundle in a fresh temp dir and return its
  path string. The topology is :sci router → :emit; the router runs
  the route program built from `transform-expr` (default: identity)."
  ([transform-expr] (bundle! :fixture/echo transform-expr))
  ([tool-id transform-expr]
   (let [dir (str (Files/createTempDirectory "replay-candidate-"
                                             (make-array FileAttribute 0)))]
     (write-file! (str dir "/manifest.edn")
                  (pr-str {:genome/format 1
                           :agent/id :main
                           :agent/entry :graph/main
                           :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                           :modules {:topology "topology.edn"
                                     :models "models.edn"
                                     :memory "memory.edn"
                                     :evolution "evolution.edn"}
                           :capabilities/requested #{:model/call}
                           :evolution {:max-risk :behavioral
                                       :mutable #{:parameters :prompts
                                                  :skills :programs}}
                           :metadata {:name "replay-fixture"
                                      :description "replay test candidate"}}))
     (write-file! (str dir "/topology.edn")
                  (pr-str {:graph/id :graph/replay
                           :entry :node/router
                           :nodes {:node/router {:node/type :sci
                                                 :program :program/route
                                                 :next :node/emit}
                                   :node/emit {:node/type :emit}}
                           :limits {:max-steps 64}}))
     (write-file! (str dir "/models.edn")
                  "{:models {:planner {:alias :reasoning/high}}}")
     (write-file! (str dir "/memory.edn") "{:memory {}}")
     (write-file! (str dir "/evolution.edn") "{:evolution {}}")
     (write-file! (str dir "/programs/route.clj") (route-source tool-id transform-expr))
     dir)))

(defn- provider-catalog
  "The on-disk provider catalog fixture (Task 2.1 resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor
  "The in-memory route program descriptor (Task 2.3 choice (a))."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- write-provider
  "A WRITE-type fixture provider (:effect :write) standing in for an
  external write adapter; :execution-count counts real executions."
  [{:keys [execution-count]}]
  (let [count (or execution-count (atom 0))]
    (reify proto/Provider
      (describe [_]
        {:tool/id :fixture/write
         :effect :write
         :input-schema [:map [:text :string]]
         :output-schema [:map [:text :string]]
         :required-action :invoke})
      (normalize-request [_ intent]
        {:tool/id :fixture/write
         :resource {:kind :tool :id :fixture/write}
         :args (get-in intent [:payload :args])})
      (execute-request! [_ authorized-request]
        (swap! count inc)
        {:text (get-in authorized-request [:args :text])}))))

(defn- episode
  "A stored-Episode-shaped map (Task 6.5 Episode contract) for replay
  case construction, with the given terminal :outcome :status."
  [status]
  {:episode/id (random-uuid)
   :session/id (random-uuid)
   :generation/id "g-replay-1"
   :genome/id (str "sha256:" (apply str (repeat 64 "0")))
   :resolution/id (str "sha256:" (apply str (repeat 64 "1")))
   :task-ref (str "sha256:" (apply str (repeat 64 "2")))
   :trace {:first-event 1 :last-event 9}
   :outcome {:status status :score nil}
   :usage {}})

(defn- tool-trace-entry
  "One recorded trace entry: a tool-call intent, its tool's recorded
  effect class (:read | :write), and the recorded provider response."
  [tool-id args effect response]
  {:intent/type :intent/tool-call
   :payload {:tool/id tool-id :args args}
   :effect effect
   :response response})

(defn- echo-decision
  "The route program's decision value for an echo tool-call."
  [text]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/echo :args {:text text}}}})

(defn- write-decision
  "The route program's decision value for a write tool-call."
  [text]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/write :args {:text text}}}})

(defn- evaluator
  "A minimal valid replay evaluator context: the provider catalog, the
  in-memory route program registry, a replay case registry, and the
  fixtureable provider catalog."
  ([cases] (evaluator cases {}))
  ([cases overrides]
   (merge {:provider/catalog (provider-catalog)
           :programs (fn [_] [(route-descriptor)])
           :replay/cases cases
           :replay/fixtures {:fixture/echo (fixture/echo-provider)
                             :fixture/write (write-provider {})}}
          overrides)))

(defn- ignore-case-equiv
  "Equivalence predicate for Step 4: the last output's :text matches
  ignoring case."
  [expected actual]
  (= (get-in expected [1 :text])
     (str/upper-case (get-in actual [1 :text]))))

;; --- Step 1: replay cases built from stored Episodes -----------------------

(deftest build-replay-case-assembles-and-validates
  (let [ep (episode :completed)
        tr [(tool-trace-entry :fixture/echo {:text "hi"} :read {:text "hi"})]
        c (replay/build-replay-case
           ep tr
           {:case/id :replay/c1
            :task-input {:op :echo :text "hi"}
            :expected-output [(echo-decision "hi") {:text "hi"}]
            :mode :fixture
            :critical? true})]
    (testing "the case carries episode provenance, oracle, mode, and trace"
      (is (= :replay/c1 (:case/id c)))
      (is (= (:episode/id ep) (:episode/id c)))
      (is (= :completed (:recorded/status c)))
      (is (= {:op :echo :text "hi"} (:task-input c)))
      (is (= :fixture (:mode c)))
      (is (true? (:critical? c)))
      (is (nil? (:output/equiv? c)) "no equiv declared -> default oracle")
      (is (= {:text "hi"} (get-in c [:responses :fixture/echo {:text "hi"}]))
          "the trace maps canonical args to the recorded response"))))

(deftest build-replay-case-rejects-malformed-input
  (testing "a malformed episode is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"episode"
          (replay/build-replay-case
           {} []
           {:case/id :replay/x :task-input {} :expected-output []
            :mode :fixture}))))
  (testing "a non-tool-call trace entry is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"trace"
          (replay/build-replay-case
           (episode :completed)
           [{:intent/type :intent/finish
             :payload {:value 1} :effect :read :response 1}]
           {:case/id :replay/x :task-input {} :expected-output []
            :mode :fixture}))))
  (testing "an unknown mode is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"mode"
          (replay/build-replay-case
           (episode :completed) []
           {:case/id :replay/x :task-input {} :expected-output []
            :mode :bogus}))))
  (testing "a missing :case/id is rejected"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"case/id"
          (replay/build-replay-case
           (episode :completed) []
           {:task-input {} :expected-output [] :mode :fixture})))))

;; --- Step 2: provider replay modes -----------------------------------------

(deftest fixture-mode-wrapper-serves-recorded-responses
  (let [executions (atom 0)
        wp (write-provider {:execution-count executions})
        responses (get (replay/responses-table
                        [(tool-trace-entry :fixture/write {:text "x"} :write {:text "x"})])
                       :fixture/write)
        wrapped (replay/replay-provider wp :fixture responses)
        normalized (proto/normalize-request
                    wrapped {:payload {:tool/id :fixture/write :args {:text "x"}}})]
    (is (= :pure (:effect (proto/describe wrapped)))
        "a replayed write is served as a pure fixture computation")
    (is (= {:text "x"} (proto/execute-request! wrapped normalized))
        "the recorded response is served, not recomputed")
    (is (zero? @executions)
        "the wrapped fixture provider never really executes in replay")))

(deftest deny-modes-reject-write-intents
  (let [responses (get (replay/responses-table
                        [(tool-trace-entry :fixture/write {:text "x"} :write {:text "x"})])
                       :fixture/write)]
    (doseq [mode [:recorded-read :forbid-write]]
      (testing (pr-str mode)
        (let [wrapped (replay/replay-provider (write-provider {}) mode responses)
              normalized (proto/normalize-request
                          wrapped {:payload {:tool/id :fixture/write
                                             :args {:text "x"}}})]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"write"
                (proto/execute-request! wrapped normalized))))))))

(deftest unrecorded-call-rejects-with-response-missing
  (let [wrapped (replay/replay-provider (fixture/echo-provider) :fixture {})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"recorded response"
          (proto/execute-request! wrapped
                                  {:tool/id :fixture/echo
                                   :resource {:kind :tool :id :fixture/echo}
                                   :args {:text "nope"}})))))

(deftest fixture-mode-replays-a-write-without-repeating-it
  (let [executions (atom 0)
        cases {:replay/write
               (replay/build-replay-case
                (episode :completed)
                [(tool-trace-entry :fixture/write {:text "x"} :write {:text "x"})]
                {:case/id :replay/write
                 :task-input {:op :write :text "x"}
                 :expected-output [(write-decision "x") {:text "x"}]
                 :mode :fixture})}
        ev (evaluator cases {:replay/fixtures
                             {:fixture/write (write-provider
                                              {:execution-count executions})}})
        report (replay/run-replay! ev (bundle! "text") [:replay/write])
        outcome (first (:replay/cases report))]
    (is (zero? @executions) "no real external write was repeated")
    (is (= :pass (:status outcome)))
    (is (true? (:output/match? outcome)))
    (is (empty? (:provider/regressions outcome)))))

(deftest recorded-read-serves-reads-and-denies-writes-as-evidence
  (testing "a read-only case passes under :recorded-read"
    (let [cases {:replay/read
                 (replay/build-replay-case
                  (episode :completed)
                  [(tool-trace-entry :fixture/echo {:text "hi"} :read {:text "hi"})]
                  {:case/id :replay/read
                   :task-input {:op :echo :text "hi"}
                   :expected-output [(echo-decision "hi") {:text "hi"}]
                   :mode :recorded-read})}
          report (replay/run-replay! (evaluator cases) (bundle! "text")
                                     [:replay/read])]
      (is (= :pass (:status (first (:replay/cases report)))))))
  (testing "a write attempt is denied and recorded as evidence, judged on output"
    (let [cases {:replay/write
                 (replay/build-replay-case
                  (episode :completed)
                  [(tool-trace-entry :fixture/write {:text "x"} :write {:text "x"})]
                  {:case/id :replay/write
                   :task-input {:op :write :text "x"}
                   :expected-output [(write-decision "x")]
                   :mode :recorded-read})}
          report (replay/run-replay! (evaluator cases) (bundle! "text")
                                     [:replay/write])
          outcome (first (:replay/cases report))
          intent (first (get-in outcome [:run :intents]))]
      (is (= :pass (:status outcome))
          "output matches; the denial is recorded evidence, not a regression")
      (is (= :error (:result/status intent)))
      (is (= :provider/replay-write-denied (:cause/error/type intent)))
      (is (empty? (:provider/regressions outcome))))))

(deftest forbid-write-counts-write-denials-as-regressions
  (let [cases {:replay/write
               (replay/build-replay-case
                (episode :completed)
                [(tool-trace-entry :fixture/write {:text "x"} :write {:text "x"})]
                {:case/id :replay/write
                 :task-input {:op :write :text "x"}
                 :expected-output [(write-decision "x")]
                 :mode :forbid-write})}
        report (replay/run-replay! (evaluator cases) (bundle! "text")
                                   [:replay/write])
        outcome (first (:replay/cases report))]
    (is (= :fail (:status outcome)))
    (is (= [:provider-regression] (:reasons outcome)))
    (is (= :write-denied (get-in outcome [:provider/regressions 0 :regression/type])))
    (is (true? (:regression? outcome))
        "a previously-successful case that now writes is a regression")))

;; --- Step 3: fix one failure, break one success ----------------------------

(deftest candidate-fixing-a-failure-but-breaking-a-success-surfaces-both
  (let [fix (replay/build-replay-case
             (episode :failed)
             [(tool-trace-entry :fixture/echo {:text "hello"} :read {:text "hello"})]
             {:case/id :replay/fix-a
              :task-input {:op :echo :text "hello"}
              :expected-output [(echo-decision "hello") {:text "hello"}]
              :mode :fixture})
        brk (replay/build-replay-case
             (episode :completed)
             [(tool-trace-entry :fixture/echo {:text "known"} :read {:text "known"})]
             {:case/id :replay/break-b
              :task-input {:op :echo :text "known"}
              :expected-output [(echo-decision "known") {:text "known"}]
              :mode :fixture})
        ;; the candidate fixes "hello" but corrupts "known" into "known!"
        candidate (bundle! "(if (= text \"known\") (str text \"!\") text)")
        report (replay/run-replay!
                (evaluator {:replay/fix-a fix :replay/break-b brk})
                candidate [:replay/fix-a :replay/break-b])
        outcomes (:replay/cases report)
        fix-outcome (first outcomes)
        brk-outcome (second outcomes)]
    (testing "the fixed failure surfaces as a pass, not a regression"
      (is (= :pass (:status fix-outcome)))
      (is (false? (:regression? fix-outcome)))
      (is (true? (:output/match? fix-outcome))))
    (testing "the broken known-success surfaces as a regression"
      (is (= :fail (:status brk-outcome)))
      (is (false? (:output/match? brk-outcome)))
      (is (= :unrecorded-call (get-in brk-outcome [:provider/regressions 0 :regression/type])))
      (is (= [:output-mismatch :provider-regression] (:reasons brk-outcome)))
      (is (true? (:regression? brk-outcome))))
    (testing "the report carries BOTH per-case outcomes and the regression"
      (is (= [:replay/fix-a :replay/break-b] (mapv :case/id outcomes)))
      (is (= 1 (:regressions (:aggregate report))))
      (is (= [:replay/break-b] (mapv :case/id (:regressions report)))))))

;; --- Step 4: output equivalence predicates ---------------------------------

(deftest output-equivalence-predicates-permit-variation
  (let [case-with (fn [equiv]
                    (replay/build-replay-case
                     (episode :completed)
                     [(tool-trace-entry :fixture/echo {:text "hello"} :read {:text "hello"})]
                     {:case/id :replay/equiv
                      :task-input {:op :echo :text "hello"}
                      :expected-output [(echo-decision "hello") {:text "HELLO"}]
                      :mode :fixture
                      :output/equiv? equiv}))]
    (testing "byte-identical output is the default oracle"
      (let [report (replay/run-replay!
                    (evaluator {:replay/equiv (case-with nil)})
                    (bundle! "text") [:replay/equiv])]
        (is (= :fail (:status (first (:replay/cases report)))))
        (is (false? (:output/match? (first (:replay/cases report)))))))
    (testing "a declared equivalence fn permits the variation"
      (let [report (replay/run-replay!
                    (evaluator {:replay/equiv (case-with ignore-case-equiv)})
                    (bundle! "text") [:replay/equiv])]
        (is (= :pass (:status (first (:replay/cases report)))))
        (is (true? (:output/match? (first (:replay/cases report)))))))
    (testing "a keyword-named equivalence resolves through the evaluator"
      (let [report (replay/run-replay!
                    (evaluator {:replay/equiv (case-with :equivalence/ignore-case)}
                               {:equivalence/by-keyword
                                {:equivalence/ignore-case ignore-case-equiv}})
                    (bundle! "text") [:replay/equiv])]
        (is (= :pass (:status (first (:replay/cases report)))))))
    (testing "an unknown equivalence keyword fails closed"
      (let [report (fn []
                     (replay/run-replay!
                      (evaluator {:replay/equiv (case-with :equivalence/nope)})
                      (bundle! "text") [:replay/equiv]))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"equivalence"
              (report)))))))

;; --- Step 5: critical cases are hard failures ------------------------------

(deftest critical-regressions-are-hard-failures
  (let [regress-case (fn [case-id input critical?]
                       (replay/build-replay-case
                        (episode :completed)
                        [(tool-trace-entry :fixture/echo {:text input} :read {:text input})]
                        {:case/id case-id
                         :task-input {:op :echo :text input}
                         :expected-output [(echo-decision input) {:text input}]
                         :mode :fixture
                         :critical? critical?}))
        ;; corrupts "known" and "other", leaves "hello" untouched
        candidate (bundle! "(if (contains? #{\"known\" \"other\"} text) (str text \"!\") text)")]
    (testing "ANY critical regression is a hard failure"
      (let [report (replay/run-replay!
                    (evaluator {:replay/critical (regress-case :replay/critical "other" true)})
                    candidate [:replay/critical])]
        (is (true? (:hard-failure? report)))
        (is (true? (:hard-failure? (first (:replay/cases report)))))
        (is (= 1 (:critical-regressions (:aggregate report))))))
    (testing "a passing critical case does not hard-fail a non-critical regression"
      (let [report (replay/run-replay!
                    (evaluator {:replay/ok (regress-case :replay/ok "hello" true)
                                :replay/plain (regress-case :replay/plain "known" false)})
                    candidate [:replay/ok :replay/plain])]
        (is (= :pass (:status (first (:replay/cases report)))))
        (is (true? (:regression? (second (:replay/cases report)))))
        (is (false? (:hard-failure? report)))
        (is (= 0 (:critical-regressions (:aggregate report)))
            "the passing critical case contributes no critical regression")))))

;; --- harness and report contract -------------------------------------------

(deftest fresh-session-per-case
  (let [mk (fn [case-id input]
             (replay/build-replay-case
              (episode :completed)
              [(tool-trace-entry :fixture/echo {:text input} :read {:text input})]
              {:case/id case-id
               :task-input {:op :echo :text input}
               :expected-output [(echo-decision input) {:text input}]
               :mode :fixture}))
        report (replay/run-replay!
                (evaluator {:replay/a (mk :replay/a "one")
                            :replay/b (mk :replay/b "two")})
                (bundle! "text") [:replay/a :replay/b])
        sessions (mapv (comp :session/id :run) (:replay/cases report))]
    (is (every? uuid? sessions))
    (is (apply distinct? sessions)
        "every replay case runs in a FRESH session (Global Constraint 23)")))

(deftest report-shape-and-aggregates
  (let [cases {:replay/a
               (replay/build-replay-case
                (episode :completed)
                [(tool-trace-entry :fixture/echo {:text "hi"} :read {:text "hi"})]
                {:case/id :replay/a
                 :task-input {:op :echo :text "hi"}
                 :expected-output [(echo-decision "hi") {:text "hi"}]
                 :mode :fixture})}
        report (replay/run-replay! (evaluator cases) (bundle! "text") [:replay/a])
        ag (:aggregate report)]
    (is (= [:replay/a] (:replay/requested report)))
    (is (= 1 (count (:replay/cases report))))
    (is (= {:cases 1 :passed 1 :failed 0 :regressions 0
            :critical 0 :critical-regressions 0 :hard-failure? false}
           ag))
    (is (empty? (:regressions report)))
    (is (false? (:hard-failure? report)))))

(deftest unknown-replay-case-fails-closed
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"case"
        (replay/run-replay! (evaluator {}) (bundle! "text") [:replay/missing]))))

(deftest trace-tool-without-fixture-fails-closed
  (let [case (replay/build-replay-case
              (episode :completed)
              [(tool-trace-entry :fixture/ghost {:text "x"} :read {:text "x"})]
              {:case/id :replay/ghost
               :task-input {:op :echo :text "x"}
               :expected-output []
               :mode :fixture})
        ev (assoc (evaluator {:replay/ghost case}) :replay/fixtures {})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"fixture"
          (replay/run-replay! ev (bundle! "text") [:replay/ghost])))))

(deftest unrecorded-tool-call-counts-as-regression
  (let [case (replay/build-replay-case
              (episode :completed)
              [(tool-trace-entry :fixture/echo {:text "x"} :read {:text "x"})]
              {:case/id :replay/x
               :task-input {:op :echo :text "x"}
               :expected-output [(echo-decision "x") {:text "x"}]
               :mode :fixture})
        ;; the candidate calls :fixture/other, which the trace never saw
        candidate (bundle! :fixture/other "text")
        report (replay/run-replay! (evaluator {:replay/x case})
                                   candidate [:replay/x])
        outcome (first (:replay/cases report))]
    (is (= :fail (:status outcome)))
    (is (= :unrecorded-tool (get-in outcome [:provider/regressions 0 :regression/type])))
    (is (true? (:regression? outcome)))))
