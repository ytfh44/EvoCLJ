(ns evoclj.eval.replay
  "G4 historical replay with representative cases (component).

  run-replay! re-executes a candidate against replay cases built from
  stored Episodes and reports per-case outcomes plus aggregate
  regressions:

      (run-replay! evaluator candidate replay-case-ids)
      ;; => {:replay/requested [ids]
      ;;     :replay/cases [<case outcome> ...]
      ;;     :aggregate {:cases n :passed n :failed n :regressions n
      ;;                 :critical n :critical-regressions n
      ;;                 :hard-failure? bool}
      ;;     :regressions [<regressed case outcomes>]
      ;;     :hard-failure? bool}

  A REPLAY CASE (Step 1) is built from a stored Episode (component)
  plus the episode's recorded intent trace — the tool calls the
  original run performed, their recorded effect class, and their
  recorded provider responses — and the evaluator's per-case oracle:

      {:case/id <keyword>
       :episode/id <uuid>            ; provenance: the stored Episode
       :recorded/status <:completed | :failed | :budget-exhausted>
       :task-input <EDN>             ; the replay input (the episode's
                                     ;   task payload, read from the
                                     ;   CAS via :task-ref by the
                                     ;   evaluator context)
       :expected-output <EDN>        ; the case oracle's correct output
       :output/equiv? <fn | keyword> ; OPTIONAL equivalence predicate;
                                     ;   default = byte-identical
       :mode <:fixture | :recorded-read | :forbid-write>
       :critical? <bool>             ; OPTIONAL, default false
       :trace [<recorded intent> ...]
       :responses {<tool/id> {<canonical args> <recorded response>}}}

  Replays must NEVER blindly repeat real external writes, so every
  external provider is FIXTURABLE: the evaluator supplies fixture
  providers (:replay/fixtures) and the replay-mode-aware wrapper
  evoclj.eval.replay/replay-provider (built on the Provider protocol,
  component) stands between the broker and the fixture. The three modes
  (Step 2) define how the wrapper behaves per tool:

    :fixture       EVERY intent (read and write) is served its recorded
                   response as a pure fixture computation; the wrapped
                   fixture provider's execute-request! NEVER runs and
                   the served descriptor marks write effects :pure
                   (nothing real happens; no idempotency key is
                   demanded of the candidate). Pure behavioral replay.
    :recorded-read reads are served their recorded responses;
                   write-type intents are DENIED with
                   :provider/replay-write-denied (fail-closed — a
                   replay never repeats a real write). The denial is
                   recorded in the run's intent evidence but is NOT
                   itself a regression: the case is judged on output.
    :forbid-write  reads are served their recorded responses; ANY
                   write-type intent is denied AND counted as a
                   regression — the replay asserts no external writes
                   happen at all.

  Read/write classification is per tool: a tool whose recorded
  descriptor carries :effect :write is write-type; :pure/:read is
  read-type. A tool call whose canonical args have no recorded
  response (:provider/replay-response-missing) or a tool absent from
  the trace (:provider/not-found) is a trace divergence and counts as
  a regression in EVERY mode.

  THE HARNESS — documented lighter-harness choice: the task permits
  the scheduler (fresh session per case, temp store) or a lighter
  harness when the scheduler's full stack is heavy. The scheduler's
  persistence stack (SQLite migration, generation/session rows, CAS
  artifacts, the append-only causal event log) exists to make LIVE
  runs durable evidence; in replay the recorded run IS already the
  persisted evidence, so a temp store would add nothing but overhead.
  run-replay! therefore re-walks the candidate's compiled topology
  with the SAME node handlers (evoclj.runtime.node/step) and the SAME
  broker (evoclj.intent.dispatch/dispatch!) the scheduler uses —
  a fresh session id and a fresh isolated SCI runtime (fresh
  Phenotype) per case (Global Constraint 23) — and feeds provider
  results back into the accumulated outputs exactly like
  run-session!, minus all persistence. The candidate Genome is loaded
  and compiled FROM SCRATCH (never a cached Mutator claim; Global
  Constraints 4, 6).

  Output equivalence (Step 4): a case may declare :output/equiv? as a
  fn (in-memory case) or as a keyword naming an equivalence in the
  evaluator's :equivalence/by-keyword registry (EDN-safe persisted
  cases, Global Constraint 22). The default oracle is byte-identical
  output (the :equivalence/byte-identical entry). An unknown keyword
  fails closed with :eval/replay-equiv-unknown.

  Hard failures (Step 5): a case may be marked :critical? true; ANY
  critical regression flips the report's :hard-failure? to true.

  EVALUATOR CONTEXT (host-side, kernel-owned; Global Constraint 19):

      {:provider/catalog <catalog map>          ; REQUIRED compile-genome
       :replay/cases <case-id -> case | (fn [id] case)>  ; REQUIRED
       :replay/fixtures <tool-id -> fixture provider>    ; REQUIRED
       :programs <registry resolver fn | vector>         ; optional
       :equivalence/by-keyword <kw -> equiv fn>          ; optional}

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/replay-context-invalid (:reason distinguishes :not-a-map,
  :catalog-missing, :cases-missing, :fixtures-missing,
  :case-ids-invalid), :eval/replay-case-invalid (:reason
  distinguishes :episode-invalid, :trace-invalid, :bad-mode,
  :missing-case-id, :missing-task-input, :missing-expected-output,
  :bad-critical, :bad-equiv), :eval/replay-case-not-found,
  :eval/replay-fixture-missing, :eval/replay-equiv-unknown."
  (:require [evoclj.compiler.core :as compiler]
            [evoclj.genome.load :as load]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.node :as node]
            [evoclj.runtime.phenotype :as phenotype])
  (:import (java.nio.charset StandardCharsets)
           (java.util Date)))

;; --- the three provider replay modes (Step 2) ------------------------------

(def replay-modes
  "The three provider replay modes (component Step 2)."
  #{:fixture :recorded-read :forbid-write})

;; --- canonicalization (Global Constraint 22) -------------------------------

(defn- canonical
  "Deterministic EDN form for hashing and response lookup: maps sorted
  by their pr-str key form, sets by their pr-str element form,
  collections realized eagerly (the same convention as
  evoclj.eval.dataset)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- canonical-args
  "The canonical lookup key for a tool call's args."
  [args]
  (canonical args))

;; --- Step 1: replay case construction --------------------------------------

(defn- case-error
  "A :eval/replay-case-invalid ExceptionInfo with a distinguishing
  :reason."
  [reason message value]
  (err/error :eval/replay-case-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-trace-entry!
  "One recorded trace entry must be a :intent/tool-call record carrying
  a :payload with a keyword :tool/id and a map :args, a :effect of
  :read or :write, and a recorded :response (nil is a legitimate
  recorded response — presence is checked with contains?)."
  [entry]
  (when-not (and (map? entry)
                 (= :intent/tool-call (:intent/type entry)))
    (throw (case-error :trace-invalid
                       "trace entries must be :intent/tool-call records"
                       entry)))
  (let [payload (:payload entry)]
    (when-not (and (map? payload)
                   (keyword? (:tool/id payload))
                   (map? (:args payload)))
      (throw (case-error :trace-invalid
                         "a trace entry payload must carry a keyword :tool/id and map :args"
                         payload))))
  (when-not (contains? #{:read :write} (:effect entry))
    (throw (case-error :trace-invalid
                       "a trace entry must declare :effect :read or :write"
                       entry)))
  (when-not (contains? entry :response)
    (throw (case-error :trace-invalid
                       "a trace entry must carry the recorded :response"
                       entry)))
  entry)

(defn responses-table
  "Build the recorded-response lookup table for a vector of trace
  entries: {<tool/id> {<canonical args> <recorded response>}}. Two
  calls with canonically equal args share one recorded response."
  [trace]
  (reduce (fn [table entry]
            (let [tool-id (get-in entry [:payload :tool/id])
                  key (canonical-args (get-in entry [:payload :args]))]
              (update table tool-id (fnil assoc {}) key (:response entry))))
          {}
          trace))

(defn build-replay-case
  "Build a validated replay case from a stored Episode and its recorded
  intent trace (component Step 1).

  `episode` is the stored Episode map (component contract); the case
  inherits its :episode/id and its terminal :outcome :status as
  :recorded/status — the baseline a later regression is measured
  against (a :completed baseline that now fails is a regression; a
  :failed baseline that now passes is a fix). `trace` is the vector of
  :intent/tool-call records the episode's run performed, each with the
  tool's recorded :effect (:read | :write) and recorded :response.

  `opts` keys:

    :case/id         <keyword>      REQUIRED
    :task-input      <EDN>          REQUIRED (the replay input; in
                                    production the evaluator reads it
                                    from the CAS via the episode's
                                    :task-ref — replay.clj is store-free)
    :expected-output <EDN>          REQUIRED (the case oracle)
    :mode            <replay-mode>  REQUIRED (:fixture | :recorded-read
                                    | :forbid-write)
    :critical?       <bool>         OPTIONAL, default false
    :output/equiv?   <fn|keyword>   OPTIONAL, default byte-identical

  The trace is normalized into the case's :responses table (canonical
  args -> recorded response per tool), which the replay providers
  serve. Returns the closed case map. Throws
  :eval/replay-case-invalid on any contract violation."
  [episode trace opts]
  (let [case-id (:case/id opts)
        task-input (:task-input opts)
        expected-output (:expected-output opts)
        mode (:mode opts)
        critical? (:critical? opts)
        output-equiv (:output/equiv? opts)
        recorded-status (:status (:outcome episode))]
    (when-not (and (map? episode) (uuid? (:episode/id episode)))
      (throw (case-error :episode-invalid
                         "episode must be a stored Episode carrying a uuid :episode/id"
                         episode)))
    (when-not (contains? #{:completed :failed :budget-exhausted} recorded-status)
      (throw (case-error :episode-invalid
                         "episode :outcome :status must be a terminal session status"
                         (:outcome episode))))
    (when-not (keyword? case-id)
      (throw (case-error :missing-case-id
                         "a replay case must carry a keyword :case/id"
                         case-id)))
    (when-not (contains? opts :task-input)
      (throw (case-error :missing-task-input
                         "a replay case must carry the :task-input"
                         opts)))
    (when-not (contains? opts :expected-output)
      (throw (case-error :missing-expected-output
                         "a replay case must carry the :expected-output oracle"
                         opts)))
    (when-not (contains? replay-modes mode)
      (throw (case-error :bad-mode
                         "a replay case must carry a valid provider replay :mode"
                         mode)))
    (when-not (or (nil? output-equiv)
                  (fn? output-equiv)
                  (keyword? output-equiv))
      (throw (case-error :bad-equiv
                         ":output/equiv? must be a fn, a keyword, or nil"
                         output-equiv)))
    (when-not (or (nil? critical?) (boolean? critical?))
      (throw (case-error :bad-critical
                         ":critical? must be a boolean"
                         critical?)))
    (let [entries (mapv validate-trace-entry! trace)]
      {:case/id case-id
       :episode/id (:episode/id episode)
       :recorded/status recorded-status
       :task-input task-input
       :expected-output expected-output
       :mode mode
       :critical? (boolean critical?)
       :output/equiv? output-equiv
       :trace entries
       :responses (responses-table entries)})))

(defn- validate-replay-case!
  "Validate a case map handed to run-replay! (a built case, or a
  hand-assembled equivalent) and return it unchanged."
  [case]
  (when-not (map? case)
    (throw (case-error :not-a-map "a replay case must be a map" case)))
  (doseq [[k reason] [[:case/id :missing-case-id]
                      [:episode/id :episode-invalid]
                      [:task-input :missing-task-input]
                      [:expected-output :missing-expected-output]
                      [:mode :bad-mode]
                      [:recorded/status :bad-recorded-status]
                      [:responses :trace-invalid]]]
    (when-not (contains? case k)
      (throw (case-error reason
                         (str "replay case is missing the " k " key")
                         case))))
  (when-not (contains? replay-modes (:mode case))
    (throw (case-error :bad-mode "invalid provider replay mode" (:mode case))))
  (when-not (map? (:responses case))
    (throw (case-error :trace-invalid "case :responses must be a map" case)))
  case)

;; --- Step 2: the replay-mode-aware provider wrapper ------------------------

(defn replay-provider
  "The replay-mode-aware provider wrapper (component Step 2). Wraps
  `fixture-provider` (an evoclj.provider.protocol/Provider) so the
  broker talks to replay instead of a real resource.

  The wrapped fixture provider is reused for its contract
  (:describe) and its input validation/canonicalization
  (:normalize-request) — recorded responses replace its computed
  result at :execute-request!, so the fixture's execute-request!
  NEVER runs in replay (no real external effect, ever).

  Behavior per case `mode` and the tool's recorded effect class
  (write-type iff the wrapped descriptor's :effect is :write):

    :fixture       serve the recorded response for every call; a
                   write tool's served descriptor marks :effect :pure
                   (the recorded response is served as a pure fixture
                   computation, so no idempotency key is demanded).
    :recorded-read serve recorded responses for read-type calls;
                   DENY write-type calls with
                   :provider/replay-write-denied.
    :forbid-write  same denial for write-type calls (the harness
                   additionally counts the denial as a regression).

  A call whose canonical args are absent from `responses` (the
  recorded-response table for this tool) throws
  :provider/replay-response-missing — the candidate diverged from the
  recorded trace (a regression at the case level in every mode).
  normalize-request always delegates to the wrapped fixture provider."
  [fixture-provider mode responses]
  (let [descriptor (proto/describe fixture-provider)
        write-tool? (= :write (:effect descriptor))
        served-descriptor (if write-tool? (assoc descriptor :effect :pure)
                                     descriptor)]
    (reify proto/Provider
      (describe [_] served-descriptor)
      (normalize-request [_ intent]
        (proto/normalize-request fixture-provider intent))
      (execute-request! [_ authorized-request]
        (let [args (:args authorized-request)]
          (when (and write-tool? (not= :fixture mode))
            (throw (err/error :provider/replay-write-denied
                              "replay denies write-type intents (a replay never repeats a real external write)"
                              {:tool/id (:tool/id descriptor)
                               :mode mode})))
          (let [key (canonical-args args)]
            (if-not (contains? responses key)
              (throw (err/error :provider/replay-response-missing
                                "no recorded response for this tool call in the episode trace"
                                {:tool/id (:tool/id descriptor)
                                 :args args}))
              (get responses key))))))))

;; --- Step 4: output equivalence --------------------------------------------

(def default-equivalences
  "The kernel-side equivalence registry. :equivalence/byte-identical is
  the default oracle; evaluator contexts may extend the registry via
  :equivalence/by-keyword (Step 4)."
  {:equivalence/byte-identical =})

(defn- resolve-equiv
  "Resolve the case's :output/equiv? to a predicate fn: a declared fn
  is used as-is, a keyword is looked up in the evaluator's
  :equivalence/by-keyword merged over default-equivalences, and nil
  means byte-identical output."
  [evaluator case]
  (let [e (:output/equiv? case)]
    (cond
      (nil? e) =
      (fn? e) e
      (keyword? e) (or (get (merge default-equivalences
                                   (:equivalence/by-keyword evaluator))
                            e)
                       (throw (err/error :eval/replay-equiv-unknown
                                         "no equivalence predicate registered under this keyword"
                                         {:equivalence/keyword e})))
      :else (throw (err/error :eval/replay-equiv-unknown
                              ":output/equiv? must be a fn, a keyword, or nil"
                              {:value (err/sanitize e)})))))

;; --- evaluator context -----------------------------------------------------

(defn- context-error
  [reason message value]
  (err/error :eval/replay-context-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-evaluator!
  [evaluator]
  (when-not (map? evaluator)
    (throw (context-error :not-a-map
                          "replay evaluator context must be a map"
                          evaluator)))
  (doseq [[k reason] [[:provider/catalog :catalog-missing]
                      [:replay/cases :cases-missing]
                      [:replay/fixtures :fixtures-missing]]]
    (when-not (contains? evaluator k)
      (throw (context-error reason
                            (str "replay evaluator context is missing the " k " key")
                            evaluator))))
  evaluator)

(defn- program-registry
  "The candidate's program descriptor registry: the context's
  :programs value, called with the loaded genome when it is a fn,
  returned as-is when it is a vector, and empty by default."
  [evaluator loaded]
  (let [p (:programs evaluator)]
    (cond
      (fn? p) (p loaded)
      (nil? p) []
      :else p)))

(defn- lookup-case
  "Resolve one replay case id in the evaluator's case registry; an
  unknown id fails closed with :eval/replay-case-not-found."
  [evaluator case-id]
  (let [cases (:replay/cases evaluator)
        case (cond
               (map? cases) (get cases case-id)
               (fn? cases) (cases case-id)
               :else (throw (context-error :cases-invalid
                                           ":replay/cases must be a map or a lookup fn"
                                           cases)))]
    (when-not case
      (throw (err/error :eval/replay-case-not-found
                        "no replay case with this id"
                        {:case/id case-id})))
    case))

;; --- the lighter harness (documented in the ns docstring) ------------------

(defn- program-sources
  "Decode every compiled program's source text from the loaded bundle
  files (Global Constraint 22: the CompiledGenome carries only
  :source/digest references; the source text lives in the bundle)."
  [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (:bytes (get-in loaded [:files (:file descriptor)])))
                         StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- leases-for
  "One CapabilityLease per tool id in the case's trace, granting a
  deterministic principal the tool's :invoke action. Uses operator
  principal for replay (isolated, no session) — the replay runner
  attributes intents to operator when no session is present, so the
  lease principal matches. Broker authorizes tool calls against
  these leases exactly as it would in a live run."
  [case _phenotype-id]
  (let [now (Date.)
        expires (Date. (+ (.getTime now) 60000))]
    (mapv (fn [tool-id]
            {:cap/id (random-uuid)
             :principal {:principal/type :operator}
             :resource {:kind :tool :id tool-id}
             :actions #{:invoke}
             :constraints {:max-calls 10000}
             :issued-at now
             :expires-at expires})
          (sort (keys (:responses case))))))
(defn- dispatch-one!
  "Dispatch one intent through the broker and record its observable
  outcome: {:tool/id ... :result/status :ok | :error :error/type
  ... :cause/error/type ...}. The :cause/error/type digs out the
  replay provider's typed denial/missing signals from the broker's
  :provider/execution-failed wrapper, so the case-level regression
  classifier can see them."
  [broker-context intent]
  (let [tool-id (get-in intent [:payload :tool/id])]
    (try
      (let [result (dispatch/dispatch! broker-context intent)]
        (if (= :ok (:result/status result))
          {:outcome {:tool/id tool-id :result/status :ok}
           :value (:value result)}
          {:outcome {:tool/id tool-id
                     :result/status :error
                     :error/type (:error/type result)
                     :cause/error/type (get-in result [:error/data :cause :error/type])}}))
      (catch Throwable t
        {:outcome {:tool/id tool-id
                   :result/status :error
                   :error/type (get-in (ex-data t) [:error/type] :replay/dispatch-threw)
                   :cause/error/type (get-in (ex-data t) [:error/type])
                   :error/message (.getMessage t)}}))))

(defn- run-topology!
  "The lighter harness: walk the candidate's compiled topology with
  the REAL node handlers (node/step) and the REAL broker
  (dispatch/dispatch!), feeding provider :ok results back into the
  accumulated outputs exactly like run-session! — minus persistence.
  A fresh session id and the case's fresh isolated SCI runtime carry
  the per-session attribution (Global Constraint 23).

  Returns {:run/status :completed | :failed | :budget-exhausted
  :session/id <fresh uuid> :output <accumulated outputs> :intents
  [<dispatch outcomes> ...] :error <serializable error data, :failed
  only>}."
  [phenotype broker-context task-input]
  (let [topology (get-in phenotype [:compiled :topology])
        entry (:entry topology)
        max-steps (get-in topology [:limits :max-steps])
        session-id (random-uuid)
        phenotype-id (:phenotype/id phenotype)
        seed {:event/id 1 :event/type :session/started :payload task-input}
        finish (fn [status error outputs intent-outcomes]
                 {:run/status status
                  :session/id session-id
                  :output outputs
                  :intents intent-outcomes
                  :error error})
        walk (fn walk [node-id input-event outputs steps intent-outcomes]
               (if (and max-steps (>= steps max-steps))
                 (finish :budget-exhausted nil outputs intent-outcomes)
                 (let [node (get (:nodes topology) node-id)]
                   (if-not node
                     (finish :failed {:error/type :replay/node-not-found
                                      :node/id node-id}
                             outputs intent-outcomes)
                     (let [runtime-state {:session/id session-id
                                          :phenotype/id phenotype-id
                                          :node/id node-id
                                          :outputs outputs
                                          :sci-runtime (:sci-runtime phenotype)}
                           stepped (try
                                     {:transition
                                      (node/validate-transition!
                                       (node/step ((node/handler-for (:node/type node)))
                                                  runtime-state node input-event))}
                                     (catch Throwable t
                                       {:failed (err/error-data t)}))]
                       (if-let [error (:failed stepped)]
                         (finish :failed error outputs intent-outcomes)
                         (let [transition (:transition stepped)]
                           (if (= :failed (:transition/status transition))
                             (finish :failed (:error transition)
                                     outputs intent-outcomes)
                             (if (= :complete (:transition/status transition))
                               (finish :completed nil (:outputs transition)
                                       intent-outcomes)
                               ;; :continue — dispatch every emitted intent
                               ;; through the broker and feed :ok results
                               ;; back into the accumulated outputs
                               (let [fed (reduce (fn [{:keys [outputs outcomes]} intent]
                                                   (let [d (dispatch-one!
                                                           broker-context intent)]
                                                     {:outputs (if (contains? d :value)
                                                                 (conj outputs (:value d))
                                                                 outputs)
                                                      :outcomes (conj outcomes
                                                                      (:outcome d))}))
                                                 {:outputs (into outputs
                                                                 (:outputs transition))
                                                  :outcomes intent-outcomes}
                                                 (:intents transition))
                                     nxt (first (:next transition))]
                                 (if nxt
                                   (walk nxt
                                         {:event/id (inc (:event/id input-event))
                                          :event/type :node/completed
                                          :payload (peek (:outputs fed))}
                                         (:outputs fed) (inc steps)
                                         (:outcomes fed))
                                   (finish :failed
                                           {:error/type :replay/dangling-run
                                            :node/id node-id}
                                           (:outputs fed)
                                           (:outcomes fed)))))))))))))]
    (walk entry seed [] 0 [])))

;; --- per-case outcome and the report ---------------------------------------

(defn- provider-regressions
  "The provider-mode regressions for a case, derived from the run's
  dispatch outcomes:

    - :provider/replay-response-missing — the candidate called a tool
      with args the episode trace never recorded (trace divergence);
      a regression in EVERY mode.
    - :provider/not-found — the candidate called a tool absent from
      the trace entirely; a regression in EVERY mode.
    - :provider/replay-write-denied — a write-type intent denied by
      the replay wrapper; a regression ONLY under :forbid-write (in
      :recorded-read the denial is recorded evidence only)."
  [case-map intent-outcomes]
  (into []
        (keep (fn [outcome]
                (let [t (or (:cause/error/type outcome) (:error/type outcome))]
                  (case t
                    :provider/replay-response-missing
                    {:regression/type :unrecorded-call
                     :tool/id (:tool/id outcome)}
                    :provider/not-found
                    {:regression/type :unrecorded-tool
                     :tool/id (:tool/id outcome)}
                    :provider/replay-write-denied
                    (when (= :forbid-write (:mode case-map))
                      {:regression/type :write-denied
                       :tool/id (:tool/id outcome)})
                    nil))))
        intent-outcomes))

(defn- case-outcome
  "The per-case outcome (component Steps 3-5):

      {:case/id ... :episode/id ... :mode ... :critical? ...
       :recorded/status ...
       :run {...}                    ; the harness run result
       :output/match? bool           ; equivalence(recorded oracle, actual)
       :provider/regressions [...]   ; Step 2 mode violations
       :reasons [...]                ; [:run-not-completed]
                                     ; [:output-mismatch] [:provider-regression]
       :status :pass | :fail
       :regression? bool             ; a :completed baseline that now fails
       :hard-failure? bool}          ; a critical case that regressed"
  [case run]
  (let [match? ((:output/equiv? case)
                (:expected-output case) (:output run))
        provider-regs (provider-regressions case (:intents run))
        reasons (cond-> []
                  (not= :completed (:run/status run)) (conj :run-not-completed)
                  (not match?) (conj :output-mismatch)
                  (seq provider-regs) (conj :provider-regression))
        status (if (empty? reasons) :pass :fail)
        regression? (and (= :completed (:recorded/status case))
                         (= :fail status))]
    {:case/id (:case/id case)
     :episode/id (:episode/id case)
     :mode (:mode case)
     :critical? (:critical? case)
     :recorded/status (:recorded/status case)
     :run run
     :output/match? match?
     :provider/regressions provider-regs
     :reasons reasons
     :status status
     :regression? regression?
     :hard-failure? (and (:critical? case) regression?)}))

(defn- fixture-provider
  "Resolve the evaluator's fixture provider for a trace tool: a
  provider value is used as-is, a 0-ary fn is called. A tool with no
  fixture fails closed — replay cannot stand in for it."
  [evaluator tool-id]
  (let [f (get (:replay/fixtures evaluator) tool-id)]
    (when-not f
      (throw (err/error :eval/replay-fixture-missing
                        "no fixture provider registered for this trace tool"
                        {:tool/id tool-id})))
    (if (fn? f) (f) f)))

(defn- run-case!
  "Run ONE replay case: build the replay provider wrappers for the
  case's trace tools (Step 2), a fresh broker context with leases for
  exactly those tools, a fresh isolated Phenotype (fresh SCI runtime —
  fresh session per case, Global Constraint 23), and run the candidate
  topology on the case's :task-input. Returns the case outcome."
  [evaluator compiled loaded case]
  (let [registry (registry/create-registry)
        tool-ids (sort (keys (:responses case)))
        _ (doseq [tool-id tool-ids]
            (registry/register!
             registry
             (replay-provider (fixture-provider evaluator tool-id)
                              (:mode case)
                              (get (:responses case) tool-id))))
        usage (atom {})
        leases (leases-for case (:compiled/phenotype-id compiled))
        broker-context (dispatch/make-broker-context
                        {:registry registry :leases leases :usage usage})
        phenotype (phenotype/instantiate
                   compiled
                   {:providers {:registry registry}
                    :capabilities {:leases leases :usage usage}
                    :program-sources (program-sources loaded compiled)})
        run (run-topology! phenotype broker-context (:task-input case))]
    (case-outcome case run)))

(defn- aggregate
  "The aggregate regression summary over the per-case outcomes."
  [outcomes]
  (let [regressed (filterv :regression? outcomes)
        critical (filterv :critical? outcomes)
        hard (filterv :hard-failure? outcomes)]
    {:cases (count outcomes)
     :passed (count (filter #(= :pass (:status %)) outcomes))
     :failed (count (filter #(= :fail (:status %)) outcomes))
     :regressions (count regressed)
     :critical (count critical)
     :critical-regressions (count hard)
     :hard-failure? (boolean (seq hard))}))

;; --- the entry point (G4) --------------------------------------------------

(defn run-replay!
  "Run `candidate` (a Genome bundle root: path string or Path) on the
  requested `replay-case-ids` and return the replay report (component
  Step 6). The candidate Genome is loaded and compiled FROM SCRATCH
  (Global Constraints 4, 6); every case runs in a fresh session with a
  fresh isolated Phenotype against replay-provider-wrapped fixtures
  (Step 2), judged by the case oracle under its output equivalence
  predicate (Step 4), with :critical regressions flipping
  :hard-failure? (Step 5).

  The report:

      {:replay/requested <requested ids in order>
       :replay/cases [<per-case outcome> ...]   ; in requested order
       :aggregate {:cases ... :passed ... :failed ... :regressions ...
                   :critical ... :critical-regressions ...
                   :hard-failure? bool}
       :regressions [<regressed outcomes> ...]
       :hard-failure? bool}"

  [evaluator candidate replay-case-ids]
  (validate-evaluator! evaluator)
  (when-not (sequential? replay-case-ids)
    (throw (context-error :case-ids-invalid
                          "replay-case-ids must be a sequential collection of case ids"
                          replay-case-ids)))
  (let [loaded (load/load-genome candidate)
        compiled (compiler/compile-genome
                  (assoc loaded :programs (program-registry evaluator loaded))
                  (:provider/catalog evaluator))
        outcomes (mapv (fn [case-id]
                         (let [case-map (lookup-case evaluator case-id)
                               case-map (validate-replay-case! case-map)
                               case-map (assoc case-map :output/equiv?
                                               (resolve-equiv evaluator case-map))]
                           (run-case! evaluator compiled loaded case-map)))
                       replay-case-ids)]
    {:replay/requested (vec replay-case-ids)
     :replay/cases outcomes
     :aggregate (aggregate outcomes)
     :regressions (filterv :regression? outcomes)
     :hard-failure? (:hard-failure? (aggregate outcomes))}))
