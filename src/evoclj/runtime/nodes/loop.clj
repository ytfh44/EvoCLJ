(ns evoclj.runtime.nodes.loop
  "The :loop node handler (Task 6.4).

  A :loop node {:node/type :loop :body :node/body :until :program/done?
  :max-iterations 8 :next :node/finish} iterates its :body until the
  :until program (a SCI program invoked inside the phenotype's isolated
  SCI runtime — the same evoclj.sci.execute/invoke! boundary the :sci
  node uses, Global Constraint 7) returns a boolean done? flag, hard
  capped at :max-iterations.

  LOOP STATE IS SESSION-LOCAL DATA (Global Constraint 23), never a SCI
  global var: the per-loop-node iteration counter travels in the
  scheduler's per-session runtime-state under :loop-state, a map of
  loop node id -> iteration count. The scheduler builds runtime-state
  fresh for every run-session! and threads it through the visit loop,
  so two sessions on ONE phenotype (ONE shared SCI runtime) can never
  see each other's counters; the handler only READS the counter from
  runtime-state and never writes any SCI var. The :until and :body
  programs are pure functions of their input — the counter can never
  leak into them.

  THE :UNTIL PROGRAM INPUT CONTRACT (normative):

    {:iteration <int>    ; the current iteration count (0-based)
     :payload <EDN>}     ; the loop's current input payload

  and it must return a BOOLEAN: true = done (exit to :next), false =
  iterate :body once more. A non-boolean result is a malformed loop
  predicate (:failed :node/invalid :reason :invalid-until-result) —
  evolvable output is runtime data, so the handler converts it into a
  :failed transition rather than guessing.

  THE BODY EDGE: when the predicate is not satisfied the handler
  returns :continue with :next [:node/body] — the body node is a
  regular node stepped by the scheduler, so any intents it emits still
  cross the kernel-owned Intent/Capability Broker (Global Constraint
  8). The body's :next points back at the :loop node — the sanctioned
  iteration edge (evoclj.compiler.topology validates the :body target
  and still rejects raw cycles that contain no :loop node). Each time
  the scheduler observes a :loop node choosing its :body, it
  increments that loop node's counter in runtime-state's :loop-state,
  so the next visit of the same :loop node reads the new count.

  THE :max-iterations CAP (the typed budget outcome — documented in
  evoclj.runtime.scheduler): when the counter reaches :max-iterations
  the handler returns a :failed transition carrying the typed error
  data {:error/type :loop/max-iterations-exceeded :max-iterations n
  :iterations n}. The scheduler recognizes this error type and routes
  it to the :budget-exhausted session state (:session/budget-exhausted
  event recording the {:max-iterations n} limit), so an unbounded
  predicate is a budget outcome — failures are evidence, never
  discarded traces — not a session failure.

  PAYLOAD FORWARDING: the scheduler feeds each next node the most
  recently accumulated session output (peek of :outputs), so on the
  FIRST visit of a :loop node (the entry-node case) :outputs is still
  empty and the body would otherwise receive nil. The handler forwards
  its input payload as the step output while the accumulated :outputs
  are empty; once any output has accumulated, the iteration
  accumulator already travels as the last accumulated output, so the
  loop contributes nothing. This keeps the accumulated session outputs
  clean (exactly the body's decisions and provider results).

  Error contract (Global Constraint 22 — plain serializable data):
  :node/runtime-invalid (malformed runtime-state — the shared checks
  plus :sci-runtime-missing, the same trust boundary as the :sci
  node), :node/input-invalid (malformed input-event), :node/invalid
  (malformed loop node — :reason :type-mismatch,
  :missing-required-key, :invalid-attribute, :invalid-max-iterations —
  or a malformed until result — :reason :invalid-until-result), and
  :node/transition-invalid (a result that fails the shared schema)."
  (:require [evoclj.kernel.error :as err]
            [evoclj.runtime.node :as node]
            [evoclj.sci.execute :as execute]))

(defn- validate-max-iterations!
  "The handler-side guard for the :max-iterations cap (defense in
  depth; evoclj.compiler.topology already validates it at compile
  time). A compiled :loop node must carry a positive integer."
  [node]
  (when-not (pos-int? (:max-iterations node))
    (throw (err/error :node/invalid
                      "a :loop node must carry a positive integer :max-iterations"
                      {:reason :invalid-max-iterations
                       :node/type :loop
                       :value (err/sanitize (:max-iterations node))})))
  node)

(defn- until-input
  "The :until program's input: the current iteration count and the
  loop's input payload (see the namespace docstring for the contract)."
  [iterations payload]
  {:iteration iterations :payload payload})

(defn- loop-exhausted
  "The typed budget outcome for an iteration count that reached the
  :max-iterations cap: a :failed transition carrying serializable
  error data. evoclj.runtime.scheduler routes this error type to the
  :budget-exhausted session state (the typed budget outcome chosen for
  Task 6.4 and documented there)."
  [iterations max-iterations]
  (node/validate-transition!
   {:transition/status :failed
    :outputs []
    :intents []
    :error {:error/type :loop/max-iterations-exceeded
            :node/type :loop
            :max-iterations max-iterations
            :iterations iterations}}))

(defn loop-handler
  "Construct the trusted :loop node handler.

  The node must carry :body (a keyword — the node id iterated), :until
  (a keyword — the compiled program id of the done? predicate loaded in
  runtime-state's :sci-runtime), a positive integer :max-iterations,
  and :next (the node id the loop exits to once done).

  Each visit invokes the :until program with {:iteration <count>
  :payload <input payload>}, where the count is read from
  runtime-state's :loop-state map (session-local data — the scheduler
  threads it, never a SCI global var). Returns

    - :continue with :next [:node/body]  ; predicate false, iterate
    - :continue with :next [<:next>]     ; predicate true, exit
    - :failed  :loop/max-iterations-exceeded  ; count hit the cap
    - :failed  <program error>           ; the :until program errored
    - :failed  :node/invalid             ; a non-boolean until result

  The :outputs are [<input payload>] only while the session's
  accumulated :outputs are empty (entry-node payload forwarding — see
  the namespace docstring); otherwise []. Every result is validated
  against the shared transition schema before it is returned."
  []
  (reify node/NodeHandler
    (step [_ runtime-state node input-event]
      (node/validate-runtime-state! runtime-state)
      (node/validate-node! node :loop)
      (node/validate-input-event! input-event)
      (validate-max-iterations! node)
      (when-not (contains? runtime-state :sci-runtime)
        (throw (err/error :node/runtime-invalid
                          "runtime-state must carry :sci-runtime for a :loop node"
                          {:reason :sci-runtime-missing})))
      (let [iterations (get-in runtime-state [:loop-state (:node/id runtime-state)] 0)
            max-iterations (:max-iterations node)
            payload (:payload input-event)
            outputs (if (empty? (:outputs runtime-state)) [payload] [])]
        (if (>= iterations max-iterations)
          (loop-exhausted iterations max-iterations)
          (let [result (execute/invoke! (:sci-runtime runtime-state)
                                        (:until node)
                                        (until-input iterations payload)
                                        (:limits runtime-state))]
            (if (= :error (:status result))
              (node/validate-transition!
               {:transition/status :failed
                :outputs []
                :intents []
                :error (:error result)})
              (let [done? (:value result)]
                (cond
                  (not (boolean? done?))
                  (node/validate-transition!
                   {:transition/status :failed
                    :outputs []
                    :intents []
                    :error (err/error-data
                            (err/error :node/invalid
                                       "the :until program must return a boolean done? flag"
                                       {:reason :invalid-until-result
                                        :node/type :loop
                                        :program/id (:until node)
                                        :value (err/sanitize done?)}))})

                  done?
                  (node/validate-transition!
                   {:transition/status :continue
                    :outputs outputs
                    :intents []
                    :next [(:next node)]})

                  :else
                  (node/validate-transition!
                   {:transition/status :continue
                    :outputs outputs
                    :intents []
                    :next [(:body node)]}))))))))))
