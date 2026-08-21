(ns evoclj.runtime.nodes.sci
  "The :sci node handler (component).

  A :sci node invokes the node's :program inside the phenotype's
  isolated SCI runtime (evoclj.sci.execute/invoke!) with the
  input-event's :payload as input, then converts the program's
  decision value into validated, fully attributed intents via the pure
  evoclj.intent.core constructors. The handler performs NO external
  effect: the program runs sandboxed (no ambient authority, Global
  Constraint 7) and the emitted intents are only requests — the component scheduler dispatches them through the broker (Global Constraint
  8). Attribution is a parameter, never guessed (Global Constraint
  20).

  PROGRAM OUTPUT CONTRACT (the decision value invoke! returns):

    nil                        -> no intents
    {:action <intent-request>} -> one intent
    [{:action <intent-request>} ...] -> one intent per entry

  where <intent-request> is {:intent/type <one of the six v0 intent
  types> :payload <the type's payload map>} — exactly the shape the
  seed route program (test/fixtures/genomes/minimal-valid/programs/
  route.clj) produces. The payload is passed to the matching
  evoclj.intent.core constructor, which validates it against the
  Intent ABI and assembles full attribution.

  Failures are runtime DATA, not thrown exceptions: a program error
  (invoke! :status :error), a malformed decision value, or an
  unconstructable intent all yield a :failed transition carrying
  serializable error data (evoclj.kernel.error/error-data), which the
  scheduler must preserve as the error artifact when it fails the
  session."
  (:require [evoclj.intent.core :as intent]
            [evoclj.kernel.error :as err]
            [evoclj.runtime.node :as node]
            [evoclj.sci.execute :as execute]))

(defn- decision-requests
  "Extract the vector of intent requests from a program decision value
  (see the namespace docstring for the contract). Throws
  :node/invalid :reason :invalid-program-output for any other shape —
  evolvable output is runtime data, so the caller converts the error
  into a :failed transition."
  [value]
  (cond
    (nil? value)
    []

    (and (map? value) (contains? value :action))
    [(:action value)]

    (vector? value)
    (mapv (fn [entry]
            (when-not (and (map? entry) (contains? entry :action))
              (throw (err/error :node/invalid
                                "program decision entries must be {:action <intent-request>}"
                                {:reason :invalid-program-output
                                 :value (err/sanitize entry)})))
            (:action entry))
          value)

    :else
    (throw (err/error :node/invalid
                      "program decision must be nil, {:action <intent-request>}, or a vector of {:action <intent-request>}"
                      {:reason :invalid-program-output
                       :value (err/sanitize value)}))))

(defn- construct-intent
  "Build one validated, fully attributed intent from an intent request
  map ({:intent/type <v0 type> :payload <payload map>}) via the
  matching evoclj.intent.core constructor, with attribution passed in
  (Global Constraint 20). The constructor validates the payload and
  attribution against the Intent ABI; a malformed request throws
  (:node/invalid for an unknown intent type, :intent/schema-invalid
  for a malformed payload)."
  [request session-id phenotype-id node-id cause-event-id budget]
  (when-not (and (map? request) (keyword? (:intent/type request)))
    (throw (err/error :node/invalid
                      "program decision action must be an intent request map"
                      {:reason :invalid-intent-request
                       :value (err/sanitize request)})))
  (let [payload (:payload request)]
    (case (:intent/type request)
      :intent/tool-call (intent/tool-call session-id phenotype-id node-id
                                          cause-event-id payload budget)
      :intent/model-call (intent/model-call session-id phenotype-id node-id
                                            cause-event-id payload budget)
      :intent/memory-read (intent/memory-read session-id phenotype-id node-id
                                              cause-event-id payload budget)
      :intent/memory-write (intent/memory-write session-id phenotype-id node-id
                                                cause-event-id payload budget)
      :intent/finish (intent/finish session-id phenotype-id node-id
                                    cause-event-id payload budget)
      :intent/fail (intent/fail session-id phenotype-id node-id
                                cause-event-id payload budget)
      (throw (err/error :node/invalid
                        "program decision carries an unknown intent type"
                        {:reason :unknown-intent-type
                         :intent/type (:intent/type request)})))))

(defn- construct-intents
  "Pure: convert a program decision value into its validated intents.
  Returns {:intents [...]} on success, or {:error <serializable error
  data>} when the evolvable output is not a valid decision — a runtime
  failure, converted to data (never thrown out of the handler)."
  [value session-id phenotype-id node-id cause-event-id budget]
  (try
    {:intents (mapv #(construct-intent % session-id phenotype-id node-id
                                       cause-event-id budget)
                    (decision-requests value))}
    (catch clojure.lang.ExceptionInfo e
      {:error (err/error-data e)})))

(defn sci-handler
  "Construct the trusted :sci node handler.

  The node must carry :program (a keyword — the compiled program id
  loaded in runtime-state's :sci-runtime). Invokes the program with
  the input-event's :payload under the optional :limits, converts the
  decision value into validated intents, and returns

    {:transition/status :continue
     :outputs [<decision value>]      ; when non-nil, else []
     :intents [<validated intents>]
     :next [<node's :next>]}          ; [] when the node declares none

  or, on a program error or malformed decision, a :failed transition
  carrying serializable error data. Every result is validated against
  the shared transition schema before it is returned."
  []
  (reify node/NodeHandler
    (step [_ runtime-state node input-event]
      (node/validate-runtime-state! runtime-state)
      (node/validate-node! node :sci)
      (node/validate-input-event! input-event)
      (when-not (contains? runtime-state :sci-runtime)
        (throw (err/error :node/runtime-invalid
                          "runtime-state must carry :sci-runtime for a :sci node"
                          {:reason :sci-runtime-missing})))
      (let [result (execute/invoke! (:sci-runtime runtime-state)
                                    (:program node)
                                    (:payload input-event)
                                    (:limits runtime-state))]
        (if (= :error (:status result))
          (node/validate-transition!
           {:transition/status :failed
            :outputs []
            :intents []
            :error (:error result)})
          (let [value (:value result)
                conversion (construct-intents
                            value
                            (:session/id runtime-state)
                            (:phenotype/id runtime-state)
                            (:node/id runtime-state)
                            (:event/id input-event)
                            (or (:budget runtime-state) node/default-budget))]
            (if-let [error (:error conversion)]
              (node/validate-transition!
               {:transition/status :failed
                :outputs []
                :intents []
                :error error})
              (node/validate-transition!
               {:transition/status :continue
                :outputs (if (nil? value) [] [value])
                :intents (:intents conversion)
                :next (node/successor node)}))))))))
