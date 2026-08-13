(ns evoclj.sci.expose
  "The explicit host surface handed into the closed SCI context
  (Task 3.1).

  Everything an evolvable program can touch beyond the pure
  clojure.core allowlist is whatever this namespace exposes: at this
  milestone exactly one namespace of PURE data constructors for the
  typed Intent ABI, `evo.api.intent`. The constructors only build plain
  maps — they hold no host state, perform no IO, and cannot reach the
  filesystem, environment, JVM, processes, or the network. The full
  typed Intent ABI arrives in Milestone 4; here the ABI is only the
  constructor shape, and the values it produces are plain EDN.

  Global Constraint 7 (evolvable code executes without ambient JVM,
  filesystem, process, network, secret, or database authority) is
  satisfied by construction: nothing in this namespace can perform an
  effect. Global Constraint 22 (only validated Clojure data crosses
  module boundaries) is satisfied too: every constructor result is a
  plain map that round-trips through pr-str / clojure.edn read-string.")

(defn tool-call
  "Build the plain-data :intent/tool-call intent map for `payload`.

  `payload` is passed through as data (validated by the caller at the
  boundary). Returns exactly {:intent/type :intent/tool-call
  :payload <payload>} — a plain map, nothing else. No effect is
  requested here; a host capability broker authorizes and executes the
  intent (Milestone 4)."
  [payload]
  {:intent/type :intent/tool-call
   :payload payload})

(defn finish
  "Build the plain-data :intent/finish intent map carrying `value`.

  `value` is passed through as data. Returns exactly
  {:intent/type :intent/finish :payload {:value <value>}} — a plain
  map, nothing else."
  [value]
  {:intent/type :intent/finish
   :payload {:value value}})

(def api-namespaces
  "The namespace map handed to SCI's :namespaces option.

  A map of namespace symbol to a map of simple var symbol to host
  value. evoclj.sci.context/make-context installs this by default and
  automatically adds every fully qualified symbol to the context's
  :allow set, so exactly these vars — and only these — become callable
  from inside the sandbox, e.g. (evo.api.intent/tool-call {...})."
  {'evo.api.intent {'tool-call tool-call
                    'finish finish}})
