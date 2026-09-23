(ns evoclj.node.types
  "The node-type vocabulary (leaf; no dependencies).

  ONE definition of what a topology node is: the closed type set, the
  per-type required keys, and the keyword-valued attribute keys. The
  compiler (evoclj.compiler.topology) and the runtime
  (evoclj.runtime.node) both consume this namespace, so a type added
  here is known to the compiler's syntax set and to the runtime's
  handler registry at once — the two used to keep separate copies, and
  the copies had already drifted (the compiler required :model for
  :llm, the runtime's handler table did not).

  Definition > validation: the sets are closed. A node type outside
  `node-types` is rejected by the compiler (:topology/invalid
  :unknown-node-type) and by evoclj.capability.core/node-effects
  (:capability/unknown-node-type) — never silently treated as a pure
  node.

  Leaf discipline: this namespace requires nothing, so any layer
  (compiler, runtime, capability) may depend on it without a cycle."
  )

(def node-types
  "The normative v0 node type set — every type the compiler knows
  syntactically (definition). Syntax IS the executable set: every known
  type has a runtime handler. The :route reservation was removed
  (ExtraModules repair): it declared only a single :next edge and no
  branch attributes, so it carried no semantics a plain edge does not
  already carry — all v0 control flow is :sci decisions + :loop
  iteration + :tool/:llm/:emit terminals. It is rejected as an unknown
  type now and returns with a handler plus edge schema when branching
  semantics are specified."
  #{:llm :sci :tool :loop :emit :memory/read :memory/write})

(def required-keys
  "Per-type keys a node must declare. A :loop carries an explicit
  Region/Loop shape: :body is the iterated node id, :exit is the normal
  successor, :until is the done? program id, and :max-iterations is a
  positive integer."
  {:llm #{:model}
   :sci #{:program}
   :tool #{:tool}
   :loop #{:exit :body :until :max-iterations}
   :emit #{}
   :memory/read #{:memory}
   :memory/write #{:memory}})

(def attribute-keys
  "Keys whose value must be a keyword when present."
  [:model :program :tool :memory :next :exit :body :until])
