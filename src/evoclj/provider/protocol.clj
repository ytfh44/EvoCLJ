(ns evoclj.provider.protocol
  "The Provider protocol (Task 4.3).

  Providers adapt REAL resources behind the kernel-owned broker: a
  model adapter talks to a model endpoint, a filesystem adapter talks
  to disk, a fixture adapter computes deterministically. Evolvable
  code never calls a provider directly — it emits typed Intents
  (evoclj.intent), and the broker (Milestone 4) drives the three
  protocol methods in a fixed order:

    1. (describe provider) — the tool descriptor: a plain, validated
       map declaring :tool/id, :effect, :input-schema,
       :output-schema, :required-action, and (optionally) a :retry
       block. This is the ONLY provider metadata that may cross the
       boundary (Global Constraint 22); secrets and constructor
       config are closed over and never appear here.

    2. (normalize-request provider intent) — turn a user-facing
       request into the CANONICAL resource descriptor, the real
       target. This runs BEFORE authorization (Global Constraint 9:
       adding a visible action/tool never grants resource authority):
       a filesystem adapter resolves \"a/../secret\" to the canonical
       protected path so authorization checks the real target, never
       the raw string.

    3. (execute-request! provider authorized-request) — perform the
       authorized effect and return the result VALUE, which the
       broker validates against :output-schema (Task 4.5) before it
       is visible anywhere.

  All three methods deal in plain validated Clojure data (Global
  Constraint 22); a provider never exposes a Java object, lazy
  sequence, or open resource across the boundary."
  (:refer-clojure :exclude []))

(defprotocol Provider
  "Adapter for one real resource behind the broker. A Provider is a
  kernel-owned host object (Global Constraint 19), never agent-mutable
  and never serialized; only its descriptor and request/result data
  cross boundaries."
  (describe [provider])
  (normalize-request [provider intent])
  (execute-request! [provider authorized-request]))
