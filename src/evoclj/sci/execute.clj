(ns evoclj.sci.execute
  "Bounded execution of evolvable programs inside the closed SCI context.

  DEPRECATED FORWARDING: The single authoritative implementation now lives
  in evoclj.sci.computation (Computation value object, C4/D4). This
  namespace retains the original public vars load-program!, invoke!,
  and execute-program as deprecated aliases that delegate to
  computation, preserving behavior with no duplication (INV-05).

  New code should use evoclj.sci.computation/make-computation and
  evoclj.sci.computation/execute directly. Old call sites continue to
  work with no behavior change; the delegating interrupt and EDN
  boundary guarantees remain identical (uncatchable interrupt,
  materialize-edn rejects Java objects)."
  (:require [evoclj.sci.computation :as computation]))

;; Deprecated forwarding aliases - single implementation in computation
;; (no duplicated allow-list, no new runtime, no behavior change).

(def ^{:deprecated "0.1.0" :doc "Deprecated alias for evoclj.sci.computation/load-program!. Retained for compatibility; forwards with no behavior change."}
  load-program!
  computation/load-program!)

(def ^{:deprecated "0.1.0" :doc "Deprecated alias for evoclj.sci.computation/invoke!. Retained for compatibility; forwards with no behavior change."}
  invoke!
  computation/invoke!)

(def ^{:deprecated "0.1.0" :doc "Deprecated alias for evoclj.sci.computation/execute-program. Retained for compatibility; forwards with no behavior change."}
  execute-program
  computation/execute-program)
