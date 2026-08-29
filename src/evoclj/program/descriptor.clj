(ns evoclj.program.descriptor
  "Program descriptor validation with closed schema registry (PLT3).

  Wraps evoclj.compiler.program validation but is the canonical
  Program descriptor namespace referenced by the PLT3 task. The
  descriptor's :input-schema / :output-schema are keywords that MUST
  resolve via the closed registry (evoclj.store.schema); phantom
  keywords (e.g. :schema/unicorn) are unrepresentable — Definition >
  validation: only registered schemas compile.

  This namespace re-exports the registry-aware compile path and ensures
  the compiled descriptor carries the resolved Malli schemas."
  (:require [evoclj.compiler.program :as program]
            [evoclj.kernel.error :as err]
            [evoclj.store.schema :as schema]
            [malli.core :as m]))

(defn compile-descriptor
  "Validate descriptor (map with :program/id :file :entry :input-schema
  :output-schema keywords) against a loaded Genome and return the
  compiled ProgramDescriptor carrying resolved Malli schemas.

  Delegates to evoclj.compiler.program/compile-program-descriptor which
  already resolves via evoclj.store.schema and fails closed on phantom
  keywords. This wrapper exists so the PLT3 contract is reachable via
  the evoclj.program.descriptor namespace."
  [descriptor loaded-genome]
  (program/compile-program-descriptor descriptor loaded-genome))

(defn descriptor->resolved
  "Resolve a descriptor's schema keywords via the closed registry.
  Returns {:input-schema <Malli> :output-schema <Malli>} or throws
  :program/invalid with :unknown-schema for phantom keywords."
  [{:keys [input-schema output-schema] :as descriptor}]
  {:input-schema (schema/resolve-schema! input-schema)
   :output-schema (schema/resolve-schema! output-schema)})

(defn phantom?
  "True when kw is not registered (phantom)."
  [kw]
  (not (schema/registered? kw)))
