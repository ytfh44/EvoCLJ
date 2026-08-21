(ns evoclj.context.registry
  "Compatibility alias — canonical namespace is evoclj.context.compression.registry."
  (:require [evoclj.context.compression.registry]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.registry)]
  (intern 'evoclj.context.registry sym (deref v#)))
