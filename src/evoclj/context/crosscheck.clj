(ns evoclj.context.crosscheck
  "Compatibility alias — canonical namespace is evoclj.context.compression.crosscheck."
  (:require [evoclj.context.compression.crosscheck]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.crosscheck)]
  (intern 'evoclj.context.crosscheck sym (deref v#)))
