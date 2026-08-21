(ns evoclj.context.error
  "Compatibility alias — canonical namespace is evoclj.context.compression.error."
  (:require [evoclj.context.compression.error]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.error)]
  (intern 'evoclj.context.error sym (deref v#)))
