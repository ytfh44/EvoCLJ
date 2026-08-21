(ns evoclj.context.compacter
  "Compatibility alias — canonical namespace is evoclj.context.compression.compacter."
  (:require [evoclj.context.compression.compacter]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.compacter)]
  (intern 'evoclj.context.compacter sym (deref v#)))
