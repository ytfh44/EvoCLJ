(ns evoclj.context.eval
  "Compatibility alias — canonical namespace is evoclj.context.compression.eval."
  (:require [evoclj.context.compression.eval]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.eval)]
  (intern 'evoclj.context.eval sym (deref v#)))
