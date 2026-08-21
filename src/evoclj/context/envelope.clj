(ns evoclj.context.envelope
  "Compatibility alias — canonical namespace is evoclj.context.compression.envelope."
  (:require [evoclj.context.compression.envelope]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.envelope)]
  (intern 'evoclj.context.envelope sym (deref v#)))
