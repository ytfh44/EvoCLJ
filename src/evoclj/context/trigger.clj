(ns evoclj.context.trigger
  "Compatibility alias — canonical namespace is evoclj.context.compression.trigger."
  (:require [evoclj.context.compression.trigger]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.trigger)]
  (intern 'evoclj.context.trigger sym (deref v#)))
