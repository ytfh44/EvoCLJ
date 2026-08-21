(ns evoclj.context.loop
  "Compatibility alias — canonical namespace is evoclj.context.compression.loop."
  (:require [evoclj.context.compression.loop]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.loop)]
  (intern 'evoclj.context.loop sym (deref v#)))
