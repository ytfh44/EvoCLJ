(ns evoclj.context.apply
  "Compatibility alias — canonical namespace is evoclj.context.compression.apply."
  (:require [evoclj.context.compression.apply]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.apply)]
  (intern 'evoclj.context.apply sym (deref v#)))
