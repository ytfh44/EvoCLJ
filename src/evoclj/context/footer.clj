(ns evoclj.context.footer
  "Compatibility alias — canonical namespace is evoclj.context.compression.footer."
  (:require [evoclj.context.compression.footer]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.footer)]
  (intern 'evoclj.context.footer sym (deref v#)))
