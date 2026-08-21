(ns evoclj.context.archivers
  "Compatibility alias — canonical namespace is evoclj.context.compression.archivers."
  (:require [evoclj.context.compression.archivers]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.archivers)]
  (intern 'evoclj.context.archivers sym (deref v#)))
