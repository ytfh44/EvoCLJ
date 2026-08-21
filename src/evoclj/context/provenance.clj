(ns evoclj.context.provenance
  "Compatibility alias — canonical namespace is evoclj.context.compression.provenance."
  (:require [evoclj.context.compression.provenance]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.provenance)]
  (intern 'evoclj.context.provenance sym (deref v#)))
