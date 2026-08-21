(ns evoclj.context.compressor
  "Compatibility alias — canonical namespace is evoclj.context.compression.compressor."
  (:require [evoclj.context.compression.compressor]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.compressor)]
  (intern 'evoclj.context.compressor sym (deref v#)))
