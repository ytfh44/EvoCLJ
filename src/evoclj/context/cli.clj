(ns evoclj.context.cli
  "Compatibility alias — canonical namespace is evoclj.context.compression.cli."
  (:require [evoclj.context.compression.cli]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.cli)]
  (intern 'evoclj.context.cli sym (deref v#)))
