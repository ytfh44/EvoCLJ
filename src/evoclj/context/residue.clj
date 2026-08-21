(ns evoclj.context.residue
  "Compatibility alias — canonical namespace is evoclj.context.compression.residue."
  (:require [evoclj.context.compression.residue]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.residue)]
  (intern 'evoclj.context.residue sym (deref v#)))
