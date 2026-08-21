(ns evoclj.context.token-estimator
  "Compatibility alias — canonical namespace is evoclj.context.compression.token-estimator."
  (:require [evoclj.context.compression.token-estimator]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.token-estimator)]
  (intern 'evoclj.context.token-estimator sym (deref v#)))
