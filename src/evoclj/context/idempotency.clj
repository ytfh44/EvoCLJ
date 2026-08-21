(ns evoclj.context.idempotency
  "Compatibility alias — canonical namespace is evoclj.context.compression.idempotency."
  (:require [evoclj.context.compression.idempotency]))
;; Re-export all public vars from canonical namespace for backward compatibility.
(doseq [[sym v#] (ns-publics 'evoclj.context.compression.idempotency)]
  (intern 'evoclj.context.idempotency sym (deref v#)))
