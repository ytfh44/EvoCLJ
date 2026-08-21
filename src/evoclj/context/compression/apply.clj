(ns evoclj.context.compression.apply
  "Applies a compressed envelope to a context string.

  The result of applying an envelope is a new context string that
  consists of the serialized envelope followed by the fresh tail — the
  most recent portion of the conversation that must not be re-compressed
  yet. This preserves the invariant that the envelope is the prefix and
  the fresh tail is the suffix, with nothing lost.

  The envelope is serialized with `envelope->edn` (pr-str). The fresh
  tail is the caller's responsibility: the apply module does NOT slice
  the context itself, because the boundary between compressed prefix and
  fresh tail is a policy decision the caller owns."
  (:require [evoclj.context.compression.error :as err]
            [evoclj.context.compression.envelope :as envelope]
            [evoclj.sci.boundary :as boundary]))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn apply-envelope
  "Apply `envelope` to `context-str`, returning a new context string.

  The new context is: (serialized-envelope) + fresh-tail + (optional
  separator). The envelope is serialized via `envelope->edn` and is
  guaranteed EDN-safe. The fresh tail is the caller's verbatim text.

  `separator` (optional) is inserted between the envelope and the fresh
  tail. Default: a newline.

  Throws :context/apply-invalid when the envelope is malformed or
  `context-str` is not a string."
  [envelope fresh-tail & [separator]]
  (envelope/validate-envelope envelope)
  (when-not (string? fresh-tail)
    (throw (err/error :context/apply-invalid
                      "fresh-tail must be a string"
                      {:value (err/sanitize fresh-tail)})))
  (let [serialized (envelope/envelope->edn envelope)
        sep (or separator "\n")
        new-context (str serialized sep fresh-tail)]
    ;; Verify the result is EDN-safe (the boundary check)
    (when-not (boundary/edn-safe? new-context)
      (throw (err/error :context/apply-invalid
                        "applied context is not EDN-safe"
                        {:value (err/sanitize new-context)})))
    new-context))

(defn envelope-prefix
  "Return just the serialized envelope prefix (no fresh tail). Useful
  for inspecting what the compressed prefix looks like."
  [envelope]
  (envelope/validate-envelope envelope)
  (envelope/envelope->edn envelope))

(defn applied-context-length
  "The length of the new context after applying `envelope` with
  `fresh-tail`. Does NOT actually build the string — just estimates
  from the envelope's token count and the fresh-tail length."
  [envelope fresh-tail]
  (envelope/validate-envelope envelope)
  (when-not (string? fresh-tail)
    (throw (err/error :context/apply-invalid
                      "fresh-tail must be a string"
                      {:value (err/sanitize fresh-tail)})))
  (+ (count (envelope/envelope->edn envelope))
     1  ; separator
     (count fresh-tail)))