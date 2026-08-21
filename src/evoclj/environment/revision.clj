(ns evoclj.environment.revision
  "Revision value - content identity separate from publication order.

  A Revision is a plain map with:
  - :revision/id   content identity as \"sha256:<64 hex>\" string
  - :revision/seq  monotonic publication order integer
  - :source/id     originating source identifier
  - :captured-at   instant millis when snapshot was taken
  - :payload       raw snapshot payload (opaque, hashed for id)

  :revision/id and :revision/seq are never conflated: id is a content
  hash, seq is a counter. The constructor keeps them distinct and the
  predicate validates the distinction."
  (:require [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types])
  (:import (java.nio.charset StandardCharsets)))

(defn payload->id
  "Derive content identity for payload as sha256:<hex>.
  Strings are hashed with text-digest (CRLF normalized), other values
  via pr-str bytes."
  [payload]
  (if (string? payload)
    (hash/text-digest payload)
    (let [s (pr-str payload)]
      (hash/file-digest (.getBytes s StandardCharsets/UTF_8)))))

(defn make-revision
  "Create a Revision map. Computes :revision/id from payload, stores
  :revision/seq as given monotonic integer. id and seq remain distinct
  keys with distinct types."
  ([source-id payload seq]
   (make-revision source-id payload seq (System/currentTimeMillis)))
  ([source-id payload seq captured-at]
   (when-not (integer? seq)
     (throw (ex-info "seq must be integer" {:seq seq})))
   {:revision/id (payload->id payload)
    :revision/seq (int seq)
    :source/id source-id
    :captured-at captured-at
    :payload payload}))

(defn revision?
  "True when x is a valid Revision map. Validates that id is a
  canonical sha256 string and seq is an integer, and that they are not
  conflated (different types, different keys)."
  [x]
  (and (map? x)
       (string? (:revision/id x))
       (types/artifact-id? (:revision/id x))
       (int? (:revision/seq x))
       (contains? x :source/id)
       (contains? x :captured-at)
       (contains? x :payload)
       ;; id and seq must not be conflated: they have different types
       (not= (type (:revision/id x)) (type (:revision/seq x)))
       (not= (:revision/id x) (str (:revision/seq x)))))

(defn revision-id
  "Return content identity of revision."
  [rev]
  (:revision/id rev))

(defn revision-seq
  "Return publication order of revision."
  [rev]
  (:revision/seq rev))
