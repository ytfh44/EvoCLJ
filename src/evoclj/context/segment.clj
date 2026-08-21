(ns evoclj.context.segment
  "ContextSegment — materialized content injected into a model request.

  A Segment is the concrete text (e.g., SKILL.md) fetched from the
  immutable artifact/tree via CAS for a ContextBinding. It is not
  derived from the current catalog projection.

  Shape:
    {:segment/logical-id [:skill \"debugging\"]
     :segment/revision-id \"sha256:…\"
     :segment/bundle-id \"bundle:…\"
     :segment/content \"# SKILL.md text …\"
     :segment/source :cas               ; always :cas in v0
     :segment/activated-at <long>}"
  (:require [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(defn segment?
  [x]
  (and (map? x)
       (vector? (:segment/logical-id x))
       (string? (:segment/revision-id x))
       (types/artifact-id? (:segment/revision-id x))
       (string? (:segment/bundle-id x))
       (string? (:segment/content x))
       (= :cas (:segment/source x))))

(defn make-segment
  "Create a ContextSegment from a binding and content string."
  [{:keys [logical-id revision-id bundle-id content activated-at source]}]
  (when-not (vector? logical-id)
    (throw (err/error :context/segment-invalid "logical-id must be vector" {:logical-id logical-id})))
  (when-not (types/artifact-id? revision-id)
    (throw (err/error :context/segment-invalid "revision-id must be sha256" {:revision-id revision-id})))
  (when-not (and (string? bundle-id) (seq bundle-id))
    (throw (err/error :context/segment-invalid "bundle-id must be non-empty string" {:bundle-id bundle-id})))
  (when-not (string? content)
    (throw (err/error :context/segment-invalid "content must be string" {:content content})))
  {:segment/logical-id logical-id
   :segment/revision-id revision-id
   :segment/bundle-id bundle-id
   :segment/content content
   :segment/source (or source :cas)
   :segment/activated-at (or activated-at (System/currentTimeMillis))})

(defn segment-from-binding
  "Build a Segment skeleton from a binding and resolved content.
  Content is the actual SKILL.md text fetched via CAS."
  [binding content]
  (make-segment {:logical-id (:logical/id binding)
                 :revision-id (:revision/id binding)
                 :bundle-id (:bundle/id binding)
                 :content content
                 :activated-at (:binding/activated-at binding)}))
