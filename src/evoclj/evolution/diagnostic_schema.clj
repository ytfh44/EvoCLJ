(ns evoclj.evolution.diagnostic-schema
  "Malli contract for diagnostic bundles.

  A DiagnosticBundle is typed evidence produced for one subject revision.
  It keeps evidence and diagnosis provenance together without introducing
  an ObservationSurface or execution authority."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(defn- schema-error!
  "Throw a typed error carrying a humanized Malli explanation."
  [expl]
  (throw (err/error :diagnostic/bundle-invalid
                    "diagnostic bundle does not satisfy the contract"
                    {:errors (me/humanize expl)})))

(def ProducerSchema
  "The producer identity for one diagnostic capture."
  [:map {:closed true}
   [:kind keyword?]
   [:version string?]])

(def LocationSchema
  "A source location attached to one diagnostic finding."
  [:map {:closed true}
   [:path string?]
   [:line {:optional true} pos-int?]
   [:column {:optional true} pos-int?]])

(def FindingSchema
  "One structured diagnostic finding."
  [:map {:closed true}
   [:rule/id string?]
   [:severity [:enum :error :warning :info]]
   [:location LocationSchema]
   [:message string?]
   [:fixable? {:optional true} boolean?]])

(def SubjectSchema
  "The immutable subject identity observed by the producer."
  [:map {:closed true}
   [:artifact/revision [:fn types/artifact-id?]]
   [:workspace/id string?]
   [:snapshot/id {:optional true} [:fn types/artifact-id?]]])

(def DiagnosticBundleSchema
  "A content-addressed diagnostic evidence bundle.

  The subject revision is part of the bundle identity. Evidence and
  diagnosis remain explicit artifact references; freshness is decided by
  a later projection against the current workspace snapshot."
  [:map {:closed true}
   [:diagnostic/id [:fn types/artifact-id?]]
   [:producer ProducerSchema]
   [:subject SubjectSchema]
   [:evidence/id [:fn types/artifact-id?]]
   [:diagnosis/id [:fn types/artifact-id?]]
   [:findings [:vector FindingSchema]]
   [:exit-status int?]
   [:status [:enum :complete :partial :failed :suppressed]]
   [:captured-at inst?]])

(defn validate-bundle
  "Validate a DiagnosticBundle, returning it unchanged on success."
  [bundle]
  (if-let [expl (m/explain DiagnosticBundleSchema bundle)]
    (schema-error! expl)
    bundle))
