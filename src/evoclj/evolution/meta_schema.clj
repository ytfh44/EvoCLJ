(ns evoclj.evolution.meta-schema
  "Malli schemas for meta-evolution parameters (S3-2)."
  (:require [malli.core :as m]))

;; --- parameter schemas -------------------------------------------------------

(def PromptParamSchema
  "A prompt parameter that can be evolved."
  [:map {:closed true}
   [:prompt/type [:enum :mutator :diagnostician :judge]]
   [:prompt/text string?]
   [:prompt/version {:optional true} pos-int?]])

(def WeightParamSchema
  "A weight parameter that can be evolved."
  [:map {:closed true}
   [:weight/name keyword?]
   [:weight/value double?]
   [:weight/min {:optional true} double?]
   [:weight/max {:optional true} double?]])

(def PolicyParamSchema
  "A policy parameter that can be evolved."
  [:map {:closed true}
   [:policy/name keyword?]
   [:policy/key keyword?]
   [:policy/value any?]])

(def MetaParameterSchema
  "Union of evolvable parameter types."
  [:or PromptParamSchema WeightParamSchema PolicyParamSchema])

(def MetaGenomeSchema
  "A meta-genome: a collection of evolvable parameters with fitness."
  [:map {:closed true}
   [:meta/params [:vector MetaParameterSchema]]
   [:meta/fitness {:optional true} double?]
   [:meta/generation-id {:optional true} string?]])

;; --- validation --------------------------------------------------------------

(defn validate-meta-genome
  "Validate x as a MetaGenome, returning it unchanged or throwing
  :evolution/meta-invalid with a Malli explanation."
  [x]
  (if (m/validate MetaGenomeSchema x)
    x
    (throw (ex-info "meta-genome does not satisfy schema"
                    {:error/type :evolution/meta-invalid
                     :explanation (m/explain MetaGenomeSchema x)}))))
