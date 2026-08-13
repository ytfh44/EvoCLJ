(ns evoclj.evolution.diagnosis-schema
  "Malli schemas for structured diagnosis (Task 7.2).

  The schemas here are the trust-boundary contracts of the diagnosis
  path (evoclj.evolution.diagnose): the Diagnosis artifact
  (content-addressed :diagnosis/id + :evidence/id provenance +
  bounded hypotheses), the Hypothesis contract, and the deterministic
  pattern adapter's constructor config.

  Normative shape (the plan's Task 7.2 interface):

      {:diagnosis/id \"sha256:...\"
       :evidence/id \"sha256:...\"
       :hypotheses
       [{:hypothesis/id #uuid
         :pattern :premature-tool-mutation
         :claim \"...\"
         :support [{:episode/id ... :event-ids [...]}]
         :counterevidence [{:episode/id ...}]
         :target {:kind :skill :id :debugging}
         :expected-effect {:metric :task/success :direction :increase}
         :confidence-band :medium}]}

  Required by the task: :support, :target, and :expected-effect are
  REQUIRED hypothesis keys. A hypothesis with ZERO evidence references
  — an empty :support vector, or a support entry with an empty
  :event-ids vector — is REJECTED: an unsupported hypothesis is not a
  hypothesis (Step 2). Closed maps everywhere: unknown keys at a trust
  boundary are rejected (:diagnosis/hypothesis-invalid,
  :diagnosis/invalid, :diagnosis/config-invalid) with a humanized
  Malli explanation."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(defn- schema-error!
  "Throw a typed error carrying a humanized Malli explanation."
  [error-type kind expl]
  (throw (err/error error-type
                    (str kind " does not satisfy the diagnosis contract")
                    {:errors (me/humanize expl)})))

(def ExpectedEffectSchema
  "The expected effect of acting on a hypothesis: a metric plus the
  direction a mutation addressing it should move that metric."
  [:map {:closed true}
   [:metric keyword?]
   [:direction [:enum :increase :decrease]]])

(def TargetSchema
  "What a mutation addressing this hypothesis would act on."
  [:map {:closed true}
   [:kind [:enum :skill :workflow]]
   [:id keyword?]])

(def SupportRefSchema
  "One support entry: an episode ref PLUS the event ids within that
  episode that substantiate the hypothesis. :event-ids must be
  non-empty — a support entry that cites an episode but zero events
  carries zero evidence."
  [:map {:closed true}
   [:episode/id uuid?]
   [:event-ids [:vector {:min 1} pos-int?]]])

(def CounterevidenceRefSchema
  "One counterevidence entry: an episode ref that weighs against the
  hypothesis (no event ids — the interface shows only the episode)."
  [:map {:closed true}
   [:episode/id uuid?]])

(def HypothesisSchema
  "A structured hypothesis. :support, :target, and :expected-effect
  are REQUIRED; :support must cite at least one episode AND at least
  one event id per entry (zero-evidence hypotheses are rejected)."
  [:map {:closed true}
   [:hypothesis/id uuid?]
   [:pattern keyword?]
   [:claim string?]
   [:support [:vector {:min 1} SupportRefSchema]]
   [:counterevidence [:vector CounterevidenceRefSchema]]
   [:target TargetSchema]
   [:expected-effect ExpectedEffectSchema]
   [:confidence-band [:enum :low :medium :high]]])

(def DiagnosisSchema
  "The Diagnosis artifact: a content-addressed :diagnosis/id, the
  :evidence/id provenance of the frozen evidence pack it was derived
  from, and the bounded :hypotheses vector."
  [:map {:closed true}
   [:diagnosis/id [:fn types/artifact-id?]]
   [:evidence/id [:fn types/artifact-id?]]
   [:hypotheses [:vector HypothesisSchema]]])

(def PatternDiagnosticianConfigSchema
  "The deterministic pattern adapter's constructor config — plain
  data ONLY (Global Constraint 11: the adapter must receive
  Evolution-set evidence and nothing else). The map is CLOSED, so no
  store handle, Selection/Audit fixture handle, or any other unknown
  key can be smuggled in; the adapter sees exactly the evidence pack
  it is handed."
  [:map {:closed true}
   [:task/success-threshold {:optional true} number?]
   [:max-hypotheses {:optional true} pos-int?]
   [:confidence-band {:optional true} [:enum :low :medium :high]]])

(defn validate-config
  "Validate a pattern-adapter config map. Returns it unchanged, or
  throws :diagnosis/config-invalid."
  [config]
  (if-let [expl (m/explain PatternDiagnosticianConfigSchema config)]
    (schema-error! :diagnosis/config-invalid "diagnostician config" expl)
    config))

(defn validate-hypothesis
  "Validate a hypothesis. Returns it unchanged, or throws
  :diagnosis/hypothesis-invalid."
  [hypothesis]
  (if-let [expl (m/explain HypothesisSchema hypothesis)]
    (schema-error! :diagnosis/hypothesis-invalid "hypothesis" expl)
    hypothesis))

(defn validate-diagnosis
  "Validate a Diagnosis artifact. Returns it unchanged, or throws
  :diagnosis/invalid."
  [diagnosis]
  (if-let [expl (m/explain DiagnosisSchema diagnosis)]
    (schema-error! :diagnosis/invalid "diagnosis" expl)
    diagnosis))
