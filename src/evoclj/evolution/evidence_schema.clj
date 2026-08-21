(ns evoclj.evolution.evidence-schema
  "Malli schemas for frozen evidence packs (component).

  The schemas here are the trust-boundary contracts of
  evoclj.evolution.evidence: the BuildEvidenceRequest (generation +
  immutable :cutoff-event-id + selector), the frozen EvidencePack
  (content-addressed :evidence/id, compact episode refs, summary), the
  per-episode ref carried in the pack (compact metadata + a CAS
  :excerpt-ref), the CAS trace-excerpt artifact (which preserves
  episode provenance), and the Episode contract the selector reads
  from the store (identical to the Episode contract of
  evoclj.runtime.episode, whose private copy lives there).

  Selector semantics (normative for this task):
    :recent n            — the n most recent eligible episodes
    :include-successes n — up to n most recent success episodes,
                           drawn from ALL eligible episodes (a
                           representation backfill when the recent
                           pool is failure-skewed)
    :include-failures n  — up to n most recent failure episodes
    :include-high-cost n — up to n most recent high-cost episodes
    :seed (optional)     — deterministic tie-break key for episodes
                           of equal recency; persisted in the pack's
                           :summary when supplied (the seed policy:
                           any randomness is seeded and the seed lives
                           IN the pack)

  USAGE ENRICHMENT (roadmap E5): :usage is OPTIONAL on the Episode
  contract and on the pack's episode refs. When present it must be a
  map of keyword → number — the model-call channel's token counts and
  cost estimate (component counters) — and non-numeric usage is
  rejected at the trust boundary. Unknown usage is ABSENT, never
  fabricated as zeros (honest accounting).

  Closed maps everywhere: unknown keys at a trust boundary are
  rejected (:evidence/request-invalid, :evidence/pack-invalid,
  :evidence/excerpt-invalid) with a humanized Malli explanation."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]))

(defn- schema-error!
  "Throw a typed error carrying a humanized Malli explanation."
  [error-type kind expl]
  (throw (err/error error-type
                    (str kind " does not satisfy the evidence contract")
                    {:errors (me/humanize expl)})))

(def UsageSchema
  "The model usage of one episode: numeric counters from the
  model-call channel (component) — token counts (:model-input-tokens,
  :model-output-tokens) and a cost estimate (:model-cost-units,
  :total-cost, :cost). Every value MUST be numeric: usage is
  accounting data, so a non-numeric value is rejected at the trust
  boundary (honest accounting — unknown usage is ABSENT, never
  fabricated as zeros)."
  [:map-of keyword? number?])

(def SelectorSchema
  "The pack selector: representation quotas over the eligible episode
  set. :seed is optional and, when present, participates as a
  deterministic tie-break for episodes of equal recency."
  [:map {:closed true}
   [:recent pos-int?]
   [:include-successes pos-int?]
   [:include-failures pos-int?]
   [:include-high-cost pos-int?]
   [:seed {:optional true} int?]])

(def BuildEvidenceRequestSchema
  "The build-evidence-pack input contract. :cutoff-event-id is the
  immutable evidence boundary: an episode enters the pack iff its
  :trace :last-event ≤ :cutoff-event-id (inclusive)."
  [:map {:closed true}
   [:generation/id string?]
   [:cutoff-event-id pos-int?]
   [:selector SelectorSchema]])

(def EpisodeSchema
  "The Episode contract the selector consumes from the store — the
  same shape evoclj.runtime.episode/materialize-episode! produces (the
  plan's `Episode`)."
  [:map {:closed true}
   [:episode/id uuid?]
   [:session/id uuid?]
   [:generation/id string?]
   [:genome/id [:fn types/genome-id?]]
   [:resolution/id [:fn types/resolution-id?]]
   [:task-ref [:fn types/artifact-id?]]
   [:trace [:map {:closed true}
            [:first-event int?]
            [:last-event int?]]]
   [:outcome map?]
   [:usage {:optional true} UsageSchema]])

(def EpisodeRefSchema
  "One entry of the pack's :episodes vector: COMPACT metadata (id,
  provenance, outcome, trace bounds, usage) plus the :excerpt-ref —
  the CAS content address of the episode's full trace excerpt. Large
  payload bodies never cross into the pack itself (Global Constraint
  21)."
  [:map {:closed true}
   [:episode/id uuid?]
   [:session/id uuid?]
   [:generation/id string?]
   [:excerpt-ref [:fn types/artifact-id?]]
   [:outcome map?]
   [:trace [:map {:closed true}
            [:first-event int?]
            [:last-event int?]]]
   [:usage {:optional true} UsageSchema]])

(def EvidencePackSchema
  "The frozen evidence pack returned by build-evidence-pack. The
  :evidence/id is a content hash over the pack data computed with the
  deterministic conventions of evoclj.genome.hash; :summary carries
  the selector, the persisted seed, and the selection counts."
  [:map {:closed true}
   [:evidence/id [:fn types/artifact-id?]]
   [:generation/id string?]
   [:cutoff-event-id pos-int?]
   [:episodes [:vector EpisodeRefSchema]]
   [:summary :map]])

(def ExcerptSchema
  "The CAS trace-excerpt artifact behind an :excerpt-ref. It preserves
  the ORIGINAL episode provenance (:episode/id, :session/id,
  :generation/id, :trace) alongside the full event trace of the
  episode's causal range."
  [:map {:closed true}
   [:excerpt/version pos-int?]
   [:episode/id uuid?]
   [:session/id uuid?]
   [:generation/id string?]
   [:trace [:map {:closed true}
            [:first-event int?]
            [:last-event int?]]]
   [:events [:vector :map]]])

(defn validate-request
  "Validate a build-evidence-pack request. Returns the request
  unchanged, or throws :evidence/request-invalid."
  [request]
  (if-let [expl (m/explain BuildEvidenceRequestSchema request)]
    (schema-error! :evidence/request-invalid "build-evidence-pack request" expl)
    request))

(defn validate-episode
  "Validate an Episode read from the store. Returns the episode
  unchanged, or throws :evidence/episode-invalid."
  [episode]
  (if-let [expl (m/explain EpisodeSchema episode)]
    (schema-error! :evidence/episode-invalid "episode" expl)
    episode))

(defn validate-pack
  "Validate a frozen evidence pack. Returns the pack unchanged, or
  throws :evidence/pack-invalid."
  [pack]
  (if-let [expl (m/explain EvidencePackSchema pack)]
    (schema-error! :evidence/pack-invalid "evidence pack" expl)
    pack))

(defn validate-excerpt
  "Validate a trace-excerpt artifact before it is stored in the CAS.
  Returns the excerpt unchanged, or throws :evidence/excerpt-invalid."
  [excerpt]
  (if-let [expl (m/explain ExcerptSchema excerpt)]
    (schema-error! :evidence/excerpt-invalid "trace excerpt" expl)
    excerpt))
