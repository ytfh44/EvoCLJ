(ns evoclj.capability.evolution-tools
  "Tool definitions and lease for the read-only evolution retrieval
  tools (component): :evolution/evidence and :evolution/history.

  The LLM mutator retrieves its context through the tool-calling loop
  instead of prompt-rendered context (roadmap E1). Both tools are
  READ-ONLY views over the kernel's durable evolution data — the
  frozen evidence pack (CAS) and the component rejection history
  (lineage rows) — and both are SUBJECT-BOUND through the capability
  broker: a tool-call intent carries the requesting phenotype's
  attribution (Global Constraint 20), and the broker authorizes it
  against a host-owned lease that binds ONE exact phenotype to the
  tool resource with :actions #{:invoke} (Global Constraint 9 — a
  visible, requestable tool is never itself a grant). The lease
  factory here is the v0 grant set a host mints for the mutator's
  subject.

  The two tools:

  - :evolution/evidence — resolve the frozen evidence pack by
    :evidence/id (its content address) or by :candidate/id (the
    candidates row's :evidence_id) and return the pack's compact
    FIELDS: episode metadata, :excerpt-refs, and the selection
    summary. No trace payload bytes ever cross the boundary (Global
    Constraint 21 — the pack itself holds only compact metadata and
    refs). An unknown :candidate/id or a missing pack artifact
    resolves to {:found false ...} — a value, never a crash.
  - :evolution/history — the rejection-history window for a
    generation lineage via evoclj.evolution.history/recent-mutation-
    history: newest proposal first, bounded by :limit (default 50,
    maximum 500 — the same bound the history module enforces).

  Both providers are constructed closed over the executor :stores map
  {:sqlite <db> :cas <CAS root>} — the store handle never crosses the
  protocol boundary (only describe / normalize-request /
  execute-request! data do, Global Constraint 22) and both descriptors
  declare :effect :pure: the ONLY effects are reads (a SELECT and
  cas/get-bytes). The dispatcher's idempotency gate therefore never
  demands an idempotency key for these tools.

  `mutator-tool-catalog` is the wire form the scheduler's tool loop
  consumes ({:name :description :parameters :tool}); the LLM mutator
  exposes it as its tool catalog so a host :model-call closure that
  implements the tool-calling loop can declare the tools and execute
  each requested call through the broker (the adapter itself holds no
  broker — Global Constraint 8)."
  (:require [clojure.edn :as edn]
            [evoclj.capability.mint :as cap-mint]
            [evoclj.evolution.history :as history]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite]
            [malli.core :as m])
  (:import (java.nio.charset StandardCharsets)
           (java.time Duration Instant)
           (java.util Date UUID)))

;; --- the tool ids -----------------------------------------------------------

(def evidence-tool-id
  "The broker tool id of the read-only evidence retrieval tool."
  :evolution/evidence)

(def history-tool-id
  "The broker tool id of the read-only history retrieval tool."
  :evolution/history)

;; --- window bounds (component contract) --------------------------------------

(def default-history-window
  "The default rejection-history window (component interface:
  {:limit 50})."
  50)

(def max-history-window
  "The hard cap on the history window: the tool's input gate and the
  history module both enforce it, so the window stays bounded."
  500)

;; --- input / output schemas --------------------------------------------------

(def EvidenceArgsSchema
  "The :evolution/evidence args contract (closed): exactly the
  :evidence/id (a canonical ArtifactId) or the :candidate/id (a uuid)
  of the candidate whose frozen pack is resolved — at least one."
  [:and
   [:map {:closed true}
    [:evidence/id {:optional true} [:fn types/artifact-id?]]
    [:candidate/id {:optional true} uuid?]]
   [:fn (fn [args]
          (or (contains? args :evidence/id)
              (contains? args :candidate/id)))]])

(def HistoryArgsSchema
  "The :evolution/history args contract (closed): a non-empty
  generation lineage (current generation first) and an optional
  :limit within [1, max-history-window]."
  [:map {:closed true}
   [:generation-lineage [:vector {:min 1} string?]]
   [:limit {:optional true} [:int {:min 1 :max max-history-window}]]])

(def EvidenceOutputSchema
  "The :evolution/evidence result: the frozen pack's compact fields,
  or the typed not-found shape. Open so future pack fields (e.g. the
  E5 :usage enrichment) validate without a schema change."
  [:map {:closed false}
   [:evidence/id {:optional true} [:fn types/artifact-id?]]
   [:generation/id {:optional true} string?]
   [:cutoff-event-id {:optional true} pos-int?]
   [:episodes {:optional true} vector?]
   [:summary {:optional true} :map]
   [:found {:optional true} boolean?]
   [:reason {:optional true} keyword?]])

(def HistoryOutputSchema
  "The :evolution/history result: a vector of component history entry
  summaries."
  [:vector :map])

;; --- descriptors ------------------------------------------------------------

(def evidence-tool-descriptor
  "The v0 tool descriptor of :evolution/evidence. :effect :pure — the
  tool only reads; the dispatcher's idempotency gate never demands a
  key for it."
  {:tool/id evidence-tool-id
   :effect :pure
   :input-schema EvidenceArgsSchema
   :output-schema EvidenceOutputSchema
   :required-action :invoke})

(def history-tool-descriptor
  "The v0 tool descriptor of :evolution/history. :effect :pure — the
  tool only reads."
  {:tool/id history-tool-id
   :effect :pure
   :input-schema HistoryArgsSchema
   :output-schema HistoryOutputSchema
   :required-action :invoke})

;; --- provider helpers -------------------------------------------------------

(defn- tool-args
  "Extract the :args map from a tool-call intent payload — the same
  contract as evoclj.provider.fixture/intent-args. A payload that is
  not a map or carries no :args is malformed and rejected with
  :provider/input-invalid before anything is normalized."
  [intent]
  (let [payload (:payload intent)]
    (when-not (and (map? payload) (contains? payload :args))
      (throw (err/error :provider/input-invalid
                        "tool-call payload must carry an :args map"
                        {:value (err/sanitize payload)})))
    (:args payload)))

(defn- validate-args!
  "The shared args gate: EDN-safety first (Global Constraint 22),
  then the tool's input schema. Throws :provider/input-invalid on any
  failure, carrying a fully serializable Malli explanation."
  [descriptor args]
  (when-not (boundary/edn-safe? args)
    (throw (err/error :provider/input-invalid
                      "tool input must be plain EDN-safe data (Global Constraint 22)"
                      {:tool/id (:tool/id descriptor)
                       :value (err/sanitize args)})))
  (when-not (m/validate (:input-schema descriptor) args)
    (throw (err/error :provider/input-invalid
                      "tool input failed input-schema validation"
                      {:tool/id (:tool/id descriptor)
                       :value (err/sanitize args)
                       :explanation (err/sanitize
                                     (m/explain (:input-schema descriptor) args))}))))

(defn- expect-args!
  "execute-request! requires the normalized request carrying :args; a
  missing :args is a host-side bug (:provider/request-invalid) — the
  dispatcher always passes the normalize-request output."
  [authorized-request]
  (when-not (and (map? authorized-request) (contains? authorized-request :args))
    (throw (err/error :provider/request-invalid
                      "execute-request! requires a normalized request"
                      {:value (err/sanitize authorized-request)}))))

(defn- evidence-id-of
  "The frozen pack ArtifactId the validated args name: the
  :evidence/id itself, or the :evidence_id of the candidates row named
  by :candidate/id. nil when no such candidate exists — the read then
  resolves to the typed {:found false :reason :candidate-not-found}
  value, never a crash."
  [store args]
  (if-let [id (:evidence/id args)]
    id
    (let [row (first (sqlite/query (:sqlite store)
                                   ["SELECT evidence_id FROM candidates
                                     WHERE id = ?"
                                    (str (:candidate/id args))]))]
      (:evidence_id row))))

;; --- the providers ----------------------------------------------------------

(defn evidence-provider
  "Build the kernel-owned READ-ONLY :evolution/evidence provider (component).

  `store` is the executor :stores map {:sqlite <db> :cas <CAS root>}
  the provider CLOSES OVER — the store handle never crosses the
  protocol boundary. normalize-request validates the args against
  EvidenceArgsSchema (:provider/input-invalid on failure) and returns
  the canonical resource {:kind :tool :id :evolution/evidence} that
  authorization is decided on (Global Constraint 9 — coverage is
  decided on the canonical form, never on raw user input).
  execute-request! resolves the frozen pack body from the CAS under
  the pack's content address (by :evidence/id, or by :candidate/id
  through the candidates row) and returns the pack's compact FIELDS
  with :evidence/id re-attached from the requested address. The only
  effects are a SELECT and cas/get-bytes; :effect :pure."
  [store]
  (reify proto/Provider
    (describe [_] evidence-tool-descriptor)
    (normalize-request [_ intent]
      (let [args (tool-args intent)
            _ (validate-args! evidence-tool-descriptor args)]
        {:tool/id evidence-tool-id
         :resource {:kind :tool :id evidence-tool-id}
         :args args}))
    (execute-request! [_ authorized-request]
      (expect-args! authorized-request)
      (let [args (:args authorized-request)
            id (evidence-id-of store args)]
        (if-not id
          {:found false
           :reason :candidate-not-found
           :candidate/id (:candidate/id args)}
          (try
            (let [bytes (cas/get-bytes (:cas store) id)
                  pack (edn/read-string (String. bytes StandardCharsets/UTF_8))]
              (assoc pack :evidence/id id))
            (catch clojure.lang.ExceptionInfo e
              (if (= :store/cas-missing (:error/type (ex-data e)))
                {:found false
                 :reason :evidence-not-found
                 :evidence/id id}
                (throw e)))))))))

(defn history-provider
  "Build the kernel-owned READ-ONLY :evolution/history provider (component).

  `store` is the executor :stores map the provider CLOSES OVER.
  normalize-request validates the args against HistoryArgsSchema
  (:provider/input-invalid on a window over the 500 cap or a malformed
  lineage) and returns the canonical resource {:kind :tool :id
  :evolution/history}. execute-request! delegates to
  evoclj.evolution.history/recent-mutation-history with the tool's own
  default window (50) when :limit is absent — the rejection-history
  window, newest proposal first, with verdicts, reasons, and metric
  deltas. The only effect is a read; :effect :pure."
  [store]
  (reify proto/Provider
    (describe [_] history-tool-descriptor)
    (normalize-request [_ intent]
      (let [args (tool-args intent)
            _ (validate-args! history-tool-descriptor args)]
        {:tool/id history-tool-id
         :resource {:kind :tool :id history-tool-id}
         :args args}))
    (execute-request! [_ authorized-request]
      (expect-args! authorized-request)
      (let [args (:args authorized-request)]
        (history/recent-mutation-history
         store
         (:generation-lineage args)
         {:limit (or (:limit args) default-history-window)})))))

;; --- the mutator's tool catalog (wire form) ----------------------------------

(def evidence-tool-catalog-entry
  "The wire declaration of :evolution/evidence for the model and the
  tool loop: {:name :description :parameters :tool} — :tool maps the
  wire function name back to the EvoCLJ tool id the scheduler executes
  through the broker."
  {:name "evolution_evidence"
   :description (str "Read-only retrieval of a frozen evolution "
                     "evidence pack: pass :evidence/id (sha256:...) or "
                     ":candidate/id (uuid) to get the pack's compact "
                     "fields (episode metadata, excerpt refs, selection "
                     "summary). Never mutates state.")
   :parameters {:type "object"
                :properties
                {:evidence/id {:type "string"
                               :description "the frozen evidence pack ArtifactId (sha256:<64 hex>)"}
                 :candidate/id {:type "string"
                                :description "a candidate uuid whose evidence pack is resolved"}}
                :required []}
   :tool evidence-tool-id})

(def history-tool-catalog-entry
  "The wire declaration of :evolution/history for the model and the
  tool loop."
  {:name "evolution_history"
   :description (str "Read-only retrieval of the recent mutation/"
                     "rejection history window for a generation "
                     "lineage: pass :generation-lineage (vector of "
                     "generation ids, current first) and optionally "
                     ":limit (default 50, max 500). Never mutates "
                     "state.")
   :parameters {:type "object"
                :properties
                {:generation-lineage {:type "array"
                                      :items {:type "string"}
                                      :description "generation lineage, current generation first"}
                 :limit {:type "integer"
                         :minimum 1
                         :maximum max-history-window
                         :description "window size (default 50, max 500)"}}
                :required ["generation-lineage"]}
   :tool history-tool-id})

(def mutator-tool-catalog
  "The tool catalog the LLM mutator declares: the two READ-ONLY
  evolution retrieval tools, in the wire form the scheduler's tool
  loop consumes ({:name :description :parameters :tool}). A host
  :model-call closure that implements the tool-calling loop moves this
  catalog into the model-call payload :tools and executes each
  requested call through the capability broker (the mutator adapter
  itself holds no broker — Global Constraint 8)."
  [evidence-tool-catalog-entry history-tool-catalog-entry])
;; --- the subject-bound lease --------------------------------------------------
(defn evolution-tool-lease
  "Mint one v0 CapabilityLease (component) binding ONE session+phenotype pair
  to ONE evolution retrieval tool (:evolution/evidence or
  :evolution/history) with :actions #{:invoke} — the grant the broker
  authorizes a tool-call against. Subject matching is dual-anchor
  (P3, [W-01]): BOTH :session/id and :phenotype/id must match exactly,
  so a sibling session from the same Genome+phenotype is a different
  subject and never matches (Global Constraint 9).

  Required: phenotype-id (sha256 string), tool-id (keyword).
  Optional opts: :cap-id (default a fresh uuid), :issued-at (default
  now), :expires-at (default one hour after :issued-at), :constraints
  (default {}), :registry (optional LeaseRegistry atom), :session/id
  (required for dual-anchor — the owning session's UUID/string).
  Delegates to evoclj.capability.mint/mint-lease! (P2 single issuance
  surface). Throws :capability/schema-invalid when :session/id is missing."
  [phenotype-id tool-id & [opts]]
  (let [registry (:registry opts)
        session-id (or (:session/id opts) (:session-id opts))
        _ (when-not session-id
            (throw (err/error :capability/schema-invalid
                              "evolution-tool-lease requires :session/id in opts (P3 dual-anchor)"
                              {:phenotype/id phenotype-id :tool/id tool-id})))
        cap-id-val (or (get opts (keyword "cap/id")) (:cap-id opts) (UUID/randomUUID))
        issued (or (:issued-at opts) (Date.))
        expires (or (:expires-at opts)
                    (Date/from (.plus (Instant/ofEpochMilli (.getTime ^Date issued))
                                      (Duration/ofHours 1))))]
    (cap-mint/mint-lease! registry
                          {:cap-id cap-id-val
                           :subject {:session/id session-id :phenotype/id phenotype-id}
                           :resource {:kind :tool :id tool-id}
                           :actions #{:invoke}
                           :constraints (or (:constraints opts) {})
                           :issued-at issued
                           :expires-at expires})))


(defn mutator-tool-leases
  "The v0 grant set a host mints for the LLM mutator's subject: both
  evolution retrieval leases for ONE phenotype id, so the mutator can
  retrieve evidence and history through the broker. Optional opts are
  forwarded to evolution-tool-lease (including :registry)."
  [phenotype-id & [opts]]
  (mapv #(evolution-tool-lease phenotype-id % opts)
        [evidence-tool-id history-tool-id]))
