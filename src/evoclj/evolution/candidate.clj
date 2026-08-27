(ns evoclj.evolution.candidate
  "Candidate records — creation, persistence, and the uniqueness rule
  (component). NO ACTIVATION RIGHTS: this namespace has no function
  that reads or writes the generations CURRENT pointer and no
  dependency on any promotion/current namespace (Global Constraint 15
  keeps promotion a separate subsystem; component owns CURRENT).

  THE CANDIDATE RECORD (docs 'Detailed Public Data Contracts'):

      {:candidate/id uuid?
       :parent/generation-id stable-id?
       :parent/genome-id GenomeId
       :candidate/genome-id GenomeId
       :mutation/id uuid?
       :evidence/id ArtifactId
       :risk keyword?
       :state keyword?
       :created-at inst?}

  STATE MACHINE (normative, component):

      :proposed → :materialized → :evaluation-pending → :evaluated
                                            └────→ :invalid

  This task implements the :proposed → :materialized →
  :evaluation-pending fragment: create-candidate produces :proposed
  records; materialize-candidate! is where :proposed → :materialized
  happens (a row is created AT materialization — 'failure before
  candidate row means no valid Candidate exists'); the only persisted
  transition in this task is :materialized → :evaluation-pending
  (transition-candidate! / mark-evaluation-pending!). The :evaluated
  and :invalid transitions arrive with the evaluator in M8, and the
  :canary/:promoted/:rejected/:stale deployment states with
  Promotion in M9.

  STATE VOCABULARY DEVIATION (documented, per Repo Convention 5): the
  component candidates.state CHECK constraint was written before the
  state machine and only admits ('materialized','evaluating',
  'eligible','promoted','rejected','stale'). The machine states are
  mapped at the row boundary: :materialized ↔ 'materialized',
  :evaluation-pending ↔ 'evaluating', :evaluated ↔ 'eligible' (M8),
  :promoted/:rejected/:stale ↔ themselves (M9). :proposed is a
  pre-persistence state with NO schema value — a candidate row only
  ever exists from :materialized onward. :invalid has no 5.1 value;
  M8 must resolve that transition's persistence.

  THE UNIQUENESS RULE (Step 3, normative): a candidate is unique by
  (parent-genome-id, mutation-hash). mutation-hash is the
  deterministic content hash of the Mutation IR EXCLUDING
  :mutation/id — the uuid is proposal-assignment metadata, not
  content — so the same parent + the same mutation CONTENT
  materialized twice (even as two proposals with different uuids)
  dedupes to the SAME auditable candidate row, while different
  content or a different parent yields a separate candidate. Both
  proposals remain durable rows in the mutations table (Global
  Constraint 16: rejected mutations stay queryable negative evidence);
  only the CANDIDATE dedupes. The 5.1 schema has no unique index for
  this rule, so the dedup is enforced at the application layer: the
  mutation row is written FIRST inside the materialization
  transaction (its INSERT takes SQLite's write lock, serializing
  concurrent materializations of the same parent+mutation), then the
  dedup lookup runs, then the candidate row is inserted — so a
  concurrent duplicate observes the first committed row and dedupes.

  MATERIALIZATION (persistence, Step 4): materialize-candidate!
  writes the candidate row into the component candidates table with
  full lineage (composite FK (parent_generation_id, parent_genome_id)
  → generations, enforcing Database Invariant 8; mutation_id FK →
  mutations). As the FK's lineage precondition, the mutation row is
  ensured (INSERT OR IGNORE by :mutation/id) in the same transaction
  — the mutations table is append-only proposals (Global Constraint
  4-6) and its rows are immutable once written. The candidate record
  must AGREE with the mutation it materializes (same :mutation/id,
  :parent/genome-id, :evidence/id, :risk); a record that disagrees
  with its own lineage is rejected. The record stores only
  references (genome/evidence/mutation ids); the candidate Genome
  BODY (component patch output) is put into the CAS by the
  orchestrator, not by this namespace.

  `store` is a CandidateStore handle
  (evoclj.store.candidate-store/make-candidate-store) — the narrow
  opaque authority that alone may do jdbc on candidates/mutations.
  Passing a raw {:sqlite ... :cas ...} map is rejected with
  :candidate/store-invalid.

  NO ACTIVATION RIGHTS: this namespace still has no function that
  reads or writes the generations CURRENT pointer and no dependency on
  any promotion/current namespace.

  Error contract (Global Constraint 22 — plain serializable data):
  :candidate/store-invalid (:reason :not-a-candidate-store),
  :candidate/invalid (contract violation, Malli
  explanations), :candidate/mutation-invalid,
  :candidate/mutation-mismatch, :candidate/parent-mismatch,
  :candidate/evidence-mismatch, :candidate/risk-mismatch,
  :candidate/not-proposed, :candidate/not-found,
  :candidate/invalid-transition."
  (:require [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.schema :as gschema]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.candidate-store :as candidate-store])
  (:import (java.util Date UUID)))

;; --- state machine (component fragment; M8 adds :evaluated/:invalid) ---------

(def states
  "The component candidate states. :proposed is the pre-persistence
  record state; :materialized and :evaluation-pending are persisted.
  The :evaluated and :invalid states arrive with the evaluator in
  M8."
  #{:proposed :materialized :evaluation-pending})

(def transitions
  "State machine edges. :proposed → :materialized is realized by
  materialize-candidate! (a row is created at materialization), not
  by transition-candidate!, which operates on persisted rows only.
  The only persisted edge in this task is :materialized →
  :evaluation-pending. M8 appends :evaluation-pending → #{:evaluated
  :invalid}; M9 appends the deployment states."
  {:proposed #{:materialized}
   :materialized #{:evaluation-pending}})

(declare find-candidate)

;; --- the public Candidate contract (docs 'Detailed Public Data Contracts') ---

(def CandidateSchema
  "The public Candidate record contract map returned by
  create-candidate / materialize-candidate! / find-candidate."
  [:map {:closed true}
   [:candidate/id uuid?]
   [:parent/generation-id string?]
   [:parent/genome-id [:fn types/genome-id?]]
   [:candidate/genome-id [:fn types/genome-id?]]
   [:mutation/id uuid?]
   [:evidence/id [:fn types/artifact-id?]]
   [:risk gschema/RiskClassSchema]
   [:state keyword?]
   [:created-at [:fn inst?]]])

(def CreateCandidateRequest
  "The create-candidate input contract (closed): the identity and
  provenance a :proposed candidate is created from. :created-at is
  optional and defaults to now."
  [:map {:closed true}
   [:parent/generation-id string?]
   [:parent/genome-id [:fn types/genome-id?]]
   [:candidate/genome-id [:fn types/genome-id?]]
   [:mutation/id uuid?]
   [:evidence/id [:fn types/artifact-id?]]
   [:risk gschema/RiskClassSchema]
   [:created-at {:optional true} [:fn inst?]]])

;; --- boundary validation ------------------------------------------------------

(defn- schema-error!
  "Throw :candidate/invalid with a humanized Malli explanation."
  [kind expl]
  (throw (err/error :candidate/invalid
                    (str kind " does not satisfy the candidate contract")
                    {:errors (me/humanize expl)})))

(defn- validate-create-request
  [request]
  (when-let [expl (m/explain CreateCandidateRequest request)]
    (schema-error! "create-candidate request" expl))
  request)

(defn- validate-candidate!
  [c]
  (when-let [expl (m/explain CandidateSchema c)]
    (schema-error! "candidate" expl))
  c)

(defn- validate-store!
  "Validate that store is a CandidateStore handle. Any map (including
  the legacy {:sqlite ... :cas ...} shape) is rejected with
  :candidate/store-invalid."
  [store]
  (when-not (instance? evoclj.store.candidate_store.CandidateStore store)
    (throw (err/error :candidate/store-invalid
                      "store must be a CandidateStore handle (evoclj.store.candidate-store/make-candidate-store)"
                      {:reason :not-a-candidate-store :value (err/sanitize store)})))
  store)

(defn- validate-mutation-shape!
  "The mutation must be a map carrying every lineage-relevant Mutation
  IR key (:mutation/id :parent/genome-id :hypothesis/id :evidence/id
  :risk :ops :expected-effect). The full IR schema and patch
  preconditions are enforced earlier by the mutator
  (evoclj.evolution.mutation); this gate only guarantees the keys this
  namespace materializes lineage from are present."
  [mutation]
  (when-not (map? mutation)
    (throw (err/error :candidate/mutation-invalid
                      "mutation must be the Mutation IR map"
                      {:value (err/sanitize mutation)})))
  (let [required #{:mutation/id :parent/genome-id :hypothesis/id
                   :evidence/id :risk :ops :expected-effect}
        missing (remove required (keys mutation))]
    (when (seq missing)
      (throw (err/error :candidate/mutation-invalid
                        "mutation is missing Mutation IR keys"
                        {:missing (vec (sort missing))})))
    mutation))

(defn- validate-agreement!
  "The candidate record must agree with the mutation it materializes:
  same mutation id, same parent Genome, same evidence pack, same risk
  class. A record that disagrees with its own lineage would corrupt
  the lineage integrity guarantees (Database Invariant 8, Global
  Constraint 17)."
  [candidate mutation]
  (when (not= (:mutation/id candidate) (:mutation/id mutation))
    (throw (err/error :candidate/mutation-mismatch
                      "candidate :mutation/id disagrees with the mutation being materialized"
                      {:candidate/id (:candidate/id candidate)
                       :candidate/mutation-id (:candidate/id candidate)
                       :mutation-id (:mutation/id mutation)})))
  (when (not= (:parent/genome-id candidate) (:parent/genome-id mutation))
    (throw (err/error :candidate/parent-mismatch
                      "candidate :parent/genome-id disagrees with the mutation's parent"
                      {:candidate/id (:candidate/id candidate)
                       :candidate/parent-genome-id (:parent/genome-id candidate)
                       :mutation/parent-genome-id (:parent/genome-id mutation)})))
  (when (not= (:evidence/id candidate) (:evidence/id mutation))
    (throw (err/error :candidate/evidence-mismatch
                      "candidate :evidence/id disagrees with the mutation's evidence pack"
                      {:candidate/id (:candidate/id candidate)
                       :candidate/evidence-id (:evidence/id candidate)
                       :mutation/evidence-id (:evidence/id mutation)})))
  (when (not= (:risk candidate) (:risk mutation))
    (throw (err/error :candidate/risk-mismatch
                      "candidate :risk disagrees with the mutation's risk class"
                      {:candidate/id (:candidate/id candidate)
                       :candidate/risk (:risk candidate)
                       :mutation/risk (:risk mutation)})))
  candidate)

;; --- Step 1: creation --------------------------------------------------------

(defn create-candidate
  "Create a NEW :proposed Candidate record (component Step 1).

  The record names the parent generation and parent Genome, the
  content-addressed candidate Genome, the mutation and evidence it
  answers, its risk class, a fresh :candidate/id, and :state
  :proposed. Pure — nothing is persisted here;
  materialize-candidate! persists the record (and is where
  :proposed → :materialized happens).

  Typed errors: :candidate/invalid (closed-map contract violation,
  Malli explanations)."
  [request]
  (validate-create-request request)
  (let [c {:candidate/id (UUID/randomUUID)
           :parent/generation-id (:parent/generation-id request)
           :parent/genome-id (:parent/genome-id request)
           :candidate/genome-id (:candidate/genome-id request)
           :mutation/id (:mutation/id request)
           :evidence/id (:evidence/id request)
           :risk (:risk request)
           :state :proposed
           :created-at (or (:created-at request) (Date.))}]
    (validate-candidate! c)
    c))

;; --- Step 3: the uniqueness rule (parent-genome-id, mutation-hash) -----------

(defn mutation-hash
  "The deterministic content hash of a Mutation IR — the uniqueness
  rule's second component (component Step 3).

  Delegates to evoclj.store.candidate-store/mutation-hash (single source)."
  [mutation]
  (candidate-store/mutation-hash mutation))

(defn dedupe-key
  "The normative candidate uniqueness rule (component Step 3): a
  candidate is unique by (parent-genome-id, mutation-hash). The same
  parent + same mutation content materialized twice dedupes to the
  same auditable candidate (one row)."
  [parent-genome-id mutation]
  {:parent/genome-id parent-genome-id
   :mutation/hash (mutation-hash mutation)})

;; --- Step 4: persistence -------------------------------------------------------

(defn materialize-candidate!
  "Materialize the immutable Candidate record for a parent Genome and
  Mutation IR (component Step 4) and return it with :state
  :materialized — or the EXISTING candidate when the uniqueness rule
  (parent-genome-id, mutation-hash) already has a row (Step 3: the
  same parent+mutation materialized twice is ONE auditable candidate).

  The materialization transaction: the mutation row is ensured first
  (INSERT OR IGNORE — its write takes SQLite's write lock, so a
  concurrent materialization of the same parent+mutation serializes
  and observes the first committed row), then the dedup lookup, then
  the candidate row insert. The candidate record must be :proposed
  and must AGREE with the mutation (same :mutation/id,
  :parent/genome-id, :evidence/id, :risk). The row stores only
  references; the candidate Genome body (component patch output) is
  put into the CAS by the orchestrator, not here.

  Fleet R: delegates to evoclj.store.candidate-store/materialize! via
  the narrow CandidateStore handle. Requires a CandidateStore; legacy
  {:sqlite :cas} maps are rejected with :candidate/store-invalid.

  Typed errors: :candidate/store-invalid, :candidate/invalid,
  :candidate/not-proposed, :candidate/mutation-invalid,
  :candidate/mutation-mismatch, :candidate/parent-mismatch,
  :candidate/evidence-mismatch, :candidate/risk-mismatch."
  [store candidate mutation]
  (validate-store! store)
  (validate-candidate! candidate)
  (when-not (= :proposed (:state candidate))
    (throw (err/error :candidate/not-proposed
                      "only a :proposed candidate can be materialized"
                      {:candidate/id (:candidate/id candidate)
                       :state (:state candidate)})))
  (validate-mutation-shape! mutation)
  (validate-agreement! candidate mutation)
  (let [result (candidate-store/materialize! store candidate mutation)]
    (validate-candidate! result)
    result))

(defn transition-candidate!
  "Compare-and-set state transition for a persisted candidate (component Step 4). Changes :state alone via one atomic UPDATE matched on
  the expected state, and returns the updated Candidate record. The
  component machine has one persisted edge: :materialized →
  :evaluation-pending (the :proposed → :materialized edge is realized
  by materialize-candidate!; :evaluated/:invalid arrive with M8).

  Fleet R: delegates to evoclj.store.candidate-store/transition! via
  the narrow CandidateStore handle. Requires a CandidateStore; legacy
  {:sqlite :cas} maps are rejected.

  Typed errors: :candidate/invalid-transition (not an edge, a target
  state with no 5.1 vocabulary value, or the stored state is not
  expected-state — including a concurrent worker that already won the
  compare-and-set), :candidate/not-found, :candidate/invalid."
  [store candidate-id expected-state new-state]
  (validate-store! store)
  (when-not (and (keyword? expected-state) (keyword? new-state))
    (throw (err/error :candidate/invalid-transition
                      "transition states must be keywords"
                      {:expected-state expected-state :new-state new-state})))
  (when-not (contains? (get transitions expected-state) new-state)
    (throw (err/error :candidate/invalid-transition
                      "not a valid transition for the component machine"
                      {:candidate/id (types/session-id candidate-id)
                       :expected-state expected-state
                       :new-state new-state})))
  (let [_ (candidate-store/transition! store candidate-id expected-state new-state)]
    (find-candidate store candidate-id)))

(defn mark-evaluation-pending!
  "Transition a :materialized candidate to :evaluation-pending
  (the only persisted transition in this task). Compare-and-set on
  the stored state."
  [store candidate-id]
  (transition-candidate! store candidate-id :materialized :evaluation-pending))

(defn find-candidate
  "The Candidate record for `candidate-id`, or nil when no candidate
  has that id. Read-only — no activation rights.

  Fleet R: delegates to evoclj.store.candidate-store/find-candidate via
  the narrow CandidateStore handle. Requires a CandidateStore; legacy
  {:sqlite :cas} maps are rejected."
  [store candidate-id]
  (validate-store! store)
  (some-> (candidate-store/find-candidate store candidate-id)
          validate-candidate!))

(defn find-candidates-by-parent
  "Every Candidate record whose parent Genome is `parent-genome-id`,
  in deterministic creation order (created_at, then id). Read-only.

  Fleet R: delegates to evoclj.store.candidate-store/find-candidates-by-parent
  via the narrow CandidateStore handle. Requires a CandidateStore; legacy
  {:sqlite :cas} maps are rejected."
  [store parent-genome-id]
  (validate-store! store)
  (->> (candidate-store/find-candidates-by-parent store parent-genome-id)
       (mapv validate-candidate!)))
