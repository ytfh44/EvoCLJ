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

  `store` is the executor :stores map {:sqlite <db> :cas <CAS root>},
  exactly as in evoclj.evolution.evidence — this namespace writes
  only candidate/mutation ROWS and never opens or closes a
  connection.

  Error contract (Global Constraint 22 — plain serializable data):
  :candidate/store-invalid (:reason :not-a-map :sqlite-missing
  :cas-missing), :candidate/invalid (contract violation, Malli
  explanations), :candidate/mutation-invalid,
  :candidate/mutation-mismatch, :candidate/parent-mismatch,
  :candidate/evidence-mismatch, :candidate/risk-mismatch,
  :candidate/not-proposed, :candidate/not-found,
  :candidate/invalid-transition."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.schema :as gschema]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

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

;; --- the component state vocabulary mapping (documented deviation) -----------

(def ^:private db-state->state
  "Map from the component candidates.state CHECK vocabulary to the
  plan's machine states. The 5.1 schema predates the machine, so the
  in-memory machine states are mapped at the row boundary; :proposed
  has no schema value (rows exist only from :materialized onward) and
  :invalid has none either (M8 must resolve its persistence)."
  {"materialized" :materialized
   "evaluating" :evaluation-pending
   "eligible" :evaluated
   "promoted" :promoted
   "rejected" :rejected
   "stale" :stale})

(def ^:private state->db-state
  "Inverse of db-state->state."
  (into {} (map (fn [[db s]] [s db]) db-state->state)))

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
  "Validate the executor :stores map {:sqlite ... :cas ...} (the
  shape evoclj.evolution.evidence defines). This namespace writes only
  rows; the :cas key is required for the boundary's shape so callers
  pass the same :stores map everywhere."
  [store]
  (when-not (map? store)
    (throw (err/error :candidate/store-invalid
                      "store must be the executor :stores map {:sqlite ... :cas ...}"
                      {:reason :not-a-map :value (err/sanitize store)})))
  (when-not (contains? store :sqlite)
    (throw (err/error :candidate/store-invalid
                      "store must carry the :sqlite handle"
                      {:reason :sqlite-missing})))
  (when-not (contains? store :cas)
    (throw (err/error :candidate/store-invalid
                      "store must carry the :cas handle"
                      {:reason :cas-missing})))
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

;; --- timestamps --------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  "Canonical ISO-8601 UTC string for a timestamp value (a
  java.util.Date, java.time.Instant, or ISO-8601 string); nil means
  now."
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (err/error :candidate/invalid
                                       "timestamp must be an inst, Instant, or ISO-8601 string"
                                       {:timestamp ts})))]
    (.format timestamp-fmt inst)))

(defn- set-busy-timeout!
  "Set SQLite's busy_timeout on the open connection carried by `db`
  (the spec-with-connection map sqlite/with-db binds). A contended
  write waits for SQLite's write lock instead of failing with
  SQLITE_BUSY — the same pattern evoclj.store.session uses for its
  compare-and-set."
  [db ms]
  (let [^java.sql.Connection conn (:connection db)]
    (with-open [stmt (.createStatement conn)]
      (.execute stmt (str "PRAGMA busy_timeout = " ms)))))

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

(defn- canonical
  "Deterministic EDN form for hashing: maps sorted by their pr-str key
  form, sets by their pr-str element form, collections realized
  eagerly. Any EDN-safe value yields a stable pr-str, so the content
  hash is a pure function of logical content (Global Constraint 6)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn mutation-hash
  "The deterministic content hash of a Mutation IR — the uniqueness
  rule's second component (component Step 3).

  sha256 over the canonical pr-str of the mutation EXCLUDING
  :mutation/id: the uuid is proposal-assignment metadata, not
  content. Two mutations with identical declarative content (parent
  Genome, hypothesis, evidence, risk, ops, expected effect) therefore
  hash identically, so re-proposing the same mutation lands on the
  same candidate; different content hashes differently."
  [mutation]
  (hash/text-digest (pr-str (canonical (dissoc mutation :mutation/id)))))

(defn dedupe-key
  "The normative candidate uniqueness rule (component Step 3): a
  candidate is unique by (parent-genome-id, mutation-hash). The same
  parent + same mutation content materialized twice dedupes to the
  same auditable candidate (one row)."
  [parent-genome-id mutation]
  {:parent/genome-id parent-genome-id
   :mutation/hash (mutation-hash mutation)})

;; --- row mapping ---------------------------------------------------------------

(defn- row->mutation
  "Reconstruct the Mutation IR content from a mutations row, for
  recomputing the content hash during the dedup lookup. The 5.1
  mutations table stores the exact IR (ops/expected_effect as EDN), so
  the recomputed hash is byte-identical to the original proposal's."
  [{:keys [id parent_genome_id hypothesis_id evidence_id risk ops
           expected_effect]}]
  {:mutation/id (UUID/fromString id)
   :parent/genome-id parent_genome_id
   :hypothesis/id (UUID/fromString hypothesis_id)
   :evidence/id evidence_id
   :risk (keyword risk)
   :ops (edn/read-string ops)
   :expected-effect (edn/read-string expected_effect)})

(defn- row->candidate
  "Convert a candidates DB row into the public Candidate record. The
  component :state vocabulary is mapped to the machine states (see
  db-state->state); an unknown value fails loudly."
  [row]
  (let [state (get db-state->state (:state row))]
    (when-not state
      (throw (err/error :candidate/invalid
                        "candidates row carries an unknown state"
                        {:candidate/id (:id row) :state (:state row)})))
    {:candidate/id (UUID/fromString (:id row))
     :parent/generation-id (:parent_generation_id row)
     :parent/genome-id (:parent_genome_id row)
     :candidate/genome-id (:genome_id row)
     :mutation/id (UUID/fromString (:mutation_id row))
     :evidence/id (:evidence_id row)
     :risk (keyword (:risk row))
     :state state
     :created-at (Date/from (Instant/parse (:created_at row)))}))

;; --- Step 4: persistence -------------------------------------------------------

(defn- insert-mutation-row!
  "Ensure the mutation row exists (INSERT OR IGNORE by :mutation/id) —
  the candidate's lineage precondition (the candidates.mutation_id
  FK). Mutations are immutable, append-only proposals (Global
  Constraints 4-6, 16); a duplicate uuid is ignored. Afterwards the
  row's parent Genome is verified against the mutation being
  materialized: reusing a mutation uuid across parents is a broken
  caller and fails loudly."
  [conn mutation ts]
  (jdbc/execute! conn
                 ["INSERT OR IGNORE INTO mutations
                   (id, parent_genome_id, hypothesis_id, evidence_id,
                    risk, ops, expected_effect, created_at)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                  (str (:mutation/id mutation))
                  (:parent/genome-id mutation)
                  (str (:hypothesis/id mutation))
                  (:evidence/id mutation)
                  (name (:risk mutation))
                  (pr-str (:ops mutation))
                  (pr-str (:expected-effect mutation))
                  ts])
  (let [row (first (jdbc/query conn
                               ["SELECT parent_genome_id FROM mutations WHERE id = ?"
                                (str (:mutation/id mutation))]))]
    (when (and row (not= (:parent/genome-id mutation) (:parent_genome_id row)))
      (throw (err/error :candidate/mutation-mismatch
                        "an existing mutation row with this id belongs to a different parent genome"
                        {:mutation/id (:mutation/id mutation)
                         :row/parent-genome-id (:parent_genome_id row)})))))

(defn- find-by-dedupe-key
  "The dedup lookup under the uniqueness rule: the earliest candidate
  row (created_at, then id) whose parent Genome and mutation CONTENT
  (mutation-hash recomputed from the stored mutation rows) match.
  Content-based, never uuid-based — two proposals of identical content
  dedupe to the first candidate. Returns a raw candidates row or nil."
  [conn parent-genome-id mh]
  (let [rows (jdbc/query conn
                         ["SELECT id, parent_genome_id, hypothesis_id, evidence_id,
                                  risk, ops, expected_effect
                          FROM mutations WHERE parent_genome_id = ?"
                          parent-genome-id])
        matching-ids (into #{}
                           (keep (fn [row]
                                   (when (= mh (mutation-hash (row->mutation row)))
                                     (:id row))))
                           rows)]
    (when (seq matching-ids)
      (let [placeholders (apply str (interpose ", " (repeat (count matching-ids) "?")))
            sql (str "SELECT * FROM candidates
                      WHERE parent_genome_id = ? AND mutation_id IN (" placeholders ")
                      ORDER BY created_at ASC, id ASC")
            params (into [parent-genome-id] matching-ids)]
        (first (jdbc/query conn (into [sql] params)))))))

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
  (let [db (:sqlite store)
        mh (mutation-hash mutation)
        ts (canonical-timestamp (:created-at candidate))]
    (sqlite/with-db [conn db]
      (set-busy-timeout! conn 10000)
      ;; lineage precondition first — see the namespace docstring for
      ;; why the write-before-read order is the dedup serialization point
      (insert-mutation-row! conn mutation ts)
      (if-let [row (find-by-dedupe-key conn (:parent/genome-id candidate) mh)]
        (validate-candidate! (row->candidate row))
        (do
          (jdbc/insert! conn :candidates
                        {:id (str (:candidate/id candidate))
                         :parent_generation_id (:parent/generation-id candidate)
                         :parent_genome_id (:parent/genome-id candidate)
                         :genome_id (:candidate/genome-id candidate)
                         :mutation_id (str (:mutation/id candidate))
                         :evidence_id (:evidence/id candidate)
                         :risk (name (:risk candidate))
                         :state "materialized"
                         :created_at ts})
          (validate-candidate!
           (row->candidate
            (first (jdbc/query conn ["SELECT * FROM candidates WHERE id = ?"
                                     (str (:candidate/id candidate))])))))))))

(defn transition-candidate!
  "Compare-and-set state transition for a persisted candidate (component Step 4). Changes :state alone via one atomic UPDATE matched on
  the expected state, and returns the updated Candidate record. The
  component machine has one persisted edge: :materialized →
  :evaluation-pending (the :proposed → :materialized edge is realized
  by materialize-candidate!; :evaluated/:invalid arrive with M8).

  Typed errors: :candidate/invalid-transition (not an edge, a target
  state with no 5.1 vocabulary value, or the stored state is not
  expected-state — including a concurrent worker that already won the
  compare-and-set), :candidate/not-found, :candidate/invalid."
  [store candidate-id expected-state new-state]
  (when-not (and (keyword? expected-state) (keyword? new-state))
    (throw (err/error :candidate/invalid-transition
                      "transition states must be keywords"
                      {:expected-state expected-state :new-state new-state})))
  (when-not (contains? (get transitions expected-state #{}) new-state)
    (throw (err/error :candidate/invalid-transition
                      "not an edge of the candidate state machine"
                      {:candidate/id (types/session-id candidate-id)
                       :expected-state expected-state
                       :new-state new-state})))
  (when-not (contains? state->db-state new-state)
    (throw (err/error :candidate/invalid-transition
                      "the target state has no component vocabulary value"
                      {:candidate/id (types/session-id candidate-id)
                       :expected-state expected-state
                       :new-state new-state})))
  (let [cid (types/session-id candidate-id)
        key (str cid)
        ts (canonical-timestamp nil)]
    (sqlite/with-db [conn (:sqlite store)]
      (set-busy-timeout! conn 10000)
      (let [count (first (jdbc/execute! conn
                                        ["UPDATE candidates
                                          SET state = ?
                                          WHERE id = ? AND state = ?"
                                         (state->db-state new-state)
                                         key
                                         (state->db-state expected-state)]))]
        (when-not (= 1 count)
          (let [row (first (jdbc/query conn
                                       ["SELECT state FROM candidates WHERE id = ?"
                                        key]))]
            (if row
              (throw (err/error :candidate/invalid-transition
                                "candidate is not in the expected state"
                                {:candidate/id cid
                                 :expected-state expected-state
                                 :new-state new-state
                                 :actual-state (db-state->state (:state row))}))
              (throw (err/error :candidate/not-found
                                "no candidate with this id"
                                {:candidate/id cid})))))))
    (find-candidate store cid)))

(defn mark-evaluation-pending!
  "Transition a :materialized candidate to :evaluation-pending
  (component Step 4, stored as 'evaluating' in the 5.1 vocabulary).
  The transition is a compare-and-set: a candidate not currently
  :materialized (or already pending) fails with
  :candidate/invalid-transition."
  [store candidate-id]
  (transition-candidate! store candidate-id :materialized :evaluation-pending))

;; --- reads ---------------------------------------------------------------------

(defn find-candidate
  "The Candidate record for `candidate-id`, or nil when no candidate
  has that id. Read-only — no activation rights."
  [store candidate-id]
  (validate-store! store)
  (some-> (first (sqlite/query (:sqlite store)
                               ["SELECT * FROM candidates WHERE id = ?"
                                (str (types/session-id candidate-id))]))
          row->candidate
          validate-candidate!))

(defn find-candidates-by-parent
  "Every Candidate record whose parent Genome is `parent-genome-id`,
  in deterministic creation order (created_at, then id). Read-only."
  [store parent-genome-id]
  (validate-store! store)
  (->> (sqlite/query (:sqlite store)
                     ["SELECT * FROM candidates WHERE parent_genome_id = ?
                       ORDER BY created_at ASC, id ASC"
                      parent-genome-id])
       (mapv (fn [row] (validate-candidate! (row->candidate row))))))
