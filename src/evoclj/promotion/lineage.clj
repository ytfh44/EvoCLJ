(ns evoclj.promotion.lineage
  "component — reconstruct the complete evolutionary history of a
  generation from the store tables (generations + candidates +
  mutations + eval_runs + promotions), with strict-mode integrity
  verification over every referenced artifact while reconstructing.

  The lineage of a generation is a recursive record:

      {:generation {...}   ; the generation's own record
       :parent {...}       ; the parent generation's RECORD, or nil
       :mutation {...}     ; the mutation that produced this generation
       :evidence {...}     ; the frozen evidence pack artifact reference
       :evaluation {...}   ; the finalized evaluation the decision was
                           ;   based on (Database Invariant 5)
       :promotion {...}    ; the promotion record that created this
                           ;   generation (or rejected it)
       :children [...]}    ; every child: promoted generations AND
                           ;   rejected candidate branches

  The history is walked DOWN through :children — every node is a full
  recursive lineage, so a top-level call reconstructs the complete
  tree. :parent carries the parent generation's record (the same shape
  as :generation) for upward identity WITHOUT a cycle (a node's
  :parent is not its parent's full lineage); full upward lineage is
  available by calling (lineage store parent-id).

  :children mixes two kinds of edges, so a lineage reports the FULL
  history — rejected branches, not only winners (component Step 2):

  * a promoted generation edge (generations.parent_id), whose node
    carries the generation record and the promotion record whose
    decision moved the CURRENT pointer (:promoted);
  * a rejected candidate branch (promotions.decision = 'rejected',
    from_generation_id = this generation), whose node has
    :generation nil — the candidate never became a generation — and
    :promotion with :decision :rejected carrying the rejection. A
    rejected branch can never have children. Its promotion row names
    the parent generation as the :to-generation (the pointer did not
    move; the parent satisfies the promotions FK and records the
    rejection — host-level rejection bookkeeping, reserved by component's schema comment).

  Every edge carries the mutation/evaluation/promotion evidence needed
  to explain it (component Step 3): :mutation and :evidence come from
  the promotion's candidate row, :evaluation from the promotion's
  finalized eval_runs row (Invariant 5), and :promotion is the
  decision record itself. A generation created by promotion therefore
  always has all five evidence fields; only a seed generation (no
  promotion) has them nil.

  INTEGRITY VERIFICATION (component Step 4): while reconstructing,
  every referenced CAS artifact is checked for existence and content
  integrity — the technique of evoclj.store.recovery (existence via
  cas/exists?, then a VERIFYING read that re-hashes the body and
  throws :store/cas-corrupt on mismatch). Verified artifacts:
  generation Genomes, candidate Genomes (including rejected ones that
  never became generations), evidence packs (the candidate's
  evidence_id), and a non-nil eval paired_results_ref. Strict mode
  (the default) fails loudly with a typed :lineage/integrity-failure
  carrying the finding; lenient mode ({:strict? false}) annotates the
  affected node with :integrity [findings] (only when a finding
  exists) and completes the reconstruction.

  STORE ARGUMENT: `store` is a map {:sqlite <db> :cas <root|config|cas>}
  — the same :store sub-shape the promotion-system contract carries —
  or a bare db (path or java.jdbc spec). Without :cas there is no
  storage root to verify against, so integrity verification is skipped
  and reconstruction is purely the store rows.

  ROLLBACK NOTE: a rollback (component) is selection-only (Global
  Constraint 18) and writes no promotions row; it is visible in the
  lineage through the generation states (:rolled-back on the displaced
  generation, :active again on the reactivated target) and the CURRENT
  pointer, not through a promotion record. A rolled-back generation
  keeps the promotion record that created it, so its history survives.

  Typed errors (Global Constraint 22 — plain serializable data):
  :lineage/generation-not-found (unknown generation id),
  :lineage/integrity-failure (strict mode found a missing or corrupt
  referenced artifact; carries :finding)."
  (:require [clojure.edn :as edn]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.util Date UUID)))

;; --- row → record helpers ----------------------------------------------------

(def ^:private row-state->kw
  "The component row vocabulary → component machine states (the mapping
  evoclj.promotion.promote documents): 'active'→:active,
  'retired'→:superseded, 'rolled-back'→:rolled-back."
  {"active" :active "retired" :superseded "rolled-back" :rolled-back})

(defn- edn-read
  "Parse a stored EDN payload (all metadata columns are NOT NULL, so a
  nil never reaches the parser)."
  [s]
  (edn/read-string s))

(defn- row-ts
  "A stored ISO-8601 UTC timestamp as a java.util.Date."
  [s]
  (Date/from (Instant/parse s)))

(defn- row-uuid
  "A stored uuid string as a java.util.UUID."
  [s]
  (UUID/fromString s))

(defn- generation-record
  "A generations row as the public generation record."
  [row]
  {:generation/id (:id row)
   :genome/id (:genome_id row)
   :resolution/id (:resolution_id row)
   :parent/id (:parent_id row)
   :state (get row-state->kw (:state row) (keyword (:state row)))
   :created-at (row-ts (:created_at row))})

(defn- mutation-record
  "A mutations row as the public mutation record (ops/expected-effect
  are the stored EDN payloads, parsed)."
  [row]
  {:mutation/id (row-uuid (:id row))
   :parent/genome-id (:parent_genome_id row)
   :hypothesis/id (row-uuid (:hypothesis_id row))
   :evidence/id (:evidence_id row)
   :risk (keyword (:risk row))
   :ops (edn-read (:ops row))
   :expected-effect (edn-read (:expected_effect row))
   :created-at (row-ts (:created_at row))})

(defn- evaluation-record
  "An eval_runs row as the public evaluation record (the finalized
  judgment is consumed verbatim — eligibility is the evaluator's data,
  never recomputed)."
  [row]
  {:evaluation/id (row-uuid (:id row))
   :candidate/id (row-uuid (:candidate_id row))
   :parent/generation-id (:parent_generation_id row)
   :profile-id (:profile_id row)
   :gates (edn-read (:gates row))
   :paired-results-ref (:paired_results_ref row)
   :summary (edn-read (:summary row))
   :eligibility (edn-read (:eligibility row))
   :status (keyword (:status row))
   :created-at (row-ts (:created_at row))})

(defn- promotion-record
  "A promotions row as the public Promotion record (the component
  contract in evoclj.promotion.schema)."
  [row]
  {:promotion/id (row-uuid (:id row))
   :candidate/id (row-uuid (:candidate_id row))
   :evaluation/id (row-uuid (:evaluation_id row))
   :from-generation (:from_generation_id row)
   :to-generation (:to_generation_id row)
   :decision (keyword (:decision row))
   :reason (edn-read (:reason row))
   :created-at (row-ts (:created_at row))})

;; --- store plumbing -----------------------------------------------------------

(defn- query
  "Run a parameterized SELECT on `db`; returns rows as a vector of
  keyword-keyed maps."
  [db sql & params]
  (sqlite/query db (into [sql] params)))

(defn- store-db
  "The sqlite db of the store argument: a config map {:sqlite <db>
  ...} or the bare db itself (path or java.jdbc spec)."
  [store]
  (if (and (map? store) (contains? store :sqlite))
    (:sqlite store)
    store))

(defn- store-cas
  "The CAS root/config of the store argument, or nil when the store
  carries no :cas (a bare db) — integrity verification is skipped."
  [store]
  (when (and (map? store) (contains? store :sqlite))
    (:cas store)))

(defn- verifying-cas
  "A cas config with read verification enabled (re-hash on every read)
  for content integrity checks — the evoclj.store.recovery technique."
  [cas-config]
  (cas/->cas (if (map? cas-config) (:root cas-config) cas-config)
             {:verify true}))

;; --- integrity verification (component Step 4) ---------------------------------

(defn- artifact-finding
  "Existence + content integrity check for ONE referenced artifact,
  reusing the evoclj.store.recovery helpers: cas/exists? (a missing
  body is reported, never assumed present), then a VERIFYING
  cas/get-bytes whose re-hash mismatch throws :store/cas-corrupt.
  Returns nil when intact, or a finding map {:artifact/id ...
  :kind :artifact-missing|:artifact-corrupt :node ... :context ...}."
  [v-cas artifact-id node context]
  (cond
    (nil? artifact-id) nil
    (not (cas/exists? v-cas artifact-id))
    {:artifact/id artifact-id :kind :artifact-missing
     :node node :context context}
    :else
    (try
      (cas/get-bytes v-cas artifact-id)
      nil
      (catch clojure.lang.ExceptionInfo e
        (when (= :store/cas-corrupt (:error/type (ex-data e)))
          {:artifact/id artifact-id :kind :artifact-corrupt
           :node node :context context})))))

(defn- node-artifacts
  "The unique CAS artifact ids referenced by one lineage node, labeled
  for the finding: the generation Genome, the promotion candidate's
  Genome (for a rejected branch, the never-activated Genome), the
  evidence pack, and a non-nil eval paired_results_ref. Global
  Constraint 21: every reference is a content hash, verified as-is."
  [genome-id candidate-row evaluation-row]
  (let [labels [[genome-id :generation "generation genome"]
                [(:genome_id candidate-row) :candidate "candidate genome"]
                [(:evidence_id candidate-row) :evidence "evidence pack"]
                [(:paired_results_ref evaluation-row) :evaluation "paired results"]]]
    (reduce (fn [acc [id node context]]
              (if (or (nil? id) (some #(= id (:artifact/id %)) acc))
                acc
                (conj acc {:artifact/id id :node node :context context})))
            []
            labels)))

(defn- verify-node!
  "Run the integrity checks for one node's referenced artifacts. Strict
  mode throws :lineage/integrity-failure on the FIRST finding; lenient
  mode returns the node annotated with :integrity [findings] when any
  finding exists, unchanged otherwise. When `v-cas` is nil (the store
  carries no :cas root) verification is skipped entirely."
  [v-cas node genome-id cand-row eval-row strict?]
  (if-not v-cas
    node
    (let [artifacts (node-artifacts genome-id cand-row eval-row)]
      (if strict?
        (do (doseq [{:keys [artifact/id] :as a} artifacts]
              (when-let [f (artifact-finding v-cas id (:node a) (:context a))]
                (throw (err/error :lineage/integrity-failure
                                  "lineage reconstruction found a missing or corrupt referenced artifact"
                                  {:finding f}))))
            node)
        (let [findings (into []
                             (keep (fn [{:keys [artifact/id] :as a}]
                                     (artifact-finding v-cas id (:node a) (:context a))))
                             artifacts)]
          (if (seq findings)
            (assoc node :integrity findings)
            node))))))

;; --- node construction ----------------------------------------------------------

(defn- candidate-row
  "The candidates row for `candidate-id`, or nil."
  [db candidate-id]
  (first (query db "SELECT * FROM candidates WHERE id = ?" candidate-id)))

(defn- promotion-row-for-generation
  "The promotion record that CREATED a generation: the 'promoted'
  decision whose :to-generation is this generation (a generation is
  created by exactly one promotion; the seed has none)."
  [db gen-id]
  (first (query db
                "SELECT * FROM promotions
                 WHERE to_generation_id = ? AND decision = 'promoted'
                 ORDER BY created_at, id LIMIT 1"
                gen-id)))

(defn- evidence-record
  "The frozen evidence pack reference (Global Constraint 21: the row
  stores a content address, never a duplicated body)."
  [cand-row]
  (when cand-row
    {:evidence/id (:evidence_id cand-row)}))

(declare lineage-node)

(defn- generation-children
  "The lineage nodes of every generation whose parent is `gen-id`
  (generations.parent_id), newest last (created_at, then id)."
  [db v-cas gen-id strict?]
  (mapv (fn [row]
          (lineage-node db v-cas (:id row) strict?))
        (query db
               "SELECT * FROM generations WHERE parent_id = ? ORDER BY created_at, id"
               gen-id)))

(defn- rejected-branches
  "The lineage nodes for every rejected candidate branch of `gen-id`:
  promotions rows with from_generation_id = `gen-id` and decision
  'rejected' (component Step 2). Each branch's candidate never became a
  generation, so :generation is nil; the node carries the
  mutation/evidence/evaluation/promotion records that explain the
  rejection, and :parent is the parent generation's record. A rejected
  branch has no children."
  [db v-cas gen-id strict?]
  (mapv (fn [promo-row]
          (let [cand (candidate-row db (:candidate_id promo-row))
                mut (when cand
                      (first (query db "SELECT * FROM mutations WHERE id = ?"
                                    (:mutation_id cand))))
                evl (first (query db "SELECT * FROM eval_runs WHERE id = ?"
                                  (:evaluation_id promo-row)))
                node {:generation nil
                      :parent (generation-record (first
                                                  (query db
                                                         "SELECT * FROM generations WHERE id = ?"
                                                         gen-id)))
                      :mutation (when mut (mutation-record mut))
                      :evidence (evidence-record cand)
                      :evaluation (when evl (evaluation-record evl))
                      :promotion (promotion-record promo-row)
                      :children []}]
            (verify-node! v-cas node nil cand evl strict?)))
        (query db
               "SELECT * FROM promotions
                WHERE from_generation_id = ? AND decision = 'rejected'
                ORDER BY created_at, id"
               gen-id)))

(defn- lineage-node
  "Reconstruct the lineage node for `gen-id` (a generations row id),
  recursively: :parent is the parent generation's record, :children
  are the promoted generations and the rejected candidate branches.
  Every referenced artifact is verified (strict throws, lenient
  annotates) — component Steps 2-4."
  [db v-cas gen-id strict?]
  (let [grow (first (query db "SELECT * FROM generations WHERE id = ?" gen-id))]
    (when-not grow
      (throw (err/error :lineage/generation-not-found
                        "no generation with this id in the store"
                        {:generation/id gen-id})))
    (let [promo (promotion-row-for-generation db gen-id)
          cand (when promo (candidate-row db (:candidate_id promo)))
          mut (when cand
                (first (query db "SELECT * FROM mutations WHERE id = ?"
                              (:mutation_id cand))))
          evl (when promo
                (first (query db "SELECT * FROM eval_runs WHERE id = ?"
                              (:evaluation_id promo))))
          parent-record (when (:parent_id grow)
                          (generation-record
                           (first (query db "SELECT * FROM generations WHERE id = ?"
                                         (:parent_id grow)))))
          node {:generation (generation-record grow)
                :parent parent-record
                :mutation (when mut (mutation-record mut))
                :evidence (evidence-record cand)
                :evaluation (when evl (evaluation-record evl))
                :promotion (when promo (promotion-record promo))
                :children (into (generation-children db v-cas gen-id strict?)
                                (rejected-branches db v-cas gen-id strict?))}]
      (verify-node! v-cas node (:genome_id grow) cand evl strict?))))

;; --- public API ------------------------------------------------------------------

(defn lineage
  "Reconstruct the complete evolutionary history of `generation-id`
  (component). See the namespace docstring for the node shape, the
  store argument, the integrity contract, and the typed errors.

  `store` is {:sqlite <db> :cas <root|config|cas>} or a bare db (path
  or java.jdbc spec); with no :cas, artifact integrity verification is
  skipped. `opts` accepts {:strict? bool} — strict (the default) fails
  closed with :lineage/integrity-failure on a missing/corrupt
  referenced artifact; lenient ({:strict? false}) annotates the
  affected node with :integrity [findings] and completes the
  reconstruction."
  [store generation-id & [opts]]
  (let [{:keys [strict?] :or {strict? true}} opts
        db (store-db store)
        v-cas (when-let [cas-config (store-cas store)]
                (verifying-cas cas-config))]
    (lineage-node db v-cas generation-id strict?)))
