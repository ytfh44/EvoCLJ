(ns evoclj.evolution.history
  "Recent-mutation history — durable, queryable negative evidence
  (component; Global Constraint 16: rejected mutations MUST remain
  durable, queryable negative evidence).

  THE STORE (component interface):

      (recent-mutation-history store generation-lineage {:limit 50})
      ;; => accepted/rejected mutation summaries with metric deltas/reasons

  `generation-lineage` is a vector of generation ids — the lineage
  from the current generation back to the root. The history of that
  lineage is the set of Mutation IR PROPOSALS whose content material-
  ized a Candidate whose parent generation is IN the lineage. A
  proposal is matched to its candidate by the component DEDUP KEY
  (parent-genome-id, mutation-hash) — NOT by the candidate row's
  mutation_id — because the uniqueness rule means a repeat proposal
  of the same content carries no candidate row of its own (it dedupes
  into the first proposal's candidate; Global Constraint 16 keeps
  both proposals durable). Entries come newest-proposal-first and are
  bounded by :limit (default 50, maximum 500 — the window is
  deliberately bounded).

  Each entry is a summary:

      {:mutation/id #uuid
       :parent/genome-id \"sha256:...\"
       :parent/generation-id \"generation-1\"
       :candidate/id #uuid
       :evidence/id \"sha256:...\"
       :risk :behavioral
       :mutation/hash \"sha256:...\" ; the component content hash — the
                                      ; exact-repeat identity
       :fingerprint \"sha256:...\"   ; the Step 2 structural fingerprint
       :state :pending | :accepted | :rejected
       :reason nil | <rejection reason>
       :metric-deltas nil | {<section> {<metric> delta}}
       :negative-evidence false | true
       :created-at inst?}

  PERSISTENCE MODEL (Step 1): this namespace WRITES NO DECISION ROWS.
  The rejection reason and metric deltas are persisted by the
  evaluation and promotion write paths — the component eval_runs and
  promotions rows (the M8 evaluator persists :summary and
  :eligibility; the M9 promotion subsystem persists :decision and
  :reason). History is the durable QUERY surface over those rows, and
  it reports :pending until evaluator results exist. The verdict is
  resolved newest-first:

    1. A promotion decision row is authoritative: 'promoted' reads
       :accepted; 'rejected', 'stale', and 'rolled-back' read
       :rejected (none of them persisted as the current generation —
       a stale or rolled-back candidate is negative evidence too).
       :reason is the promotion's :reason EDN.
    2. Otherwise a FINALIZED eval run's :eligibility decides:
       :eligible? true reads :accepted, false reads :rejected with
       the eligibility :reasons as the rejection reason. A RUNNING
       run is not a result yet.
    3. Otherwise the evaluator has no results: :pending, :reason nil.

  Metric deltas are derived from the eval run's :summary (the component shape {:hard ... :utility ... :cost ... :complexity ...}):
  every leaf map carrying BOTH a numeric :parent and a numeric
  :candidate value yields candidate - parent under its metric key,
  in its section; leaves without a numeric parent/candidate pair
  (e.g. :integrity {:parent :pass :candidate :pass}) and empty
  sections are omitted.

  SIMILARITY FINGERPRINT (Step 2): `mutation-fingerprint` /
  `op-fingerprint` are a DETERMINISTIC STRUCTURAL digest over the
  targeted files + op types + NORMALIZED selectors of a mutation —
  never an LLM semantic judgment. Payload keys (:value :text :form
  :edge :node :expect/hash) are not part of the fingerprint, so two
  mutations acting on the same file+op+selector with different
  payloads share a fingerprint, while a different file, op type, or
  selector differs. Selector normalization: EDN :path elements stay
  type-stable (a keyword key and a string key are genuinely
  different targets — cross-type collapsing would make unrelated
  mutations collide); text anchors have CRLF/CR line endings
  normalized to LF (the evoclj.genome.hash canonical text
  convention); form selectors canonicalize a scalar to its
  one-element vector form (the component schema admits both spellings
  for the same target); op ORDER never changes the fingerprint (the
  same edits reordered are the same target set). Selector kinds are
  tagged (:edn-path :anchor :form :node :edge) so no two op families
  can collide. The digest is the repo's canonical sha256 content
  hash, so the fingerprint is a pure function of logical content
  (Global Constraint 6).

  NEGATIVE EVIDENCE (Step 3): an entry whose :mutation/hash (the
  component content hash — same parent Genome + same mutation content)
  appeared on an EARLIER entry in the returned window with :state
  :rejected is an EXACT repeat and is flagged :negative-evidence true
  for the Mutator. Fingerprint SIMILARITY alone never flags.

  NO BANNING (Step 4): history is evidence only. No entry is filtered
  out, no similar mutation is banned or suppressed here — the public
  surface is exactly the evidence API (recent-mutation-history,
  mutation-fingerprint, op-fingerprint) and the final proposal logic
  (component) decides what to do with the evidence.

  Error contract (Global Constraint 22 — plain serializable data):
  :history/store-invalid (:reason :not-a-map :sqlite-missing
  :cas-missing), :history/request-invalid (closed-map contract
  violation or an over-bounded window), :history/op-invalid (a
  malformed or unknown op, a non-map mutation, or a non-canonical
  selector element — fail-closed, so corrupt rows cannot silently
  produce garbage fingerprints)."
  (:require [clojure.edn :as edn]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.mutation-schema :as ms]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.util Date UUID)))

;; --- the window -----------------------------------------------------------------

(def ^:private default-limit
  "The default recent-history window size (component interface:
  {:limit 50})."
  50)

(def ^:private max-window
  "The hard cap on the history window: the query and the negative-
  evidence scan stay bounded."
  500)

;; --- store trust boundary -------------------------------------------------------

(defn- validate-store!
  "Validate the store trust boundary: the executor :stores map
  {:sqlite <db> :cas <CAS root>} — the same shape every evolution
  namespace validates. History reads only rows; :cas is required for
  boundary-shape consistency so callers pass the same map everywhere."
  [store]
  (when-not (map? store)
    (throw (err/error :history/store-invalid
                      "store must be the executor :stores map {:sqlite ... :cas ...}"
                      {:reason :not-a-map :value (err/sanitize store)})))
  (when-not (contains? store :sqlite)
    (throw (err/error :history/store-invalid
                      "store must carry the :sqlite handle"
                      {:reason :sqlite-missing})))
  (when-not (contains? store :cas)
    (throw (err/error :history/store-invalid
                      "store must carry the :cas handle"
                      {:reason :cas-missing})))
  store)

;; --- request validation ----------------------------------------------------------

(def ^:private HistoryRequestSchema
  "The recent-mutation-history request contract (closed): a non-empty
  vector of generation ids (the lineage from the current generation
  back to the root) and an optional positive-integer :limit."
  [:map {:closed true}
   [:generation-lineage [:vector {:min 1} string?]]
   [:limit {:optional true} [:and int? pos-int?]]])

(defn- validate-request!
  [request]
  (when-let [expl (m/explain HistoryRequestSchema request)]
    (throw (err/error :history/request-invalid
                      "recent-mutation-history request does not satisfy the contract"
                      {:errors (me/humanize expl)})))
  (when-let [limit (:limit request)]
    (when (> limit max-window)
      (throw (err/error :history/request-invalid
                        "the history window is bounded"
                        {:limit limit :max max-window}))))
  request)

;; --- canonical hashing conventions (the repo's digest convention) ---------------

(defn- canonical
  "Deterministic EDN form for hashing — the same convention as
  evoclj.evolution.candidate/canonical: maps sorted by their pr-str
  key form, sets by their pr-str element form, collections realized
  eagerly. Any EDN-safe value yields a stable pr-str, so the
  fingerprint is a pure function of logical content (Global
  Constraint 6)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- digest
  "The canonical sha256 content digest (\"sha256:<64 hex>\") of the
  canonical pr-str of `data`."
  [data]
  (hash/text-digest (pr-str (canonical data))))

;; --- Step 2: the similarity fingerprint ------------------------------------------

(defn- scalar-normalized
  "A normalized selector scalar: keywords, strings, symbols, and
  positive integers stay themselves. Cross-type collapsing is
  deliberately NOT performed — in EDN data and Clojure forms a
  keyword key and a string (or symbol) are genuinely different
  targets, so merging them would make unrelated mutations collide.
  Anything else fails closed (:history/op-invalid)."
  [x]
  (cond
    (keyword? x) x
    (string? x) x
    (symbol? x) x
    (pos-int? x) x
    :else (throw (err/error :history/op-invalid
                            "selector element is not a keyword, string, symbol, or positive integer"
                            {:element (err/sanitize x)}))))

(defn- normalized-edn-path
  "An EDN navigation :path normalized to a vector of type-stable
  scalars. A path is already ordered — the order is part of the
  target."
  [path]
  (mapv scalar-normalized path))

(defn- normalized-anchor
  "A text anchor normalized: string anchors get CRLF/CR line endings
  normalized to LF (the repo's canonical text convention,
  evoclj.genome.hash); line-offset anchors stay integers."
  [anchor]
  (if (string? anchor)
    (hash/normalize-line-endings anchor)
    (scalar-normalized anchor)))

(defn- normalized-form-selector
  "A form :selector normalized. The component schema admits a scalar
  AND a one-element vector for the same target, so a scalar is
  canonicalized to its one-element vector form; every element stays
  type-stable."
  [selector]
  (mapv scalar-normalized (if (vector? selector) selector [selector])))

(defn- op-target
  "The structural target of one op — the fingerprint atom (Step 2):
  {:file <targeted file> :op <op type> :selector <normalized
  selector>}. Payload keys (:value :text :form :edge :node
  :expect/hash) are NOT part of the target, so two mutations acting
  on the same file+op+selector with different payloads share a
  fingerprint. Selector kinds are tagged so no two op families can
  collide:

      [:edn-path ...] — the normalized :path of the EDN value ops
      [:anchor ...]   — the normalized :anchor of the text ops
      [:form ...]     — the normalized :selector of the form ops
      [:node <id>]    — the :node/id of the topology node ops
      [:edge <from> <to>] — the ordered endpoints of the edge ops

  The op must satisfy the component op schema (fail-closed:
  :mutation/op-invalid on a malformed or unknown op, :history/
  op-invalid on an op outside the language)."
  [op]
  (ms/validate-op op)
  (let [selector (case (:op op)
                   (:set-edn :delete-edn)
                   [:edn-path (normalized-edn-path (:path op))]

                   (:insert-text :replace-text :delete-text)
                   [:anchor (normalized-anchor (:anchor op))]

                   (:replace-form :insert-form :delete-form)
                   [:form (normalized-form-selector (:selector op))]

                   :add-node
                   [:node (:node/id (:node op))]

                   (:remove-node :update-node)
                   [:node (:node/id op)]

                   (:add-edge :remove-edge)
                   [:edge [(:from (:edge op)) (:to (:edge op))]]

                   (throw (err/error :history/op-invalid
                                     "cannot fingerprint an op outside the op language"
                                     {:op (:op op)})))]
    {:file (:file op)
     :op (:op op)
     :selector selector}))

(defn op-fingerprint
  "The Step 2 similarity fingerprint of ONE op: the canonical sha256
  digest of its structural target {:file :op :selector} — payloads
  excluded. Typed errors: :mutation/op-invalid (malformed op),
  :history/op-invalid (op outside the language)."
  [op]
  (digest {:targets [(op-target op)]}))

(defn mutation-fingerprint
  "The Step 2 similarity fingerprint of a whole Mutation IR: the
  canonical sha256 digest over the SORTED set of its ops' structural
  targets. Op order never changes the fingerprint (the same edits in
  a different order are the same target set); payloads never do.
  NOT an LLM semantic judgment — a pure deterministic function of
  file names, op types, and normalized selectors.

  Typed errors: :history/op-invalid (a non-map mutation, an empty
  :ops vector, or an op outside the language), :mutation/op-invalid
  (a malformed op)."
  [mutation]
  (when-not (map? mutation)
    (throw (err/error :history/op-invalid
                      "mutation must be a Mutation IR map"
                      {:value (err/sanitize mutation)})))
  (when-not (seq (:ops mutation))
    (throw (err/error :history/op-invalid
                      "a mutation must carry a non-empty :ops vector"
                      {:mutation (err/sanitize (dissoc mutation :ops))})))
  (digest {:targets (sort-by pr-str (mapv op-target (:ops mutation)))}))

;; --- row mapping -----------------------------------------------------------------

(defn- row->mutation
  "Reconstruct the Mutation IR content from a mutations row — the
  same reconstruction evoclj.evolution.candidate uses — so the exact
  repeat identity (component :mutation/hash) can be recomputed
  content-identically."
  [{:keys [id parent_genome_id hypothesis_id evidence_id risk ops
           expected_effect]}]
  {:mutation/id (UUID/fromString id)
   :parent/genome-id parent_genome_id
   :hypothesis/id (UUID/fromString hypothesis_id)
   :evidence/id evidence_id
   :risk (keyword risk)
   :ops (edn/read-string ops)
   :expected-effect (edn/read-string expected_effect)})

(defn- dedupe-key-of
  "The component dedup identity of a mutation row: (parent-genome-id,
  mutation-hash) — the same rule evoclj.evolution.candidate uses to
  deduplicate candidates."
  [mutation-row]
  (let [m (row->mutation mutation-row)]
    {:parent/genome-id (:parent/genome-id m)
     :mutation/hash (candidate/mutation-hash m)}))

(defn- candidate-by-dedupe-key
  "Map the component dedup key (parent-genome-id, mutation-hash) →
  candidate row for the in-lineage candidates. The candidate's
  mutation_id references the CANONICAL proposal of its content; the
  content hash IS the dedup identity, so every proposal row whose
  content deduped into that candidate resolves to it — including
  repeat proposals that carry no candidate row of their own (component
  Step 3: the same parent + mutation content is ONE candidate)."
  [db candidate-rows]
  (let [mutation-ids (distinct (map :mutation_id candidate-rows))
        placeholders (apply str (interpose ", " (repeat (count mutation-ids) "?")))
        mrows (sqlite/query db
                            (into [(str "SELECT * FROM mutations
                                         WHERE id IN (" placeholders ")")]
                                  mutation-ids))
        mrow-by-id (into {} (map (fn [r] [(:id r) r])) mrows)]
    (into {}
          (keep (fn [crow]
                  (when-let [mrow (get mrow-by-id (:mutation_id crow))]
                    [(dedupe-key-of mrow) crow])))
          candidate-rows)))

;; --- Step 1: verdict resolution ---------------------------------------------------

(defn- leaf-delta
  "The numeric delta of one evaluation-summary leaf. A map carrying
  BOTH a numeric :parent and a numeric :candidate value yields
  candidate - parent (auxiliary non-numeric keys such as :violations
  are ignored). Any other map recurses and keeps only non-empty child
  sections. Anything that is not a map yields nothing."
  [x]
  (cond
    (map? x)
    (if (and (number? (:parent x)) (number? (:candidate x)))
      (- (:candidate x) (:parent x))
      (let [children (into {}
                           (keep (fn [[k v]]
                                   (let [d (leaf-delta v)]
                                     (when (and (some? d)
                                                (not (and (map? d) (empty? d))))
                                       [k d]))))
                           x)]
        (when (seq children) children)))
    :else nil))

(defn- metric-deltas
  "The metric deltas of an evaluation summary (Step 1): candidate -
  parent per numeric leaf, grouped by the summary's own sections
  (:hard :utility :cost :complexity). nil when nothing numeric is
  present."
  [summary]
  (leaf-delta summary))

(defn- resolve-verdict
  "Resolve the durable verdict for one candidate from its newest
  promotion decision and its newest FINALIZED eval run:

  1. A promotion decision row is authoritative: 'promoted' reads
     :accepted; 'rejected', 'stale', and 'rolled-back' read
     :rejected (none persisted as the current generation). :reason
     is the promotion's :reason EDN.
  2. Otherwise a finalized eval run's :eligibility decides:
     :eligible? true reads :accepted; false reads :rejected with the
     eligibility :reasons as the rejection reason. A RUNNING run is
     not a result yet.
  3. Otherwise the evaluator has no results: :pending.

  Metric deltas come from the eval run's :summary whenever a
  finalized run exists (in every branch)."
  [promotion eval-run]
  (let [deltas (some-> eval-run :summary edn/read-string metric-deltas)]
    (cond
      promotion
      (let [decision (keyword (:decision promotion))]
        {:state (if (= :promoted decision) :accepted :rejected)
         :reason (edn/read-string (:reason promotion))
         :metric-deltas deltas})

      (and eval-run (= "finalized" (:status eval-run)))
      (let [eligibility (edn/read-string (:eligibility eval-run))
            eligible? (:eligible? eligibility)]
        {:state (if eligible? :accepted :rejected)
         :reason (when-not eligible? (:reasons eligibility))
         :metric-deltas deltas})

      :else
      {:state :pending :reason nil :metric-deltas nil})))

(defn- newest-per-candidate
  "Map candidate-id → the FIRST (most recent) row per candidate.
  `rows` arrive newest-first; the first occurrence wins."
  [rows]
  (reduce (fn [acc row]
            (if (contains? acc (:candidate_id row))
              acc
              (assoc acc (:candidate_id row) row)))
          {}
          rows))

(defn- verdicts-by-candidate
  "One durable verdict per candidate id: the newest promotion
  decision plus the newest FINALIZED eval run (a running run is not
  a result yet — Step 1)."
  [db candidate-ids]
  (let [ids (distinct candidate-ids)]
    (if (empty? ids)
      {}
      (let [placeholders (apply str (interpose ", " (repeat (count ids) "?")))
            eval-rows (sqlite/query db
                                    (into [(str "SELECT * FROM eval_runs
                                                 WHERE candidate_id IN ("
                                                 placeholders
                                                 ")
                                                 ORDER BY created_at DESC, id DESC")]
                                          ids))
            promo-rows (sqlite/query db
                                     (into [(str "SELECT * FROM promotions
                                                  WHERE candidate_id IN ("
                                                  placeholders
                                                  ")
                                                  ORDER BY created_at DESC, id DESC")]
                                           ids))
            final-evals (newest-per-candidate
                         (filter #(= "finalized" (:status %)) eval-rows))
            promos (newest-per-candidate promo-rows)]
        (into {}
              (map (fn [cid]
                     [cid (resolve-verdict (get promos cid)
                                           (get final-evals cid))]))
              ids)))))

;; --- Step 3: negative evidence ------------------------------------------------------

(defn- mark-negative-evidence
  "Flag EXACT repeats of recently rejected mutations (Step 3).

  Walking the window oldest-first, an entry is an exact repeat — and
  flagged :negative-evidence true — when an EARLIER entry in the
  window shares its :mutation/hash (the component content hash: same
  parent Genome + same mutation content) with :state :rejected.
  Fingerprint similarity alone never flags (Step 4)."
  [entries]
  (-> (reduce (fn [[acc rejected-hashes] entry]
                (let [flag (contains? rejected-hashes (:mutation/hash entry))
                      rejected-hashes' (if (= :rejected (:state entry))
                                         (conj rejected-hashes
                                               (:mutation/hash entry))
                                         rejected-hashes)]
                  [(conj acc (assoc entry :negative-evidence flag))
                   rejected-hashes']))
              [[] #{}]
              (reverse entries))
      first
      reverse
      vec))

;; --- entry assembly -----------------------------------------------------------------

(defn- row->entry
  "Build one history summary from a proposal row, the candidate row
  its content deduped into, and the candidate's resolved verdict."
  [mutation-row candidate-row verdict]
  (let [m (row->mutation mutation-row)]
    {:mutation/id (:mutation/id m)
     :parent/genome-id (:parent/genome-id m)
     :parent/generation-id (:parent_generation_id candidate-row)
     :candidate/id (UUID/fromString (:id candidate-row))
     :evidence/id (:evidence/id m)
     :risk (:risk m)
     :mutation/hash (candidate/mutation-hash m)
     :fingerprint (mutation-fingerprint m)
     :state (:state verdict)
     :reason (:reason verdict)
     :metric-deltas (:metric-deltas verdict)
     :negative-evidence false
     :created-at (Date/from (Instant/parse (:created_at mutation-row)))}))

;; --- the public entry point ----------------------------------------------------------

(defn recent-mutation-history
  "Return the recent mutation history of a generation lineage (component): one entry per Mutation IR proposal whose materialized
  Candidate's parent generation is IN `generation-lineage` (a vector
  of generation ids — the lineage from the current generation back
  to the root), newest proposal first, bounded by :limit (default
  50, maximum 500). See the namespace docstring for the entry
  contract, the verdict resolution order (Step 1), the fingerprint
  (Step 2), the exact-repeat flag (Step 3), and the evidence-only
  guarantee (Step 4).

  Typed errors: :history/store-invalid, :history/request-invalid,
  :history/op-invalid (a corrupt op row)."
  [store generation-lineage & [opts]]
  (validate-store! store)
  (validate-request! (assoc (or opts {})
                            :generation-lineage (vec generation-lineage)))
  (let [db (:sqlite store)
        lineage (vec generation-lineage)
        limit (or (:limit opts) default-limit)
        lineage-ph (apply str (interpose ", " (repeat (count lineage) "?")))
        ;; the lineage scope: candidates whose parent generation is IN
        ;; the lineage (candidate.parent_generation_id is the
        ;; generation whose Genome the mutation mutated)
        candidate-rows (sqlite/query db
                                     (into [(str "SELECT * FROM candidates
                                                  WHERE parent_generation_id IN ("
                                                  lineage-ph
                                                  ")
                                                  ORDER BY created_at ASC, id ASC")]
                                           lineage))
        cand-by-key (candidate-by-dedupe-key db candidate-rows)
        parent-genomes (distinct (map :parent_genome_id candidate-rows))
        parent-ph (apply str (interpose ", " (repeat (count parent-genomes) "?")))
        proposal-rows (if (empty? parent-genomes)
                        []
                        (sqlite/query db
                                      (into [(str "SELECT * FROM mutations
                                                   WHERE parent_genome_id IN ("
                                                   parent-ph
                                                   ")")]
                                            parent-genomes)))
        ;; pair each proposal with the candidate its content deduped
        ;; into (component rule); unmaterialized proposals have no
        ;; candidate and are not part of the history
        paired (keep (fn [mrow]
                       (when-let [crow (get cand-by-key (dedupe-key-of mrow))]
                         [mrow crow]))
                     proposal-rows)
        ;; newest proposal first (created_at, then id)
        ordered (sort (fn [[ma _] [mb _]]
                        (compare [(:created_at mb) (:id mb)]
                                 [(:created_at ma) (:id ma)]))
                      paired)
        windowed (take limit ordered)
        verdicts (verdicts-by-candidate db (mapv (comp :id second) windowed))
        entries (mapv (fn [[mrow crow]]
                        (row->entry mrow crow
                                    (get verdicts (:id crow))))
                      windowed)]
    (mark-negative-evidence entries)))
