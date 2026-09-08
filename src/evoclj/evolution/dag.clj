(ns evoclj.evolution.dag
  "Pure contracts for speculative evolution branches.

  This namespace deliberately contains no persistence or CURRENT-pointer logic.
  It canonicalizes ordered parent edges and merge plans, plans a strict
  structured merge of ordinary Mutation IR operations, and provides bounded
  deterministic frontier/beam helpers for callers such as the scheduler."
  (:require [evoclj.genome.hash :as hash]
             [evoclj.genome.patch-clj :as patch-clj]
             [evoclj.genome.patch-edn :as patch-edn]
             [evoclj.genome.patch-text :as patch-text]
             [evoclj.kernel.error :as err]))

(def ^:private provenance-keys
  #{:mutation/id :candidate/id :hypothesis/id :evidence/id :created-at
    :provenance :source :source/id :parent/source :branch/id :edge/id
    :metadata :reason :risk})

(defn- bytes-key [x]
  (let [a (.getBytes (str x) java.nio.charset.StandardCharsets/UTF_8)]
    (vec (map #(bit-and 0xff %) a))))

(defn- canonicalize [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b]
                                   (compare (bytes-key a) (bytes-key b))))
                   (keep (fn [[k v]]
                           (when-not (contains? provenance-keys k)
                             [k (canonicalize v)])))
                   x)
    (set? x) (vec (sort #(compare (bytes-key %1) (bytes-key %2))
                        (map canonicalize x)))
    (sequential? x) (mapv canonicalize x)
    :else x))

(defn canonical-parent-edges
  "Return ordered, immutable parent edges.

  Parent order is bytewise by parent id; supplied ordinals and provenance do
  not affect the result. Each edge is normalized to :ordinal/:parent-id and
  optional :role/:merge-plan-digest fields. Duplicate parent ids are rejected.
  `known-parents` may be supplied as a set to reject dangling references."
  ([parents] (canonical-parent-edges parents nil))
  ([parents known-parents]
   (let [raw (cond
               (map? parents) (or (:parent-edges parents) (:parents parents) [])
               :else parents)
         edges (mapv (fn [p]
                       (if (map? p)
                         {:parent-id (or (:parent-id p) (:id p) (:generation/id p)
                                         (:candidate/id p) (:genome/id p)
                                         (:parent/generation-id p) (:parent/candidate-id p)
                                         (:parent/genome-id p))
                          :role (or (:role p) :parent)
                          :merge-plan-digest (:merge-plan-digest p)}
                         {:parent-id p :role :parent})) raw)
         ids (mapv :parent-id edges)]
     (when (some nil? ids)
       (throw (err/error :evolution/dag-invalid
                         "parent edges require an id" {:parents parents})))
     (when (not= (count ids) (count (distinct ids)))
       (throw (err/error :evolution/dag-invalid
                         "parent edges must not contain duplicate parents"
                         {:parents ids})))
     (when (and known-parents (some #(not (contains? (set known-parents) %)) ids))
       (throw (err/error :evolution/dag-dangling-parent
                         "parent edge references an unknown parent"
                         {:parents ids :known (vec known-parents)})))
     (mapv (fn [ordinal edge]
             (assoc (select-keys edge [:parent-id :role :merge-plan-digest])
                    :ordinal ordinal))
           (range)
           (sort-by (comp bytes-key :parent-id) edges)))))

(defn parent-edge-key
  "Stable dedupe key for an ordered parent set and merge plan digest."
  [parents merge-plan]
  (let [edges (canonical-parent-edges parents)
        digest (if (string? merge-plan) merge-plan (hash/text-digest (pr-str (canonicalize merge-plan))))]
    (hash/text-digest (pr-str {:parents (mapv #(select-keys % [:ordinal :parent-id]) edges)
                               :merge-plan-digest digest}))))

(defn canonical-merge-plan
  "Canonical merge-plan contract. The returned value is provenance-free and
  stable across map/set/input ordering."
  [{:keys [parents parent-edges ops base common-base preimages] :as plan}]
  (let [ps (canonical-parent-edges (or parent-edges parents []))
        ops* (->> (or ops [])
                  (map #(apply dissoc % provenance-keys))
                  (map canonicalize)
                  (sort-by pr-str)
                  vec)]
    (when (empty? ps)
      (throw (err/error :evolution/merge-invalid
                        "a merge plan requires at least one parent" {})))
    (when-not (or base common-base preimages)
      (throw (err/error :evolution/merge-missing-base
                        "merge plans require an explicit common base/preimages" {})))
    {:parents ps
     :ops ops*
     :base (canonicalize (or base common-base))
     :preimages (canonicalize preimages)}))

(defn merge-plan-digest [plan]
  (hash/text-digest (pr-str (canonical-merge-plan plan))))

(defn- selector-of [op]
  (or (:selector op) (:select op) (:anchor op) (:index op) (:path op)
      (:node/id op) (:edge op) (:key op)))

(defn- prefix? [a b]
  (and (vector? a) (vector? b)
       (or (= a (subvec b 0 (min (count a) (count b))))
           (= b (subvec a 0 (min (count a) (count b)))))))

(defn- overlap? [a b]
  (and (= (:file a) (:file b))
       (let [sa (selector-of a) sb (selector-of b)]
         (or (= sa sb)
             (prefix? sa sb)
             (and (nil? sa) (nil? sb))
             (#{:delete-text :delete-form :delete-edn :remove-node :remove-edge}
              (:op a))
             (#{:delete-text :delete-form :delete-edn :remove-node :remove-edge}
              (:op b))))))

(defn- identical-op? [a b]
  (= (dissoc a :expect/hash :preimage) (dissoc b :expect/hash :preimage)))

(defn- conflict? [a b]
  (and (overlap? a b)
       (not (identical-op? a b))))

(defn- target-content [base op]
  (or (get-in base [:files (:file op)])
      (get-in base [:genome/files (:file op)])
      (get-in base [:preimages (:file op)])
      (get-in base [:preimages (str (:file op))])))

(defn- apply-raw-op [content op]
  (case (:op op)
    (:set-edn :delete-edn :add-node :remove-node :add-edge :remove-edge :update-node)
    (patch-edn/apply-op content op)
    (:insert-text :replace-text :delete-text)
    (patch-text/apply-op content op)
    (:replace-form :insert-form :delete-form)
    (patch-clj/apply-op content op)
    (throw (err/error :evolution/merge-op-invalid
                      "merge plan contains an unknown Mutation op"
                      {:op (:op op)}))))

(defn- common-base-files [base preimages]
  (or (:files base) (:genome/files base) (:files preimages) preimages))

(defn plan-merge
  "Plan a strict merge of ordinary canonical Mutation IR operations.

  Inputs are {:base {:files {path content}} :parents [...] :mutations [...]}
  (or :ops directly). Every destructive op must have an explicit preimage or
  be derivable from the common base. Duplicate disjoint operations coalesce;
  overlap, selector/prefix, topology and delete-vs-edit conflicts reject.
  Expect hashes are computed by the kernel from the common base and from each
  deterministic intermediate application. No last-write-wins behavior exists."
  [{:keys [base common-base preimages parents parent-edges mutations ops] :as request}]
  (let [base* (or base common-base {})
        files (common-base-files base* preimages)
        all-ops (vec (or ops (mapcat :ops mutations)))
        canonical-ops (->> all-ops
                           (map #(apply dissoc % provenance-keys))
                           (sort-by pr-str)
                           vec)
        _ (when (empty? (or parents parent-edges))
            (throw (err/error :evolution/merge-invalid "merge requires parents" {})))
        _ (doseq [[a b] (for [a canonical-ops b canonical-ops :when (< (compare (pr-str a) (pr-str b)) 0)] [a b])]
            (when (conflict? a b)
              (throw (err/error :evolution/merge-conflict
                                "merge operations overlap or are destructive conflicts"
                                {:left a :right b}))))
        deduped (reduce (fn [acc op]
                          (if (some #(identical-op? % op) acc) acc (conj acc op))) [] canonical-ops)
        result (reduce (fn [{:keys [files ops]} op]
                         (let [file (:file op)
                               content (get files file (target-content base* op))]
                           (when (nil? content)
                             (throw (err/error :evolution/merge-missing-preimage
                                               "merge op has no common-base preimage"
                                               {:op op :file file})))
                           (let [expect (hash/text-digest content)
                                 op* (assoc op :expect/hash expect)
                                 next-content (apply-raw-op content op*)]
                             {:files (assoc files file next-content)
                              :ops (conj ops op*)})))
                       {:files (or files {}) :ops []}
                       (sort-by pr-str deduped))
        plan (canonical-merge-plan {:parents (or parents parent-edges)
                                    :ops (:ops result)
                                    :base base*
                                    :preimages files})]
    (let [digest (merge-plan-digest plan)]
      {:merge-plan plan
       :ops (:ops result)
       :files (:files result)
       :digest digest
       :parent-edge-key (parent-edge-key (:parents plan) digest)})))

(defn deterministic-order
  "Stable candidate order: score descending, then digest, genome/id, and id bytewise."
  [candidates score-fn]
  (sort-by (fn [candidate]
             [(- (double (or (score-fn candidate) 0.0)))
              (bytes-key (or (:genome/digest candidate)
                             (:candidate/digest candidate)
                             (:digest candidate)
                             (:genome/id candidate)
                             (:candidate/genome-id candidate)
                             (:candidate/id candidate)
                             (:id candidate)
                             ""))
              (bytes-key (or (:candidate/id candidate) (:id candidate) ""))]) candidates))

(defn bounded-frontier
  "Select at most K deterministic candidates from a frontier under an eval budget.
  The result retains rejected/unselected entries as evidence and never mutates
  CURRENT or any store."
  [candidates {:keys [k frontier eval-budget score-fn seed]
               :or {k 1 eval-budget Long/MAX_VALUE score-fn (constantly 0.0)}}]
  (let [limit (long (max 0 (min (or k 1) (or frontier k 1))))
        ordered (vec (deterministic-order (take (long (max 0 eval-budget)) candidates) score-fn))]
    {:selected (vec (take limit ordered))
     :unselected (vec (drop limit ordered))
     :evaluated (count ordered)
     :eval-budget eval-budget
     :k limit
     :frontier (or frontier k 1)
     :seed seed}))

(defn beam-search
  "Deterministic bounded beam search over pure `expand`, `evaluate` and
  `id-fn` functions. Expansion stops at the evaluation budget and returns all
  unselected/rejected branches as evidence."
  [initial {:keys [k eval-budget expand evaluate id-fn seed]
            :or {k 1 eval-budget Long/MAX_VALUE evaluate (constantly 0.0) id-fn str}}]
  (loop [frontier (vec initial) seen #{} budget (long eval-budget) levels []]
    (if (or (zero? budget) (empty? frontier))
      {:frontier frontier :levels levels :evaluated (- (long eval-budget) budget)
       :eval-budget eval-budget :k k :seed seed}
      (let [ordered (vec (deterministic-order frontier evaluate))
            selected (vec (take k ordered))
            rest* (vec (drop k ordered))
            children (vec (mapcat expand selected))
            fresh (vec (remove #(contains? seen (id-fn %)) children))
            next-frontier (vec (take k (deterministic-order fresh evaluate)))]
        (recur next-frontier
               (into seen (map id-fn selected))
               (dec budget)
               (conj levels {:selected selected :unselected (into rest* (remove (set selected) fresh))
                             :expanded (count children)}))))))

;; Compatibility aliases for callers that use contract terminology.
(def canonical-parent-set canonical-parent-edges)
(def canonical-parents canonical-parent-edges)
(def merge-mutations plan-merge)
(def merge-plan plan-merge)
(def select-frontier bounded-frontier)
