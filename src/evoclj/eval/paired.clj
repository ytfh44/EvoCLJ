(ns evoclj.eval.paired
  "G5 isolated paired Selection runner (Task 8.4).

  run-paired-selection! evaluates the parent and the candidate as a
  PAIRED comparison on the same selection cases, the same derived
  environment fixture seed, and the same repetitions:

      (run-paired-selection! evaluator
        {:parent-generation \"G42\" :candidate-id \"C17\"
         :case-set [:sel/c1] :repetitions 3})
      ;; => {:parent {...} :candidate {...} :pairs [...]}

  PERFORMANCE CLAIMS ARE PAIRED AND CONTEMPORANEOUS (acceptance):

  - Step 1 — ONE persisted seed per case/repetition, shared by both
    sides. derive-seed is a deterministic content hash over the
    evaluator's seed base, the case id, and the 1-based repetition;
    the same value is handed to the fixture providers of BOTH sides
    (evoclj.eval.runner calls 1-ary fixture fns with it), so parent
    and candidate observe the SAME fixture version wherever the
    provider supports determinism. The seed is persisted on every pair
    (:pair/seed).
  - Step 2 — execution order alternates: pair 1 parent-then-candidate,
    pair 2 candidate-then-parent, ... (execution-order), reducing
    temporal/provider bias.
  - Step 3 — the parent is RE-EVALUATED NOW. run-paired-selection!
    accepts NO parent score input; every pair runs the parent's Genome
    from scratch (fresh temp stores, fresh compile, fresh session) via
    evoclj.eval.runner/run-side! — a fresh candidate is never compared
    to a stale historical parent score.
  - Step 4 — every side of every pair is a FRESH Phenotype instance
    (fresh isolated SCI runtime — fresh session namespaces) running in
    fresh temp stores (Global Constraint 23); each side result carries
    its :side/instance-id and :side/session-id as proof.
  - Step 5 — the Mutator receives only post-evaluation
    aggregate/approved diagnostics (evolution-diagnostics): per-side
    summaries plus per-pair scores, never case prompts, expected
    outputs, or verifier internals. The enforcement boundary is the
    evolution layer (a later task); HERE the surface is documented and
    its cleanliness is asserted — hidden-data-contaminants must be
    empty over the returned result, and run-paired-selection! throws
    :eval/paired-result-contaminated if it ever is not.
  - Step 6 — case-level results persist as content-hash artifact refs
    (Global Constraint 21) and, when the evaluator carries an
    evaluator-only :artifact/root, as EDN files under that root —
    an ACL/path that is NEVER mounted into candidate workspaces. The
    persisted artifact carries ONLY case IDs + scores + outputs;
    case bodies stay in the selection dataset (evoclj.eval.dataset),
    which is never mounted anywhere in this flow.

  EVALUATOR CONTEXT (host-side, kernel-owned):

      {:provider/catalog <catalog map>        ; REQUIRED compile-genome
       :selection/cases <case-id -> case | (fn [id] case)>  ; REQUIRED
       :selection/fixtures <tool-id -> (fn [seed] provider) | provider>
       :programs <registry resolver fn | vector>            ; optional
       :seed <string>                                       ; optional;
       ;  default derived from parent-generation + candidate-id
       :equivalence/by-keyword <kw -> equiv fn>             ; optional
       :artifact/root <directory path>                      ; optional;
       ;  evaluator-only artifact path for case-level results
       :model/registry <model registry atom>                ; optional;
       ;  switches on real model execution for :llm topologies: the
       ;  runner injects it into the broker context and grants a model
       ;  lease (see evoclj.eval.runner). Absent → :llm genomes fail
       ;  closed with :provider/not-found :reason :no-model-registry.
       :model/resource {:kind :model :id \"<provider>/*\"}   ; optional;
       ;  the model lease resource template; default {:kind :model
       ;  :id \"*/*\"} matches no concrete id (fail-closed default)}

  The selection case contract (the case bodies the evaluator resolves
  from the selection dataset):

      {:case/id <keyword>
       :task-input <EDN>             ; the prompt fed to the session
       :expected-output <EDN>        ; the oracle — hidden from results
       :tools #{<tool/id> ...}       ; tools the case exercises
       :output/equiv? <fn | keyword | nil>   ; default byte-identical
       :critical? <bool>}            ; optional, default false

  REQUEST contract: :parent-generation and :candidate-id are strings;
  :case-set is an ordered sequential collection of selection case ids;
  :repetitions is a positive integer (default 1).

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/paired-context-invalid (:reason distinguishes :not-a-map,
  :catalog-missing, :cases-missing, :artifact-root-invalid),
  :eval/paired-request-invalid (:reason distinguishes :not-a-map,
  :parent-generation-invalid, :candidate-id-invalid, :case-set-invalid,
  :case-set-empty, :repetitions-invalid),
  :eval/paired-case-not-found, :eval/paired-case-invalid,
  :eval/paired-equiv-unknown, :eval/paired-result-contaminated."
  (:require [evoclj.eval.runner :as runner]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- Step 1: deterministic shared seeds -------------------------------------

(defn derive-seed
  "The persisted random seed/fixture version for ONE (case,
  repetition): a deterministic content hash over the evaluator's seed
  base, the case id, and the 1-based repetition index (Step 1). The
  SAME value serves BOTH sides of the pair — parent and candidate run
  against the same fixture version wherever the provider supports
  determinism. Deterministic: identical inputs always yield identical
  seeds, so a run is reproducible and queryable."
  [seed-base case-id repetition]
  (hash/text-digest (pr-str [seed-base case-id repetition])))

(defn- seed-base
  "The run's seed base: the evaluator's :seed when given, otherwise a
  deterministic hash over the (parent-generation, candidate-id) pair —
  distinct evaluations derive distinct seed spaces."
  [evaluator request]
  (or (:seed evaluator)
      (hash/text-digest (pr-str [(:parent-generation request)
                                 (:candidate-id request)]))))

;; --- Step 2: alternating execution order -------------------------------------

(defn execution-order
  "The execution order for pair index i (0-based): parent-then-candidate
  on even indexes, candidate-then-parent on odd indexes (Step 2). Both
  sides always run; the ORDER alone alternates to reduce
  temporal/provider bias."
  [i]
  (if (even? i) [:parent :candidate] [:candidate :parent]))

;; --- hidden-data assertion (Step 5) ------------------------------------------

(def hidden-case-keys
  "Keys that must never appear in a paired result artifact: case
  prompts/task inputs, expected outputs, and full case bodies."
  #{:task-input :expected-output :expected/body :case/body :prompt})

(defn hidden-data-contaminants
  "Recursively find every hidden-case key present in `value` (Step 5).
  Returns a vector of the offending keys — empty means the value is a
  clean paired result artifact carrying only case IDs + scores +
  outputs. This is the STRUCTURAL assertion: the artifact never embeds
  a case prompt, expected output, or case body by construction."
  [value]
  (cond
    (map? value)
    (into (filterv hidden-case-keys (keys value))
          (mapcat hidden-data-contaminants)
          (vals value))
    (vector? value) (mapcat hidden-data-contaminants value)
    (seq? value) (mapcat hidden-data-contaminants value)
    (set? value) (mapcat hidden-data-contaminants value)
    :else []))

(defn- assert-clean-result!
  "Fail closed if the paired result embeds any hidden case data (Step
  5 — the assertion the task requires)."
  [result]
  (let [contaminants (hidden-data-contaminants result)]
    (when (seq contaminants)
      (throw (err/error :eval/paired-result-contaminated
                        "paired result artifact embeds hidden case data"
                        {:contaminants contaminants})))))

;; --- boundary validation -----------------------------------------------------

(defn- context-error
  [reason message value]
  (err/error :eval/paired-context-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-evaluator!
  [evaluator]
  (when-not (map? evaluator)
    (throw (context-error :not-a-map
                          "paired evaluator context must be a map"
                          evaluator)))
  (doseq [[k reason] [[:provider/catalog :catalog-missing]
                      [:selection/cases :cases-missing]]]
    (when-not (contains? evaluator k)
      (throw (context-error reason
                            (str "paired evaluator context is missing the " k " key")
                            evaluator))))
  (when-let [root (:artifact/root evaluator)]
    (when-not (string? root)
      (throw (context-error :artifact-root-invalid
                            ":artifact/root must be a directory path string"
                            root))))
  evaluator)

(defn- request-error
  [reason message value]
  (err/error :eval/paired-request-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-request!
  [request]
  (when-not (map? request)
    (throw (request-error :not-a-map
                          "paired selection request must be a map"
                          request)))
  (when-not (and (string? (:parent-generation request))
                 (seq (:parent-generation request)))
    (throw (request-error :parent-generation-invalid
                          ":parent-generation must be a non-empty string"
                          (:parent-generation request))))
  (when-not (and (string? (:candidate-id request))
                 (seq (:candidate-id request)))
    (throw (request-error :candidate-id-invalid
                          ":candidate-id must be a non-empty string"
                          (:candidate-id request))))
  (let [case-set (:case-set request)]
    (when-not (and (sequential? case-set) (every? keyword? case-set))
      (throw (request-error :case-set-invalid
                            ":case-set must be an ordered collection of keyword case ids"
                            case-set)))
    (when (empty? case-set)
      (throw (request-error :case-set-empty
                            ":case-set must not be empty"
                            case-set))))
  (let [repetitions (or (:repetitions request) 1)]
    (when-not (and (int? repetitions) (pos? repetitions))
      (throw (request-error :repetitions-invalid
                            ":repetitions must be a positive integer"
                            repetitions))))
  request)

(defn- case-error
  [reason message value]
  (err/error :eval/paired-case-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- lookup-case
  "Resolve one selection case id in the evaluator's case registry; an
  unknown id fails closed with :eval/paired-case-not-found."
  [evaluator case-id]
  (let [cases (:selection/cases evaluator)
        case-map (cond
                   (map? cases) (get cases case-id)
                   (fn? cases) (cases case-id)
                   :else (throw (context-error :cases-invalid
                                               ":selection/cases must be a map or a lookup fn"
                                               cases)))]
    (when-not case-map
      (throw (err/error :eval/paired-case-not-found
                        "no selection case with this id"
                        {:case/id case-id})))
    case-map))

(defn- validate-case!
  "A selection case must carry a keyword :case/id, a :task-input (the
  prompt fed to the session — it may be any EDN value, so presence is
  checked with contains?), and an :expected-output oracle; :tools is
  an optional set of keyword tool ids; :output/equiv? is an optional
  fn, keyword, or nil; :critical? is an optional boolean."
  [case-map]
  (when-not (map? case-map)
    (throw (case-error :not-a-map
                       "a selection case must be a map"
                       case-map)))
  (when-not (keyword? (:case/id case-map))
    (throw (case-error :missing-case-id
                       "a selection case must carry a keyword :case/id"
                       case-map)))
  (when-not (contains? case-map :task-input)
    (throw (case-error :missing-task-input
                       "a selection case must carry the :task-input"
                       case-map)))
  (when-not (contains? case-map :expected-output)
    (throw (case-error :missing-expected-output
                       "a selection case must carry the :expected-output oracle"
                       case-map)))
  (when-let [tools (:tools case-map)]
    (when-not (and (set? tools) (every? keyword? tools))
      (throw (case-error :tools-invalid
                         ":tools must be a set of keyword tool ids"
                         tools))))
  (let [equiv (:output/equiv? case-map)]
    (when-not (or (nil? equiv) (fn? equiv) (keyword? equiv))
      (throw (case-error :bad-equiv
                         ":output/equiv? must be a fn, a keyword, or nil"
                         equiv))))
  (let [critical? (:critical? case-map)]
    (when-not (or (nil? critical?) (boolean? critical?))
      (throw (case-error :bad-critical
                         ":critical? must be a boolean"
                         critical?))))
  case-map)

;; --- output equivalence (default byte-identical) -----------------------------

(def default-equivalences
  "The kernel-side equivalence registry. :equivalence/byte-identical is
  the default oracle; evaluator contexts may extend the registry via
  :equivalence/by-keyword."
  {:equivalence/byte-identical =})

(defn- resolve-equiv
  "Resolve the case's :output/equiv? to a predicate fn: a declared fn
  is used as-is, a keyword is looked up in the evaluator's
  :equivalence/by-keyword merged over default-equivalences, and nil
  means byte-identical output."
  [evaluator case-map]
  (let [e (:output/equiv? case-map)]
    (cond
      (nil? e) =
      (fn? e) e
      (keyword? e) (or (get (merge default-equivalences
                                   (:equivalence/by-keyword evaluator))
                            e)
                       (throw (err/error :eval/paired-equiv-unknown
                                         "no equivalence predicate registered under this keyword"
                                         {:equivalence/keyword e})))
      :else (throw (err/error :eval/paired-equiv-unknown
                              ":output/equiv? must be a fn, a keyword, or nil"
                              {:value (err/sanitize e)})))))

;; --- scoring and case-level outcomes -----------------------------------------

(defn- side-score
  "A side's score: 1.0 when the run completed AND its outputs satisfy
  the case oracle, 0.0 otherwise (failed/budget-exhausted runs and
  output mismatches score zero)."
  [run-side equiv-fn expected-output]
  (if (and (= :completed (:side/status run-side))
           (equiv-fn expected-output (:side/outputs run-side)))
    1.0
    0.0))

(declare resolve-genome-root)

(defn- outcome-status
  "The pair-level outcome from the two side scores: a strictly better
  candidate is :candidate-wins, a strictly better parent is
  :parent-wins, equal non-zero scores tie, and two zero scores are
  :both-failed."
  [parent-score candidate-score]
  (cond
    (> candidate-score parent-score) :candidate-wins
    (> parent-score candidate-score) :parent-wins
    (zero? parent-score) :both-failed
    :else :tie))

(defn- build-pair
  "One pair record: the derived persisted seed (Step 1), the
  alternating execution order (Step 2), both freshly-run sides
  (Steps 3-4), and a case-level outcome carrying ONLY case IDs +
  scores (Step 5)."
  [evaluator request case-map repetition pair-index seed order equiv-fn]
  (let [side-id {:parent (:parent-generation request)
                 :candidate (:candidate-id request)}
        side-results (into {}
                           (map (fn [kind]
                                  [kind
                                   (runner/run-side!
                                    evaluator
                                    {:genome/root (resolve-genome-root evaluator (side-id kind))
                                     :side/kind kind
                                     :side/id (side-id kind)
                                     :generation/id (:parent-generation request)}
                                    case-map seed)]))
                           order)
        parent-side (:parent side-results)
        candidate-side (:candidate side-results)
        parent-score (side-score parent-side equiv-fn (:expected-output case-map))
        candidate-score (side-score candidate-side equiv-fn (:expected-output case-map))
        sides {:parent (assoc parent-side :side/score parent-score)
               :candidate (assoc candidate-side :side/score candidate-score)}
        case-outcome {:case/id (:case/id case-map)
                      :repetition repetition
                      :pair/seed seed
                      :order order
                      :score/parent parent-score
                      :score/candidate candidate-score
                      :delta (- candidate-score parent-score)
                      :status (outcome-status parent-score candidate-score)
                      :critical? (boolean (:critical? case-map))}]
    {:pair/index pair-index
     :case/id (:case/id case-map)
     :repetition repetition
     :pair/seed seed
     :order order
     :sides sides
     :case/outcome case-outcome}))

(defn- resolve-genome-root
  "Resolve one side's Genome bundle root: the evaluator's :genome/root
  fn called with the side id, or its :genome/roots map. Unknown ids
  fail closed with :eval/paired-genome-unresolved."
  [evaluator id]
  (let [r (:genome/root evaluator)
        roots (:genome/roots evaluator)]
    (cond
      (fn? r) (r id)
      (map? roots) (get roots id)
      :else (throw (err/error :eval/paired-genome-unresolved
                              "no genome root resolver in the evaluator context"
                              {:side/id id})))))

;; --- Step 6: persistence of case-level results -------------------------------

(defn- write-file!
  "Write `content` as UTF-8 to `path`, creating parent directories."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(defn- case-level-result
  "The persisted case-level result for one pair: case ID, repetition,
  seed, order, and per-side status + score + outputs ONLY. Never the
  case prompt (:task-input), never the oracle (:expected-output),
  never the case body (Step 5)."
  [pair]
  {:case/id (:case/id pair)
   :repetition (:repetition pair)
   :pair/seed (:pair/seed pair)
   :order (:order pair)
   :parent (select-keys (get-in pair [:sides :parent])
                        [:side/status :side/score :side/outputs])
   :candidate (select-keys (get-in pair [:sides :candidate])
                           [:side/status :side/score :side/outputs])
   :delta (get-in pair [:case/outcome :delta])})

(defn- persist-case-result!
  "Persist one case-level result: its content-hash artifact ref
  (Global Constraint 21 — the ref is the durable reference; SQLite
  rows reference it, they never duplicate the body) and, when the
  evaluator carries an evaluator-only :artifact/root, an EDN file
  under <root>/<case-id>/<repetition>.edn — an ACL/path that is never
  mounted into candidate workspaces (Global Constraint 23). Returns
  the pair record augmented with :result/artifact-ref and
  :result/artifact-path."
  [evaluator pair]
  (let [artifact (case-level-result pair)
        ref (hash/text-digest (pr-str artifact))
        path (when-let [root (:artifact/root evaluator)]
               (let [dir (str root java.io.File/separator (name (:case/id pair)))
                     file (str dir java.io.File/separator
                               (:repetition pair) ".edn")]
                 (write-file! file (pr-str artifact))
                 file))]
    (assoc pair :result/artifact-ref ref :result/artifact-path path)))

;; --- summaries ---------------------------------------------------------------

(defn- summarize-side
  "The aggregate summary for one side across all pairs: how many
  sides ran, how many scored 1.0, how many scored 0.0, and the total
  score (Step 5 — the aggregate/approved diagnostic the Mutator may
  see)."
  [pairs kind id]
  (let [sides (map #(get-in % [:sides kind]) pairs)
        scores (map :side/score sides)]
    {:side/kind kind
     :side/id id
     :cases (count sides)
     :passed (count (filter #(= 1.0 %) scores))
     :failed (count (filter #(= 0.0 %) scores))
     :score (reduce + 0.0 scores)}))

(defn- aggregate
  "The run-level aggregate over all pairs."
  [pairs]
  (let [statuses (map (comp :status :case/outcome) pairs)]
    {:pairs (count pairs)
     :parent-wins (count (filter #{:parent-wins} statuses))
     :candidate-wins (count (filter #{:candidate-wins} statuses))
     :ties (count (filter #{:tie} statuses))
     :parent-score (reduce + 0.0 (map (comp :score/parent :case/outcome) pairs))
     :candidate-score (reduce + 0.0 (map (comp :score/candidate :case/outcome) pairs))
     :delta (- (reduce + 0.0 (map (comp :score/candidate :case/outcome) pairs))
               (reduce + 0.0 (map (comp :score/parent :case/outcome) pairs)))}))

;; --- Step 5: the Mutator-facing diagnostic surface ---------------------------

(defn evolution-diagnostics
  "The post-evaluation aggregate/approved diagnostics the Mutator may
  receive (Step 5): the per-side summaries plus per-pair scores ONLY.

      {:parent {...aggregate...} :candidate {...aggregate...}
       :pairs [{:pair/index :case/id :repetition :pair/seed :order
                :score/parent :score/candidate :delta :status} ...]}

  No outputs, no case prompts, no expected outputs, no verifier
  internals. The evolution boundary (a later task) enforces that the
  Mutator receives nothing beyond this surface; here it is documented
  and its cleanliness is asserted (hidden-data-contaminants is empty
  over it)."
  [result]
  {:parent (select-keys (:parent result)
                        [:side/kind :side/id :cases :passed :failed :score])
   :candidate (select-keys (:candidate result)
                           [:side/kind :side/id :cases :passed :failed :score])
   :pairs (mapv (fn [p]
                  (merge (select-keys p [:pair/index :case/id :repetition
                                         :pair/seed :order])
                         (select-keys (:case/outcome p)
                                      [:score/parent :score/candidate
                                       :delta :status])))
                (:pairs result))})

;; --- the entry point (G5) ----------------------------------------------------

(defn run-paired-selection!
  "Run the paired Selection comparison (G5, Task 8.4).

  For every case in :case-set, for every repetition 1..:repetitions:
  derive ONE persisted seed (Step 1), alternate the execution order by
  pair index (Step 2), and run BOTH sides through the full scheduler
  with fresh temp stores, fresh compiles, fresh Phenotypes, and fresh
  pinned sessions (Steps 3-4 — the parent is re-evaluated now, never a
  stale historical score). Each pair's case-level result is persisted
  (Step 6) and every artifact is asserted clean of hidden case data
  (Step 5).

  Returns:

      {:parent {:side/kind :parent :side/id <parent-generation>
                :cases n :passed n :failed n :score n}
       :candidate {<same, :side/id = candidate-id>}
       :pairs [<pair record> ...]
       :seed/base <the run's seed base>
       :aggregate {:pairs n :parent-wins n :candidate-wins n :ties n
                   :parent-score n :candidate-score n :delta n}}"
  [evaluator request]
  (validate-evaluator! evaluator)
  (validate-request! request)
  (let [seed-base (seed-base evaluator request)
        repetitions (or (:repetitions request) 1)
        ;; resolve and validate the case bodies ONCE (they carry the
        ;; oracle); bodies never cross into any result artifact
        cases (mapv (fn [case-id]
                      (validate-case! (lookup-case evaluator case-id)))
                    (:case-set request))
        expanded (for [case-map cases
                       r (range 1 (inc repetitions))]
                   [case-map r])
        pairs (mapv (fn [[case-map r] pair-index]
                      (let [seed (derive-seed seed-base (:case/id case-map) r)
                            order (execution-order pair-index)
                            equiv-fn (resolve-equiv evaluator case-map)
                            pair (build-pair evaluator request case-map r
                                             pair-index seed order equiv-fn)]
                        (persist-case-result! evaluator pair)))
                    expanded
                    (range (count expanded)))]
    (let [result {:parent (summarize-side pairs :parent (:parent-generation request))
                  :candidate (summarize-side pairs :candidate (:candidate-id request))
                  :pairs pairs
                  :seed/base seed-base
                  :aggregate (aggregate pairs)}]
      (assert-clean-result! result)
      result)))
